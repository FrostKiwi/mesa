/*
 * Copyright © 2019 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "ibc.h"
#include "ibc_builder.h"

#include "brw_eu.h"

static unsigned
ref_stride(const ibc_ref *ref)
{
   switch (ref->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      return 0;

   case IBC_REG_FILE_HW_GRF:
      return ref->hw_grf.hstride;

   case IBC_REG_FILE_LOGICAL:
      if (ref->logical.broadcast || ref->reg->logical.simd_width == 1)
         return 0;
      return ref->reg->logical.stride;

   case IBC_REG_FILE_FLAG:
      assert(ref->type != IBC_TYPE_FLAG);
      return 0;
   }

   unreachable("Unknown register file");
}

static ibc_ref
simd_restricted_src(ibc_builder *b, ibc_ref src,
                    uint8_t old_simd_group, uint8_t new_simd_group,
                    uint8_t simd_width, unsigned num_comps)
{
   if (src.file == IBC_REG_FILE_NONE || src.file == IBC_REG_FILE_IMM)
      return src;

   ibc_ref_simd_slice(&src, new_simd_group - old_simd_group);

   /* If the source is WLR, then we have two cases:
    *
    *  1. This read is dominated by the final write.  In this case, it's
    *     "locked" and we don't need a MOV instruction.
    *
    *  2. This is self-read which contributes to building the WLR value.  In
    *     this case, we require things to be trivially splittable.
    *
    * In either case, the right thing to do is to just return src.
    */
   if (src.reg && src.reg->is_wlr)
      return src;

   return ibc_MOV_raw(b, src);
}

static void
fixup_split_write_link(ibc_reg_write *write, ibc_ref *dest,
                       ibc_reg_write *split_write, ibc_ref *split_dest)
{
   if (dest->file != IBC_REG_FILE_NONE && dest->reg == split_dest->reg) {
      /* For WLR multi-writes, we need to ensure that the write list is
       * in the correct order and it may have gotten out-of-order thanks
       * to this lowering.
       */
      list_del(&split_write->link);
      list_addtail(&split_write->link, &write->link);
   }
}

bool
ibc_lower_simd_width(ibc_shader *shader)
{
   bool progress = false;

   ibc_assign_logical_reg_strides(shader);

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      unsigned split_simd_width = instr->simd_width;
      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU: {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);
         split_simd_width = MIN2(split_simd_width, 32);

         unsigned num_srcs = ibc_alu_op_infos[alu->op].num_srcs;
         for (unsigned j = 0; j < num_srcs; j++) {
            /* Can't span more than two registers */
            const unsigned src_stride = ref_stride(&alu->src[j].ref);
            if (src_stride > 0)
               split_simd_width = MIN2(split_simd_width, 64 / src_stride);
         }

         /* Can't span more than two registers */
         const unsigned dest_stride = ref_stride(&alu->dest);
         if (dest_stride > 0)
            split_simd_width = MIN2(split_simd_width, 64 / dest_stride);
         break;
      }

      case IBC_INSTR_TYPE_INTRINSIC: {
         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
         switch (intrin->op) {
         case IBC_INTRINSIC_OP_FB_WRITE:
            split_simd_width = ibc_lower_simd_width_fb_write_max_width(intrin);
            break;
         default:
            split_simd_width = MIN2(split_simd_width, 16); /* TODO */
            break;
         }
         break;
      }

      default:
         continue;
      }
      if (instr->simd_width <= split_simd_width)
         continue;

      const unsigned num_splits = instr->simd_width / split_simd_width;

      /* We've decided to split.  Set up the builder */
      assert(b._group_stack_size == 0);
      ibc_builder_push_group(&b, instr->simd_group,
                                 instr->simd_width);
      b.cursor = ibc_after_instr(instr);

      ibc_ref *dest = instr->type == IBC_INSTR_TYPE_ALU ?
                      &ibc_instr_as_alu(instr)->dest :
                      &ibc_instr_as_intrinsic(instr)->dest;
      const unsigned num_dest_comps =
         instr->type == IBC_INSTR_TYPE_ALU ? 1 :
         ibc_instr_as_intrinsic(instr)->num_dest_comps;

      /* 4 == 32 (max simd width) / 8 (min simd width) */
      ibc_ref split_dests[4];
      assert(num_splits <= ARRAY_SIZE(split_dests));
      if (dest->file == IBC_REG_FILE_NONE) {
         /* If the destination is NONE, just copy it to all the split
          * instruction destinations.
          */
         for (unsigned i = 0; i < num_splits; i++)
            split_dests[i] = *dest;
      } else if (dest->reg->is_wlr &&
                 !list_is_singular(&dest->reg->writes)) {
         /* WLR multi-writes are expected to be trivially splittable and
          * we have to naively split them in order to maintain the WLR
          * properties.
          */
         for (unsigned i = 0; i < num_splits; i++) {
            split_dests[i] = *dest;
            ibc_ref_simd_slice(&split_dests[i], i * split_simd_width);
         }
      } else if (dest->file != IBC_REG_FILE_NONE) {
         /* For everything else, we emit a SIMD zip after the instruction
          * we're splitting.
          */
         ibc_intrinsic_instr *zip =
            ibc_intrinsic_instr_create(b.shader, IBC_INTRINSIC_OP_SIMD_ZIP,
                                       b.simd_group, b.simd_width,
                                       num_splits);

         for (unsigned i = 0; i < num_splits; i++) {
            ibc_reg *split_dest_reg =
               ibc_logical_reg_create(b.shader,
                                      ibc_type_bit_size(dest->type),
                                      num_dest_comps,
                                      b.simd_group + i * split_simd_width,
                                      split_simd_width);

            zip->src[i].ref = ibc_typed_ref(split_dest_reg, IBC_TYPE_INVALID);
            zip->src[i].simd_group = split_dest_reg->logical.simd_group;
            zip->src[i].simd_width = split_dest_reg->logical.simd_width;
            zip->src[i].num_comps = split_dest_reg->logical.num_comps;

            split_dests[i] = ibc_typed_ref(split_dest_reg, dest->type);
         }

         zip->dest = *dest;
         zip->dest.type = ibc_type_bit_type(zip->dest.type);
         zip->num_dest_comps = num_dest_comps;
         ibc_builder_insert_instr(&b, &zip->instr);

         /* We want to insert the split instructions before the zip. */
         b.cursor = ibc_before_instr(&zip->instr);
      }

      for (unsigned i = 0; i < num_splits; i++) {
         const unsigned split_simd_rel_group = i * split_simd_width;
         if (instr->we_all) {
            ibc_builder_push_we_all(&b, split_simd_width);
         } else {
            ibc_builder_push_group(&b, split_simd_rel_group,
                                       split_simd_width);
         }

         switch (instr->type) {
         case IBC_INSTR_TYPE_ALU: {
            ibc_alu_instr *alu = ibc_instr_as_alu(instr);

            ibc_alu_instr *split =
               ibc_alu_instr_create(shader, alu->op,
                                    b.simd_group, b.simd_width);
            unsigned num_srcs = ibc_alu_op_infos[alu->op].num_srcs;
            for (unsigned j = 0; j < num_srcs; j++) {
               split->src[j] = alu->src[j];
               split->src[j].ref =
                  simd_restricted_src(&b, alu->src[j].ref,
                                      instr->simd_group,
                                      instr->simd_group + split_simd_rel_group,
                                      split_simd_width, 1);
            }

            split->cmod = alu->cmod;
            split->instr.we_all = b.we_all;
            split->instr.predicate = alu->instr.predicate;
            split->instr.pred_inverse = alu->instr.pred_inverse;
            split->instr.flag = alu->instr.flag;
            ibc_ref_simd_slice(&split->instr.flag, split_simd_rel_group);

            split->dest = split_dests[i];
            ibc_builder_insert_instr(&b, &split->instr);
            fixup_split_write_link(&alu->dest_write, &alu->dest,
                                   &split->dest_write, &split->dest);
            break;
         }

         case IBC_INSTR_TYPE_INTRINSIC: {
            ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);

            ibc_intrinsic_instr *split =
               ibc_intrinsic_instr_create(shader, intrin->op,
                                          b.simd_group, b.simd_width,
                                          intrin->num_srcs);
            split->can_reorder = intrin->can_reorder;
            split->has_side_effects = intrin->has_side_effects;

            switch (intrin->op) {
            case IBC_INTRINSIC_OP_SIMD_ZIP: {
               unsigned src_idx = 0;
               for (unsigned j = 0; j < intrin->num_srcs; j++) {
                  const unsigned src_group = MAX2(intrin->src[j].simd_group,
                                                  split->instr.simd_group);
                  const unsigned src_group_end =
                     MIN2(intrin->src[j].simd_group + intrin->src[j].simd_width,
                          split->instr.simd_group + split->instr.simd_width);
                  if (src_group >= src_group_end)
                     continue;

                  const unsigned src_width = src_group_end - src_group;

                  split->src[src_idx] = intrin->src[j];
                  split->src[src_idx].simd_group = src_group;
                  split->src[src_idx].simd_width = src_width;
                  split->src[src_idx].ref =
                     simd_restricted_src(&b, intrin->src[j].ref,
                                             src_group,
                                             intrin->src[j].simd_group,
                                             src_width,
                                             intrin->src[i].num_comps);
                  src_idx++;
               }
               split->num_srcs = src_idx;
               break;
            }
            default:
               for (unsigned j = 0; j < intrin->num_srcs; j++) {
                  assert(intrin->src[j].simd_group == intrin->instr.simd_group);
                  assert(intrin->src[j].simd_width == intrin->instr.simd_width);

                  split->src[j] = intrin->src[j];
                  split->src[j].simd_group = b.simd_group;
                  split->src[j].simd_width = b.simd_width;
                  split->src[j].ref =
                     simd_restricted_src(&b, intrin->src[j].ref,
                                             intrin->src[j].simd_group,
                                             split->src[j].simd_group,
                                             split_simd_width,
                                             intrin->src[i].num_comps);
               }
               break;
            }

            split->instr.we_all = b.we_all;
            split->instr.predicate = intrin->instr.predicate;
            split->instr.pred_inverse = intrin->instr.pred_inverse;
            split->instr.flag = intrin->instr.flag;
            ibc_ref_simd_slice(&split->instr.flag, split_simd_rel_group);

            split->dest = split_dests[i];
            split->num_dest_comps = intrin->num_dest_comps;
            ibc_builder_insert_instr(&b, &split->instr);
            fixup_split_write_link(&intrin->dest_write, &intrin->dest,
                                   &split->dest_write, &split->dest);
            break;
         }

         default:
            unreachable("Unhandled IBC instruction type");
         }

         ibc_builder_pop(&b);
      }

      ibc_instr_remove(instr);
      ibc_builder_pop(&b);
      progress = true;
   }

   return progress;
}
