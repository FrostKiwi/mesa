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

static ibc_reg_ref
simd_offset_ref(ibc_reg_ref ref, unsigned simd_group_offset)
{
   switch (ref.file) {
   case IBC_REG_FILE_LOGICAL:
      return ref;

   case IBC_REG_FILE_HW_GRF:
      if (ref.hw_grf.stride == 0)
         return ref;

      ref.hw_grf.offset += simd_group_offset * ref.hw_grf.stride;
      return ref;

   default:
      unreachable("Unhandled register file");
   }
}

static ibc_reg_ref
simd_restricted_src(ibc_builder *b, unsigned src_simd_group, ibc_reg_ref src,
                    unsigned num_comps)
{
   if (src.file == IBC_REG_FILE_NONE || src.file == IBC_REG_FILE_IMM)
      return src;

   src = simd_offset_ref(src, b->simd_group - src_simd_group);

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
   if (src.reg->is_wlr)
      return src;

   return ibc_MOV_raw(b, src);
}

bool
ibc_lower_simd_width(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader, 32);

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr_safe(instr, block) {
         if (instr->type == IBC_INSTR_TYPE_SEND ||
             instr->type == IBC_INSTR_TYPE_JUMP)
            continue;

         assert(instr->type == IBC_INSTR_TYPE_ALU ||
                instr->type == IBC_INSTR_TYPE_INTRINSIC);

         const unsigned split_simd_width = 16; /* TODO */
         if (instr->simd_width <= split_simd_width)
            continue;

         const unsigned num_splits = instr->simd_width / split_simd_width;

         /* We've decided to split.  Set up the builder */
         assert(b._group_stack_size == 0);
         ibc_builder_push_group(&b, instr->simd_group,
                                    instr->simd_width);
         b.cursor = ibc_after_instr(instr);

         ibc_reg_ref *dest = instr->type == IBC_INSTR_TYPE_ALU ?
                             &ibc_instr_as_alu(instr)->dest :
                             &ibc_instr_as_intrinsic(instr)->dest;
         const unsigned num_dest_comps = 1; /* TODO */

         /* 4 == 32 (max simd width) / 8 (min simd width) */
         ibc_reg_ref split_dests[4];
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
            for (unsigned i = 0; i < num_splits; i++)
               split_dests[i] = simd_offset_ref(*dest, i * split_simd_width);
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

               zip->src[i].ref = ibc_ref(split_dest_reg);
               zip->src[i].simd_group = split_dest_reg->logical.simd_group;
               zip->src[i].simd_width = split_dest_reg->logical.simd_width;
               zip->src[i].num_comps = split_dest_reg->logical.num_comps;

               split_dests[i] = ibc_typed_ref(split_dest_reg, dest->type);
            }

            zip->dest = *dest;
            zip->num_dest_comps = num_dest_comps;
            ibc_builder_insert_instr(&b, &zip->instr);

            /* We want to insert the split instructions before the zip. */
            b.cursor = ibc_before_instr(&zip->instr);
         }

         for (unsigned i = 0; i < num_splits; i++) {
            ibc_builder_push_group(&b, i * split_simd_width, split_simd_width);

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
                     simd_restricted_src(&b, alu->instr.simd_group,
                                             alu->src[j].ref, 1);
               }

               split->cmod = alu->cmod;
               split->instr.predicate = alu->instr.predicate;
               split->instr.pred_inverse = alu->instr.pred_inverse;
               if (alu->instr.flag.file != IBC_REG_FILE_NONE) {
                  assert(alu->instr.flag.file == IBC_REG_FILE_LOGICAL);
                  split->instr.flag = alu->instr.flag;
               }

               split->dest = split_dests[i];

               ibc_builder_insert_instr(&b, &split->instr);
               break;
            }

            case IBC_INSTR_TYPE_INTRINSIC: {
               ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);

               ibc_intrinsic_instr *split =
                  ibc_intrinsic_instr_create(shader, intrin->op,
                                             b.simd_group, b.simd_width,
                                             intrin->num_srcs);
               split->has_side_effects = intrin->has_side_effects;

               for (unsigned j = 0; j < intrin->num_srcs; j++) {
                  split->src[j] = intrin->src[j];
                  split->src[j].ref =
                     simd_restricted_src(&b, intrin->instr.simd_group,
                                             intrin->src[j].ref,
                                             intrin->src[i].num_comps);
                  split->src[j].simd_group = b.simd_group;
                  split->src[j].simd_width = b.simd_width;
               }

               split->instr.predicate = intrin->instr.predicate;
               split->instr.pred_inverse = intrin->instr.pred_inverse;
               if (intrin->instr.flag.file != IBC_REG_FILE_NONE) {
                  assert(intrin->instr.flag.file == IBC_REG_FILE_LOGICAL);
                  split->instr.flag = intrin->instr.flag;
               }

               split->dest = split_dests[i];

               ibc_builder_insert_instr(&b, &split->instr);
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
   }

   return progress;
}
