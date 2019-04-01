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
simd_restricted_src(ibc_builder *b, unsigned src_simd_group, ibc_reg_ref src,
                    unsigned num_comps)
{
   assert(num_comps == 1);

   switch (src.file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      return src;
   case IBC_REG_FILE_LOGICAL:
      if (src.reg->logical.simd_width == 1) {
         return src;
      } else {
         return ibc_MOV(b, src.type, src);
      }
   case IBC_REG_FILE_HW_GRF:
      if (src.stride == 0) {
         return src;
      } else {
         assert(b->simd_group >= src_simd_group);
         src.offset += src.stride * b->simd_group - src_simd_group;
         return ibc_MOV(b, src.type, src);
      }
   }
   unreachable("Unknown register file");
}

static void
build_simd_zip(ibc_builder *b, ibc_reg_ref dest, ibc_reg_ref *srcs,
               unsigned src_simd_width, unsigned num_srcs, unsigned num_comps)
{
   /* Record this before we push the builder stack */
   unsigned zip_simd_group = b->simd_group;

   assert(num_comps == 1); /* TODO */

   /* This reg is now written by multiple instructions. */
   if (dest.file == IBC_REG_FILE_LOGICAL)
      ((ibc_reg *)dest.reg)->logical.ssa = NULL;

   /* TODO: We want an SSA zip instruction */
   for (unsigned i = 0; i < num_srcs; i++) {
      ibc_builder_push_group(b, i * src_simd_width, src_simd_width);

      assert(dest.file == IBC_REG_FILE_LOGICAL ||
             dest.file == IBC_REG_FILE_HW_GRF);
      ibc_reg_ref mov_dest = dest;
      if (dest.file == IBC_REG_FILE_HW_GRF)
         mov_dest.offset += dest.stride * b->simd_group - zip_simd_group;

      ibc_build_alu1(b, IBC_ALU_OP_MOV, mov_dest, srcs[i]);

      ibc_builder_pop(b);
   }
}

bool
ibc_lower_simd_width(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader, 32);

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr_safe(instr, block) {
         switch (instr->type) {
         case IBC_INSTR_TYPE_ALU: {
            ibc_alu_instr *alu = ibc_instr_as_alu(instr);

            unsigned max_width = 16; /* TODO */
            if (alu->instr.simd_width <= max_width)
               continue;

            /* Insert after this instruction */
            b.cursor = ibc_after_instr(&alu->instr);
            assert(b._group_stack_size == 0);
            ibc_builder_push_group(&b, alu->instr.simd_group,
                                   alu->instr.simd_width);

            ibc_reg_ref dests[4];
            for (unsigned i = 0; i < alu->instr.simd_width / max_width; i++) {
               ibc_builder_push_group(&b, i * max_width, max_width);

               ibc_alu_instr *split =
                  ibc_alu_instr_create(shader, alu->op,
                                       b.simd_group, b.simd_width);
               unsigned num_srcs = 3; /* TODO */
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

               split->dest = alu->dest;
               if (alu->dest.ref.file != IBC_REG_FILE_NONE) {
                  ibc_reg *dest_reg =
                     ibc_builder_new_logical_reg(&b, alu->dest.ref.type, 1);
                  dests[i] = ibc_typed_ref(dest_reg, alu->dest.ref.type);
                  dest_reg->logical.ssa = &split->instr;
                  split->dest.ref = dests[i];
               }

               ibc_builder_insert_instr(&b, &split->instr);

               ibc_builder_pop(&b);
            }

            if (alu->dest.ref.file != IBC_REG_FILE_NONE) {
               build_simd_zip(&b, alu->dest.ref, dests, max_width,
                              alu->instr.simd_width / max_width, 1);
            }

            ibc_instr_remove(&alu->instr);

            ibc_builder_pop(&b);
            progress = true;
            continue;
         }

         case IBC_INSTR_TYPE_SEND:
            /* We can't do anything with these.  They had better already be
             * able to handle the bit size.
             */
            continue;

         case IBC_INSTR_TYPE_INTRINSIC: {
            ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);

            unsigned max_width = 16; /* TODO */
            if (intrin->instr.simd_width <= max_width)
               continue;

            /* Insert after this instruction */
            b.cursor = ibc_after_instr(&intrin->instr);
            assert(b._group_stack_size == 0);
            ibc_builder_push_group(&b, intrin->instr.simd_group,
                                   intrin->instr.simd_width);

            ibc_reg_ref dests[4];
            for (unsigned i = 0; i < intrin->instr.simd_width / max_width; i++) {
               ibc_builder_push_group(&b, i * max_width, max_width);

               ibc_intrinsic_instr *split =
                  ibc_intrinsic_instr_create(shader, intrin->op,
                                             b.simd_group, b.simd_width,
                                             intrin->num_srcs);
               for (unsigned j = 0; j < intrin->num_srcs; j++) {
                  split->src[j] = intrin->src[j];
                  split->src[j].ref =
                     simd_restricted_src(&b, intrin->instr.simd_group,
                                             intrin->src[j].ref,
                                             intrin->src[i].num_comps);
                  split->src[j].simd_group = b.simd_group;
                  split->src[j].simd_width = b.simd_width;
               }

               split->dest = intrin->dest;
               if (intrin->dest.ref.file != IBC_REG_FILE_NONE) {
                  ibc_reg *dest_reg =
                     ibc_builder_new_logical_reg(&b, intrin->dest.ref.type,
                                                 intrin->dest.num_comps);
                  dests[i] = ibc_typed_ref(dest_reg, intrin->dest.ref.type);
                  split->dest.ref = dests[i];
               }
               split->dest.simd_group = b.simd_group;
               split->dest.simd_width = b.simd_width;

               split->instr.predicate = intrin->instr.predicate;
               split->instr.pred_inverse = intrin->instr.pred_inverse;
               if (intrin->instr.flag.file != IBC_REG_FILE_NONE) {
                  assert(intrin->instr.flag.file == IBC_REG_FILE_LOGICAL);
                  split->instr.flag = intrin->instr.flag;
               }

               ibc_builder_insert_instr(&b, &split->instr);

               ibc_builder_pop(&b);
            }

            if (intrin->dest.ref.file != IBC_REG_FILE_NONE) {
               build_simd_zip(&b, intrin->dest.ref, dests, max_width,
                              intrin->instr.simd_width / max_width,
                              intrin->dest.num_comps);
            }

            ibc_instr_remove(&intrin->instr);

            ibc_builder_pop(&b);
            progress = true;
            continue;
         }

         case IBC_INSTR_TYPE_JUMP:
            continue;
         }
         unreachable("Unsupported IBC instruction type");
      }
   }

   return progress;
}
