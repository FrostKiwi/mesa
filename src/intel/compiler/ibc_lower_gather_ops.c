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

static unsigned
reg_ref_stride(const ibc_reg_ref *ref)
{
   switch (ref->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      return 0;

   case IBC_REG_FILE_HW_GRF:
      return ref->hw_grf.stride;

   case IBC_REG_FILE_LOGICAL:
      /* TODO: Is this correct? */
      if (ref->reg->logical.bit_size == 1)
         return 0;

      return ibc_logical_reg_stride(ref->reg);

   case IBC_REG_FILE_FLAG:
      unreachable("Should not see flags here");
   }

   unreachable("Unknown register file");
}

static ibc_reg_ref
simd_slice_ref(ibc_reg_ref ref, uint8_t rel_simd_group, uint8_t simd_width)
{
   switch (ref.file) {
   case IBC_REG_FILE_LOGICAL:
      return ref;

   case IBC_REG_FILE_HW_GRF:
      ibc_hw_grf_slice_simd_group(&ref.hw_grf, rel_simd_group, simd_width);
      return ref;

   default:
      unreachable("Unhandled register file");
   }
}

static void
build_MOV_raw(ibc_builder *b, ibc_reg_ref dest, ibc_reg_ref src)
{
   const unsigned total_simd_width = b->simd_width;
   unsigned split_simd_width = total_simd_width;

   /* Can't span more than two registers
    *
    * TODO: Unify with lower_simd_width
    */
   const unsigned src_stride = reg_ref_stride(&src);
   if (src_stride > 0)
      split_simd_width = MIN2(split_simd_width, 64 / src_stride);
   const unsigned dest_stride = reg_ref_stride(&dest);
   if (dest_stride > 0)
      split_simd_width = MIN2(split_simd_width, 64 / dest_stride);

   assert(src.type == dest.type);
   assert(ibc_type_base_type(src.type) == IBC_TYPE_INVALID);
   src.type |= IBC_TYPE_UINT;
   dest.type |= IBC_TYPE_UINT;

   for (unsigned g = 0; g < total_simd_width; g += split_simd_width) {
      ibc_builder_push_group(b, g, split_simd_width);
      ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
      ibc_builder_pop(b);
   }
}

/** Lowers gather operations like SIMD ZIP, VEC, etc. */
bool
ibc_lower_gather_ops(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader, 32);

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr_safe(instr, block) {
         if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
            continue;

         b.cursor = ibc_after_instr(instr);

         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
         switch (intrin->op) {
         case IBC_INTRINSIC_OP_SIMD_ZIP:
            for (unsigned i = 0; i < intrin->num_srcs; i++) {
               const unsigned rel_group = intrin->src[i].simd_group -
                                          instr->simd_group;
               const unsigned width = intrin->src[i].simd_width;
               ibc_builder_push_group(&b, rel_group, intrin->src[i].simd_width);
               assert(intrin->src[i].num_comps == 1); /* TODO */
               build_MOV_raw(&b, simd_slice_ref(intrin->dest,
                                                rel_group, width),
                             simd_slice_ref(intrin->src[i].ref,
                                            rel_group, width));
               ibc_builder_pop(&b);
            }
            break;
         default:
            continue;
         }

         ibc_instr_remove(instr);
         progress = true;
      }
   }

   return progress;
}
