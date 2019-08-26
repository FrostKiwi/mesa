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
ref_stride(const ibc_ref *ref)
{
   switch (ref->file) {
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      return 0;

   case IBC_FILE_HW_GRF:
      return ref->hw_grf.hstride;

   case IBC_FILE_LOGICAL:
      return ref->reg->logical.stride;

   case IBC_FILE_FLAG:
      return 0;
   }

   unreachable("Unknown register file");
}

static void
build_MOV_raw(ibc_builder *b, ibc_ref dest, ibc_ref src)
{
   const unsigned total_simd_width = b->simd_width;
   unsigned split_simd_width = total_simd_width;

   /* Can't span more than two registers
    *
    * TODO: Unify with lower_simd_width
    */
   const unsigned src_stride = ref_stride(&src);
   if (src_stride > 0)
      split_simd_width = MIN2(split_simd_width, 64 / src_stride);
   const unsigned dest_stride = ref_stride(&dest);
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

   ibc_assign_logical_reg_strides(shader);

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      b.cursor = ibc_after_instr(instr);

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_SIMD_ZIP:
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            const unsigned rel_group = intrin->src[i].simd_group -
                                       instr->simd_group;
            ibc_builder_push_group(&b, intrin->src[i].simd_group,
                                       intrin->src[i].simd_width);
            assert(b.simd_group == intrin->src[i].simd_group);
            assert(intrin->src[i].num_comps == intrin->num_dest_comps);
            if (intrin->src[0].num_comps > 1) {
               assert(intrin->src[i].ref.file == IBC_FILE_IMM ||
                      intrin->src[i].ref.file == IBC_FILE_LOGICAL);
               assert(intrin->dest.file == IBC_FILE_LOGICAL);
            }
            ibc_ref mov_dest = intrin->dest;
            ibc_ref mov_src = intrin->src[i].ref;
            ibc_ref_simd_slice(&mov_dest, rel_group);
            ibc_ref_simd_slice(&mov_src, rel_group);
            for (unsigned j = 0; j < intrin->src[i].num_comps; j++) {
               build_MOV_raw(&b, mov_dest, mov_src);
               if (mov_src.file == IBC_FILE_LOGICAL)
                  mov_src.logical.comp++;
               mov_dest.logical.comp++;
            }
            ibc_builder_pop(&b);
         }
         break;

      case IBC_INTRINSIC_OP_VEC:
         if (instr->we_all)
            ibc_builder_push_we_all(&b, instr->simd_width);
         else
            ibc_builder_push_group(&b, instr->simd_group, instr->simd_width);

         assert(intrin->dest.file == IBC_FILE_LOGICAL);
         assert(intrin->num_srcs == intrin->num_dest_comps);

         ibc_ref dest = intrin->dest;
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            build_MOV_raw(&b, dest, intrin->src[i].ref);
            dest.logical.comp++;
         }

         ibc_builder_pop(&b);
         break;

      default:
         continue;
      }

      ibc_instr_remove(instr);
      progress = true;
   }

   return progress;
}
