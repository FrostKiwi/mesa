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

#include "util/bitscan.h"

/** An optimization for reducing gather intrinsics
 *
 * This optimization takes gather intrinsics which are doing some form of
 * no-op gather and tries to reduce them to the minimal form.  For instance, a
 * VEC which combines sequential components from the same vector may get
 * reduced to a single-src VEC which can then be trivially copy propagated.
 */
bool
ibc_opt_gather(ibc_shader *shader)
{
   bool progress = false;

   ibc_foreach_instr(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_SIMD_ZIP:
         while (intrin->num_srcs > 1) {
            assert(util_is_power_of_two_or_zero(intrin->num_srcs));
            bool can_merge = true;
            for (unsigned i = 0; i < intrin->num_srcs; i += 2) {
               ibc_intrinsic_src *left = &intrin->src[i];
               ibc_intrinsic_src *right = &intrin->src[i + 1];

               assert(left->simd_width == right->simd_width);
               assert(left->simd_group + left->simd_width == right->simd_group);
               ibc_ref ref = left->ref;
               ibc_ref_simd_slice(&ref, left->simd_width);
               if (!ibc_refs_equal(ref, right->ref)) {
                  can_merge = false;
                  break;
               }
            }

            if (!can_merge)
               break;

            for (unsigned i = 0; i < intrin->num_srcs / 2; i ++) {
               if (i > 0)
                  intrin->src[i] = intrin->src[i * 2];
               intrin->src[i].simd_width *= 2;
            }
            intrin->num_srcs /= 2;
            progress = true;
         }
         break;

      case IBC_INTRINSIC_OP_PACK:
         while (intrin->num_srcs > 1) {
            assert(util_is_power_of_two_or_zero(intrin->num_srcs));
            bool can_merge = true;
            for (unsigned i = 0; i < intrin->num_srcs; i += 2) {
               ibc_intrinsic_src *left = &intrin->src[i];
               ibc_intrinsic_src *right = &intrin->src[i + 1];

               /* We can't take a byte offset of an immediate.  If everything
                * were immediates we could, in theory, constant fold but that
                * should never happen.
                */
               if (left->ref.file == IBC_FILE_IMM) {
                  can_merge = false;
                  break;
               }

               ibc_ref ref = left->ref;
               ibc_ref_byte_offset(&ref, ibc_type_byte_size(ref.type));
               if (!ibc_refs_equal(ref, right->ref)) {
                  can_merge = false;
                  break;
               }
            }

            if (!can_merge)
               break;

            for (unsigned i = 0; i < intrin->num_srcs / 2; i ++) {
               if (i > 0)
                  intrin->src[i] = intrin->src[i * 2];
               intrin->src[i].ref.type =
                  ibc_type_base_type(intrin->src[i].ref.type) |
                  (ibc_type_bit_size(intrin->src[i].ref.type) * 2);
            }
            intrin->num_srcs /= 2;
            progress = true;
         }
         break;

      case IBC_INTRINSIC_OP_VEC:
      case IBC_INTRINSIC_OP_MESSAGE: {
         unsigned num_srcs = 1;
         for (unsigned i = 1; i < intrin->num_srcs; i++) {
            ibc_intrinsic_src *prev_src = &intrin->src[num_srcs - 1];
            ibc_ref prev_ref = prev_src->ref;
            ibc_ref_comp_offset(&prev_ref, prev_src->num_comps,
                                prev_src->simd_width);
            if (prev_src->simd_group == intrin->src[i].simd_group &&
                prev_src->simd_width == intrin->src[i].simd_width &&
                prev_ref.file != IBC_FILE_IMM &&
                ibc_refs_equal(prev_ref, intrin->src[i].ref)) {
               /* This source folds in with the previous one */
               prev_src->num_comps += intrin->src[i].num_comps;
            } else {
               /* This is its own source */
               assert(num_srcs <= i);
               if (num_srcs < i)
                  intrin->src[num_srcs] = intrin->src[i];
               num_srcs++;
            }
         }
         if (num_srcs < intrin->num_srcs) {
            progress = true;
            intrin->num_srcs = num_srcs;
         }
         break;
      }

      default:
         break;
      }
   }

   return progress;
}
