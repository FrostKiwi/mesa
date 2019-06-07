/*
 * Copyright © 2018 Intel Corporation
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

static void
MOV_vec_to(ibc_builder *b, ibc_reg_ref dest, ibc_reg_ref src,
           unsigned num_comps)
{
   assert(src.file == IBC_REG_FILE_LOGICAL);
   assert(dest.file == IBC_REG_FILE_LOGICAL);

   for (unsigned i = 0; i < num_comps; i++) {
      for (unsigned g = 0; g < b->simd_width; g += 16) {
         ibc_builder_push_group(b, g, MIN2(b->simd_width, 16));
         ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
         ibc_builder_pop(b);
      }
      src.logical.comp++;
      dest.logical.comp++;
   }
}

bool
ibc_lower_phis(ibc_shader *shader)
{
   ibc_builder b;
   ibc_builder_init(&b, shader);

   bool progress = false;
   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_PHI)
         continue;

      ibc_phi_instr *phi = ibc_instr_as_phi(instr);

      b.cursor = ibc_after_instr(&phi->instr);

      ibc_reg_ref tmp =
         ibc_builder_new_logical_reg(&b, phi->dest.type, phi->num_comps);
      ((ibc_reg *)tmp.reg)->is_wlr = false;

      MOV_vec_to(&b, phi->dest, tmp, phi->num_comps);

      ibc_foreach_phi_src(phi_src, phi) {
         b.cursor = ibc_before_instr(&phi_src->pred->instr);
         MOV_vec_to(&b, tmp, phi_src->ref, phi->num_comps);
      }

      ibc_instr_remove(&phi->instr);
      progress = true;
   }

   return progress;
}
