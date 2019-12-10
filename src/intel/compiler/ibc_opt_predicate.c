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

bool
ibc_opt_predicate(ibc_shader *shader)
{
   bool progress = false;

   ibc_foreach_instr(instr, shader) {
      if (instr->predicate == IBC_PREDICATE_NONE)
         continue;

      assert(instr->flag.type == IBC_TYPE_FLAG);
      if (instr->flag.reg == NULL)
         continue;

      ibc_instr *pred_ssa = ibc_reg_ssa_instr(instr->flag.reg);
      if (pred_ssa == NULL || pred_ssa->type != IBC_INSTR_TYPE_ALU)
         continue;

      ibc_alu_instr *pred_alu = ibc_instr_as_alu(pred_ssa);
      if (pred_alu->op == IBC_ALU_OP_NOT &&
          pred_alu->dest.type == IBC_TYPE_FLAG &&
          pred_alu->src[0].ref.type == IBC_TYPE_FLAG &&
          pred_alu->src[0].ref.reg != NULL &&
          ibc_ref_read_is_static(pred_alu->src[0].ref)) {
         ibc_instr_set_ref(instr, &instr->flag, pred_alu->src[0].ref);
         instr->predicate = ibc_predicate_invert(instr->predicate);
         progress = true;
      }
   }

   return progress;
}
