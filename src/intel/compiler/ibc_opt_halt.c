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
ibc_opt_halt(ibc_shader *shader)
{
   if (shader->stage != MESA_SHADER_FRAGMENT)
      return false;

   ibc_flow_instr *halt_merge = NULL;
   ibc_foreach_flow_instr_reverse(flow, shader) {
      if (flow->op == IBC_FLOW_OP_HALT_MERGE) {
#ifndef NDEBUG
         assert(halt_merge == NULL);
         halt_merge = flow;
#else
         halt_merge = flow;
         break;
#endif
      }
   }

   if (halt_merge == NULL)
      return false;

   bool progress = false;

   ibc_instr *prev_instr = ibc_instr_prev(&halt_merge->instr);
   if (prev_instr->type == IBC_INSTR_TYPE_FLOW &&
       ibc_instr_as_flow(prev_instr)->op == IBC_FLOW_OP_HALT_JUMP) {
      /* This instruction is a HALT_JUMP immediately followed by the
       * HALT_MERGE that it jumps to.  We can just up and delete it since it
       * will immediately fall through.
       */
      ibc_flow_instr *halt_jump = ibc_instr_as_flow(prev_instr);

      ibc_foreach_flow_pred(pred, halt_merge) {
         if (pred->instr == halt_jump) {
            list_del(&pred->link);
            break;
         }
      }

      list_del(&halt_jump->instr.link);
      list_del(&halt_jump->flow_link);
      progress = true;
   }

   if (list_is_singular(&halt_merge->preds)) {
      /* In this case, there are no actual HALT_JUMP instructions jumping to
       * this HALT_MERGE so we can just delete it.
       */
      list_del(&halt_merge->instr.link);
      list_del(&halt_merge->flow_link);
      progress = true;
   }

   return progress;
}
