/*
 * Copyright © Microsoft Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "nir.h"
#include "glsl_types.h"
#include "nir_types.h"
#include "nir_builder.h"

#include "clc_nir.h"
#include "../compiler/dxil_nir.h"

static bool
lower_load_global_invocation_id(nir_builder *b, nir_intrinsic_instr *intr,
                                nir_variable *var)
{
   nir_intrinsic_instr *load;

   b->cursor = nir_after_instr(&intr->instr);

   nir_ssa_def *offset =
      build_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                          nir_imm_int(b, 0),
                          nir_dest_num_components(intr->dest),
                          nir_dest_bit_size(intr->dest));
   nir_ssa_def *result = nir_iadd(b, &intr->dest.ssa, offset);
   nir_ssa_def_rewrite_uses_after(&intr->dest.ssa, nir_src_for_ssa(result),
                                  result->parent_instr);
   return true;
}

/* Make sure you only call this lowering pass once. */
bool
clc_nir_lower_kernel_global_work_offset(nir_shader *nir, nir_variable *var)
{
   bool progress = false;

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

            if (intr->intrinsic == nir_intrinsic_load_global_invocation_id)
               progress |= lower_load_global_invocation_id(&b, intr, var);
         }
      }
   }

   return progress;
}
