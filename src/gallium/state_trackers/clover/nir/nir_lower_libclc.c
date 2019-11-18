/*
 * Copyright © 2019 Red Hat
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
 *
 */

/* Pass to find libclc functions from a clc library shader and inline
 * them into a user shader.
 * This pass should only be called once, but it also has to iterate
 * itself to make sure all instances are lowered, before validation.
 */
#include "nir.h"
#include "nir_builder.h"
#include "nir_lower_libclc.h"

static bool
lower_clc_block(nir_block *block, nir_builder *b,
                const nir_shader *clc_shader)
{
   bool progress = false;

   nir_foreach_instr_safe(instr, block) {
      if (instr->type != nir_instr_type_call)
         continue;

      progress = true;

      nir_call_instr *call = nir_instr_as_call(instr);
      nir_function *func = NULL;
      nir_foreach_function(function, clc_shader) {
         if (strcmp(function->name, call->callee->name) == 0) {
            func = function;
            break;
         }
      }
      if (!func || !func->impl) {
         return NULL;
      }

      nir_ssa_def *params[4] = { NULL, };

      for (unsigned i = 0; i < call->num_params; i++) {
         params[i] = nir_ssa_for_src(b, call->params[i],
                                     call->callee->params[i].num_components);
      }

      b->cursor = nir_instr_remove(&call->instr);
      nir_inline_function_impl(b, func->impl, params);
   }
   return progress;
}

static bool
nir_lower_libclc_impl(nir_function_impl *impl,
                      const nir_shader *clc_shader)
{
   nir_builder b;
   nir_builder_init(&b, impl);

   bool progress = false;
   nir_foreach_block_safe(block, impl) {
      progress |= lower_clc_block(block, &b, clc_shader);
   }
   if (progress) {
      nir_index_ssa_defs(impl);
      nir_index_local_regs(impl);
      nir_metadata_preserve(impl, nir_metadata_none);
   } else {
#ifndef NDEBUG
      impl->valid_metadata &= ~nir_metadata_not_properly_reset;
#endif
   }
   return progress;
}

bool
nir_lower_libclc(nir_shader *shader,
                 const nir_shader *clc_shader)
{
   bool progress = false;

   /* do progress passes inside the pass */
   do {
     progress = false;
     nir_foreach_function(function, shader) {
       if (function->impl)
         progress |= nir_lower_libclc_impl(function->impl, clc_shader);
     }
   } while(progress);
   return progress;
}
