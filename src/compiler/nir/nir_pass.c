/*
 * Copyright © 2014 Intel Corporation
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
 * Authors:
 *    Jason Ekstrand (jason@jlekstrand.net)
 */

#include "nir.h"
#include "nir_builder.h"

/*
 * Handles management of NIR passes and metadata.
 */

void
nir_metadata_require(nir_function_impl *impl, nir_metadata required, ...)
{
#define NEEDS_UPDATE(X) ((required & ~impl->valid_metadata) & (X))

   if (NEEDS_UPDATE(nir_metadata_block_index))
      nir_index_blocks(impl);
   if (NEEDS_UPDATE(nir_metadata_dominance))
      nir_calc_dominance_impl(impl);
   if (NEEDS_UPDATE(nir_metadata_live_ssa_defs))
      nir_live_ssa_defs_impl(impl);
   if (NEEDS_UPDATE(nir_metadata_loop_analysis)) {
      va_list ap;
      va_start(ap, required);
      nir_loop_analyze_impl(impl, va_arg(ap, nir_variable_mode));
      va_end(ap);
   }

#undef NEEDS_UPDATE

   impl->valid_metadata |= required;
}

void
nir_metadata_preserve(nir_function_impl *impl, nir_metadata preserved)
{
   impl->valid_metadata &= preserved;
}

#ifndef NDEBUG
/**
 * Make sure passes properly invalidate metadata (part 1).
 *
 * Call this before running a pass to set a bogus metadata flag, which will
 * only be preserved if the pass forgets to call nir_metadata_preserve().
 */
void
nir_metadata_set_validation_flag(nir_shader *shader)
{
   nir_foreach_function(function, shader) {
      if (function->impl) {
         function->impl->valid_metadata |= nir_metadata_not_properly_reset;
      }
   }
}

/**
 * Make sure passes properly invalidate metadata (part 2).
 *
 * Call this after a pass makes progress to verify that the bogus metadata set by
 * the earlier function was properly thrown away.  Note that passes may not call
 * nir_metadata_preserve() if they don't actually make any changes at all.
 */
void
nir_metadata_check_validation_flag(nir_shader *shader)
{
   nir_foreach_function(function, shader) {
      if (function->impl) {
         assert(!(function->impl->valid_metadata &
                  nir_metadata_not_properly_reset));
      }
   }
}
#endif

struct wrapper_state {
   const nir_pass *pass;
   void *data;
};

static bool
filter_func_wrapper(const nir_instr *instr, const void *_state)
{
   const struct wrapper_state *state = _state;
   const nir_ssa_def *def = nir_instr_ssa_def((nir_instr *)instr);
   if (def == NULL)
      return false;

   return state->pass->ssa_def_filter_func(def, state->data);
}

static nir_ssa_def *
pass_func_wrapper(nir_builder *b, nir_instr *instr, void *_state)

{
   struct wrapper_state *state = _state;
   nir_ssa_def *def = nir_instr_ssa_def(instr);
   return state->pass->ssa_def_pass_func(b, def, state->data);
}

static bool
function_impl_run_pass(nir_function_impl *impl,
                       const nir_pass *pass, void *data, void *mem_ctx)
{
   bool progress;
   if (pass->impl_pass_func) {
      progress = pass->impl_pass_func(impl, data, mem_ctx);
   } else if (pass->block_pass_func) {
      progress = false;
      nir_builder build;
      nir_builder_init(&build, impl);
      nir_foreach_block_safe(block, impl) {
         build.cursor = nir_before_block(block);
         if (pass->block_pass_func(&build, block, data, mem_ctx))
            progress = true;
      }
   } else if (pass->instr_pass_func) {
      progress = false;
      nir_builder build;
      nir_builder_init(&build, impl);
      nir_foreach_block(block, impl) {
         nir_foreach_instr_safe(instr, block) {
            build.cursor = nir_before_instr(instr);
            if (pass->instr_pass_func(&build, instr, data, mem_ctx))
               progress = true;
         }
      }
   } else {
      struct wrapper_state wrapper_state = {
         .pass = pass,
         .data = data,
      };
      progress = nir_function_impl_lower_instructions(impl,
                                                      filter_func_wrapper,
                                                      pass_func_wrapper,
                                                      &wrapper_state);
   }

   if (progress) {
      nir_metadata_preserve(impl, pass->metadata_preserved);
   } else {
#ifndef NDEBUG
      impl->valid_metadata &= ~nir_metadata_not_properly_reset;
#endif
   }

   return progress;
}

bool
nir_function_impl_run_pass(nir_function_impl *impl,
                           const nir_pass *pass, void *data)
{
   void *mem_ctx = ralloc_context(NULL);
   bool progress = function_impl_run_pass(impl, pass, data, mem_ctx);
   ralloc_free(mem_ctx);
   return progress;
}

bool
nir_shader_run_pass(nir_shader *shader, const nir_pass *pass, void *data)
{
   void *mem_ctx = ralloc_context(NULL);

   bool progress;
   if (pass->shader_pass_func) {
      progress = pass->shader_pass_func(shader, data, mem_ctx);

      nir_foreach_function(func, shader) {
         if (!func->impl)
            continue;

         if (progress) {
            nir_metadata_preserve(func->impl, pass->metadata_preserved);
         } else {
#ifndef NDEBUG
            func->impl->valid_metadata &= ~nir_metadata_not_properly_reset;
#endif
         }
      }

   } else {
      progress = false;
      nir_foreach_function(func, shader) {
         if (func->impl != NULL &&
             function_impl_run_pass(func->impl, pass, data, mem_ctx))
            progress = true;
      }
   }

   ralloc_free(mem_ctx);

   return progress;
}
