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
lower_clc_deref_instr(nir_instr *instr, nir_builder *b,
                      const nir_shader *clc_shader,
                      struct hash_table *copy_vars)
{
   nir_deref_instr *deref = nir_instr_as_deref(instr);
   if (deref->deref_type != nir_deref_type_var ||
       !nir_variable_is_global(deref->var))
      return false;

   const struct exec_list *list = &clc_shader->uniforms;
   if (deref->mode == nir_var_system_value)
      list = &clc_shader->system_values;

   foreach_list_typed(nir_variable, clc_var, node, list) {
      if (clc_var != deref->var)
         continue;

      nir_variable *user_var;
      struct hash_entry *entry = _mesa_hash_table_search(copy_vars, deref->var);
      if (entry) {
         user_var = entry->data;
      } else {
         user_var = nir_variable_clone(deref->var, b->shader);
         nir_shader_add_variable(b->shader, user_var);
         _mesa_hash_table_insert(copy_vars, deref->var, user_var);
      }

      b->cursor = nir_before_instr(instr);
      nir_deref_instr *new = nir_build_deref_var(b, user_var);
      nir_ssa_def_rewrite_uses(&deref->dest.ssa, nir_src_for_ssa(&new->dest.ssa));
      nir_instr_remove(instr);
      return true;
   }
   return false;
}

static bool
lower_clc_call_instr(nir_instr *instr, nir_builder *b,
                     const nir_shader *clc_shader)
{
   nir_call_instr *call = nir_instr_as_call(instr);
   nir_function *func = NULL;
   nir_foreach_function(function, clc_shader) {
   if (strcmp(function->name, call->callee->name) == 0) {
      func = function;
      break;
     }
   }
   if (!func || !func->impl) {
     return false;
   }

   nir_ssa_def *params[32] = { NULL, };

   for (unsigned i = 0; i < call->num_params; i++) {
      params[i] = nir_ssa_for_src(b, call->params[i],
                                  call->callee->params[i].num_components);
   }

   b->cursor = nir_instr_remove(&call->instr);
   nir_inline_function_impl(b, func->impl, params);

   return true;
}

static bool
nir_lower_libclc_impl(nir_function_impl *impl,
                      const nir_shader *clc_shader,
                      struct hash_table *copy_vars)
{
   nir_builder b;
   nir_builder_init(&b, impl);

   bool progress = false;
   nir_foreach_block_safe(block, impl) {
      nir_foreach_instr_safe(instr, block) {
         if (instr->type == nir_instr_type_call) {
            progress |= lower_clc_call_instr(instr, &b, clc_shader);
         } else if (instr->type == nir_instr_type_deref) {
            progress |= lower_clc_deref_instr(instr, &b, clc_shader, copy_vars);
         }
      }
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
   void *ra_ctx = ralloc_context(NULL);
   struct hash_table *copy_vars = _mesa_pointer_hash_table_create(ra_ctx);
   bool progress = false;

   /* do progress passes inside the pass */
   do {
     progress = false;
     nir_foreach_function(function, shader) {
       if (function->impl)
         progress |= nir_lower_libclc_impl(function->impl, clc_shader,
                                           copy_vars);
     }
   } while(progress);

   ralloc_free(ra_ctx);

   return progress;
}
