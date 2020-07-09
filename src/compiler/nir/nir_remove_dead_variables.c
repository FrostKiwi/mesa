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
 *    Connor Abbott (cwabbott0@gmail.com)
 *
 */

#include "nir.h"

static bool
deref_used_for_not_store(nir_deref_instr *deref)
{
   nir_foreach_use(src, &deref->dest.ssa) {
      switch (src->parent_instr->type) {
      case nir_instr_type_deref:
         if (deref_used_for_not_store(nir_instr_as_deref(src->parent_instr)))
            return true;
         break;

      case nir_instr_type_intrinsic: {
         nir_intrinsic_instr *intrin =
            nir_instr_as_intrinsic(src->parent_instr);
         /* The first source of copy and store intrinsics is the deref to
          * write.  Don't record those.
          */
         if ((intrin->intrinsic != nir_intrinsic_store_deref &&
              intrin->intrinsic != nir_intrinsic_copy_deref) ||
             src != &intrin->src[0])
            return true;
         break;
      }

      default:
         /* If it's used by any other instruction type (most likely a texture
          * or call instruction), consider it used.
          */
         return true;
      }
   }

   return false;
}

static void
add_var_use_deref(nir_deref_instr *deref, struct set *live)
{
   if (deref->deref_type != nir_deref_type_var)
      return;

   /* If it's not a local that never escapes the shader, then any access at
    * all means we need to keep it alive.
    */
   assert(deref->mode == deref->var->data.mode);
   if (!(deref->mode & (nir_var_function_temp | nir_var_shader_temp | nir_var_mem_shared)) ||
       deref_used_for_not_store(deref))
      _mesa_set_add(live, deref->var);
}

static void
add_var_use_texture_sampler(nir_tex_instr *tex, struct set *samplers, struct set *live)
{
   /* derefs are already handled, and we don't care about bindless textures */
   if (nir_tex_instr_src_index(tex, nir_tex_src_sampler_deref) != -1 ||
       nir_tex_instr_src_index(tex, nir_tex_src_sampler_handle) != -1)
      return;

   /* since we have to check the range of arrays of samplers we can't use
    * set_search here */
   set_foreach(samplers, entry) {
      nir_variable *var = (nir_variable *)entry->key;
      unsigned range = (glsl_type_is_array(var->type)) ? glsl_get_aoa_size(var->type) : 1;
      if (tex->sampler_index >= var->data.binding &&
          tex->sampler_index < var->data.binding  + range)  {
         _mesa_set_add(live, entry->key);
         /* sampler is live, no need to check again */
         _mesa_set_remove(samplers, entry);
         break;
      }
   }
}

static void
add_var_use_shader(nir_shader *shader, struct set *live, nir_variable_mode modes)
{
   struct set *samplers = NULL;

   if (modes & nir_var_uniform) {
      samplers = _mesa_pointer_set_create(NULL);
      nir_foreach_variable(var, &shader->uniforms) {
         const struct glsl_type *type = glsl_without_array(var->type);
         if (glsl_type_is_sampler(type))
            _mesa_set_add(samplers, var);
      }
   }

   nir_foreach_function(function, shader) {
      if (function->impl) {
         nir_foreach_block(block, function->impl) {
            nir_foreach_instr(instr, block) {
               if (instr->type == nir_instr_type_deref)
                  add_var_use_deref(nir_instr_as_deref(instr), live);
               if (modes & nir_var_uniform &&
                   instr->type == nir_instr_type_tex &&
                   samplers->size > 0)
                  add_var_use_texture_sampler(nir_instr_as_tex(instr), samplers, live);
            }
         }
      }
   }

   _mesa_set_destroy(samplers, NULL);
}

static void
remove_dead_var_writes(nir_shader *shader, struct set *live)
{
   nir_foreach_function(function, shader) {
      if (!function->impl)
         continue;

      nir_foreach_block(block, function->impl) {
         nir_foreach_instr_safe(instr, block) {
            switch (instr->type) {
            case nir_instr_type_deref: {
               nir_deref_instr *deref = nir_instr_as_deref(instr);
               if (deref->deref_type == nir_deref_type_cast &&
                   !nir_deref_instr_parent(deref))
                  continue;

               nir_variable_mode parent_mode;
               if (deref->deref_type == nir_deref_type_var)
                  parent_mode = deref->var->data.mode;
               else
                  parent_mode = nir_deref_instr_parent(deref)->mode;

               /* If the parent mode is 0, then it references a dead variable.
                * Flag this deref as dead and remove it.
                */
               if (parent_mode == 0) {
                  deref->mode = 0;
                  nir_instr_remove(&deref->instr);
               }
               break;
            }

            case nir_instr_type_intrinsic: {
               nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
               if (intrin->intrinsic != nir_intrinsic_copy_deref &&
                   intrin->intrinsic != nir_intrinsic_store_deref)
                  break;

               if (nir_src_as_deref(intrin->src[0])->mode == 0)
                  nir_instr_remove(instr);
               break;
            }

            default:
               break; /* Nothing to do */
            }
         }
      }
   }
}

static bool
remove_dead_vars(struct exec_list *var_list, struct set *live, nir_variable_mode mode)
{
   bool progress = false;

   foreach_list_typed_safe(nir_variable, var, node, var_list) {
      if (var->data.mode != mode)
         continue;

      struct set_entry *entry = _mesa_set_search(live, var);
      if (entry == NULL) {
         /* Mark this variable as used by setting the mode to 0 */
         var->data.mode = 0;
         exec_node_remove(&var->node);
         progress = true;
      }
   }

   return progress;
}

bool
nir_remove_dead_variables(nir_shader *shader, nir_variable_mode modes)
{
   bool progress = false;
   struct set *live = _mesa_pointer_set_create(NULL);

   add_var_use_shader(shader, live, modes);

   if (modes & nir_var_uniform)
      progress = remove_dead_vars(&shader->uniforms, live, nir_var_uniform) || progress;

   if (modes & nir_var_mem_ubo)
      progress = remove_dead_vars(&shader->uniforms, live, nir_var_mem_ubo) || progress;

   if (modes & nir_var_shader_in)
      progress = remove_dead_vars(&shader->inputs, live, nir_var_shader_in) || progress;

   if (modes & nir_var_shader_out)
      progress = remove_dead_vars(&shader->outputs, live, nir_var_shader_out) || progress;

   if (modes & nir_var_shader_temp)
      progress = remove_dead_vars(&shader->globals, live, nir_var_shader_temp) || progress;

   if (modes & nir_var_system_value)
      progress = remove_dead_vars(&shader->system_values, live, nir_var_system_value) || progress;

   if (modes & nir_var_mem_shared)
      progress = remove_dead_vars(&shader->shared, live, nir_var_mem_shared) || progress;

   if (modes & nir_var_function_temp) {
      nir_foreach_function(function, shader) {
         if (function->impl) {
            if (remove_dead_vars(&function->impl->locals, live, nir_var_function_temp))
               progress = true;
         }
      }
   }

   if (progress) {
      remove_dead_var_writes(shader, live);

      nir_foreach_function(function, shader) {
         if (function->impl) {
            nir_metadata_preserve(function->impl, nir_metadata_block_index |
                                                  nir_metadata_dominance);
         }
      }
   }

   _mesa_set_destroy(live, NULL);
   return progress;
}
