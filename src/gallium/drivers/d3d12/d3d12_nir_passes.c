/*
 * Copyright © Microsoft Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "d3d12_nir_passes.h"
#include "d3d12_compiler.h"
#include "nir_builder.h"
#include "nir_builtin_builder.h"
#include "program/prog_instruction.h"

/**
 * Lower Y Flip:
 *
 * We can't do a Y flip simply by negating the viewport height,
 * so we need to lower the flip into the NIR shader.
 */

static nir_ssa_def *
get_flip(nir_builder *b, nir_variable **flip)
{
   const gl_state_index16 tokens[5] = { STATE_INTERNAL, STATE_INTERNAL_DRIVER, D3D12_STATE_VAR_Y_FLIP };
   if (*flip == NULL) {
      nir_variable *var = nir_variable_create(b->shader,
                                              nir_var_uniform,
                                              glsl_float_type(),
                                              "d3d12_FlipY");

      var->num_state_slots = 1;
      var->state_slots = ralloc_array(var, nir_state_slot, 1);
      memcpy(var->state_slots[0].tokens, tokens,
             sizeof(var->state_slots[0].tokens));
      var->data.how_declared = nir_var_hidden;
      b->shader->num_uniforms++;
      *flip = var;
   }
   return nir_load_var(b, *flip);
}

static void
lower_pos_write(nir_builder *b, struct nir_instr *instr, nir_variable **flip)
{
   if (instr->type != nir_instr_type_intrinsic)
      return;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_store_deref)
      return;

   nir_variable *var = nir_intrinsic_get_var(intr, 0);
   if (var->data.mode != nir_var_shader_out ||
       var->data.location != VARYING_SLOT_POS)
      return;

   b->cursor = nir_before_instr(&intr->instr);

   nir_ssa_def *pos = nir_ssa_for_src(b, intr->src[1], 4);
   nir_ssa_def *flip_y = get_flip(b, flip);
   nir_ssa_def *def = nir_vec4(b,
                               nir_channel(b, pos, 0),
                               nir_fmul(b, nir_channel(b, pos, 1), flip_y),
                               nir_channel(b, pos, 2),
                               nir_channel(b, pos, 3));
   nir_instr_rewrite_src(&intr->instr, intr->src + 1, nir_src_for_ssa(def));
}

void
d3d12_lower_yflip(nir_shader *nir)
{
   nir_variable *flip = NULL;

   if (nir->info.stage != MESA_SHADER_VERTEX)
      return;

   nir_foreach_function(function, nir) {
      if (function->impl) {
         nir_builder b;
         nir_builder_init(&b, function->impl);

         nir_foreach_block(block, function->impl) {
            nir_foreach_instr_safe(instr, block) {
               lower_pos_write(&b, instr, &flip);
            }
         }

         nir_metadata_preserve(function->impl, nir_metadata_block_index |
                                               nir_metadata_dominance);
      }
   }
}

/**
 * Lower State Vars:
 *
 * All uniforms related to internal D3D12 variables are
 * condensed into a UBO that is appended at the end of the
 * current ones.
 */

static unsigned
get_state_var_offset(struct d3d12_shader *shader, enum d3d12_state_var var)
{
   for (unsigned i = 0; i < shader->num_state_vars; ++i) {
      if (shader->state_vars[i].var == var)
         return shader->state_vars[i].offset;
   }

   unsigned offset = shader->state_vars_size;
   shader->state_vars[shader->num_state_vars].offset = offset;
   shader->state_vars[shader->num_state_vars].var = var;
   shader->state_vars_size += 4; /* Use 4-words slots no matter the variable size */
   shader->num_state_vars++;

   return offset;
}

static bool
lower_instr(nir_intrinsic_instr *instr, nir_builder *b,
            struct d3d12_shader *shader, unsigned binding)
{
   nir_variable *variable = NULL;
   nir_deref_instr *deref = NULL;

   b->cursor = nir_before_instr(&instr->instr);

   if (instr->intrinsic == nir_intrinsic_load_uniform) {
      nir_foreach_variable(var, &b->shader->uniforms) {
         if (var->data.driver_location == nir_intrinsic_base(instr)) {
            variable = var;
            break;
         }
      }
   } else if (instr->intrinsic == nir_intrinsic_load_deref) {
      deref = nir_src_as_deref(instr->src[0]);
      variable = nir_intrinsic_get_var(instr, 0);
   }

   if (variable == NULL ||
       variable->num_state_slots != 1 ||
       variable->state_slots[0].tokens[1] != STATE_INTERNAL_DRIVER)
      return false;

   enum d3d12_state_var var = variable->state_slots[0].tokens[2];
   nir_ssa_def *ubo_idx = nir_imm_int(b, binding);
   nir_ssa_def *ubo_offset =  nir_imm_int(b, get_state_var_offset(shader, var));
   nir_intrinsic_instr *load =
      nir_intrinsic_instr_create(b->shader, nir_intrinsic_load_ubo);
   load->num_components = instr->num_components;
   load->src[0] = nir_src_for_ssa(ubo_idx);
   load->src[1] = nir_src_for_ssa(ubo_offset);
   assert(instr->dest.ssa.bit_size >= 8);
   nir_intrinsic_set_align(load, instr->dest.ssa.bit_size / 8, 0);
   nir_ssa_dest_init(&load->instr, &load->dest,
                     load->num_components, instr->dest.ssa.bit_size,
                     instr->dest.ssa.name);
   nir_builder_instr_insert(b, &load->instr);
   nir_ssa_def_rewrite_uses(&instr->dest.ssa, nir_src_for_ssa(&load->dest.ssa));

   /* Remove the old load_* instruction and any parent derefs */
   nir_instr_remove(&instr->instr);
   for (nir_deref_instr *d = deref; d; d = nir_deref_instr_parent(d)) {
      /* If anyone is using this deref, leave it alone */
      assert(d->dest.is_ssa);
      if (!list_is_empty(&d->dest.ssa.uses))
         break;

      nir_instr_remove(&d->instr);
   }

   return true;
}

bool
d3d12_lower_state_vars(nir_shader *nir, struct d3d12_shader *shader)
{
   bool progress = false;

   /* The state var UBO is added after all the other UBOs if it already
    * exists it will be replaced by using the same binding */
   unsigned binding = nir->info.num_ubos;
   nir_foreach_variable_safe(var, &nir->uniforms) {
      if (var->num_state_slots == 1 &&
          var->state_slots[0].tokens[1] == STATE_INTERNAL_DRIVER) {
         if (var->data.mode == nir_var_mem_ubo) {
            binding = var->data.binding;
         }
      }
   }

   nir_foreach_function(function, nir) {
      if (function->impl) {
         nir_builder builder;
         nir_builder_init(&builder, function->impl);
         nir_foreach_block(block, function->impl) {
            nir_foreach_instr_safe(instr, block) {
               if (instr->type == nir_instr_type_intrinsic)
                  progress |= lower_instr(nir_instr_as_intrinsic(instr),
                                          &builder,
                                          shader,
                                          binding);
            }
         }

         nir_metadata_preserve(function->impl, nir_metadata_block_index |
                                               nir_metadata_dominance);
      }
   }

   if (progress) {
      assert(shader->num_state_vars > 0);

      /* Remove state variables */
      nir_foreach_variable_safe(var, &nir->uniforms) {
         if (var->num_state_slots == 1 &&
             var->state_slots[0].tokens[1] == STATE_INTERNAL_DRIVER) {
            exec_node_remove(&var->node);
            nir->num_uniforms--;
         }
      }

      const gl_state_index16 tokens[5] = { STATE_INTERNAL, STATE_INTERNAL_DRIVER };
      const struct glsl_type *type = glsl_array_type(glsl_vec4_type(),
                                                     shader->state_vars_size / 4, 0);
      nir_variable *ubo = nir_variable_create(nir, nir_var_mem_ubo, type,
                                                  "d3d12_state_vars");
      if (binding == nir->info.num_ubos)
         nir->info.num_ubos++;
      ubo->data.binding = binding;
      ubo->num_state_slots = 1;
      ubo->state_slots = ralloc_array(ubo, nir_state_slot, 1);
      memcpy(ubo->state_slots[0].tokens, tokens,
              sizeof(ubo->state_slots[0].tokens));

      struct glsl_struct_field field = {
          .type = type,
          .name = "data",
          .location = -1,
      };
      ubo->interface_type =
              glsl_interface_type(&field, 1, GLSL_INTERFACE_PACKING_STD430,
                                  false, "__d3d12_state_vars_interface");
   }

   return progress;
}

static const struct glsl_type *
get_bare_samplers_for_type(const struct glsl_type *type)
{
   if (glsl_type_is_sampler(type)) {
      if (glsl_sampler_type_is_shadow(type))
         return glsl_bare_shadow_sampler_type();
      else
         return glsl_bare_sampler_type();
   } else if (glsl_type_is_array(type)) {
      return glsl_array_type(
         get_bare_samplers_for_type(glsl_get_array_element(type)),
         glsl_get_length(type),
         0 /*explicit size*/);
   }
   assert(!"Unexpected type");
   return NULL;
}

void
d3d12_create_bare_samplers(nir_shader *nir)
{
   nir_foreach_variable_safe(var, &nir->uniforms) {
      const struct glsl_type *type = glsl_without_array(var->type);
      if (glsl_type_is_sampler(type) && glsl_get_sampler_result_type(type) != GLSL_TYPE_VOID) {
         /* Since samplers are already lowered to be accessed by index, all we need to do
         /* here is create a bare sampler with the same binding */
         nir_variable *clone = nir_variable_clone(var, nir);
         clone->type = get_bare_samplers_for_type(var->type);
         nir_shader_add_variable(nir, clone);
      }
   }
}

bool
lower_bool_input_filter(const nir_instr *instr,
                        UNUSED const void *_options)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic == nir_intrinsic_load_front_face)
      return true;

   if (intr->intrinsic == nir_intrinsic_load_deref) {
      nir_deref_instr *deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
      nir_variable *var = nir_deref_instr_get_variable(deref);
      return var->data.mode == nir_var_shader_in &&
             glsl_get_base_type(var->type) == GLSL_TYPE_BOOL;
   }

   return false;
}

static nir_ssa_def *
lower_bool_input_impl(nir_builder *b, nir_instr *instr,
                      UNUSED void *_options)
{
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   if (intr->intrinsic == nir_intrinsic_load_deref) {
      nir_deref_instr *deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
      nir_variable *var = nir_deref_instr_get_variable(deref);

      /* rewrite var->type */
      var->type = glsl_vector_type(GLSL_TYPE_INT,
                                   glsl_get_vector_elements(var->type));
      deref->type = var->type;
   }

   intr->dest.ssa.bit_size = 32;
   return nir_i2b1(b, &intr->dest.ssa);
}

bool
d3d12_lower_bool_input(struct nir_shader *s)
{
   return nir_shader_lower_instructions(s, lower_bool_input_filter,
                                        lower_bool_input_impl, NULL);
}

static bool
lower_color_write(nir_builder *b, struct nir_instr *instr, unsigned nr_cbufs)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_store_deref)
      return false;

   nir_deref_instr *deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
   nir_variable *var = nir_deref_instr_get_variable(deref);

   if (var->data.mode != nir_var_shader_out ||
       var->data.location != FRAG_RESULT_COLOR)
      return false;

   /* lower the original write to data #0 */
   var->name = ralloc_strdup(var, "gl_FragData[0]");
   var->data.location = FRAG_RESULT_DATA0;
   var->data.driver_location = 0;

   b->cursor = nir_after_instr(&intr->instr);

   /* Then create new variables and write them as well */
   nir_ssa_def *value = nir_ssa_for_src(b, intr->src[1],
                                        nir_src_num_components(intr->src[1]));
   unsigned writemask = nir_intrinsic_write_mask(intr);
   for (int i = 1; i < nr_cbufs; ++i) {
      char name[256];
      snprintf(name, sizeof(name), "gl_FragData[%d]", i);
      nir_variable *new_var = nir_variable_create(b->shader,
                                                  nir_var_shader_out,
                                                  var->type, name);
      new_var->data.location = FRAG_RESULT_DATA0 + i;
      new_var->data.driver_location = i;
      nir_store_var(b, new_var, value, writemask);
   }

   return true;
}

bool
d3d12_lower_frag_result(struct nir_shader *nir, unsigned nr_cbufs)
{
   bool progress = false;
   if (nir->info.stage != MESA_SHADER_FRAGMENT)
      return false;

   nir_foreach_function(function, nir) {
      if (function->impl) {
         nir_builder b;
         nir_builder_init(&b, function->impl);

         nir_foreach_block(block, function->impl) {
            nir_foreach_instr_safe(instr, block) {
               progress |= lower_color_write(&b, instr, nr_cbufs);
            }
         }

         nir_metadata_preserve(function->impl, nir_metadata_block_index |
                                               nir_metadata_dominance);
      }
   }
   return progress;
}

bool
d3d12_fix_stencil_export_type(struct nir_shader *s)
{
   if (!(s->info.outputs_written & (1 << FRAG_RESULT_STENCIL)))
      return false;

   nir_variable *stencil_export = NULL;
   nir_foreach_variable(var, &s->outputs) {
      if (var->data.location == FRAG_RESULT_STENCIL) {
         var->type = glsl_uint_type();
         stencil_export = var;
         break;
      }
   }

   assert(stencil_export);

   nir_foreach_function(function, s) {
      if (function->impl) {
         nir_foreach_block(block, function->impl) {
            nir_foreach_instr_safe(instr, block) {
               if (instr->type == nir_instr_type_deref) {
                  nir_deref_instr *deref = nir_instr_as_deref(instr);
                  if (deref->var == stencil_export)
                     deref->type = stencil_export->type;
               }
            }
         }
      }
   }

   return true;
}
