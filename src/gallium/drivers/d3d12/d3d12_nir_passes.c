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
#include "nir_builder.h"

bool lower_bool_loads_filter(const nir_instr *instr,
                                  UNUSED const void *_options)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *op = nir_instr_as_intrinsic(instr);
   if (!nir_intrinsic_infos[op->intrinsic].has_dest)
      return false;

   assert(op->dest.is_ssa);

   if (op->intrinsic == nir_intrinsic_load_front_face &&
       op->dest.ssa.bit_size == 1)
      return true;

   if (op->intrinsic == nir_intrinsic_load_deref) {
      nir_deref_instr *deref = nir_instr_as_deref(op->src[0].ssa->parent_instr);
      if (deref->deref_type == nir_deref_type_var) {
         nir_variable *var = nir_deref_instr_get_variable(deref);
         enum glsl_base_type type = glsl_get_base_type(var->type);
         return type == GLSL_TYPE_BOOL;
      }
   }
   return false;
}

static nir_ssa_def *
lower_bool_loads_impl(nir_builder *b, nir_instr *instr,
                           UNUSED void *_options)
{
   assert(instr->type == nir_instr_type_intrinsic);
   nir_intrinsic_instr *load = nir_instr_as_intrinsic(instr);

   switch (load->intrinsic) {
   case nir_intrinsic_load_front_face:
      load->dest.ssa.bit_size = 32;
      b->cursor = nir_after_instr(instr);
      break;
   case nir_intrinsic_load_deref: {
         nir_deref_instr *deref = nir_instr_as_deref(load->src[0].ssa->parent_instr);
         nir_variable *var = nir_deref_instr_get_variable(deref);
         assert(glsl_type_is_scalar(var->type));
         deref->type = var->type = glsl_uint_type();
         load->dest.ssa.bit_size = 32;
         b->cursor = nir_after_instr(instr);
         break;
      }
   default:
      unreachable("This intrinsic is not lowered");
   }

   return nir_ine(b, &load->dest.ssa, nir_imm_int(b, 0));
}

bool
d3d12_lower_bool_loads(struct nir_shader *s)
{
   return nir_shader_lower_instructions(s,
                                        lower_bool_loads_filter,
                                        lower_bool_loads_impl,
                                        NULL);
}
