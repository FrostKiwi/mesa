/*
 * Copyright (c) 2020 Collabora Ltd.
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

#include "dxil_enums.h"
#include "dxil_module.h"
#include "dxil_signature.h"

#include "glsl_types.h"
#include "util/u_debug.h"

#include <string.h>

/*
 * The signatures are written into the stream in two pieces:
 * DxilProgramSignatureElement is a fixes size structure that gets dumped
 * to the stream in order of the registers and each contains an offset
 * to the semantic name string. Then these strings are dumped into the stream.
 */

static enum dxil_semantic_kind
get_semantic_vs_in_name(nir_variable *var, char buffer[64])
{
   enum dxil_semantic_kind kind = DXIL_SEM_INVALID;
   const char *name = "UNDEFINED";
   switch (var->data.location) {
   case VERT_ATTRIB_POS:
      /* The position is not handled as system value here */
      name = "POSITION";
      kind = DXIL_SEM_ARBITRARY;
      break;
   case VERT_ATTRIB_NORMAL:
      name = "NORMAL";
      kind = DXIL_SEM_ARBITRARY;
      break;
   case VERT_ATTRIB_COLOR0:
      name = "COLOR0";
      kind = DXIL_SEM_ARBITRARY;
      break;
   case VERT_ATTRIB_COLOR1:
      name = "COLOR1";
      kind = DXIL_SEM_ARBITRARY;
      break;
   default:
      if( var->data.location >= VERT_ATTRIB_GENERIC0 &&
          var->data.location <= VERT_ATTRIB_GENERIC15) {
         name = "GENERIC";
         kind = DXIL_SEM_ARBITRARY;
      }
   }

   snprintf(buffer, 64, "%s", name);
   return kind;
}

static enum dxil_semantic_kind
get_semantic_ps_outname(nir_variable *var, char buffer[64])
{
   enum dxil_semantic_kind kind = DXIL_SEM_INVALID;
   switch (var->data.location) {
   case FRAG_RESULT_COLOR:
   case FRAG_RESULT_DATA0:
   case FRAG_RESULT_DATA1:
   case FRAG_RESULT_DATA2:
   case FRAG_RESULT_DATA3:
   case FRAG_RESULT_DATA4:
   case FRAG_RESULT_DATA5:
   case FRAG_RESULT_DATA6:
   case FRAG_RESULT_DATA7:
      snprintf(buffer, 64, "%s", "SV_Target");
      kind = DXIL_SEM_TARGET;
      break;
   case FRAG_RESULT_DEPTH:
      snprintf(buffer, 64, "%s", "DEPTH");
      kind = DXIL_SEM_DEPTH;
      break;
   case FRAG_RESULT_STENCIL:
      snprintf(buffer, 64, "%s", "STENCIL");
      kind = DXIL_SEM_STENCIL_REF; //??
      break;
   case FRAG_RESULT_SAMPLE_MASK:
      snprintf(buffer, 64, "%s", "SAMPLE_MASK");
      kind = DXIL_SEM_COVERAGE; //??
      break;
   default:
      snprintf(buffer, 64, "%s", "UNDEFINED");
      break;
   }
   return kind;
}

static enum dxil_semantic_kind
get_semantic_name(nir_variable *var, char buffer[64])
{
   enum dxil_semantic_kind kind = DXIL_SEM_INVALID;
   if (var->data.location == VARYING_SLOT_POS) {
      assert(glsl_get_components(var->type) == 4);
      snprintf(buffer, 64, "%s", "SV_Position");
      kind = DXIL_SEM_POSITION;
   } else if (var->data.location >= VARYING_SLOT_VAR0 &&
              var->data.location <= VARYING_SLOT_VAR31) {
      snprintf(buffer, 64, "%s%d", "VARYING", var->data.location - VARYING_SLOT_VAR0);
      kind = DXIL_SEM_ARBITRARY;
   } else {
      snprintf(buffer, 64, "%s%d", "OTHER", var->data.location - VARYING_SLOT_POS);
   }
   return kind;
}

static uint8_t
get_interpolation(unsigned mode)
{
   switch (mode) {
   case INTERP_MODE_NONE: return DXIL_INTERP_LINEAR;
   case INTERP_MODE_FLAT: return DXIL_INTERP_CONSTANT;
   case INTERP_MODE_NOPERSPECTIVE: return DXIL_INTERP_LINEAR_NOPERSPECTIVE;
   case INTERP_MODE_SMOOTH: return DXIL_INTERP_LINEAR;
   }
   return DXIL_INTERP_LINEAR;
}

static const struct dxil_mdnode *
fill_SV_param_nodes(struct dxil_module *mod, unsigned record_id, char *semantic_name,
                    enum dxil_semantic_kind semantic_kind, uint8_t columns,
                    uint8_t interpolation)
{

   const struct dxil_mdnode *SV_params_nodes[11];
   /* For this to always work we should use vectorize_io, but for FS out and VS in
    * this is not implemented globally */
   const struct dxil_mdnode *flattened_semantics[1] = {
      dxil_get_metadata_int32(mod, 0)
   };

   SV_params_nodes[0] = dxil_get_metadata_int32(mod, (int)record_id);
   SV_params_nodes[1] = dxil_get_metadata_string(mod, semantic_name);
   SV_params_nodes[2] = dxil_get_metadata_int8(mod, 9); // component name (enum)
   SV_params_nodes[3] = dxil_get_metadata_int8(mod, (int8_t)semantic_kind); // semantic kind
   SV_params_nodes[4] = dxil_get_metadata_node(mod, flattened_semantics,
                                         ARRAY_SIZE(flattened_semantics)); // metadata enumerating semantic indices of flattened param
   SV_params_nodes[5] = dxil_get_metadata_int8(mod, (int8_t)interpolation); // interpolation mode

   /* Below values are set for */
   SV_params_nodes[6] = dxil_get_metadata_int32(mod, 1); // num elms per row
   SV_params_nodes[7] = dxil_get_metadata_int8(mod, (int8_t)columns); // num elms per column
   SV_params_nodes[8] = dxil_get_metadata_int32(mod, 0); // start row of elm packing location
   SV_params_nodes[9] = dxil_get_metadata_int8(mod, 0); // start column of elm packing location
   SV_params_nodes[10] = 0; // optional Metadata

   return dxil_get_metadata_node(mod, SV_params_nodes, ARRAY_SIZE(SV_params_nodes));
}

static const struct dxil_mdnode *
get_input_signature(struct dxil_module *mod, nir_shader *s)
{
   unsigned record_id = 0;
   unsigned num_inputs = exec_list_length(&s->inputs);
   if (num_inputs <= 0)
      return NULL;

   struct dxil_mdnode **inputs = malloc(sizeof(const struct dxil_mdnode *) * num_inputs);

   /* For this to always work we should use vectorize_io, but for FS out and VS in
    * this is not implemented globally */
   const struct dxil_mdnode *flattened_semantics[1] = {
      dxil_get_metadata_int32(mod, 0)
   };

   nir_foreach_variable(var, &s->inputs) {
      char semantic_name[64] = "";
       enum dxil_semantic_kind semantic_kind;
      uint8_t interpolation = 0;
      if (s->info.stage == MESA_SHADER_VERTEX)
         semantic_kind = get_semantic_vs_in_name(var, semantic_name);
      else {
         semantic_kind = get_semantic_name(var, semantic_name);
         interpolation = get_interpolation(var->data.interpolation);
      }
      uint8_t columns = (uint8_t)glsl_get_components(var->type);
      inputs[record_id] = fill_SV_param_nodes(mod, record_id, semantic_name,
                                              semantic_kind, columns, interpolation);
      ++record_id;
   }
   assert(record_id == num_inputs);
   mod->num_sig_inputs = num_inputs;
   const struct dxil_mdnode *retval = dxil_get_metadata_node(mod, inputs, num_inputs);
   free(inputs);
   return retval;
}

static const struct dxil_mdnode *
get_output_signature(struct dxil_module *mod, nir_shader *s)
{
   unsigned record_id = 0;
   unsigned num_outputs = exec_list_length(&s->outputs);
   if (num_outputs <= 0)
      return NULL;

   struct dxil_mdnode **outputs = malloc(sizeof(const struct dxil_mdnode *) * num_outputs);


   nir_foreach_variable(var, &s->outputs) {
      enum dxil_semantic_kind semantic_kind;
      char semantic_name[64] = "";
      uint8_t interpolation = 0;
      uint8_t columns = (uint8_t)glsl_get_components(var->type);

      if (s->info.stage == MESA_SHADER_FRAGMENT) {
         semantic_kind = get_semantic_ps_outname(var, semantic_name);
      } else {
         semantic_kind = get_semantic_name(var, semantic_name);
         interpolation = get_interpolation(var->data.interpolation);
      }

      outputs[record_id] = fill_SV_param_nodes(mod, record_id, semantic_name,
                                               semantic_kind, columns, interpolation);
      ++record_id;
   }
   assert(record_id == num_outputs);

   const struct dxil_mdnode *retval = dxil_get_metadata_node(mod, outputs, num_outputs);
   free(outputs);
   mod->num_sig_outputs = num_outputs;
   return retval;
}

const struct dxil_mdnode *
get_signatures(struct dxil_module *mod, nir_shader *s)
{
   const struct dxil_mdnode *input_signature = get_input_signature(mod, s);
   const struct dxil_mdnode *output_signature = get_output_signature(mod, s);

   const struct dxil_mdnode *SV_nodes[3] = {
      input_signature,
      output_signature,
      NULL
   };
   if (output_signature || input_signature)
      return dxil_get_metadata_node(mod, SV_nodes, ARRAY_SIZE(SV_nodes));
   else
      return NULL;
}
