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

#include "nir_to_dxil.h"
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
   const char *name = dxil_vs_attr_index_to_name(var->data.driver_location);
   assert(strlen(name) < 63);
   strcpy(buffer, name);
   return DXIL_SEM_ARBITRARY;
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
      assert((var->data.location == FRAG_RESULT_COLOR ||
              var->data.location == FRAG_RESULT_DATA0) &&
             "time to implement semantic indices for PS outputs");
      break;
   case FRAG_RESULT_DEPTH:
      snprintf(buffer, 64, "%s", "SV_Depth");
      kind = DXIL_SEM_DEPTH;
      break;
   case FRAG_RESULT_STENCIL:
      snprintf(buffer, 64, "%s", "SV_StencilRef");
      kind = DXIL_SEM_STENCIL_REF; //??
      break;
   case FRAG_RESULT_SAMPLE_MASK:
      snprintf(buffer, 64, "%s", "SV_Coverage");
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
      return DXIL_SEM_POSITION;
   }

   int index = var->data.location - VARYING_SLOT_POS;
   const char idx1 = 'A' + (char)(index >> 4);
   const char idx2 = 'A' + (char)(index & 0xf);
   snprintf(buffer, 64, "VARYING%c%c", idx1, idx2);
   return  DXIL_SEM_ARBITRARY;
}

static enum dxil_prog_sig_semantic
prog_semantic_from_kind(enum dxil_semantic_kind kind)
{
   switch (kind) {
   case DXIL_SEM_ARBITRARY: return DXIL_PROG_SEM_UNDEFINED;
   case DXIL_SEM_VERTEX_ID: return DXIL_PROG_SEM_VERTEX_ID;
   case DXIL_SEM_INSTANCE_ID: return DXIL_PROG_SEM_INSTANCE_ID;
   case DXIL_SEM_POSITION: return DXIL_PROG_SEM_POSITION;
   case DXIL_SEM_COVERAGE: return DXIL_PROG_SEM_COVERAGE;
   case DXIL_SEM_INNER_COVERAGE: return DXIL_PROG_SEM_INNER_COVERAGE;
   case DXIL_SEM_PRIMITIVE_ID: return DXIL_PROG_SEM_PRIMITIVE_ID;
   case DXIL_SEM_SAMPLE_INDEX: return DXIL_PROG_SEM_SAMPLE_INDEX;
   case DXIL_SEM_IS_FRONT_FACE: return DXIL_PROG_SEM_IS_FRONTFACE;
   case DXIL_SEM_RENDERTARGET_ARRAY_INDEX: return DXIL_PROG_SEM_RENDERTARGET_ARRAY_INDEX;
   case DXIL_SEM_VIEWPORT_ARRAY_INDEX: return DXIL_PROG_SEM_VIEWPORT_ARRAY_INDEX;
   case DXIL_SEM_CLIP_DISTANCE: return DXIL_PROG_SEM_CLIP_DISTANCE;
   case DXIL_SEM_CULL_DISTANCE: return DXIL_PROG_SEM_CULL_DISTANCE;
   case DXIL_SEM_BARYCENTRICS: return DXIL_PROG_SEM_BARYCENTRICS;
   case DXIL_SEM_SHADING_RATE: return DXIL_PROG_SEM_SHADING_RATE;
   case DXIL_SEM_CULL_PRIMITIVE: return DXIL_PROG_SEM_CULL_PRIMITIVE;
   case DXIL_SEM_TARGET: return DXIL_PROG_SEM_TARGET;
   case DXIL_SEM_DEPTH: return DXIL_PROG_SEM_DEPTH;
   case DXIL_SEM_DEPTH_LE: return DXIL_PROG_SEM_DEPTH_LE;
   case DXIL_SEM_DEPTH_GE: return DXIL_PROG_SEM_DEPTH_GE;
   default:
       return DXIL_PROG_SEM_UNDEFINED;
   }
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

static
uint32_t
copy_semantic_name_to_string(struct _mesa_string_buffer *string_out, const char *name)
{
   /*  copy the semantic name */
   uint32_t retval = string_out->length;
   size_t name_len = strlen(name) + 1;
   _mesa_string_buffer_append_len(string_out, name, name_len);
   return retval;
}

static
uint32_t
append_semantic_index_to_table(struct dxil_psv_sem_index_table *table, uint32_t index)
{
   for (unsigned i = 0; i < table->size; ++i) {
      if (table->data[i] == index)
         return i;
   }
   uint32_t retval = table->size;
   assert(table->size < 80);
   table->data[table->size++] = index;
   return retval;
}

static const struct dxil_mdnode *
fill_SV_param_nodes(struct dxil_module *mod, unsigned record_id,
                    char *semantic_name, enum dxil_semantic_kind semantic_kind,
                    uint32_t start_row, uint32_t rows,
                    uint32_t start_col, uint8_t columns,
                    uint8_t interpolation) {

   const struct dxil_mdnode *SV_params_nodes[11];
   /* For this to always work we should use vectorize_io, but for FS out and VS in
    * this is not implemented globally */
   const struct dxil_mdnode *flattened_semantics[1] = {
      dxil_get_metadata_int32(mod, 0)
   };

   SV_params_nodes[0] = dxil_get_metadata_int32(mod, (int)record_id); // Unique element ID
   SV_params_nodes[1] = dxil_get_metadata_string(mod, semantic_name); // Element name
   SV_params_nodes[2] = dxil_get_metadata_int8(mod, 9); // Element type
   SV_params_nodes[3] = dxil_get_metadata_int8(mod, (int8_t)semantic_kind); // Effective system value
   SV_params_nodes[4] = dxil_get_metadata_node(mod, flattened_semantics,
                                         ARRAY_SIZE(flattened_semantics)); // Semantic index vector
   SV_params_nodes[5] = dxil_get_metadata_int8(mod, (int8_t)interpolation); // Interpolation mode
   SV_params_nodes[6] = dxil_get_metadata_int32(mod, rows); // Number of rows
   SV_params_nodes[7] = dxil_get_metadata_int8(mod, columns); // Number of columns
   SV_params_nodes[8] = dxil_get_metadata_int32(mod, start_row); // Element packing start row
   SV_params_nodes[9] = dxil_get_metadata_int8(mod, start_col); // Element packing start column
   SV_params_nodes[10] = 0; // optional Metadata

   return dxil_get_metadata_node(mod, SV_params_nodes, ARRAY_SIZE(SV_params_nodes));
}

static void
fill_signature_element(struct dxil_signature_element *elm,
                       enum dxil_semantic_kind semantic_kind,
                       uint32_t record_id, uint8_t comp_type,
                       uint32_t reg, uint8_t start_col, uint8_t cols)
{
   memset(elm, 0, sizeof(struct dxil_signature_element));
   // elm->stream = 0;
   // elm->semantic_name_offset = 0;  // Offset needs to be filled out when writing
   // elm->semantic_index = 0; // NIR should have packed everything like we need it
   elm->system_value = (uint32_t) prog_semantic_from_kind(semantic_kind);
   elm->comp_type = (uint32_t) comp_type;
   elm->reg = reg;

   assert(cols + start_col <= 4);
   elm->mask = (uint8_t) (((1 << cols) - 1) << start_col);
   // elm->never_writes_mask = 0;
   elm->min_precision = DXIL_MIN_PREC_DEFAULT;
}

static bool
fill_psv_signature_element(struct dxil_psv_signature_element *psv_elm,
                           enum dxil_semantic_kind semantic_kind,
                           uint8_t start_col, uint8_t columns,
                           uint8_t interpolation, uint8_t comp_type,
                           const char *semantic_name, uint8_t start_row,
                           struct dxil_module *mod)
{
   memset(psv_elm, 0, sizeof(struct dxil_psv_signature_element));
   psv_elm->rows = 1;  // var->num_state_slots ?
   psv_elm->start_row = start_row;
   psv_elm->cols_and_start = (1u << 6) | (start_col << 4) | columns;
   psv_elm->semantic_kind = (uint8_t)semantic_kind;
   psv_elm->component_type = comp_type; //`??
   psv_elm->interpolation_mode = interpolation;
   /* to be filled later
     psv_elm->dynamic_mask_and_stream = 0;
     psv_elm->semantic_indexes_offset = 0;
   */
   if (semantic_kind == DXIL_SEM_ARBITRARY && strlen(semantic_name)) {
      psv_elm->semantic_name_offset =
            copy_semantic_name_to_string(mod->sem_string_table, semantic_name);

      /* TODO: clean up memory */
      if (psv_elm->semantic_name_offset == (uint32_t)-1)
         return false;
   }

   append_semantic_index_to_table(&mod->sem_index_table, 0);
   return true;
}

static const char *in_sysvalue_name(nir_variable *var)
{
   switch (var->data.location) {
   case VARYING_SLOT_POS:
      return "POS";
   case VARYING_SLOT_FACE:
      return "FACE";
   default:
      return "NONE";
   }
}

static const struct dxil_mdnode *
get_input_signature(struct dxil_module *mod, nir_shader *s)
{
   if (exec_list_is_empty(&s->inputs))
      return NULL;

   const struct dxil_mdnode *inputs[64];
   unsigned num_inputs = 0;

   nir_foreach_variable(var, &s->inputs) {
      char semantic_name[64] = "";
      enum dxil_semantic_kind semantic_kind;
      uint8_t interpolation = 0;
      uint8_t comp_type = dxil_get_prog_sig_comp_type(var->type);
      uint8_t start_row = (uint8_t)var->data.driver_location;
      uint8_t start_col = (uint8_t)var->data.location_frac;
      uint8_t cols = (uint8_t)glsl_get_components(var->type);

      if (s->info.stage == MESA_SHADER_VERTEX) {
         semantic_kind = get_semantic_vs_in_name(var, semantic_name);
         mod->inputs[num_inputs].sysvalue = false;
      } else {
         semantic_kind = get_semantic_name(var, semantic_name);
         interpolation = get_interpolation(var->data.interpolation);
         mod->inputs[num_inputs].sysvalue = in_sysvalue_name(var);
      }
      inputs[num_inputs] = fill_SV_param_nodes(mod, num_inputs, semantic_name,
                                               semantic_kind, start_row, 1, start_col, cols,
                                               interpolation);

      mod->inputs[num_inputs].name = ralloc_strdup(mod->ralloc_ctx,
                                                   semantic_name);
      struct dxil_signature_element *elm = &mod->inputs[num_inputs].sig;

      fill_signature_element(elm, semantic_kind, num_inputs, comp_type, start_row, start_col, cols);

      struct dxil_psv_signature_element *psv_elm = &mod->psv_inputs[num_inputs];
      if (!fill_psv_signature_element(psv_elm, semantic_kind, start_col, cols,
                                      interpolation, elm->comp_type,
                                      semantic_name, start_row, mod))
         return NULL;

      ++num_inputs;
      assert(num_inputs < ARRAY_SIZE(inputs));
   }
   mod->num_sig_inputs = num_inputs;
   const struct dxil_mdnode *retval = dxil_get_metadata_node(mod, inputs, num_inputs);
   return retval;
}

static const char *out_sysvalue_name(nir_variable *var)
{
   switch (var->data.location) {
   case VARYING_SLOT_POS:
      return "POS";
   default:
      return "NO";
   }
}

static const struct dxil_mdnode *
get_output_signature(struct dxil_module *mod, nir_shader *s)
{
   if (exec_list_is_empty(&s->outputs))
      return NULL;

   const struct dxil_mdnode *outputs[64];
   unsigned num_outputs = 0;
   nir_foreach_variable(var, &s->outputs) {
      enum dxil_semantic_kind semantic_kind;
      char semantic_name[64] = "";
      uint8_t interpolation = 0;
      uint8_t comp_type = dxil_get_prog_sig_comp_type(var->type);
      uint8_t start_row = var->data.driver_location;
      uint8_t start_col = var->data.location_frac;
      uint8_t cols = (uint8_t)glsl_get_components(var->type);

      if (s->info.stage == MESA_SHADER_FRAGMENT) {
         semantic_kind = get_semantic_ps_outname(var, semantic_name);
         mod->outputs[num_outputs].sysvalue = "TARGET";
      } else {
         semantic_kind = get_semantic_name(var, semantic_name);
         interpolation = get_interpolation(var->data.interpolation);
         mod->outputs[num_outputs].sysvalue = out_sysvalue_name(var);
      }

      mod->info.has_out_position |= semantic_kind == DXIL_SEM_POSITION;

      outputs[num_outputs] = fill_SV_param_nodes(mod, num_outputs, semantic_name,
                                                 semantic_kind, start_row, 1,
                                                 start_col, cols, interpolation);
      mod->outputs[num_outputs].name = ralloc_strdup(mod->ralloc_ctx,
                                                     semantic_name);
      struct dxil_signature_element *elm = &mod->outputs[num_outputs].sig;
      fill_signature_element(elm, semantic_kind, num_outputs, comp_type, start_row, start_col, cols);

      /* This is fishy, logic suggests that the LHS should be 0xf, but from the
       * validation it needs to be 0xff */
      elm->never_writes_mask = 0xff & ~elm->mask;

      struct dxil_psv_signature_element *psv_elm = &mod->psv_outputs[num_outputs];
      if (!fill_psv_signature_element(psv_elm, semantic_kind, start_col, cols,
                                      interpolation, comp_type,
                                      semantic_name, start_row, mod))
         return NULL;

      ++num_outputs;
      assert(num_outputs < ARRAY_SIZE(outputs));
   }

   const struct dxil_mdnode *retval = dxil_get_metadata_node(mod, outputs, num_outputs);
   mod->num_sig_outputs = num_outputs;
   return retval;
}

const struct dxil_mdnode *
get_signatures(struct dxil_module *mod, nir_shader *s)
{
   /* DXC does the same: Add an empty string before everything else */
   mod->sem_string_table = _mesa_string_buffer_create(NULL, 1024);
   copy_semantic_name_to_string(mod->sem_string_table, "");

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
