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

#include "d3d12_compiler.h"
#include "d3d12_context.h"
#include "d3d12_debug.h"
#include "d3d12_screen.h"
#include "nir_to_dxil.h"

#include "nir.h"
#include "compiler/nir/nir_builder.h"

#include "util/u_memory.h"
#include "util/u_simple_shaders.h"

static void
nir_emit_vertex(nir_builder *b, unsigned stream_id)
{
   nir_intrinsic_instr *instr;

   instr = nir_intrinsic_instr_create(b->shader, nir_intrinsic_emit_vertex);
   nir_intrinsic_set_stream_id(instr, stream_id);
   nir_builder_instr_insert(b, &instr->instr);
}

static d3d12_shader_selector*
d3d12_make_passthrough_gs(struct d3d12_context *ctx, struct d3d12_gs_variant_key *key)
{
   struct d3d12_shader_selector *gs;
   uint64_t varyings = key->varyings_mask;
   nir_builder b;
   nir_shader *nir;
   nir_intrinsic_instr *instr;
   struct pipe_shader_state templ;

   nir_builder_init_simple_shader(&b, NULL, MESA_SHADER_GEOMETRY,
                                  dxil_get_nir_compiler_options());

   nir = b.shader;
   nir->info.inputs_read = varyings;
   nir->info.outputs_written = varyings;
   nir->info.gs.input_primitive = GL_POINTS;
   nir->info.gs.output_primitive = GL_POINTS;
   nir->info.gs.vertices_in = 1;
   nir->info.gs.vertices_out = 1;
   nir->info.gs.invocations = 1;
   nir->info.gs.active_stream_mask = 1;
   nir->info.name = ralloc_strdup(nir, "passthrough");

   /* Copy inputs to outputs. */
   while (varyings) {
      nir_variable *in, *out;
      char tmp[100];
      const int i = u_bit_scan64(&varyings);

      snprintf(tmp, ARRAY_SIZE(tmp), "in_%d", key->varyings[i].driver_location);
      in = nir_variable_create(nir,
                               nir_var_shader_in,
                               key->varyings[i].type,
                               tmp);
      in->data.location = i;
      in->data.driver_location = key->varyings[i].driver_location;
      in->data.interpolation = key->varyings[i].interpolation;

      snprintf(tmp, ARRAY_SIZE(tmp), "out_%d", key->varyings[i].driver_location);
      out = nir_variable_create(nir,
                                nir_var_shader_out,
                                key->varyings[i].type,
                                tmp);
      out->data.location = i;
      out->data.driver_location = key->varyings[i].driver_location;
      out->data.interpolation = key->varyings[i].interpolation;

      nir_copy_var(&b, out, in);
   }

   /* EmitVertex */
   nir_emit_vertex(&b, 0);

   /* EndPrimitive */
   instr = nir_intrinsic_instr_create(nir, nir_intrinsic_end_primitive);
   nir_intrinsic_set_stream_id(instr, 0);
   nir_builder_instr_insert(&b, &instr->instr);

   NIR_PASS_V(nir, nir_lower_var_copies);
   nir_validate_shader(nir, "in d3d12_create_passthrough_gs");

   templ.type = PIPE_SHADER_IR_NIR;
   templ.ir.nir = nir;
   templ.stream_output.num_outputs = 0;

   gs = d3d12_create_shader(ctx, PIPE_SHADER_GEOMETRY, &templ);

   return gs;
}

struct emit_primitives_context
{
   struct d3d12_context *ctx;
   nir_builder b;

   unsigned num_vars;
   nir_variable *in[MAX_VARYING];
   nir_variable *out[MAX_VARYING];

   nir_loop *loop;
   nir_deref_instr *loop_index_deref;
   nir_ssa_def *loop_index;
   nir_ssa_def *edgeflag_cmp;
};

static bool
d3d12_begin_emit_primitives_gs(struct emit_primitives_context *emit_ctx,
                               struct d3d12_context *ctx,
                               struct d3d12_gs_variant_key *key,
                               uint16_t output_primitive,
                               unsigned vertices_out)
{
   nir_builder *b = &emit_ctx->b;
   nir_intrinsic_instr *instr;
   nir_variable *edgeflag_var = NULL;
   uint64_t varyings = key->varyings_mask;

   emit_ctx->ctx = ctx;

   nir_builder_init_simple_shader(b, NULL, MESA_SHADER_GEOMETRY,
                                  dxil_get_nir_compiler_options());

   nir_shader *nir = b->shader;
   nir->info.inputs_read = varyings;
   nir->info.outputs_written = varyings;
   nir->info.gs.input_primitive = GL_TRIANGLES;
   nir->info.gs.output_primitive = output_primitive;
   nir->info.gs.vertices_in = 3;
   nir->info.gs.vertices_out = vertices_out;
   nir->info.gs.invocations = 1;
   nir->info.gs.active_stream_mask = 1;
   nir->info.name = ralloc_strdup(nir, "edgeflags");

   while (varyings) {
      char tmp[100];
      const int i = u_bit_scan64(&varyings);

      snprintf(tmp, ARRAY_SIZE(tmp), "in_%d", emit_ctx->num_vars);
      emit_ctx->in[emit_ctx->num_vars] = nir_variable_create(nir,
                                                             nir_var_shader_in,
                                                             glsl_array_type(key->varyings[i].type, 3, 0),
                                                             tmp);
      emit_ctx->in[emit_ctx->num_vars]->data.location = i;
      emit_ctx->in[emit_ctx->num_vars]->data.driver_location = key->varyings[i].driver_location;
      emit_ctx->in[emit_ctx->num_vars]->data.interpolation = key->varyings[i].interpolation;

      /* Don't create an output for the edge flag variable */
      if (i == VARYING_SLOT_EDGE) {
         edgeflag_var = emit_ctx->in[emit_ctx->num_vars];
         continue;
      }

      snprintf(tmp, ARRAY_SIZE(tmp), "out_%d", emit_ctx->num_vars);
      emit_ctx->out[emit_ctx->num_vars] = nir_variable_create(nir,
                                                              nir_var_shader_out,
                                                              key->varyings[i].type,
                                                              tmp);
      emit_ctx->out[emit_ctx->num_vars]->data.location = i;
      emit_ctx->out[emit_ctx->num_vars]->data.driver_location = key->varyings[i].driver_location;
      emit_ctx->out[emit_ctx->num_vars]->data.interpolation = key->varyings[i].interpolation;

      emit_ctx->num_vars++;
   }

   /* Temporary variable "loop_index" to loop over input vertices */
   nir_function_impl *impl = nir_shader_get_entrypoint(nir);
   nir_variable *loop_index_var =
      nir_local_variable_create(impl, glsl_uint_type(), "loop_index");
   emit_ctx->loop_index_deref = nir_build_deref_var(b, loop_index_var);
   nir_store_deref(b, emit_ctx->loop_index_deref, nir_imm_int(b, 0), 1);

   /**
    *  while {
    *     if (loop_index >= 3)
    *        break;
    */
   emit_ctx->loop = nir_push_loop(b);

   emit_ctx->loop_index = nir_load_deref(b, emit_ctx->loop_index_deref);
   nir_ssa_def *cmp = nir_ige(b, emit_ctx->loop_index,
                              nir_imm_int(b, 3));
   nir_if *loop_check = nir_push_if(b, cmp);
   nir_jump(b, nir_jump_break);
   nir_pop_if(b, loop_check);

   if (edgeflag_var) {
      nir_ssa_def *edge_flag =
         nir_load_deref(b, nir_build_deref_array(b, nir_build_deref_var(b, edgeflag_var), emit_ctx->loop_index));
      emit_ctx->edgeflag_cmp = nir_feq(b, nir_channel(b, edge_flag, 0), nir_imm_float(b, 1.0));
   }

   return true;
}

static struct d3d12_shader_selector *
d3d12_finish_emit_primitives_gs(struct emit_primitives_context *emit_ctx)
{
   struct d3d12_shader_selector *gs;
   struct pipe_shader_state templ;
   nir_builder *b = &emit_ctx->b;
   nir_shader *nir = b->shader;

   /**
    *     EndPrimitive();
    *     loop_index++;
    *  }
    */
   nir_intrinsic_instr *end_primitive = nir_intrinsic_instr_create(nir, nir_intrinsic_end_primitive);
   nir_intrinsic_set_stream_id(end_primitive, 0);
   nir_builder_instr_insert(b, &end_primitive->instr);
   nir_store_deref(b, emit_ctx->loop_index_deref, nir_iadd_imm(b, emit_ctx->loop_index, 1), 1);
   nir_pop_loop(b, emit_ctx->loop);
   nir_validate_shader(nir, "in d3d12_lower_edge_flags");

   NIR_PASS_V(nir, nir_lower_var_copies);

   templ.type = PIPE_SHADER_IR_NIR;
   templ.ir.nir = nir;
   templ.stream_output.num_outputs = 0;

   return d3d12_create_shader(emit_ctx->ctx, PIPE_SHADER_GEOMETRY, &templ);
}

static d3d12_shader_selector*
d3d12_emit_points(struct d3d12_context *ctx, struct d3d12_gs_variant_key *key)
{
   struct emit_primitives_context emit_ctx = {0};
   nir_builder *b = &emit_ctx.b;
   nir_intrinsic_instr *instr;

   d3d12_begin_emit_primitives_gs(&emit_ctx, ctx, key, GL_POINTS, 3);

   for (unsigned i = 0; i < emit_ctx.num_vars; ++i) {
      nir_deref_instr *in_value = nir_build_deref_array(b, nir_build_deref_var(b, emit_ctx.in[i]), emit_ctx.loop_index);
      nir_copy_deref(b, nir_build_deref_var(b, emit_ctx.out[i]), in_value);
   }
   nir_emit_vertex(b, 0);

   return d3d12_finish_emit_primitives_gs(&emit_ctx);;
}

static uint32_t
hash_gs_variant_key(const void *key)
{
   return _mesa_hash_data(key, sizeof(struct d3d12_gs_variant_key));
}

static bool
equals_gs_variant_key(const void *a, const void *b)
{
   return memcmp(a, b, sizeof(struct d3d12_gs_variant_key)) == 0;
}

void
d3d12_gs_variant_cache_init(struct d3d12_context *ctx)
{
   ctx->gs_variant_cache = _mesa_hash_table_create(NULL, NULL, equals_gs_variant_key);
}

static void
delete_entry(struct hash_entry *entry)
{
   d3d12_shader_free((d3d12_shader_selector *)entry->data);
}

void
d3d12_gs_variant_cache_destroy(struct d3d12_context *ctx)
{
   _mesa_hash_table_destroy(ctx->gs_variant_cache, delete_entry);
}

static struct d3d12_shader_selector *
create_geometry_shader_variant(struct d3d12_context *ctx, struct d3d12_gs_variant_key *key)
{
   d3d12_shader_selector *gs = NULL;

   if (key->passthrough)
      gs = d3d12_make_passthrough_gs(ctx, key);
   else if (key->fill_mode == PIPE_POLYGON_MODE_POINT)
      gs = d3d12_emit_points(ctx, key);

   if (gs) {
      d3d12_shader_selector *vs = ctx->gfx_stages[PIPE_SHADER_VERTEX];
      gs->enabled_stream_outputs = vs->enabled_stream_outputs;
      memcpy(&gs->so_info, &vs->so_info, sizeof(gs->so_info));
      gs->is_gs_variant = true;
      gs->gs_key = *key;
   }

   return gs;
}

d3d12_shader_selector *
d3d12_get_gs_variant(struct d3d12_context *ctx, struct d3d12_gs_variant_key *key)
{
   uint32_t hash = hash_gs_variant_key(key);
   struct hash_entry *entry = _mesa_hash_table_search_pre_hashed(ctx->gs_variant_cache,
                                                                 hash, key);
   if (!entry) {
      d3d12_shader_selector *gs = create_geometry_shader_variant(ctx, key);
      entry = _mesa_hash_table_insert_pre_hashed(ctx->gs_variant_cache,
                                                 hash, &gs->gs_key, gs);
      assert(entry);
   }

   return (d3d12_shader_selector *)entry->data;
}
