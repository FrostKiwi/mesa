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
#include "d3d12_nir_passes.h"
#include "nir_to_dxil.h"

#include "pipe/p_state.h"

#include "nir.h"
#include "nir/nir_draw_helpers.h"
#include "nir/tgsi_to_nir.h"
#include "compiler/nir/nir_builder.h"
#include "tgsi/tgsi_from_mesa.h"
#include "tgsi/tgsi_ureg.h"

#include "util/u_memory.h"
#include "util/u_prim.h"
#include "util/u_simple_shaders.h"

#include <d3d12.h>
#include <dxcapi.h>
#include <wrl.h>

extern "C" {
#include "tgsi/tgsi_parse.h"
#include "tgsi/tgsi_point_sprite.h"
}

using Microsoft::WRL::ComPtr;

struct d3d12_validation_tools
{
   d3d12_validation_tools();

   bool validate_and_sign(struct blob *dxil);

   void disassemble(struct blob *dxil);

   void load_dxil_dll();

   struct HModule {
      HModule();
      ~HModule();

      bool load(LPCSTR file_name);
      operator HMODULE () const;
   private:
      HMODULE module;
   };

   HModule dxil_module;
   HModule dxc_compiler_module;
   ComPtr<IDxcCompiler> compiler;
   ComPtr<IDxcValidator> validator;
   ComPtr<IDxcLibrary> library;
};

struct d3d12_validation_tools *d3d12_validator_create()
{
   return new d3d12_validation_tools();
}

void d3d12_validator_destroy(struct d3d12_validation_tools *validator)
{
   delete validator;
}


const void *
d3d12_get_compiler_options(struct pipe_screen *screen,
                           enum pipe_shader_ir ir,
                           enum pipe_shader_type shader)
{
   assert(ir == PIPE_SHADER_IR_NIR);
   return dxil_get_nir_compiler_options();
}

static uint32_t
resource_dimension(enum glsl_sampler_dim dim)
{
   switch (dim) {
   case GLSL_SAMPLER_DIM_1D:
      return RESOURCE_DIMENSION_TEXTURE1D;
   case GLSL_SAMPLER_DIM_2D:
      return RESOURCE_DIMENSION_TEXTURE2D;
   case GLSL_SAMPLER_DIM_3D:
      return RESOURCE_DIMENSION_TEXTURE3D;
   case GLSL_SAMPLER_DIM_CUBE:
      return RESOURCE_DIMENSION_TEXTURECUBE;
   default:
      return RESOURCE_DIMENSION_UNKNOWN;
   }
}

static struct d3d12_shader *
compile_nir(struct d3d12_context *ctx, struct d3d12_shader_selector *sel,
            struct d3d12_shader_key *key, struct nir_shader *nir)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   struct d3d12_shader *shader = rzalloc(sel, d3d12_shader);
   shader->key = *key;
   shader->nir = nir;
   sel->current = shader;

   struct nir_lower_tex_options tex_options = { };
   tex_options.lower_txp = ~0u; /* No equivalent for textureProj */
   tex_options.lower_rect = true;
   tex_options.lower_rect_offset = true;

   NIR_PASS_V(nir, nir_lower_samplers);
   NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_uniform);
   NIR_PASS_V(nir, d3d12_create_bare_samplers);
   NIR_PASS_V(nir, nir_lower_tex, &tex_options);

   if (key->samples_int_textures)
      NIR_PASS_V(nir, dxil_lower_sample_to_txf_for_integer_tex,
                 key->tex_wrap_states, key->swizzle_state,
                 screen->base.get_paramf(&screen->base, PIPE_CAPF_MAX_TEXTURE_LOD_BIAS));

   if (key->vs.needs_format_emulation)
      d3d12_nir_lower_vs_vertex_conversion(nir, key->vs.format_conversion);

   uint32_t num_ubos_before_lower_to_ubo = nir->info.num_ubos;
   uint32_t num_uniforms_before_lower_to_ubo = nir->num_uniforms;
   NIR_PASS_V(nir, nir_lower_uniforms_to_ubo, 16);
   shader->has_default_ubo0 = num_uniforms_before_lower_to_ubo > 0 &&
                              nir->info.num_ubos > num_ubos_before_lower_to_ubo;

   if (key->last_vertex_processing_stage) {
      if (key->invert_depth)
         NIR_PASS_V(nir, d3d12_nir_invert_depth);
      NIR_PASS_V(nir, nir_lower_clip_halfz);
      NIR_PASS_V(nir, d3d12_lower_yflip);
   }
   NIR_PASS_V(nir, nir_lower_packed_ubo_loads);
   NIR_PASS_V(nir, d3d12_lower_load_first_vertex);
   NIR_PASS_V(nir, d3d12_lower_state_vars, shader);
   NIR_PASS_V(nir, d3d12_lower_bool_input);

   struct nir_to_dxil_options opts = {};
   opts.interpolate_at_vertex = screen->have_load_at_vertex;
   opts.lower_int16 = !screen->opts4.Native16BitShaderOpsSupported;
   opts.ubo_binding_offset = shader->has_default_ubo0 ? 0 : 1;
   opts.provoking_vertex = key->fs.provoking_vertex;

   struct blob tmp;
   if (!nir_to_dxil(nir, &opts, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return NULL;
   }

   // Non-ubo variables
   nir_foreach_variable(var, &nir->uniforms) {
      auto type = glsl_without_array(var->type);
      if (glsl_type_is_sampler(type) && glsl_get_sampler_result_type(type) != GLSL_TYPE_VOID) {
         unsigned count = glsl_type_is_array(var->type) ? glsl_get_aoa_size(var->type) : 1;
         for (unsigned i = 0; i < count; ++i) {
            shader->srv_bindings[shader->num_srv_bindings].index = var->data.binding + i;
            shader->srv_bindings[shader->num_srv_bindings].binding = var->data.binding;
            shader->srv_bindings[shader->num_srv_bindings].dimension = resource_dimension(glsl_get_sampler_dim(type));
            shader->num_srv_bindings++;
         }
      }
   }

   // Ubo variables
   if(nir->info.num_ubos) {
      // Ignore state_vars ubo as it is bound as root constants
      unsigned num_ubo_bindings = nir->info.num_ubos - (shader->state_vars_used ? 1 : 0);
      for(unsigned i = opts.ubo_binding_offset; i < num_ubo_bindings; ++i) {
         shader->cb_bindings[shader->num_cb_bindings++].binding = i;
      }
   }
   ctx->validation_tools->validate_and_sign(&tmp);

   if (d3d12_debug & D3D12_DEBUG_DISASS) {
      ctx->validation_tools->disassemble(&tmp);
   }

   blob_finish_get_buffer(&tmp, &shader->bytecode, &shader->bytecode_length);

   if (d3d12_debug & D3D12_DEBUG_DXIL) {
      char buf[256];
      static int i;
      snprintf(buf, sizeof(buf), "dump%02d.dxil", i++);
      FILE *fp = fopen(buf, "wb");
      fwrite(shader->bytecode, sizeof(char), shader->bytecode_length, fp);
      fclose(fp);
      fprintf(stderr, "wrote '%s'...\n", buf);
   }
   return shader;
}

struct d3d12_selection_context {
   struct d3d12_context *ctx;
   const struct pipe_draw_info *dinfo;
   bool needs_point_sprite_lowering;
   bool manual_depth_range;
   bool samples_int_textures;
   bool compare_with_lod_bias_grad;
   unsigned missing_dual_src_outputs;
   unsigned frag_result_color_lowering;
};

static void
nir_emit_vertex(nir_builder *b, unsigned stream_id)
{
   nir_intrinsic_instr *instr;

   instr = nir_intrinsic_instr_create(b->shader, nir_intrinsic_emit_vertex);
   nir_intrinsic_set_stream_id(instr, stream_id);
   nir_builder_instr_insert(b, &instr->instr);
}

static d3d12_shader_selector*
d3d12_make_passthrough_gs(struct d3d12_context *ctx, d3d12_shader_selector *vs)
{
   struct d3d12_shader_selector *gs;
   nir_builder b;
   nir_shader *nir;
   nir_intrinsic_instr *instr;
   struct pipe_shader_state templ;

   assert(vs);

   nir_builder_init_simple_shader(&b, NULL, MESA_SHADER_GEOMETRY, // XXX
                                  dxil_get_nir_compiler_options());

   nir = b.shader;
   nir->info.inputs_read = vs->current->nir->info.outputs_written;
   nir->info.outputs_written = vs->current->nir->info.outputs_written;
   nir->info.gs.input_primitive = GL_POINTS;
   nir->info.gs.output_primitive = GL_POINTS;
   nir->info.gs.vertices_in = 1;
   nir->info.gs.vertices_out = 1;
   nir->info.gs.invocations = 1;
   nir->info.gs.active_stream_mask = 1;
   nir->info.name = ralloc_strdup(nir, "passthrough");

   /* Copy inputs to outputs. */
   unsigned i = 0;
   nir_foreach_variable(var, &vs->current->nir->outputs) {
      nir_variable *in, *out;
      char tmp[100];

      snprintf(tmp, ARRAY_SIZE(tmp), "in_%d", i);
      in = nir_variable_create(nir,
                               nir_var_shader_in,
                               var->type,
                               tmp);
      in->data.location = var->data.location;
      in->data.driver_location = i;
      in->data.interpolation = var->data.interpolation;

      snprintf(tmp, ARRAY_SIZE(tmp), "out_%d", i);
      out = nir_variable_create(nir,
                                nir_var_shader_out,
                                var->type,
                                tmp);
      out->data.location = var->data.location;
      out->data.driver_location = i;
      out->data.interpolation = var->data.interpolation;

      nir_copy_var(&b, out, in);
      i++;
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
   gs->passthrough = 1;
   gs->passthrough_varyings = vs->current->nir->info.outputs_written;
   gs->enabled_stream_outputs = vs->enabled_stream_outputs;
   memcpy(&gs->so_info, &vs->so_info, sizeof(gs->so_info));

   return gs;
}

static unsigned
missing_dual_src_outputs(struct d3d12_context *ctx)
{
   if (!ctx->gfx_pipeline_state.blend->is_dual_src)
      return 0;

   struct d3d12_shader_selector *fs = ctx->gfx_stages[PIPE_SHADER_FRAGMENT];
   nir_shader *s = fs->initial;

   unsigned indices_seen = 0;
   nir_foreach_function(function, s) {
      if (function->impl) {
         nir_foreach_block(block, function->impl) {
            nir_foreach_instr(instr, block) {
               if (instr->type != nir_instr_type_intrinsic)
                  continue;

               nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
               if (intr->intrinsic != nir_intrinsic_store_deref)
                  continue;

               nir_variable *var = nir_intrinsic_get_var(intr, 0);
               if (var->data.mode != nir_var_shader_out ||
                   (var->data.location != FRAG_RESULT_COLOR &&
                    var->data.location != FRAG_RESULT_DATA0))
                  continue;

               indices_seen |= 1u << var->data.index;
               if ((indices_seen & 3) == 3)
                  return 0;
            }
         }
      }
   }

   return 3 & ~indices_seen;
}

static unsigned
frag_result_color_lowering(struct d3d12_context *ctx)
{
   struct d3d12_shader_selector *fs = ctx->gfx_stages[PIPE_SHADER_FRAGMENT];
   assert(fs);

   if (fs->initial->info.outputs_written & BITFIELD64_BIT(FRAG_RESULT_COLOR))
      return ctx->fb.nr_cbufs > 1 ? ctx->fb.nr_cbufs : 0;

   return 0;
}

static bool
manual_depth_range(struct d3d12_context *ctx)
{
   if (!d3d12_need_zero_one_depth_range(ctx))
      return false;

   /**
    * If we can't use the D3D12 zero-one depth-range, we might have to apply
    * depth-range ourselves.
    *
    * Because we only need to override the depth-range to zero-one range in
    * the case where we write frag-depth, we only need to apply manual
    * depth-range to gl_FragCoord.z.
    *
    * No extra care is needed to be taken in the case where gl_FragDepth is
    * written conditionally, because the GLSL 4.60 spec states:
    *
    *    If a shader statically assigns a value to gl_FragDepth, and there
    *    is an execution path through the shader that does not set
    *    gl_FragDepth, then the value of the fragment’s depth may be
    *    undefined for executions of the shader that take that path. That
    *    is, if the set of linked fragment shaders statically contain a
    *    write to gl_FragDepth, then it is responsible for always writing
    *    it.
    */

   struct d3d12_shader_selector *fs = ctx->gfx_stages[PIPE_SHADER_FRAGMENT];
   return fs && fs->initial->info.inputs_read & VARYING_BIT_POS;
}

static bool
needs_point_sprite_lowering(struct d3d12_context *ctx, const struct pipe_draw_info *dinfo)
{
   struct d3d12_shader_selector *vs = ctx->gfx_stages[PIPE_SHADER_VERTEX];
   struct d3d12_shader_selector *gs = ctx->gfx_stages[PIPE_SHADER_GEOMETRY];

   if (gs != NULL && !gs->passthrough) {
      /* There is an user GS; Check if it outputs points with PSIZE */
      return (gs->initial->info.gs.output_primitive == GL_POINTS &&
              gs->initial->info.outputs_written & VARYING_BIT_PSIZ);
   } else {
      /* No user GS; check if we are drawing wide points */
      return (dinfo->mode == PIPE_PRIM_POINTS &&
              (ctx->gfx_pipeline_state.rast->base.point_size > 1.0 ||
               vs->initial->info.outputs_written & VARYING_BIT_PSIZ) &&
              (vs->initial->info.outputs_written & VARYING_BIT_POS));
   }
}

static unsigned
get_provoking_vertex(struct d3d12_selection_context *sel_ctx, d3d12_shader_selector *prev)
{
   if (sel_ctx->ctx->gfx_pipeline_state.rast && sel_ctx->ctx->gfx_pipeline_state.rast->base.flatshade_first)
      return 0;

   /* Make sure GL prims match Gallium prims */
   STATIC_ASSERT(GL_POINTS == PIPE_PRIM_POINTS);
   STATIC_ASSERT(GL_LINES == PIPE_PRIM_LINES);
   STATIC_ASSERT(GL_LINE_STRIP == PIPE_PRIM_LINE_STRIP);

   enum pipe_prim_type mode;
   switch (prev->stage) {
   case PIPE_SHADER_GEOMETRY:
      mode = (enum pipe_prim_type)prev->current->nir->info.gs.output_primitive;
      break;
   case PIPE_SHADER_VERTEX:
      mode = sel_ctx->dinfo ? sel_ctx->dinfo->mode : PIPE_PRIM_TRIANGLES;
      break;
   default:
      unreachable("Tesselation shaders are not supported");
   }

   return u_prim_vertex_count(mode)->min - 1;
}

static void
validate_geometry_shader(struct d3d12_selection_context *sel_ctx)
{
   struct d3d12_context *ctx = sel_ctx->ctx;

   d3d12_shader_selector *gs = ctx->gfx_stages[PIPE_SHADER_GEOMETRY];

   /* Determine whether we need to create/recreate the passthrough geometry shader */
   bool have_passthrough_gs = gs != NULL && gs->passthrough;

   if (sel_ctx->needs_point_sprite_lowering) {
      d3d12_shader_selector *vs = ctx->gfx_stages[PIPE_SHADER_VERTEX];

      /* Make sure the passthrough inputs are matching the vs outputs */
      if (have_passthrough_gs &&
          gs->passthrough_varyings != vs->current->nir->info.outputs_written) {
         d3d12_shader_free(gs);
         gs = NULL;
      }

      if (gs == NULL)
         gs = d3d12_make_passthrough_gs(ctx, vs);

      ctx->gfx_stages[PIPE_SHADER_GEOMETRY] = gs;
   } else if (have_passthrough_gs) {
      /* We don't need a GS for lowering point sprites, so remove the
       * passthrough shader */
      ctx->gfx_stages[PIPE_SHADER_GEOMETRY] = NULL;
      d3d12_shader_free(gs);
   }
}

static bool
d3d12_compare_shader_keys(const d3d12_shader_key *expect, const d3d12_shader_key *have)
{
   assert(expect->stage == have->stage);
   assert(expect);
   assert(have);

   /* Because we only add varyings we check that a shader has at least the expected in-
    * and outputs. */
   if ((expect->required_varying_inputs & ~have->required_varying_inputs) ||
       (expect->required_varying_outputs & ~have->required_varying_outputs) ||
       (expect->next_varying_inputs != have->next_varying_inputs) ||
       (expect->prev_varying_outputs != have->prev_varying_outputs))
      return false;

   if (expect->stage == PIPE_SHADER_GEOMETRY) {
      if (expect->gs.writes_psize) {
         if (!have->gs.writes_psize ||
             expect->gs.point_pos_stream_out != have->gs.point_pos_stream_out ||
             expect->gs.sprite_coord_enable != have->gs.sprite_coord_enable ||
             expect->gs.sprite_origin_upper_left != have->gs.sprite_origin_upper_left)
            return false;
      } else if (have->gs.writes_psize) {
         return false;
      }
   } else if (expect->stage == PIPE_SHADER_FRAGMENT) {
      if (expect->fs.frag_result_color_lowering != have->fs.frag_result_color_lowering)
         return false;
      if (expect->fs.manual_depth_range != have->fs.manual_depth_range)
         return false;
      if (expect->fs.polygon_stipple != have->fs.polygon_stipple)
         return false;
   }

   if (expect->samples_int_textures != have->samples_int_textures)
      return false;

   if (expect->n_texture_states != have->n_texture_states)
      return false;

   if (memcmp(expect->tex_wrap_states, have->tex_wrap_states,
              expect->n_texture_states * sizeof(dxil_wrap_sampler_state)))
      return false;

   if (memcmp(expect->swizzle_state, have->swizzle_state,
              expect->n_texture_states * sizeof(dxil_texture_swizzle_state)))
      return false;

   if (memcmp(expect->sampler_compare_funcs, have->sampler_compare_funcs,
              expect->n_texture_states * sizeof(enum compare_func)))
      return false;

   if (expect->invert_depth != have->invert_depth)
      return false;

   if (expect->stage == PIPE_SHADER_VERTEX) {
      if (expect->vs.needs_format_emulation != have->vs.needs_format_emulation)
         return false;

      if (expect->vs.needs_format_emulation) {
         if (memcmp(expect->vs.format_conversion, have->vs.format_conversion,
                    PIPE_MAX_ATTRIBS * sizeof (enum pipe_format)))
            return false;
      }
   }

   if (expect->fs.provoking_vertex != have->fs.provoking_vertex)
      return false;

   return true;
}

static void
d3d12_fill_shader_key(struct d3d12_selection_context *sel_ctx,
                      d3d12_shader_key *key, pipe_shader_type stage,
                      d3d12_shader_selector *prev, d3d12_shader_selector *next)
{
   uint64_t system_generated_in_values =
         VARYING_BIT_FACE |
         VARYING_BIT_PNTC |
         VARYING_BIT_PRIMITIVE_ID;

   uint64_t system_out_values =
         VARYING_BIT_CLIP_DIST0 |
         VARYING_BIT_CLIP_DIST1;

   memset(key, 0, sizeof(d3d12_shader_key));
   key->stage = stage;

   /* We require as inputs what the previous stage has written,
    * except certain system values */
   if (prev) {
      if (stage == PIPE_SHADER_FRAGMENT || stage == PIPE_SHADER_GEOMETRY)
         system_out_values |= VARYING_BIT_POS;
      if (stage == PIPE_SHADER_FRAGMENT)
         system_out_values |= VARYING_BIT_PSIZ;
      key->required_varying_inputs = prev->current->nir->info.outputs_written & ~system_out_values;
      key->prev_varying_outputs = prev->current->nir->info.outputs_written;
   }

   /* We require as outputs what the next stage reads,
    * except certain system values */
   if (next) {
      if (!next->passthrough) {
         if (stage == PIPE_SHADER_VERTEX)
            system_generated_in_values |= VARYING_BIT_POS;
         key->required_varying_outputs = next->current->nir->info.inputs_read & ~system_generated_in_values;
      }
      key->next_varying_inputs = next->current->nir->info.inputs_read;
   }

   if (stage == PIPE_SHADER_GEOMETRY ||
       (stage == PIPE_SHADER_VERTEX && (!next || next->stage != PIPE_SHADER_GEOMETRY))) {
      key->last_vertex_processing_stage = 1;
      key->invert_depth = sel_ctx->ctx->reverse_depth_range;
      if (sel_ctx->ctx->pstipple.enabled)
         key->next_varying_inputs |= VARYING_BIT_POS;
   }

   if (stage == PIPE_SHADER_GEOMETRY &&
       sel_ctx->needs_point_sprite_lowering &&
       sel_ctx->ctx->gfx_pipeline_state.rast) {
      struct pipe_rasterizer_state *rast = &sel_ctx->ctx->gfx_pipeline_state.rast->base;
      key->gs.writes_psize = 1;
      key->gs.sprite_coord_enable = rast->sprite_coord_enable;
      key->gs.sprite_origin_upper_left = (rast->sprite_coord_mode != PIPE_SPRITE_COORD_LOWER_LEFT);
      if (sel_ctx->ctx->flip_y < 0)
         key->gs.sprite_origin_upper_left = !key->gs.sprite_origin_upper_left;
      key->gs.aa_point = rast->point_smooth;
      key->gs.stream_output_factor = 6;
   } else if (stage == PIPE_SHADER_FRAGMENT) {
      key->fs.missing_dual_src_outputs = sel_ctx->missing_dual_src_outputs;
      key->fs.frag_result_color_lowering = sel_ctx->frag_result_color_lowering;
      key->fs.manual_depth_range = sel_ctx->manual_depth_range;
      key->fs.polygon_stipple = sel_ctx->ctx->pstipple.enabled;
   }

   if (sel_ctx->samples_int_textures) {
      key->samples_int_textures = sel_ctx->samples_int_textures;
      key->n_texture_states = sel_ctx->ctx->num_sampler_views[stage];
      /* Copy only states with integer textures */
      for(int i = 0; i < key->n_texture_states; ++i) {
         auto& wrap_state = sel_ctx->ctx->tex_wrap_states[stage][i];
         if (wrap_state.is_int_sampler) {
            memcpy(&key->tex_wrap_states[i], &wrap_state, sizeof(wrap_state));
            key->swizzle_state[i] = sel_ctx->ctx->tex_swizzle_state[stage][i];
         }
      }
   }

   if (sel_ctx->compare_with_lod_bias_grad) {
      key->n_texture_states = sel_ctx->ctx->num_sampler_views[stage];
      memcpy(key->sampler_compare_funcs, sel_ctx->ctx->tex_compare_func[stage],
             key->n_texture_states * sizeof(enum compare_func));
      memcpy(key->swizzle_state, sel_ctx->ctx->tex_swizzle_state[stage],
             key->n_texture_states * sizeof(dxil_texture_swizzle_state));
   }

   if (stage == PIPE_SHADER_VERTEX && sel_ctx->ctx->gfx_pipeline_state.ves) {
      key->vs.needs_format_emulation = sel_ctx->ctx->gfx_pipeline_state.ves->needs_format_emulation;
      if (key->vs.needs_format_emulation) {
         memcpy(key->vs.format_conversion, sel_ctx->ctx->gfx_pipeline_state.ves->format_conversion,
                sel_ctx->ctx->gfx_pipeline_state.ves->num_elements * sizeof(enum pipe_format));
      }
   }

   /* Only set the key value if the driver actually supports changing the provoking vertex */
   if (stage == PIPE_SHADER_FRAGMENT && sel_ctx->ctx->gfx_pipeline_state.rast &&
       d3d12_screen(sel_ctx->ctx->base.screen)->have_load_at_vertex)
      key->fs.provoking_vertex = get_provoking_vertex(sel_ctx, prev);
}

static void
select_shader_variant(struct d3d12_selection_context *sel_ctx, d3d12_shader_selector *sel,
                     d3d12_shader_selector *prev, d3d12_shader_selector *next)
{
   struct d3d12_context *ctx = sel_ctx->ctx;
   d3d12_shader_key key;
   nir_shader *new_nir_variant;
   unsigned pstipple_binding = UINT32_MAX;

   sel_ctx->samples_int_textures = sel->samples_int_textures;
   sel_ctx->compare_with_lod_bias_grad = sel->compare_with_lod_bias_grad;
   d3d12_fill_shader_key(sel_ctx, &key, sel->stage, prev, next);

   /* Check for an existing variant */
   for (d3d12_shader *variant = sel->first; variant;
        variant = variant->next_variant) {

      if (d3d12_compare_shader_keys(&key, &variant->key)) {
         sel->current = variant;
         return;
      }
   }

   /* Clone the NIR shader */
   new_nir_variant = nir_shader_clone(sel, sel->initial);

   /* Apply any needed lowering passes */
   if (key.gs.writes_psize) {
      NIR_PASS_V(new_nir_variant, d3d12_lower_point_sprite,
                 !key.gs.sprite_origin_upper_left,
                 key.gs.sprite_coord_enable,
                 next ? next->current->nir->info.inputs_read : 0);

      nir_function_impl *impl = nir_shader_get_entrypoint(new_nir_variant);
      nir_shader_gather_info(new_nir_variant, impl);
   }

   if (key.fs.polygon_stipple) {
      NIR_PASS_V(new_nir_variant, nir_lower_pstipple_fs,
                 &pstipple_binding, 0, false);

      nir_function_impl *impl = nir_shader_get_entrypoint(new_nir_variant);
      nir_shader_gather_info(new_nir_variant, impl);
   }

   if (key.fs.missing_dual_src_outputs) {
      NIR_PASS_V(new_nir_variant, d3d12_add_missing_dual_src_target,
                 key.fs.missing_dual_src_outputs);
   } else if (key.fs.frag_result_color_lowering) {
      NIR_PASS_V(new_nir_variant, d3d12_lower_frag_result,
                 key.fs.frag_result_color_lowering);
   }

   if (key.fs.manual_depth_range)
      NIR_PASS_V(new_nir_variant, d3d12_lower_depth_range);

   if (sel_ctx->compare_with_lod_bias_grad)
      NIR_PASS_V(new_nir_variant, d3d12_lower_sample_tex_compare, key.n_texture_states,
                 key.sampler_compare_funcs, key.swizzle_state);

   /* Add the needed in and outputs, and re-sort */
   uint64_t mask = key.required_varying_inputs & ~new_nir_variant->info.inputs_read;

   if (prev) {
      if (mask) {
         nir_foreach_variable(var, &prev->current->nir->outputs) {
            if (mask & (1ull << var->data.location)) {
               nir_variable *new_var = nir_variable_clone(var, new_nir_variant);
               new_var->data.mode = nir_var_shader_in;
               new_var->data.driver_location = exec_list_length(&new_nir_variant->inputs);
               exec_list_push_tail(&new_nir_variant->inputs, &new_var->node);
            }
         }
      }
      d3d12_reassign_driver_locations(&new_nir_variant->inputs,
                                      prev->current->nir->info.outputs_written);
   }

   mask = key.required_varying_outputs & ~new_nir_variant->info.outputs_written;

   if (next) {
      if (mask && !next->passthrough) {
         nir_foreach_variable(var, &next->current->nir->inputs) {
            if (mask & (1ull << var->data.location)) {
               nir_variable *new_var = nir_variable_clone(var, new_nir_variant);
               new_var->data.mode = nir_var_shader_out;
               new_var->data.driver_location = exec_list_length(&new_nir_variant->outputs);
               exec_list_push_tail(&new_nir_variant->outputs, &new_var->node);
            }
         }
      }
      d3d12_reassign_driver_locations(&new_nir_variant->outputs, key.next_varying_inputs);
   }

   d3d12_shader *new_variant = compile_nir(ctx, sel, &key, new_nir_variant);
   assert(new_variant);

   /* keep track of polygon stipple texture binding */
   new_variant->pstipple_binding = pstipple_binding;

   /* prepend the new shader in the selector chain and pick it */
   new_variant->next_variant = sel->first;
   sel->current = sel->first = new_variant;
}

static d3d12_shader_selector *
get_prev_shader(struct d3d12_context *ctx, pipe_shader_type current)
{
   /* No TESS_CTRL or TESS_EVAL yet */

   switch (current) {
   case PIPE_SHADER_VERTEX:
      return NULL;
   case PIPE_SHADER_FRAGMENT:
      if (ctx->gfx_stages[PIPE_SHADER_GEOMETRY])
         return ctx->gfx_stages[PIPE_SHADER_GEOMETRY];
      /* fallthrough */
   case PIPE_SHADER_GEOMETRY:
      return ctx->gfx_stages[PIPE_SHADER_VERTEX];
   default:
      unreachable("shader type not supported");
   }
}

static d3d12_shader_selector *
get_next_shader(struct d3d12_context *ctx, pipe_shader_type current)
{
   /* No TESS_CTRL or TESS_EVAL yet */

   switch (current) {
   case PIPE_SHADER_VERTEX:
      if (ctx->gfx_stages[PIPE_SHADER_GEOMETRY])
         return ctx->gfx_stages[PIPE_SHADER_GEOMETRY];
      /* fallthrough */
   case PIPE_SHADER_GEOMETRY:
      return ctx->gfx_stages[PIPE_SHADER_FRAGMENT];
   case PIPE_SHADER_FRAGMENT:
      return NULL;
   default:
      unreachable("shader type not supported");
   }
}

enum tex_scan_flags {
   TEX_SAMPLE_INTEGER_TEXTURE = 1 << 0,
   TEX_CMP_WITH_LOD_BIAS_GRAD = 1 << 1,
   TEX_SCAN_ALL_FLAGS         = (1 << 2) - 1
};

static unsigned
scan_texture_use(nir_shader *nir)
{
   unsigned result = 0;
   nir_foreach_function(func, nir) {
      nir_foreach_block(block, func->impl) {
         nir_foreach_instr(instr, block) {
            if (instr->type == nir_instr_type_tex) {
               auto tex = nir_instr_as_tex(instr);
               switch (tex->op) {
               case nir_texop_txb:
               case nir_texop_txl:
               case nir_texop_txd:
                  if (tex->is_shadow)
                     result |= TEX_CMP_WITH_LOD_BIAS_GRAD;
                  /* fallthrough */
               case nir_texop_tex:
                  if (tex->dest_type & (nir_type_int | nir_type_uint))
                     result |= TEX_SAMPLE_INTEGER_TEXTURE;
               default:
                  ;
               }
            }
            if (TEX_SCAN_ALL_FLAGS == result)
               return result;
         }
      }
   }
   return result;
}

static uint64_t
update_so_info(struct pipe_stream_output_info *so_info,
               uint64_t outputs_written)
{
   uint64_t so_outputs = 0;
   uint8_t reverse_map[64] = {0};
   unsigned slot = 0;

   while (outputs_written)
      reverse_map[slot++] = u_bit_scan64(&outputs_written);

   for (unsigned i = 0; i < so_info->num_outputs; i++) {
      struct pipe_stream_output *output = &so_info->output[i];

      /* Map Gallium's condensed "slots" back to real VARYING_SLOT_* enums */
      output->register_index = reverse_map[output->register_index];

      so_outputs |= 1ull << output->register_index;
   }

   return so_outputs;
}

struct d3d12_shader_selector *
d3d12_create_shader(struct d3d12_context *ctx,
                    pipe_shader_type stage,
                    const struct pipe_shader_state *shader)
{
   struct d3d12_shader_selector *sel = rzalloc(nullptr, d3d12_shader_selector);
   sel->stage = stage;

   struct nir_shader *nir = NULL;

   if (shader->type == PIPE_SHADER_IR_NIR) {
      nir = (nir_shader *)shader->ir.nir;
   } else {
      assert(shader->type == PIPE_SHADER_IR_TGSI);
      nir = tgsi_to_nir(shader->tokens, ctx->base.screen);
   }

   nir_shader_gather_info(nir, nir_shader_get_entrypoint(nir));

   unsigned tex_scan_result = scan_texture_use(nir);
   sel->samples_int_textures = (tex_scan_result & TEX_SAMPLE_INTEGER_TEXTURE) != 0;
   sel->compare_with_lod_bias_grad = (tex_scan_result & TEX_CMP_WITH_LOD_BIAS_GRAD) != 0;

   memcpy(&sel->so_info, &shader->stream_output, sizeof(sel->so_info));
   sel->enabled_stream_outputs = update_so_info(&sel->so_info,
                                                nir->info.outputs_written);

   assert(nir != NULL);
   d3d12_shader_selector *prev = get_prev_shader(ctx, sel->stage);
   d3d12_shader_selector *next = get_next_shader(ctx, sel->stage);

   uint64_t in_mask = nir->info.stage == MESA_SHADER_VERTEX ?
                         0 : VARYING_BIT_PRIMITIVE_ID;

   uint64_t out_mask = nir->info.stage == MESA_SHADER_FRAGMENT ?
                          (1ull << FRAG_RESULT_STENCIL) :
                          VARYING_BIT_PRIMITIVE_ID;

   d3d12_fix_io_uint_type(nir, in_mask, out_mask);

   if (nir->info.stage != MESA_SHADER_VERTEX)
      nir->info.inputs_read =
            d3d12_reassign_driver_locations(&nir->inputs,
                                            prev ? prev->current->nir->info.outputs_written : 0);
   else
      nir->info.inputs_read = d3d12_sort_by_driver_location(&nir->inputs);

   if (nir->info.stage != MESA_SHADER_FRAGMENT) {
      nir->info.outputs_written =
            d3d12_reassign_driver_locations(&nir->outputs,
                                            next ? next->current->nir->info.inputs_read : 0);
   } else {
      NIR_PASS_V(nir, nir_lower_fragcoord_wtrans);
      d3d12_sort_ps_outputs(&nir->outputs);
   }

   /* Integer cube maps are not supported in DirectX because sampling is not supported
    * on integer textures and TextureLoad is not supported for cube maps, so we have to
    * lower integer cube maps to be handled like 2D textures arrays*/
   NIR_PASS_V(nir, d3d12_lower_int_cubmap_to_array);

   /* Keep this initial shader as the blue print for possible variants */
   sel->initial = nir;

   /*
    * We must compile some shader here, because if the previous or a next shaders exists later
    * when the shaders are bound, then the key evaluation in the shader selector will access
    * the current variant of these  prev and next shader, and we can only assign
    * a current variant when it has been successfully compiled.
    *
    * For shaders that require lowering because certain instructions are not available
    * and their emulation is state depended (like sampling an integer texture that must be
    * emulated and needs handling of boundary conditions, or shadow compare sampling with LOD),
    * we must go through the shader selector here to create a compilable variant.
    * For shaders that are not depended on the state this is just compiling the original
    * shader.
    *
    * TODO: get rid of having to compiling the shader here if it can be forseen that it will
    * be thrown away (i.e. it depends on states that are likely to change before the shader is
    * used for the first time)
    */
   struct d3d12_selection_context sel_ctx = {0};
   sel_ctx.ctx = ctx;
   select_shader_variant(&sel_ctx, sel, prev, next);

   if (!sel->current) {
      ralloc_free(sel);
      return NULL;
   }

   return sel;
}

void
d3d12_select_shader_variants(struct d3d12_context *ctx, const struct pipe_draw_info *dinfo)
{
   static unsigned order[] = {PIPE_SHADER_VERTEX, PIPE_SHADER_GEOMETRY, PIPE_SHADER_FRAGMENT};
   struct d3d12_selection_context sel_ctx;

   sel_ctx.ctx = ctx;
   sel_ctx.dinfo = dinfo;
   sel_ctx.needs_point_sprite_lowering = needs_point_sprite_lowering(ctx, dinfo);
   sel_ctx.missing_dual_src_outputs = missing_dual_src_outputs(ctx);
   sel_ctx.frag_result_color_lowering = frag_result_color_lowering(ctx);
   sel_ctx.manual_depth_range = manual_depth_range(ctx);

   validate_geometry_shader(&sel_ctx);

   for (int i = 0; i < ARRAY_SIZE(order); ++i) {
      auto sel = ctx->gfx_stages[order[i]];
      if (!sel)
         continue;

      d3d12_shader_selector *prev = get_prev_shader(ctx, sel->stage);
      d3d12_shader_selector *next = get_next_shader(ctx, sel->stage);

      select_shader_variant(&sel_ctx, sel, prev, next);
   }
}

void
d3d12_shader_free(struct d3d12_shader_selector *sel)
{
   auto shader = sel->first;
   while (shader) {
      free(shader->bytecode);
      shader = shader->next_variant;
   }
   ralloc_free(sel->initial);
   ralloc_free(sel);
}

// Used to get path to self
extern "C" extern IMAGE_DOS_HEADER __ImageBase;

void d3d12_validation_tools::load_dxil_dll()
{
   if (!dxil_module.load("dxil.dll")) {
      char selfPath[MAX_PATH] = "";
      uint32_t pathSize = GetModuleFileNameA((HINSTANCE)&__ImageBase, selfPath, sizeof(selfPath));
      if (pathSize == 0 || pathSize == sizeof(selfPath)) {
         debug_printf("D3D12: Unable to get path to self");
         return;
      }

      auto lastSlash = strrchr(selfPath, '\\');
      if (!lastSlash) {
         debug_printf("D3D12: Unable to get path to self");
         return;
      }

      *(lastSlash + 1) = '\0';
      if (strcat_s(selfPath, "dxil.dll") != 0) {
         debug_printf("D3D12: Unable to get path to dxil.dll next to self");
         return;
      }

      dxil_module.load(selfPath);
   }
}

d3d12_validation_tools::d3d12_validation_tools()
{
   load_dxil_dll();
   DxcCreateInstanceProc dxil_create_func = (DxcCreateInstanceProc)GetProcAddress(dxil_module, "DxcCreateInstance");
   assert(dxil_create_func);

   HRESULT hr = dxil_create_func(CLSID_DxcValidator,  IID_PPV_ARGS(&validator));
   if (FAILED(hr)) {
      debug_printf("D3D12: Unable to create validator\n");
   }

   DxcCreateInstanceProc compiler_create_func  = nullptr;
   if(dxc_compiler_module.load("dxcompiler.dll"))
      compiler_create_func = (DxcCreateInstanceProc)GetProcAddress(dxc_compiler_module, "DxcCreateInstance");

   if (compiler_create_func) {
      hr = compiler_create_func(CLSID_DxcLibrary, IID_PPV_ARGS(&library));
      if (FAILED(hr)) {
         debug_printf("D3D12: Unable to create library instance: %x\n", hr);
      }

      if (d3d12_debug & D3D12_DEBUG_DISASS) {
         hr = compiler_create_func(CLSID_DxcCompiler, IID_PPV_ARGS(&compiler));
         if (FAILED(hr)) {
            debug_printf("D3D12: Unable to create compiler instance\n");
         }
      }
   } else if (d3d12_debug & D3D12_DEBUG_DISASS) {
      debug_printf("D3D12: Disassembly requested but compiler couldn't be loaded\n");
   }
}

d3d12_validation_tools::HModule::HModule():
   module(0)
{
}

d3d12_validation_tools::HModule::~HModule()
{
   if (module)
      ::FreeLibrary(module);
}

inline
d3d12_validation_tools::HModule::operator HMODULE () const
{
   return module;
}

bool
d3d12_validation_tools::HModule::load(LPCSTR file_name)
{
   module = ::LoadLibrary(file_name);
   return module != nullptr;
}


class ShaderBlob : public IDxcBlob {
public:
   ShaderBlob(blob* data) : m_data(data) {}

   LPVOID STDMETHODCALLTYPE GetBufferPointer(void) override { return m_data->data; }

   SIZE_T STDMETHODCALLTYPE GetBufferSize() override { return m_data->size; }

   HRESULT STDMETHODCALLTYPE QueryInterface(REFIID, void**) override { return E_NOINTERFACE; }

   ULONG STDMETHODCALLTYPE AddRef() override { return 1; }

   ULONG STDMETHODCALLTYPE Release() override { return 0; }

   blob* m_data;
};

bool d3d12_validation_tools::validate_and_sign(struct blob *dxil)
{
   ShaderBlob source(dxil);

   ComPtr<IDxcOperationResult> result;
   if (!validator)
      return false;

   validator->Validate(&source, DxcValidatorFlags_InPlaceEdit, &result);
   HRESULT validationStatus;
   result->GetStatus(&validationStatus);
   if (FAILED(validationStatus) && library) {
      ComPtr<IDxcBlobEncoding> printBlob, printBlobUtf8;
      result->GetErrorBuffer(&printBlob);
      library->GetBlobAsUtf8(printBlob.Get(), printBlobUtf8.GetAddressOf());

      char *errorString;
      if (printBlobUtf8) {
         errorString = reinterpret_cast<char*>(printBlobUtf8->GetBufferPointer());
      }

      errorString[printBlobUtf8->GetBufferSize() - 1] = 0;
      debug_printf("== VALIDATION ERROR =============================================\n%s\n"
                   "== END ==========================================================\n",
                   errorString);

      return false;
   }
   return true;

}

void d3d12_validation_tools::disassemble(struct blob *dxil)
{
   if (!compiler) {
      fprintf(stderr, "D3D12: No Disassembler\n");
      return;
   }
   ShaderBlob source(dxil);
   IDxcBlobEncoding* pDisassembly = nullptr;

   if (FAILED(compiler->Disassemble(&source, &pDisassembly))) {
      fprintf(stderr, "D3D12: Disassembler failed\n");
      return;
   }

   ComPtr<IDxcBlobEncoding> dissassably(pDisassembly);
   ComPtr<IDxcBlobEncoding> blobUtf8;
   library->GetBlobAsUtf8(pDisassembly, blobUtf8.GetAddressOf());
   if (!blobUtf8) {
      fprintf(stderr, "D3D12: Unable to get utf8 encoding\n");
      return;
   }

   char *disassembly = reinterpret_cast<char*>(blobUtf8->GetBufferPointer());
   disassembly[blobUtf8->GetBufferSize() - 1] = 0;

   fprintf(stderr, "== BEGIN SHADER ============================================\n"
           "%s\n"
           "== END SHADER ==============================================\n",
           disassembly);
}

/* Sort io values so that first come normal varyings,
 * then system values, and then system generated values.
 */
static void insert_sorted(struct exec_list *var_list, nir_variable *new_var)
{
   nir_foreach_variable(var, var_list) {
      if (var->data.driver_location > new_var->data.driver_location ||
          (var->data.driver_location == new_var->data.driver_location &&
           var->data.location > new_var->data.location)) {
         exec_node_insert_node_before(&var->node, &new_var->node);
         return;
      }
   }
   exec_list_push_tail(var_list, &new_var->node);
}

/* Order varyings according to driver location */
uint64_t
d3d12_sort_by_driver_location(exec_list *io)
{
   uint64_t result = 0;
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   nir_foreach_variable_safe(var, io) {
      exec_node_remove(&var->node);
      insert_sorted(&new_list, var);
      result |= 1ull << var->data.location;
   }
   exec_list_move_nodes_to(&new_list, io);
   return result;
}

/* Sort PS outputs so that color outputs come first */
void
d3d12_sort_ps_outputs(exec_list *io)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   nir_foreach_variable_safe(var, io) {
      exec_node_remove(&var->node);
      /* We use the driver_location here to avoid introducing a new
       * struct or member variable here. The true, updated driver location
       * will be written below, after sorting */
      switch (var->data.location) {
      case FRAG_RESULT_DEPTH:
         var->data.driver_location = 1;
         break;
      case FRAG_RESULT_STENCIL:
         var->data.driver_location = 2;
         break;
      case FRAG_RESULT_SAMPLE_MASK:
         var->data.driver_location = 3;
         break;
      default:
         var->data.driver_location = 0;
      }
      insert_sorted(&new_list, var);
   }
   exec_list_move_nodes_to(&new_list, io);

   unsigned driver_loc = 0;
   nir_foreach_variable(var, io) {
      var->data.driver_location = driver_loc++;
   }
}

/* Order between stage values so that normal varyings come first,
 * then sysvalues and then system generated values.
 */
uint64_t
d3d12_reassign_driver_locations(exec_list *io, uint64_t other_stage_mask)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   uint64_t result = 0;
   nir_foreach_variable_safe(var, io) {
      exec_node_remove(&var->node);
      /* We use the driver_location here to avoid introducing a new
       * struct or member variable here. The true, updated driver location
       * will be written below, after sorting */
      var->data.driver_location = nir_var_to_dxil_sysvalue_type(var, other_stage_mask);
      insert_sorted(&new_list, var);
   }
   exec_list_move_nodes_to(&new_list, io);

   unsigned driver_loc = 0;
   nir_foreach_variable(var, io) {
      result |= 1ull << var->data.location;
      var->data.driver_location = driver_loc++;
   }
   return result;
}
