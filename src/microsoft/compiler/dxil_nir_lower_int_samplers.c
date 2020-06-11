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

#include "dxil_nir_lower_int_samplers.h"
#include "nir_builder.h"
#include "nir_builtin_builder.h"

bool
lower_sample_to_txf_for_integer_tex_filter(const nir_instr *instr,
                                           UNUSED const void *_options)
{
   if (instr->type != nir_instr_type_tex)
      return false;

   nir_tex_instr *tex = nir_instr_as_tex(instr);
   if (tex->op != nir_texop_tex &&
       tex->op != nir_texop_txb &&
       tex->op != nir_texop_txl &&
       tex->op != nir_texop_txd)
      return false;

   return (tex->dest_type & (nir_type_int | nir_type_uint));
}

nir_ssa_def *
dx_get_texture_lod(nir_builder *b, nir_tex_instr *tex)
{
   nir_tex_instr *tql;

   unsigned num_srcs = 0;
   for (unsigned i = 0; i < tex->num_srcs; i++) {
      if (tex->src[i].src_type == nir_tex_src_coord ||
          tex->src[i].src_type == nir_tex_src_texture_deref ||
          tex->src[i].src_type == nir_tex_src_sampler_deref ||
          tex->src[i].src_type == nir_tex_src_texture_offset ||
          tex->src[i].src_type == nir_tex_src_sampler_offset ||
          tex->src[i].src_type == nir_tex_src_texture_handle ||
          tex->src[i].src_type == nir_tex_src_sampler_handle)
         num_srcs++;
   }

   tql = nir_tex_instr_create(b->shader, num_srcs);
   tql->op = nir_texop_lod;
   tql->coord_components = tex->coord_components;
   tql->sampler_dim = tex->sampler_dim;
   tql->is_array = tex->is_array;
   tql->is_shadow = tex->is_shadow;
   tql->is_new_style_shadow = tex->is_new_style_shadow;
   tql->texture_index = tex->texture_index;
   tql->sampler_index = tex->sampler_index;
   tql->dest_type = nir_type_float;

   unsigned idx = 0;
   for (unsigned i = 0; i < tex->num_srcs; i++) {
      if (tex->src[i].src_type == nir_tex_src_coord ||
          tex->src[i].src_type == nir_tex_src_texture_deref ||
          tex->src[i].src_type == nir_tex_src_sampler_deref ||
          tex->src[i].src_type == nir_tex_src_texture_offset ||
          tex->src[i].src_type == nir_tex_src_sampler_offset ||
          tex->src[i].src_type == nir_tex_src_texture_handle ||
          tex->src[i].src_type == nir_tex_src_sampler_handle) {
         nir_src_copy(&tql->src[idx].src, &tex->src[i].src, tql);
         tql->src[idx].src_type = tex->src[i].src_type;
         idx++;
      }
   }

   nir_ssa_dest_init(&tql->instr, &tql->dest, 2, 32, NULL);
   nir_builder_instr_insert(b, &tql->instr);

   /* DirectX LOD only has a value in x channel */
   return nir_channel(b, &tql->dest.ssa, 0);
}

typedef struct {
   nir_ssa_def *coords;
   nir_ssa_def *use_border_color;
   nir_ssa_def *size_correction;
} wrap_result_t;

typedef struct {
   nir_ssa_def *lod;
   nir_ssa_def *size;
   int ncoord_comp;
   wrap_result_t wrap[3];
} wrap_lower_param_t;

static void
wrap_clamp_to_edge(nir_builder *b, wrap_result_t *wrap_params)
{
   /* clamp coordinate */
   wrap_params->coords = nir_fsat(b, wrap_params->coords);

   /* Correct the final lookup coordinate to be inside the texture */
   wrap_params->size_correction = nir_imm_float(b, -.1);
}

static void
wrap_repeat(nir_builder *b, wrap_result_t *wrap_params)
{
   /* For repeate we just strip the integer part of the coordinate */
   wrap_params->coords = nir_ffract(b, wrap_params->coords);
}

static void
wrap_mirror_repeat(nir_builder *b, wrap_result_t *wrap_params)
{
   // 1 - |2 * frac( 0.5 * |c|) - 1|
   nir_ssa_def *a = nir_fmul_imm(b, nir_fabs(b, wrap_params->coords), 0.5);
   a = nir_fadd_imm(b, nir_fmul_imm(b, nir_ffract(b, a), 2.0), -1.0);
   wrap_params->coords = nir_fadd_imm(b, nir_fneg(b, nir_fabs(b, a)), 1.0);
}

static void
wrap_mirror_clamp_to_edge(nir_builder *b, wrap_result_t *wrap_params)
{
   /* This needs a test */
   nir_ssa_def *a = nir_fsat(b, nir_fmul_imm(b, nir_fabs(b, wrap_params->coords), 0.5));
   a = nir_fadd_imm(b, nir_fmul_imm(b, a, 2.0), -1.0);
   wrap_params->coords = nir_fadd_imm(b, nir_fneg(b, nir_fabs(b, a)), 1.0);

   /* Correct the final lookup coordinate to be inside the texture */
   wrap_params->size_correction = nir_imm_float(b, -.1);
}

static void
wrap_clamp(nir_builder *b, wrap_result_t *wrap_params)
{
   /* For clamping we have to check whether the coordinate is in [0,1) */
   nir_ssa_def *is_low = nir_flt(b, wrap_params->coords, nir_imm_float(b, 0.0 ));
   nir_ssa_def *is_high = nir_fge(b, wrap_params->coords, nir_imm_float(b, 1.0 ));
   wrap_params->use_border_color = nir_ior(b, is_low, is_high);
}

static void
wrap_mirror_clamp(nir_builder *b, wrap_result_t *wrap_params)
{
   /* Within the boundaries this acts like mirror_repeat */
   wrap_mirror_repeat(b, wrap_params);

   /* We still have to takle care of the boundaries */
   nir_ssa_def *is_low = nir_flt(b, wrap_params->coords, nir_imm_float(b, -1.0 ));
   nir_ssa_def *is_high = nir_flt(b,nir_imm_float(b, 2.0 ), wrap_params->coords);
   wrap_params->use_border_color = nir_ior(b, is_low, is_high);
}

static wrap_result_t
wrap_coords(nir_builder *b, nir_ssa_def *coords, enum pipe_tex_wrap wrap)
{
   wrap_result_t result = {coords, nir_imm_false(b), nir_imm_int(b, 0)};

   switch (wrap) {
   case PIPE_TEX_WRAP_CLAMP:
   case PIPE_TEX_WRAP_CLAMP_TO_EDGE:
      wrap_clamp_to_edge(b, &result);
      break;
   case PIPE_TEX_WRAP_REPEAT:
      wrap_repeat(b, &result);
      break;
   case PIPE_TEX_WRAP_MIRROR_REPEAT:
      wrap_mirror_repeat(b, &result);
      break;
   case PIPE_TEX_WRAP_MIRROR_CLAMP:
   case PIPE_TEX_WRAP_MIRROR_CLAMP_TO_EDGE:
      wrap_mirror_clamp_to_edge(b, &result);
      break;
   case PIPE_TEX_WRAP_CLAMP_TO_BORDER:
      wrap_clamp(b, &result);
      break;
   case PIPE_TEX_WRAP_MIRROR_CLAMP_TO_BORDER:
      wrap_mirror_clamp(b, &result);
      break;
   }
   return result;
}

static nir_ssa_def *
load_bordercolor(nir_builder *b, nir_tex_instr *tex, dxil_wrap_sampler_state *active_state)
{
   nir_const_value const_value[4] = {{0}};
   int ndest_comp = nir_dest_num_components(tex->dest);

   for (int i = 0; i < ndest_comp; ++i)
      const_value[i].f32 = active_state->border_color[i];

   return nir_build_imm(b, ndest_comp, 32, const_value);
}

nir_tex_instr *
create_txf_from_tex(nir_builder *b, nir_tex_instr *tex)
{
   nir_tex_instr *txf;

   unsigned num_srcs = 0;
   for (unsigned i = 0; i < tex->num_srcs; i++) {
      if (tex->src[i].src_type == nir_tex_src_texture_deref ||
          tex->src[i].src_type == nir_tex_src_texture_offset ||
          tex->src[i].src_type == nir_tex_src_texture_handle)
         num_srcs++;
   }

   txf = nir_tex_instr_create(b->shader, num_srcs);
   txf->op = nir_texop_txf;
   txf->sampler_dim = tex->sampler_dim;
   txf->is_array = tex->is_array;
   txf->is_shadow = tex->is_shadow;
   txf->is_new_style_shadow = tex->is_new_style_shadow;
   txf->texture_index = tex->texture_index;
   txf->sampler_index = tex->sampler_index;
   txf->dest_type = tex->dest_type;

   unsigned idx = 0;
   for (unsigned i = 0; i < tex->num_srcs; i++) {
      if (tex->src[i].src_type == nir_tex_src_texture_deref ||
          tex->src[i].src_type == nir_tex_src_texture_offset ||
          tex->src[i].src_type == nir_tex_src_texture_handle) {
         nir_src_copy(&txf->src[idx].src, &tex->src[i].src, txf);
         txf->src[idx].src_type = tex->src[i].src_type;
         idx++;
      }
   }

   nir_ssa_dest_init(&txf->instr, &txf->dest,
                     nir_tex_instr_dest_size(txf), 32, NULL);
   nir_builder_instr_insert(b, &txf->instr);

   return txf;
}

static nir_ssa_def *
load_texel(nir_builder *b, nir_tex_instr *tex, wrap_lower_param_t *params)
{
   nir_ssa_def *texcoord = NULL;
   nir_ssa_def *size_corr = NULL;

   /* Put coordinates back together */
   switch (params->ncoord_comp) {
   case 1:
      texcoord = params->wrap[0].coords;
      size_corr = params->wrap[0].size_correction;
      break;
   case 2:
      texcoord = nir_vec2(b, params->wrap[0].coords, params->wrap[1].coords);
      size_corr = nir_vec2(b, params->wrap[0].size_correction, params->wrap[1].size_correction);
      break;
   case 3:
      texcoord = nir_vec3(b, params->wrap[0].coords, params->wrap[1].coords, params->wrap[2].coords);
      size_corr = nir_vec3(b, params->wrap[0].size_correction, params->wrap[1].size_correction,
            params->wrap[2].size_correction);
      break;
   default:
      ;
   }

   /* Evaluate the integer lookup coordinates for the requested LOD, don't touch the
    * array index */
   nir_ssa_def *new_coord = NULL;
   if (!tex->is_array) {
      new_coord = nir_f2i32(b, nir_fadd(b, nir_fmul(b, params->size, texcoord), size_corr));
   } else {
      nir_ssa_def *array_index = nir_channel(b, texcoord, 1 << params->ncoord_comp);
      int mask = (1 << params->ncoord_comp) - 1;
      nir_ssa_def *coord =
            nir_f2i32(b, nir_fmul(b, nir_fadd(b, nir_channels(b, params->size, mask),
                                  nir_channels(b, texcoord, mask)), size_corr));
      switch (params->ncoord_comp) {
      case 1: new_coord = nir_vec2(b, coord, array_index);
      case 2: new_coord = nir_vec3(b, nir_channel(b, coord, 0),
                                   nir_channel(b, coord, 1),
                                   array_index);
      default:
         unreachable("unsupported number of non-array coordinates");
      }
   }

   nir_tex_instr *load = create_txf_from_tex(b, tex);
   nir_tex_instr_add_src(load, nir_tex_src_lod, nir_src_for_ssa(params->lod));
   nir_tex_instr_add_src(load, nir_tex_src_coord, nir_src_for_ssa(new_coord));
   b->cursor = nir_after_instr(&load->instr);
   return &load->dest.ssa;
}


static nir_ssa_def *
lower_sample_to_txf_for_integer_tex_impl(nir_builder *b, nir_instr *instr,
                                         void *options)
{
   dxil_wrap_sampler_states *states = (dxil_wrap_sampler_states *)options;
   wrap_lower_param_t params = {0};

   nir_tex_instr *tex = nir_instr_as_tex(instr);
   dxil_wrap_sampler_state *active_state = &states->states[tex->sampler_index];

   b->cursor = nir_before_instr(instr);

   int coord_index = nir_tex_instr_src_index(tex, nir_tex_src_coord);
   nir_ssa_def *old_coord = tex->src[coord_index].src.ssa;
   params.ncoord_comp = nir_src_num_components(tex->src[coord_index].src);
   if (tex->is_array)
      params.ncoord_comp -= 1;

   /* This helper to get the texture size always uses LOD 0, and DirectX doesn't support
    * giving another LOD when querying the texture size */
   nir_ssa_def *size0 = nir_get_texture_size(b, tex);

   /* Evaluate the LOD to be used for the texel fetch */
   if (unlikely(tex->op == nir_texop_txl)) {
      int lod_index = nir_tex_instr_src_index(tex, nir_tex_src_lod);
      /* if we have an explicite LOD, take it, otherwise evaluate it */
      params.lod = tex->src[lod_index].src.ssa;
   } else if (unlikely(tex->op == nir_texop_txd)) {
      int ddx_index = nir_tex_instr_src_index(tex, nir_tex_src_ddx);
      int ddy_index = nir_tex_instr_src_index(tex, nir_tex_src_ddy);
      assert(ddx_index >= 0 && ddy_index >= 0);

      nir_ssa_def *grad = nir_fmax(b,
                                   tex->src[ddx_index].src.ssa,
                                   tex->src[ddy_index].src.ssa);

      nir_ssa_def *r = nir_fmul(b, grad, nir_i2f32(b, size0));
      nir_ssa_def *rho = nir_channel(b, r, 0);
      for (int i = 1; i < params.ncoord_comp; ++i)
         rho = nir_fmax(b, rho, nir_channel(b, r, i));
      params.lod = nir_flog2(b, rho);
   } else {
      params.lod = dx_get_texture_lod(b, tex);
   }

   /* Apply LOD bias */
   if (unlikely(tex->op == nir_texop_txb)) {
      int bias_index = nir_tex_instr_src_index(tex, nir_tex_src_bias);
      params.lod = nir_fadd(b, params.lod, tex->src[bias_index].src.ssa);
   }
   params.lod = nir_f2i32(b, params.lod);
   params.size = nir_i2f32(b, nir_ishr(b, size0, params.lod));

   nir_ssa_def *coord_help[3];

   for (int i = 0; i < params.ncoord_comp; ++i)
      coord_help[i] = nir_channel(b, old_coord, i);

   /* Correct the texture coordinates for the offsets, but offsets are in pixel space
    * and handling the boundary coordinates in pixel space is ugly, so scale.
    */
   int offset_index = nir_tex_instr_src_index(tex, nir_tex_src_offset);
   if (offset_index >= 0) {
      nir_ssa_def *offset = tex->src[offset_index].src.ssa;
      nir_ssa_def *scale = nir_frcp(b, nir_channels(b, params.size, (1 << params.ncoord_comp) - 1));
      nir_ssa_def *shift = nir_fmul(b, nir_i2f32(b, offset), scale);
      for (int i = 0; i < params.ncoord_comp; ++i)
         coord_help[i] = nir_fadd(b, coord_help[i], nir_channel(b, shift, i));
   }

   nir_ssa_def *use_border_color = nir_imm_false(b);

   switch (params.ncoord_comp) {
   case 3:
      params.wrap[2] = wrap_coords(b, coord_help[2], active_state->wrap_t);
      use_border_color = nir_ior(b, use_border_color, params.wrap[2].use_border_color);
      /* fallthrough */
   case 2:
      params.wrap[1] = wrap_coords(b, coord_help[1], active_state->wrap_s);
      use_border_color = nir_ior(b, use_border_color, params.wrap[1].use_border_color);
      /* fallthrough */
   case 1:
      params.wrap[0] = wrap_coords(b, coord_help[0], active_state->wrap_r);
      use_border_color = nir_ior(b, use_border_color, params.wrap[0].use_border_color);
      break;
   default:
      unreachable("unsupported coordinate count");
   }

   nir_if *border_if = nir_push_if(b, use_border_color);
   nir_ssa_def *border_color = load_bordercolor(b, tex, active_state);
   nir_if *border_else = nir_push_else(b, border_if);
   nir_ssa_def *sampler_color = load_texel(b, tex, &params);
   nir_pop_if(b, border_else);

   return nir_if_phi(b, border_color, sampler_color);
}

/* Sampling from integer textures is not allowed in DirectX, so we have
 * to use texel fetches. For this we have to scale the coordiantes
 * to be integer based, and evaluate the LOD the texel fetch has to be
 * applied on, and take care of the boundary conditions .
 */
bool
dxil_lower_sample_to_txf_for_integer_tex(nir_shader *s,
                                         dxil_wrap_sampler_states *state)
{
   if (!state)
      return false;

   bool result =
         nir_shader_lower_instructions(s,
                                       lower_sample_to_txf_for_integer_tex_filter,
                                       lower_sample_to_txf_for_integer_tex_impl,
                                       state);
   return result;
}
