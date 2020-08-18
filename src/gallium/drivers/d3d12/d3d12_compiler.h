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

#ifndef D3D12_COMPILER_H
#define D3D12_COMPILER_H

#include "d3d12_context.h"

#include "pipe/p_defines.h"
#include "pipe/p_state.h"

#include "compiler/shader_info.h"
#include "program/prog_statevars.h"

#include "nir.h"

struct pipe_screen;

#ifdef __cplusplus
extern "C" {
#endif

struct d3d12_validation_tools *d3d12_validator_create();

void d3d12_validator_destroy(struct d3d12_validation_tools *validator);

const void *
d3d12_get_compiler_options(struct pipe_screen *screen,
                           enum pipe_shader_ir ir,
                           enum pipe_shader_type shader);

struct d3d12_shader_key {
   enum pipe_shader_type stage;

   uint64_t required_varying_inputs;
   uint64_t required_varying_outputs;
   uint64_t next_varying_inputs;
   uint64_t prev_varying_outputs;
   unsigned last_vertex_processing_stage : 1;
   unsigned invert_depth : 1;
   unsigned samples_int_textures : 1;

   struct {
      unsigned needs_format_emulation:1;
      enum pipe_format format_conversion[PIPE_MAX_ATTRIBS];
   } vs;

   struct {
      unsigned sprite_coord_enable:24;
      unsigned sprite_origin_upper_left:1;
      unsigned point_pos_stream_out:1;
      unsigned writes_psize:1;
      unsigned aa_point:1;
      unsigned stream_output_factor:3;
   } gs;

   struct {
      unsigned missing_dual_src_outputs : 2;
      unsigned frag_result_color_lowering : 4;
      unsigned provoking_vertex : 2;
      unsigned manual_depth_range : 1;
      unsigned polygon_stipple : 1;
   } fs;

   int n_texture_states;
   dxil_wrap_sampler_state tex_wrap_states[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   dxil_texture_swizzle_state swizzle_state[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   enum compare_func sampler_compare_funcs[PIPE_MAX_SHADER_SAMPLER_VIEWS];
};

struct d3d12_shader {
   void *bytecode;
   size_t bytecode_length;

   nir_shader *nir;

   struct {
      unsigned binding;
   } cb_bindings[PIPE_MAX_CONSTANT_BUFFERS];
   size_t num_cb_bindings;

   struct {
      enum d3d12_state_var var;
      unsigned offset;
   } state_vars[D3D12_MAX_STATE_VARS];
   unsigned num_state_vars;
   size_t state_vars_size;
   bool state_vars_used;

   struct {
      int index;
      int binding;
      uint32_t dimension;
   } srv_bindings[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   size_t num_srv_bindings;

   bool has_default_ubo0;
   unsigned pstipple_binding;

   struct d3d12_shader_key key;
   struct d3d12_shader *next_variant;
};

struct d3d12_gs_variant_key
{
   unsigned passthrough:1;

   struct {
      const struct glsl_type *type;
      unsigned interpolation:3;   // INTERP_MODE_COUNT = 5
      unsigned driver_location:6; // VARYING_SLOT_MAX = 64
   } varyings[VARYING_SLOT_MAX];
   uint64_t varyings_mask;

};

struct d3d12_shader_selector {
   enum pipe_shader_type stage;
   nir_shader *initial;
   struct d3d12_shader *first;
   struct d3d12_shader *current;

   struct pipe_stream_output_info so_info;
   uint64_t enabled_stream_outputs;

   unsigned samples_int_textures:1;
   unsigned compare_with_lod_bias_grad:1;

   bool is_gs_variant;
   struct d3d12_gs_variant_key gs_key;
};


struct d3d12_shader_selector *
d3d12_create_shader(struct d3d12_context *ctx,
                    enum pipe_shader_type stage,
                    const struct pipe_shader_state *shader);

void
d3d12_shader_free(struct d3d12_shader_selector *shader);

void
d3d12_select_shader_variants(struct d3d12_context *ctx,
                             const struct pipe_draw_info *dinfo);

void
d3d12_gs_variant_cache_init(struct d3d12_context *ctx);

void
d3d12_gs_variant_cache_destroy(struct d3d12_context *ctx);

struct d3d12_shader_selector *
d3d12_get_gs_variant(struct d3d12_context *ctx, struct d3d12_gs_variant_key *key);

uint64_t
d3d12_reassign_driver_locations(struct exec_list *io, uint64_t other_stage_mask);

uint64_t
d3d12_sort_by_driver_location(struct exec_list *io);

void
d3d12_sort_ps_outputs(struct exec_list *io);

#ifdef __cplusplus
}
#endif

#endif
