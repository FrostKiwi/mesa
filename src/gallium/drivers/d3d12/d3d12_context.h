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

#ifndef D3D12_CONTEXT_H
#define D3D12_CONTEXT_H

#include "d3d12_batch.h"
#include "d3d12_descriptor_pool.h"
#include "d3d12_pipeline_state.h"

#include "pipe/p_context.h"
#include "pipe/p_state.h"
#include "util/list.h"
#include "util/slab.h"
#include "util/u_suballoc.h"

#include <d3d12.h>

#define D3D12_GFX_SHADER_STAGES (PIPE_SHADER_TYPES - 1)
#define D3D12_MAX_POINT_SIZE 255.0f

enum d3d12_dirty_flags
{
   D3D12_DIRTY_NONE             = 0,
   D3D12_DIRTY_BLEND            = (1 << 0),
   D3D12_DIRTY_RASTERIZER       = (1 << 1),
   D3D12_DIRTY_ZSA              = (1 << 2),
   D3D12_DIRTY_VERTEX_ELEMENTS  = (1 << 3),
   D3D12_DIRTY_BLEND_COLOR      = (1 << 4),
   D3D12_DIRTY_STENCIL_REF      = (1 << 5),
   D3D12_DIRTY_SAMPLE_MASK      = (1 << 6),
   D3D12_DIRTY_VIEWPORT         = (1 << 7),
   D3D12_DIRTY_FRAMEBUFFER      = (1 << 8),
   D3D12_DIRTY_SCISSOR          = (1 << 9),
   D3D12_DIRTY_VERTEX_BUFFERS   = (1 << 10),
   D3D12_DIRTY_INDEX_BUFFER     = (1 << 11),
   D3D12_DIRTY_PRIM_MODE        = (1 << 12),
   D3D12_DIRTY_SHADER           = (1 << 13),
   D3D12_DIRTY_ROOT_SIGNATURE   = (1 << 14),
};

enum d3d12_shader_dirty_flags
{
   D3D12_SHADER_DIRTY_CONSTBUF      = (1 << 0),
   D3D12_SHADER_DIRTY_SAMPLER_VIEWS = (1 << 1),
   D3D12_SHADER_DIRTY_SAMPLERS      = (1 << 2),
};

#define D3D12_DIRTY_PSO (D3D12_DIRTY_BLEND | D3D12_DIRTY_RASTERIZER | D3D12_DIRTY_ZSA | \
                         D3D12_DIRTY_FRAMEBUFFER | D3D12_DIRTY_SAMPLE_MASK | \
                         D3D12_DIRTY_VERTEX_ELEMENTS | D3D12_DIRTY_PRIM_MODE | \
                         D3D12_DIRTY_SHADER | D3D12_DIRTY_ROOT_SIGNATURE)

#define D3D12_SHADER_DIRTY_ALL (D3D12_SHADER_DIRTY_CONSTBUF | D3D12_SHADER_DIRTY_SAMPLER_VIEWS | \
                                D3D12_SHADER_DIRTY_SAMPLERS)


enum resource_dimension
{
   RESOURCE_DIMENSION_UNKNOWN = 0,
   RESOURCE_DIMENSION_BUFFER = 1,
   RESOURCE_DIMENSION_TEXTURE1D = 2,
   RESOURCE_DIMENSION_TEXTURE2D = 3,
   RESOURCE_DIMENSION_TEXTURE2DMS = 4,
   RESOURCE_DIMENSION_TEXTURE3D = 5,
   RESOURCE_DIMENSION_TEXTURECUBE = 6,
   RESOURCE_DIMENSION_TEXTURE1DARRAY = 7,
   RESOURCE_DIMENSION_TEXTURE2DARRAY = 8,
   RESOURCE_DIMENSION_TEXTURE2DMSARRAY = 9,
   RESOURCE_DIMENSION_TEXTURECUBEARRAY = 10,
   RESOURCE_DIMENSION_COUNT
};

struct d3d12_sampler_state {
   D3D12_STATIC_SAMPLER_DESC desc;
   struct d3d12_descriptor_handle handle;
};

enum d3d12_blend_factor_flags {
   D3D12_BLEND_FACTOR_NONE  = 0,
   D3D12_BLEND_FACTOR_COLOR = 1 << 0,
   D3D12_BLEND_FACTOR_ALPHA = 1 << 1,
   D3D12_BLEND_FACTOR_ANY   = 1 << 2,
};

struct d3d12_sampler_view {
   struct pipe_sampler_view base;
   struct d3d12_descriptor_handle handle;
};

static inline struct d3d12_sampler_view *
d3d12_sampler_view(struct pipe_sampler_view *pview)
{
   return (struct d3d12_sampler_view *)pview;
}

struct d3d12_shader_state {
   struct d3d12_shader *current;
   unsigned state_dirty;
};

struct blitter_context;
struct primconvert_context;
struct d3d12_validation_tools;

struct d3d12_context {
   struct pipe_context base;
   struct slab_child_pool transfer_pool;
   struct primconvert_context *primconvert;
   struct blitter_context *blitter;
   struct u_suballocator *query_allocator;
   struct hash_table *pso_cache;

   struct d3d12_batch batches[4];
   unsigned current_batch_idx;

   struct pipe_constant_buffer cbufs[PIPE_SHADER_TYPES][PIPE_MAX_CONSTANT_BUFFERS];
   struct pipe_framebuffer_state fb;
   struct pipe_vertex_buffer vbs[PIPE_MAX_ATTRIBS];
   D3D12_VERTEX_BUFFER_VIEW vbvs[PIPE_MAX_ATTRIBS];
   unsigned num_vbs;
   float flip_y;
   struct pipe_viewport_state viewport_states[PIPE_MAX_VIEWPORTS];
   D3D12_VIEWPORT viewports[PIPE_MAX_VIEWPORTS];
   unsigned num_viewports;
   struct pipe_scissor_state scissor_states[PIPE_MAX_VIEWPORTS];
   D3D12_RECT scissors[PIPE_MAX_VIEWPORTS];
   unsigned num_scissors;
   float blend_factor[4];
   struct pipe_stencil_ref stencil_ref;
   struct pipe_sampler_view *sampler_views[PIPE_SHADER_TYPES][PIPE_MAX_SHADER_SAMPLER_VIEWS];
   unsigned num_sampler_views[PIPE_SHADER_TYPES];
   struct d3d12_sampler_state *samplers[PIPE_SHADER_TYPES][PIPE_MAX_SHADER_SAMPLER_VIEWS];
   unsigned num_samplers[PIPE_SHADER_TYPES];
   D3D12_INDEX_BUFFER_VIEW ibv;

   struct d3d12_shader_selector *gfx_stages[D3D12_GFX_SHADER_STAGES];

   struct d3d12_gfx_pipeline_state gfx_pipeline_state;
   unsigned shader_dirty[D3D12_GFX_SHADER_STAGES];
   unsigned state_dirty;
   unsigned cmdlist_dirty;
   ID3D12PipelineState *current_pso;

   ID3D12Fence *cmdqueue_fence;
   int fence_value;
   ID3D12GraphicsCommandList *cmdlist;

   struct list_head active_queries;
   bool queries_disabled;

   struct d3d12_descriptor_heap *rtv_heap;
   struct d3d12_descriptor_heap *dsv_heap;
   struct d3d12_descriptor_pool *sampler_pool;
   struct d3d12_descriptor_pool *view_pool;

   struct d3d12_descriptor_handle null_srvs[RESOURCE_DIMENSION_COUNT];
   struct d3d12_descriptor_handle null_sampler;

   PFN_D3D12_SERIALIZE_VERSIONED_ROOT_SIGNATURE D3D12SerializeVersionedRootSignature;
   struct d3d12_validation_tools *validation_tools;

};

static inline struct d3d12_context *
d3d12_context(struct pipe_context *context)
{
   return (struct d3d12_context *)context;
}

static inline struct d3d12_batch *
d3d12_current_batch(struct d3d12_context *ctx)
{
   assert(ctx->current_batch_idx < ARRAY_SIZE(ctx->batches));
   return ctx->batches + ctx->current_batch_idx;
}

struct pipe_context *
d3d12_context_create(struct pipe_screen *pscreen, void *priv, unsigned flags);

void
d3d12_flush_cmdlist(struct d3d12_context *ctx);

void
d3d12_flush_cmdlist_and_wait(struct d3d12_context *ctx);

void
d3d12_resource_barrier(struct d3d12_context *ctx,
                       struct d3d12_resource *res,
                       D3D12_RESOURCE_STATES before,
                       D3D12_RESOURCE_STATES after);

void
d3d12_draw_vbo(struct pipe_context *pctx,
               const struct pipe_draw_info *dinfo);

void
d3d12_blit(struct pipe_context *pctx,
           const struct pipe_blit_info *info);

void
d3d12_context_query_init(struct pipe_context *pctx);

#endif
