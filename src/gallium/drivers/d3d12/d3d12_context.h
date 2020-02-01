/*
 * Copyright 2019 Collabora Ltd.
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

#ifndef D3D12_CONTEXT_H
#define D3D12_CONTEXT_H

#include "d3d12_descriptor_pool.h"

#include "pipe/p_context.h"
#include "pipe/p_state.h"
#include "util/slab.h"

#include <d3d12.h>

#define D3D12_GFX_SHADER_STAGES (PIPE_SHADER_TYPES - 1)

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

struct d3d12_vertex_elements_state {
   D3D12_INPUT_ELEMENT_DESC elements[PIPE_MAX_ATTRIBS];
   unsigned num_elements;
};

struct d3d12_rasterizer_state {
   struct pipe_rasterizer_state base;
   D3D12_RASTERIZER_DESC desc;
};

struct d3d12_blend_state {
   D3D12_BLEND_DESC desc;
   bool need_blend_factor;
};

struct d3d12_depth_stencil_alpha_state {
   D3D12_DEPTH_STENCIL_DESC desc;
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

struct primconvert_context;
struct d3d12_validation_tools;

struct d3d12_context {
   struct pipe_context base;
   struct slab_child_pool transfer_pool;
   struct primconvert_context *primconvert;

   struct pipe_constant_buffer cbufs[PIPE_SHADER_TYPES][PIPE_MAX_CONSTANT_BUFFERS];
   struct pipe_framebuffer_state fb;
   struct d3d12_vertex_elements_state *ves;
   struct pipe_vertex_buffer vbs[PIPE_MAX_ATTRIBS];
   D3D12_VERTEX_BUFFER_VIEW vbvs[PIPE_MAX_ATTRIBS];
   unsigned num_vbs;
   struct pipe_viewport_state viewport_states[PIPE_MAX_VIEWPORTS];
   D3D12_VIEWPORT viewports[PIPE_MAX_VIEWPORTS];
   unsigned num_viewports;
   struct pipe_scissor_state scissor_states[PIPE_MAX_VIEWPORTS];
   D3D12_RECT scissors[PIPE_MAX_VIEWPORTS];
   unsigned num_scissors;
   struct d3d12_blend_state *blend;
   float blend_factor[4];
   struct d3d12_depth_stencil_alpha_state *depth_stencil_alpha_state;
   struct d3d12_rasterizer_state *rast;
   struct pipe_sampler_view *sampler_views[PIPE_SHADER_TYPES][PIPE_MAX_SHADER_SAMPLER_VIEWS];
   unsigned num_sampler_views[PIPE_SHADER_TYPES];

   struct d3d12_shader *gfx_stages[D3D12_GFX_SHADER_STAGES];
   unsigned dirty_program : 1;

   HANDLE event;
   ID3D12Fence *cmdqueue_fence;
   int fence_value;
   ID3D12CommandAllocator *cmdalloc;
   ID3D12GraphicsCommandList *cmdlist;

   struct d3d12_descriptor_heap *rtv_heap;
   struct d3d12_descriptor_heap *dsv_heap;
   struct d3d12_descriptor_pool *view_pool;
   struct d3d12_descriptor_heap *view_heap;

   struct d3d12_descriptor_handle null_srvs[RESOURCE_DIMENSION_COUNT];

   PFN_D3D12_SERIALIZE_VERSIONED_ROOT_SIGNATURE D3D12SerializeVersionedRootSignature;
   struct d3d12_validation_tools *validation_tools;

};

static inline struct d3d12_context *
d3d12_context(struct pipe_context *context)
{
   return (struct d3d12_context *)context;
}

struct pipe_context *
d3d12_context_create(struct pipe_screen *pscreen, void *priv, unsigned flags);

void
d3d12_flush_cmdlist(struct d3d12_context *ctx);

void
d3d12_resource_barrier(struct d3d12_context *ctx,
                       struct d3d12_resource *res,
                       D3D12_RESOURCE_STATES before,
                       D3D12_RESOURCE_STATES after);

void
d3d12_draw_vbo(struct pipe_context *pctx,
               const struct pipe_draw_info *dinfo);

#endif
