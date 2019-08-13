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

#include "d3d12_context.h"

#include "d3d12_format.h"
#include "d3d12_resource.h"
#include "d3d12_screen.h"
#include "d3d12_surface.h"

#include "util/u_framebuffer.h"
#include "util/u_memory.h"

static void
d3d12_context_destroy(struct pipe_context *pctx)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   slab_destroy_child(&ctx->transfer_pool);
   FREE(ctx);
}

static void *
d3d12_create_vertex_elements_state(struct pipe_context *pctx,
                                   unsigned num_elements,
                                   const struct pipe_vertex_element *elements)
{
   return NULL;
}

static void
d3d12_bind_vertex_elements_state(struct pipe_context *pctx,
                                 void *ve)
{
}

static void
d3d12_delete_vertex_elements_state(struct pipe_context *pctx,
                                   void *ve)
{
}

static void *
d3d12_create_blend_state(struct pipe_context *pctx,
                         const struct pipe_blend_state *blend_state)
{
   return NULL;
}

static void
d3d12_bind_blend_state(struct pipe_context *pctx, void *blend_state)
{
}

static void
d3d12_delete_blend_state(struct pipe_context *pctx, void *blend_state)
{
}

static void *
d3d12_create_depth_stencil_alpha_state(struct pipe_context *pctx,
                                       const struct pipe_depth_stencil_alpha_state *blend_state)
{
   return NULL;
}

static void
d3d12_bind_depth_stencil_alpha_state(struct pipe_context *pctx,
                                     void *blend_state)
{
}

static void
d3d12_delete_depth_stencil_alpha_state(struct pipe_context *pctx,
                                       void *dsa_state)
{
}

static void *
d3d12_create_rasterizer_state(struct pipe_context *pctx,
                              const struct pipe_rasterizer_state *rs_state)
{
   return NULL;
}

static void
d3d12_bind_rasterizer_state(struct pipe_context *pctx, void *rs_state)
{
}

static void
d3d12_delete_rasterizer_state(struct pipe_context *pctx, void *rs_state)
{
}

static void *
d3d12_create_sampler_state(struct pipe_context *pctx,
                           const struct pipe_sampler_state *state)
{
   return NULL;
}

static void
d3d12_bind_sampler_states(struct pipe_context *pctx,
                          enum pipe_shader_type shader,
                          unsigned start_slot,
                          unsigned num_samplers,
                          void **samplers)
{
}

static void
d3d12_delete_sampler_state(struct pipe_context *pctx,
                           void *ss)
{
}

static struct pipe_sampler_view *
d3d12_create_sampler_view(struct pipe_context *pctx,
                          struct pipe_resource *texture,
                          const struct pipe_sampler_view *state)
{
   return NULL;
}

static void
d3d12_set_sampler_views(struct pipe_context *pctx,
                        enum pipe_shader_type shader_type,
                        unsigned start_slot,
                        unsigned num_views,
                        struct pipe_sampler_view **views)
{
}

static void
d3d12_destroy_sampler_view(struct pipe_context *pctx,
                           struct pipe_sampler_view *view)
{
}

static void *
d3d12_create_vs_state(struct pipe_context *pctx,
                      const struct pipe_shader_state *shader)
{
   return NULL;
}

static void
d3d12_bind_vs_state(struct pipe_context *pctx,
                    void *vss)
{
}

static void
d3d12_delete_vs_state(struct pipe_context *pctx,
                      void *vs)
{
}

static void *
d3d12_create_fs_state(struct pipe_context *pctx,
                      const struct pipe_shader_state *shader)
{
   return NULL;
}

static void
d3d12_bind_fs_state(struct pipe_context *pctx,
                    void *vss)
{
}

static void
d3d12_delete_fs_state(struct pipe_context *pctx,
                      void *vs)
{
}

static void
d3d12_set_polygon_stipple(struct pipe_context *pctx,
                          const struct pipe_poly_stipple *ps)
{
}

static void
d3d12_set_vertex_buffers(struct pipe_context *pctx,
                         unsigned start_slot,
                         unsigned num_buffers,
                         const struct pipe_vertex_buffer *buffers)
{
}

static void
d3d12_set_viewport_states(struct pipe_context *pctx,
                          unsigned start_slot,
                          unsigned num_viewports,
                          const struct pipe_viewport_state *state)
{
}

static void
d3d12_set_constant_buffer(struct pipe_context *pctx,
                          enum pipe_shader_type shader, uint index,
                          const struct pipe_constant_buffer *buf)
{
}

static void
d3d12_set_framebuffer_state(struct pipe_context *pctx,
                            const struct pipe_framebuffer_state *state)
{
   util_copy_framebuffer_state(&d3d12_context(pctx)->fb, state);
}

void
d3d12_flush_cmdlist(struct d3d12_context *ctx)
{
   if (FAILED(ctx->cmdlist->Close())) {
      debug_printf("D3D12: closing ID3D12GraphicsCommandList failed\n");
      return;
   }

   ID3D12CommandList* cmdlists[] = { ctx->cmdlist };
   ctx->cmdqueue->ExecuteCommandLists(1, cmdlists);
   int value = ++ctx->fence_value;
   ctx->cmdqueue_fence->SetEventOnCompletion(value, ctx->event);
   ctx->cmdqueue->Signal(ctx->cmdqueue_fence, value);
   WaitForSingleObject(ctx->event, INFINITE);

   if (FAILED(ctx->cmdalloc->Reset())) {
      debug_printf("D3D12: resetting ID3D12CommandAllocator failed\n");
      return;
   }

   if (FAILED(ctx->cmdlist->Reset(ctx->cmdalloc, NULL))) {
      debug_printf("D3D12: resetting ID3D12GraphicsCommandList failed\n");
      return;
   }
}

static void
d3d12_clear(struct pipe_context *pctx,
            unsigned buffers,
            const union pipe_color_union *color,
            double depth, unsigned stencil)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   if (buffers & PIPE_CLEAR_COLOR) {
      for (int i = 0; i < ctx->fb.nr_cbufs; ++i) {
         struct d3d12_surface* surf = d3d12_surface(ctx->fb.cbufs[i]);
         ctx->cmdlist->ClearRenderTargetView(surf->desc_handle, color->f,
                                             0, NULL);
      }
   }

   if (buffers & PIPE_CLEAR_DEPTHSTENCIL && ctx->fb.zsbuf) {
      struct d3d12_surface* surf = d3d12_surface(ctx->fb.zsbuf);

      D3D12_CLEAR_FLAGS flags = (D3D12_CLEAR_FLAGS)0;
      if (buffers & PIPE_CLEAR_DEPTH)
         flags |= D3D12_CLEAR_FLAG_DEPTH;
      if (buffers & PIPE_CLEAR_STENCIL)
         flags |= D3D12_CLEAR_FLAG_STENCIL;

      ctx->cmdlist->ClearDepthStencilView(surf->desc_handle, flags,
                                          depth, stencil, 0, NULL);
   }

   d3d12_flush_cmdlist(ctx);
}

static void
d3d12_flush(struct pipe_context *pipe,
            struct pipe_fence_handle **fence,
            unsigned flags)
{
}

struct pipe_context *
d3d12_context_create(struct pipe_screen *pscreen, void *priv, unsigned flags)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);

   struct d3d12_context *ctx = CALLOC_STRUCT(d3d12_context);
   if (!ctx)
      return NULL;

   ctx->base.screen = pscreen;
   ctx->base.priv = priv;

   ctx->base.destroy = d3d12_context_destroy;

   ctx->base.create_vertex_elements_state = d3d12_create_vertex_elements_state;
   ctx->base.bind_vertex_elements_state = d3d12_bind_vertex_elements_state;
   ctx->base.delete_vertex_elements_state = d3d12_delete_vertex_elements_state;

   ctx->base.create_blend_state = d3d12_create_blend_state;
   ctx->base.bind_blend_state = d3d12_bind_blend_state;
   ctx->base.delete_blend_state = d3d12_delete_blend_state;

   ctx->base.create_depth_stencil_alpha_state = d3d12_create_depth_stencil_alpha_state;
   ctx->base.bind_depth_stencil_alpha_state = d3d12_bind_depth_stencil_alpha_state;
   ctx->base.delete_depth_stencil_alpha_state = d3d12_delete_depth_stencil_alpha_state;

   ctx->base.create_rasterizer_state = d3d12_create_rasterizer_state;
   ctx->base.bind_rasterizer_state = d3d12_bind_rasterizer_state;
   ctx->base.delete_rasterizer_state = d3d12_delete_rasterizer_state;

   ctx->base.create_sampler_state = d3d12_create_sampler_state;
   ctx->base.bind_sampler_states = d3d12_bind_sampler_states;
   ctx->base.delete_sampler_state = d3d12_delete_sampler_state;

   ctx->base.create_sampler_view = d3d12_create_sampler_view;
   ctx->base.set_sampler_views = d3d12_set_sampler_views;
   ctx->base.sampler_view_destroy = d3d12_destroy_sampler_view;

   ctx->base.create_vs_state = d3d12_create_vs_state;
   ctx->base.bind_vs_state = d3d12_bind_vs_state;
   ctx->base.delete_vs_state = d3d12_delete_vs_state;

   ctx->base.create_fs_state = d3d12_create_fs_state;
   ctx->base.bind_fs_state = d3d12_bind_fs_state;
   ctx->base.delete_fs_state = d3d12_delete_fs_state;

   ctx->base.set_polygon_stipple = d3d12_set_polygon_stipple;
   ctx->base.set_vertex_buffers = d3d12_set_vertex_buffers;
   ctx->base.set_viewport_states = d3d12_set_viewport_states;
   ctx->base.set_constant_buffer = d3d12_set_constant_buffer;
   ctx->base.set_framebuffer_state = d3d12_set_framebuffer_state;

   ctx->base.clear = d3d12_clear;
   ctx->base.draw_vbo = d3d12_draw_vbo;
   ctx->base.flush = d3d12_flush;

   d3d12_context_surface_init(&ctx->base);
   d3d12_context_resource_init(&ctx->base);

   slab_create_child(&ctx->transfer_pool, &d3d12_screen(pscreen)->transfer_pool);

   HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
   if (!hD3D12Mod) {
      debug_printf("D3D12: failed to load D3D12.DLL\n");
      return NULL;
   }
   ctx->D3D12SerializeRootSignature = (PFN_D3D12_SERIALIZE_ROOT_SIGNATURE)GetProcAddress(hD3D12Mod, "D3D12SerializeRootSignature");

   ctx->event = CreateEvent(NULL, FALSE, FALSE, NULL);
   if (FAILED(screen->dev->CreateFence(0, D3D12_FENCE_FLAG_NONE,
                                       __uuidof(ctx->cmdqueue_fence),
                                       (void **)&ctx->cmdqueue_fence))) {
      FREE(ctx);
      return NULL;
   }

   D3D12_COMMAND_QUEUE_DESC queue_desc;
   queue_desc.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
   queue_desc.Priority = D3D12_COMMAND_QUEUE_PRIORITY_NORMAL;
   queue_desc.Flags = D3D12_COMMAND_QUEUE_FLAG_NONE;
   queue_desc.NodeMask = 0;
   if (FAILED(screen->dev->CreateCommandQueue(&queue_desc,
                                              __uuidof(ctx->cmdqueue),
                                              (void **)&ctx->cmdqueue))) {
      FREE(ctx);
      return NULL;
   }


   if (FAILED(screen->dev->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                  __uuidof(ctx->cmdalloc),
                                                  (void **)&ctx->cmdalloc))) {
      FREE(ctx);
      return NULL;
   }

   if (FAILED(screen->dev->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
                                             ctx->cmdalloc, NULL,
                                             __uuidof(ctx->cmdlist),
                                             (void **)&ctx->cmdlist))) {
      FREE(ctx);
      return NULL;
   }

   D3D12_DESCRIPTOR_HEAP_DESC heap_desc = {};

   heap_desc.NumDescriptors = 100000;
   heap_desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
   heap_desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_NONE;
   if (FAILED(screen->dev->CreateDescriptorHeap(&heap_desc,
                                                __uuidof(ctx->rtv_heap),
                                                (void **)&ctx->rtv_heap))) {
      FREE(ctx);
      return NULL;
   }

   heap_desc.NumDescriptors = 10;
   heap_desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
   heap_desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_NONE;
   if (FAILED(screen->dev->CreateDescriptorHeap(&heap_desc,
                                                __uuidof(ctx->dsv_heap),
                                                (void **)&ctx->dsv_heap))) {
      FREE(ctx);
      return NULL;
   }

   ctx->rtv_increment = screen->dev->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
   ctx->dsv_increment = screen->dev->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_DSV);

   ctx->rtv_index = ctx->dsv_index = 0;

   return &ctx->base;
}
