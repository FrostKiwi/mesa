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

#include "d3d12_resource.h"
#include "d3d12_screen.h"
#include "d3d12_surface.h"

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
}

static void
d3d12_clear(struct pipe_context *pctx,
            unsigned buffers,
            const union pipe_color_union *color,
            double depth, unsigned stencil)
{
}

static void
d3d12_draw_vbo(struct pipe_context *pctx,
               const struct pipe_draw_info *dinfo)
{
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

   return &ctx->base;
}
