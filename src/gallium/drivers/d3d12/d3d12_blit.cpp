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

#include "d3d12_context.h"
#include "d3d12_debug.h"
#include "d3d12_format.h"
#include "d3d12_resource.h"
#include "d3d12_screen.h"

#include "util/u_blitter.h"
#include "util/format/u_format.h"

void
copy_subregion_no_barriers(struct d3d12_context *ctx,
                           struct d3d12_resource *dst,
                           unsigned dst_level,
                           unsigned dstx, unsigned dsty, unsigned dstz,
                           struct d3d12_resource *src,
                           unsigned src_level,
                           const struct pipe_box *psrc_box)
{
   D3D12_TEXTURE_COPY_LOCATION src_loc, dst_loc;
   D3D12_BOX src_box = {};
   unsigned src_z = psrc_box->z;

   int src_subres_stride = src->base.last_level + 1;
   int dst_subres_stride = dst->base.last_level + 1;

   int stencil_src_res_offset = 1;
   int stencil_dst_res_offset = 1;

   int src_nres = 1;
   int dst_nres = 1;

   if (dst->base.format == PIPE_FORMAT_Z24_UNORM_S8_UINT ||
       dst->base.format == PIPE_FORMAT_S8_UINT_Z24_UNORM) {
      stencil_dst_res_offset = dst_subres_stride * dst->base.array_size;
      src_nres = 2;
   }

   if (src->base.format == PIPE_FORMAT_Z24_UNORM_S8_UINT ||
       src->base.format == PIPE_FORMAT_S8_UINT_Z24_UNORM) {
      stencil_src_res_offset = src_subres_stride * src->base.array_size;
      dst_nres = 2;
   }

   int nsubres = min(src_nres, dst_nres);

   for (int subres = 0; subres < nsubres; ++subres) {
      if (dst->base.target == PIPE_TEXTURE_CUBE ||
          dst->base.target == PIPE_TEXTURE_1D_ARRAY ||
          dst->base.target == PIPE_TEXTURE_2D_ARRAY) {
         dst_level = dst_subres_stride * dstz + dst_level;
         dstz = 0;
      }
      dst_level += subres * stencil_dst_res_offset;

      if (src->base.target == PIPE_TEXTURE_CUBE ||
          src->base.target == PIPE_TEXTURE_1D_ARRAY ||
          src->base.target == PIPE_TEXTURE_2D_ARRAY) {
         src_level = src_z * src_subres_stride + src_level;
         src_z = 0;
      }
      src_level += subres * stencil_src_res_offset;

      src_box.left = psrc_box->x;
      src_box.right = psrc_box->x + psrc_box->width;
      src_box.top = psrc_box->y;
      src_box.bottom = psrc_box->y + psrc_box->height;
      src_box.front = src_z;
      src_box.back = src_z + psrc_box->depth;

      src_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
      src_loc.SubresourceIndex = src_level;
      src_loc.pResource = src->res;

      dst_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
      dst_loc.SubresourceIndex = dst_level;
      dst_loc.pResource = dst->res;

      ctx->cmdlist->CopyTextureRegion(&dst_loc, dstx, dsty, dstz,
                                      &src_loc, &src_box);
   }
}

void
d3d12_blit(struct pipe_context *pctx,
           const struct pipe_blit_info *info)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   if (D3D12_DEBUG_BLIT & d3d12_debug) {
      debug_printf("D3D12 BLIT: from %s@%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(info->src.format), info->src.level,
                   info->src.box.x, info->src.box.y, info->src.box.z,
                   info->src.box.width, info->src.box.height, info->src.box.depth);
      debug_printf("      to   %s@%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(info->dst.format), info->dst.level,
                   info->dst.box.x, info->dst.box.y, info->dst.box.z,
                   info->dst.box.width, info->dst.box.height, info->dst.box.depth);
   }

   if (!util_blitter_is_blit_supported(ctx->blitter, info)) {
      debug_printf("blit unsupported %s -> %s\n",
              util_format_short_name(info->src.resource->format),
              util_format_short_name(info->dst.resource->format));
      return;
   }

   util_blitter_save_blend(ctx->blitter, ctx->gfx_pipeline_state.blend);
   util_blitter_save_depth_stencil_alpha(ctx->blitter, ctx->gfx_pipeline_state.zsa);
   util_blitter_save_vertex_elements(ctx->blitter, ctx->gfx_pipeline_state.ves);
   util_blitter_save_stencil_ref(ctx->blitter, &ctx->stencil_ref);
   util_blitter_save_rasterizer(ctx->blitter, ctx->gfx_pipeline_state.rast);
   util_blitter_save_fragment_shader(ctx->blitter, ctx->gfx_stages[PIPE_SHADER_FRAGMENT]);
   util_blitter_save_vertex_shader(ctx->blitter, ctx->gfx_stages[PIPE_SHADER_VERTEX]);
   util_blitter_save_geometry_shader(ctx->blitter, ctx->gfx_stages[PIPE_SHADER_GEOMETRY]);
   util_blitter_save_framebuffer(ctx->blitter, &ctx->fb);
   util_blitter_save_viewport(ctx->blitter, ctx->viewport_states);
   util_blitter_save_scissor(ctx->blitter, ctx->scissor_states);
   util_blitter_save_fragment_sampler_states(ctx->blitter,
                                             ctx->num_samplers[PIPE_SHADER_FRAGMENT],
                                             (void **)ctx->samplers[PIPE_SHADER_FRAGMENT]);
   util_blitter_save_fragment_sampler_views(ctx->blitter,
                                            ctx->num_sampler_views[PIPE_SHADER_FRAGMENT],
                                            ctx->sampler_views[PIPE_SHADER_FRAGMENT]);
   util_blitter_save_fragment_constant_buffer_slot(ctx->blitter, ctx->cbufs[PIPE_SHADER_FRAGMENT]);
   util_blitter_save_vertex_buffer_slot(ctx->blitter, ctx->vbs);
   util_blitter_save_sample_mask(ctx->blitter, ctx->gfx_pipeline_state.sample_mask);

   util_blitter_blit(ctx->blitter, info);
}

static void
d3d12_resource_copy_region(struct pipe_context *pctx,
                           struct pipe_resource *pdst,
                           unsigned dst_level,
                           unsigned dstx, unsigned dsty, unsigned dstz,
                           struct pipe_resource *psrc,
                           unsigned src_level,
                           const struct pipe_box *psrc_box)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   struct d3d12_resource *dst = d3d12_resource(pdst);
   struct d3d12_resource *src = d3d12_resource(psrc);

   if (D3D12_DEBUG_BLIT & d3d12_debug) {
      debug_printf("D3D12 COPY: from %s@%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(psrc->format), src_level,
                   psrc_box->x, psrc_box->y, psrc_box->z,
                   psrc_box->width, psrc_box->height, psrc_box->depth);
      debug_printf("      to   %s@%d %dx%dx%d\n",
                   util_format_name(pdst->format), dst_level,
                   dstx, dsty, dstz);
   }

   d3d12_resource_barrier(ctx, dst,
                          D3D12_RESOURCE_STATE_COMMON,
                          D3D12_RESOURCE_STATE_COPY_DEST);
   d3d12_resource_barrier(ctx, src,
                          D3D12_RESOURCE_STATE_COMMON,
                          D3D12_RESOURCE_STATE_COPY_SOURCE);

   d3d12_batch_reference_resource(batch, src);
   d3d12_batch_reference_resource(batch, dst);

   copy_subregion_no_barriers(ctx, dst, dst_level, dstx, dsty, dstz,
                              src, src_level, psrc_box);

   d3d12_resource_barrier(ctx, src,
                          D3D12_RESOURCE_STATE_COPY_SOURCE,
                          D3D12_RESOURCE_STATE_COMMON);
   d3d12_resource_barrier(ctx, dst,
                          D3D12_RESOURCE_STATE_COPY_DEST,
                          D3D12_RESOURCE_STATE_COMMON);
}

void
d3d12_context_blit_init(struct pipe_context *ctx)
{
   ctx->resource_copy_region = d3d12_resource_copy_region;
   ctx->blit = d3d12_blit;
}
