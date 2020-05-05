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


static bool
formats_are_copy_comaptible(enum pipe_format src, enum pipe_format dst)
{
   if (src == dst)
      return true;

   /* We can skip the stencil copy */
   if (util_format_get_depth_only(src) == dst ||
       util_format_get_depth_only(dst) == src)
      return true;

   return false;
}

static bool
box_fits(const struct pipe_box *box, const struct pipe_resource *res, int level)
{
   unsigned lwidth = u_minify(res->width0, level);
   unsigned lheight= u_minify(res->height0, level);
   unsigned ldepth = res->target == PIPE_TEXTURE_3D ? u_minify(res->depth0, level) :
                                                      res->array_size;

   unsigned wb = box->x;
   unsigned we = box->x + box->width;

   unsigned hb = box->y;
   unsigned he = box->y + box->height;

   unsigned db = box->z;
   unsigned de = box->z + box->depth;

   return (wb <= lwidth && we <= lwidth &&
           hb <= lheight && he <= lheight &&
           db <= ldepth && de <= ldepth);
}

static bool
direct_copy_supported(const struct pipe_blit_info *info)
{
   if (info->scissor_enable || info->alpha_blend ||
       info->render_condition_enable) {
      return false;
   }

   if (!formats_are_copy_comaptible(info->src.format, info->dst.format))
      return false;

   if (util_format_is_depth_or_stencil(info->src.format) && !(info->mask & PIPE_MASK_ZS)) {
      return false;
   }

   if (!util_format_is_depth_or_stencil(info->src.format)) {
      if (util_format_get_mask(info->dst.format) != info->mask ||
          util_format_get_mask(info->src.format) != info->mask)
         return false;
   }

   if (!box_fits(&info->dst.box, info->dst.resource, info->dst.level)) {
      return false;
   }
   if (!box_fits(&info->src.box, info->src.resource, info->src.level)) {
      return false;
   }

   if (info->src.box.width != info->dst.box.width) {
      return false;
   }

   if (info->src.box.height != info->dst.box.height) {
      return false;
   }

   if (info->src.box.depth != info->dst.box.depth) {
      return false;
   }

   return true;
}

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

static void
copy_resource_y_flipped(struct d3d12_context *ctx,
                        const struct pipe_blit_info *info)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   struct d3d12_resource *src = d3d12_resource(info->src.resource);
   struct d3d12_resource *dst = d3d12_resource(info->dst.resource);

   if (D3D12_DEBUG_BLIT & d3d12_debug) {
      debug_printf("D3D12 BLIT as COPY: from %s@%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(src->base.format), info->src.level,
                   info->src.box.x, info->src.box.y, info->src.box.z,
                   info->src.box.width, info->src.box.height, info->src.box.depth);
      debug_printf("      to   %s@%d %dx%dx%d\n",
                   util_format_name(dst->base.format), info->dst.level,
                   info->dst.box.x, info->dst.box.y, info->dst.box.z);
   }

   d3d12_resource_barrier(ctx, dst,
                          D3D12_RESOURCE_STATE_COMMON,
                          D3D12_RESOURCE_STATE_COPY_DEST);
   d3d12_resource_barrier(ctx, src,
                          D3D12_RESOURCE_STATE_COMMON,
                          D3D12_RESOURCE_STATE_COPY_SOURCE);

   d3d12_batch_reference_resource(batch, src);
   d3d12_batch_reference_resource(batch, dst);

   struct pipe_box src_box = info->src.box;
   int src_inc = info->src.box.height > 0 ? 1 : -1;
   int dst_inc = info->dst.box.height > 0 ? 1 : -1;
   src_box.height = 1;
   int rows_to_copy = abs(info->src.box.height);

   if (info->src.box.height < 0)
      --src_box.y;

   for (int y = 0, dest_y = info->dst.box.y; y < rows_to_copy;
        ++y, src_box.y += src_inc, dest_y += dst_inc) {

      copy_subregion_no_barriers(ctx, dst, info->dst.level,
                                       info->dst.box.x, dest_y, info->dst.box.z,
                                       src, info->src.level, &src_box);
   }
   d3d12_resource_barrier(ctx, src,
                          D3D12_RESOURCE_STATE_COPY_SOURCE,
                          D3D12_RESOURCE_STATE_COMMON);
   d3d12_resource_barrier(ctx, dst,
                          D3D12_RESOURCE_STATE_COPY_DEST,
                          D3D12_RESOURCE_STATE_COMMON);
}

static void
direct_copy(struct d3d12_context *ctx,
            const struct pipe_blit_info *info)
{
   /* No flipping, we can forward this directly to resource_copy_region */
   if (info->src.box.height == info->dst.box.height) {
      ctx->base.resource_copy_region(&ctx->base, info->dst.resource, info->dst.level,
                                     info->dst.box.x, info->dst.box.y, info->dst.box.z,
                                     info->src.resource, info->src.level,
                                     &info->src.box);
   } else {
      assert(info->src.box.height == -info->dst.box.height);
      copy_resource_y_flipped(ctx, info);
   }
}

static void
util_blit(struct d3d12_context *ctx,
          const struct pipe_blit_info *info)
{
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

   if (direct_copy_supported(info))
      direct_copy(ctx, info);
   else if (util_blitter_is_blit_supported(ctx->blitter, info))
      util_blit(ctx, info);
   else
      debug_printf("blit unsupported %s -> %s\n",
                 util_format_short_name(info->src.resource->format),
                 util_format_short_name(info->dst.resource->format));
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
