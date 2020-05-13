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
resolve_supported(const struct pipe_blit_info *info)
{
   // need actually be a resolve operation
   if (info->src.resource->nr_samples <= 1 ||
       info->dst.resource->nr_samples > 1)
      return false;

   // check for unsupported operations
   if (util_format_get_mask(info->dst.format) != info->mask ||
       util_format_get_mask(info->src.format) != info->mask ||
       info->filter != PIPE_TEX_FILTER_NEAREST ||
       info->scissor_enable ||
       info->num_window_rectangles > 0 ||
       info->alpha_blend)
      return false;

   // formats need to match
   struct d3d12_resource *src = d3d12_resource(info->src.resource);
   struct d3d12_resource *dst = d3d12_resource(info->dst.resource);
   if (src->format != dst->format)
      return false;

   // sizes needs to match
   if (info->src.box.width != info->dst.box.width ||
       info->src.box.height != info->dst.box.height)
      return false;

   // can only resolve full subresource
   if (info->src.box.width != u_minify(info->src.resource->width0,
                                       info->src.level) ||
       info->src.box.height != u_minify(info->src.resource->height0,
                                        info->src.level) ||
       info->dst.box.width != u_minify(info->dst.resource->width0,
                                           info->dst.level) ||
       info->dst.box.height != u_minify(info->dst.resource->height0,
                                            info->dst.level))
      return false;

   return true;
}

static void
blit_resolve(struct d3d12_context *ctx, const struct pipe_blit_info *info)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   struct d3d12_resource *src = d3d12_resource(info->src.resource);
   struct d3d12_resource *dst = d3d12_resource(info->dst.resource);

   d3d12_transition_resource_state(ctx, src,
                                   D3D12_RESOURCE_STATE_RESOLVE_SOURCE,
                                   SubresourceTransitionFlags_None);
   d3d12_transition_resource_state(ctx, dst,
                                   D3D12_RESOURCE_STATE_RESOLVE_DEST,
                                   SubresourceTransitionFlags_None);

   d3d12_apply_resource_states(ctx, false);

   d3d12_batch_reference_resource(batch, src);
   d3d12_batch_reference_resource(batch, dst);

   assert(src->format == dst->format);
   ctx->cmdlist->ResolveSubresource(
      d3d12_resource_resource(dst), info->dst.level,
      d3d12_resource_resource(src), info->src.level,
      src->format);
}

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
direct_copy_supported(struct d3d12_screen *screen,
                      const struct pipe_blit_info *info)
{
   if (info->scissor_enable || info->alpha_blend ||
       info->render_condition_enable ||
       info->src.resource->nr_samples != info->dst.resource->nr_samples) {
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

   if ((screen->opts2.ProgrammableSamplePositionsTier ==
        D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_NOT_SUPPORTED &&
        (info->src.resource->bind & PIPE_BIND_DEPTH_STENCIL ||
         info->dst.resource->bind & PIPE_BIND_DEPTH_STENCIL)) ||
        info->src.resource->nr_samples > 1) {

      if (info->dst.box.x != 0 ||
          info->dst.box.y != 0 ||
          info->dst.box.z != 0)
         return false;

      if (info->src.box.x != 0 ||
          info->src.box.y != 0 ||
          info->src.box.z != 0 ||
          info->src.box.width != u_minify(info->src.resource->width0,
                                          info->src.level) ||
          info->src.box.height != u_minify(info->src.resource->height0,
                                           info->src.level) ||
          info->src.box.depth != u_minify(info->src.resource->depth0,
                                          info->src.level))
         return false;
   }


   return true;
}

static void
copy_subregion_no_barriers(struct d3d12_context *ctx,
                           struct d3d12_resource *dst,
                           unsigned dst_level,
                           unsigned dstx, unsigned dsty, unsigned dstz,
                           struct d3d12_resource *src,
                           unsigned src_level,
                           const struct pipe_box *psrc_box)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   D3D12_TEXTURE_COPY_LOCATION src_loc, dst_loc;
   unsigned src_z = psrc_box->z;

   int src_subres_stride = src->base.last_level + 1;
   int dst_subres_stride = dst->base.last_level + 1;

   int src_array_size = src->base.array_size;
   int dst_array_size = dst->base.array_size;

   if (dst->base.target == PIPE_TEXTURE_CUBE)
      dst_array_size *= 6;

   if (src->base.target == PIPE_TEXTURE_CUBE)
      src_array_size *= 6;

   int stencil_src_res_offset = 1;
   int stencil_dst_res_offset = 1;

   int src_nres = 1;
   int dst_nres = 1;

   if (dst->base.format == PIPE_FORMAT_Z24_UNORM_S8_UINT ||
       dst->base.format == PIPE_FORMAT_S8_UINT_Z24_UNORM) {
      stencil_dst_res_offset = dst_subres_stride * dst_array_size;
      src_nres = 2;
   }

   if (src->base.format == PIPE_FORMAT_Z24_UNORM_S8_UINT ||
       src->base.format == PIPE_FORMAT_S8_UINT_Z24_UNORM) {
      stencil_src_res_offset = src_subres_stride * src_array_size;
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

      src_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
      src_loc.SubresourceIndex = src_level;
      src_loc.pResource = d3d12_resource_resource(src);

      dst_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
      dst_loc.SubresourceIndex = dst_level;
      dst_loc.pResource = d3d12_resource_resource(dst);

      if (psrc_box->x == 0 && psrc_box->y == 0 && psrc_box->z == 0 &&
          psrc_box->width == u_minify(src->base.width0, src_level) &&
          psrc_box->height == u_minify(src->base.height0, src_level) &&
          psrc_box->depth == u_minify(src->base.depth0, src_level)) {

         assert((dstx == 0 && dsty == 0 && dstz == 0) ||
                screen->opts2.ProgrammableSamplePositionsTier !=
                D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_NOT_SUPPORTED ||
                (!util_format_is_depth_or_stencil(dst->base.format) &&
                 !util_format_is_depth_or_stencil(src->base.format) &&
                  dst->base.nr_samples <= 1 &&
                  src->base.nr_samples <= 1));

         ctx->cmdlist->CopyTextureRegion(&dst_loc, dstx, dsty, dstz,
                                         &src_loc, NULL);

      } else {
         D3D12_BOX src_box;
         src_box.left = psrc_box->x;
         src_box.right = psrc_box->x + psrc_box->width;
         src_box.top = psrc_box->y;
         src_box.bottom = psrc_box->y + psrc_box->height;
         src_box.front = src_z;
         src_box.back = src_z + psrc_box->depth;

         assert((screen->opts2.ProgrammableSamplePositionsTier !=
                 D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_NOT_SUPPORTED ||
                 (!util_format_is_depth_or_stencil(dst->base.format) &&
                  !util_format_is_depth_or_stencil(src->base.format))) &&
                dst->base.nr_samples <= 1 &&
                src->base.nr_samples <= 1);

         ctx->cmdlist->CopyTextureRegion(&dst_loc, dstx, dsty, dstz,
                                         &src_loc, &src_box);
      }
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

   /* HACK: We're not supposed to be able to copy from/to the same subresource,
            but it works if we only set one state (COPY_DEST) */
   if (src != dst)
      d3d12_transition_resource_state(ctx, src,
                                      D3D12_RESOURCE_STATE_COPY_SOURCE,
                                      SubresourceTransitionFlags_None);
   d3d12_transition_resource_state(ctx, dst,
                                   D3D12_RESOURCE_STATE_COPY_DEST,
                                   SubresourceTransitionFlags_None);
   d3d12_apply_resource_states(ctx, false);

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

   if (resolve_supported(info))
      blit_resolve(ctx, info);
   else if (direct_copy_supported(d3d12_screen(pctx->screen), info))
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

   /* HACK: We're not supposed to be able to copy from/to the same subresource,
            but it works if we only set one state (COPY_DEST) */
   if (src != dst)
      d3d12_transition_resource_state(ctx, src,
                                      D3D12_RESOURCE_STATE_COPY_SOURCE,
                                      SubresourceTransitionFlags_None);
   d3d12_transition_resource_state(ctx, dst,
                                   D3D12_RESOURCE_STATE_COPY_DEST,
                                   SubresourceTransitionFlags_None);

   d3d12_apply_resource_states(ctx, false);

   d3d12_batch_reference_resource(batch, src);
   d3d12_batch_reference_resource(batch, dst);

   copy_subregion_no_barriers(ctx, dst, dst_level, dstx, dsty, dstz,
                              src, src_level, psrc_box);
}

void
d3d12_context_blit_init(struct pipe_context *ctx)
{
   ctx->resource_copy_region = d3d12_resource_copy_region;
   ctx->blit = d3d12_blit;
}
