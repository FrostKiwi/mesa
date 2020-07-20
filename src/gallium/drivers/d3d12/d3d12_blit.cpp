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

static void
copy_buffer_region_no_barriers(struct d3d12_context *ctx,
                               struct d3d12_resource *dst,
                               uint64_t dst_offset,
                               struct d3d12_resource *src,
                               uint64_t src_offset,
                               uint64_t size)
{
   uint64_t dst_off, src_off;
   ID3D12Resource *dst_buf = d3d12_resource_underlying(dst, &dst_off);
   ID3D12Resource *src_buf = d3d12_resource_underlying(src, &src_off);

   ctx->cmdlist->CopyBufferRegion(dst_buf, dst_offset + dst_off,
                                  src_buf, src_offset + src_off,
                                  size);
}

static bool
resolve_supported(const struct pipe_blit_info *info)
{
   // need actually be a resolve operation
   if (info->src.resource->nr_samples <= 1 ||
       info->dst.resource->nr_samples > 1)
      return false;

   // check for unsupported operations
   if (util_format_is_depth_or_stencil(info->src.format) && !(info->mask & PIPE_MASK_ZS)) {
      return false;
   } else {
      if (util_format_get_mask(info->dst.format) != info->mask ||
          util_format_get_mask(info->src.format) != info->mask)
         return false;
   }

   if (info->filter != PIPE_TEX_FILTER_NEAREST ||
       info->scissor_enable ||
       info->num_window_rectangles > 0 ||
       info->alpha_blend)
      return false;

   // formats need to match
   struct d3d12_resource *src = d3d12_resource(info->src.resource);
   struct d3d12_resource *dst = d3d12_resource(info->dst.resource);
   if (src->dxgi_format != dst->dxgi_format)
      return false;

   if (util_format_is_pure_integer(src->base.format))
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

   DXGI_FORMAT dxgi_format = d3d12_get_resource_srv_format(src->base.format);

   assert(src->dxgi_format == dst->dxgi_format);
   ctx->cmdlist->ResolveSubresource(
      d3d12_resource_resource(dst), info->dst.level,
      d3d12_resource_resource(src), info->src.level,
      dxgi_format);
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
                      const struct pipe_blit_info *info,
                      bool have_predication)
{
   if (info->scissor_enable || info->alpha_blend ||
       (have_predication && info->render_condition_enable) ||
       MAX2(info->src.resource->nr_samples, 1) != MAX2(info->dst.resource->nr_samples, 1)) {
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

   if (abs(info->src.box.height) != info->dst.box.height) {
      return false;
   }

   if (info->src.box.height != info->dst.box.height &&
       (!util_format_is_depth_or_stencil(info->src.format) ||
        screen->opts2.ProgrammableSamplePositionsTier ==
        D3D12_PROGRAMMABLE_SAMPLE_POSITIONS_TIER_NOT_SUPPORTED)) {
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

inline static unsigned
get_subresource_id(enum pipe_texture_target target, unsigned subres, unsigned stride,
                   unsigned z, unsigned *updated_z)
{
   if (target == PIPE_TEXTURE_CUBE ||
       target == PIPE_TEXTURE_1D_ARRAY ||
       target == PIPE_TEXTURE_2D_ARRAY) {
      subres += stride * z + subres;
      if (updated_z)
         *updated_z = 0;
   }
   return subres;
}

static void
copy_subregion_no_barriers(struct d3d12_context *ctx,
                           struct d3d12_resource *dst,
                           unsigned dst_level,
                           unsigned dstx, unsigned dsty, unsigned dstz,
                           struct d3d12_resource *src,
                           unsigned src_level,
                           const struct pipe_box *psrc_box,
                           unsigned mask)
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
       dst->base.format == PIPE_FORMAT_S8_UINT_Z24_UNORM ||
       dst->base.format == PIPE_FORMAT_Z32_FLOAT_S8X24_UINT) {
      stencil_dst_res_offset = dst_subres_stride * dst_array_size;
      src_nres = 2;
   }

   if (src->base.format == PIPE_FORMAT_Z24_UNORM_S8_UINT ||
       src->base.format == PIPE_FORMAT_S8_UINT_Z24_UNORM ||
       dst->base.format == PIPE_FORMAT_Z32_FLOAT_S8X24_UINT) {
      stencil_src_res_offset = src_subres_stride * src_array_size;
      dst_nres = 2;
   }

   static_assert(PIPE_MASK_S == 0x20 && PIPE_MASK_Z == 0x10, "unexpected ZS format mask");
   int nsubres = min(src_nres, dst_nres);
   unsigned subresource_copy_mask = nsubres > 1 ? mask >> 4 : 1;

   for (int subres = 0; subres < nsubres; ++subres) {

      if (!(subresource_copy_mask & (1 << subres)))
         continue;

      src_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
      src_loc.SubresourceIndex = get_subresource_id(src->base.target, src_level, src_subres_stride, src_z, &src_z) +
                                 subres * stencil_src_res_offset;
      src_loc.pResource = d3d12_resource_resource(src);

      dst_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
      dst_loc.SubresourceIndex = get_subresource_id(dst->base.target, dst_level, dst_subres_stride, dstz, &dstz) +
                                 subres * stencil_dst_res_offset;
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
copy_resource_y_flipped_no_barriers(struct d3d12_context *ctx,
                                    struct d3d12_resource *dst,
                                    unsigned dst_level,
                                    const struct pipe_box *pdst_box,
                                    struct d3d12_resource *src,
                                    unsigned src_level,
                                    const struct pipe_box *psrc_box,
                                    unsigned mask)
{
   if (D3D12_DEBUG_BLIT & d3d12_debug) {
      debug_printf("D3D12 BLIT as COPY: from %s@%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(src->base.format), src_level,
                   psrc_box->x, psrc_box->y, psrc_box->z,
                   psrc_box->width, psrc_box->height, psrc_box->depth);
      debug_printf("      to   %s@%d %dx%dx%d\n",
                   util_format_name(dst->base.format), dst_level,
                   pdst_box->x, pdst_box->y, pdst_box->z);
   }

   struct pipe_box src_box = *psrc_box;
   int src_inc = psrc_box->height > 0 ? 1 : -1;
   int dst_inc = pdst_box->height > 0 ? 1 : -1;
   src_box.height = 1;
   int rows_to_copy = abs(psrc_box->height);

   if (psrc_box->height < 0)
      --src_box.y;

   for (int y = 0, dest_y = pdst_box->y; y < rows_to_copy;
        ++y, src_box.y += src_inc, dest_y += dst_inc) {
      copy_subregion_no_barriers(ctx, dst, dst_level,
                                 pdst_box->x, dest_y, pdst_box->z,
                                 src, src_level, &src_box, mask);
   }
}

void
d3d12_direct_copy(struct d3d12_context *ctx,
                  struct d3d12_resource *dst,
                  unsigned dst_level,
                  const struct pipe_box *pdst_box,
                  struct d3d12_resource *src,
                  unsigned src_level,
                  const struct pipe_box *psrc_box,
                  unsigned mask)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);

   if (D3D12_DEBUG_BLIT & d3d12_debug)
      debug_printf("BLIT: Direct copy\n");

   d3d12_transition_subresources_state(ctx, src, src_level, 1, 0, 1, 0,
                                       d3d12_get_format_num_planes(src->base.format),
                                       D3D12_RESOURCE_STATE_COPY_SOURCE,
                                       SubresourceTransitionFlags_None);

   d3d12_transition_subresources_state(ctx, dst, dst_level, 1, 0, 1, 0,
                                       d3d12_get_format_num_planes(dst->base.format),
                                       D3D12_RESOURCE_STATE_COPY_DEST,
                                       SubresourceTransitionFlags_None);

   d3d12_apply_resource_states(ctx, false);

   d3d12_batch_reference_resource(batch, src);
   d3d12_batch_reference_resource(batch, dst);

   if (src->base.target == PIPE_BUFFER) {
      copy_buffer_region_no_barriers(ctx, dst, pdst_box->x,
                                     src, psrc_box->x, psrc_box->width);
   } else if (psrc_box->height == pdst_box->height) {
      /* No flipping, we can forward this directly to resource_copy_region */
      copy_subregion_no_barriers(ctx, dst, dst_level,
                                 pdst_box->x, pdst_box->y, pdst_box->z,
                                 src, src_level, psrc_box, mask);
   } else {
      assert(psrc_box->height == -pdst_box->height);
      copy_resource_y_flipped_no_barriers(ctx, dst, dst_level, pdst_box,
                                          src, src_level, psrc_box, mask);
   }
}

static bool
is_same_resource(const struct pipe_blit_info *info)
{
   return d3d12_resource_resource(d3d12_resource(info->src.resource)) ==
             d3d12_resource_resource(d3d12_resource(info->dst.resource)) &&
          info->src.level == info->dst.level;
}

static struct pipe_resource *
create_staging_resource(struct d3d12_context *ctx,
                        struct d3d12_resource *src,
                        unsigned src_level,
                        const struct pipe_box *src_box,
                        struct pipe_box *dst_box,
                        unsigned mask)

{
   struct pipe_resource templ = {{0}};
   struct pipe_resource *staging_res;

   templ.format = src->base.format;
   templ.width0 = src_box->width;
   templ.height0 = src_box->height;
   templ.depth0 = src_box->depth;
   templ.array_size = 1;
   templ.nr_samples = 1;
   templ.nr_storage_samples = 1;
   templ.usage = PIPE_USAGE_DEFAULT | PIPE_USAGE_STAGING;
   templ.bind = util_format_is_depth_or_stencil(templ.format) ? PIPE_BIND_DEPTH_STENCIL : PIPE_BIND_RENDER_TARGET;
   templ.target = src->base.target;

   staging_res = ctx->base.screen->resource_create(ctx->base.screen, &templ);

   dst_box->x = 0;
   dst_box->y = 0;
   dst_box->z = 0;
   dst_box->width = src_box->width;
   dst_box->height = src_box->height;
   dst_box->depth = src_box->depth;

   d3d12_direct_copy(ctx, d3d12_resource(staging_res), 0, dst_box,
                     src, src_level, src_box, mask);

   return staging_res;
}

static void
blit_same_resource(struct d3d12_context *ctx,
                   const struct pipe_blit_info *info)
{
   struct pipe_blit_info dst_info = *info;

   dst_info.src.level = 0;
   dst_info.src.resource = create_staging_resource(ctx, d3d12_resource(info->src.resource),
                                                   info->src.level,
                                                   &info->src.box,
                                                   &dst_info.src.box, PIPE_MASK_RGBAZS);
   ctx->base.blit(&ctx->base, &dst_info);
   pipe_resource_reference(&dst_info.src.resource, NULL);
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
   util_blitter_save_so_targets(ctx->blitter, ctx->num_so_targets, ctx->so_targets);

   util_blitter_blit(ctx->blitter, info);
}

void
d3d12_blit(struct pipe_context *pctx,
           const struct pipe_blit_info *info)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   if (!info->render_condition_enable && ctx->current_predication) {
      if (D3D12_DEBUG_BLIT & d3d12_debug)
         debug_printf("D3D12 BLIT: Disable predication\n");
      ctx->cmdlist->SetPredication(nullptr, 0, D3D12_PREDICATION_OP_EQUAL_ZERO);
   }

   if (D3D12_DEBUG_BLIT & d3d12_debug) {
      debug_printf("D3D12 BLIT: from %s@%d msaa:%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(info->src.format), info->src.level,
                   info->src.resource->nr_samples,
                   info->src.box.x, info->src.box.y, info->src.box.z,
                   info->src.box.width, info->src.box.height, info->src.box.depth);
      debug_printf("            to   %s@%d msaa:%d %dx%dx%d + %dx%dx%d ",
                   util_format_name(info->dst.format), info->dst.level,
                   info->dst.resource->nr_samples,
                   info->dst.box.x, info->dst.box.y, info->dst.box.z,
                   info->dst.box.width, info->dst.box.height, info->dst.box.depth);
      debug_printf("| flags %s%s%s\n",
                   info->render_condition_enable ? "cond " : "",
                   info->scissor_enable ? "scissor " : "",
                   info->alpha_blend ? "blend" : "");
   }

   if (is_same_resource(info))
      blit_same_resource(ctx, info);
   else if (resolve_supported(info))
      blit_resolve(ctx, info);
   else if (direct_copy_supported(d3d12_screen(pctx->screen), info, ctx->current_predication != nullptr))
      d3d12_direct_copy(ctx, d3d12_resource(info->dst.resource),
                        info->dst.level, &info->dst.box,
                        d3d12_resource(info->src.resource),
                        info->src.level, &info->src.box, info->mask);
   else if (util_blitter_is_blit_supported(ctx->blitter, info))
      util_blit(ctx, info);
   else
      debug_printf("blit unsupported %s -> %s\n",
                 util_format_short_name(info->src.resource->format),
                 util_format_short_name(info->dst.resource->format));

   if (!info->render_condition_enable && ctx->current_predication) {
      ctx->cmdlist->SetPredication(
               d3d12_resource_resource(ctx->current_predication), 0, D3D12_PREDICATION_OP_EQUAL_ZERO);
      if (D3D12_DEBUG_BLIT & d3d12_debug)
         debug_printf("D3D12 BLIT: Re-enable predication\n");
   }

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
   struct d3d12_resource *dst = d3d12_resource(pdst);
   struct d3d12_resource *src = d3d12_resource(psrc);
   struct pipe_resource *staging_res = NULL;
   const struct pipe_box *src_box = psrc_box;
   struct pipe_box staging_box, dst_box;

   if (D3D12_DEBUG_BLIT & d3d12_debug) {
      debug_printf("D3D12 COPY: from %s@%d %dx%dx%d + %dx%dx%d\n",
                   util_format_name(psrc->format), src_level,
                   psrc_box->x, psrc_box->y, psrc_box->z,
                   psrc_box->width, psrc_box->height, psrc_box->depth);
      debug_printf("      to   %s@%d %dx%dx%d\n",
                   util_format_name(pdst->format), dst_level,
                   dstx, dsty, dstz);
   }

   /* Use an intermediate resource if copying from/to the same subresource */
   if (d3d12_resource_resource(dst) == d3d12_resource_resource(src) && dst_level == src_level) {
      staging_res = create_staging_resource(ctx, src, src_level, psrc_box, &staging_box, PIPE_MASK_RGBAZS);
      src = d3d12_resource(staging_res);
      src_level = 0;
      src_box = &staging_box;
   }

   dst_box.x = dstx;
   dst_box.y = dsty;
   dst_box.z = dstz;
   dst_box.width = psrc_box->width;
   dst_box.height = psrc_box->height;

   d3d12_direct_copy(ctx, dst, dst_level, &dst_box,
                     src, src_level, src_box, PIPE_MASK_RGBAZS);

   if (staging_res)
      pipe_resource_reference(&staging_res, NULL);
}

void
d3d12_context_blit_init(struct pipe_context *ctx)
{
   ctx->resource_copy_region = d3d12_resource_copy_region;
   ctx->blit = d3d12_blit;
}
