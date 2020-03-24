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

#include "d3d12_resource.h"

#include "d3d12_context.h"
#include "d3d12_format.h"
#include "d3d12_screen.h"

#include "util/slab.h"
#include "util/format/u_format.h"
#include "util/u_inlines.h"
#include "util/u_memory.h"

#include "state_tracker/sw_winsys.h"

#include <d3d12.h>

static void
d3d12_resource_destroy(struct pipe_screen *pscreen,
                       struct pipe_resource *presource)
{
   struct d3d12_resource *resource = d3d12_resource(presource);
   FREE(resource);
}

static struct pipe_resource *
d3d12_resource_create(struct pipe_screen *pscreen,
                      const struct pipe_resource *templ)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);
   struct d3d12_resource *res = CALLOC_STRUCT(d3d12_resource);
   const uint32_t bind_ds_and_sv = PIPE_BIND_SAMPLER_VIEW | PIPE_BIND_DEPTH_STENCIL;
   const bool use_as_ds_and_sv = (templ->bind & bind_ds_and_sv) == bind_ds_and_sv;

   res->base = *templ;

   pipe_reference_init(&res->base.reference, 1);
   res->base.screen = pscreen;
   res->format = templ->target == PIPE_BUFFER ? DXGI_FORMAT_UNKNOWN :
                 d3d12_get_format(templ->format);

   D3D12_RESOURCE_DESC desc;
   desc.Format = use_as_ds_and_sv ?
                    d3d12_get_resource_base_format(res->format):
                    res->format;
   desc.Alignment = D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT;
   desc.Width = templ->width0;
   desc.Height = templ->height0;
   desc.DepthOrArraySize = templ->array_size;
   desc.MipLevels = templ->last_level + 1;

   desc.SampleDesc.Count = MAX2(templ->nr_samples, 1);
   desc.SampleDesc.Quality = 0; /* TODO: figure this one out */

   switch (templ->target) {
   case PIPE_BUFFER:
      desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
      break;

   case PIPE_TEXTURE_1D:
   case PIPE_TEXTURE_1D_ARRAY:
      desc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE1D;
      break;

   case PIPE_TEXTURE_CUBE:
   case PIPE_TEXTURE_CUBE_ARRAY:
      desc.DepthOrArraySize *= 6;
      /* fall-through */
   case PIPE_TEXTURE_2D:
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_RECT:
      desc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
      break;

   case PIPE_TEXTURE_3D:
      desc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE3D;
      desc.DepthOrArraySize = templ->depth0;
      break;
   }

   desc.Flags = D3D12_RESOURCE_FLAG_NONE;

   /*
    * TODO: unsure if *all* of these should block shader-resources
    */
   unsigned srv_bits = PIPE_BIND_VERTEX_BUFFER | PIPE_BIND_INDEX_BUFFER |
                       PIPE_BIND_CONSTANT_BUFFER | PIPE_BIND_SAMPLER_VIEW |
                       PIPE_BIND_SHADER_IMAGE |
                       PIPE_BIND_COMMAND_ARGS_BUFFER |
                       PIPE_BIND_STREAM_OUTPUT;
   if ((templ->bind & srv_bits) == 0)
      desc.Flags |= D3D12_RESOURCE_FLAG_DENY_SHADER_RESOURCE;

   if (templ->bind & PIPE_BIND_SHADER_BUFFER)
      desc.Flags |= D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS;

   if (templ->bind & PIPE_BIND_RENDER_TARGET)
      desc.Flags |= D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET;

   if (templ->bind & PIPE_BIND_DEPTH_STENCIL)
      desc.Flags |= D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;

   desc.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
   if ((templ->bind & (PIPE_BIND_SCANOUT |
                      PIPE_BIND_SHARED | PIPE_BIND_LINEAR)) ||
       templ->usage == PIPE_USAGE_STAGING ||
       templ->target == PIPE_BUFFER)
      desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

   D3D12_HEAP_TYPE heap_type = D3D12_HEAP_TYPE_DEFAULT;

   if (templ->bind & (PIPE_BIND_DISPLAY_TARGET |
                      PIPE_BIND_SCANOUT |
                      PIPE_BIND_SHARED))
      heap_type = D3D12_HEAP_TYPE_READBACK;
   else if (templ->target == PIPE_BUFFER ||
       templ->usage == PIPE_USAGE_STAGING)
      heap_type = D3D12_HEAP_TYPE_UPLOAD;

   D3D12_HEAP_PROPERTIES heap_pris = screen->dev->GetCustomHeapProperties(0, heap_type);

   HRESULT hres = screen->dev->CreateCommittedResource(&heap_pris,
                                                   D3D12_HEAP_FLAG_NONE,
                                                   &desc,
                                                   D3D12_RESOURCE_STATE_COMMON,
                                                   NULL,
                                                   __uuidof(ID3D12Resource),
                                                   (void **)&res->res);
   if (FAILED(hres)) {
      FREE(res);
      return NULL;
   }

   if (screen->winsys && (templ->bind & (PIPE_BIND_DISPLAY_TARGET |
                                         PIPE_BIND_SCANOUT |
                                         PIPE_BIND_SHARED))) {
      struct sw_winsys *winsys = screen->winsys;
      res->dt = winsys->displaytarget_create(screen->winsys,
                                             res->base.bind,
                                             res->base.format,
                                             templ->width0,
                                             templ->height0,
                                             64, NULL,
                                             &res->dt_stride);
   }

   return &res->base;
}

void
d3d12_screen_resource_init(struct pipe_screen *pscreen)
{
   pscreen->resource_create = d3d12_resource_create;
   pscreen->resource_destroy = d3d12_resource_destroy;
}

static bool
d3d12_transfer_copy_bufimage(struct d3d12_context *ctx,
                             struct d3d12_resource *res,
                             struct d3d12_resource *staging_res,
                             struct d3d12_transfer *trans,
                             unsigned stride,
                             bool buf2img)
{
   ID3D12Device* dev = d3d12_screen(ctx->base.screen)->dev;
   D3D12_TEXTURE_COPY_LOCATION buf_loc = {};
   D3D12_TEXTURE_COPY_LOCATION tex_loc = {};
   D3D12_TEXTURE_COPY_LOCATION *src, *dst;
   D3D12_BOX src_box = {};
   UINT dst_x, dst_y, dst_z;

   if (buf2img) {
      d3d12_resource_barrier(ctx, res,
                             D3D12_RESOURCE_STATE_COMMON,
                             D3D12_RESOURCE_STATE_COPY_DEST);
      src = &buf_loc;
      dst = &tex_loc;
      dst_x = trans->base.box.x;
      dst_y = trans->base.box.y;
      dst_z = res->base.target == PIPE_TEXTURE_CUBE ? 0 : trans->base.box.z;
   } else {
      d3d12_resource_barrier(ctx, res,
                             D3D12_RESOURCE_STATE_COMMON,
                             D3D12_RESOURCE_STATE_COPY_SOURCE);
      src = &tex_loc;
      dst = &buf_loc;
      dst_x = dst_y = dst_z = 0;
      src_box.left = trans->base.box.x;
      src_box.right = trans->base.box.x + trans->base.box.width;
      src_box.top = trans->base.box.y;
      src_box.bottom = trans->base.box.y + trans->base.box.height;
      src_box.front = trans->base.box.z;
      src_box.back = trans->base.box.z + trans->base.box.depth;
   }

   int subres = res->base.target == PIPE_TEXTURE_CUBE ?
                   trans->base.box.z * (res->base.last_level + 1) + trans->base.level :
                   trans->base.level;

   D3D12_PLACED_SUBRESOURCE_FOOTPRINT footprint;
   auto descr = res->res->GetDesc();
   dev->GetCopyableFootprints(&descr, 0, 1, 0, &footprint, nullptr, nullptr, nullptr);

   tex_loc.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
   tex_loc.SubresourceIndex = subres;
   tex_loc.pResource = res->res;

   buf_loc.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
   buf_loc.PlacedFootprint = footprint;
   buf_loc.PlacedFootprint.Footprint.Width = trans->base.box.width;
   buf_loc.PlacedFootprint.Footprint.Height = trans->base.box.height;
   buf_loc.PlacedFootprint.Footprint.Depth = trans->base.box.depth;
   buf_loc.PlacedFootprint.Footprint.RowPitch = stride;

   buf_loc.pResource = staging_res->res;

   ctx->cmdlist->CopyTextureRegion(dst, dst_x, dst_y, dst_z, src, buf2img ? NULL : &src_box);

   d3d12_resource_barrier(ctx, res,
                          buf2img ? D3D12_RESOURCE_STATE_COPY_DEST : D3D12_RESOURCE_STATE_COPY_SOURCE,
                          D3D12_RESOURCE_STATE_COMMON);

   return true;
}

static unsigned
linear_offset(int x, int y, int z, unsigned stride, unsigned layer_stride)
{
   return x +
          y * stride +
          z * layer_stride;
}

static void *
d3d12_transfer_map(struct pipe_context *pctx,
                   struct pipe_resource *pres,
                   unsigned level,
                   unsigned usage,
                   const struct pipe_box *box,
                   struct pipe_transfer **transfer)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_screen *screen = d3d12_screen(pctx->screen);
   struct d3d12_resource *res = d3d12_resource(pres);

   if (usage & PIPE_TRANSFER_MAP_DIRECTLY)
      return NULL;

   struct d3d12_transfer *trans = (struct d3d12_transfer *)slab_alloc(&ctx->transfer_pool);
   struct pipe_transfer *ptrans = &trans->base;
   if (!trans)
      return NULL;

   memset(trans, 0, sizeof(*trans));
   pipe_resource_reference(&ptrans->resource, pres);

   ptrans->resource = pres;
   ptrans->level = level;
   ptrans->usage = (enum pipe_transfer_usage)usage;
   ptrans->box = *box;

   D3D12_RANGE range;
   void *ptr;
   if (pres->target == PIPE_BUFFER) {
      range.Begin = box->x;
      range.End = box->x + box->width;

      if (FAILED(res->res->Map(0, &range, &ptr)))
         return NULL;

      ptrans->stride = 0;
      ptrans->layer_stride = 0;
   } else if (res->res->GetDesc().Layout == D3D12_TEXTURE_LAYOUT_ROW_MAJOR) {

      ptrans->stride = util_format_get_stride(pres->format, box->width);
      ptrans->layer_stride = util_format_get_2d_size(pres->format,
                                                     ptrans->stride,
                                                     box->height);

      range.Begin = linear_offset(box->x, box->y, box->z,
                                  ptrans->stride, ptrans->layer_stride);
      range.End = linear_offset(box->x + box->width,
                                box->y + box->height,
                                box->z + box->depth,
                                ptrans->stride, ptrans->layer_stride);

      if (FAILED(res->res->Map(0, &range, &ptr)))
         return NULL;
   } else {
      ptrans->stride = align(util_format_get_stride(pres->format, box->width),
                              D3D12_TEXTURE_DATA_PITCH_ALIGNMENT);
      ptrans->layer_stride = util_format_get_2d_size(pres->format,
                                                     ptrans->stride,
                                                     box->height);

      trans->staging_res = pipe_buffer_create(pctx->screen, 0,
                                              PIPE_USAGE_STAGING,
                                              ptrans->layer_stride * box->depth);
      if (!trans->staging_res)
         return NULL;

      struct d3d12_resource *staging_res = d3d12_resource(trans->staging_res);

      if (usage & PIPE_TRANSFER_READ) {
         bool ret = d3d12_transfer_copy_bufimage(ctx, res, staging_res, trans, ptrans->stride, false);
         if (ret == false)
            return NULL;
         d3d12_flush_cmdlist_and_wait(ctx);
      }

      range.Begin = 0;
      range.End = ptrans->layer_stride * box->depth;

      if (FAILED(staging_res->res->Map(0, &range, &ptr)))
         return NULL;
   }

   *transfer = ptrans;
   return ((uint8_t *)ptr) + range.Begin;
}

static void
d3d12_transfer_unmap(struct pipe_context *pctx,
                     struct pipe_transfer *ptrans)
{
   struct d3d12_screen *screen = d3d12_screen(pctx->screen);
   struct d3d12_resource *res = d3d12_resource(ptrans->resource);
   struct d3d12_transfer *trans = (struct d3d12_transfer *)ptrans;
   D3D12_RANGE range = { 0, 0 };

   if (trans->staging_res) {
      struct d3d12_resource *staging_res = d3d12_resource(trans->staging_res);

      if (trans->base.usage & PIPE_TRANSFER_WRITE) {
         range.Begin = 0;
         range.End = ptrans->layer_stride * ptrans->box.depth;
      }
      staging_res->res->Unmap(0, &range);

      if (trans->base.usage & PIPE_TRANSFER_WRITE) {
         struct d3d12_context *ctx = d3d12_context(pctx);

         d3d12_transfer_copy_bufimage(ctx, res, staging_res, trans, ptrans->stride, true);
      }

      pipe_resource_reference(&trans->staging_res, NULL);
   } else {
      if (trans->base.usage & PIPE_TRANSFER_WRITE) {
         range.Begin = ptrans->box.x;
         range.End = ptrans->box.x + ptrans->box.width;
      }
      res->res->Unmap(0, &range);
   }

   pipe_resource_reference(&ptrans->resource, NULL);
   slab_free(&d3d12_context(pctx)->transfer_pool, ptrans);
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
   D3D12_TEXTURE_COPY_LOCATION src_loc, dst_loc;
   D3D12_BOX src_box = {};
   unsigned src_z = psrc_box->z;

   d3d12_resource_barrier(ctx, dst,
                          D3D12_RESOURCE_STATE_COMMON,
                          D3D12_RESOURCE_STATE_COPY_DEST);
   d3d12_resource_barrier(ctx, src,
                          D3D12_RESOURCE_STATE_COMMON,
                          D3D12_RESOURCE_STATE_COPY_SOURCE);

   if (pdst->target == PIPE_TEXTURE_CUBE) {
      dst_level = dstz * (pdst->last_level + 1) + dst_level;
      dstz = 0;
   }

   if (psrc->target == PIPE_TEXTURE_CUBE) {
      src_level = src_z * (psrc->last_level + 1) + src_level;
      src_z = 0;
   }


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

   d3d12_resource_barrier(ctx, src,
                          D3D12_RESOURCE_STATE_COPY_SOURCE,
                          D3D12_RESOURCE_STATE_COMMON);
   d3d12_resource_barrier(ctx, dst,
                          D3D12_RESOURCE_STATE_COPY_DEST,
                          D3D12_RESOURCE_STATE_COMMON);
}

void
d3d12_context_resource_init(struct pipe_context *pctx)
{
   pctx->transfer_map = d3d12_transfer_map;
   pctx->transfer_unmap = d3d12_transfer_unmap;

   pctx->transfer_flush_region = u_default_transfer_flush_region;
   pctx->buffer_subdata = u_default_buffer_subdata;
   pctx->texture_subdata = u_default_texture_subdata;

   pctx->resource_copy_region = d3d12_resource_copy_region;
}
