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
#include "d3d12_format.h"
#include "d3d12_resource.h"
#include "d3d12_screen.h"
#include "d3d12_surface.h"

#include "util/format/u_format.h"
#include "util/u_inlines.h"
#include "util/u_memory.h"

static struct pipe_surface *
d3d12_create_surface(struct pipe_context *pctx,
                     struct pipe_resource *pres,
                     const struct pipe_surface *tpl)
{
   struct d3d12_resource *res = d3d12_resource(pres);
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_screen *screen = d3d12_screen(pctx->screen);

   bool is_depth_or_stencil = util_format_is_depth_or_stencil(tpl->format);
   unsigned bind = is_depth_or_stencil ? PIPE_BIND_DEPTH_STENCIL : PIPE_BIND_RENDER_TARGET;

   /* Don't bother if we don't support the requested format as RT or DS */
   if (!pctx->screen->is_format_supported(pctx->screen, tpl->format, PIPE_TEXTURE_2D,
                                          tpl->nr_samples, tpl->nr_samples,bind))
      return NULL;

   struct d3d12_surface *surface = CALLOC_STRUCT(d3d12_surface);
   if (!surface)
      return NULL;

   pipe_resource_reference(&surface->base.texture, pres);
   pipe_reference_init(&surface->base.reference, 1);
   surface->base.context = pctx;
   surface->base.format = tpl->format;
   surface->base.width = u_minify(pres->width0, tpl->u.tex.level);
   surface->base.height = u_minify(pres->height0, tpl->u.tex.level);
   surface->base.u.tex.level = tpl->u.tex.level;
   surface->base.u.tex.first_layer = tpl->u.tex.first_layer;
   surface->base.u.tex.last_layer = tpl->u.tex.last_layer;

   DXGI_FORMAT dxgi_format = d3d12_get_resource_rt_format(tpl->format);

   if (is_depth_or_stencil) {
      d3d12_descriptor_heap_alloc_handle(ctx->dsv_heap, &surface->desc_handle);
      D3D12_DEPTH_STENCIL_VIEW_DESC desc;
      desc.Format = dxgi_format;
      desc.Flags = D3D12_DSV_FLAG_NONE;
      switch (pres->target) {
      case PIPE_TEXTURE_1D:
         desc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE1D;
         desc.Texture1D.MipSlice = tpl->u.tex.level;
         break;

      case PIPE_TEXTURE_1D_ARRAY:
         desc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE1DARRAY;
         desc.Texture1DArray.MipSlice = tpl->u.tex.level;
         desc.Texture1DArray.FirstArraySlice = tpl->u.tex.first_layer;
         desc.Texture1DArray.ArraySize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
         break;

      case PIPE_TEXTURE_2D:
      case PIPE_TEXTURE_RECT:
         if (pres->nr_samples > 0)
            desc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2DMS;
         else {
            desc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
            desc.Texture2D.MipSlice = tpl->u.tex.level;
         }
         break;

      case PIPE_TEXTURE_2D_ARRAY:
      case PIPE_TEXTURE_CUBE:
         if (pres->nr_samples > 0) {
            desc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2DMSARRAY;
            desc.Texture2DMSArray.FirstArraySlice = tpl->u.tex.first_layer;
            desc.Texture2DMSArray.ArraySize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
         } else {
            desc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2DARRAY;
            desc.Texture2DArray.MipSlice = tpl->u.tex.level;
            desc.Texture2DArray.FirstArraySlice = tpl->u.tex.first_layer;
            desc.Texture2DArray.ArraySize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
         }
         break;

      default:
         unreachable("unsupported target"); // dunno how to support, if needed
         break;
      }

      screen->dev->CreateDepthStencilView(d3d12_resource_resource(res), &desc,
                                          surface->desc_handle.cpu_handle);
   } else {
      D3D12_RENDER_TARGET_VIEW_DESC desc;
      desc.Format = dxgi_format;

      switch (pres->target) {
      case PIPE_TEXTURE_1D:
         desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE1D;
         desc.Texture1D.MipSlice = tpl->u.tex.level;
         break;

      case PIPE_TEXTURE_1D_ARRAY:
         desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE1DARRAY;
         desc.Texture1DArray.MipSlice = tpl->u.tex.level;
         desc.Texture1DArray.FirstArraySlice = tpl->u.tex.first_layer;
         desc.Texture1DArray.ArraySize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
         break;

      case PIPE_TEXTURE_2D:
      case PIPE_TEXTURE_RECT:
         if (pres->nr_samples > 0)
            desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE2DMS;
         else {
            desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE2D;
            desc.Texture2D.MipSlice = tpl->u.tex.level;
            desc.Texture2D.PlaneSlice = 0;
         }
         break;

      case PIPE_TEXTURE_2D_ARRAY:
      case PIPE_TEXTURE_CUBE:
         if (pres->nr_samples > 0) {
            desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE2DMSARRAY;
            desc.Texture2DMSArray.FirstArraySlice = tpl->u.tex.first_layer;
            desc.Texture2DMSArray.ArraySize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
         } else {
            desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE2DARRAY;
            desc.Texture2DArray.MipSlice = tpl->u.tex.level;
            desc.Texture2DArray.FirstArraySlice = tpl->u.tex.first_layer;
            desc.Texture2DArray.ArraySize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
            desc.Texture2DArray.PlaneSlice = 0;
         }
         break;

      case PIPE_TEXTURE_3D:
         desc.ViewDimension = D3D12_RTV_DIMENSION_TEXTURE3D;
         desc.Texture3D.MipSlice = tpl->u.tex.level;
         desc.Texture3D.FirstWSlice = tpl->u.tex.first_layer;
         desc.Texture3D.WSize = tpl->u.tex.last_layer - tpl->u.tex.first_layer + 1;
         break;

      default:
         unreachable("unsupported target"); // dunno how to support, if needed
         break;
      }

      d3d12_descriptor_heap_alloc_handle(ctx->rtv_heap, &surface->desc_handle);
      screen->dev->CreateRenderTargetView(d3d12_resource_resource(res), &desc,
                                          surface->desc_handle.cpu_handle);
   }

   return &surface->base;
}

static void
d3d12_surface_destroy(struct pipe_context *pctx,
                      struct pipe_surface *psurf)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_surface *surface = (struct d3d12_surface*) psurf;

   d3d12_descriptor_handle_free(&surface->desc_handle);
   pipe_resource_reference(&psurf->texture, NULL);
   FREE(surface);
}

void
d3d12_context_surface_init(struct pipe_context *context)
{
   context->create_surface = d3d12_create_surface;
   context->surface_destroy = d3d12_surface_destroy;
}
