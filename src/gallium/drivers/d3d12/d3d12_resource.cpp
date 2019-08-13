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

#include "d3d12_resource.h"

#include "d3d12_context.h"
#include "d3d12_format.h"
#include "d3d12_screen.h"

#include "util/slab.h"
#include "util/format/u_format.h"
#include "util/u_inlines.h"
#include "util/u_memory.h"

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

   res->base = *templ;

   pipe_reference_init(&res->base.reference, 1);
   res->base.screen = pscreen;

   D3D12_RESOURCE_DESC desc;
   desc.Format = templ->target == PIPE_BUFFER ? DXGI_FORMAT_UNKNOWN :
                 d3d12_get_format(templ->format);
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

   return &res->base;
}

void
d3d12_screen_resource_init(struct pipe_screen *pscreen)
{
   pscreen->resource_create = d3d12_resource_create;
   pscreen->resource_destroy = d3d12_resource_destroy;
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

   void *ptr;
   if (FAILED(res->res->Map(0, NULL, &ptr)))
      return NULL;

   struct pipe_transfer *ptrans = (struct pipe_transfer *)slab_alloc(&ctx->transfer_pool);
   if (!ptrans)
      return NULL;

   memset(ptrans, 0, sizeof(*ptrans));
   pipe_resource_reference(&ptrans->resource, pres);

   ptrans->resource = pres;
   ptrans->level = level;
   ptrans->usage = (enum pipe_transfer_usage)usage;
   ptrans->box = *box;

   *transfer = ptrans;
   return ptr;
}

static void
d3d12_transfer_unmap(struct pipe_context *pctx,
                     struct pipe_transfer *ptrans)
{
   struct d3d12_screen *screen = d3d12_screen(pctx->screen);
   struct d3d12_resource *res = d3d12_resource(ptrans->resource);
   res->res->Unmap(0, NULL);
}

void
d3d12_context_resource_init(struct pipe_context *pctx)
{
   pctx->transfer_map = d3d12_transfer_map;
   pctx->transfer_unmap = d3d12_transfer_unmap;

   pctx->transfer_flush_region = u_default_transfer_flush_region;
   pctx->buffer_subdata = u_default_buffer_subdata;
   pctx->texture_subdata = u_default_texture_subdata;
}
