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

#ifndef D3D12_RESOURCE_H
#define D3D12_RESOURCE_H

struct pipe_screen;
#include "util/u_transfer.h"

#include <d3d12.h>

struct d3d12_resource {
   struct pipe_resource base;
   ID3D12Resource *res;

   DXGI_FORMAT format;
   unsigned mip_levels;
   struct sw_displaytarget *dt;
   unsigned dt_stride;
};

struct d3d12_transfer {
   struct pipe_transfer base;
   struct pipe_resource *staging_res;
   void *data;
};

static inline struct d3d12_resource *
d3d12_resource(struct pipe_resource *r)
{
   return (struct d3d12_resource *)r;
}

void
d3d12_screen_resource_init(struct pipe_screen *pscreen);

void
d3d12_context_resource_init(struct pipe_context *pctx);

#endif
