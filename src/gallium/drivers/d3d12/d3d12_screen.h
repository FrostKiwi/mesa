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

#ifndef D3D12_SCREEN_H
#define D3D12_SCREEN_H

#include "pipe/p_screen.h"

#include "util/slab.h"

#include <d3d12.h>
#include <dxgi1_4.h>

struct d3d12_screen {
   struct pipe_screen base;
   struct sw_winsys *winsys;

   IDXGIAdapter1 *adapter;
   ID3D12Device *dev;
   ID3D12CommandQueue *cmdqueue;

   struct slab_parent_pool transfer_pool;

   /* capabilities */
   D3D_FEATURE_LEVEL max_feature_level;
   D3D12_FEATURE_DATA_ARCHITECTURE architecture;
   D3D12_FEATURE_DATA_D3D12_OPTIONS opts;
   D3D12_FEATURE_DATA_D3D12_OPTIONS3 opts3;

   /* description */
   DXGI_ADAPTER_DESC1 adapter_desc;
};

static inline struct d3d12_screen *
d3d12_screen(struct pipe_screen *pipe)
{
   return (struct d3d12_screen *)pipe;
}

#endif
