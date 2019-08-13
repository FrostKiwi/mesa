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

#ifndef D3D12_CONTEXT_H
#define D3D12_CONTEXT_H

#include "pipe/p_context.h"
#include "pipe/p_state.h"
#include "util/slab.h"

#include <d3d12.h>

#define D3D12_DEBUG_VERBOSE 1
extern int d3d12_debug;

struct d3d12_vertex_elements_state {
   D3D12_INPUT_ELEMENT_DESC elements[PIPE_MAX_ATTRIBS];
   unsigned num_elements;
};

struct d3d12_context {
   struct pipe_context base;
   struct slab_child_pool transfer_pool;

   struct pipe_framebuffer_state fb;
   struct d3d12_vertex_elements_state *ves;

   HANDLE event;
   ID3D12CommandQueue *cmdqueue;
   ID3D12Fence *cmdqueue_fence;
   int fence_value;
   ID3D12CommandAllocator *cmdalloc;
   ID3D12GraphicsCommandList *cmdlist;

   ID3D12DescriptorHeap *rtv_heap;
   ID3D12DescriptorHeap *dsv_heap;
   UINT rtv_increment, dsv_increment;
   int rtv_index, dsv_index;

   PFN_D3D12_SERIALIZE_ROOT_SIGNATURE D3D12SerializeRootSignature;
};

static inline struct d3d12_context *
d3d12_context(struct pipe_context *context)
{
   return (struct d3d12_context *)context;
}

struct pipe_context *
d3d12_context_create(struct pipe_screen *pscreen, void *priv, unsigned flags);

void
d3d12_flush_cmdlist(struct d3d12_context *ctx);

void
d3d12_draw_vbo(struct pipe_context *pctx,
               const struct pipe_draw_info *dinfo);

#endif
