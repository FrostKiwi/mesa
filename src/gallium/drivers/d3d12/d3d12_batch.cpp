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

#include "d3d12_batch.h"
#include "d3d12_context.h"
#include "d3d12_fence.h"
#include "d3d12_query.h"
#include "d3d12_screen.h"

#include "util/u_inlines.h"

bool
d3d12_init_batch(struct d3d12_context *ctx, struct d3d12_batch *batch)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);

   if (FAILED(screen->dev->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                  __uuidof(batch->cmdalloc),
                                                  (void **)&batch->cmdalloc)))
      return false;


   batch->sampler_heap =
      d3d12_descriptor_heap_new(screen->dev,
                                D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER,
                                D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
                                128);

   batch->view_heap =
      d3d12_descriptor_heap_new(screen->dev,
                                D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
                                D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
                                1024);

   if (!batch->sampler_heap && !batch->view_heap)
      return false;

   return true;
}

static void
reset_batch(struct d3d12_context *ctx, struct d3d12_batch *batch)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);

   // batch hasn't been submitted before
   if (!batch->fence)
      return;

   d3d12_fence_finish(batch->fence, PIPE_TIMEOUT_INFINITE);
   d3d12_fence_reference(&batch->fence, NULL);

   d3d12_descriptor_heap_clear(batch->view_heap);
   d3d12_descriptor_heap_clear(batch->sampler_heap);

   if (FAILED(batch->cmdalloc->Reset())) {
      debug_printf("D3D12: resetting ID3D12CommandAllocator failed\n");
      return;
   }
}

void
d3d12_destroy_batch(struct d3d12_context *ctx, struct d3d12_batch *batch)
{
   reset_batch(ctx, batch);
   batch->cmdalloc->Release();
   d3d12_descriptor_heap_free(batch->sampler_heap);
   d3d12_descriptor_heap_free(batch->view_heap);
}

void
d3d12_start_batch(struct d3d12_context *ctx, struct d3d12_batch *batch)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   ID3D12DescriptorHeap* heaps[2] = { d3d12_descriptor_heap_get(batch->view_heap),
                                      d3d12_descriptor_heap_get(batch->sampler_heap) };

   reset_batch(ctx, batch);

   /* Create or reset global command list */
   if (ctx->cmdlist) {
      if (FAILED(ctx->cmdlist->Reset(batch->cmdalloc, NULL))) {
         debug_printf("D3D12: resetting ID3D12GraphicsCommandList failed\n");
         return;
      }
   } else {
      if (FAILED(screen->dev->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                batch->cmdalloc, NULL,
                                                __uuidof(ctx->cmdlist),
                                                (void **)&ctx->cmdlist))) {
         debug_printf("D3D12: creating ID3D12GraphicsCommandList failed\n");
         return; 
      }
   }

   ctx->cmdlist->SetDescriptorHeaps(2, heaps);

   if (!ctx->queries_disabled)
      d3d12_resume_queries(ctx);
}

void
d3d12_end_batch(struct d3d12_context *ctx, struct d3d12_batch *batch)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);

   if (!ctx->queries_disabled)
      d3d12_suspend_queries(ctx);

   if (FAILED(ctx->cmdlist->Close())) {
      debug_printf("D3D12: closing ID3D12GraphicsCommandList failed\n");
      return;
   }

   ID3D12CommandList* cmdlists[] = { ctx->cmdlist };
   screen->cmdqueue->ExecuteCommandLists(1, cmdlists);
   batch->fence = d3d12_create_fence(screen, ctx);
}
