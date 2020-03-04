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

#include "d3d12_query.h"
#include "d3d12_context.h"
#include "d3d12_resource.h"
#include "d3d12_screen.h"

#include "util/u_dump.h"
#include "util/u_inlines.h"
#include "util/u_memory.h"

struct d3d12_query {
   enum pipe_query_type type;

   ID3D12QueryHeap *query_heap;
   unsigned curr_query, num_queries;

   D3D12_QUERY_TYPE d3d12qtype;

   pipe_resource *buffer;
   unsigned buffer_offset;
   int fence_value;

   struct list_head active_list;
};

static D3D12_QUERY_HEAP_TYPE
d3d12_query_heap_type(unsigned query_type)
{
   switch (query_type) {
   case PIPE_QUERY_OCCLUSION_COUNTER:
   case PIPE_QUERY_OCCLUSION_PREDICATE:
   case PIPE_QUERY_OCCLUSION_PREDICATE_CONSERVATIVE:
      return D3D12_QUERY_HEAP_TYPE_OCCLUSION;
   default:
      debug_printf("unknown query: %s\n",
                   util_str_query_type(query_type, true));
      unreachable("d3d12: unknown query type");
   }
}

static D3D12_QUERY_TYPE
d3d12_query_type(unsigned query_type)
{
   switch (query_type) {
   case PIPE_QUERY_OCCLUSION_COUNTER:
      return D3D12_QUERY_TYPE_OCCLUSION;
   case PIPE_QUERY_OCCLUSION_PREDICATE:
   case PIPE_QUERY_OCCLUSION_PREDICATE_CONSERVATIVE:
      return D3D12_QUERY_TYPE_BINARY_OCCLUSION;
   default:
      debug_printf("unknown query: %s\n",
                   util_str_query_type(query_type, true));
      unreachable("d3d12: unknown query type");
   }
}

static struct pipe_query *
d3d12_create_query(struct pipe_context *pctx,
                   unsigned query_type, unsigned index)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_screen *screen = d3d12_screen(pctx->screen);
   struct d3d12_query *query = CALLOC_STRUCT(d3d12_query);
   D3D12_QUERY_HEAP_DESC desc = {};
   D3D12_RESOURCE_DESC res_desc = {};

   if (!query)
      return NULL;

   query->type = (pipe_query_type)query_type;
   query->d3d12qtype = d3d12_query_type(query_type);
   query->num_queries = 16;
   query->curr_query = 0;

   desc.Count = query->num_queries;
   desc.Type = d3d12_query_heap_type(query_type);
   if (FAILED(screen->dev->CreateQueryHeap(&desc,
                                           __uuidof(query->query_heap),
                                           (void **)&query->query_heap))) {
      FREE(query);
      return NULL;
   }

   /* Query result goes into a readback buffer */
   size_t buffer_size = sizeof(uint64_t) * query->num_queries;
   u_suballocator_alloc(ctx->query_allocator, buffer_size, 256,
                        &query->buffer_offset, &query->buffer);

   return (struct pipe_query *)query;
}

static void
d3d12_destroy_query(struct pipe_context *pctx,
                    struct pipe_query *q)
{
   struct d3d12_query *query = (struct d3d12_query *)q;

   query->query_heap->Release();
}

static bool
accumulate_result(struct d3d12_context *ctx, struct d3d12_query *q,
                  union pipe_query_result *result, bool write)
{
   struct pipe_transfer *transfer = NULL;
   unsigned access = PIPE_TRANSFER_READ;
   uint64_t *results;

   if (write)
      access |= PIPE_TRANSFER_WRITE;
   results = (uint64_t *)pipe_buffer_map_range(&ctx->base, q->buffer, q->buffer_offset,
                                               q->num_queries * sizeof(uint64_t),
                                               access, &transfer);

   if (results == NULL)
      return false;

   util_query_clear_result(result, q->type);
   for (int i = 0; i < q->curr_query; ++i) {
      switch (q->type) {
      case PIPE_QUERY_OCCLUSION_PREDICATE:
      case PIPE_QUERY_OCCLUSION_PREDICATE_CONSERVATIVE:
         result->b |= results[i] != 0;
         break;

      case PIPE_QUERY_OCCLUSION_COUNTER:
         result->u64 += results[i];
         break;

      default:
         debug_printf("unsupported query type: %s\n",
                      util_str_query_type(q->type, true));
         unreachable("unexpected query type");
      }
   }

   if (write)
      results[0] = result->u64;

   pipe_buffer_unmap(&ctx->base, transfer);

   return true;
}

static void
begin_query(struct d3d12_context *ctx, struct d3d12_query *q, bool restart)
{
   if (restart) {
      q->curr_query = 0;
   } else if (q->curr_query == q->num_queries) {
      union pipe_query_result result;

      /* Accumulate current results and store in first slot */
      d3d12_flush_cmdlist(ctx);
      accumulate_result(ctx, q, &result, true);
      q->curr_query = 1;
   }

   ctx->cmdlist->BeginQuery(q->query_heap, q->d3d12qtype, q->curr_query);
}

static bool
d3d12_begin_query(struct pipe_context *pctx,
                  struct pipe_query *q)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_query *query = (struct d3d12_query *)q;

   begin_query(ctx, query, true);
   list_addtail(&query->active_list, &ctx->active_queries);

   return true;
}

static void
end_query(struct d3d12_context *ctx, struct d3d12_query *q)
{
   struct d3d12_resource *res = (struct d3d12_resource *)q->buffer;
   unsigned offset = q->buffer_offset + q->curr_query * sizeof(uint64_t);

   ctx->cmdlist->EndQuery(q->query_heap, q->d3d12qtype, q->curr_query);
   ctx->cmdlist->ResolveQueryData(q->query_heap, q->d3d12qtype, q->curr_query,
                                  1, res->res, offset);

   assert(q->curr_query < q->num_queries);
   q->curr_query++;
}

static bool
d3d12_end_query(struct pipe_context *pctx,
               struct pipe_query *q)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_query *query = (struct d3d12_query *)q;

   end_query(ctx, query);
   query->fence_value = ctx->fence_value + 1;
   list_delinit(&query->active_list);

   return true;
}

static bool
d3d12_get_query_result(struct pipe_context *pctx,
                      struct pipe_query *q,
                      bool wait,
                      union pipe_query_result *result)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_query *query = (struct d3d12_query *)q;

   if (ctx->fence_value < query->fence_value) {
      if (!wait)
         return false;
      d3d12_flush_cmdlist(ctx);
   }

   return accumulate_result(ctx, query, result, false);
}

void
d3d12_suspend_queries(struct d3d12_context *ctx)
{
   list_for_each_entry(struct d3d12_query, query, &ctx->active_queries, active_list) {
      end_query(ctx, query);
   }
}

void
d3d12_resume_queries(struct d3d12_context *ctx)
{
   list_for_each_entry(struct d3d12_query, query, &ctx->active_queries, active_list) {
      begin_query(ctx, query, false);
   }
}

static void
d3d12_set_active_query_state(struct pipe_context *pctx, bool enable)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   ctx->queries_disabled = !enable;

   if (enable)
      d3d12_resume_queries(ctx);
   else
      d3d12_suspend_queries(ctx);
}

void
d3d12_context_query_init(struct pipe_context *pctx)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   list_inithead(&ctx->active_queries);

   ctx->query_allocator =
       u_suballocator_create(&ctx->base, 4056, 0, 
                             (pipe_resource_usage) (PIPE_USAGE_STAGING | PIPE_USAGE_STREAM),
                             0, true);

   pctx->create_query = d3d12_create_query;
   pctx->destroy_query = d3d12_destroy_query;
   pctx->begin_query = d3d12_begin_query;
   pctx->end_query = d3d12_end_query;
   pctx->get_query_result = d3d12_get_query_result;
   pctx->set_active_query_state = d3d12_set_active_query_state;
}
