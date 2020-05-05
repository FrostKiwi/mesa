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

#include "d3d12_pipeline_state.h"
#include "d3d12_compiler.h"
#include "d3d12_context.h"
#include "d3d12_screen.h"

#include "util/hash_table.h"
#include "util/set.h"
#include "util/u_memory.h"
#include "util/u_prim.h"

struct d3d12_pso_entry {
   struct d3d12_gfx_pipeline_state key;
   ID3D12PipelineState *pso;
};

static bool
depth_bias(struct d3d12_rasterizer_state *state, enum pipe_prim_type reduced_prim)
{
   switch (reduced_prim) {
   case PIPE_PRIM_POINTS:
      return state->base.offset_point;

   case PIPE_PRIM_LINES:
      return state->base.offset_line;

   case PIPE_PRIM_TRIANGLES:
      return state->base.offset_tri;

   default:
      unreachable("unexpected reduced prim");
   }
}

static D3D12_PRIMITIVE_TOPOLOGY_TYPE
topology_type(enum pipe_prim_type reduced_prim)
{
   switch (reduced_prim) {
   case PIPE_PRIM_POINTS:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT;

   case PIPE_PRIM_LINES:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;

   case PIPE_PRIM_TRIANGLES:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;

   case PIPE_PRIM_PATCHES:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;

   default:
      debug_printf("pipe_prim_type: %s\n", u_prim_name(reduced_prim));
      unreachable("unexpected enum pipe_prim_type");
   }
}

static ID3D12PipelineState *
create_gfx_pipeline_state(struct d3d12_context *ctx)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   struct d3d12_gfx_pipeline_state *state = &ctx->gfx_pipeline_state;
   enum pipe_prim_type reduced_prim = u_reduced_prim(state->prim_type);

   D3D12_GRAPHICS_PIPELINE_STATE_DESC pso_desc = { 0 };
   pso_desc.pRootSignature = state->root_signature;

   if (state->stages[PIPE_SHADER_VERTEX]) {
      auto shader = state->stages[PIPE_SHADER_VERTEX];
      pso_desc.VS.BytecodeLength = shader->bytecode_length;
      pso_desc.VS.pShaderBytecode = shader->bytecode;
   }

   if (state->stages[PIPE_SHADER_FRAGMENT]) {
      auto shader = state->stages[PIPE_SHADER_FRAGMENT];
      pso_desc.PS.BytecodeLength = shader->bytecode_length;
      pso_desc.PS.pShaderBytecode = shader->bytecode;
   }

   if (state->stages[PIPE_SHADER_GEOMETRY]) {
      auto shader = state->stages[PIPE_SHADER_GEOMETRY];
      pso_desc.GS.BytecodeLength = shader->bytecode_length;
      pso_desc.GS.pShaderBytecode = shader->bytecode;
   }

   pso_desc.BlendState = state->blend->desc;
   pso_desc.DepthStencilState = state->zsa->desc;
   pso_desc.SampleMask = state->sample_mask;
   pso_desc.RasterizerState = state->rast->desc;

   if (reduced_prim != PIPE_PRIM_TRIANGLES)
      pso_desc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;

   if (depth_bias(state->rast, reduced_prim)) {
      pso_desc.RasterizerState.DepthBias = state->rast->base.offset_units;
      pso_desc.RasterizerState.DepthBiasClamp = state->rast->base.offset_clamp;
      pso_desc.RasterizerState.SlopeScaledDepthBias = state->rast->base.offset_scale;
   }

   pso_desc.InputLayout.pInputElementDescs = state->ves->elements;
   pso_desc.InputLayout.NumElements = state->ves->num_elements;

   pso_desc.IBStripCutValue = D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED; // TODO

   pso_desc.PrimitiveTopologyType = topology_type(reduced_prim);

   pso_desc.NumRenderTargets = ctx->fb.nr_cbufs;
   for (int i = 0; i < ctx->fb.nr_cbufs; ++i)
      pso_desc.RTVFormats[i] = state->rtv_formats[i];
   pso_desc.DSVFormat = state->dsv_format;

   pso_desc.SampleDesc.Count = state->samples;
   pso_desc.SampleDesc.Quality = 0;

   pso_desc.NodeMask = 0;

   pso_desc.CachedPSO.pCachedBlob = NULL;
   pso_desc.CachedPSO.CachedBlobSizeInBytes = 0;

   pso_desc.Flags = D3D12_PIPELINE_STATE_FLAG_NONE;

   ID3D12PipelineState *ret;
   if (FAILED(screen->dev->CreateGraphicsPipelineState(&pso_desc,
                                                       __uuidof(ret),
                                                       (void **)&ret))) {
      debug_printf("D3D12: CreateGraphicsPipelineState failed!\n");
      return NULL;
   }

   return ret;
}

static uint32_t
hash_gfx_pipeline_state(const void *key)
{
   return _mesa_hash_data(key, sizeof(struct d3d12_gfx_pipeline_state));
}

static bool
equals_gfx_pipeline_state(const void *a, const void *b)
{
   return memcmp(a, b, sizeof(struct d3d12_gfx_pipeline_state)) == 0;
}

ID3D12PipelineState *
d3d12_get_gfx_pipeline_state(struct d3d12_context *ctx)
{
   uint32_t hash = hash_gfx_pipeline_state(&ctx->gfx_pipeline_state);
   struct hash_entry *entry = _mesa_hash_table_search_pre_hashed(ctx->pso_cache, hash,
                                                                 &ctx->gfx_pipeline_state);
   if (!entry) {
      struct d3d12_pso_entry *data = (struct d3d12_pso_entry *)MALLOC(sizeof(struct d3d12_pso_entry));
      if (!data)
         return NULL;

      data->key = ctx->gfx_pipeline_state;
      data->pso = create_gfx_pipeline_state(ctx);
      if (!data->pso)
         return NULL;

      entry = _mesa_hash_table_insert_pre_hashed(ctx->pso_cache, hash, &data->key, data);
      assert(entry);
   }

   return ((struct d3d12_pso_entry *)(entry->data))->pso;
}

void
d3d12_gfx_pipeline_state_cache_init(struct d3d12_context *ctx)
{
   ctx->pso_cache = _mesa_hash_table_create(NULL, NULL, equals_gfx_pipeline_state);
}

static void
delete_entry(struct hash_entry *entry)
{
   struct d3d12_pso_entry *data = (struct d3d12_pso_entry *)entry->data;
   data->pso->Release();
   FREE(data);
}

void
d3d12_gfx_pipeline_state_cache_destroy(struct d3d12_context *ctx)
{
   _mesa_hash_table_destroy(ctx->pso_cache, delete_entry);
}

void
d3d12_gfx_pipeline_state_cache_invalidate(struct d3d12_context *ctx, const void *state)
{
   hash_table_foreach(ctx->pso_cache, entry) {
      const struct d3d12_gfx_pipeline_state *key = (struct d3d12_gfx_pipeline_state *)entry->key;
      if (key->blend == state || key->zsa == state || key->rast == state) {
         if (ctx->current_pso == (ID3D12PipelineState *)entry->data)
            ctx->current_pso = NULL;
         _mesa_hash_table_remove(ctx->pso_cache, entry);
         delete_entry(entry);
      }
   }
}

void
d3d12_gfx_pipeline_state_cache_invalidate_shader(struct d3d12_context *ctx,
                                                 enum pipe_shader_type stage,
                                                 struct d3d12_shader_selector *selector)
{
   struct d3d12_shader *shader = selector->first;

   while (shader) {
      hash_table_foreach(ctx->pso_cache, entry) {
         const struct d3d12_gfx_pipeline_state *key = (struct d3d12_gfx_pipeline_state *)entry->key;
         if (key->stages[stage] == shader) {
            if (ctx->current_pso == (ID3D12PipelineState *)entry->data)
               ctx->current_pso = NULL;
            _mesa_hash_table_remove(ctx->pso_cache, entry);
            delete_entry(entry);
         }
      }
      shader = shader->next_variant;
   }
}
