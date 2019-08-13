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

#include "d3d12_context.h"
#include "d3d12_format.h"
#include "d3d12_screen.h"

#include "util/u_debug.h"
#include "util/u_prim.h"


static ID3D12RootSignature *
get_root_signature(struct d3d12_context *ctx)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   D3D12_ROOT_SIGNATURE_DESC root_sig_desc;
   root_sig_desc.NumParameters = 0;
   root_sig_desc.pParameters = NULL;
   root_sig_desc.NumStaticSamplers = 0;
   root_sig_desc.pStaticSamplers = NULL;
   root_sig_desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_NONE;

   if (true)
      root_sig_desc.Flags |= D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

   ID3DBlob *sig, *error;
   if (FAILED(ctx->D3D12SerializeRootSignature(&root_sig_desc,
                                               D3D_ROOT_SIGNATURE_VERSION_1,
                                               &sig, &error))) {
      debug_printf("D3D12SerializeRootSignature failed\n");
      return NULL;
   }

   ID3D12RootSignature *ret;
   if (FAILED(screen->dev->CreateRootSignature(0,
                                               sig->GetBufferPointer(),
                                               sig->GetBufferSize(),
                                               __uuidof(ret),
                                               (void **)&ret))) {
      debug_printf("CreateRootSignature failed\n");
      return NULL;
   }
   return ret;
}

static D3D12_PRIMITIVE_TOPOLOGY_TYPE
topology_type(enum pipe_prim_type prim_type)
{
   switch (prim_type) {
   case PIPE_PRIM_POINTS:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT;

   case PIPE_PRIM_LINES:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;

   case PIPE_PRIM_TRIANGLES:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;

   case PIPE_PRIM_PATCHES:
      return D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;

   default:
      debug_printf("pipe_prim_type: %s\n", u_prim_name(prim_type));
      unreachable("unexpected enum pipe_prim_type");
   }
}

#include "vertex_shader.h"

static ID3D12PipelineState *
get_gfx_pipeline_state(struct d3d12_context *ctx,
                       ID3D12RootSignature *root_sig,
                       enum pipe_prim_type prim_type)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);

   D3D12_GRAPHICS_PIPELINE_STATE_DESC pso_desc = { 0 };
   pso_desc.pRootSignature = root_sig;

   pso_desc.VS.BytecodeLength = ARRAY_SIZE(vertex_shader);
   pso_desc.VS.pShaderBytecode = vertex_shader;

   // pso_desc.PS = TODO
   // pso_desc.BlendState = TODO

   pso_desc.SampleMask = UINT_MAX;

   pso_desc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID; // TODO
   pso_desc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE; // TODO
   pso_desc.RasterizerState.FrontCounterClockwise = TRUE; // TODO
   pso_desc.RasterizerState.DepthBias = 0; // TODO
   pso_desc.RasterizerState.DepthBiasClamp = 0; // TODO
   pso_desc.RasterizerState.SlopeScaledDepthBias = 0; // TODO
   pso_desc.RasterizerState.DepthClipEnable = TRUE; // TODO
   pso_desc.RasterizerState.MultisampleEnable = FALSE; // TODO
   pso_desc.RasterizerState.AntialiasedLineEnable = FALSE; // TODO
   pso_desc.RasterizerState.ForcedSampleCount = 0; // TODO
   pso_desc.RasterizerState.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF; // TODO

   pso_desc.DepthStencilState.DepthEnable = TRUE; // TODO
   pso_desc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL; // TODO
   pso_desc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS; // TODO
   pso_desc.DepthStencilState.StencilEnable = FALSE; // TODO
#if 0
   pso_desc.DepthStencilState.StencilReadMask;
   pso_desc.DepthStencilState.StencilWriteMask;
   pso_desc.DepthStencilState.FrontFace;
   pso_desc.DepthStencilState.BackFace;
#endif

   pso_desc.InputLayout.pInputElementDescs = ctx->ves->elements;
   pso_desc.InputLayout.NumElements = ctx->ves->num_elements;

   pso_desc.IBStripCutValue = D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED; // TODO

   pso_desc.PrimitiveTopologyType = topology_type(prim_type);

   pso_desc.NumRenderTargets = ctx->fb.nr_cbufs;
   for (int i = 0; i < ctx->fb.nr_cbufs; ++i)
      pso_desc.RTVFormats[i] = d3d12_get_format(ctx->fb.cbufs[i]->format);

   if (ctx->fb.zsbuf)
      pso_desc.DSVFormat = d3d12_get_format(ctx->fb.zsbuf->format);
   else
      pso_desc.DSVFormat = DXGI_FORMAT_UNKNOWN;

   pso_desc.SampleDesc.Count = 1;
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

void
d3d12_draw_vbo(struct pipe_context *pctx,
               const struct pipe_draw_info *dinfo)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   ID3D12RootSignature *root_sig = get_root_signature(ctx);
   ID3D12PipelineState *pipeline_state = get_gfx_pipeline_state(ctx, root_sig,
                                                                u_reduced_prim(dinfo->mode));

   d3d12_flush_cmdlist(ctx);
}
