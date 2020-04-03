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

#include "d3d12_compiler.h"
#include "d3d12_context.h"
#include "d3d12_format.h"
#include "d3d12_resource.h"
#include "d3d12_screen.h"
#include "d3d12_surface.h"

#include "util/u_debug.h"
#include "util/u_helpers.h"
#include "util/u_inlines.h"
#include "util/u_prim.h"
#include "util/u_math.h"

#include <wrl.h>

using Microsoft::WRL::ComPtr;

extern "C" {
#include "indices/u_primconvert.h"
}

static D3D12_SHADER_VISIBILITY
get_shader_visibility(enum pipe_shader_type stage)
{
   switch (stage) {
   case PIPE_SHADER_VERTEX:
      return D3D12_SHADER_VISIBILITY_VERTEX;
   case PIPE_SHADER_FRAGMENT:
      return D3D12_SHADER_VISIBILITY_PIXEL;
   case PIPE_SHADER_GEOMETRY:
      return D3D12_SHADER_VISIBILITY_GEOMETRY;
   case PIPE_SHADER_TESS_CTRL:
      return D3D12_SHADER_VISIBILITY_HULL;
   case PIPE_SHADER_TESS_EVAL:
      return D3D12_SHADER_VISIBILITY_DOMAIN;
   default:
      unreachable("unknown shader stage");
   }
}

static inline void
init_constant_root_param(D3D12_ROOT_PARAMETER1 *param,
                         unsigned reg,
                         unsigned size,
                         D3D12_SHADER_VISIBILITY visibility)
{
   param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
   param->ShaderVisibility = visibility;
   param->Constants.RegisterSpace = 0;
   param->Constants.ShaderRegister = reg;
   param->Constants.Num32BitValues = size;
}

static inline void
init_range_root_param(D3D12_ROOT_PARAMETER1 *param,
                      D3D12_DESCRIPTOR_RANGE1 *range,
                      D3D12_DESCRIPTOR_RANGE_TYPE type,
                      uint32_t num_descs,
                      D3D12_SHADER_VISIBILITY visibility)
{
   range->RangeType = type;
   range->NumDescriptors = num_descs;
   range->BaseShaderRegister = 0; /* We only have one range/table for each desc type */
   range->RegisterSpace = 0;
   if (type == D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER)
      range->Flags = D3D12_DESCRIPTOR_RANGE_FLAG_NONE;
   else
      range->Flags = D3D12_DESCRIPTOR_RANGE_FLAG_DATA_VOLATILE;
   range->OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

   param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
   param->DescriptorTable.NumDescriptorRanges = 1;
   param->DescriptorTable.pDescriptorRanges = range;
   param->ShaderVisibility = visibility;
}

static ID3D12RootSignature *
get_root_signature(struct d3d12_context *ctx)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   D3D12_ROOT_PARAMETER1 root_params[PIPE_SHADER_TYPES * D3D12_NUM_BINDING_TYPES];
   D3D12_DESCRIPTOR_RANGE1 desc_ranges[PIPE_SHADER_TYPES * D3D12_NUM_BINDING_TYPES];
   unsigned num_params = 0;

   for (unsigned i = 0; i < D3D12_GFX_SHADER_STAGES; ++i) {

      if (!ctx->gfx_stages[i])
         continue;

      struct d3d12_shader *shader = ctx->gfx_stages[i]->current;
      assert(shader);

      D3D12_SHADER_VISIBILITY visibility = get_shader_visibility((enum pipe_shader_type)i);

      if (shader->num_cb_bindings > 0) {
         assert(num_params < PIPE_SHADER_TYPES * D3D12_NUM_BINDING_TYPES);
         init_range_root_param(&root_params[num_params],
                               &desc_ranges[num_params],
                               D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
                               shader->num_cb_bindings,
                               visibility);
         num_params++;
      }

      if (shader->num_srv_bindings > 0) {
         init_range_root_param(&root_params[num_params],
                               &desc_ranges[num_params],
                               D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
                               shader->num_srv_bindings,
                               visibility);
         num_params++;
      }

      if (shader->num_srv_bindings > 0) {
         init_range_root_param(&root_params[num_params],
                               &desc_ranges[num_params],
                               D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER,
                               shader->num_srv_bindings,
                               visibility);
         num_params++;
      }

      if (shader->num_state_vars > 0) {
         init_constant_root_param(&root_params[num_params],
                                  shader->state_vars_binding,
                                  shader->state_vars_size,
                                  visibility);
         num_params++;
      }
   }

   D3D12_VERSIONED_ROOT_SIGNATURE_DESC root_sig_desc;
   root_sig_desc.Version = D3D_ROOT_SIGNATURE_VERSION_1_1;
   root_sig_desc.Desc_1_1.NumParameters = num_params;
   root_sig_desc.Desc_1_1.pParameters = (num_params > 0) ? root_params : NULL;
   root_sig_desc.Desc_1_1.NumStaticSamplers = 0;
   root_sig_desc.Desc_1_1.pStaticSamplers = NULL;
   root_sig_desc.Desc_1_1.Flags = D3D12_ROOT_SIGNATURE_FLAG_NONE;

   /* TODO Only enable this flag when needed (optimization) */
   root_sig_desc.Desc_1_1.Flags |= D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

   ComPtr<ID3DBlob> sig, error;
   if (FAILED(ctx->D3D12SerializeVersionedRootSignature(&root_sig_desc,
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

static D3D12_GPU_DESCRIPTOR_HANDLE
fill_cbv_descriptors(struct d3d12_context *ctx,
                     struct d3d12_shader *shader,
                     int stage)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   struct d3d12_descriptor_handle table_start;
   d2d12_descriptor_heap_get_next_handle(batch->view_heap, &table_start);

   for (unsigned i = 0; i < shader->num_cb_bindings; i++) {
      unsigned binding = shader->cb_bindings[i].binding;
      struct pipe_constant_buffer *buffer = &ctx->cbufs[stage][binding];

      assert(buffer->buffer_size > 0);
      assert(buffer->buffer);

      struct d3d12_resource *res = d3d12_resource(buffer->buffer);
      D3D12_CONSTANT_BUFFER_VIEW_DESC cbv_desc = {};
      cbv_desc.BufferLocation = res->res->GetGPUVirtualAddress() + buffer->buffer_offset;
      cbv_desc.SizeInBytes = align(buffer->buffer_size, 256);
      d3d12_batch_reference_resource(batch, res);

      struct d3d12_descriptor_handle handle;
      d3d12_descriptor_heap_alloc_handle(batch->view_heap, &handle);
      d3d12_screen(ctx->base.screen)->dev->CreateConstantBufferView(&cbv_desc, handle.cpu_handle);
   }

   return table_start.gpu_handle;
}

static D3D12_GPU_DESCRIPTOR_HANDLE
fill_srv_descriptors(struct d3d12_context *ctx,
                     struct d3d12_shader *shader,
                     unsigned stage)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   D3D12_CPU_DESCRIPTOR_HANDLE descs[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   struct d3d12_descriptor_handle table_start;

   d2d12_descriptor_heap_get_next_handle(batch->view_heap, &table_start);

   for (int i = 0; i < shader->num_srv_bindings; i++)
   {
      int index = shader->srv_bindings[i].index;
      struct d3d12_sampler_view *view =
         (struct d3d12_sampler_view*) ctx->sampler_views[stage][index];

      if (view != NULL) {
         descs[i] = view->handle.cpu_handle ;
         d3d12_batch_reference_sampler_view(batch, view);
      } else {
         descs[i] = ctx->null_srvs[shader->srv_bindings[i].dimension].cpu_handle;
      }
   }

   d3d12_descriptor_heap_append_handles(batch->view_heap, descs, shader->num_srv_bindings);

   return table_start.gpu_handle;
}

static D3D12_GPU_DESCRIPTOR_HANDLE
fill_sampler_descriptors(struct d3d12_context *ctx,
                         struct d3d12_shader *shader,
                         unsigned stage)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   D3D12_CPU_DESCRIPTOR_HANDLE descs[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   struct d3d12_descriptor_handle table_start;

   d2d12_descriptor_heap_get_next_handle(batch->sampler_heap, &table_start);

   for (int i = 0; i < shader->num_srv_bindings; i++)
   {
      int index = shader->srv_bindings[i].index;
      struct d3d12_sampler_state *sampler = ctx->samplers[stage][index];

      if (sampler != NULL)
         descs[i] = sampler->handle.cpu_handle;
      else
         descs[i] = ctx->null_sampler.cpu_handle;
   }

   d3d12_descriptor_heap_append_handles(batch->sampler_heap, descs, shader->num_srv_bindings);
   return table_start.gpu_handle;
}

static unsigned
fill_state_vars(struct d3d12_context *ctx,
                struct d3d12_shader *shader,
                uint32_t *values)
{
   unsigned size = 0;

   for (unsigned j = 0; j < shader->num_state_vars; ++j) {
      uint32_t *ptr = values + size;

      switch (shader->state_vars[j].var) {
      case D3D12_STATE_VAR_Y_FLIP:
         ptr[0] = fui(ctx->flip_y);
         size += 4;
         break;
      case D3D12_STATE_VAR_PT_SPRITE:
         ptr[0] = fui(1.0 / ctx->viewports[0].Width);
         ptr[1] = fui(1.0 / ctx->viewports[0].Height);
         ptr[2] = fui(ctx->rast->base.point_size);
         ptr[3] = fui(D3D12_MAX_POINT_SIZE);
         size += 4;
         break;
      default:
         unreachable("unknown state variable");
      }
   }

   return size;
}

static bool
check_descriptors_left(struct d3d12_context *ctx)
{
   struct d3d12_batch *batch = d3d12_current_batch(ctx);
   unsigned needed_descs = 0;

   for (unsigned i = 0; i < D3D12_GFX_SHADER_STAGES; ++i) {
      struct d3d12_shader_selector *shader = ctx->gfx_stages[i];

      if (!shader)
         continue;

      needed_descs += shader->current->num_cb_bindings;
      needed_descs += shader->current->num_srv_bindings;
   }

   if (d3d12_descriptor_heap_get_remaining_handles(batch->view_heap) < needed_descs)
      return false;

   needed_descs = 0;
   for (unsigned i = 0; i < D3D12_GFX_SHADER_STAGES; ++i) {
      struct d3d12_shader_selector *shader = ctx->gfx_stages[i];

      if (!shader)
         continue;

      needed_descs += shader->current->num_srv_bindings;
   }

   if (d3d12_descriptor_heap_get_remaining_handles(batch->sampler_heap) < needed_descs)
      return false;

   return true;
}

static void
set_graphics_root_parameters(struct d3d12_context *ctx)
{
   unsigned num_params = 0;

   for (unsigned i = 0; i < D3D12_GFX_SHADER_STAGES; ++i) {
      if (!ctx->gfx_stages[i])
         continue;

      struct d3d12_shader *shader = ctx->gfx_stages[i]->current;
      assert(shader);

      if (shader->num_cb_bindings > 0) {
         ctx->cmdlist->SetGraphicsRootDescriptorTable(num_params++,
                                                      fill_cbv_descriptors(ctx, shader, i));
      }
      if (shader->num_srv_bindings > 0) {
         ctx->cmdlist->SetGraphicsRootDescriptorTable(num_params++,
                                                      fill_srv_descriptors(ctx, shader, i));
         ctx->cmdlist->SetGraphicsRootDescriptorTable(num_params++,
                                                      fill_sampler_descriptors(ctx, shader, i));
      }
      if (shader->num_state_vars > 0) {
         uint32_t constants[D3D12_MAX_STATE_VARS * 4];
         unsigned size = fill_state_vars(ctx, shader, constants);
         ctx->cmdlist->SetGraphicsRoot32BitConstants(num_params++, size, constants, 0);
      }
   }
}

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
static D3D_PRIMITIVE_TOPOLOGY
topology(enum pipe_prim_type prim_type)
{
   switch (prim_type) {
   case PIPE_PRIM_POINTS:
      return D3D_PRIMITIVE_TOPOLOGY_POINTLIST;

   case PIPE_PRIM_LINES:
      return D3D_PRIMITIVE_TOPOLOGY_LINELIST;

   case PIPE_PRIM_LINE_STRIP:
      return D3D_PRIMITIVE_TOPOLOGY_LINESTRIP;

   case PIPE_PRIM_TRIANGLES:
      return D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;

   case PIPE_PRIM_TRIANGLE_STRIP:
      return D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP;

/*
   case PIPE_PRIM_PATCHES:
      return D3D_PRIMITIVE_TOPOLOGY_PATCHLIST;
*/

   case PIPE_PRIM_QUADS:
   case PIPE_PRIM_QUAD_STRIP:
      return D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST; /* HACK: this is just wrong! */

   default:
      debug_printf("pipe_prim_type: %s\n", u_prim_name(prim_type));
      unreachable("unexpected enum pipe_prim_type");
   }
}

static ID3D12PipelineState *
get_gfx_pipeline_state(struct d3d12_context *ctx,
                       ID3D12RootSignature *root_sig,
                       enum pipe_prim_type prim_type)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   enum pipe_prim_type reduced_prim = u_reduced_prim(prim_type);

   D3D12_GRAPHICS_PIPELINE_STATE_DESC pso_desc = { 0 };
   pso_desc.pRootSignature = root_sig;

   if (ctx->gfx_stages[PIPE_SHADER_VERTEX]) {
      auto shader = ctx->gfx_stages[PIPE_SHADER_VERTEX]->current;
      pso_desc.VS.BytecodeLength = shader->bytecode_length;
      pso_desc.VS.pShaderBytecode = shader->bytecode;
   }

   if (ctx->gfx_stages[PIPE_SHADER_FRAGMENT]) {
      auto shader = ctx->gfx_stages[PIPE_SHADER_FRAGMENT]->current;
      pso_desc.PS.BytecodeLength = shader->bytecode_length;
      pso_desc.PS.pShaderBytecode = shader->bytecode;
   }

   if (ctx->gfx_stages[PIPE_SHADER_GEOMETRY]) {
      auto shader = ctx->gfx_stages[PIPE_SHADER_GEOMETRY]->current;
      pso_desc.GS.BytecodeLength = shader->bytecode_length;
      pso_desc.GS.pShaderBytecode = shader->bytecode;
   }

   pso_desc.BlendState = ctx->blend->desc;
   pso_desc.DepthStencilState = ctx->depth_stencil_alpha_state->desc;
   pso_desc.SampleMask = ctx->sample_mask;
   pso_desc.RasterizerState = ctx->rast->desc;

   if (reduced_prim != PIPE_PRIM_TRIANGLES)
      pso_desc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;

   if (depth_bias(ctx->rast, reduced_prim)) {
      pso_desc.RasterizerState.DepthBias = ctx->rast->base.offset_units;
      pso_desc.RasterizerState.DepthBiasClamp = ctx->rast->base.offset_clamp;
      pso_desc.RasterizerState.SlopeScaledDepthBias = ctx->rast->base.offset_scale;
   }

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

static DXGI_FORMAT
ib_format(unsigned index_size)
{
   switch (index_size) {
   case 1: return DXGI_FORMAT_R8_UINT;
   case 2: return DXGI_FORMAT_R16_UINT;
   case 4: return DXGI_FORMAT_R32_UINT;

   default:
      unreachable("unexpected index-buffer size");
   }
}

void
d3d12_draw_vbo(struct pipe_context *pctx,
               const struct pipe_draw_info *dinfo)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   struct d3d12_batch *batch;

   d3d12_select_shader_variants(ctx, dinfo);

   if (dinfo->mode >= PIPE_PRIM_QUADS ||
       dinfo->mode == PIPE_PRIM_LINE_LOOP ||
       dinfo->mode == PIPE_PRIM_TRIANGLE_FAN ||
       dinfo->index_size == 1) {
      if (!u_trim_pipe_prim(dinfo->mode, (unsigned *)&dinfo->count))
         return;

      util_primconvert_save_rasterizer_state(ctx->primconvert, &ctx->rast->base);
      util_primconvert_draw_vbo(ctx->primconvert, dinfo);
      return;
   }

   if (!check_descriptors_left(ctx))
      d3d12_flush_cmdlist(ctx);
   batch = d3d12_current_batch(ctx);

   /* this should *really* be fixed at a higher level than here! */
   enum pipe_prim_type reduced_prim = u_reduced_prim(dinfo->mode);
   if (reduced_prim == PIPE_PRIM_TRIANGLES &&
       ctx->rast->base.cull_face == PIPE_FACE_FRONT_AND_BACK)
      return;

   unsigned index_offset = 0;
   struct pipe_resource *index_buffer = NULL;
   if (dinfo->index_size > 0) {
      assert(dinfo->index_size != 1);
      if (dinfo->has_user_indices) {
         if (!util_upload_index_buffer(pctx, dinfo, &index_buffer,
             &index_offset, 4)) {
            debug_printf("util_upload_index_buffer() failed\n");
            return;
         }
      } else
         index_buffer = dinfo->index.resource;
   }

   ID3D12RootSignature *root_sig = get_root_signature(ctx);
   ID3D12PipelineState *pipeline_state =
      get_gfx_pipeline_state(ctx, root_sig, reduced_prim);
   assert(pipeline_state);

   d3d12_batch_reference_object(batch, root_sig);
   ctx->cmdlist->SetGraphicsRootSignature(root_sig);
   root_sig->Release();

   set_graphics_root_parameters(ctx);

   ctx->cmdlist->RSSetViewports(ctx->num_viewports, ctx->viewports);
   if (ctx->rast->base.scissor && ctx->num_scissors > 0)
      ctx->cmdlist->RSSetScissorRects(ctx->num_scissors, ctx->scissors);
   else {
      D3D12_RECT fb_scissor;
      fb_scissor.left = 0;
      fb_scissor.top = 0;
      fb_scissor.right = ctx->fb.width;
      fb_scissor.bottom = ctx->fb.height;
      ctx->cmdlist->RSSetScissorRects(1, &fb_scissor);
   }

   d3d12_batch_reference_object(batch, pipeline_state);
   ctx->cmdlist->SetPipelineState(pipeline_state);
   pipeline_state->Release();

   if (ctx->blend->blend_factor_flags & (D3D12_BLEND_FACTOR_COLOR | D3D12_BLEND_FACTOR_ANY)) {
      ctx->cmdlist->OMSetBlendFactor(ctx->blend_factor);
   } else if (ctx->blend->blend_factor_flags & D3D12_BLEND_FACTOR_ALPHA) {
      float alpha_const[4] = { ctx->blend_factor[3], ctx->blend_factor[3],
                               ctx->blend_factor[3], ctx->blend_factor[3] };
      ctx->cmdlist->OMSetBlendFactor(alpha_const);
   }


   D3D12_CPU_DESCRIPTOR_HANDLE render_targets[PIPE_MAX_COLOR_BUFS] = {};
   D3D12_CPU_DESCRIPTOR_HANDLE *depth_desc = NULL, tmp_desc;
   for (int i = 0; i < ctx->fb.nr_cbufs; ++i) {
      struct d3d12_surface *surface = d3d12_surface(ctx->fb.cbufs[i]);
      render_targets[i] = surface->desc_handle.cpu_handle;
      d3d12_batch_reference_surface(batch, surface);
   }
   if (ctx->fb.zsbuf) {
      struct d3d12_surface *surface = d3d12_surface(ctx->fb.zsbuf);
      tmp_desc = surface->desc_handle.cpu_handle;
      d3d12_batch_reference_surface(batch, surface);
      depth_desc = &tmp_desc;
   }
   ctx->cmdlist->OMSetRenderTargets(ctx->fb.nr_cbufs, render_targets, FALSE, depth_desc);
   ctx->cmdlist->OMSetStencilRef(ctx->stencil_ref.ref_value[0]);

   ctx->cmdlist->IASetPrimitiveTopology(topology(dinfo->mode));

   for (int i = 0; i < ctx->num_vbs; ++i) {
      if (ctx->vbs[i].buffer.resource)
         d3d12_batch_reference_resource(batch, d3d12_resource(ctx->vbs[i].buffer.resource));
   }
   ctx->cmdlist->IASetVertexBuffers(0, ctx->num_vbs, ctx->vbvs);

   for (int i = 0; i < ctx->fb.nr_cbufs; ++i) {
      struct pipe_surface *psurf = ctx->fb.cbufs[i];
      d3d12_resource_barrier(ctx, d3d12_resource(psurf->texture),
                             D3D12_RESOURCE_STATE_COMMON,
                             D3D12_RESOURCE_STATE_RENDER_TARGET);
   }
   if (ctx->fb.zsbuf) {
      d3d12_resource_barrier(ctx, d3d12_resource(ctx->fb.zsbuf->texture),
                             D3D12_RESOURCE_STATE_COMMON,
                             D3D12_RESOURCE_STATE_DEPTH_WRITE);
   }

   if (dinfo->index_size > 0) {
      struct d3d12_resource *res = d3d12_resource(index_buffer);
      D3D12_INDEX_BUFFER_VIEW ibv;
      ibv.BufferLocation = res->res->GetGPUVirtualAddress() + index_offset;
      ibv.SizeInBytes = res->base.width0 - index_offset;
      ibv.Format = ib_format(dinfo->index_size);

      d3d12_batch_reference_resource(batch, res);
      ctx->cmdlist->IASetIndexBuffer(&ibv);
      ctx->cmdlist->DrawIndexedInstanced(dinfo->count, dinfo->instance_count,
                                         dinfo->start, dinfo->index_bias,
                                         dinfo->start_instance);
   } else
      ctx->cmdlist->DrawInstanced(dinfo->count, dinfo->instance_count,
                                  dinfo->start, dinfo->start_instance);

   for (int i = 0; i < ctx->fb.nr_cbufs; ++i) {
      struct pipe_surface *psurf = ctx->fb.cbufs[i];
      d3d12_resource_barrier(ctx, d3d12_resource(psurf->texture),
                             D3D12_RESOURCE_STATE_RENDER_TARGET,
                             D3D12_RESOURCE_STATE_COMMON);
   }
   if (ctx->fb.zsbuf) {
      d3d12_resource_barrier(ctx, d3d12_resource(ctx->fb.zsbuf->texture),
                             D3D12_RESOURCE_STATE_DEPTH_WRITE,
                             D3D12_RESOURCE_STATE_COMMON);
   }

   if (dinfo->index_size && index_buffer != dinfo->index.resource)
      pipe_resource_reference(&index_buffer, NULL);
}
