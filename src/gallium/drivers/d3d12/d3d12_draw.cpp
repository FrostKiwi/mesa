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
#include "d3d12_root_signature.h"
#include "d3d12_screen.h"
#include "d3d12_surface.h"

#include "util/u_debug.h"
#include "util/u_helpers.h"
#include "util/u_inlines.h"
#include "util/u_prim.h"
#include "util/u_math.h"

extern "C" {
#include "indices/u_primconvert.h"
}

static const D3D12_RECT MAX_SCISSOR = { D3D12_VIEWPORT_BOUNDS_MIN,
                                        D3D12_VIEWPORT_BOUNDS_MIN,
                                        D3D12_VIEWPORT_BOUNDS_MAX,
                                        D3D12_VIEWPORT_BOUNDS_MAX };

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
         ptr[2] = fui(ctx->gfx_pipeline_state.rast->base.point_size);
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
      uint64_t dirty = ctx->shader_dirty[i];
      assert(shader);

      if (shader->num_cb_bindings > 0) {
         if (dirty & D3D12_SHADER_DIRTY_CONSTBUF)
            ctx->cmdlist->SetGraphicsRootDescriptorTable(num_params, fill_cbv_descriptors(ctx, shader, i));
         num_params++;
      }
      if (shader->num_srv_bindings > 0) {
         if (dirty & D3D12_SHADER_DIRTY_SAMPLER_VIEWS)
            ctx->cmdlist->SetGraphicsRootDescriptorTable(num_params, fill_srv_descriptors(ctx, shader, i));
         num_params++;
         if (dirty & D3D12_SHADER_DIRTY_SAMPLERS)
            ctx->cmdlist->SetGraphicsRootDescriptorTable(num_params, fill_sampler_descriptors(ctx, shader, i));
         num_params++;
      }
      /* TODO Don't always update state vars */
      if (shader->num_state_vars > 0) {
         uint32_t constants[D3D12_MAX_STATE_VARS * 4];
         unsigned size = fill_state_vars(ctx, shader, constants);
         ctx->cmdlist->SetGraphicsRoot32BitConstants(num_params, size, constants, 0);
         num_params++;
      }
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

   if (dinfo->mode >= PIPE_PRIM_QUADS ||
       dinfo->mode == PIPE_PRIM_LINE_LOOP ||
       dinfo->mode == PIPE_PRIM_TRIANGLE_FAN ||
       dinfo->index_size == 1) {
      if (!u_trim_pipe_prim(dinfo->mode, (unsigned *)&dinfo->count))
         return;

      util_primconvert_save_rasterizer_state(ctx->primconvert, &ctx->gfx_pipeline_state.rast->base);
      util_primconvert_draw_vbo(ctx->primconvert, dinfo);
      return;
   }

   /* this should *really* be fixed at a higher level than here! */
   enum pipe_prim_type reduced_prim = u_reduced_prim(dinfo->mode);
   if (reduced_prim == PIPE_PRIM_TRIANGLES &&
       ctx->gfx_pipeline_state.rast->base.cull_face == PIPE_FACE_FRONT_AND_BACK)
      return;

   if (ctx->gfx_pipeline_state.prim_type != dinfo->mode) {
      ctx->gfx_pipeline_state.prim_type = dinfo->mode;
      ctx->state_dirty |= D3D12_DIRTY_PRIM_MODE;
   }

   d3d12_select_shader_variants(ctx, dinfo);
   for (unsigned i = 0; i < D3D12_GFX_SHADER_STAGES; ++i) {
      struct d3d12_shader *shader = ctx->gfx_stages[i] ? ctx->gfx_stages[i]->current : NULL;
      if (ctx->gfx_pipeline_state.stages[i] != shader) {
         ctx->gfx_pipeline_state.stages[i] = shader;
         ctx->state_dirty |= D3D12_DIRTY_SHADER;
         ctx->shader_dirty[i] |= D3D12_SHADER_DIRTY_ALL;
      }
   }

   if (!ctx->gfx_pipeline_state.root_signature || ctx->state_dirty & D3D12_DIRTY_SHADER) {
      ID3D12RootSignature *root_signature = d3d12_get_root_signature(ctx);
      if (ctx->gfx_pipeline_state.root_signature != root_signature) {
         ctx->gfx_pipeline_state.root_signature = root_signature;
         ctx->state_dirty |= D3D12_DIRTY_ROOT_SIGNATURE;
      }
   }

   if (!ctx->current_pso || ctx->state_dirty & D3D12_DIRTY_PSO) {
      ctx->current_pso = d3d12_get_gfx_pipeline_state(ctx);
      assert(ctx->current_pso);
   }

   ctx->cmdlist_dirty |= ctx->state_dirty;

   if (!check_descriptors_left(ctx))
      d3d12_flush_cmdlist(ctx);
   batch = d3d12_current_batch(ctx);

   if (ctx->cmdlist_dirty & D3D12_DIRTY_ROOT_SIGNATURE) {
      d3d12_batch_reference_object(batch, ctx->gfx_pipeline_state.root_signature);
      ctx->cmdlist->SetGraphicsRootSignature(ctx->gfx_pipeline_state.root_signature);
   }

   if (ctx->cmdlist_dirty & D3D12_DIRTY_PSO) {
      assert(ctx->current_pso);
      d3d12_batch_reference_object(batch, ctx->current_pso);
      ctx->cmdlist->SetPipelineState(ctx->current_pso);
   }

   set_graphics_root_parameters(ctx);

   if (ctx->cmdlist_dirty & D3D12_DIRTY_VIEWPORT)
      ctx->cmdlist->RSSetViewports(ctx->num_viewports, ctx->viewports);

   if (ctx->cmdlist_dirty & D3D12_DIRTY_SCISSOR) {
      if (ctx->gfx_pipeline_state.rast->base.scissor && ctx->num_scissors > 0)
         ctx->cmdlist->RSSetScissorRects(ctx->num_scissors, ctx->scissors);
      else
         ctx->cmdlist->RSSetScissorRects(1, &MAX_SCISSOR);
   }

   if (ctx->cmdlist_dirty & D3D12_DIRTY_BLEND_COLOR) {
      unsigned blend_factor_flags = ctx->gfx_pipeline_state.blend->blend_factor_flags;
      if (blend_factor_flags & (D3D12_BLEND_FACTOR_COLOR | D3D12_BLEND_FACTOR_ANY)) {
         ctx->cmdlist->OMSetBlendFactor(ctx->blend_factor);
      } else if (blend_factor_flags & D3D12_BLEND_FACTOR_ALPHA) {
         float alpha_const[4] = { ctx->blend_factor[3], ctx->blend_factor[3],
                                 ctx->blend_factor[3], ctx->blend_factor[3] };
         ctx->cmdlist->OMSetBlendFactor(alpha_const);
      }
   }

   if (ctx->cmdlist_dirty & D3D12_DIRTY_STENCIL_REF)
      ctx->cmdlist->OMSetStencilRef(ctx->stencil_ref.ref_value[0]);

   if (ctx->cmdlist_dirty & D3D12_DIRTY_PRIM_MODE)
      ctx->cmdlist->IASetPrimitiveTopology(topology(dinfo->mode));

   if (ctx->cmdlist_dirty & D3D12_DIRTY_VERTEX_BUFFERS) {
      for (unsigned i = 0; i < ctx->num_vbs; ++i) {
         if (ctx->vbs[i].buffer.resource) {
            struct d3d12_resource *res = d3d12_resource(ctx->vbs[i].buffer.resource);
            d3d12_batch_reference_resource(batch, res);
         }
      }
      ctx->cmdlist->IASetVertexBuffers(0, ctx->num_vbs, ctx->vbvs);
   }

   if (dinfo->index_size > 0) {
      assert(dinfo->index_size != 1);
      unsigned index_offset = 0;
      struct pipe_resource *index_buffer = NULL;

      if (dinfo->has_user_indices) {
         if (!util_upload_index_buffer(pctx, dinfo, &index_buffer,
             &index_offset, 4)) {
            debug_printf("util_upload_index_buffer() failed\n");
            return;
         }
      } else {
         index_buffer = dinfo->index.resource;
      }

      D3D12_INDEX_BUFFER_VIEW ibv;
      struct d3d12_resource *res = d3d12_resource(index_buffer);
      ibv.BufferLocation = res->res->GetGPUVirtualAddress() + index_offset;
      ibv.SizeInBytes = res->base.width0 - index_offset;
      ibv.Format = ib_format(dinfo->index_size);
      if (ctx->cmdlist_dirty & D3D12_DIRTY_INDEX_BUFFER ||
          memcmp(&ctx->ibv, &ibv, sizeof(D3D12_INDEX_BUFFER_VIEW)) != 0) {
         ctx->ibv = ibv;
         d3d12_batch_reference_resource(batch, res);
         ctx->cmdlist->IASetIndexBuffer(&ibv);
      }

      if (dinfo->has_user_indices)
         pipe_resource_reference(&index_buffer, NULL);
   }

   if (ctx->cmdlist_dirty & D3D12_DIRTY_FRAMEBUFFER) {
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
   }

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

   if (dinfo->index_size > 0)
      ctx->cmdlist->DrawIndexedInstanced(dinfo->count, dinfo->instance_count,
                                         dinfo->start, dinfo->index_bias,
                                         dinfo->start_instance);
   else
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

   ctx->state_dirty = 0;
   ctx->cmdlist_dirty = 0;
   for (unsigned i = 0; i < D3D12_GFX_SHADER_STAGES; ++i)
      ctx->shader_dirty[i] = 0;
}
