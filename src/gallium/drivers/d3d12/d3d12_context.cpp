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

#include "d3d12_compiler.h"
#include "d3d12_format.h"
#include "d3d12_resource.h"
#include "d3d12_screen.h"
#include "d3d12_surface.h"

#include "util/u_framebuffer.h"
#include "util/u_helpers.h"
#include "util/u_memory.h"
#include "util/u_upload_mgr.h"

extern "C" {
#include "indices/u_primconvert.h"
}

static void
d3d12_context_destroy(struct pipe_context *pctx)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   d3d12_validator_destroy(ctx->validation_tools);

   /* FIXME: Wait for the queue to be idle */
   ctx->cmdalloc->Release();
   ctx->cmdlist->Release();
   ctx->cmdqueue_fence->Release();
   CloseHandle(ctx->event);
   d3d12_descriptor_heap_free(ctx->rtv_heap);
   d3d12_descriptor_heap_free(ctx->dsv_heap);
   util_primconvert_destroy(ctx->primconvert);
   slab_destroy_child(&ctx->transfer_pool);
   FREE(ctx);
}

static void *
d3d12_create_vertex_elements_state(struct pipe_context *pctx,
                                   unsigned num_elements,
                                   const struct pipe_vertex_element *elements)
{
   struct d3d12_vertex_elements_state *cso = CALLOC_STRUCT(d3d12_vertex_elements_state);
   if (!cso)
      return NULL;

   for (unsigned i = 0; i < num_elements; ++i) {
      cso->elements[i].SemanticName = "TEXCOORD";
      cso->elements[i].SemanticIndex = i;
      cso->elements[i].Format = d3d12_get_format(elements[i].src_format);
      assert(cso->elements[i].Format != DXGI_FORMAT_UNKNOWN);
      cso->elements[i].InputSlot = elements[i].vertex_buffer_index;
      cso->elements[i].AlignedByteOffset = elements[i].src_offset;

      if (elements[i].instance_divisor) {
         cso->elements[i].InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_INSTANCE_DATA;
         cso->elements[i].InstanceDataStepRate = elements[i].instance_divisor;
      } else {
         cso->elements[i].InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA;
         cso->elements[i].InstanceDataStepRate = 0;
      }

      // HACK THE PLANET!
      if (i == 0) {
         cso->elements[i].SemanticName = "POSITION";
         cso->elements[i].SemanticIndex = 0;
      } else if (i == 1) {
         cso->elements[i].SemanticName = "COLOR";
         cso->elements[i].SemanticIndex = 0;
      }
   }
   // HACK THE PLANET 2.0!
   if (num_elements == 1) {
      cso->elements[1] = cso->elements[0];
      cso->elements[1].SemanticName = "COLOR";
      num_elements++;
   }

   cso->num_elements = num_elements;
   return cso;
}

static void
d3d12_bind_vertex_elements_state(struct pipe_context *pctx,
                                 void *ve)
{
   d3d12_context(pctx)->ves = (struct d3d12_vertex_elements_state *)ve;
}

static void
d3d12_delete_vertex_elements_state(struct pipe_context *pctx,
                                   void *ve)
{
   FREE(ve);
}

static D3D12_BLEND
blend_factor(enum pipe_blendfactor factor)
{
   switch (factor) {
   case PIPE_BLENDFACTOR_ZERO: return D3D12_BLEND_ZERO;
   case PIPE_BLENDFACTOR_ONE: return D3D12_BLEND_ONE;
   case PIPE_BLENDFACTOR_SRC_COLOR: return D3D12_BLEND_SRC_COLOR;
   case PIPE_BLENDFACTOR_SRC_ALPHA: return D3D12_BLEND_SRC_ALPHA;
   case PIPE_BLENDFACTOR_DST_ALPHA: return D3D12_BLEND_DEST_ALPHA;
   case PIPE_BLENDFACTOR_DST_COLOR: return D3D12_BLEND_DEST_COLOR;
   case PIPE_BLENDFACTOR_SRC_ALPHA_SATURATE: return D3D12_BLEND_SRC_ALPHA_SAT;
   case PIPE_BLENDFACTOR_CONST_COLOR: return D3D12_BLEND_BLEND_FACTOR;
   case PIPE_BLENDFACTOR_SRC1_COLOR: return D3D12_BLEND_SRC1_COLOR;
   case PIPE_BLENDFACTOR_SRC1_ALPHA: return D3D12_BLEND_SRC1_ALPHA;
   case PIPE_BLENDFACTOR_INV_SRC_COLOR: return D3D12_BLEND_INV_SRC_COLOR;
   case PIPE_BLENDFACTOR_INV_SRC_ALPHA: return D3D12_BLEND_INV_SRC_ALPHA;
   case PIPE_BLENDFACTOR_INV_DST_ALPHA: return D3D12_BLEND_INV_DEST_ALPHA;
   case PIPE_BLENDFACTOR_INV_DST_COLOR: return D3D12_BLEND_INV_DEST_COLOR;
   case PIPE_BLENDFACTOR_INV_CONST_COLOR: return D3D12_BLEND_INV_BLEND_FACTOR;
   case PIPE_BLENDFACTOR_INV_SRC1_COLOR: return D3D12_BLEND_INV_SRC1_COLOR;
   case PIPE_BLENDFACTOR_INV_SRC1_ALPHA: return D3D12_BLEND_INV_SRC1_ALPHA;
   case PIPE_BLENDFACTOR_CONST_ALPHA: return D3D12_BLEND_BLEND_FACTOR; /* Doesn't exist in D3D12 */
   case PIPE_BLENDFACTOR_INV_CONST_ALPHA: return D3D12_BLEND_INV_BLEND_FACTOR; /* Doesn't exist in D3D12 */
   }
   unreachable("unexpected blend factor");
}

static bool
need_blend_factor(enum pipe_blendfactor factor)
{
   switch (factor) {
   case PIPE_BLENDFACTOR_CONST_COLOR:
   case PIPE_BLENDFACTOR_CONST_ALPHA:
   case PIPE_BLENDFACTOR_INV_CONST_COLOR:
   case PIPE_BLENDFACTOR_INV_CONST_ALPHA:
      return true;

   default:
      return false;
   }
}

static D3D12_BLEND_OP
blend_op(enum pipe_blend_func func)
{
   switch (func) {
   case PIPE_BLEND_ADD: return D3D12_BLEND_OP_ADD;
   case PIPE_BLEND_SUBTRACT: return D3D12_BLEND_OP_SUBTRACT;
   case PIPE_BLEND_REVERSE_SUBTRACT: return D3D12_BLEND_OP_REV_SUBTRACT;
   case PIPE_BLEND_MIN: return D3D12_BLEND_OP_MIN;
   case PIPE_BLEND_MAX: return D3D12_BLEND_OP_MAX;
   }
   unreachable("unexpected blend function");
}

static D3D12_COMPARISON_FUNC
compare_op(enum pipe_compare_func op)
{
   switch (op) {
      case PIPE_FUNC_NEVER: return D3D12_COMPARISON_FUNC_NEVER;
      case PIPE_FUNC_LESS: return D3D12_COMPARISON_FUNC_LESS;
      case PIPE_FUNC_EQUAL: return D3D12_COMPARISON_FUNC_EQUAL;
      case PIPE_FUNC_LEQUAL: return D3D12_COMPARISON_FUNC_LESS_EQUAL;
      case PIPE_FUNC_GREATER: return D3D12_COMPARISON_FUNC_GREATER;
      case PIPE_FUNC_NOTEQUAL: return D3D12_COMPARISON_FUNC_NOT_EQUAL;
      case PIPE_FUNC_GEQUAL: return D3D12_COMPARISON_FUNC_GREATER_EQUAL;
      case PIPE_FUNC_ALWAYS: return D3D12_COMPARISON_FUNC_ALWAYS;
   }
   unreachable("unexpected compare");
}

static D3D12_LOGIC_OP
logic_op(enum pipe_logicop func)
{
   switch (func) {
   case PIPE_LOGICOP_CLEAR: return D3D12_LOGIC_OP_CLEAR;
   case PIPE_LOGICOP_NOR: return D3D12_LOGIC_OP_NOR;
   case PIPE_LOGICOP_AND_INVERTED: return D3D12_LOGIC_OP_AND_INVERTED;
   case PIPE_LOGICOP_COPY_INVERTED: return D3D12_LOGIC_OP_COPY_INVERTED;
   case PIPE_LOGICOP_AND_REVERSE: return D3D12_LOGIC_OP_AND_REVERSE;
   case PIPE_LOGICOP_INVERT: return D3D12_LOGIC_OP_INVERT;
   case PIPE_LOGICOP_XOR: return D3D12_LOGIC_OP_XOR;
   case PIPE_LOGICOP_NAND: return D3D12_LOGIC_OP_NAND;
   case PIPE_LOGICOP_AND: return D3D12_LOGIC_OP_AND;
   case PIPE_LOGICOP_EQUIV: return D3D12_LOGIC_OP_EQUIV;
   case PIPE_LOGICOP_NOOP: return D3D12_LOGIC_OP_NOOP;
   case PIPE_LOGICOP_OR_INVERTED: return D3D12_LOGIC_OP_OR_INVERTED;
   case PIPE_LOGICOP_COPY: return D3D12_LOGIC_OP_COPY;
   case PIPE_LOGICOP_OR_REVERSE: return D3D12_LOGIC_OP_OR_REVERSE;
   case PIPE_LOGICOP_OR: return D3D12_LOGIC_OP_OR;
   case PIPE_LOGICOP_SET: return D3D12_LOGIC_OP_SET;
   }
   unreachable("unexpected logicop function");
}

static UINT8
color_write_mask(unsigned colormask)
{
   UINT8 mask = 0;

   if (colormask & PIPE_MASK_R)
      mask |= D3D12_COLOR_WRITE_ENABLE_RED;
   if (colormask & PIPE_MASK_G)
      mask |= D3D12_COLOR_WRITE_ENABLE_GREEN;
   if (colormask & PIPE_MASK_B)
      mask |= D3D12_COLOR_WRITE_ENABLE_BLUE;
   if (colormask & PIPE_MASK_A)
      mask |= D3D12_COLOR_WRITE_ENABLE_ALPHA;

   return mask;
}

static void *
d3d12_create_blend_state(struct pipe_context *pctx,
                         const struct pipe_blend_state *blend_state)
{
   struct d3d12_blend_state *state = CALLOC_STRUCT(d3d12_blend_state);
   if (!state)
      return NULL;

   /* TODO Dithering */

   state->desc.AlphaToCoverageEnable = blend_state->alpha_to_coverage;

   int num_targets = blend_state->independent_blend_enable ? PIPE_MAX_COLOR_BUFS : 1;
   for (int i = 0; i < num_targets; ++i) {
      const struct pipe_rt_blend_state *rt = blend_state->rt + i;

      if (rt->blend_enable) {
         state->desc.RenderTarget[i].BlendEnable = TRUE;
         state->desc.RenderTarget[i].SrcBlend = blend_factor((pipe_blendfactor) rt->rgb_src_factor);
         state->desc.RenderTarget[i].DestBlend = blend_factor((pipe_blendfactor) rt->rgb_dst_factor);
         state->desc.RenderTarget[i].BlendOp = blend_op((pipe_blend_func) rt->rgb_func);
         state->desc.RenderTarget[i].SrcBlendAlpha = blend_factor((pipe_blendfactor) rt->alpha_src_factor);
         state->desc.RenderTarget[i].DestBlendAlpha = blend_factor((pipe_blendfactor) rt->alpha_dst_factor);
         state->desc.RenderTarget[i].BlendOpAlpha = blend_op((pipe_blend_func) rt->alpha_func);

         if (need_blend_factor((pipe_blendfactor) rt->rgb_src_factor) ||
             need_blend_factor((pipe_blendfactor) rt->rgb_dst_factor) ||
             need_blend_factor((pipe_blendfactor) rt->alpha_src_factor) ||
             need_blend_factor((pipe_blendfactor) rt->alpha_dst_factor))
            state->need_blend_factor = TRUE;

         if (blend_state->logicop_enable) {
            state->desc.RenderTarget[i].LogicOpEnable = TRUE;
            state->desc.RenderTarget[i].LogicOp = logic_op((pipe_logicop) blend_state->logicop_func);
         }
      }

      state->desc.RenderTarget[i].RenderTargetWriteMask = color_write_mask(rt->colormask);
   }

   return state;
}

static void
d3d12_bind_blend_state(struct pipe_context *pctx, void *blend_state)
{
   d3d12_context(pctx)->blend = (struct d3d12_blend_state *) blend_state;
}

static void
d3d12_delete_blend_state(struct pipe_context *pctx, void *blend_state)
{
   FREE(blend_state);
}

static D3D12_STENCIL_OP
stencil_op(enum pipe_stencil_op op)
{
   switch (op) {
   case PIPE_STENCIL_OP_KEEP: return D3D12_STENCIL_OP_KEEP;
   case PIPE_STENCIL_OP_ZERO: return D3D12_STENCIL_OP_ZERO;
   case PIPE_STENCIL_OP_REPLACE: return D3D12_STENCIL_OP_REPLACE;
   case PIPE_STENCIL_OP_INCR: return D3D12_STENCIL_OP_INCR_SAT;
   case PIPE_STENCIL_OP_DECR: return D3D12_STENCIL_OP_DECR_SAT;
   case PIPE_STENCIL_OP_INCR_WRAP: return D3D12_STENCIL_OP_INCR;
   case PIPE_STENCIL_OP_DECR_WRAP: return D3D12_STENCIL_OP_DECR;
   case PIPE_STENCIL_OP_INVERT: return D3D12_STENCIL_OP_INVERT;
   }
   unreachable("unexpected op");
}

static D3D12_DEPTH_STENCILOP_DESC
stencil_op_state(const struct pipe_stencil_state *src)
{
   D3D12_DEPTH_STENCILOP_DESC ret;
   ret.StencilFailOp = stencil_op((pipe_stencil_op) src->fail_op);
   ret.StencilPassOp = stencil_op((pipe_stencil_op) src->zpass_op);
   ret.StencilDepthFailOp = stencil_op((pipe_stencil_op) src->zfail_op);
   ret.StencilFunc = compare_op((pipe_compare_func) src->func);
   return ret;
}

static void *
d3d12_create_depth_stencil_alpha_state(struct pipe_context *pctx,
                                       const struct pipe_depth_stencil_alpha_state *depth_stencil_alpha)
{
   struct d3d12_depth_stencil_alpha_state *dsa = CALLOC_STRUCT(d3d12_depth_stencil_alpha_state);
   if (!dsa)
      return NULL;

   if (depth_stencil_alpha->depth.enabled) {
      dsa->desc.DepthEnable = TRUE;
      dsa->desc.DepthFunc = compare_op((pipe_compare_func) depth_stencil_alpha->depth.func);
   }

   /* TODO Add support for GL_depth_bound_tests */
   #if 0
   if (depth_stencil_alpha->depth.bounds_test) {
      dsa->desc.DepthBoundsTestEnable = TRUE;
      dsa->min_depth_bounds = depth_stencil_alpha->depth.bounds_min;
      dsa->max_depth_bounds = depth_stencil_alpha->depth.bounds_max;
   }
   #endif

   if (depth_stencil_alpha->stencil[0].enabled) {
      dsa->desc.StencilEnable = TRUE;
      dsa->desc.FrontFace = stencil_op_state(depth_stencil_alpha->stencil);
   }

   if (depth_stencil_alpha->stencil[0].enabled) // XXX not index == 1?
      dsa->desc.BackFace = stencil_op_state(depth_stencil_alpha->stencil + 1);
   else
      dsa->desc.BackFace = dsa->desc.FrontFace;

   dsa->desc.StencilReadMask = depth_stencil_alpha->stencil[0].valuemask; /* FIXME Back face mask */
   dsa->desc.StencilWriteMask = depth_stencil_alpha->stencil[0].writemask; /* FIXME Back face mask */
   dsa->desc.DepthWriteMask = (D3D12_DEPTH_WRITE_MASK) depth_stencil_alpha->depth.writemask;

   return dsa;
}

static void
d3d12_bind_depth_stencil_alpha_state(struct pipe_context *pctx,
                                     void *dsa)
{
   d3d12_context(pctx)->depth_stencil_alpha_state = (struct d3d12_depth_stencil_alpha_state *) dsa;
}

static void
d3d12_delete_depth_stencil_alpha_state(struct pipe_context *pctx,
                                       void *dsa_state)
{
   FREE(dsa_state);
}

static D3D12_FILL_MODE
fill_mode(unsigned mode)
{
   switch (mode) {
   case PIPE_POLYGON_MODE_FILL:
      return D3D12_FILL_MODE_SOLID;
   case PIPE_POLYGON_MODE_LINE:
      return D3D12_FILL_MODE_WIREFRAME;

   default:
      unreachable("unsupported fill-mode");
   }
}

static D3D12_CULL_MODE
cull_mode(unsigned mode)
{
   switch (mode) {
   case PIPE_FACE_NONE:
      return D3D12_CULL_MODE_NONE;
   case PIPE_FACE_FRONT:
      return D3D12_CULL_MODE_FRONT;
   case PIPE_FACE_BACK:
      return D3D12_CULL_MODE_BACK;

   default:
      unreachable("unsupported cull-mode");
   }
}

static void *
d3d12_create_rasterizer_state(struct pipe_context *pctx,
                              const struct pipe_rasterizer_state *rs_state)
{
   struct d3d12_rasterizer_state *cso = CALLOC_STRUCT(d3d12_rasterizer_state);
   if (!cso)
      return NULL;

   cso->base = *rs_state;

   if (rs_state->fill_front != rs_state->fill_back)
      debug_printf("D3D12: unsupported fill-mode combination\n");

   assert(rs_state->depth_clip_near == rs_state->depth_clip_far);

   cso->desc.FillMode = fill_mode(rs_state->fill_front);
   cso->desc.CullMode = cull_mode(rs_state->cull_face);
   cso->desc.FrontCounterClockwise = rs_state->front_ccw;
   cso->desc.DepthBias = 0; // TODO
   cso->desc.DepthBiasClamp = 0; // TODO
   cso->desc.SlopeScaledDepthBias = 0; // TODO
   cso->desc.DepthClipEnable = rs_state->depth_clip_near;
   cso->desc.MultisampleEnable = rs_state->multisample;
   cso->desc.AntialiasedLineEnable = rs_state->line_smooth;
   cso->desc.ForcedSampleCount = 0; // TODO
   cso->desc.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF; // TODO

   return cso;
}

static void
d3d12_bind_rasterizer_state(struct pipe_context *pctx, void *rs_state)
{
   d3d12_context(pctx)->rast = (struct d3d12_rasterizer_state *)rs_state;
}

static void
d3d12_delete_rasterizer_state(struct pipe_context *pctx, void *rs_state)
{
   FREE(rs_state);
}

static void *
d3d12_create_sampler_state(struct pipe_context *pctx,
                           const struct pipe_sampler_state *state)
{
   return NULL;
}

static void
d3d12_bind_sampler_states(struct pipe_context *pctx,
                          enum pipe_shader_type shader,
                          unsigned start_slot,
                          unsigned num_samplers,
                          void **samplers)
{
}

static void
d3d12_delete_sampler_state(struct pipe_context *pctx,
                           void *ss)
{
}

static struct pipe_sampler_view *
d3d12_create_sampler_view(struct pipe_context *pctx,
                          struct pipe_resource *texture,
                          const struct pipe_sampler_view *state)
{
   return NULL;
}

static void
d3d12_set_sampler_views(struct pipe_context *pctx,
                        enum pipe_shader_type shader_type,
                        unsigned start_slot,
                        unsigned num_views,
                        struct pipe_sampler_view **views)
{
}

static void
d3d12_destroy_sampler_view(struct pipe_context *pctx,
                           struct pipe_sampler_view *view)
{
}

static void
bind_stage(struct d3d12_context *ctx, enum pipe_shader_type stage,
           struct d3d12_shader *shader)
{
   assert(stage < PIPE_SHADER_COMPUTE);
   ctx->gfx_stages[stage] = shader;
   ctx->dirty_program = true;
}

static void *
d3d12_create_vs_state(struct pipe_context *pctx,
                      const struct pipe_shader_state *shader)
{
   struct nir_shader *nir;

   assert(shader->type == PIPE_SHADER_IR_NIR);
   nir = (struct nir_shader *)shader->ir.nir;

   return d3d12_compile_nir(d3d12_context(pctx), nir);
}

static void
d3d12_bind_vs_state(struct pipe_context *pctx,
                    void *vss)
{
   bind_stage(d3d12_context(pctx), PIPE_SHADER_VERTEX, (struct d3d12_shader *) vss);
}

static void
d3d12_delete_vs_state(struct pipe_context *pctx,
                      void *vs)
{
   d3d12_shader_free((struct d3d12_shader *) vs);
}

static void *
d3d12_create_fs_state(struct pipe_context *pctx,
                      const struct pipe_shader_state *shader)
{
   struct nir_shader *nir;

   assert(shader->type == PIPE_SHADER_IR_NIR);
   nir = (struct nir_shader *)shader->ir.nir;

   return d3d12_compile_nir(d3d12_context(pctx), nir);
}

static void
d3d12_bind_fs_state(struct pipe_context *pctx,
                    void *fss)
{
   bind_stage(d3d12_context(pctx), PIPE_SHADER_FRAGMENT, (struct d3d12_shader *) fss);
}

static void
d3d12_delete_fs_state(struct pipe_context *pctx,
                      void *fs)
{
   d3d12_shader_free((struct d3d12_shader *) fs);
}

static void
d3d12_set_polygon_stipple(struct pipe_context *pctx,
                          const struct pipe_poly_stipple *ps)
{
}

static void
d3d12_set_vertex_buffers(struct pipe_context *pctx,
                         unsigned start_slot,
                         unsigned num_buffers,
                         const struct pipe_vertex_buffer *buffers)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   util_set_vertex_buffers_count(ctx->vbs, &ctx->num_vbs,
                                 buffers, start_slot, num_buffers);

   for (unsigned i = 0; i < ctx->num_vbs; ++i) {
      const struct pipe_vertex_buffer* buf = ctx->vbs + i;
      struct d3d12_resource *res = d3d12_resource(buf->buffer.resource);
      ctx->vbvs[i].BufferLocation = res->res->GetGPUVirtualAddress();
      ctx->vbvs[i].StrideInBytes = buf->stride;
      ctx->vbvs[i].SizeInBytes = res->base.width0;
   }
}

static void
d3d12_set_viewport_states(struct pipe_context *pctx,
                          unsigned start_slot,
                          unsigned num_viewports,
                          const struct pipe_viewport_state *state)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   for (unsigned i = 0; i < num_viewports; ++i) {
      ctx->viewports[start_slot + i].TopLeftX = state[i].translate[0] - state[i].scale[0];
      ctx->viewports[start_slot + i].TopLeftY = state[i].translate[1] + state[i].scale[1];
      ctx->viewports[start_slot + i].Width = state[i].scale[0] * 2;
      ctx->viewports[start_slot + i].Height = -state[i].scale[1] * 2;
      ctx->viewports[start_slot + i].MinDepth = state[i].translate[2] - state[i].scale[2];
      ctx->viewports[start_slot + i].MaxDepth = state[i].translate[2] + state[i].scale[2];
      ctx->viewport_states[start_slot + i] = state[i];
   }
   ctx->num_viewports = start_slot + num_viewports;
}


static void
d3d12_set_scissor_states(struct pipe_context *pctx,
                         unsigned start_slot, unsigned num_scissors,
                         const struct pipe_scissor_state *states)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   for (unsigned i = 0; i < num_scissors; i++) {
      ctx->scissors[start_slot + i].left = states[i].minx;
      ctx->scissors[start_slot + i].top = states[i].miny;
      ctx->scissors[start_slot + i].right = states[i].maxx;
      ctx->scissors[start_slot + i].bottom = states[i].maxy;
      ctx->scissor_states[start_slot + i] = states[i];
   }
   ctx->num_scissors = start_slot + num_scissors;
}

static void
d3d12_set_constant_buffer(struct pipe_context *pctx,
                          enum pipe_shader_type shader, uint index,
                          const struct pipe_constant_buffer *buf)
{
}

static void
d3d12_set_framebuffer_state(struct pipe_context *pctx,
                            const struct pipe_framebuffer_state *state)
{
   util_copy_framebuffer_state(&d3d12_context(pctx)->fb, state);
}

static void
d3d12_set_blend_color(struct pipe_context *pctx,
                     const struct pipe_blend_color *color)
{
   struct d3d12_context *ctx = d3d12_context(pctx);
   memcpy(ctx->blend_factor, color->color, sizeof(float) * 4);
}

void
d3d12_flush_cmdlist(struct d3d12_context *ctx)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   if (FAILED(ctx->cmdlist->Close())) {
      debug_printf("D3D12: closing ID3D12GraphicsCommandList failed\n");
      return;
   }

   ID3D12CommandList* cmdlists[] = { ctx->cmdlist };
   screen->cmdqueue->ExecuteCommandLists(1, cmdlists);
   int value = ++ctx->fence_value;
   ctx->cmdqueue_fence->SetEventOnCompletion(value, ctx->event);
   screen->cmdqueue->Signal(ctx->cmdqueue_fence, value);
   WaitForSingleObject(ctx->event, INFINITE);

   if (FAILED(ctx->cmdalloc->Reset())) {
      debug_printf("D3D12: resetting ID3D12CommandAllocator failed\n");
      return;
   }

   if (FAILED(ctx->cmdlist->Reset(ctx->cmdalloc, NULL))) {
      debug_printf("D3D12: resetting ID3D12GraphicsCommandList failed\n");
      return;
   }
}

void
d3d12_resource_barrier(struct d3d12_context *ctx,
                       struct d3d12_resource *res,
                       D3D12_RESOURCE_STATES before,
                       D3D12_RESOURCE_STATES after)
{
   D3D12_RESOURCE_BARRIER barrier;
   barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
   barrier.Flags = D3D12_RESOURCE_BARRIER_FLAG_NONE;
   barrier.Transition.pResource = res->res;
   barrier.Transition.Subresource = 0;
   barrier.Transition.StateBefore = before;
   barrier.Transition.StateAfter = after;
   ctx->cmdlist->ResourceBarrier(1, &barrier);
}

static void
d3d12_clear(struct pipe_context *pctx,
            unsigned buffers,
            const union pipe_color_union *color,
            double depth, unsigned stencil)
{
   struct d3d12_context *ctx = d3d12_context(pctx);

   if (buffers & PIPE_CLEAR_COLOR) {
      for (int i = 0; i < ctx->fb.nr_cbufs; ++i) {
         struct pipe_surface *psurf = ctx->fb.cbufs[i];
         struct d3d12_surface *surf = d3d12_surface(psurf);
         d3d12_resource_barrier(ctx, d3d12_resource(psurf->texture),
                                D3D12_RESOURCE_STATE_COMMON,
                                D3D12_RESOURCE_STATE_RENDER_TARGET);
         ctx->cmdlist->ClearRenderTargetView(surf->desc_handle.cpu_handle,
                                             color->f, 0, NULL);
         d3d12_resource_barrier(ctx, d3d12_resource(psurf->texture),
                                D3D12_RESOURCE_STATE_RENDER_TARGET,
                                D3D12_RESOURCE_STATE_COMMON);
      }
   }

   if (buffers & PIPE_CLEAR_DEPTHSTENCIL && ctx->fb.zsbuf) {
      struct d3d12_surface* surf = d3d12_surface(ctx->fb.zsbuf);

      D3D12_CLEAR_FLAGS flags = (D3D12_CLEAR_FLAGS)0;
      if (buffers & PIPE_CLEAR_DEPTH)
         flags |= D3D12_CLEAR_FLAG_DEPTH;
      if (buffers & PIPE_CLEAR_STENCIL)
         flags |= D3D12_CLEAR_FLAG_STENCIL;

      d3d12_resource_barrier(ctx, d3d12_resource(ctx->fb.zsbuf->texture),
                             D3D12_RESOURCE_STATE_COMMON,
                             D3D12_RESOURCE_STATE_DEPTH_WRITE);
      ctx->cmdlist->ClearDepthStencilView(surf->desc_handle.cpu_handle, flags,
                                          depth, stencil, 0, NULL);
      d3d12_resource_barrier(ctx, d3d12_resource(ctx->fb.zsbuf->texture),
                             D3D12_RESOURCE_STATE_DEPTH_WRITE,
                             D3D12_RESOURCE_STATE_COMMON);
   }

   d3d12_flush_cmdlist(ctx);
}

static void
d3d12_flush(struct pipe_context *pipe,
            struct pipe_fence_handle **fence,
            unsigned flags)
{
}

struct pipe_context *
d3d12_context_create(struct pipe_screen *pscreen, void *priv, unsigned flags)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);

   struct d3d12_context *ctx = CALLOC_STRUCT(d3d12_context);
   if (!ctx)
      return NULL;

   ctx->base.screen = pscreen;
   ctx->base.priv = priv;

   ctx->base.destroy = d3d12_context_destroy;

   ctx->base.create_vertex_elements_state = d3d12_create_vertex_elements_state;
   ctx->base.bind_vertex_elements_state = d3d12_bind_vertex_elements_state;
   ctx->base.delete_vertex_elements_state = d3d12_delete_vertex_elements_state;

   ctx->base.create_blend_state = d3d12_create_blend_state;
   ctx->base.bind_blend_state = d3d12_bind_blend_state;
   ctx->base.delete_blend_state = d3d12_delete_blend_state;

   ctx->base.create_depth_stencil_alpha_state = d3d12_create_depth_stencil_alpha_state;
   ctx->base.bind_depth_stencil_alpha_state = d3d12_bind_depth_stencil_alpha_state;
   ctx->base.delete_depth_stencil_alpha_state = d3d12_delete_depth_stencil_alpha_state;

   ctx->base.create_rasterizer_state = d3d12_create_rasterizer_state;
   ctx->base.bind_rasterizer_state = d3d12_bind_rasterizer_state;
   ctx->base.delete_rasterizer_state = d3d12_delete_rasterizer_state;

   ctx->base.create_sampler_state = d3d12_create_sampler_state;
   ctx->base.bind_sampler_states = d3d12_bind_sampler_states;
   ctx->base.delete_sampler_state = d3d12_delete_sampler_state;

   ctx->base.create_sampler_view = d3d12_create_sampler_view;
   ctx->base.set_sampler_views = d3d12_set_sampler_views;
   ctx->base.sampler_view_destroy = d3d12_destroy_sampler_view;

   ctx->base.create_vs_state = d3d12_create_vs_state;
   ctx->base.bind_vs_state = d3d12_bind_vs_state;
   ctx->base.delete_vs_state = d3d12_delete_vs_state;

   ctx->base.create_fs_state = d3d12_create_fs_state;
   ctx->base.bind_fs_state = d3d12_bind_fs_state;
   ctx->base.delete_fs_state = d3d12_delete_fs_state;

   ctx->base.set_polygon_stipple = d3d12_set_polygon_stipple;
   ctx->base.set_vertex_buffers = d3d12_set_vertex_buffers;
   ctx->base.set_viewport_states = d3d12_set_viewport_states;
   ctx->base.set_scissor_states = d3d12_set_scissor_states;
   ctx->base.set_constant_buffer = d3d12_set_constant_buffer;
   ctx->base.set_framebuffer_state = d3d12_set_framebuffer_state;
   ctx->base.set_blend_color = d3d12_set_blend_color;

   ctx->base.clear = d3d12_clear;
   ctx->base.draw_vbo = d3d12_draw_vbo;
   ctx->base.flush = d3d12_flush;

   d3d12_context_surface_init(&ctx->base);
   d3d12_context_resource_init(&ctx->base);

   slab_create_child(&ctx->transfer_pool, &d3d12_screen(pscreen)->transfer_pool);

   ctx->base.stream_uploader = u_upload_create_default(&ctx->base);
   ctx->base.const_uploader = ctx->base.stream_uploader;

   int prim_hwsupport = 1 << PIPE_PRIM_POINTS |
                        1 << PIPE_PRIM_LINES |
                        1 << PIPE_PRIM_LINE_STRIP |
                        1 << PIPE_PRIM_TRIANGLES |
                        1 << PIPE_PRIM_TRIANGLE_STRIP;

   ctx->primconvert = util_primconvert_create(&ctx->base, prim_hwsupport);
   if (!ctx->primconvert) {
      debug_printf("D3D12: failed to create primconvert\n");
      return NULL;
   }

   HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
   if (!hD3D12Mod) {
      debug_printf("D3D12: failed to load D3D12.DLL\n");
      return NULL;
   }
   ctx->D3D12SerializeRootSignature = (PFN_D3D12_SERIALIZE_ROOT_SIGNATURE)GetProcAddress(hD3D12Mod, "D3D12SerializeRootSignature");

   ctx->event = CreateEvent(NULL, FALSE, FALSE, NULL);
   if (FAILED(screen->dev->CreateFence(0, D3D12_FENCE_FLAG_NONE,
                                       __uuidof(ctx->cmdqueue_fence),
                                       (void **)&ctx->cmdqueue_fence))) {
      FREE(ctx);
      return NULL;
   }

   if (FAILED(screen->dev->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                  __uuidof(ctx->cmdalloc),
                                                  (void **)&ctx->cmdalloc))) {
      FREE(ctx);
      return NULL;
   }

   if (FAILED(screen->dev->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
                                             ctx->cmdalloc, NULL,
                                             __uuidof(ctx->cmdlist),
                                             (void **)&ctx->cmdlist))) {
      FREE(ctx);
      return NULL;
   }

   D3D12_DESCRIPTOR_HEAP_DESC heap_desc = {};

   ctx->rtv_heap = d3d12_descriptor_heap_new(screen->dev,
                                             D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
                                             D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
                                             100000);
   if (!ctx->rtv_heap) {
      FREE(ctx);
      return NULL;
   }

   ctx->dsv_heap = d3d12_descriptor_heap_new(screen->dev,
                                             D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
                                             D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
                                             10);
   if (!ctx->dsv_heap) {
      FREE(ctx);
      return NULL;
   }

   ctx->validation_tools = d3d12_validator_create();

   return &ctx->base;
}
