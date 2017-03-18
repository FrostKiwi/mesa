/*
 * Copyright © 2016 Intel Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef BLORP_GENX_EXEC_H
#define BLORP_GENX_EXEC_H

#include "blorp_priv.h"
#include "common/gen_device_info.h"
#include "common/gen_sample_positions.h"
#include "intel_aub.h"

/**
 * This file provides the blorp pipeline setup and execution functionality.
 * It defines the following function:
 *
 * static void
 * blorp_exec(struct blorp_context *blorp, void *batch_data,
 *            const struct blorp_params *params);
 *
 * It is the job of whoever includes this header to wrap this in something
 * to get an externally visible symbol.
 *
 * In order for the blorp_exec function to work, the driver must provide
 * implementations of the static helper functions defined below.
 */

static void *
blorp_emit_dwords(struct blorp_batch *batch, unsigned n);

static uint64_t
blorp_emit_reloc(struct blorp_batch *batch,
                 void *location, struct blorp_address address, uint32_t delta);

static void *
blorp_alloc_dynamic_state(struct blorp_batch *batch,
                          enum aub_state_struct_type type,
                          uint32_t size,
                          uint32_t alignment,
                          uint32_t *offset);
static void *
blorp_alloc_vertex_buffer(struct blorp_batch *batch, uint32_t size,
                          struct blorp_address *addr);

static void
blorp_alloc_binding_table(struct blorp_batch *batch, unsigned num_entries,
                          unsigned state_size, unsigned state_alignment,
                          uint32_t *bt_offset, uint32_t *surface_offsets,
                          void **surface_maps);

static void
blorp_flush_range(struct blorp_batch *batch, void *start, size_t size);

static void
blorp_surface_reloc(struct blorp_batch *batch, uint32_t ss_offset,
                    struct blorp_address address, uint32_t delta);

static void
blorp_emit_urb_config(struct blorp_batch *batch, unsigned vs_entry_size);

/*****  BEGIN blorp_exec implementation  ******/

#include "genxml/gen_macros.h"

static uint64_t
_blorp_combine_address(struct blorp_batch *batch, void *location,
                       struct blorp_address address, uint32_t delta)
{
   if (address.buffer == NULL) {
      return address.offset + delta;
   } else {
      return blorp_emit_reloc(batch, location, address, delta);
   }
}

#define __gen_address_type struct blorp_address
#define __gen_user_data struct blorp_batch
#define __gen_combine_address _blorp_combine_address

#include "genxml/genX_pack.h"

#define _blorp_cmd_length(cmd) cmd ## _length
#define _blorp_cmd_length_bias(cmd) cmd ## _length_bias
#define _blorp_cmd_header(cmd) cmd ## _header
#define _blorp_cmd_pack(cmd) cmd ## _pack

#define blorp_emit(batch, cmd, name)                              \
   for (struct cmd name = { _blorp_cmd_header(cmd) },             \
        *_dst = blorp_emit_dwords(batch, _blorp_cmd_length(cmd)); \
        __builtin_expect(_dst != NULL, 1);                        \
        _blorp_cmd_pack(cmd)(batch, (void *)_dst, &name),         \
        _dst = NULL)

#define blorp_emitn(batch, cmd, n) ({                       \
      uint32_t *_dw = blorp_emit_dwords(batch, n);          \
      if (_dw) {                                            \
         struct cmd template = {                            \
            _blorp_cmd_header(cmd),                         \
            .DWordLength = n - _blorp_cmd_length_bias(cmd), \
         };                                                 \
         _blorp_cmd_pack(cmd)(batch, _dw, &template);       \
      }                                                     \
      _dw ? _dw + 1 : NULL; /* Array starts at dw[1] */     \
   })

#define STRUCT_ZERO(S) ({ struct S t; memset(&t, 0, sizeof(t)); t; })

#define blorp_emit_dynamic(batch, state, name, aub, align, offset)      \
   for (struct state name = STRUCT_ZERO(state),                         \
        *_dst = blorp_alloc_dynamic_state(batch, aub,                   \
                                          _blorp_cmd_length(state) * 4, \
                                          align, offset);               \
        __builtin_expect(_dst != NULL, 1);                              \
        _blorp_cmd_pack(state)(batch, (void *)_dst, &name),             \
        blorp_flush_range(batch, _dst, _blorp_cmd_length(state) * 4),   \
        _dst = NULL)

static void
blorp_emit_vertex_data(struct blorp_batch *batch,
                       const struct blorp_params *params,
                       struct blorp_address *addr,
                       uint32_t *size)
{
   const float vertices[] = {
      /* v0 */ (float)params->x1, (float)params->y1, params->z,
      /* v1 */ (float)params->x0, (float)params->y1, params->z,
      /* v2 */ (float)params->x0, (float)params->y0, params->z,
   };

   void *data = blorp_alloc_vertex_buffer(batch, sizeof(vertices), addr);
   memcpy(data, vertices, sizeof(vertices));
   *size = sizeof(vertices);
   blorp_flush_range(batch, data, *size);
}

static void
blorp_emit_input_varying_data(struct blorp_batch *batch,
                              const struct blorp_params *params,
                              struct blorp_address *addr,
                              uint32_t *size)
{
   const unsigned vec4_size_in_bytes = 4 * sizeof(float);
   const unsigned max_num_varyings =
      DIV_ROUND_UP(sizeof(params->wm_inputs), vec4_size_in_bytes);
   const unsigned num_varyings =
      params->wm_prog_data ? params->wm_prog_data->num_varying_inputs : 0;

   *size = 16 + num_varyings * vec4_size_in_bytes;

   const uint32_t *const inputs_src = (const uint32_t *)&params->wm_inputs;
   void *data = blorp_alloc_vertex_buffer(batch, *size, addr);
   uint32_t *inputs = data;

   /* Copy in the VS inputs */
   assert(sizeof(params->vs_inputs) == 16);
   memcpy(inputs, &params->vs_inputs, sizeof(params->vs_inputs));
   inputs += 4;

   if (params->wm_prog_data) {
      /* Walk over the attribute slots, determine if the attribute is used by
       * the program and when necessary copy the values from the input storage
       * to the vertex data buffer.
       */
      for (unsigned i = 0; i < max_num_varyings; i++) {
         const gl_varying_slot attr = VARYING_SLOT_VAR0 + i;

         const int input_index = params->wm_prog_data->urb_setup[attr];
         if (input_index < 0)
            continue;

         memcpy(inputs, inputs_src + i * 4, vec4_size_in_bytes);

         inputs += 4;
      }
   }

   blorp_flush_range(batch, data, *size);
}

static void
blorp_emit_vertex_buffers(struct blorp_batch *batch,
                          const struct blorp_params *params)
{
   struct GENX(VERTEX_BUFFER_STATE) vb[2];
   memset(vb, 0, sizeof(vb));

   uint32_t size;
   blorp_emit_vertex_data(batch, params, &vb[0].BufferStartingAddress, &size);
   vb[0].VertexBufferIndex = 0;
   vb[0].BufferPitch = 3 * sizeof(float);
#if GEN_GEN >= 5
   vb[0].VertexBufferMOCS = batch->blorp->mocs.vb;
#endif
#if GEN_GEN >= 7
   vb[0].AddressModifyEnable = true;
#endif
#if GEN_GEN >= 8
   vb[0].BufferSize = size;
#elif GEN_GEN >= 5
   vb[0].BufferAccessType = VERTEXDATA;
   vb[0].EndAddress = vb[0].BufferStartingAddress;
   vb[0].EndAddress.offset += size - 1;
#endif

   blorp_emit_input_varying_data(batch, params,
                                 &vb[1].BufferStartingAddress, &size);
   vb[1].VertexBufferIndex = 1;
   vb[1].BufferPitch = 0;
#if GEN_GEN >= 5
   vb[1].VertexBufferMOCS = batch->blorp->mocs.vb;
#endif
#if GEN_GEN >= 7
   vb[1].AddressModifyEnable = true;
#endif
#if GEN_GEN >= 8
   vb[1].BufferSize = size;
#elif GEN_GEN >= 5
   vb[1].BufferAccessType = INSTANCEDATA;
   vb[1].EndAddress = vb[1].BufferStartingAddress;
   vb[1].EndAddress.offset += size - 1;
#endif

   const unsigned num_dwords = 1 + GENX(VERTEX_BUFFER_STATE_length) * 2;
   uint32_t *dw = blorp_emitn(batch, GENX(3DSTATE_VERTEX_BUFFERS), num_dwords);
   if (!dw)
      return;

   for (unsigned i = 0; i < 2; i++) {
      GENX(VERTEX_BUFFER_STATE_pack)(batch, dw, &vb[i]);
      dw += GENX(VERTEX_BUFFER_STATE_length);
   }
}

static void
blorp_emit_vertex_elements(struct blorp_batch *batch,
                           const struct blorp_params *params)
{
   const unsigned num_varyings =
      params->wm_prog_data ? params->wm_prog_data->num_varying_inputs : 0;
   const unsigned num_elements = 2 + num_varyings;

   struct GENX(VERTEX_ELEMENT_STATE) ve[num_elements];
   memset(ve, 0, num_elements * sizeof(*ve));

   /* Setup VBO for the rectangle primitive..
    *
    * A rectangle primitive (3DPRIM_RECTLIST) consists of only three
    * vertices. The vertices reside in screen space with DirectX
    * coordinates (that is, (0, 0) is the upper left corner).
    *
    *   v2 ------ implied
    *    |        |
    *    |        |
    *   v1 ----- v0
    *
    * Since the VS is disabled, the clipper loads each VUE directly from
    * the URB. This is controlled by the 3DSTATE_VERTEX_BUFFERS and
    * 3DSTATE_VERTEX_ELEMENTS packets below. The VUE contents are as follows:
    *   dw0: Reserved, MBZ.
    *   dw1: Render Target Array Index. Below vertex fetcher gets programmed
    *        to assign this with primitive instance identifier which will be
    *        used for layered clears. All other renders have only one instance
    *        and therefore the value will be effectively zero.
    *   dw2: Viewport Index. The HiZ op disables viewport mapping and
    *        scissoring, so set the dword to 0.
    *   dw3: Point Width: The HiZ op does not emit the POINTLIST primitive,
    *        so set the dword to 0.
    *   dw4: Vertex Position X.
    *   dw5: Vertex Position Y.
    *   dw6: Vertex Position Z.
    *   dw7: Vertex Position W.
    *
    *   dw8: Flat vertex input 0
    *   dw9: Flat vertex input 1
    *   ...
    *   dwn: Flat vertex input n - 8
    *
    * For details, see the Sandybridge PRM, Volume 2, Part 1, Section 1.5.1
    * "Vertex URB Entry (VUE) Formats".
    *
    * Only vertex position X and Y are going to be variable, Z is fixed to
    * zero and W to one. Header words dw0,2,3 are zero. There is no need to
    * include the fixed values in the vertex buffer. Vertex fetcher can be
    * instructed to fill vertex elements with constant values of one and zero
    * instead of reading them from the buffer.
    * Flat inputs are program constants that are not interpolated. Moreover
    * their values will be the same between vertices.
    *
    * See the vertex element setup below.
    */
   ve[0].VertexBufferIndex = 1;
   ve[0].Valid = true;
   ve[0].SourceElementFormat = ISL_FORMAT_R32G32B32A32_FLOAT;
   ve[0].SourceElementOffset = 0;
   ve[0].Component0Control = VFCOMP_STORE_SRC;

   /* From Gen8 onwards hardware is no more instructed to overwrite components
    * using an element specifier. Instead one has separate 3DSTATE_VF_SGVS
    * (System Generated Value Setup) state packet for it.
    */
#if GEN_GEN >= 8
   ve[0].Component1Control = VFCOMP_STORE_0;
#elif GEN_GEN >= 5
   ve[0].Component1Control = VFCOMP_STORE_IID;
#else
   ve[0].Component1Control = VFCOMP_STORE_0;
#endif
   ve[0].Component2Control = VFCOMP_STORE_SRC;
   ve[0].Component3Control = VFCOMP_STORE_SRC;

   ve[1].VertexBufferIndex = 0;
   ve[1].Valid = true;
   ve[1].SourceElementFormat = ISL_FORMAT_R32G32B32_FLOAT;
   ve[1].SourceElementOffset = 0;
   ve[1].Component0Control = VFCOMP_STORE_SRC;
   ve[1].Component1Control = VFCOMP_STORE_SRC;
   ve[1].Component2Control = VFCOMP_STORE_SRC;
   ve[1].Component3Control = VFCOMP_STORE_1_FP;

   for (unsigned i = 0; i < num_varyings; ++i) {
      ve[i + 2].VertexBufferIndex = 1;
      ve[i + 2].Valid = true;
      ve[i + 2].SourceElementFormat = ISL_FORMAT_R32G32B32A32_FLOAT;
      ve[i + 2].SourceElementOffset = 16 + i * 4 * sizeof(float);
      ve[i + 2].Component0Control = VFCOMP_STORE_SRC;
      ve[i + 2].Component1Control = VFCOMP_STORE_SRC;
      ve[i + 2].Component2Control = VFCOMP_STORE_SRC;
      ve[i + 2].Component3Control = VFCOMP_STORE_SRC;
   }

   const unsigned num_dwords =
      1 + GENX(VERTEX_ELEMENT_STATE_length) * num_elements;
   uint32_t *dw = blorp_emitn(batch, GENX(3DSTATE_VERTEX_ELEMENTS), num_dwords);
   if (!dw)
      return;

   for (unsigned i = 0; i < num_elements; i++) {
      GENX(VERTEX_ELEMENT_STATE_pack)(batch, dw, &ve[i]);
      dw += GENX(VERTEX_ELEMENT_STATE_length);
   }

#if GEN_GEN >= 8
   /* Overwrite Render Target Array Index (2nd dword) in the VUE header with
    * primitive instance identifier. This is used for layered clears.
    */
   blorp_emit(batch, GENX(3DSTATE_VF_SGVS), sgvs) {
      sgvs.InstanceIDEnable = true;
      sgvs.InstanceIDComponentNumber = COMP_1;
      sgvs.InstanceIDElementOffset = 0;
   }

   for (unsigned i = 0; i < num_elements; i++) {
      blorp_emit(batch, GENX(3DSTATE_VF_INSTANCING), vf) {
         vf.VertexElementIndex = i;
         vf.InstancingEnable = false;
      }
   }

   blorp_emit(batch, GENX(3DSTATE_VF_TOPOLOGY), topo) {
      topo.PrimitiveTopologyType = _3DPRIM_RECTLIST;
   }
#endif
}

static void
blorp_emit_surface_state(struct blorp_batch *batch,
                         const struct brw_blorp_surface_info *surface,
                         void *state, uint32_t state_offset,
                         bool is_render_target)
{
   const struct isl_device *isl_dev = batch->blorp->isl_dev;
   struct isl_surf surf = surface->surf;

   if (surf.dim == ISL_SURF_DIM_1D &&
       surf.dim_layout == ISL_DIM_LAYOUT_GEN4_2D) {
      assert(surf.logical_level0_px.height == 1);
      surf.dim = ISL_SURF_DIM_2D;
   }

   /* Blorp doesn't support HiZ in any of the blit or slow-clear paths */
   enum isl_aux_usage aux_usage = surface->aux_usage;
   if (aux_usage == ISL_AUX_USAGE_HIZ)
      aux_usage = ISL_AUX_USAGE_NONE;

   const uint32_t mocs =
      is_render_target ? batch->blorp->mocs.rb : batch->blorp->mocs.tex;

   isl_surf_fill_state(batch->blorp->isl_dev, state,
                       .surf = &surf, .view = &surface->view,
                       .aux_surf = &surface->aux_surf, .aux_usage = aux_usage,
                       .mocs = mocs, .clear_color = surface->clear_color);

   blorp_surface_reloc(batch, state_offset + isl_dev->ss.addr_offset,
                       surface->addr, 0);

   if (aux_usage != ISL_AUX_USAGE_NONE) {
      /* On gen7 and prior, the bottom 12 bits of the MCS base address are
       * used to store other information.  This should be ok, however, because
       * surface buffer addresses are always 4K page alinged.
       */
      assert((surface->aux_addr.offset & 0xfff) == 0);
      uint32_t *aux_addr = state + isl_dev->ss.aux_addr_offset;
      blorp_surface_reloc(batch, state_offset + isl_dev->ss.aux_addr_offset,
                          surface->aux_addr, *aux_addr);
   }

   blorp_flush_range(batch, state, GENX(RENDER_SURFACE_STATE_length) * 4);
}

static void
blorp_emit_null_surface_state(struct blorp_batch *batch,
                              const struct brw_blorp_surface_info *surface,
                              uint32_t *state)
{
   struct GENX(RENDER_SURFACE_STATE) ss = {
      .SurfaceType = SURFTYPE_NULL,
      .SurfaceFormat = ISL_FORMAT_R8G8B8A8_UNORM,
      .Width = surface->surf.logical_level0_px.width - 1,
      .Height = surface->surf.logical_level0_px.height - 1,
      .MIPCountLOD = surface->view.base_level,
      .MinimumArrayElement = surface->view.base_array_layer,
      .Depth = surface->view.array_len - 1,
      .RenderTargetViewExtent = surface->view.array_len - 1,
#if GEN_GEN >= 5
      .NumberofMultisamples = ffs(surface->surf.samples) - 1,
#endif

#if GEN_GEN >= 7
      .SurfaceArray = surface->surf.dim != ISL_SURF_DIM_3D,
#endif

#if GEN_GEN >= 8
      .TileMode = YMAJOR,
#else
      .TiledSurface = true,
#endif
   };

   GENX(RENDER_SURFACE_STATE_pack)(NULL, state, &ss);

   blorp_flush_range(batch, state, GENX(RENDER_SURFACE_STATE_length) * 4);
}

static void
blorp_emit_surface_states(struct blorp_batch *batch,
                          const struct blorp_params *params)
{
   const struct isl_device *isl_dev = batch->blorp->isl_dev;
   uint32_t bind_offset, surface_offsets[2];
   void *surface_maps[2];

   if (params->use_pre_baked_binding_table) {
      bind_offset = params->pre_baked_binding_table_offset;
   } else {
      unsigned num_surfaces = 1 + params->src.enabled;
      blorp_alloc_binding_table(batch, num_surfaces,
                                isl_dev->ss.size, isl_dev->ss.align,
                                &bind_offset, surface_offsets, surface_maps);

      if (params->dst.enabled) {
         blorp_emit_surface_state(batch, &params->dst,
                                  surface_maps[BLORP_RENDERBUFFER_BT_INDEX],
                                  surface_offsets[BLORP_RENDERBUFFER_BT_INDEX],
                                  true);
      } else {
         assert(params->depth.enabled || params->stencil.enabled);
         const struct brw_blorp_surface_info *surface =
            params->depth.enabled ? &params->depth : &params->stencil;
         blorp_emit_null_surface_state(batch, surface,
                                       surface_maps[BLORP_RENDERBUFFER_BT_INDEX]);
      }

      if (params->src.enabled) {
         blorp_emit_surface_state(batch, &params->src,
                                  surface_maps[BLORP_TEXTURE_BT_INDEX],
                                  surface_offsets[BLORP_TEXTURE_BT_INDEX], false);
      }
   }

#if GEN_GEN >= 7
   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS_VS), bt);
   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS_HS), bt);
   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS_DS), bt);
   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS_GS), bt);

   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS_PS), bt) {
      bt.PointertoPSBindingTable = bind_offset;
   }
#elif GEN_GEN >= 5
   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS), bt) {
      bt.PSBindingTableChange = true;
      bt.PointertoPSBindingTable = bind_offset;
   }
#else
   blorp_emit(batch, GENX(3DSTATE_BINDING_TABLE_POINTERS), bt) {
      bt.PointertoPSBindingTable = bind_offset;
   }
#endif
}

#include "blorp_gen6_exec_priv.h"

#endif /* BLORP_GENX_EXEC_H */
