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

static struct blorp_address
blorp_general_state_address(struct blorp_batch *batch, uint32_t offset);

static struct blorp_address
blorp_emit_vs_state(struct blorp_batch *batch,
                    const struct blorp_params *params)
{
   uint32_t offset;
   blorp_emit_dynamic(batch, GENX(VS_STATE), vs,
                      AUB_TRACE_VS_STATE, 64, &offset) {
      vs.URBEntryAllocationSize = 16;
      vs.NumberofURBEntries =
         (params->wm_prog_data->num_varying_inputs + 3) / 4 - 1;
      vs.VSFunctionEnable = false;
   }

   return blorp_general_state_address(batch, offset);
}

static struct blorp_address
blorp_emit_sf_state(struct blorp_batch *batch,
                    const struct blorp_params *params)
{
   uint32_t offset;
   blorp_emit_dynamic(batch, GENX(SF_STATE), sf,
                      AUB_TRACE_SF_STATE, 64, &offset) {
   }

   return blorp_general_state_address(batch, offset);
}

static struct blorp_address
blorp_emit_sampler_state(struct blorp_batch *batch,
                         const struct blorp_params *params)
{
   uint32_t offset;
   blorp_emit_dynamic(batch, GENX(SAMPLER_STATE), sampler,
                      AUB_TRACE_SAMPLER_STATE, 32, &offset) {
      sampler.MipModeFilter = MIPFILTER_NONE;
      sampler.MagModeFilter = MAPFILTER_LINEAR;
      sampler.MinModeFilter = MAPFILTER_LINEAR;
      sampler.MinLOD = 0;
      sampler.MaxLOD = 0;
      sampler.TCXAddressControlMode = TCM_CLAMP;
      sampler.TCYAddressControlMode = TCM_CLAMP;
      sampler.TCZAddressControlMode = TCM_CLAMP;
      sampler.MaximumAnisotropy = RATIO21;
      sampler.RAddressMinFilterRoundingEnable = true;
      sampler.RAddressMagFilterRoundingEnable = true;
      sampler.VAddressMinFilterRoundingEnable = true;
      sampler.VAddressMagFilterRoundingEnable = true;
      sampler.UAddressMinFilterRoundingEnable = true;
      sampler.UAddressMagFilterRoundingEnable = true;
   }

   return blorp_general_state_address(batch, offset);
}

static struct blorp_address
blorp_emit_wm_state(struct blorp_batch *batch,
                    const struct blorp_params *params)
{
   uint32_t offset;
   blorp_emit_dynamic(batch, GENX(WM_STATE), wm,
                      AUB_TRACE_WM_STATE, 64, &offset) {
      wm.KernelStartPointer0 =
         blorp_general_state_address(batch, params->wm_prog_kernel);
      wm.SamplerStatePointer = blorp_emit_sampler_state(batch, params);
   }

   return blorp_general_state_address(batch, offset);
}

static struct blorp_address
blorp_emit_cc_viewport(struct blorp_batch *batch,
                       const struct blorp_params *params)
{
   uint32_t offset;
   blorp_emit_dynamic(batch, GENX(CC_VIEWPORT), vp,
                      AUB_TRACE_CC_VP_STATE, 32, &offset) {
      vp.MinimumDepth = 0.0;
      vp.MaximumDepth = 1.0;
   }

   return blorp_general_state_address(batch, offset);
}

static struct blorp_address
blorp_emit_color_calc_state(struct blorp_batch *batch,
                            const struct blorp_params *params)
{
   uint32_t offset;
   blorp_emit_dynamic(batch, GENX(COLOR_CALC_STATE), cc,
                      AUB_TRACE_CC_STATE, 64, &offset) {
      cc.ColorCalculatorViewportStatePointer =
         blorp_emit_cc_viewport(batch, params);
   }

   return blorp_general_state_address(batch, offset);
}

/**
 * \brief Execute a blit or render pass operation.
 *
 * To execute the operation, this function manually constructs and emits a
 * batch to draw a rectangle primitive. The batchbuffer is flushed before
 * constructing and after emitting the batch.
 *
 * This function alters no GL state.
 */
static void
blorp_exec(struct blorp_batch *batch, const struct blorp_params *params)
{
   blorp_emit_vertex_buffers(batch, params);
   blorp_emit_vertex_elements(batch, params);

   blorp_emit(batch, GENX(3DSTATE_PIPELINED_POINTERS), pp) {
      pp.PointertoVS_STATE = blorp_emit_vs_state(batch, params);
      pp.GSEnable = false;
      pp.CLIPEnable = false;
      pp.PointertoSF_STATE = blorp_emit_sf_state(batch, params);
      pp.PointertoWM_STATE = blorp_emit_wm_state(batch, params);
      pp.PointertoCOLOR_CALC_STATE = blorp_emit_color_calc_state(batch, params);
   }

   if (params->wm_prog_data)
      blorp_emit_surface_states(batch, params);

   blorp_emit(batch, GENX(3DSTATE_DEPTH_BUFFER), db) {
      db.SurfaceType = SURFTYPE_NULL;
      db.SurfaceFormat = D32_FLOAT;
   }

   blorp_emit(batch, GENX(3DSTATE_DRAWING_RECTANGLE), rect) {
      rect.ClippedDrawingRectangleXMax = MAX2(params->x1, params->x0) - 1;
      rect.ClippedDrawingRectangleYMax = MAX2(params->y1, params->y0) - 1;
   }

   assert(params->num_layers == 1);
   blorp_emit(batch, GENX(3DPRIMITIVE), prim) {
      prim.VertexAccessType = SEQUENTIAL;
      prim.PrimitiveTopologyType = _3DPRIM_RECTLIST;
      prim.VertexCount = 3;
   }
}
