/*
 * Copyright © 2015 Intel Corporation
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

#include "common/gen_l3_config.h"

#include "brw_context.h"
#include "brw_defines.h"
#include "brw_state.h"
#include "intel_batchbuffer.h"

/**
 * Calculate the desired L3 partitioning based on the current state of the
 * pipeline.  For now this simply returns the conservative defaults calculated
 * by get_default_l3_weights(), but we could probably do better by gathering
 * more statistics from the pipeline state (e.g. guess of expected URB usage
 * and bound surfaces), or by using feed-back from performance counters.
 */
static struct gen_l3_weights
get_pipeline_state_l3_weights(const struct brw_context *brw)
{
   const struct brw_stage_state *stage_states[] = {
      [MESA_SHADER_VERTEX] = &brw->vs.base,
      [MESA_SHADER_TESS_CTRL] = &brw->tcs.base,
      [MESA_SHADER_TESS_EVAL] = &brw->tes.base,
      [MESA_SHADER_GEOMETRY] = &brw->gs.base,
      [MESA_SHADER_FRAGMENT] = &brw->wm.base,
      [MESA_SHADER_COMPUTE] = &brw->cs.base
   };
   bool needs_dc = false, needs_slm = false;

   for (unsigned i = 0; i < ARRAY_SIZE(stage_states); i++) {
      const struct gl_program *prog =
         brw->ctx._Shader->CurrentProgram[stage_states[i]->stage];
      const struct brw_stage_prog_data *prog_data = stage_states[i]->prog_data;

      needs_dc |= (prog && (prog->sh.data->NumAtomicBuffers ||
                            prog->sh.data->NumShaderStorageBlocks ||
                            prog->info.num_images)) ||
         (prog_data && prog_data->total_scratch);
      needs_slm |= prog_data && prog_data->total_shared;
   }

   return gen_get_default_l3_weights(&brw->screen->devinfo,
                                     needs_dc, needs_slm);
}

/**
 * Program the hardware to use the specified L3 configuration.
 */
static void
setup_l3_config(struct brw_context *brw, const struct gen_l3_config *cfg)
{
   const struct gen_device_info *devinfo = &brw->screen->devinfo;
   const bool has_dc = cfg->n[GEN_L3P_DC] || cfg->n[GEN_L3P_ALL];
   const bool has_is = cfg->n[GEN_L3P_IS] || cfg->n[GEN_L3P_RO] ||
                       cfg->n[GEN_L3P_ALL];
   const bool has_c = cfg->n[GEN_L3P_C] || cfg->n[GEN_L3P_RO] ||
                      cfg->n[GEN_L3P_ALL];
   const bool has_t = cfg->n[GEN_L3P_T] || cfg->n[GEN_L3P_RO] ||
                      cfg->n[GEN_L3P_ALL];
   const bool has_slm = cfg->n[GEN_L3P_SLM];

   /* According to the hardware docs, the L3 partitioning can only be changed
    * while the pipeline is completely drained and the caches are flushed,
    * which involves a first PIPE_CONTROL flush which stalls the pipeline...
    */
   brw_emit_pipe_control_flush(brw,
                               PIPE_CONTROL_DATA_CACHE_FLUSH |
                               PIPE_CONTROL_NO_WRITE |
                               PIPE_CONTROL_CS_STALL);

   /* ...followed by a second pipelined PIPE_CONTROL that initiates
    * invalidation of the relevant caches.  Note that because RO invalidation
    * happens at the top of the pipeline (i.e. right away as the PIPE_CONTROL
    * command is processed by the CS) we cannot combine it with the previous
    * stalling flush as the hardware documentation suggests, because that
    * would cause the CS to stall on previous rendering *after* RO
    * invalidation and wouldn't prevent the RO caches from being polluted by
    * concurrent rendering before the stall completes.  This intentionally
    * doesn't implement the SKL+ hardware workaround suggesting to enable CS
    * stall on PIPE_CONTROLs with the texture cache invalidation bit set for
    * GPGPU workloads because the previous and subsequent PIPE_CONTROLs
    * already guarantee that there is no concurrent GPGPU kernel execution
    * (see SKL HSD 2132585).
    */
   brw_emit_pipe_control_flush(brw,
                               PIPE_CONTROL_TEXTURE_CACHE_INVALIDATE |
                               PIPE_CONTROL_CONST_CACHE_INVALIDATE |
                               PIPE_CONTROL_INSTRUCTION_INVALIDATE |
                               PIPE_CONTROL_STATE_CACHE_INVALIDATE |
                               PIPE_CONTROL_NO_WRITE);

   /* Now send a third stalling flush to make sure that invalidation is
    * complete when the L3 configuration registers are modified.
    */
   brw_emit_pipe_control_flush(brw,
                               PIPE_CONTROL_DATA_CACHE_FLUSH |
                               PIPE_CONTROL_NO_WRITE |
                               PIPE_CONTROL_CS_STALL);

   assert(!cfg->n[GEN_L3P_IS] && !cfg->n[GEN_L3P_C] && !cfg->n[GEN_L3P_T]);

   const unsigned imm_data = ((has_slm ? GEN8_L3CNTLREG_SLM_ENABLE : 0) |
      SET_FIELD(cfg->n[GEN_L3P_URB], GEN8_L3CNTLREG_URB_ALLOC) |
      SET_FIELD(cfg->n[GEN_L3P_RO], GEN8_L3CNTLREG_RO_ALLOC) |
      SET_FIELD(cfg->n[GEN_L3P_DC], GEN8_L3CNTLREG_DC_ALLOC) |
      SET_FIELD(cfg->n[GEN_L3P_ALL], GEN8_L3CNTLREG_ALL_ALLOC));

   /* Set up the L3 partitioning. */
   brw_load_register_imm32(brw, GEN8_L3CNTLREG, imm_data);
}

/**
 * Update the URB size in the context state for the specified L3
 * configuration.
 */
static void
update_urb_size(struct brw_context *brw, const struct gen_l3_config *cfg)
{
   const struct gen_device_info *devinfo = &brw->screen->devinfo;
   const unsigned sz = gen_get_l3_config_urb_size(devinfo, cfg);

   if (brw->urb.size != sz) {
      brw->urb.size = sz;
      brw->ctx.NewDriverState |= BRW_NEW_URB_SIZE;
   }
}

static void
emit_l3_state(struct brw_context *brw)
{
   const struct gen_l3_weights w = get_pipeline_state_l3_weights(brw);
   const float dw = gen_diff_l3_weights(w, gen_get_l3_config_weights(brw->l3.config));
   /* The distance between any two compatible weight vectors cannot exceed two
    * due to the triangle inequality.
    */
   const float large_dw_threshold = 2.0;
   /* Somewhat arbitrary, simply makes sure that there will be no repeated
    * transitions to the same L3 configuration, could probably do better here.
    */
   const float small_dw_threshold = 0.5;
   /* If we're emitting a new batch the caches should already be clean and the
    * transition should be relatively cheap, so it shouldn't hurt much to use
    * the smaller threshold.  Otherwise use the larger threshold so that we
    * only reprogram the L3 mid-batch if the most recently programmed
    * configuration is incompatible with the current pipeline state.
    */
   const float dw_threshold = (brw->ctx.NewDriverState & BRW_NEW_BATCH ?
                               small_dw_threshold : large_dw_threshold);

   if (dw > dw_threshold) {
      const struct gen_l3_config *const cfg =
         gen_get_l3_config(&brw->screen->devinfo, w);

      setup_l3_config(brw, cfg);
      update_urb_size(brw, cfg);
      brw->l3.config = cfg;

      if (unlikely(INTEL_DEBUG & DEBUG_L3)) {
         fprintf(stderr, "L3 config transition (%f > %f): ", dw, dw_threshold);
         gen_dump_l3_config(cfg, stderr);
      }
   }
}

const struct brw_tracked_state gen7_l3_state = {
   .dirty = {
      .mesa = 0,
      .brw = BRW_NEW_BATCH |
             BRW_NEW_BLORP |
             BRW_NEW_CS_PROG_DATA |
             BRW_NEW_FS_PROG_DATA |
             BRW_NEW_GS_PROG_DATA |
             BRW_NEW_TCS_PROG_DATA |
             BRW_NEW_TES_PROG_DATA |
             BRW_NEW_VS_PROG_DATA,
   },
   .emit = emit_l3_state
};
