/*
 * Copyright © 2017 Intel Corporation
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
#include <stdio.h>
#include <errno.h>
#include "pipe/p_defines.h"
#include "pipe/p_state.h"
#include "pipe/p_context.h"
#include "pipe/p_screen.h"
#include "util/u_atomic.h"
#include "compiler/nir/nir.h"
#include "compiler/nir/nir_builder.h"
#include "intel/compiler/brw_compiler.h"
#include "intel/compiler/brw_nir.h"
#include "iris_context.h"

static unsigned
get_new_program_id(struct iris_screen *screen)
{
   return p_atomic_inc_return(&screen->program_id);
}

struct iris_uncompiled_shader {
   struct pipe_shader_state base;
   unsigned program_id;
};

// XXX: need unify_interfaces() at link time...

static void *
iris_create_shader_state(struct pipe_context *ctx,
                         const struct pipe_shader_state *state)
{
   //struct iris_context *ice = (struct iris_context *)ctx;
   struct iris_screen *screen = (struct iris_screen *)ctx->screen;

   assert(state->type == PIPE_SHADER_IR_NIR);

   nir_shader *nir = state->ir.nir;

   struct iris_uncompiled_shader *ish =
      calloc(1, sizeof(struct iris_uncompiled_shader));
   if (!ish)
      return NULL;

   nir = brw_preprocess_nir(screen->compiler, nir);
   //NIR_PASS_V(nir, brw_nir_lower_uniforms, true);

   ish->program_id = get_new_program_id(screen);
   ish->base.type = PIPE_SHADER_IR_NIR;
   ish->base.ir.nir = nir;

   return ish;
}

static void
iris_delete_shader_state(struct pipe_context *ctx, void *hwcso)
{
   struct iris_uncompiled_shader *ish = hwcso;

   ralloc_free(ish->base.ir.nir);
   free(ish);
}

static void
iris_codegen_vs(struct iris_context *ice)
{
   const struct brw_compiler *compiler = brw->screen->compiler;
   const struct gen_device_info *devinfo = &brw->screen->devinfo;
   const GLuint *program;
   struct brw_vs_prog_data prog_data;
   struct brw_stage_prog_data *stage_prog_data = &prog_data.base.base;
   void *mem_ctx;
   bool start_busy = false;
   double start_time = 0;

   memset(&prog_data, 0, sizeof(prog_data));

   /* Use ALT floating point mode for ARB programs so that 0^0 == 1. */
   if (vp->program.is_arb_asm)
      stage_prog_data->use_alt_mode = true;

   mem_ctx = ralloc_context(NULL);

   brw_assign_common_binding_table_offsets(devinfo, &vp->program,
                                           &prog_data.base.base, 0);

   if (!vp->program.is_arb_asm) {
      brw_nir_setup_glsl_uniforms(mem_ctx, vp->program.nir, &vp->program,
                                  &prog_data.base.base,
                                  compiler->scalar_stage[MESA_SHADER_VERTEX]);
      brw_nir_analyze_ubo_ranges(compiler, vp->program.nir,
                                 prog_data.base.base.ubo_ranges);
   } else {
      brw_nir_setup_arb_uniforms(mem_ctx, vp->program.nir, &vp->program,
                                 &prog_data.base.base);
   }

   uint64_t outputs_written =
      brw_vs_outputs_written(brw, key, vp->program.nir->info.outputs_written);

   brw_compute_vue_map(devinfo,
                       &prog_data.base.vue_map, outputs_written,
                       vp->program.nir->info.separate_shader);

   if (0) {
      _mesa_fprint_program_opt(stderr, &vp->program, PROG_PRINT_DEBUG, true);
   }

   if (unlikely(brw->perf_debug)) {
      start_busy = (brw->batch.last_bo &&
                    brw_bo_busy(brw->batch.last_bo));
      start_time = get_time();
   }

   if (unlikely(INTEL_DEBUG & DEBUG_VS)) {
      if (vp->program.is_arb_asm)
         brw_dump_arb_asm("vertex", &vp->program);
   }

   int st_index = -1;
   if (INTEL_DEBUG & DEBUG_SHADER_TIME) {
      st_index = brw_get_shader_time_index(brw, &vp->program, ST_VS,
                                           !vp->program.is_arb_asm);
   }

   /* Emit GEN4 code.
    */
   char *error_str;
   program = brw_compile_vs(compiler, brw, mem_ctx, key, &prog_data,
                            vp->program.nir,
                            st_index, &error_str);
   if (program == NULL) {
      if (!vp->program.is_arb_asm) {
         vp->program.sh.data->LinkStatus = linking_failure;
         ralloc_strcat(&vp->program.sh.data->InfoLog, error_str);
      }

      _mesa_problem(NULL, "Failed to compile vertex shader: %s\n", error_str);

      ralloc_free(mem_ctx);
      return false;
   }

   if (unlikely(brw->perf_debug)) {
      if (vp->compiled_once) {
         brw_vs_debug_recompile(brw, &vp->program, key);
      }
      if (start_busy && !brw_bo_busy(brw->batch.last_bo)) {
         perf_debug("VS compile took %.03f ms and stalled the GPU\n",
                    (get_time() - start_time) * 1000);
      }
      vp->compiled_once = true;
   }

   /* Scratch space is used for register spilling */
   brw_alloc_stage_scratch(brw, &brw->vs.base,
                           prog_data.base.base.total_scratch);

   /* The param and pull_param arrays will be freed by the shader cache. */
   ralloc_steal(NULL, prog_data.base.base.param);
   ralloc_steal(NULL, prog_data.base.base.pull_param);
   brw_upload_cache(&brw->cache, BRW_CACHE_VS_PROG,
                    key, sizeof(struct brw_vs_prog_key),
                    program, prog_data.base.base.program_size,
                    &prog_data, sizeof(prog_data),
                    &brw->vs.base.prog_offset, &brw->vs.base.prog_data);
   ralloc_free(mem_ctx);

   return true;
}

void
iris_upload_vs_prog(struct iris_context *iris)
{
   struct brw_vs_prog_key key;
   /* BRW_NEW_VERTEX_PROGRAM */
   struct brw_program *vp =
      (struct brw_program *) brw->programs[MESA_SHADER_VERTEX];

   if (!brw_vs_state_dirty(brw))
      return;

   brw_vs_populate_key(brw, &key);

   if (brw_search_cache(&brw->cache, BRW_CACHE_VS_PROG,
                        &key, sizeof(key),
                        &brw->vs.base.prog_offset, &brw->vs.base.prog_data))
      return;

   vp = (struct brw_program *) brw->programs[MESA_SHADER_VERTEX];
   vp->id = key.program_string_id;

   MAYBE_UNUSED bool success = brw_codegen_vs_prog(brw, vp, &key);
   assert(success);
}

static void
iris_update_compiled_shaders(struct iris_context *ice)
{
   iris_update_compiled_vs(ice);
   // ...
}

void
iris_init_program_functions(struct pipe_context *ctx)
{
   ctx->create_vs_state = iris_create_shader_state;
   ctx->create_tcs_state = iris_create_shader_state;
   ctx->create_tes_state = iris_create_shader_state;
   ctx->create_gs_state = iris_create_shader_state;
   ctx->create_fs_state = iris_create_shader_state;

   ctx->delete_vs_state = iris_delete_shader_state;
   ctx->delete_tcs_state = iris_delete_shader_state;
   ctx->delete_tes_state = iris_delete_shader_state;
   ctx->delete_gs_state = iris_delete_shader_state;
   ctx->delete_fs_state = iris_delete_shader_state;
}
