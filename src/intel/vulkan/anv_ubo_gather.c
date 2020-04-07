/*
 * Copyright © 2020 Intel Corporation
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

#include "anv_private.h"
#include "nir/nir_builder.h"
#include "compiler/brw_nir.h"
#include "compiler/brw_nir_ubo_gather.h"

void
anv_cmd_gather_state_finish(struct anv_cmd_buffer *cmd_buffer,
                            struct anv_cmd_gather_state *gather)
{
   util_dynarray_foreach(&gather->used_bos, struct anv_bo *, bo)
      anv_bo_pool_free(&cmd_buffer->device->batch_bo_pool, *bo);
   util_dynarray_fini(&gather->used_bos);
}

void
anv_cmd_gather_state_invalidate(struct anv_cmd_gather_state *gather)
{
   gather->bo = NULL;
   gather->count = 0;
   gather->dirty = ~0;
}

struct anv_shader_bin *
anv_get_gather_shader_bin(struct anv_device *device)
{
   const char key[] = "gather shader";
   struct anv_shader_bin *bin =
      anv_pipeline_cache_search(&device->default_pipeline_cache,
                                key, sizeof(key));
   if (bin)
      return bin;

   const struct brw_compiler *compiler = device->physical->compiler;
   void *mem_ctx = ralloc_context(NULL);

   nir_shader *nir = brw_nir_create_gather_vs(compiler, mem_ctx);
   brw_preprocess_nir(compiler, nir, NULL);
   nir_shader_gather_info(nir, nir_shader_get_entrypoint(nir));

   struct brw_vs_prog_data vs_prog_data;
   memset(&vs_prog_data, 0, sizeof(vs_prog_data));

   vs_prog_data.inputs_read = nir->info.inputs_read;

   brw_compute_vue_map(compiler->devinfo,
                       &vs_prog_data.base.vue_map,
                       nir->info.outputs_written,
                       nir->info.separate_shader,
                       1 /* pos_slots */);

   struct brw_vs_prog_key vs_key = { 0, };

   const unsigned *program =
      brw_compile_vs(compiler, device, mem_ctx, &vs_key,
                     &vs_prog_data, nir, -1, NULL, NULL);

   struct anv_pipeline_bind_map bind_map = {
      .surface_count = 0,
      .sampler_count = 0,
   };

   bin = anv_pipeline_cache_upload_kernel(&device->default_pipeline_cache,
                                          MESA_SHADER_VERTEX, key, sizeof(key),
                                          program,
                                          vs_prog_data.base.base.program_size,
                                          NULL, 0, /* Constant data */
                                          &vs_prog_data.base.base,
                                          sizeof(vs_prog_data),
                                          NULL, 0, NULL, &bind_map);

   ralloc_free(mem_ctx);

   /* The cache already has a reference and it's not going anywhere so there
    * is no need to hold a second reference.
    */
   anv_shader_bin_unref(device, bin);

   return bin;
}
