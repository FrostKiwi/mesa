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

#include "clc_compiler.h"
#include "../compiler/nir_to_dxil.h"

#include "util/u_debug.h"
#include "nir/nir_builder.h"

int clc_compile_from_source(
   const char *source,
   const char *source_name,
   const struct clc_define defines[], // should be sorted by name
   size_t num_defines,
   const struct clc_header headers[], // should be sorted by name
   size_t num_headers,
   clc_msg_callback warning_callback,
   clc_msg_callback error_callback,
   struct clc_metadata *metadata,
   void **blob,
   size_t *blob_size)
{
   nir_builder b;
   nir_builder_init_simple_shader(&b, NULL, MESA_SHADER_KERNEL, NULL);
   b.shader->info.name = ralloc_strdup(b.shader, "dummy_kernel");

   nir_variable *output_buffer = nir_variable_create(b.shader,
                                                     nir_var_mem_ssbo,
                                                     glsl_uint_type(),
                                                     "OutputBuffer");

   nir_intrinsic_instr *local_invocation_id =
      nir_intrinsic_instr_create(b.shader,
                                 nir_intrinsic_load_local_invocation_id);
   nir_ssa_dest_init(&local_invocation_id->instr,
                     &local_invocation_id->dest,
                     3,
                     32,
                     "local_invocation_id");
   nir_builder_instr_insert(&b, &local_invocation_id->instr);

   nir_ssa_def *index = nir_channel(&b, &local_invocation_id->dest.ssa, 0);
   nir_ssa_def *value = nir_vec4(&b, index, index, index, index);

   nir_intrinsic_instr *store_ssbo =
      nir_intrinsic_instr_create(b.shader,
                                 nir_intrinsic_store_ssbo);
   store_ssbo->num_components = 4;
   nir_intrinsic_set_write_mask(store_ssbo, 0xf);

   store_ssbo->src[0] = nir_src_for_ssa(value);
   store_ssbo->src[1] = nir_src_for_ssa(nir_imm_int(&b, 0));
   store_ssbo->src[2] = nir_src_for_ssa(index);
   nir_builder_instr_insert(&b, &store_ssbo->instr);

   NIR_PASS_V(b.shader, nir_lower_alu_to_scalar, NULL, NULL);

   struct blob tmp;
   if (!nir_to_dxil(b.shader, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return -1;
   }

   blob_finish_get_buffer(&tmp, blob, blob_size);
   return 0;
}
