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

#include "clc_compiler.h"
#include "clc_to_spirv.h"
#include "../compiler/nir_to_dxil.h"

#include "util/u_debug.h"
#include "spirv/nir_spirv.h"

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
   uint32_t *spv_src;
   size_t spv_size;
   char *err_log;
   struct nir_shader *nir;
   int ret;


   const struct spirv_to_nir_options spirv_options = {
      .environment = NIR_SPIRV_OPENCL,
      .constant_as_global = true,
      .caps = {
         .address = true,
         .float64 = true,
         .int8 = true,
         .int16 = true,
         .int64 = true,
         .kernel = true,
      },
   };

   ret = clc_to_spirv(source, source_name,
                      defines, num_defines,
                      headers, num_headers,
                     /* TODO: callbacks ... */
                     &spv_src, &spv_size,
                     &err_log);

   if (ret < 0) {
      fprintf(stderr, "D3D12: clc_to_spirv failed: %s\n", err_log);
      free(err_log);
      return -1;
   }

   glsl_type_singleton_init_or_ref();

   nir = spirv_to_nir(spv_src, spv_size / 4,
                      NULL, 0,
                      MESA_SHADER_KERNEL, "main_test",
                      &spirv_options,
                      dxil_get_nir_compiler_options(),
                      false);
   free(spv_src);
   if (!nir) {
      fprintf(stderr, "D3D12: spirv_to_nir failed\n");
      return -1;
   }

   NIR_PASS_V(nir, nir_lower_goto_ifs);
   NIR_PASS_V(nir, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS_V(nir, nir_lower_returns);
   NIR_PASS_V(nir, nir_inline_functions);
   NIR_PASS_V(nir, nir_opt_deref);
   foreach_list_typed_safe(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         exec_node_remove(&func->node);
   }
   assert(exec_list_length(&nir->functions) == 1);
   NIR_PASS_V(nir, nir_lower_variable_initializers, ~nir_var_function_temp);

   nir_variable_mode modes = nir_var_shader_in | nir_var_mem_global |
                             nir_var_mem_shared;
   nir_address_format format = nir->info.cs.ptr_size == 64 ?
      nir_address_format_64bit_global : nir_address_format_32bit_global;
   NIR_PASS_V(nir, nir_lower_explicit_io, modes, format);

   NIR_PASS_V(nir, nir_lower_system_values);

   struct nir_to_dxil_options opts = {
      .interpolate_at_vertex = false
   };

   struct blob tmp;
   if (!nir_to_dxil(nir, &opts, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return -1;
   }

   ralloc_free(nir);
   glsl_type_singleton_decref();

   blob_finish_get_buffer(&tmp, blob, blob_size);
   return 0;
}

void clc_free_blob(void *blob)
{
   free(blob);
}
