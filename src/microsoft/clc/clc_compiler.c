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
#include "clc_helpers.h"
#include "../compiler/nir_to_dxil.h"

#include "util/u_debug.h"
#include <util/u_math.h>
#include "spirv/nir_spirv.h"

struct clc_object *
clc_compile(const struct clc_compile_args *args,
            const struct clc_logger *logger)
{
   struct clc_object *obj;
   char *err_log;
   int ret;

   obj = malloc(sizeof(*obj));
   if (!obj) {
      fprintf(stderr, "D3D12: failed to allocate a clc_object");
      return NULL;
   }

   /* TODO: callbacks ... */
   ret = clc_to_spirv(args, &obj->spvbin, &err_log);
   if (ret < 0) {
      fprintf(stderr, "D3D12: clc_to_spirv failed: %s\n", err_log);
      free(err_log);
      free(obj);
      return NULL;
   }

   return obj;
}

struct clc_object *
clc_link(const struct clc_object **in_objs,
         unsigned num_in_objs,
         const struct clc_logger *logger)
{
   struct clc_object *out_obj;
   char *err_log;
   int ret;

   out_obj = malloc(sizeof(*out_obj));
   if (!out_obj) {
      fprintf(stderr, "D3D12: failed to allocate a clc_object");
      return NULL;
   }

   ret = clc_link_spirv_binaries((const struct spirv_binary **)in_objs,
                                 num_in_objs, &out_obj->spvbin, &err_log);
   if (ret < 0) {
      fprintf(stderr, "D3D12: clc_link_spirv_binaries failed: %s\n", err_log);
      free(err_log);
      free(out_obj);
      return NULL;
   }

   return out_obj;
}

void clc_free_object(struct clc_object *obj)
{
   clc_free_spirv_binary(&obj->spvbin);
   free(obj);
}

struct clc_dxil_object *
clc_to_dxil(const struct clc_object *obj,
            const char *entrypoint,
            const struct clc_logger *logger)
{
   struct clc_dxil_object *dxil;
   struct nir_shader *nir;
   char *err_log;
   int ret;

   dxil = calloc(1, sizeof(*dxil));
   if (!dxil) {
      fprintf(stderr, "D3D12: failed to allocate the dxil object\n");
      return NULL;
   }

   const struct spirv_to_nir_options spirv_options = {
      .environment = NIR_SPIRV_OPENCL,
      .constant_as_global = false,
      .caps = {
         .address = true,
         .float64 = true,
         .int8 = true,
         .int16 = true,
         .int64 = true,
         .kernel = true,
      },
   };
   const struct nir_shader_compiler_options *nir_options =
      dxil_get_nir_compiler_options();

   glsl_type_singleton_init_or_ref();

   nir = spirv_to_nir(obj->spvbin.data, obj->spvbin.size / 4,
                      NULL, 0,
                      MESA_SHADER_KERNEL, entrypoint,
                      &spirv_options,
                      nir_options,
                      false);
   if (!nir) {
      fprintf(stderr, "D3D12: spirv_to_nir failed\n");
      goto err_free_dxil;
   }
   nir->info.cs.local_size_variable = true;

   NIR_PASS_V(nir, nir_lower_goto_ifs);
   NIR_PASS_V(nir, nir_opt_dead_cf);

   // Calculate input offsets.
   unsigned offset = 0;
   nir_foreach_variable_safe(var, &nir->inputs) {
      offset = align(offset, glsl_get_cl_alignment(var->type));
      var->data.driver_location = offset;
      offset += glsl_get_cl_size(var->type);
   }

   // Inline all functions first.
   // according to the comment on nir_inline_functions
   NIR_PASS_V(nir, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS_V(nir, nir_lower_returns);
   NIR_PASS_V(nir, nir_inline_functions);
   NIR_PASS_V(nir, nir_opt_deref);

   // Pick off the single entrypoint that we want.
   foreach_list_typed_safe(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         exec_node_remove(&func->node);
   }
   assert(exec_list_length(&nir->functions) == 1);

   NIR_PASS_V(nir, nir_lower_variable_initializers, ~nir_var_function_temp);

   // copy propagate to prepare for lower_explicit_io
   NIR_PASS_V(nir, nir_split_var_copies);
   NIR_PASS_V(nir, nir_opt_copy_prop_vars);
   NIR_PASS_V(nir, nir_lower_var_copies);
   NIR_PASS_V(nir, nir_lower_vars_to_ssa);
   NIR_PASS_V(nir, nir_lower_alu);
   NIR_PASS_V(nir, nir_opt_dce);

   nir_variable_mode modes = nir_var_shader_in | nir_var_mem_global |
                             nir_var_mem_shared;
   nir_address_format format = nir->info.cs.ptr_size == 64 ?
      nir_address_format_64bit_global : nir_address_format_32bit_global;
   NIR_PASS_V(nir, nir_lower_explicit_io, modes, format);

   NIR_PASS_V(nir, nir_lower_system_values);
   if (nir_options->lower_int64_options)
      NIR_PASS_V(nir, nir_lower_int64, nir_options->lower_int64_options);

   NIR_PASS_V(nir, nir_opt_dce);

   nir_validate_shader(nir, "Validate before feeding NIR to the DXIL compiler");
   struct nir_to_dxil_options opts = {
      .interpolate_at_vertex = false
   };

   struct blob tmp;
   if (!nir_to_dxil(nir, &opts, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      goto err_free_dxil;
   }

   struct clc_dxil_metadata *metadata = &dxil->metadata;

   metadata->num_consts = 0;
   nir_foreach_variable(var, &nir->uniforms) {
      if (var->data.mode == nir_var_mem_ubo && var->constant_initializer) {
         if (glsl_type_is_array(var->type)) {
            int size = glsl_get_cl_size(var->type);
            uint8_t *data = malloc(size);
            if (!data)
               goto err_free_dxil;

            const struct glsl_type *elm_type = glsl_get_array_element(var->type);
            assert(glsl_type_is_scalar(elm_type)); // TODO: recursive iteration through types?
            assert(glsl_get_base_type(elm_type) == GLSL_TYPE_UINT); // TODO: more base-types
            int elm_size = glsl_get_cl_size(elm_type);
            for (unsigned i = 0; i < var->constant_initializer->num_elements; i++)
               memcpy(data + elm_size * i,
                      &var->constant_initializer->elements[i]->values[0].u32, elm_size);

            metadata->consts[metadata->num_consts].data = data;
            metadata->consts[metadata->num_consts].size = size;
            metadata->num_consts++;
         } else
            unreachable("unexpected constant initializer");
      }
   }

   metadata->num_uavs = util_bitcount64(nir->info.cs.global_inputs);

   ralloc_free(nir);
   glsl_type_singleton_decref();

   blob_finish_get_buffer(&tmp, &dxil->binary.data,
                          &dxil->binary.size);
   return dxil;

err_free_dxil:
   clc_free_dxil_object(dxil);
   return NULL;
}

void clc_free_dxil_object(struct clc_dxil_object *dxil)
{
   for (unsigned i = 0; i < dxil->metadata.num_consts; i++)
      free(dxil->metadata.consts[i].data);

   free(dxil->binary.data);
   free(dxil);
}
