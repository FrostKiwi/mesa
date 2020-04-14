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
#include "nir_builder.h"

/*
 * DXIL doesn't support reading and writing global memory through pointers,
 * but needs a buffer-index and offset instead. This code lowers this to
 * DXIL specific intrinsics, so we can deal with these limitations as early
 * as possible.
 *
 * In principle, this is very similar to SSBOs, so at some point we might
 * want to unify that.
 */

static nir_ssa_def *
ptr_to_buffer(nir_builder *b, nir_ssa_def *ptr)
{
   return nir_ishr(b, ptr, nir_imm_int(b, 28));
}

static nir_ssa_def *
ptr_to_offset(nir_builder *b, nir_ssa_def *ptr)
{
   return nir_iand(b, ptr, nir_imm_int(b, 0x0ffffffc));
}

static void
lower_load_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->num_components == 1); // no support for vectors

   /* source 'pointer' */
   assert(intr->src[0].is_ssa);
   nir_ssa_def *ptr = intr->src[0].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   nir_ssa_def *result;
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   if (bit_size == 64) {
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_load_global_dxil);

      load->num_components = 2;
      assert(intr->dest.is_ssa);
      load->src[0] = nir_src_for_ssa(buffer);
      load->src[1] = nir_src_for_ssa(offset);

      nir_ssa_dest_init(&load->instr, &load->dest,
                        2, 32, intr->dest.ssa.name);
      nir_builder_instr_insert(b, &load->instr);

      result = &load->dest.ssa;
      nir_ssa_def *value_lo = nir_channel(b, result, 0);
      nir_ssa_def *value_hi = nir_channel(b, result, 1);
      result = nir_ior(b, nir_u2u64(b, value_lo),
                          nir_ishl(b, nir_u2u64(b, value_hi),
                                      nir_imm_int(b, 32)));
   } else {
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_load_global_dxil);
      load->num_components = 1;

      assert(intr->dest.is_ssa);
      load->src[0] = nir_src_for_ssa(buffer);
      load->src[1] = nir_src_for_ssa(offset);
      nir_ssa_dest_init(&load->instr, &load->dest, load->num_components,
                        32, intr->dest.ssa.name);
      nir_builder_instr_insert(b, &load->instr);

      result = &load->dest.ssa;
      switch (bit_size) {
      case 8:
         result = nir_extract_u8(b, result,
                                    nir_iand(b, ptr, nir_imm_int(b, 3)));
         result = nir_u2u8(b, result);
         break;

      case 16:
         result = nir_extract_u16(b, result,
                                     nir_iand(b, nir_ushr(b, ptr,
                                                             nir_imm_int(b, 1)),
                                                 nir_imm_int(b, 1)));
         result = nir_u2u16(b, result);
         break;

      case 32:
         /* nothing */
         break;

      default:
         unreachable("unexpected bit_size");
      }
   }

   nir_ssa_def_rewrite_uses(&intr->dest.ssa,
                            nir_src_for_ssa(result));
   nir_instr_remove(&intr->instr);
}

static void
lower_store_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->num_components == 1); // no support for vectors

   /* source 'pointer' */
   assert(intr->src[1].is_ssa);
   nir_ssa_def *ptr = intr->src[1].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   assert(intr->src[0].is_ssa);
   nir_ssa_def *value = intr->src[0].ssa;

   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   if (bit_size == 64) {
      nir_intrinsic_instr *store =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_store_global_dxil);
      store->num_components = 2;
      nir_ssa_def *value_lo = nir_u2u32(b, value);
      nir_ssa_def *value_hi = nir_u2u32(b, nir_ushr(b, value,
                                                       nir_imm_int(b, 32)));

      store->src[0] = nir_src_for_ssa(nir_vec2(b, value_lo, value_hi));
      store->src[1] = nir_src_for_ssa(buffer);
      store->src[2] = nir_src_for_ssa(offset);
      nir_builder_instr_insert(b, &store->instr);
   } else {
      nir_ssa_def *shift = NULL, *mask;
      switch (bit_size) {
      case 8:
         shift = nir_iand(b, ptr, nir_imm_int(b, 3));
         break;

      case 16:
         shift = nir_iand(b, ptr, nir_imm_int(b, 2));
         break;

      case 32:
         /* nothing */
         break;

      default:
         unreachable("unexpected bit_size");
      }

      if (shift) {
         /* need to decompose this into 32-bit masked writes */
         shift = nir_imul_imm(b, shift, 8);
         value = nir_ishl(b, nir_u2u32(b, value), shift);
         nir_ssa_def *mask =
            nir_inot(b, nir_ishl(b, nir_imm_int(b, (1 << bit_size) - 1),
                                    shift));

         nir_intrinsic_instr *store =
            nir_intrinsic_instr_create(b->shader,
                                       nir_intrinsic_store_global_masked_dxil);
         store->num_components = 1;
         store->src[0] = nir_src_for_ssa(value);
         store->src[1] = nir_src_for_ssa(mask);
         store->src[2] = nir_src_for_ssa(buffer);
         store->src[3] = nir_src_for_ssa(offset);
         nir_builder_instr_insert(b, &store->instr);
      } else {
         nir_intrinsic_instr *store =
            nir_intrinsic_instr_create(b->shader,
                                       nir_intrinsic_store_global_dxil);
         store->num_components = 1;
         store->src[0] = nir_src_for_ssa(value);
         store->src[1] = nir_src_for_ssa(buffer);
         store->src[2] = nir_src_for_ssa(offset);
         nir_builder_instr_insert(b, &store->instr);
      }
   }

   nir_instr_remove(&intr->instr);
}

static void
lower_global_mem_to_dxil(struct nir_shader *nir)
{
   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;
            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

            switch (intr->intrinsic) {
            case nir_intrinsic_load_global:
               lower_load_global(&b, intr);
               break;
            case nir_intrinsic_store_global:
               lower_store_global(&b, intr);
               break;
            }
         }
      }
   }
}

enum clc_debug_flags {
   CLC_DEBUG_DUMP_SPIRV = 1 << 0,
   CLC_DEBUG_VERBOSE = 1 << 1,
};

static const struct debug_named_value debug_options[] = {
   { "dump_spirv",  CLC_DEBUG_DUMP_SPIRV, "Dump spirv blobs" },
   { "verbose",  CLC_DEBUG_VERBOSE, NULL },
   DEBUG_NAMED_VALUE_END
};

DEBUG_GET_ONCE_FLAGS_OPTION(debug_clc, "CLC_DEBUG", debug_options, 0)

static void
clc_print_kernels_info(const struct clc_object *obj)
{
   fprintf(stdout, "Kernels:\n");
   for (unsigned i = 0; i < obj->num_kernels; i++) {
      const struct clc_kernel_arg *args = obj->kernels[i].args;
      bool first = true;

      fprintf(stdout, "\tvoid %s(", obj->kernels[i].name);
      for (unsigned j = 0; j < obj->kernels[i].num_args; j++) {
         if (!first)
            fprintf(stdout, ", ");
         else
            first = false;

         switch (args[j].address_qualifier) {
         case CLC_KERNEL_ARG_ADDRESS_GLOBAL:
            fprintf(stdout, "__global ");
            break;
         case CLC_KERNEL_ARG_ADDRESS_LOCAL:
            fprintf(stdout, "__local ");
            break;
	 case CLC_KERNEL_ARG_ADDRESS_CONSTANT:
            fprintf(stdout, "__constant ");
            break;
         default:
            break;
         }

         if (args[j].type_qualifier & CLC_KERNEL_ARG_TYPE_VOLATILE)
            fprintf(stdout, "volatile ");
         if (args[j].type_qualifier & CLC_KERNEL_ARG_TYPE_CONST)
            fprintf(stdout, "const ");
         if (args[j].type_qualifier & CLC_KERNEL_ARG_TYPE_RESTRICT)
            fprintf(stdout, "restrict ");

         fprintf(stdout, "%s %s", args[j].type_name, args[j].name);
      }
      fprintf(stdout, ");\n");
   }
}

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

   if (debug_get_option_debug_clc() & CLC_DEBUG_DUMP_SPIRV)
      clc_dump_spirv(&obj->spvbin, stdout);

   obj->kernels = clc_spirv_get_kernels_info(&obj->spvbin, &obj->num_kernels);

   if (debug_get_option_debug_clc() & CLC_DEBUG_VERBOSE)
      clc_print_kernels_info(obj);

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

   if (debug_get_option_debug_clc() & CLC_DEBUG_DUMP_SPIRV)
      clc_dump_spirv(&out_obj->spvbin, stdout);

   out_obj->kernels = clc_spirv_get_kernels_info(&out_obj->spvbin,
                                                 &out_obj->num_kernels);

   if (debug_get_option_debug_clc() & CLC_DEBUG_VERBOSE)
      clc_print_kernels_info(out_obj);

   return out_obj;
}

void clc_free_object(struct clc_object *obj)
{
   clc_free_kernels_info(obj->kernels, obj->num_kernels);
   clc_free_spirv_binary(&obj->spvbin);
   free(obj);
}

static unsigned
lower_bit_size_callback(const nir_alu_instr *alu, UNUSED void *data)
{
   switch (nir_dest_bit_size(alu->dest.dest)) {
   case 8:  return 16;

   case 1:
   case 16:
   case 32:
   case 64: return 0;

   default:
      unreachable("unexpected bit_size");
   }
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

   for (unsigned i = 0; i < obj->num_kernels; i++) {
      if (!strcmp(obj->kernels[i].name, entrypoint)) {
         dxil->kernel = &obj->kernels[i];
         break;
      }
   }

   if (!dxil->kernel) {
      fprintf(stderr, "D3D12: no '%s' kernel found\n", entrypoint);
      goto err_free_dxil;
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

   // Calculate UBO bindings
   unsigned binding = 0;
   nir_foreach_variable_safe(var, &nir->uniforms) {
      if (var->data.mode == nir_var_mem_ubo)
         var->data.binding = binding++;
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

   NIR_PASS_V(nir, lower_global_mem_to_dxil);

   NIR_PASS_V(nir, nir_lower_bit_size, lower_bit_size_callback, NULL);

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

   metadata->args = calloc(dxil->kernel->num_args,
                           sizeof(*metadata->args));
   if (!metadata->args) {
      debug_printf("D3D12: failed to allocate arg positions\n");
      goto err_free_dxil;
   }

   unsigned i = 0, uav_id = 0;
   nir_foreach_variable(var, &nir->inputs) {
      metadata->args[i].offset = var->data.driver_location;
      metadata->args[i].size = glsl_get_cl_size(var->type);
      metadata->kernel_inputs_buf_size = MAX2(metadata->kernel_inputs_buf_size,
                                              metadata->args[i].offset +
                                              metadata->args[i].size);
      if (dxil->kernel->args[i].address_qualifier == CLC_KERNEL_ARG_ADDRESS_GLOBAL)
         metadata->args[i].buf_id = uav_id++;
      i++;
   }

   assert(i == dxil->kernel->num_args);

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
            metadata->consts[metadata->num_consts].cbv_id = metadata->num_consts;
            metadata->num_consts++;
         } else
            unreachable("unexpected constant initializer");
      }
   }

   metadata->kernel_inputs_cbv_id = metadata->num_consts;
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
