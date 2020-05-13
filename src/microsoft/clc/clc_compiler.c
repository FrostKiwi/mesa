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

#include "nir.h"
#include "glsl_types.h"
#include "nir_types.h"
#include "nir_lower_libclc.h"
#include "clc_compiler.h"
#include "clc_helpers.h"
#include "../compiler/dxil_nir.h"
#include "../compiler/nir_to_dxil.h"

#include "util/u_debug.h"
#include <util/u_math.h>
#include "spirv/nir_spirv.h"
#include "nir_builder.h"

#include "spirv64-mesa3d-.spv.h"

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

static enum glsl_base_type
glsl_base_type_for_image_pipe_format(enum pipe_format format)
{
   switch (format)
   {
   case PIPE_FORMAT_R32G32B32A32_FLOAT: return GLSL_TYPE_FLOAT;
   case PIPE_FORMAT_R16G16B16A16_FLOAT: return GLSL_TYPE_FLOAT16;
   case PIPE_FORMAT_R32G32B32A32_SINT: return GLSL_TYPE_INT;
   case PIPE_FORMAT_R32G32B32A32_UINT: return GLSL_TYPE_UINT;
   }
   return GLSL_TYPE_VOID;
}

static void
clc_lower_input_image_deref(nir_builder *b, nir_deref_instr *deref, struct clc_dxil_metadata *metadata, unsigned *num_srvs, unsigned *num_uavs)
{
   // The input variable here isn't actually an image, it's just the
   // image format data.
   //
   // For every use of an image in a different way, we'll add an
   // appropriate uniform to match it. That can result in up to
   // 3 uniforms (float4, int4, uint4) for each image. Only one of these 
   // formats will actually produce correct data, but a single kernel 
   // could use runtime conditionals to potentially access any of them.
   //
   // If the image is used in a query that doesn't have a corresponding
   // DXIL intrinsic (CL image channel order or channel format), then
   // we'll add a kernel input for that data that'll be lowered by the 
   // explicit IO pass later on.
   //
   // After all that, we can remove the image input variable and deref.

   enum image_uniform_type {
      FLOAT4,
      INT4,
      UINT4,
      IMAGE_UNIFORM_TYPE_COUNT
   };

   nir_ssa_def *uniform_deref_dests[IMAGE_UNIFORM_TYPE_COUNT] = {0};

   nir_variable *in_var = nir_deref_instr_get_variable(deref);
   assert(in_var->data.mode == nir_var_shader_in);
   enum gl_access_qualifier access = in_var->data.access;

   int metadata_index = 0;
   while (metadata->args[metadata_index].image.buf_ids[0] != in_var->data.binding)
      metadata_index++;

   unsigned num_buf_ids = 0;

   nir_foreach_use_safe(src, &deref->dest.ssa) {
      // When we add samplers, we can have tex instructions here too, but just
      // intrinsics for now
      nir_intrinsic_instr *intrinsic = nir_instr_as_intrinsic(src->parent_instr);
      enum image_uniform_type type;
      enum nir_alu_type dest_type;

      switch (intrinsic->intrinsic) {
      case nir_intrinsic_image_deref_load: {
         enum pipe_format intr_format = nir_intrinsic_format(intrinsic);

         switch (intr_format) {
         case PIPE_FORMAT_R32G32B32A32_FLOAT: type = FLOAT4; dest_type = nir_type_float; break;
         case PIPE_FORMAT_R32G32B32A32_SINT: type = INT4; dest_type = nir_type_int; break;
         case PIPE_FORMAT_R32G32B32A32_UINT: type = UINT4; dest_type = nir_type_uint; break;
         default: assert(!"Unsupported image format for load.");
         }

         nir_ssa_def *image_deref = uniform_deref_dests[type];
         if (!image_deref) {
            assert(!(in_var->data.access & ACCESS_NON_READABLE));
            const struct glsl_type* new_var_type;
            if (in_var->data.access & ACCESS_NON_WRITEABLE) {
               // Non-writeable images should be converted to samplers,
               // since they may have texture operations done on them
               new_var_type = glsl_sampler_type(glsl_get_sampler_dim(in_var->type),
                     false, false, glsl_base_type_for_image_pipe_format(intr_format));
            } else {
               new_var_type = glsl_image_type(glsl_get_sampler_dim(in_var->type),
                     false, glsl_base_type_for_image_pipe_format(intr_format));
            }

            nir_variable *uniform = nir_variable_create(b->shader, nir_var_uniform, new_var_type, NULL);
            uniform->data.access = in_var->data.access;
            uniform->data.image.format = intr_format;
            uniform->data.binding = in_var->data.binding;
            if (num_buf_ids > 0) {
               // Need to assign a new binding
               int *binding_counter = (in_var->data.access & ACCESS_NON_WRITEABLE) ? num_srvs : num_uavs;
               metadata->args[metadata_index].image.buf_ids[num_buf_ids] = uniform->data.binding = (*binding_counter)++;
            }
            num_buf_ids++;

            b->cursor = nir_after_instr(&deref->instr);
            nir_deref_instr *deref_uniform = nir_build_deref_var(b, uniform);
            image_deref = uniform_deref_dests[type] = &deref_uniform->dest.ssa;
         }

         if (in_var->data.access & ACCESS_NON_WRITEABLE) {
            // Read of a read-only resource, convert to sampler fetch
            b->cursor = nir_before_instr(&intrinsic->instr);
            nir_tex_instr *tex = nir_tex_instr_create(b->shader, 2); // No LOD/MSAA

            tex->op = nir_texop_txf;
            tex->sampler_dim = glsl_get_sampler_dim(in_var->type);
            tex->src[0].src = nir_src_for_ssa(image_deref);
            tex->src[0].src_type = nir_tex_src_texture_deref;
            tex->src[1].src = nir_src_for_ssa(intrinsic->src[1].ssa);
            tex->src[1].src_type = nir_tex_src_coord;
            tex->dest_type = dest_type;
            nir_ssa_dest_init(&tex->instr, &tex->dest, 4, 32, NULL);

            nir_builder_instr_insert(b, &tex->instr);
            nir_ssa_def_rewrite_uses(&intrinsic->dest.ssa, nir_src_for_ssa(&tex->dest.ssa));
            nir_instr_remove(&intrinsic->instr);
         } else {
            // Read of a read-write resource, leave as image intrinsic
            nir_src uniform_src = nir_src_for_ssa(image_deref);
            nir_instr_rewrite_src(&intrinsic->instr, src, uniform_src);
         }

         break;
      }

      default:
         assert(!"Unsupported image intrinsic");
      }
   }

   metadata->args[metadata_index].image.num_buf_ids = num_buf_ids;

   nir_instr_remove(&deref->instr);
   exec_node_remove(&in_var->node);
}

static void
clc_lower_images(nir_shader *nir, struct clc_dxil_metadata *metadata, unsigned *num_srvs, unsigned *num_uavs)
{
   nir_foreach_function(func, nir) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type == nir_instr_type_deref) {
               nir_deref_instr *deref = nir_instr_as_deref(instr);

               if (deref->mode == nir_var_shader_in &&
                   glsl_type_is_image(deref->type)) {
                  assert(deref->deref_type == nir_deref_type_var);
                  clc_lower_input_image_deref(&b, deref, metadata, num_srvs, num_uavs);
               }
            }
         }
      }
   }
}

static void
clc_lower_64bit_semantics(nir_shader *nir)
{
   nir_foreach_function(func, nir) {
      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type == nir_instr_type_intrinsic) {
               nir_intrinsic_instr *intrinsic = nir_instr_as_intrinsic(instr);
               switch (intrinsic->intrinsic) {
               case nir_intrinsic_load_global_invocation_id:
               case nir_intrinsic_load_local_invocation_id:
               case nir_intrinsic_load_work_group_id:
                  break;
               default:
                  continue;
               }

               if (nir_instr_ssa_def(instr)->bit_size != 64)
                  continue;

               intrinsic->dest.ssa.bit_size = 32;
               b.cursor = nir_after_instr(instr);

               nir_ssa_def *i64 = nir_u2u64(&b, &intrinsic->dest.ssa);
               nir_ssa_def_rewrite_uses_after(
                  &intrinsic->dest.ssa,
                  nir_src_for_ssa(i64),
                  i64->parent_instr);
            }
         }
      }
   }
}

struct clc_context *
clc_context_new(void)
{
   struct clc_context *ctx = calloc(1, sizeof(*ctx));
   if (!ctx) {
      fprintf(stderr, "D3D12: failed to allocate a clc_context");
      return NULL;
   }

   const struct spirv_to_nir_options libclc_spirv_options = {
      .environment = NIR_SPIRV_OPENCL,
      .constant_as_global = false,
      .mangle = clc_fn_mangle_libclc,
      .create_library = true,
      .ubo_addr_format = nir_address_format_32bit_index_offset_pack64,
      .global_addr_format = nir_address_format_32bit_index_offset_pack64,
      .shared_addr_format = nir_address_format_32bit_offset_as_64bit,
      .temp_addr_format = nir_address_format_32bit_offset_as_64bit,
      .caps = {
         .address = true,
         .float64 = true,
         .int8 = true,
         .int16 = true,
         .int64 = true,
         .kernel = true,
      },
   };
   const struct nir_shader_compiler_options *libclc_nir_options =
      dxil_get_nir_compiler_options();

   glsl_type_singleton_init_or_ref();

   ctx->libclc_nir =
      spirv_to_nir((uint32_t *) libclc_spirv_bytecode,
                   sizeof(libclc_spirv_bytecode) / 4,
                   NULL, 0, MESA_SHADER_KERNEL, "libclc_spirv",
                   &libclc_spirv_options, libclc_nir_options, false);
   if (!ctx->libclc_nir) {
      fprintf(stderr, "D3D12: spirv_to_nir failed on libclc blob\n");
      goto err_free_ctx;
   }
   NIR_PASS_V(ctx->libclc_nir, nir_lower_goto_ifs);
   NIR_PASS_V(ctx->libclc_nir, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS_V(ctx->libclc_nir, nir_lower_returns);

   return ctx;

err_free_ctx:
   free(ctx);
   return NULL;
}

void
clc_free_context(struct clc_context *ctx)
{
   ralloc_free(ctx->libclc_nir);
   free(ctx);
   glsl_type_singleton_decref();
};

struct clc_object *
clc_compile(struct clc_context *ctx,
            const struct clc_compile_args *args,
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
clc_link(struct clc_context *ctx,
         const struct clc_linker_args *args,
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

   ret = clc_link_spirv_binaries(args, &out_obj->spvbin, &err_log);
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
   if (nir_op_infos[alu->op].is_conversion)
      return 0;
   switch (nir_dest_bit_size(alu->dest.dest)) {
   case 8: return 16;

   case 1:
   case 16:
   case 32:
   case 64: return 0;

   default:
      unreachable("unexpected bit_size");
   }
}

static void
shared_type_info(const struct glsl_type *type, unsigned *size, unsigned *align)
{
   *size = glsl_get_cl_size(type);
   *align = glsl_get_cl_alignment(type);
}

static nir_variable *
add_kernel_inputs_var(struct clc_dxil_object *dxil, nir_shader *nir,
                      unsigned *cbv_id)
{
   if (!dxil->kernel->num_args)
      return NULL;

   struct clc_dxil_metadata *metadata = &dxil->metadata;
   unsigned size = 0;

   nir_foreach_variable(var, &nir->inputs)
      size = MAX2(size,
                  var->data.driver_location +
                  glsl_get_cl_size(var->type));

   size = align(size, 4);

   nir_variable *var =
      nir_variable_create(nir, nir_var_mem_ubo,
                          glsl_array_type(glsl_uint_type(),
                                          size / 4, 0),
                          "kernel_inputs");
   var->data.binding = (*cbv_id)++;
   var->data.how_declared = nir_var_hidden;
   return var;
}

static nir_variable *
add_global_work_offset_var(struct clc_dxil_object *dxil,
                           struct nir_shader *nir, unsigned *cbv_id)
{
   struct clc_dxil_metadata *metadata = &dxil->metadata;
   nir_variable *var =
      nir_variable_create(nir, nir_var_mem_ubo,
                          glsl_array_type(glsl_uint_type(), 3, 0),
                          "kernel_global_work_offset");
   var->data.binding = (*cbv_id)++;
   var->data.how_declared = nir_var_hidden;
   return var;
}

static void
clc_lower_ubo_to_ssbo(nir_shader *nir,
                      const struct clc_kernel_info *kerninfo, unsigned *uav_id)
{
   /* First mark constant inputs as global inputs. */
   for (unsigned i = 0; i < kerninfo->num_args; i++) {
      if (kerninfo->args[i].address_qualifier == CLC_KERNEL_ARG_ADDRESS_CONSTANT) {
         assert(!(nir->info.cs.global_inputs & BITFIELD_BIT(i)));
         nir->info.cs.global_inputs |= BITFIELD_BIT(i);
      }
   }

   /* Update UBO vars and assign them a binding. */
   nir_foreach_variable(var, &nir->uniforms) {
      if (var->data.mode == nir_var_mem_ubo) {
         var->data.mode = nir_var_mem_ssbo;
         var->data.binding = (*uav_id)++;
      }
   }

   /* And finally patch all the derefs referincing the constant
    * variables/pointers.
    */
   nir_foreach_function(func, nir) {
      if (!func->is_entrypoint)
         continue;

      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_deref)
               continue;

            nir_deref_instr *deref = nir_instr_as_deref(instr);

            if (deref->mode != nir_var_mem_ubo)
               continue;

            deref->mode = nir_var_mem_ssbo;
         }
      }
   }
}

static void
clc_lower_global_to_ssbo(nir_shader *nir)
{
   nir_foreach_function(func, nir) {
      if (!func->is_entrypoint)
         continue;

      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_deref)
               continue;

            nir_deref_instr *deref = nir_instr_as_deref(instr);

            if (deref->mode != nir_var_mem_global)
               continue;

            deref->mode = nir_var_mem_ssbo;
         }
      }
   }
}

struct clc_dxil_object *
clc_to_dxil(struct clc_context *ctx,
            const struct clc_object *obj,
            const char *entrypoint,
            const struct clc_runtime_kernel_conf *conf,
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
      .clc_shader = ctx->libclc_nir,
      .mangle = clc_fn_mangle_libclc,
      .ubo_addr_format = nir_address_format_32bit_index_offset_pack64,
      .global_addr_format = nir_address_format_32bit_index_offset_pack64,
      .shared_addr_format = nir_address_format_32bit_offset_as_64bit,
      .temp_addr_format = nir_address_format_32bit_offset_as_64bit,
      .caps = {
         .address = true,
         .float64 = true,
         .int8 = true,
         .int16 = true,
         .int64 = true,
         .kernel = true,
         .kernel_image = true,
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

   struct clc_dxil_metadata *metadata = &dxil->metadata;

   metadata->args = calloc(dxil->kernel->num_args,
                           sizeof(*metadata->args));
   if (!metadata->args) {
      debug_printf("D3D12: failed to allocate arg positions\n");
      goto err_free_dxil;
   }

   // Calculate input offsets/metadata.
   unsigned i = 0, uav_id = 0, offset = 0;
   nir_foreach_variable(var, &nir->inputs) {
      unsigned size = glsl_get_cl_size(var->type);
      offset = align(offset, glsl_get_cl_alignment(var->type));
      var->data.driver_location = offset;

      metadata->args[i].offset = offset;
      metadata->args[i].size = size;
      metadata->kernel_inputs_buf_size = MAX2(metadata->kernel_inputs_buf_size,
                                              offset + size);
      if ((dxil->kernel->args[i].address_qualifier == CLC_KERNEL_ARG_ADDRESS_GLOBAL ||
           dxil->kernel->args[i].address_qualifier == CLC_KERNEL_ARG_ADDRESS_CONSTANT) &&
          // Ignore images during this pass - global memory buffers need to have contiguous bindings
          !glsl_type_is_image(var->type)) {
         metadata->args[i].globconstptr.buf_id = uav_id++;
      }
      i++;
      offset += size;
   }

   assert(i == dxil->kernel->num_args);

   // Second pass over inputs to calculate image bindings
   unsigned srv_id = 0;
   i = 0;
   nir_foreach_variable(var, &nir->inputs) {
      if (glsl_type_is_image(var->type)) {
         if (var->data.access == ACCESS_NON_WRITEABLE) {
            metadata->args[i].image.buf_ids[0] = srv_id++;
         } else {
            // Write or read-write are UAVs
            metadata->args[i].image.buf_ids[0] = uav_id++;
         }

         metadata->args[i].image.num_buf_ids = 1;
         var->data.binding = metadata->args[i].image.buf_ids[0];
      }
      i++;
   }

   // Inline all functions first.
   // according to the comment on nir_inline_functions
   NIR_PASS_V(nir, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS_V(nir, nir_lower_returns);
   NIR_PASS_V(nir, nir_lower_libclc, ctx->libclc_nir);
   NIR_PASS_V(nir, nir_inline_functions);
   NIR_PASS_V(nir, nir_opt_deref);

   // Pick off the single entrypoint that we want.
   foreach_list_typed_safe(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         exec_node_remove(&func->node);
   }
   assert(exec_list_length(&nir->functions) == 1);

   NIR_PASS_V(nir, nir_lower_variable_initializers, ~(nir_var_function_temp | nir_var_shader_temp));

   // Needs to come before lower_explicit_io
   NIR_PASS_V(nir, clc_lower_images, metadata, &srv_id, &uav_id);
   NIR_PASS_V(nir, nir_lower_samplers);

   // copy propagate to prepare for lower_explicit_io
   NIR_PASS_V(nir, nir_split_var_copies);
   NIR_PASS_V(nir, nir_opt_copy_prop_vars);
   NIR_PASS_V(nir, nir_lower_var_copies);
   NIR_PASS_V(nir, nir_lower_vars_to_ssa);
   NIR_PASS_V(nir, nir_lower_alu);
   NIR_PASS_V(nir, nir_opt_dce);

   NIR_PASS_V(nir, dxil_nir_lower_ubo_to_temp);
   NIR_PASS_V(nir, clc_lower_ubo_to_ssbo, dxil->kernel, &uav_id);
   NIR_PASS_V(nir, clc_lower_global_to_ssbo);
   NIR_PASS_V(nir, dxil_nir_lower_deref_ssbo);

   NIR_PASS_V(nir, nir_lower_vars_to_explicit_types,
              nir_var_mem_shared, shared_type_info);

   assert(nir->info.cs.ptr_size == 64);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_mem_ssbo,
              nir_address_format_32bit_index_offset_pack64);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_shader_in,
              nir_address_format_32bit_global);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_mem_shared,
              nir_address_format_32bit_offset_as_64bit);

   NIR_PASS_V(nir, nir_lower_system_values);
   NIR_PASS_V(nir, clc_lower_64bit_semantics);
   if (nir_options->lower_int64_options)
      NIR_PASS_V(nir, nir_lower_int64, nir_options->lower_int64_options);

   NIR_PASS_V(nir, nir_opt_deref);
   NIR_PASS_V(nir, nir_lower_vars_to_ssa);

   unsigned cbv_id = 0;

   nir_variable *inputs_var =
      add_kernel_inputs_var(dxil, nir, &cbv_id);
   nir_variable *global_work_offset_var =
      add_global_work_offset_var(dxil, nir, &cbv_id);

   NIR_PASS_V(nir, dxil_nir_lower_kernel_input_loads, inputs_var);
   NIR_PASS_V(nir, dxil_nir_lower_kernel_global_work_offset,
              global_work_offset_var);
   NIR_PASS_V(nir, dxil_nir_lower_loads_stores_to_dxil);
   NIR_PASS_V(nir, dxil_nir_opt_alu_deref_srcs);
   NIR_PASS_V(nir, dxil_nir_lower_atomics_to_dxil);

   NIR_PASS_V(nir, nir_lower_bit_size, lower_bit_size_callback, NULL);
   
   if (conf && conf->lower_int64) {
      NIR_PASS_V(nir, nir_lower_int64, ~0u);
   }

   nir_validate_shader(nir, "Validate before feeding NIR to the DXIL compiler");
   struct nir_to_dxil_options opts = {
      .interpolate_at_vertex = false
   };

   // Patch the localsize before calling nir_to_dxil().
   if (conf) {
      for (unsigned i = 0; i < ARRAY_SIZE(nir->info.cs.local_size); i++) {
         if (!conf->local_size[i] ||
             conf->local_size[i] == nir->info.cs.local_size[i])
            continue;

         if (nir->info.cs.local_size[i] &&
             nir->info.cs.local_size[i] != conf->local_size[i]) {
            debug_printf("D3D12: runtime local size does not match reqd_work_group_size() values\n");
            goto err_free_dxil;
         }

         nir->info.cs.local_size[i] = conf->local_size[i];
      }
   }

   for (unsigned i = 0; i < dxil->kernel->num_args; i++) {
      if (dxil->kernel->args[i].address_qualifier != CLC_KERNEL_ARG_ADDRESS_LOCAL)
         continue;

      /* If we don't have the runtime conf yet, we just create a dummy variable.
       * This will be adjusted when clc_to_dxil() is called with a conf
       * argument.
       */
      unsigned size = 4;
      if (conf && conf->args)
         size = conf->args[i].localptr.size;

      /* The alignment required for the pointee type is not easy to get from
       * here, so let's base our logic on the size itself. Anything bigger than
       * the maximum alignment constraint (which is 128 bytes, since ulong16 or
       * doubl16 size are the biggest base types) should be aligned on this
       * maximum alignment constraint. For smaller types, we use the size
       * itself to calculate the alignment.
       */
      unsigned alignment = size < 128 ? (1 << (ffs(size) - 1)) : 128;

      nir->info.cs.shared_size = align(nir->info.cs.shared_size, alignment);
      metadata->args[i].localptr.sharedmem_offset = nir->info.cs.shared_size;
      nir->info.cs.shared_size += size;
   }

   struct blob tmp;
   if (!nir_to_dxil(nir, &opts, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      goto err_free_dxil;
   }

   memcpy(metadata->local_size, nir->info.cs.local_size,
          sizeof(metadata->local_size));
   memcpy(metadata->local_size_hint, nir->info.cs.local_size_hint,
          sizeof(metadata->local_size));

   nir_foreach_variable(var, &nir->uniforms) {
      if (var->data.mode == nir_var_mem_ssbo && var->constant_initializer) {
         if (glsl_type_is_array(var->type)) {
            int size = align(glsl_get_cl_size(var->type), 4);
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
            metadata->consts[metadata->num_consts].uav_id = var->data.binding;
            metadata->num_consts++;
         } else
            unreachable("unexpected constant initializer");
      }
   }

   metadata->kernel_inputs_cbv_id = inputs_var ? inputs_var->data.binding : 0;
   metadata->global_work_offset_cbv_id = global_work_offset_var->data.binding;
   metadata->num_uavs = uav_id;

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
