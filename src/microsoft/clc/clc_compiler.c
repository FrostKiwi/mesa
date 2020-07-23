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
#include "clc_nir.h"
#include "../compiler/dxil_nir.h"
#include "../compiler/dxil_nir_lower_int_samplers.h"
#include "../compiler/nir_to_dxil.h"

#include "util/u_debug.h"
#include <util/u_math.h>
#include "spirv/nir_spirv.h"
#include "nir_builder.h"
#include "nir_builtin_builder.h"

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
   switch (format) {
   case PIPE_FORMAT_R32G32B32A32_FLOAT: return GLSL_TYPE_FLOAT;
   case PIPE_FORMAT_R16G16B16A16_FLOAT: return GLSL_TYPE_FLOAT16;
   case PIPE_FORMAT_R32G32B32A32_SINT: return GLSL_TYPE_INT;
   case PIPE_FORMAT_R32G32B32A32_UINT: return GLSL_TYPE_UINT;
   default: unreachable("Unexpected image format");
   }
}

static enum pipe_format
pipe_format_for_nir_format(enum nir_alu_type format)
{
   switch (format) {
   case nir_type_float: return PIPE_FORMAT_R32G32B32A32_FLOAT;
   case nir_type_float16: return PIPE_FORMAT_R16G16B16A16_FLOAT;
   case nir_type_int: return PIPE_FORMAT_R32G32B32A32_SINT;
   case nir_type_uint: return PIPE_FORMAT_R32G32B32A32_UINT;
   default: unreachable("Unexpected nir format");
   }
}

struct clc_image_lower_context
{
   struct clc_dxil_metadata *metadata;
   unsigned *num_srvs;
   unsigned *num_uavs;
   nir_deref_instr *deref;
   unsigned num_buf_ids;
   int metadata_index;
};

static nir_ssa_def *
lower_image_deref_impl(nir_builder *b, struct clc_image_lower_context *context,
                       const struct glsl_type *new_var_type,
                       unsigned *num_bindings, enum pipe_format image_format)
{
   nir_variable *in_var = nir_deref_instr_get_variable(context->deref);
   nir_variable *uniform = nir_variable_create(b->shader, nir_var_uniform, new_var_type, NULL);
   uniform->data.access = in_var->data.access;
   uniform->data.image.format = image_format;
   uniform->data.binding = in_var->data.binding;
   if (context->num_buf_ids > 0) {
      // Need to assign a new binding
      context->metadata->args[context->metadata_index].
         image.buf_ids[context->num_buf_ids] = uniform->data.binding = (*num_bindings)++;
   }
   context->num_buf_ids++;

   b->cursor = nir_after_instr(&context->deref->instr);
   nir_deref_instr* deref_uniform = nir_build_deref_var(b, uniform);
   return &deref_uniform->dest.ssa;
}

static nir_ssa_def *
lower_read_only_image_deref(nir_builder *b, struct clc_image_lower_context *context,
                            enum pipe_format image_format)
{
   nir_variable *in_var = nir_deref_instr_get_variable(context->deref);

   // Non-writeable images should be converted to samplers,
   // since they may have texture operations done on them
   const struct glsl_type *new_var_type =
      glsl_sampler_type(glsl_get_sampler_dim(in_var->type),
            false, glsl_sampler_type_is_array(in_var->type),
            glsl_base_type_for_image_pipe_format(image_format));
   return lower_image_deref_impl(b, context, new_var_type, context->num_srvs, image_format);
}

static nir_ssa_def *
lower_read_write_image_deref(nir_builder *b, struct clc_image_lower_context *context,
                             enum pipe_format image_format)
{
   nir_variable *in_var = nir_deref_instr_get_variable(context->deref);
   const struct glsl_type *new_var_type =
      glsl_image_type(glsl_get_sampler_dim(in_var->type),
         glsl_sampler_type_is_array(in_var->type),
         glsl_base_type_for_image_pipe_format(image_format));
   return lower_image_deref_impl(b, context, new_var_type, context->num_uavs, image_format);
}

static void
clc_lower_input_image_deref(nir_builder *b, struct clc_image_lower_context *context)
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
   nir_ssa_def *format_deref_dest = NULL, *order_deref_dest = NULL;

   nir_variable *in_var = nir_deref_instr_get_variable(context->deref);
   assert(in_var->data.mode == nir_var_shader_in);
   enum gl_access_qualifier access = in_var->data.access;

   context->metadata_index = 0;
   while (context->metadata->args[context->metadata_index].image.buf_ids[0] != in_var->data.binding)
      context->metadata_index++;

   context->num_buf_ids = 0;

   for (int pass = 0; pass < 2; ++pass) {
      nir_foreach_use_safe(src, &context->deref->dest.ssa) {
         enum image_uniform_type type;

         if (src->parent_instr->type == nir_instr_type_intrinsic) {
            nir_intrinsic_instr *intrinsic = nir_instr_as_intrinsic(src->parent_instr);
            enum nir_alu_type dest_type;

            switch (intrinsic->intrinsic) {
            case nir_intrinsic_image_deref_load:
            case nir_intrinsic_image_deref_store: {
               enum pipe_format intr_format = nir_intrinsic_format(intrinsic);

               switch (intr_format) {
               case PIPE_FORMAT_R32G32B32A32_FLOAT: type = FLOAT4; dest_type = nir_type_float; break;
               case PIPE_FORMAT_R32G32B32A32_SINT: type = INT4; dest_type = nir_type_int; break;
               case PIPE_FORMAT_R32G32B32A32_UINT: type = UINT4; dest_type = nir_type_uint; break;
               default: unreachable("Unsupported image format for load.");
               }

               nir_ssa_def *image_deref = uniform_deref_dests[type];
               if (!image_deref) {
                  const struct glsl_type *new_var_type;
                  if (in_var->data.access & ACCESS_NON_WRITEABLE) {
                     image_deref = lower_read_only_image_deref(b, context, intr_format);
                  } else {
                     image_deref = lower_read_write_image_deref(b, context, intr_format);
                  }

                  uniform_deref_dests[type] = image_deref;
               }

               if (in_var->data.access & ACCESS_NON_WRITEABLE) {
                  // Read of a read-only resource, convert to sampler fetch
                  assert(intrinsic->intrinsic == nir_intrinsic_image_deref_load);
                  b->cursor = nir_before_instr(&intrinsic->instr);

                  nir_deref_instr *deref = nir_src_as_deref(intrinsic->src[0]);
                  unsigned coord_comps = glsl_get_sampler_coordinate_components(deref->type);
                  nir_ssa_def *coord =
                     nir_channels(b, intrinsic->src[1].ssa, (1 << coord_comps) - 1);
                  nir_tex_instr *tex = nir_tex_instr_create(b->shader, 3);

                  tex->op = nir_texop_txf;
                  tex->is_array = glsl_sampler_type_is_array(in_var->type);
                  tex->sampler_dim = glsl_get_sampler_dim(in_var->type);
                  tex->src[0].src = nir_src_for_ssa(image_deref);
                  tex->src[0].src_type = nir_tex_src_texture_deref;
                  tex->src[1].src = nir_src_for_ssa(coord);
                  tex->src[1].src_type = nir_tex_src_coord;
                  tex->src[2].src = nir_src_for_ssa(nir_imm_int(b, 0));
                  tex->src[2].src_type = nir_tex_src_lod;
                  tex->coord_components = tex->src[1].src.ssa->num_components;
                  tex->dest_type = dest_type;
                  nir_ssa_dest_init(&tex->instr, &tex->dest, 4, 32, NULL);

                  nir_builder_instr_insert(b, &tex->instr);
                  nir_ssa_def_rewrite_uses(&intrinsic->dest.ssa, nir_src_for_ssa(&tex->dest.ssa));
                  nir_instr_remove(&intrinsic->instr);
               } else {
                  // Op on a writable image, leave as image intrinsic
                  nir_src uniform_src = nir_src_for_ssa(image_deref);
                  nir_instr_rewrite_src(&intrinsic->instr, src, uniform_src);
               }
               break;
            }

            case nir_intrinsic_image_deref_size:
            case nir_intrinsic_image_deref_size_lod: {
               nir_ssa_def *image_deref = NULL;
               for (unsigned i = 0; i < IMAGE_UNIFORM_TYPE_COUNT; ++i) {
                  if (uniform_deref_dests[i]) {
                     image_deref = uniform_deref_dests[i];
                     break;
                  }
               }
               if (!image_deref) {
                  // Skip for now and come back to it
                  if (pass == 0)
                     break;

                  type = FLOAT4;
                  const struct glsl_type *new_var_type;
                  if (in_var->data.access & ACCESS_NON_WRITEABLE) {
                     image_deref = lower_read_only_image_deref(b, context, PIPE_FORMAT_R32G32B32A32_FLOAT);
                  } else {
                     image_deref = lower_read_write_image_deref(b, context, PIPE_FORMAT_R32G32B32A32_FLOAT);
                  }

                  uniform_deref_dests[type] = image_deref;
               }

               if (in_var->data.access & ACCESS_NON_WRITEABLE) {
                  // Size query on non-readable resource, convert to txs
                  b->cursor = nir_before_instr(&intrinsic->instr);
                  unsigned num_src = 1;
                  if (intrinsic->intrinsic == nir_intrinsic_image_deref_size_lod)
                     num_src = 2;
                  nir_tex_instr *tex = nir_tex_instr_create(b->shader, num_src);

                  tex->op = nir_texop_txs;
                  tex->is_array = glsl_sampler_type_is_array(in_var->type);
                  tex->sampler_dim = glsl_get_sampler_dim(in_var->type);
                  tex->src[0].src = nir_src_for_ssa(image_deref);
                  tex->src[0].src_type = nir_tex_src_texture_deref;
                  if (intrinsic->intrinsic == nir_intrinsic_image_deref_size_lod) {
                     tex->src[1].src = nir_src_for_ssa(intrinsic->src[1].ssa);
                     tex->src[1].src_type = nir_tex_src_lod;
                  }
                  tex->dest_type = nir_type_uint;
                  nir_ssa_dest_init(&tex->instr, &tex->dest, nir_tex_instr_dest_size(tex), 32, NULL);

                  nir_builder_instr_insert(b, &tex->instr);
                  nir_ssa_def_rewrite_uses(&intrinsic->dest.ssa, nir_src_for_ssa(&tex->dest.ssa));
                  nir_instr_remove(&intrinsic->instr);
               } else {
                  // Op on a writable image, leave as image intrinsic
                  nir_src uniform_src = nir_src_for_ssa(image_deref);
                  nir_instr_rewrite_src(&intrinsic->instr, src, uniform_src);
               }
               break;
            }

            case nir_intrinsic_image_deref_format:
            case nir_intrinsic_image_deref_order: {
               nir_ssa_def **cached_deref = intrinsic->intrinsic == nir_intrinsic_image_deref_format ?
                  &format_deref_dest : &order_deref_dest;
               if (!*cached_deref) {
                  nir_variable *new_input = nir_variable_create(b->shader, nir_var_shader_in, glsl_uint_type(), NULL);
                  new_input->data.driver_location = in_var->data.driver_location;
                  if (intrinsic->intrinsic == nir_intrinsic_image_deref_format) {
                     /* Match cl_image_format { image_channel_order, image_channel_data_type }; */
                     new_input->data.driver_location += glsl_get_cl_size(new_input->type);
                  }

                  b->cursor = nir_after_instr(&context->deref->instr);
                  *cached_deref = nir_load_var(b, new_input);
               }

               /* No actual intrinsic needed here, just reference the loaded variable */
               nir_ssa_def_rewrite_uses(&intrinsic->dest.ssa, nir_src_for_ssa(*cached_deref));
               nir_instr_remove(&intrinsic->instr);
               break;
            }

            default:
               unreachable("Unsupported image intrinsic");
            }
         } else if (src->parent_instr->type == nir_instr_type_tex) {
            assert(in_var->data.access & ACCESS_NON_WRITEABLE);
            nir_tex_instr *tex = nir_instr_as_tex(src->parent_instr);

            switch (tex->dest_type) {
            case nir_type_float: type = FLOAT4; break;
            case nir_type_int: type = INT4; break;
            case nir_type_uint: type = UINT4; break;
            default: unreachable("Unsupported image format for sample.");
            }

            nir_ssa_def *image_deref = uniform_deref_dests[type];
            if (!image_deref) {
               enum pipe_format image_format = pipe_format_for_nir_format(tex->dest_type);
               image_deref = uniform_deref_dests[type] =
                  lower_read_only_image_deref(b, context, image_format);
            }

            nir_src uniform_src = nir_src_for_ssa(image_deref);
            nir_instr_rewrite_src(&tex->instr, src, uniform_src);
         }
      }
   }

   context->metadata->args[context->metadata_index].image.num_buf_ids = context->num_buf_ids;

   nir_instr_remove(&context->deref->instr);
   exec_node_remove(&in_var->node);
}

static void
clc_lower_sampler_deref(nir_builder *b, nir_deref_instr *deref)
{
    nir_variable *in_var = nir_deref_instr_get_variable(deref);
    nir_variable *uniform = nir_variable_create(
       b->shader, nir_var_uniform, in_var->type, NULL);
    uniform->data.binding = in_var->data.binding;

    b->cursor = nir_after_instr(&deref->instr);
    nir_deref_instr *deref_uniform = nir_build_deref_var(b, uniform);
    nir_ssa_def_rewrite_uses(&deref->dest.ssa,
       nir_src_for_ssa(&deref_uniform->dest.ssa));

    nir_instr_remove(&deref->instr);
    exec_node_remove(&in_var->node);
}

static void
clc_lower_images(nir_shader *nir, struct clc_image_lower_context *context)
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
               context->deref = nir_instr_as_deref(instr);

               if (context->deref->mode == nir_var_shader_in &&
                   glsl_type_is_image(context->deref->type)) {
                  assert(context->deref->deref_type == nir_deref_type_var);
                  clc_lower_input_image_deref(&b, context);
               } else if (context->deref->mode == nir_var_shader_in &&
                  glsl_type_is_sampler(context->deref->type)) {
                  assert(context->deref->deref_type == nir_deref_type_var);
                  clc_lower_sampler_deref(&b, context->deref);
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
               case nir_intrinsic_load_global_invocation_id_with_offset:
               case nir_intrinsic_load_global_invocation_offset:
               case nir_intrinsic_load_local_invocation_id:
               case nir_intrinsic_load_work_group_id:
               case nir_intrinsic_load_work_group_offset:
               case nir_intrinsic_load_work_group_id_with_offset:
               case nir_intrinsic_load_num_total_work_groups:
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

static void
clc_lower_nonnormalized_samplers(nir_shader *nir,
                                 const dxil_wrap_sampler_state *states)
{
   nir_foreach_function(func, nir) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_tex)
               continue;
            nir_tex_instr *tex = nir_instr_as_tex(instr);

            int sampler_src_idx = nir_tex_instr_src_index(tex, nir_tex_src_sampler_deref);
            if (sampler_src_idx == -1)
               continue;

            nir_src *sampler_src = &tex->src[sampler_src_idx].src;
            assert(sampler_src->is_ssa && sampler_src->ssa->parent_instr->type == nir_instr_type_deref);
            nir_variable *sampler = nir_deref_instr_get_variable(
               nir_instr_as_deref(sampler_src->ssa->parent_instr));

            // If the sampler returns ints, we'll handle this in the int lowering pass
            if (nir_alu_type_get_base_type(tex->dest_type) != nir_type_float)
               continue;

            // If sampler uses normalized coords, nothing to do
            if (!states[sampler->data.binding].is_nonnormalized_coords)
               continue;

            b.cursor = nir_before_instr(&tex->instr);

            int coords_idx = nir_tex_instr_src_index(tex, nir_tex_src_coord);
            assert(coords_idx != -1);
            nir_ssa_def *coords =
               nir_ssa_for_src(&b, tex->src[coords_idx].src, tex->coord_components);

            nir_ssa_def *txs = nir_i2f32(&b, nir_get_texture_size(&b, tex));

            // Normalize coords for tex
            nir_ssa_def *scale = nir_frcp(&b, txs);
            nir_ssa_def *comps[4];
            for (unsigned i = 0; i < coords->num_components; ++i) {
               comps[i] = nir_channel(&b, coords, i);
               if (tex->is_array && i == coords->num_components - 1) {
                  // Don't scale the array index, but do clamp it
                  comps[i] = nir_fround_even(&b, comps[i]);
                  comps[i] = nir_fmax(&b, comps[i], nir_imm_float(&b, 0.0f));
                  comps[i] = nir_fmin(&b, comps[i], nir_fsub(&b, nir_channel(&b, txs, i), nir_imm_float(&b, 1.0f)));
                  break;
               }

               // The CTS is pretty clear that this value has to be floored for nearest sampling
               // but must not be for linear sampling.
               if (!states[sampler->data.binding].is_linear_filtering)
                  comps[i] = nir_ffloor(&b, comps[i]);
               comps[i] = nir_fmul(&b, comps[i], nir_channel(&b, scale, i));
            }
            nir_ssa_def *normalized_coords = nir_vec(&b, comps, coords->num_components);
            nir_instr_rewrite_src(&tex->instr,
                                  &tex->src[coords_idx].src,
                                  nir_src_for_ssa(normalized_coords));
         }
      }
   }
}


struct clc_context *
clc_context_new(const struct clc_logger *logger)
{
   struct clc_context *ctx = calloc(1, sizeof(*ctx));
   if (!ctx) {
      clc_error(logger, "D3D12: failed to allocate a clc_context");
      return NULL;
   }

   const struct spirv_to_nir_options libclc_spirv_options = {
      .environment = NIR_SPIRV_OPENCL,
      .constant_as_global = false,
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
      clc_error(logger, "D3D12: spirv_to_nir failed on libclc blob");
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
   int ret;

   obj = calloc(1, sizeof(*obj));
   if (!obj) {
      clc_error(logger, "D3D12: failed to allocate a clc_object");
      return NULL;
   }

   ret = clc_to_spirv(args, &obj->spvbin, logger);
   if (ret < 0) {
      free(obj);
      return NULL;
   }

   if (debug_get_option_debug_clc() & CLC_DEBUG_DUMP_SPIRV)
      clc_dump_spirv(&obj->spvbin, stdout);

   return obj;
}

struct clc_object *
clc_link(struct clc_context *ctx,
         const struct clc_linker_args *args,
         const struct clc_logger *logger)
{
   struct clc_object *out_obj;
   int ret;

   out_obj = malloc(sizeof(*out_obj));
   if (!out_obj) {
      clc_error(logger, "failed to allocate a clc_object");
      return NULL;
   }

   ret = clc_link_spirv_binaries(args, &out_obj->spvbin, logger);
   if (ret < 0) {
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
add_work_properties_var(struct clc_dxil_object *dxil,
                           struct nir_shader *nir, unsigned *cbv_id)
{
   struct clc_dxil_metadata *metadata = &dxil->metadata;
   nir_variable *var =
      nir_variable_create(nir, nir_var_mem_ubo,
                          glsl_array_type(glsl_uint_type(),
                                          sizeof(struct clc_work_properties_data) / sizeof(unsigned),
                                          0),
                          "kernel_work_properies");
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

static void
copy_const_initializer(const nir_constant *constant, const struct glsl_type *type,
                       uint8_t *data)
{
   unsigned size = glsl_get_cl_size(type);

   if (glsl_type_is_array(type)) {
      const struct glsl_type *elm_type = glsl_get_array_element(type);
      unsigned step_size = glsl_get_explicit_stride(type);

      for (unsigned i = 0; i < constant->num_elements; i++) {
         copy_const_initializer(constant->elements[i], elm_type,
                                data + (i * step_size));
      }
   } else if (glsl_type_is_struct(type)) {
      for (unsigned i = 0; i < constant->num_elements; i++) {
         const struct glsl_type *elm_type = glsl_get_struct_field(type, i);
         int offset = glsl_get_struct_field_offset(type, i);
         copy_const_initializer(constant->elements[i], elm_type, data + offset);
      }
   } else {
      assert(glsl_type_is_vector_or_scalar(type));

      for (unsigned i = 0; i < glsl_get_components(type); i++) {
         switch (glsl_get_bit_size(type)) {
         case 64:
            *((uint64_t *)data) = constant->values[i].u64;
            break;
         case 32:
            *((uint32_t *)data) = constant->values[i].u32;
            break;
         case 16:
            *((uint16_t *)data) = constant->values[i].u16;
            break;
         case 8:
            *((uint8_t *)data) = constant->values[i].u8;
            break;
         default:
            unreachable("Invalid base type");
         }

         data += glsl_get_bit_size(type) / 8;
      }
   }
}

static const struct glsl_type *
get_cast_type(unsigned bit_size)
{
   switch (bit_size) {
   case 64:
      return glsl_int64_t_type();
   case 32:
      return glsl_int_type();
   case 16:
      return glsl_int16_t_type();
   case 8:
      return glsl_int8_t_type();
   }
   unreachable("Invalid bit_size");
}

static void
split_unaligned_load(nir_builder *b, nir_intrinsic_instr *intrin)
{
   unsigned alignment = nir_intrinsic_align(intrin);
   enum gl_access_qualifier access = nir_intrinsic_access(intrin);
   nir_ssa_def *srcs[NIR_MAX_VEC_COMPONENTS * NIR_MAX_VEC_COMPONENTS * sizeof(int64_t) / 8];
   unsigned comp_size = intrin->dest.ssa.bit_size / 8;
   unsigned num_comps = intrin->dest.ssa.num_components;

   b->cursor = nir_before_instr(&intrin->instr);

   nir_deref_instr *ptr = nir_src_as_deref(intrin->src[0]);
   const struct glsl_type *cast_type = get_cast_type(alignment * 8);
   nir_deref_instr *cast = nir_build_deref_cast(b, &ptr->dest.ssa, ptr->mode, cast_type, alignment);

   unsigned num_loads = DIV_ROUND_UP(comp_size * num_comps, alignment);
   for (unsigned i = 0; i < num_loads; ++i) {
      nir_deref_instr *elem = nir_build_deref_ptr_as_array(b, cast, nir_imm_intN_t(b, i, cast->dest.ssa.bit_size));
      srcs[i] = nir_load_deref_with_access_and_align(b, elem, access, alignment, 0);
   }

   nir_ssa_def *new_dest = nir_extract_bits(b, srcs, num_loads, 0, num_comps, intrin->dest.ssa.bit_size);
   nir_ssa_def_rewrite_uses(&intrin->dest.ssa, nir_src_for_ssa(new_dest));
   nir_instr_remove(&intrin->instr);
}

static void
split_unaligned_store(nir_builder *b, nir_intrinsic_instr *intrin)
{
   unsigned alignment = nir_intrinsic_align(intrin);
   enum gl_access_qualifier access = nir_intrinsic_access(intrin);

   assert(intrin->src[1].is_ssa);
   nir_ssa_def *value = intrin->src[1].ssa;
   unsigned comp_size = value->bit_size / 8;
   unsigned num_comps = value->num_components;

   b->cursor = nir_before_instr(&intrin->instr);

   nir_deref_instr *ptr = nir_src_as_deref(intrin->src[0]);
   const struct glsl_type *cast_type = get_cast_type(alignment * 8);
   nir_deref_instr *cast = nir_build_deref_cast(b, &ptr->dest.ssa, ptr->mode, cast_type, alignment);

   unsigned num_stores = DIV_ROUND_UP(comp_size * num_comps, alignment);
   for (unsigned i = 0; i < num_stores; ++i) {
      nir_ssa_def *substore_val = nir_extract_bits(b, &value, 1, i * alignment * 8, 1, alignment * 8);
      nir_deref_instr *elem = nir_build_deref_ptr_as_array(b, cast, nir_imm_intN_t(b, i, cast->dest.ssa.bit_size));
      nir_store_deref_with_access_and_align(b, elem, substore_val, ~0, access, alignment, 0);
   }

   nir_instr_remove(&intrin->instr);
}

static bool
split_unaligned_loads_stores(nir_shader *shader)
{
   bool progress = false;

   nir_foreach_function(function, shader) {
      if (!function->impl)
         continue;

      nir_builder b;
      nir_builder_init(&b, function->impl);

      nir_foreach_block(block, function->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;
            nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
            if (intrin->intrinsic != nir_intrinsic_load_deref &&
                intrin->intrinsic != nir_intrinsic_store_deref)
               continue;
            unsigned alignment = nir_intrinsic_align(intrin);
            nir_deref_instr *deref = nir_src_as_deref(intrin->src[0]);

            /* Alignment = 0 means naturally aligned. We can load anything at
             * 4-byte alignment, except for UBOs (AKA CBs where the granularity
             * is 16 bytes.
             */
            if (alignment == 0 ||
                alignment >= (deref->mode == nir_var_mem_ubo ? 16 : 4))
               continue;

            nir_ssa_def *val;
            if (intrin->intrinsic == nir_intrinsic_load_deref) {
               assert(intrin->dest.is_ssa);
               val = &intrin->dest.ssa;
            } else {
               assert(intrin->src[1].is_ssa);
               val = intrin->src[1].ssa;
            }

            unsigned natural_alignment =
               val->bit_size / 8 *
               (val->num_components == 3 ? 4 : val->num_components);

            if (alignment >= natural_alignment)
               continue;

            if (intrin->intrinsic == nir_intrinsic_load_deref)
               split_unaligned_load(&b, intrin);
            else
               split_unaligned_store(&b, intrin);
            progress = true;
         }
      }
   }

   return progress;
}

static enum pipe_tex_wrap
wrap_from_cl_addressing(unsigned addressing_mode)
{
   switch (addressing_mode)
   {
   default:
   case ADDRESSING_MODE_NONE:
   case ADDRESSING_MODE_CLAMP:
      // Since OpenCL's only border color is 0's and D3D specs out-of-bounds loads to return 0, don't apply any wrap mode
      return (enum pipe_tex_wrap)-1;
   case ADDRESSING_MODE_CLAMP_TO_EDGE: return PIPE_TEX_WRAP_CLAMP_TO_EDGE;
   case ADDRESSING_MODE_REPEAT: return PIPE_TEX_WRAP_REPEAT;
   case ADDRESSING_MODE_REPEAT_MIRRORED: return PIPE_TEX_WRAP_MIRROR_REPEAT;
   }
}

static bool shader_has_double(nir_shader *nir)
{
   bool progress = false;

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;

      assert(func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_alu)
               continue;

             nir_alu_instr *alu = nir_instr_as_alu(instr);
             const nir_op_info *info = &nir_op_infos[alu->op];

             if (info->output_type & nir_type_float &&
                 nir_dest_bit_size(alu->dest.dest) == 64)
                 return true;
         }
      }
   }

   return false;
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
      clc_error(logger, "failed to allocate the dxil object");
      return NULL;
   }

   for (unsigned i = 0; i < obj->num_kernels; i++) {
      if (!strcmp(obj->kernels[i].name, entrypoint)) {
         dxil->kernel = &obj->kernels[i];
         break;
      }
   }

   if (!dxil->kernel) {
      clc_error(logger, "no '%s' kernel found", entrypoint);
      goto err_free_dxil;
   }

   const struct spirv_to_nir_options spirv_options = {
      .environment = NIR_SPIRV_OPENCL,
      .constant_as_global = false,
      .clc_shader = ctx->libclc_nir,
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
         .literal_sampler = true,
         .printf = true,
      },
   };
   nir_shader_compiler_options nir_options =
      *dxil_get_nir_compiler_options();
   nir_options.has_cs_global_work_offsets = conf && conf->support_global_work_id_offsets;
   nir_options.has_cs_work_group_offsets = conf && conf->support_work_group_id_offsets;
   nir_options.lower_cs_global_id_from_local = nir_options.has_cs_work_group_offsets;

   if (conf && conf->lower_bit_size & 64) {
      nir_options.lower_pack_64_2x32_split = false;
      nir_options.lower_unpack_64_2x32_split = false;
      nir_options.lower_int64_options = ~0;
   }

   if (conf && conf->lower_bit_size & 16)
      nir_options.support_16bit_alu = true;

   glsl_type_singleton_init_or_ref();

   nir = spirv_to_nir(obj->spvbin.data, obj->spvbin.size / 4,
                      NULL, 0,
                      MESA_SHADER_KERNEL, entrypoint,
                      &spirv_options,
                      &nir_options,
                      false);
   if (!nir) {
      clc_error(logger, "spirv_to_nir() failed");
      goto err_free_dxil;
   }
   nir->info.cs.local_size_variable = true;

   NIR_PASS_V(nir, nir_lower_goto_ifs);
   NIR_PASS_V(nir, nir_opt_dead_cf);

   // Before removing dead uniforms, dedupe constant samplers to make more dead uniforms
   NIR_PASS_V(nir, clc_nir_dedupe_const_samplers);
   NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_uniform | nir_var_mem_ubo);

   struct clc_dxil_metadata *metadata = &dxil->metadata;

   metadata->args = calloc(dxil->kernel->num_args,
                           sizeof(*metadata->args));
   if (!metadata->args) {
      clc_error(logger, "failed to allocate arg positions");
      goto err_free_dxil;
   }

   // Calculate input offsets/metadata.
   unsigned i = 0, uav_id = 0, sampler_id = 0, offset = 0;
   dxil_wrap_sampler_state int_sampler_states[PIPE_MAX_SHADER_SAMPLER_VIEWS] = {{{0}}};
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
      } else if (glsl_type_is_sampler(var->type)) {
         unsigned address_mode = conf ? conf->args[i].sampler.addressing_mode : 0u;
         int_sampler_states[sampler_id].wrap_r =
            int_sampler_states[sampler_id].wrap_s =
            int_sampler_states[sampler_id].wrap_t = wrap_from_cl_addressing(address_mode);
         int_sampler_states[sampler_id].is_nonnormalized_coords =
            conf ? !conf->args[i].sampler.normalized_coords : 0;
         int_sampler_states[sampler_id].is_linear_filtering =
            conf ? conf->args[i].sampler.linear_filtering : 0;
         metadata->args[i].sampler.sampler_id = var->data.binding = sampler_id++;
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

   // Assign bindings for constant samplers
   nir_foreach_variable_safe(var, &nir->uniforms) {
      if (glsl_type_is_sampler(var->type) &&
          var->constant_initializer) {
         int_sampler_states[sampler_id].wrap_r =
            int_sampler_states[sampler_id].wrap_s =
            int_sampler_states[sampler_id].wrap_t =
            wrap_from_cl_addressing(var->constant_initializer->values[0].u32);
         int_sampler_states[sampler_id].is_nonnormalized_coords =
            !var->constant_initializer->values[1].u32;
         int_sampler_states[sampler_id].is_linear_filtering =
            var->constant_initializer->values[2].u32 == FILTER_MODE_LINEAR;
         var->data.binding = sampler_id++;
      }
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

   // Lower memcpy
   NIR_PASS_V(nir, dxil_nir_lower_memcpy_deref);

   bool has_printf = false;
   NIR_PASS(has_printf, nir, clc_nir_lower_printf, uav_id);
   metadata->printf_uav_id = has_printf ? uav_id++ : -1;

   // Needs to come before lower_explicit_io
   struct clc_image_lower_context image_lower_context = { metadata, &srv_id, &uav_id };
   NIR_PASS_V(nir, clc_lower_images, &image_lower_context);
   NIR_PASS_V(nir, clc_lower_nonnormalized_samplers, int_sampler_states);
   NIR_PASS_V(nir, nir_lower_samplers);
   NIR_PASS_V(nir, dxil_lower_sample_to_txf_for_integer_tex,
              int_sampler_states, NULL);

   // copy propagate to prepare for lower_explicit_io
   NIR_PASS_V(nir, nir_split_var_copies);
   NIR_PASS_V(nir, nir_opt_copy_prop_vars);
   NIR_PASS_V(nir, nir_lower_var_copies);
   NIR_PASS_V(nir, nir_lower_vars_to_ssa);
   NIR_PASS_V(nir, nir_lower_alu);
   NIR_PASS_V(nir, nir_opt_dce);
   NIR_PASS_V(nir, split_unaligned_loads_stores);

   NIR_PASS_V(nir, dxil_nir_lower_ubo_to_temp);
   NIR_PASS_V(nir, clc_lower_ubo_to_ssbo, dxil->kernel, &uav_id);
   NIR_PASS_V(nir, clc_lower_global_to_ssbo);
   NIR_PASS_V(nir, dxil_nir_lower_deref_ssbo);

   assert(nir->scratch_size == 0);
   NIR_PASS_V(nir, nir_lower_vars_to_explicit_types,
              nir_var_mem_shared | nir_var_function_temp,
              glsl_get_cl_type_size_align);

   assert(nir->info.cs.ptr_size == 64);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_mem_ssbo,
              nir_address_format_32bit_index_offset_pack64);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_shader_in,
              nir_address_format_32bit_global);
   NIR_PASS_V(nir, nir_lower_explicit_io,
              nir_var_mem_shared | nir_var_function_temp,
              nir_address_format_32bit_offset_as_64bit);

   NIR_PASS_V(nir, nir_lower_system_values);
   NIR_PASS_V(nir, clc_lower_64bit_semantics);

   NIR_PASS_V(nir, nir_opt_deref);
   NIR_PASS_V(nir, nir_lower_vars_to_ssa);

   unsigned cbv_id = 0;

   nir_variable *inputs_var =
      add_kernel_inputs_var(dxil, nir, &cbv_id);
   nir_variable *work_properties_var =
      add_work_properties_var(dxil, nir, &cbv_id);

   // Patch the localsize before calling clc_nir_lower_system_values().
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

   NIR_PASS_V(nir, clc_nir_lower_kernel_input_loads, inputs_var);
   NIR_PASS_V(nir, split_unaligned_loads_stores);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_mem_ubo,
              nir_address_format_32bit_index_offset);
   NIR_PASS_V(nir, clc_nir_lower_system_values, work_properties_var);
   NIR_PASS_V(nir, dxil_nir_lower_loads_stores_to_dxil);
   NIR_PASS_V(nir, dxil_nir_opt_alu_deref_srcs);
   NIR_PASS_V(nir, dxil_nir_lower_atomics_to_dxil);

   // Convert pack to pack_split
   NIR_PASS_V(nir, nir_lower_pack);
   // Lower pack_split to bit math
   NIR_PASS_V(nir, nir_opt_algebraic);

   NIR_PASS_V(nir, nir_opt_dce);

   nir_validate_shader(nir, "Validate before feeding NIR to the DXIL compiler");
   struct nir_to_dxil_options opts = {
      .interpolate_at_vertex = false,
      .lower_int16 = (conf && (conf->lower_bit_size & 16) != 0),
      .ubo_binding_offset = 0
   };

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

   metadata->local_mem_size = nir->info.cs.shared_size;
   metadata->priv_mem_size = nir->scratch_size;

   /* DXIL double math is too limited compared to what NIR expects. Let's refuse
    * to compile a shader when it contains double operations until we have
    * double lowering hooked up.
    */
   if (shader_has_double(nir)) {
      clc_error(logger, "NIR shader contains doubles, which we don't support yet");
      goto err_free_dxil;
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

            copy_const_initializer(var->constant_initializer, var->type, data);
            metadata->consts[metadata->num_consts].data = data;
            metadata->consts[metadata->num_consts].size = size;
            metadata->consts[metadata->num_consts].uav_id = var->data.binding;
            metadata->num_consts++;
         } else
            unreachable("unexpected constant initializer");
      } else if (var->data.mode == nir_var_uniform &&
                 glsl_type_is_sampler(var->type) &&
                 var->constant_initializer) {
         assert(metadata->num_const_samplers < CLC_MAX_SAMPLERS);
         metadata->const_samplers[metadata->num_const_samplers].sampler_id = var->data.binding;
         metadata->const_samplers[metadata->num_const_samplers].addressing_mode = var->constant_initializer->values[0].u32;
         metadata->const_samplers[metadata->num_const_samplers].normalized_coords = var->constant_initializer->values[1].u32;
         metadata->const_samplers[metadata->num_const_samplers].filter_mode = var->constant_initializer->values[2].u32;
         metadata->num_const_samplers++;
      }
   }

   metadata->kernel_inputs_cbv_id = inputs_var ? inputs_var->data.binding : 0;
   metadata->work_properties_cbv_id = work_properties_var->data.binding;
   metadata->num_uavs = uav_id;
   metadata->num_srvs = srv_id;
   metadata->num_samplers = sampler_id;

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
