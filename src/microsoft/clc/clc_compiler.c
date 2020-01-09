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
#include "../compiler/dxil_module.h"
#include "../compiler/dxil_container.h"
#include "../compiler/dxil.h"

#include "util/u_debug.h"
#include "nir/nir_builder.h"

#include "git_sha1.h"

#include <stdint.h>

static bool
emit_llvm_ident(struct dxil_module *m)
{
   const struct dxil_mdnode *compiler = dxil_get_metadata_string(m, "Mesa version " PACKAGE_VERSION MESA_GIT_SHA1);
   if (!compiler)
      return false;

   const struct dxil_mdnode *llvm_ident = dxil_get_metadata_node(m, &compiler, 1);
   return llvm_ident &&
          dxil_add_metadata_named_node(m, "llvm.ident", &llvm_ident, 1);
}

static bool
emit_dx_versions(struct dxil_module *m, int major, int minor)
{
   const struct dxil_mdnode *major_node = dxil_get_metadata_int32(m, major);
   const struct dxil_mdnode *minor_node = dxil_get_metadata_int32(m, minor);
   const struct dxil_mdnode *version_nodes[] = { major_node, minor_node };
   const struct dxil_mdnode *version = dxil_get_metadata_node(m, version_nodes,
                                                     ARRAY_SIZE(version_nodes));
   return dxil_add_metadata_named_node(m, "dx.version", &version, 1) &&
          dxil_add_metadata_named_node(m, "dx.valver", &version, 1);
}

static const char *
get_shader_kind_str(enum dxil_shader_kind kind)
{
   switch (kind) {
   case DXIL_PIXEL_SHADER:
      return "ps";
   case DXIL_VERTEX_SHADER:
      return "vs";
   case DXIL_GEOMETRY_SHADER:
      return "gs";
   case DXIL_HULL_SHADER:
      return "hs";
   case DXIL_DOMAIN_SHADER:
      return "ds";
   case DXIL_COMPUTE_SHADER:
      return "cs";
   default:
      unreachable("invalid shader kind");
   }
}

static bool
emit_dx_shader_model(struct dxil_module *m)
{
   const struct dxil_mdnode *type_node = dxil_get_metadata_string(m, get_shader_kind_str(m->shader_kind));
   const struct dxil_mdnode *major_node = dxil_get_metadata_int32(m, m->major_version);
   const struct dxil_mdnode *minor_node = dxil_get_metadata_int32(m, m->minor_version);
   const struct dxil_mdnode *shader_model[] = { type_node, major_node,
                                                minor_node };
   const struct dxil_mdnode *dx_shader_model = dxil_get_metadata_node(m, shader_model, ARRAY_SIZE(shader_model));

   return dxil_add_metadata_named_node(m, "dx.shaderModel",
                                       &dx_shader_model, 1);
}

enum dxil_component_type {
   DXIL_COMP_TYPE_INVALID = 0,
   DXIL_COMP_TYPE_I1 = 1,
   DXIL_COMP_TYPE_I16 = 2,
   DXIL_COMP_TYPE_U16 = 3,
   DXIL_COMP_TYPE_I32 = 4,
   DXIL_COMP_TYPE_U32 = 5,
   DXIL_COMP_TYPE_I64 = 6,
   DXIL_COMP_TYPE_U64 = 7,
   DXIL_COMP_TYPE_F16 = 8,
   DXIL_COMP_TYPE_F32 = 9,
   DXIL_COMP_TYPE_F64 = 10,
   DXIL_COMP_TYPE_SNORMF16 = 11,
   DXIL_COMP_TYPE_UNORMF16 = 12,
   DXIL_COMP_TYPE_SNORMF32 = 13,
   DXIL_COMP_TYPE_UNORMF32 = 14,
   DXIL_COMP_TYPE_SNORMF64 = 15,
   DXIL_COMP_TYPE_UNORMF64 = 16
};

enum {
   DXIL_TYPED_BUFFER_ELEMENT_TYPE_TAG = 0,
   DXIL_STRUCTURED_BUFFER_ELEMENT_STRIDE_TAG = 1
};

static const struct dxil_mdnode *
emit_uav_metadata(struct dxil_module *m, const struct dxil_type *struct_type,
                  const char *name, enum dxil_component_type comp_type)
{
   const struct dxil_type *pointer_type = dxil_module_get_pointer_type(m, struct_type);
   const struct dxil_value *pointer_undef = dxil_module_get_undef(m, pointer_type);

   const struct dxil_mdnode *buffer_element_type_tag = dxil_get_metadata_int32(m, DXIL_TYPED_BUFFER_ELEMENT_TYPE_TAG);
   const struct dxil_mdnode *element_type = dxil_get_metadata_int32(m, comp_type);
   const struct dxil_mdnode *metadata_tag_nodes[] = {
      buffer_element_type_tag, element_type
   };
   const struct dxil_mdnode *metadata_tags = dxil_get_metadata_node(m, metadata_tag_nodes, ARRAY_SIZE(metadata_tag_nodes));

   const struct dxil_mdnode *global_constant_symbol = dxil_get_metadata_value(m, pointer_type, pointer_undef);
   const struct dxil_mdnode *name_node = dxil_get_metadata_string(m, name);
   const struct dxil_mdnode *resource_id = dxil_get_metadata_int32(m, 0);
   const struct dxil_mdnode *bind_id = dxil_get_metadata_int32(m, 0);
   const struct dxil_mdnode *bind_lower_bound = dxil_get_metadata_int32(m, 0);
   const struct dxil_mdnode *bind_range = dxil_get_metadata_int32(m, 1);
   const struct dxil_mdnode *uav_resource_shape = dxil_get_metadata_int32(m, 10);
   const struct dxil_mdnode *globally_coherent = dxil_get_metadata_int1(m, false);
   const struct dxil_mdnode *has_counter = dxil_get_metadata_int1(m, false);
   const struct dxil_mdnode *is_rov = dxil_get_metadata_int1(m, false);
   const struct dxil_mdnode *fields[] = {
      resource_id, // for createHandle
      global_constant_symbol,
      name_node,
      bind_id,
      bind_lower_bound,
      bind_range,
      uav_resource_shape,
      globally_coherent,
      has_counter,
      is_rov,
      metadata_tags
   };
   return dxil_get_metadata_node(m, fields, ARRAY_SIZE(fields));
}

static const struct dxil_type *
get_dx_handle_type(struct dxil_module *m)
{
   const struct dxil_type *int8_type = dxil_module_get_int_type(m, 8);
   if (!int8_type)
      return NULL;

   const struct dxil_type *ptr_type = dxil_module_get_pointer_type(m, int8_type);
   if (!ptr_type)
      return NULL;

   return dxil_module_get_struct_type(m, "dx.types.Handle", &ptr_type, 1);
}

static const struct dxil_type *
get_glsl_basetype(struct dxil_module *m, enum glsl_base_type type)
{
   switch (type) {
   case GLSL_TYPE_BOOL:
      return dxil_module_get_int_type(m, 1);

   case GLSL_TYPE_UINT:
   case GLSL_TYPE_INT:
      return dxil_module_get_int_type(m, 32);

   default:
      debug_printf("type: %s\n", glsl_get_type_name(glsl_scalar_type(type)));
      unreachable("unexpected GLSL type");
   }
}

static const struct dxil_type *
get_glsl_type(struct dxil_module *m, const struct glsl_type *type)
{
   assert(type);

   if (!glsl_type_is_scalar(type)) {
      debug_printf("type: %s\n", glsl_get_type_name(type));
      unreachable("unexpected glsl type");
   }

   return get_glsl_basetype(m, glsl_get_base_type(type));

}

static enum dxil_component_type
get_comp_type(const struct glsl_type *type)
{
   switch (glsl_get_base_type(type)) {
   case GLSL_TYPE_UINT: return DXIL_COMP_TYPE_U32;
   case GLSL_TYPE_INT: return DXIL_COMP_TYPE_I32;
   case GLSL_TYPE_FLOAT: return DXIL_COMP_TYPE_F32;
   case GLSL_TYPE_FLOAT16: return DXIL_COMP_TYPE_F16;
   case GLSL_TYPE_DOUBLE: return DXIL_COMP_TYPE_F64;
   case GLSL_TYPE_UINT16: return DXIL_COMP_TYPE_U16;
   case GLSL_TYPE_INT16: return DXIL_COMP_TYPE_I16;
   case GLSL_TYPE_UINT64: return DXIL_COMP_TYPE_U64;
   case GLSL_TYPE_INT64: return DXIL_COMP_TYPE_I64;
   case GLSL_TYPE_BOOL: return DXIL_COMP_TYPE_I1;

   default:
      debug_printf("type: %s\n", glsl_get_type_name(type));
      unreachable("unexpected glsl type");
   }
}

#define MAX_UAVS 64

struct dxil_def {
   const struct dxil_value *chans[NIR_MAX_VEC_COMPONENTS];
};

struct ntd_context {
   struct dxil_module mod;

   const struct dxil_mdnode *uav_metadata_nodes[MAX_UAVS];
   const struct dxil_value *uav_handles[MAX_UAVS];
   unsigned num_uavs;

   struct dxil_def *defs;
   unsigned num_defs;

   const struct dxil_func *threadid_func,
                          *bufferstore_func,
                          *createhandle_func;
};

static const struct dxil_value *
emit_threadid_call(struct ntd_context *ctx, const struct dxil_value *comp)
{
   if (!ctx->threadid_func) {
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      if (!int32_type)
         return NULL;

      const struct dxil_type *arg_types[] = {
         int32_type,
         int32_type
      };

      const struct dxil_type *func_type =
         dxil_module_add_function_type(&ctx->mod, int32_type,
                                       arg_types, ARRAY_SIZE(arg_types));
      if (!func_type)
         return NULL;

      ctx->threadid_func = dxil_add_function_decl(&ctx->mod,
         "dx.op.threadId.i32", func_type, DXIL_ATTR_KIND_READ_NONE);
      if (!ctx->threadid_func)
         return NULL;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, 93);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, ctx->threadid_func, args, ARRAY_SIZE(args));
}

static bool
emit_bufferstore_call(struct ntd_context *ctx,
                      const struct dxil_value *handle,
                      const struct dxil_value *coord[2],
                      const struct dxil_value *value[4],
                      const struct dxil_value *write_mask)
{
   if (!ctx->bufferstore_func) {
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      const struct dxil_type *int8_type = dxil_module_get_int_type(&ctx->mod, 8);
      const struct dxil_type *handle_type = get_dx_handle_type(&ctx->mod);
      const struct dxil_type *void_type = dxil_module_get_void_type(&ctx->mod);
      if (!int32_type || !int8_type || !handle_type || !void_type)
         return false;

      const struct dxil_type *arg_types[] = {
         int32_type,
         handle_type,
         int32_type,
         int32_type,
         int32_type,
         int32_type,
         int32_type,
         int32_type,
         int8_type
      };

      const struct dxil_type *func_type =
         dxil_module_add_function_type(&ctx->mod, void_type,
                                       arg_types, ARRAY_SIZE(arg_types));
      if (!func_type)
         return false;

      ctx->bufferstore_func = dxil_add_function_decl(&ctx->mod,
                                                     "dx.op.bufferStore.i32",
                                                     func_type,
                                                     DXIL_ATTR_KIND_NONE);
      if (!ctx->bufferstore_func)
         return false;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, 69);
   const struct dxil_value *args[] = {
      opcode, handle, coord[0], coord[1],
      value[0], value[1], value[2], value[3],
      write_mask
   };

   return dxil_emit_call_void(&ctx->mod, ctx->bufferstore_func,
                              args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_createhandle_call(struct ntd_context *ctx,
                       const struct dxil_value *resource_class,
                       const struct dxil_value *resource_range_id,
                       const struct dxil_value *resource_range_index,
                       const struct dxil_value *non_uniform_resource_index)
{
   if (!ctx->createhandle_func) {
      const struct dxil_type *int1_type = dxil_module_get_int_type(&ctx->mod, 1);
      const struct dxil_type *int8_type = dxil_module_get_int_type(&ctx->mod, 8);
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      const struct dxil_type *handle_type = get_dx_handle_type(&ctx->mod);
      if (!int1_type || !int8_type || !int32_type || !handle_type)
         return NULL;

      const struct dxil_type *arg_types[] = {
         int32_type,
         int8_type,
         int32_type,
         int32_type,
         int1_type
      };

      const struct dxil_type *func_type =
         dxil_module_add_function_type(&ctx->mod, handle_type, arg_types,
                                       ARRAY_SIZE(arg_types));
      if (!func_type)
         return NULL;

      ctx->createhandle_func = dxil_add_function_decl(&ctx->mod,
                                                      "dx.op.createHandle",
                                                      func_type,
                                                      DXIL_ATTR_KIND_READ_ONLY);
      if (!ctx->createhandle_func)
         return NULL;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, 57);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
      opcode,
      resource_class,
      resource_range_id,
      resource_range_index,
      non_uniform_resource_index
   };

   return dxil_emit_call(&ctx->mod, ctx->createhandle_func,
                         args, ARRAY_SIZE(args));
}

static bool
emit_uav(struct ntd_context *ctx, nir_variable *var)
{
   assert(ctx->num_uavs < MAX_UAVS);

   const struct dxil_type *ssbo_type = get_glsl_type(&ctx->mod, var->type);
   if (!ssbo_type)
      return false;

   const struct dxil_type *ssbo_struct_type = dxil_module_get_struct_type(&ctx->mod, NULL, &ssbo_type, 1);
   if (!ssbo_struct_type)
      return false;

   const struct dxil_gvar *ssbo_gvar = dxil_add_global_var(&ctx->mod, ssbo_struct_type, true, 3);
   if (!ssbo_gvar)
      return false;

   enum dxil_component_type comp_type = get_comp_type(var->type);
   const struct dxil_mdnode *uav_meta = emit_uav_metadata(&ctx->mod, ssbo_struct_type,
                                                          var->name, comp_type);

   if (!uav_meta)
      return false;

   ctx->uav_metadata_nodes[ctx->num_uavs] = uav_meta;

   const struct dxil_value *int8_1 = dxil_module_get_int8_const(&ctx->mod, 1);
   const struct dxil_value *int32_0 = dxil_module_get_int32_const(&ctx->mod, 0);
   const struct dxil_value *int1_0 = dxil_module_get_int1_const(&ctx->mod, false);
   if (!int8_1 || !int32_0 || !int1_0)
      return false;

   const struct dxil_value *handle = emit_createhandle_call(ctx, int8_1, int32_0, int32_0, int1_0);
   if (!handle)
      return false;

   ctx->uav_handles[ctx->num_uavs] = handle;
   ctx->num_uavs++;

   return true;
}

static bool
emit_metadata(struct ntd_context *ctx)
{
   if (!emit_llvm_ident(&ctx->mod) ||
       !emit_dx_versions(&ctx->mod, 1, 0) ||
       !emit_dx_shader_model(&ctx->mod))
      return false;

   const struct dxil_type *void_type = dxil_module_get_void_type(&ctx->mod);
   const struct dxil_type *main_func_type = dxil_module_add_function_type(&ctx->mod, void_type, NULL, 0);
   const struct dxil_func *main_func = dxil_add_function_def(&ctx->mod, "main", main_func_type);
   if (!main_func)
      return false;

   const struct dxil_mdnode *uav_metadata = dxil_get_metadata_node(&ctx->mod, ctx->uav_metadata_nodes, ctx->num_uavs);
   const struct dxil_mdnode *resources_nodes[] = {
      NULL, uav_metadata, NULL, NULL
   };
   const struct dxil_mdnode *resources_node = dxil_get_metadata_node(&ctx->mod, resources_nodes,
                                                      ARRAY_SIZE(resources_nodes));

   const struct dxil_mdnode *main_entrypoint = dxil_get_metadata_func(&ctx->mod, main_func);
   const struct dxil_mdnode *node27 = dxil_get_metadata_node(&ctx->mod, NULL, 0);

   const struct dxil_mdnode *node4 = dxil_get_metadata_int32(&ctx->mod, 0);
   const struct dxil_mdnode *nodes_4_27_27[] = {
      node4, node27, node27
   };
   const struct dxil_mdnode *node28 = dxil_get_metadata_node(&ctx->mod, nodes_4_27_27,
                                                      ARRAY_SIZE(nodes_4_27_27));

   const struct dxil_mdnode *node29 = dxil_get_metadata_node(&ctx->mod, &node28, 1);

   const struct dxil_mdnode *node3 = dxil_get_metadata_int32(&ctx->mod, 1);
   const struct dxil_mdnode *main_type_annotation_nodes[] = {
      node3, main_entrypoint, node29
   };
   const struct dxil_mdnode *main_type_annotation = dxil_get_metadata_node(&ctx->mod, main_type_annotation_nodes,
                                                                           ARRAY_SIZE(main_type_annotation_nodes));

   const struct dxil_mdnode *main_name = dxil_get_metadata_string(&ctx->mod, "main");

   const struct dxil_mdnode *nodes_3_3_3[] = { node3, node3, node3 };
   const struct dxil_mdnode *node32 = dxil_get_metadata_node(&ctx->mod, nodes_3_3_3,
                                                      ARRAY_SIZE(nodes_3_3_3));

   const struct dxil_mdnode *node19 = dxil_get_metadata_int32(&ctx->mod, 4);
   const struct dxil_mdnode *nodes_19_32[] = { node19, node32 };
   const struct dxil_mdnode *node33 = dxil_get_metadata_node(&ctx->mod, nodes_19_32,
                                                      ARRAY_SIZE(nodes_19_32));
   const struct dxil_mdnode *main_entrypoint_metadata[] = {
      main_entrypoint,
      main_name,
      NULL, /* list of signatures */
      resources_node, /* list of resources */
      node33 /* list of caps and other properties */
   };
   const struct dxil_mdnode *dx_resources = resources_node,
                     *dx_type_annotations[] = { main_type_annotation },
                     *dx_entry_point = dxil_get_metadata_node(&ctx->mod, main_entrypoint_metadata,
                                                              ARRAY_SIZE(main_entrypoint_metadata));

   return dxil_add_metadata_named_node(&ctx->mod, "dx.resources",
                                       &dx_resources, 1) &&
          dxil_add_metadata_named_node(&ctx->mod, "dx.typeAnnotations",
                                       dx_type_annotations,
                                       ARRAY_SIZE(dx_type_annotations)) &&
          dxil_add_metadata_named_node(&ctx->mod, "dx.entryPoints",
                                       &dx_entry_point, 1);
}

static void
store_ssa_def(struct ntd_context *ctx, nir_ssa_def *ssa, unsigned chan,
              const struct dxil_value *value)
{
   assert(ssa->index < ctx->num_defs);
   assert(chan < NIR_MAX_VEC_COMPONENTS);
   ctx->defs[ssa->index].chans[chan] = value;
}

static void
store_dest(struct ntd_context *ctx, nir_dest *dest, unsigned chan,
           const struct dxil_value *value)
{
   assert(dest->is_ssa);
   assert(value);
   store_ssa_def(ctx, &dest->ssa, chan, value);
}

static void
store_alu_dest(struct ntd_context *ctx, nir_alu_instr *alu, unsigned chan,
               const struct dxil_value *value)
{
   assert(!alu->dest.saturate);
   store_dest(ctx, &alu->dest.dest, chan, value);
}

static const struct dxil_value *
get_src_ssa(struct ntd_context *ctx, const nir_ssa_def *ssa, unsigned chan)
{
   assert(ssa->index < ctx->num_defs);
   assert(ctx->defs[ssa->index].chans[chan]);
   return ctx->defs[ssa->index].chans[chan];
}

static const struct dxil_value *
get_src(struct ntd_context *ctx, nir_src *src, unsigned chan)
{
   assert(src->is_ssa);
   return get_src_ssa(ctx, src->ssa, chan);
}

static const struct dxil_value *
get_alu_src(struct ntd_context *ctx, nir_alu_instr *alu, unsigned src)
{
   assert(!alu->src[src].abs);
   assert(!alu->src[src].negate);

   assert(util_is_power_of_two_or_zero(alu->dest.write_mask));
   unsigned chan = ffs(alu->dest.write_mask) - 1;

   return get_src(ctx, &alu->src[src].src, chan);
}

static void
emit_alu(struct ntd_context *ctx, nir_alu_instr *alu)
{
   /* handle vec-instructions first; they are the only ones that can have
      non-power-of-two write-masks */
   switch (alu->op) {
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
      for (int i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
         store_alu_dest(ctx, alu, i, get_src(ctx, &alu->src[i].src, 0));
      return;
   }

   const struct dxil_value *src[4];
   assert(nir_op_infos[alu->op].num_inputs <= 4);
   for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
      src[i] = get_alu_src(ctx, alu, i);

   switch (alu->op) {
   case nir_op_mov:
      assert(nir_dest_num_components(alu->dest.dest) == 1);
      store_alu_dest(ctx, alu, 0, src[0]);
      break;

   case nir_op_vec4:
      for (int i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
         store_alu_dest(ctx, alu, i, src[i]);
      break;

   default:
      fprintf(stderr, "emit_alu: not implemented (%s)\n",
              nir_op_infos[alu->op].name);
      unreachable("unsupported opcode");
      return;
   }
}

static void
emit_load_local_invocation_id(struct ntd_context *ctx,
                              nir_intrinsic_instr *intr)
{
   for (int i = 0; i < nir_intrinsic_dest_components(intr); i++) {
      const struct dxil_value
         *idx = dxil_module_get_int32_const(&ctx->mod, i),
         *threadid = emit_threadid_call(ctx, idx);
      store_dest(ctx, &intr->dest, i, threadid);
   }
}

static void
emit_store_ssbo(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_type *int32_type =
      dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_value *int32_undef =
      dxil_module_get_undef(&ctx->mod, int32_type);

   const struct dxil_value *coord[2] = {
      get_src(ctx, &intr->src[2], 0),
      int32_undef
   };
   const struct dxil_value *value[4] = {
      get_src(ctx, &intr->src[0], 0),
      get_src(ctx, &intr->src[0], 1),
      get_src(ctx, &intr->src[0], 2),
      get_src(ctx, &intr->src[0], 3)
   };

   const struct dxil_value *write_mask =
      dxil_module_get_int8_const(&ctx->mod, nir_intrinsic_write_mask(intr));

   nir_const_value *const_ssbo = nir_src_as_const_value(intr->src[1]);
   if (const_ssbo) {
      unsigned idx = const_ssbo->u32;
      assert(ctx->num_uavs > idx);
      emit_bufferstore_call(ctx, ctx->uav_handles[idx], coord, value, write_mask);
   } else
      unreachable("dynamic ssbo addressing not implemented");
}

static void
emit_intrinsic(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   switch (intr->intrinsic) {
   case nir_intrinsic_load_local_invocation_id:
      emit_load_local_invocation_id(ctx, intr);
      // unreachable("nir_intrinsic_load_local_invocation_id not implemented");
      break;

   case nir_intrinsic_store_ssbo:
      emit_store_ssbo(ctx, intr);
      break;

   default:
      fprintf(stderr, "emit_intrinsic: not implemented (%s)\n",
              nir_intrinsic_infos[intr->intrinsic].name);
      unreachable("unsupported intrinsic");
   }
}

static void
emit_load_const(struct ntd_context *ctx, nir_load_const_instr *load_const)
{
   assert(load_const->def.bit_size == 32);
   assert(load_const->def.num_components == 1);

   const struct dxil_value
      *value = dxil_module_get_int32_const(&ctx->mod,
                                           load_const->value[0].u32);
   store_ssa_def(ctx, &load_const->def, 0, value);
}

static void
emit_block(struct ntd_context *ctx, struct nir_block *block)
{
   nir_foreach_instr(instr, block) {
      switch (instr->type) {
      case nir_instr_type_alu:
         emit_alu(ctx, nir_instr_as_alu(instr));
         break;
      case nir_instr_type_intrinsic:
         emit_intrinsic(ctx, nir_instr_as_intrinsic(instr));
         break;
      case nir_instr_type_load_const:
         emit_load_const(ctx, nir_instr_as_load_const(instr));
         break;
      default:
         unreachable("unsupported instruction type");
      }
   }
}

static void
emit_cf_list(struct ntd_context *ctx, struct exec_list *list)
{
   foreach_list_typed(nir_cf_node, node, node, list) {
      switch (node->type) {
      case nir_cf_node_block:
         emit_block(ctx, nir_cf_node_as_block(node));
         break;

      default:
         unreachable("unsupported cf-list node");
         break;
      }
   }
}

static bool
emit_module(struct ntd_context *ctx, nir_shader *s)
{
   nir_foreach_variable(var, &s->uniforms) {
      switch (var->data.mode) {
      case nir_var_mem_ssbo:
         if (!var->interface_type) {
            /* this is an SSBO, emit as UAV */
            if (!emit_uav(ctx, var))
               return false;
         }
         break;
      }
   }

   nir_function_impl *entry = nir_shader_get_entrypoint(s);
   nir_metadata_require(entry, nir_metadata_block_index);

   ctx->defs = malloc(sizeof(struct dxil_def) * entry->ssa_alloc);
   if (!ctx->defs)
      return false;
   ctx->num_defs = entry->ssa_alloc;

   emit_cf_list(ctx, &entry->body);

   if (!dxil_emit_ret_void(&ctx->mod))
      return false;

   free(ctx->defs);

   return emit_metadata(ctx) &&
          dxil_emit_module(&ctx->mod);
}

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

   struct dxil_container container;
   dxil_container_init(&container);

   struct dxil_features features = { 0 };
   if (!dxil_container_add_features(&container, &features)) {
      debug_printf("D3D12: dxil_container_add_features failed\n");
      return -1;
   }

   if (!dxil_container_add_input_signature(&container) ||
       !dxil_container_add_output_signature(&container)) {
      debug_printf("D3D12: failed to write input/output signature\n");
      return -1;
   }

   const struct dxil_resource resources[] = {
      { DXIL_RES_UAV_TYPED, 0, 0, 0 }
   };
   if (!dxil_container_add_state_validation(&container, resources,
                                            ARRAY_SIZE(resources))) {
      debug_printf("D3D12: failed to write state-validation\n");
      return -1;
   }

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

   struct ntd_context ctx = { 0 };
   dxil_module_init(&ctx.mod);
   ctx.mod.shader_kind = DXIL_COMPUTE_SHADER;
   ctx.mod.major_version = 6;
   ctx.mod.minor_version = 0;
   if (!emit_module(&ctx, b.shader)) {
      debug_printf("D3D12: dxil_container_add_module failed\n");
      return -1;
   }

   if (!dxil_container_add_module(&container, &ctx.mod)) {
      debug_printf("D3D12: failed to write module\n");
      return -1;
   }

   struct blob tmp;
   blob_init(&tmp);
   if (!dxil_container_write(&container, &tmp)) {
      debug_printf("D3D12: dxil_container_write failed\n");
      return -1;
   }

   blob_finish_get_buffer(&tmp, blob, blob_size);
   return 0;
}
