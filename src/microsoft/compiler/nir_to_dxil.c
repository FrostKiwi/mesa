/*
 * Copyright 2019-2020 Collabora Ltd.
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

#include "nir_to_dxil.h"

#include "dxil_module.h"
#include "dxil_container.h"
#include "dxil.h"

#include "util/u_debug.h"
#include "nir.h"

#include "git_sha1.h"

#include <stdint.h>

#define DEBUG_NIR_TO_DXIL

#ifdef DEBUG_NIR_TO_DXIL
#define NIR_INSTR_UNSUPPORTED(instr) \
   do { \
      fprintf(stderr, "Unsupported instruction:"); \
      nir_print_instr(instr, stderr); \
      fprintf(stderr, "\n"); \
   } while (0)
#else
#define NIR_INSTR_UNSUPPORTED(instr)
#endif


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

enum dxil_intr {
   DXIL_INTR_FRC = 22,

   DXIL_INTR_ROUND_NE = 26,
   DXIL_INTR_ROUND_NI = 27,
   DXIL_INTR_ROUND_PI = 28,
   DXIL_INTR_ROUND_Z = 29,

   DXIL_INTR_IMAX = 37,
   DXIL_INTR_IMIN = 38,
   DXIL_INTR_UMAX = 39,
   DXIL_INTR_UMIN = 40,

   DXIL_INTR_CREATE_HANDLE = 57,

   DXIL_INTR_BUFFER_STORE = 69,

   DXIL_INTR_THREAD_ID = 93,
   DXIL_INTR_GROUP_ID = 94,
   DXIL_INTR_THREAD_ID_IN_GROUP = 95,
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
   const struct dxil_mdnode *name_node = dxil_get_metadata_string(m, name ? name : "");
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

enum overload_type {
   I32,
   I64,
   F32,
   F64,
   NUM_OVERLOADS
};

static const char *overload_str[NUM_OVERLOADS] = {
   [I32] = "i32",
   [I64] = "i64",
   [F32] = "f32",
   [F64] = "f64",
};

struct ntd_context {
   struct dxil_module mod;

   const struct dxil_mdnode *uav_metadata_nodes[MAX_UAVS];
   const struct dxil_value *uav_handles[MAX_UAVS];
   unsigned num_uavs;

   struct dxil_resource resources[MAX_UAVS];
   unsigned num_resources;

   struct dxil_def *defs;
   unsigned num_defs;

   const struct dxil_func *unary_funcs[NUM_OVERLOADS],
                          *binary_funcs[NUM_OVERLOADS],
                          *threadid_func,
                          *threadidingroup_func,
                          *groupid_func,
                          *bufferstore_func,
                          *createhandle_func;
};

static const struct dxil_type *
get_overload_type(struct ntd_context *ctx, enum overload_type overload)
{
   switch (overload) {
   case I32: return dxil_module_get_int_type(&ctx->mod, 32);
   case I64: return dxil_module_get_int_type(&ctx->mod, 64);
   case F32: return dxil_module_get_float_type(&ctx->mod, 32);
   case F64: return dxil_module_get_float_type(&ctx->mod, 64);
   default:
      unreachable("unexpected overload type");
   }
}

static const struct dxil_value *
emit_unary_call(struct ntd_context *ctx, enum overload_type overload,
                enum dxil_intr intr,
                const struct dxil_value *op0)
{
   if (!ctx->unary_funcs[overload]) {
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      const struct dxil_type *type = get_overload_type(ctx, overload);
      if (!int32_type || !type)
         return NULL;

      const struct dxil_type *arg_types[] = {
         int32_type,
         type,
      };

      const struct dxil_type *func_type =
         dxil_module_add_function_type(&ctx->mod, type,
                                       arg_types, ARRAY_SIZE(arg_types));
      if (!func_type)
         return NULL;

      char name[100];
      snprintf(name, ARRAY_SIZE(name), "dx.op.unary.%s",
               overload_str[overload]);

      ctx->unary_funcs[overload] = dxil_add_function_decl(&ctx->mod,
         name, func_type, DXIL_ATTR_KIND_READ_NONE);
      if (!ctx->unary_funcs[overload])
         return NULL;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, intr);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     op0
   };

   return dxil_emit_call(&ctx->mod, ctx->unary_funcs[overload],
                         args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_binary_call(struct ntd_context *ctx, enum overload_type overload,
                 enum dxil_intr intr,
                 const struct dxil_value *op0, const struct dxil_value *op1)
{
   if (!ctx->binary_funcs[overload]) {
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      const struct dxil_type *type = get_overload_type(ctx, overload);
      if (!int32_type || !type)
         return NULL;

      const struct dxil_type *arg_types[] = {
         int32_type,
         type,
         type
      };

      const struct dxil_type *func_type =
         dxil_module_add_function_type(&ctx->mod, type,
                                       arg_types, ARRAY_SIZE(arg_types));
      if (!func_type)
         return NULL;

      char name[100];
      snprintf(name, ARRAY_SIZE(name), "dx.op.binary.%s",
               overload_str[overload]);

      ctx->binary_funcs[overload] = dxil_add_function_decl(&ctx->mod, name,
         func_type, DXIL_ATTR_KIND_READ_NONE);
      if (!ctx->binary_funcs[overload])
         return NULL;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, intr);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     op0,
     op1
   };

   return dxil_emit_call(&ctx->mod, ctx->binary_funcs[overload],
                         args, ARRAY_SIZE(args));
}

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

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
       DXIL_INTR_THREAD_ID);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, ctx->threadid_func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_threadidingroup_call(struct ntd_context *ctx,
                          const struct dxil_value *comp)
{
   if (!ctx->threadidingroup_func) {
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

      ctx->threadidingroup_func = dxil_add_function_decl(&ctx->mod,
         "dx.op.threadIdInGroup.i32", func_type, DXIL_ATTR_KIND_READ_NONE);
      if (!ctx->threadidingroup_func)
         return NULL;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
       DXIL_INTR_THREAD_ID_IN_GROUP);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, ctx->threadidingroup_func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_groupid_call(struct ntd_context *ctx, const struct dxil_value *comp)
{
   if (!ctx->groupid_func) {
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

      ctx->groupid_func = dxil_add_function_decl(&ctx->mod,
         "dx.op.groupId.i32", func_type, DXIL_ATTR_KIND_READ_NONE);
      if (!ctx->groupid_func)
         return NULL;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
       DXIL_INTR_GROUP_ID);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, ctx->groupid_func, args, ARRAY_SIZE(args));
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

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
      DXIL_INTR_BUFFER_STORE);
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

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
      DXIL_INTR_CREATE_HANDLE);
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
   assert(ctx->num_uavs < ARRAY_SIZE(ctx->uav_metadata_nodes));
   assert(ctx->num_uavs < ARRAY_SIZE(ctx->uav_handles));
   assert(ctx->num_resources < ARRAY_SIZE(ctx->resources));

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

   ctx->resources[ctx->num_resources].resource_type = DXIL_RES_UAV_TYPED;
   ctx->resources[ctx->num_resources].space = 0;
   ctx->resources[ctx->num_resources].lower_bound = 0;
   ctx->resources[ctx->num_resources].upper_bound = 0;
   ctx->num_resources++;

   const struct dxil_value *resource_class_value = dxil_module_get_int8_const(&ctx->mod, 1);
   const struct dxil_value *resource_range_id_value = dxil_module_get_int32_const(&ctx->mod, 0);
   const struct dxil_value *resource_range_index_value = dxil_module_get_int32_const(&ctx->mod, 0);
   const struct dxil_value *non_uniform_resource_index_value = dxil_module_get_int1_const(&ctx->mod, false);
   if (!resource_class_value || !resource_range_id_value ||
       !resource_range_id_value || !non_uniform_resource_index_value)
      return false;

   const struct dxil_value *handle = emit_createhandle_call(ctx,
      resource_class_value, resource_range_id_value,
      resource_range_index_value, non_uniform_resource_index_value);
   if (!handle)
      return false;

   ctx->uav_handles[ctx->num_uavs] = handle;
   ctx->num_uavs++;

   return true;
}

static const struct dxil_mdnode *
emit_threads(struct ntd_context *ctx, nir_shader *s)
{
   const struct dxil_mdnode *threads_x = dxil_get_metadata_int32(&ctx->mod, MAX2(s->info.cs.local_size[0], 1));
   const struct dxil_mdnode *threads_y = dxil_get_metadata_int32(&ctx->mod, MAX2(s->info.cs.local_size[1], 1));
   const struct dxil_mdnode *threads_z = dxil_get_metadata_int32(&ctx->mod, MAX2(s->info.cs.local_size[2], 1));
   if (!threads_x || !threads_y || !threads_z)
      return false;

   const struct dxil_mdnode *threads_nodes[] = { threads_x, threads_y, threads_z };
   return dxil_get_metadata_node(&ctx->mod, threads_nodes, ARRAY_SIZE(threads_nodes));
}

static int64_t
get_module_flags(struct ntd_context *ctx)
{
   /* See the DXIL documentation for the definition of these flags:
    *
    * https://github.com/Microsoft/DirectXShaderCompiler/blob/master/docs/DXIL.rst#shader-flags
    */

   uint64_t flags = 0;
   if (ctx->mod.feats.doubles)
      flags |= (1 << 2);
   if (ctx->mod.feats.min_precision)
      flags |= (1 << 5);
   if (ctx->mod.feats.dx11_1_double_extensions)
      flags |= (1 << 6);
   if (ctx->mod.feats.inner_coverage)
      flags |= (1 << 10);
   if (ctx->mod.feats.use_64uavs)
      flags |= (1 << 15);
   if (ctx->mod.feats.cs_4x_raw_sb)
      flags |= (1 << 17);
   if (ctx->mod.feats.wave_ops)
      flags |= (1 << 19);
   if (ctx->mod.feats.int64_ops)
      flags |= (1 << 20);

   return flags;
}

static bool
emit_metadata(struct ntd_context *ctx, nir_shader *s)
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

   const struct dxil_mdnode *shader_properties = NULL;
   if (ctx->mod.shader_kind == DXIL_COMPUTE_SHADER) {
      const struct dxil_mdnode *shader_property_nodes[4];
      size_t num_shader_property_nodes = 0;

      uint64_t flags = get_module_flags(ctx);
      if (flags != 0) {
         const struct dxil_mdnode *shader_flags_tag = dxil_get_metadata_int32(&ctx->mod, 0);
         const struct dxil_mdnode *shader_flags = dxil_get_metadata_int64(&ctx->mod, flags);
         if (!shader_flags_tag || !shader_flags)
            return false;

         assert(num_shader_property_nodes <= ARRAY_SIZE(shader_property_nodes) - 2);
         shader_property_nodes[num_shader_property_nodes++] = shader_flags_tag;
         shader_property_nodes[num_shader_property_nodes++] = shader_flags;
      }

      const struct dxil_mdnode *num_thread_tag = dxil_get_metadata_int32(&ctx->mod, 4);
      const struct dxil_mdnode *num_threads = emit_threads(ctx, s);
      if (!num_thread_tag || !num_threads)
         return false;

      assert(num_shader_property_nodes <= ARRAY_SIZE(shader_property_nodes) - 2);
      shader_property_nodes[num_shader_property_nodes++] = num_thread_tag;
      shader_property_nodes[num_shader_property_nodes++] = num_threads;

      shader_properties = dxil_get_metadata_node(&ctx->mod, shader_property_nodes,
                                                 num_shader_property_nodes);
      if (!shader_properties)
         return false;
   }

   const struct dxil_mdnode *main_entrypoint_metadata[] = {
      main_entrypoint,
      main_name,
      NULL, /* list of signatures */
      resources_node, /* list of resources */
      shader_properties /* list of caps and other properties */
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

static const struct dxil_value *
bitcast_to_int(struct ntd_context *ctx, unsigned bit_size,
               const struct dxil_value *value)
{
   const struct dxil_type *type = dxil_module_get_int_type(&ctx->mod, bit_size);
   if (!type)
      return NULL;

   return dxil_emit_cast(&ctx->mod, DXIL_CAST_BITCAST, type, value);
}

static const struct dxil_value *
bitcast_to_float(struct ntd_context *ctx, unsigned bit_size,
                 const struct dxil_value *value)
{
   const struct dxil_type *type = dxil_module_get_float_type(&ctx->mod, bit_size);
   if (!type)
      return NULL;

   return dxil_emit_cast(&ctx->mod, DXIL_CAST_BITCAST, type, value);
}

static void
store_ssa_def(struct ntd_context *ctx, nir_ssa_def *ssa, unsigned chan,
              const struct dxil_value *value)
{
   assert(ssa->index < ctx->num_defs);
   assert(chan < ssa->num_components);
   ctx->defs[ssa->index].chans[chan] = value;
}

static void
store_dest_int(struct ntd_context *ctx, nir_dest *dest, unsigned chan,
               const struct dxil_value *value)
{
   assert(dest->is_ssa);
   assert(value);
   store_ssa_def(ctx, &dest->ssa, chan, value);
}

static void
store_dest(struct ntd_context *ctx, nir_dest *dest, unsigned chan,
           const struct dxil_value *value, nir_alu_type type)
{
   switch (nir_alu_type_get_base_type(type)) {
   case nir_type_uint:
   case nir_type_int:
      assert(nir_dest_bit_size(*dest) != 1);
      if (nir_dest_bit_size(*dest) == 64)
         ctx->mod.feats.int64_ops = true;
      break;

   case nir_type_float:
      assert(nir_dest_bit_size(*dest) != 1);
      if (nir_dest_bit_size(*dest) == 64) {
         ctx->mod.feats.doubles = true;
         ctx->mod.feats.int64_ops = true;
      }
      value = bitcast_to_int(ctx, nir_dest_bit_size(*dest), value);
      break;

   case nir_type_bool:
      assert(nir_dest_bit_size(*dest) == 1);
      /* nothing to do */
      break;

   default:
      unreachable("unsupported nir_alu_type");
   }

   store_dest_int(ctx, dest, chan, value);
}

static void
store_alu_dest(struct ntd_context *ctx, nir_alu_instr *alu, unsigned chan,
               const struct dxil_value *value)
{
   assert(!alu->dest.saturate);
   store_dest(ctx, &alu->dest.dest, chan, value,
              nir_op_infos[alu->op].output_type);
}

static const struct dxil_value *
get_src_ssa(struct ntd_context *ctx, const nir_ssa_def *ssa, unsigned chan)
{
   assert(ssa->index < ctx->num_defs);
   assert(chan < ssa->num_components);
   assert(ctx->defs[ssa->index].chans[chan]);
   return ctx->defs[ssa->index].chans[chan];
}

static const struct dxil_value *
get_src(struct ntd_context *ctx, nir_src *src, unsigned chan,
        nir_alu_type type)
{
   assert(src->is_ssa);
   const struct dxil_value *value = get_src_ssa(ctx, src->ssa, chan);

   switch (nir_alu_type_get_base_type(type)) {
   case nir_type_int:
   case nir_type_uint:
      assert(nir_src_bit_size(*src) >= 32);
      assert(nir_src_bit_size(*src) != 64 || ctx->mod.feats.int64_ops);
      /* nohing to do */
      return value;

   case nir_type_float:
      assert(nir_src_bit_size(*src) >= 32);
      assert(nir_src_bit_size(*src) != 64 || (ctx->mod.feats.doubles &&
                                              ctx->mod.feats.int64_ops));
      return bitcast_to_float(ctx, nir_src_bit_size(*src), value);

   case nir_type_bool:
      assert(nir_src_bit_size(*src) == 1);
      /* nothing to do */
      return value;

   default:
      unreachable("unknown nir_alu_type");
   }
}

static const struct dxil_value *
get_alu_src(struct ntd_context *ctx, nir_alu_instr *alu, unsigned src)
{
   assert(!alu->src[src].abs);
   assert(!alu->src[src].negate);

   unsigned chan = alu->src[src].swizzle[0];
   return get_src(ctx, &alu->src[src].src, chan,
                  nir_op_infos[alu->op].input_types[src]);
}

static bool
emit_binop(struct ntd_context *ctx, nir_alu_instr *alu,
           enum dxil_bin_opcode opcode,
           const struct dxil_value *op0, const struct dxil_value *op1)
{
   const struct dxil_value *v = dxil_emit_binop(&ctx->mod, opcode, op0, op1);
   if (!v)
      return false;
   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static bool
emit_cmp(struct ntd_context *ctx, nir_alu_instr *alu,
         enum dxil_cmp_pred pred,
         const struct dxil_value *op0, const struct dxil_value *op1)
{
   const struct dxil_value *v = dxil_emit_cmp(&ctx->mod, pred, op0, op1);
   if (!v)
      return false;
   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static enum dxil_cast_opcode
get_cast_op(nir_alu_instr *alu)
{
   unsigned dst_bits = nir_dest_bit_size(alu->dest.dest);
   unsigned src_bits = nir_src_bit_size(alu->src[0].src);

   switch (alu->op) {
   /* float -> float */
   case nir_op_f2f32:
   case nir_op_f2f64:
      assert(dst_bits != src_bits);
      if (dst_bits < src_bits)
         return DXIL_CAST_FPTRUNC;
      else
         return DXIL_CAST_FPEXT;

   /* int -> int */
   case nir_op_i2i32:
   case nir_op_i2i64:
      assert(dst_bits != src_bits);
      if (dst_bits < src_bits)
         return DXIL_CAST_TRUNC;
      else
         return DXIL_CAST_SEXT;

   /* uint -> uint */
   case nir_op_u2u32:
   case nir_op_u2u64:
      assert(dst_bits != src_bits);
      if (dst_bits < src_bits)
         return DXIL_CAST_TRUNC;
      else
         return DXIL_CAST_ZEXT;

   /* float -> int */
   case nir_op_f2i32:
   case nir_op_f2i64:
      return DXIL_CAST_FPTOSI;

   /* float -> uint */
   case nir_op_f2u32:
   case nir_op_f2u64:
      return DXIL_CAST_FPTOUI;

   /* int -> float */
   case nir_op_i2f32:
   case nir_op_i2f64:
      return DXIL_CAST_SITOFP;

   /* uint -> float */
   case nir_op_u2f32:
   case nir_op_u2f64:
      return DXIL_CAST_UITOFP;

   default:
      unreachable("unexpected cast op");
   }
}

static const struct dxil_type *
get_cast_dest_type(struct ntd_context *ctx, nir_alu_instr *alu)
{
   unsigned dst_bits = nir_dest_bit_size(alu->dest.dest);
   switch (nir_alu_type_get_base_type(nir_op_infos[alu->op].output_type)) {
   case nir_type_int:
   case nir_type_uint:
      return dxil_module_get_int_type(&ctx->mod, dst_bits);

   case nir_type_float:
      return dxil_module_get_float_type(&ctx->mod, dst_bits);

   default:
      unreachable("unknown nir_alu_type");
   }
}

static bool
is_double(nir_alu_type alu_type, unsigned bit_size)
{
   return nir_alu_type_get_base_type(alu_type) == nir_type_float &&
          bit_size == 64;
}

static bool
emit_cast(struct ntd_context *ctx, nir_alu_instr *alu,
          const struct dxil_value *value)
{
   enum dxil_cast_opcode opcode = get_cast_op(alu);
   const struct dxil_type *type = get_cast_dest_type(ctx, alu);
   if (!type)
      return false;

   const nir_op_info *info = &nir_op_infos[alu->op];
   switch (opcode) {
   case DXIL_CAST_UITOFP:
   case DXIL_CAST_SITOFP:
      if (is_double(info->output_type, nir_dest_bit_size(alu->dest.dest)))
         ctx->mod.feats.dx11_1_double_extensions = true;
      break;
   case DXIL_CAST_FPTOUI:
   case DXIL_CAST_FPTOSI:
      if (is_double(info->input_types[0], nir_src_bit_size(alu->src[0].src)))
         ctx->mod.feats.dx11_1_double_extensions = true;
      break;
   }

   const struct dxil_value *v = dxil_emit_cast(&ctx->mod, opcode, type,
                                               value);
   if (!v)
      return false;
   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static enum overload_type
get_overload(nir_alu_type alu_type, unsigned bit_size)
{
   switch (nir_alu_type_get_base_type(alu_type)) {
   case nir_type_int:
   case nir_type_uint:
      switch (bit_size) {
      case 32: return I32;
      case 64: return I64;
      default:
         unreachable("unexpected bit_size");
      }
   case nir_type_float:
      switch (bit_size) {
      case 32: return F32;
      case 64: return F64;
      default:
         unreachable("unexpected bit_size");
      }
   default:
      unreachable("unexpected output type");
   }
}

static bool
emit_unary_intin(struct ntd_context *ctx, nir_alu_instr *alu,
                 enum dxil_intr intr, const struct dxil_value *op)
{
   const nir_op_info *info = &nir_op_infos[alu->op];
   assert(info->output_type == info->input_types[0]);
   unsigned dst_bits = nir_dest_bit_size(alu->dest.dest);
   assert(nir_src_bit_size(alu->src[0].src) == dst_bits);
   enum overload_type overload = get_overload(info->output_type, dst_bits);

   const struct dxil_value *v = emit_unary_call(ctx, overload, intr, op);
   if (!v)
      return false;
   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static bool
emit_binary_intin(struct ntd_context *ctx, nir_alu_instr *alu,
                  enum dxil_intr intr,
                  const struct dxil_value *op0, const struct dxil_value *op1)
{
   const nir_op_info *info = &nir_op_infos[alu->op];
   assert(info->output_type == info->input_types[0]);
   assert(info->output_type == info->input_types[1]);
   unsigned dst_bits = nir_dest_bit_size(alu->dest.dest);
   assert(nir_src_bit_size(alu->src[0].src) == dst_bits);
   assert(nir_src_bit_size(alu->src[1].src) == dst_bits);
   enum overload_type overload = get_overload(info->output_type, dst_bits);

   const struct dxil_value *v = emit_binary_call(ctx, overload, intr,
                                                 op0, op1);
   if (!v)
      return false;
   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static bool
emit_alu(struct ntd_context *ctx, nir_alu_instr *alu)
{
   /* handle vec-instructions first; they are the only ones that produce
    * vector results.
    */
   switch (alu->op) {
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
      for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
         store_alu_dest(ctx, alu, i, get_alu_src(ctx, alu, 0));
      return true;
   default:
      /* silence warnings */
      ;
   }

   /* other ops should be scalar */
   assert(alu->dest.write_mask == 1);
   const struct dxil_value *src[4];
   assert(nir_op_infos[alu->op].num_inputs <= 4);
   for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
      src[i] = get_alu_src(ctx, alu, i);

   switch (alu->op) {
   case nir_op_mov:
      assert(nir_dest_num_components(alu->dest.dest) == 1);
      store_alu_dest(ctx, alu, 0, src[0]);
      return true;

   case nir_op_iadd:
   case nir_op_fadd: return emit_binop(ctx, alu, DXIL_BINOP_ADD, src[0], src[1]);

   case nir_op_isub:
   case nir_op_fsub: return emit_binop(ctx, alu, DXIL_BINOP_SUB, src[0], src[1]);

   case nir_op_imul:
   case nir_op_fmul: return emit_binop(ctx, alu, DXIL_BINOP_MUL, src[0], src[1]);

   case nir_op_idiv:
   case nir_op_fdiv: return emit_binop(ctx, alu, DXIL_BINOP_SDIV, src[0], src[1]);

   case nir_op_udiv: return emit_binop(ctx, alu, DXIL_BINOP_UDIV, src[0], src[1]);
   case nir_op_irem: return emit_binop(ctx, alu, DXIL_BINOP_SREM, src[0], src[1]);
   case nir_op_imod: return emit_binop(ctx, alu, DXIL_BINOP_UREM, src[0], src[1]);
   case nir_op_ishl: return emit_binop(ctx, alu, DXIL_BINOP_SHL, src[0], src[1]);
   case nir_op_ishr: return emit_binop(ctx, alu, DXIL_BINOP_ASHR, src[0], src[1]);
   case nir_op_ushr: return emit_binop(ctx, alu, DXIL_BINOP_LSHR, src[0], src[1]);
   case nir_op_iand: return emit_binop(ctx, alu, DXIL_BINOP_AND, src[0], src[1]);
   case nir_op_ior:  return emit_binop(ctx, alu, DXIL_BINOP_OR, src[0], src[1]);
   case nir_op_ixor: return emit_binop(ctx, alu, DXIL_BINOP_XOR, src[0], src[1]);
   case nir_op_ine:  return emit_cmp(ctx, alu, DXIL_ICMP_NE, src[0], src[1]);
   case nir_op_bcsel: {
         const struct dxil_value *v = dxil_emit_select(&ctx->mod, src[0],
                                                       src[1], src[2]);
         if (!v)
            return false;
         store_alu_dest(ctx, alu, 0, v);
         return true;
      }
   case nir_op_ftrunc: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_Z, src[0]);
   case nir_op_fceil: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_PI, src[0]);
   case nir_op_ffloor: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_NI, src[0]);
   case nir_op_ffract: return emit_unary_intin(ctx, alu, DXIL_INTR_FRC, src[0]);
   case nir_op_fround_even: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_NE, src[0]);
   case nir_op_imax: return emit_binary_intin(ctx, alu, DXIL_INTR_IMAX, src[0], src[1]);
   case nir_op_imin: return emit_binary_intin(ctx, alu, DXIL_INTR_IMIN, src[0], src[1]);
   case nir_op_umax: return emit_binary_intin(ctx, alu, DXIL_INTR_UMAX, src[0], src[1]);
   case nir_op_umin: return emit_binary_intin(ctx, alu, DXIL_INTR_UMIN, src[0], src[1]);

   case nir_op_f2f32:
   case nir_op_f2i32:
   case nir_op_f2u32:
   case nir_op_i2f32:
   case nir_op_i2i32:
   case nir_op_u2f32:
   case nir_op_u2u32:
      return emit_cast(ctx, alu, src[0]);

   default:
      NIR_INSTR_UNSUPPORTED(&alu->instr);
      assert("Unimplemented ALU instruction");
      return false;
   }
}

static bool
emit_load_global_invocation_id(struct ntd_context *ctx,
                                    nir_intrinsic_instr *intr)
{
   assert(intr->dest.is_ssa);
   nir_component_mask_t comps = nir_ssa_def_components_read(&intr->dest.ssa);

   for (int i = 0; i < nir_intrinsic_dest_components(intr); i++) {
      if (comps & (1 << i)) {
         const struct dxil_value *idx = dxil_module_get_int32_const(&ctx->mod, i);
         if (!idx)
            return false;
         const struct dxil_value *threadid = emit_threadid_call(ctx, idx);
         if (!threadid)
            return false;
         store_dest_int(ctx, &intr->dest, i, threadid);
      }
   }
   return true;
}

static bool
emit_load_local_invocation_id(struct ntd_context *ctx,
                              nir_intrinsic_instr *intr)
{
   assert(intr->dest.is_ssa);
   nir_component_mask_t comps = nir_ssa_def_components_read(&intr->dest.ssa);

   for (int i = 0; i < nir_intrinsic_dest_components(intr); i++) {
      if (comps & (1 << i)) {
         const struct dxil_value
            *idx = dxil_module_get_int32_const(&ctx->mod, i);
         if (!idx)
            return false;
         const struct dxil_value
            *threadidingroup = emit_threadidingroup_call(ctx, idx);
         if (!threadidingroup)
            return false;
         store_dest_int(ctx, &intr->dest, i, threadidingroup);
      }
   }
   return true;
}

static bool
emit_load_local_work_group_id(struct ntd_context *ctx,
                              nir_intrinsic_instr *intr)
{
   assert(intr->dest.is_ssa);
   nir_component_mask_t comps = nir_ssa_def_components_read(&intr->dest.ssa);

   for (int i = 0; i < nir_intrinsic_dest_components(intr); i++) {
      if (comps & (1 << i)) {
         const struct dxil_value *idx = dxil_module_get_int32_const(&ctx->mod, i);
         if (idx)
            return false;
         const struct dxil_value *groupid = emit_groupid_call(ctx, idx);
         if (!groupid)
            return false;
         store_dest_int(ctx, &intr->dest, i, groupid);
      }
   }
   return true;
}

static bool
emit_store_ssbo(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_type *int32_type =
      dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_value *int32_undef =
      dxil_module_get_undef(&ctx->mod, int32_type);

   const struct dxil_value *coord[2] = {
      get_src(ctx, &intr->src[2], 0, nir_type_uint),
      int32_undef
   };
   const struct dxil_value *value[4] = {
      get_src(ctx, &intr->src[0], 0, nir_type_uint),
      get_src(ctx, &intr->src[0], 1, nir_type_uint),
      get_src(ctx, &intr->src[0], 2, nir_type_uint),
      get_src(ctx, &intr->src[0], 3, nir_type_uint)
   };

   const struct dxil_value *write_mask =
      dxil_module_get_int8_const(&ctx->mod, nir_intrinsic_write_mask(intr));

   nir_const_value *const_ssbo = nir_src_as_const_value(intr->src[1]);
   if (const_ssbo) {
      unsigned idx = const_ssbo->u32;
      assert(ctx->num_uavs > idx);
      return emit_bufferstore_call(ctx, ctx->uav_handles[idx], coord, value, write_mask);
   } else {
      assert("dynamic ssbo addressing not implemented");
      return false;
   }
}

static bool
emit_intrinsic(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   switch (intr->intrinsic) {
   case nir_intrinsic_load_global_invocation_id:
      return emit_load_global_invocation_id(ctx, intr);
   case nir_intrinsic_load_local_invocation_id:
      return emit_load_local_invocation_id(ctx, intr);
   case nir_intrinsic_load_work_group_id:
      return emit_load_local_work_group_id(ctx, intr);
   case nir_intrinsic_store_ssbo:
      return emit_store_ssbo(ctx, intr);
   case nir_intrinsic_load_num_work_groups:
   case nir_intrinsic_load_local_group_size:
   default:
      NIR_INSTR_UNSUPPORTED(&intr->instr);
      assert("Unimplemented intrinsic instruction");
      return false;
   }
}

static bool
emit_load_const(struct ntd_context *ctx, nir_load_const_instr *load_const)
{
   for (int i = 0; i < load_const->def.num_components; ++i) {
      const struct dxil_value *value;
      switch (load_const->def.bit_size) {
      case 1:
         value = dxil_module_get_int1_const(&ctx->mod,
                                            load_const->value[i].b);
         break;
      case 8:
         value = dxil_module_get_int8_const(&ctx->mod,
                                             load_const->value[i].u8);
         break;
      case 32:
         value = dxil_module_get_int32_const(&ctx->mod,
                                             load_const->value[i].u32);
         break;
      case 64:
         ctx->mod.feats.int64_ops = true;
         value = dxil_module_get_int64_const(&ctx->mod,
                                             load_const->value[i].u64);
         break;
      default:
         unreachable("unexpected bit_size");
      }
      if (!value)
         return false;

      store_ssa_def(ctx, &load_const->def, i, value);
   }
   return true;
}

static bool emit_instr(struct ntd_context *ctx, struct nir_instr* instr)
{
   switch (instr->type) {
   case nir_instr_type_alu:
      return emit_alu(ctx, nir_instr_as_alu(instr));
   case nir_instr_type_intrinsic:
      return emit_intrinsic(ctx, nir_instr_as_intrinsic(instr));
   case nir_instr_type_load_const:
      return emit_load_const(ctx, nir_instr_as_load_const(instr));
   default:
      NIR_INSTR_UNSUPPORTED(instr);
      assert("Unimplemented instruction type");
      return false;
   }
}


static bool
emit_block(struct ntd_context *ctx, struct nir_block *block)
{
   nir_foreach_instr(instr, block) {
      if (!emit_instr(ctx, instr))  {
         return false;
      }
   }
   return true;
}

static bool
emit_cf_list(struct ntd_context *ctx, struct exec_list *list)
{
   foreach_list_typed(nir_cf_node, node, node, list) {
      switch (node->type) {
      case nir_cf_node_block:
         if (!emit_block(ctx, nir_cf_node_as_block(node)))
            return false;
         break;

      default:
         unreachable("unsupported cf-list node");
         break;
      }
   }
   return true;
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

   if (!emit_cf_list(ctx, &entry->body))
      return false;

   if (!dxil_emit_ret_void(&ctx->mod))
      return false;

   free(ctx->defs);

   return emit_metadata(ctx, s) &&
          dxil_emit_module(&ctx->mod);
}

unsigned int
get_dxil_shader_kind(struct nir_shader *s)
{
   switch (s->info.stage) {
   case MESA_SHADER_VERTEX:
      return DXIL_VERTEX_SHADER;
   case MESA_SHADER_FRAGMENT:
      return DXIL_PIXEL_SHADER;
   case MESA_SHADER_KERNEL:
   case MESA_SHADER_COMPUTE:
      return DXIL_COMPUTE_SHADER;
   default:
      unreachable("unknown shader stage in nir_to_dxil");
      return DXIL_COMPUTE_SHADER;
   }
}

bool
nir_to_dxil(struct nir_shader *s, struct blob *blob)
{
   struct ntd_context ctx = { 0 };
   dxil_module_init(&ctx.mod);
   ctx.mod.shader_kind = get_dxil_shader_kind(s);
   ctx.mod.major_version = 6;
   ctx.mod.minor_version = 0;

   if (!emit_module(&ctx, s)) {
      debug_printf("D3D12: dxil_container_add_module failed\n");
      return false;
   }

   struct dxil_container container;
   dxil_container_init(&container);
   if (!dxil_container_add_features(&container, &ctx.mod.feats)) {
      debug_printf("D3D12: dxil_container_add_features failed\n");
      return false;
   }

   if (!dxil_container_add_input_signature(&container) ||
       !dxil_container_add_output_signature(&container)) {
      debug_printf("D3D12: failed to write input/output signature\n");
      return false;
   }


   if (!dxil_container_add_state_validation(&container, ctx.resources,
                                            ctx.num_resources)) {
      debug_printf("D3D12: failed to write state-validation\n");
      return false;
   }

   if (!dxil_container_add_module(&container, &ctx.mod)) {
      debug_printf("D3D12: failed to write module\n");
      return false;
   }

   blob_init(blob);
   if (!dxil_container_write(&container, blob)) {
      debug_printf("D3D12: dxil_container_write failed\n");
      return false;
   }

   return true;
}
