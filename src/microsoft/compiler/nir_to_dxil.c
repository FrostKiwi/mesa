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
#include "dxil_signature.h"
#include "dxil_enums.h"

#include "util/u_debug.h"
#include "nir/nir_builder.h"

#include "git_sha1.h"

#include <stdint.h>

int debug_dxil = 0;

static const struct debug_named_value
debug_options[] = {
   { "verbose", DXIL_DEBUG_VERBOSE, NULL },
   { "dump_blob",  DXIL_DEBUG_DUMP_BLOB , "Write shader blobs" },
   { "trace",  DXIL_DEBUG_TRACE , "Trace instruction conversion" },
   DEBUG_NAMED_VALUE_END
};

DEBUG_GET_ONCE_FLAGS_OPTION(debug_dxil, "DXIL_DEBUG", debug_options, 0)

#ifdef DEBUG
#define NIR_INSTR_UNSUPPORTED(instr) \
   if (debug_dxil & DXIL_DEBUG_VERBOSE) \
   do { \
      fprintf(stderr, "Unsupported instruction:"); \
      nir_print_instr(instr, stderr); \
      fprintf(stderr, "\n"); \
   } while (0)

#define TRACE_CONVERSION(instr) \
   if (debug_dxil & DXIL_DEBUG_TRACE) \
      do { \
         fprintf(stderr, "Convert '"); \
         nir_print_instr(instr, stderr); \
         fprintf(stderr, "'\n"); \
      } while (0)
#else
#define NIR_INSTR_UNSUPPORTED(instr)
#define TRACE_CONVERSION(instr)
#endif

static const nir_shader_compiler_options
nir_options = {
   .lower_negate = true,
   .lower_inot = true,
   .lower_isign = true,
   .lower_iabs = true,
   .lower_fmod = true,
   .lower_extract_word = true,
   .lower_extract_byte = true,
};

const nir_shader_compiler_options*
dxil_get_nir_compiler_options(void)
{
   return &nir_options;
}

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
emit_named_version(struct dxil_module *m, const char *name,
                   int major, int minor)
{
   const struct dxil_mdnode *major_node = dxil_get_metadata_int32(m, major);
   const struct dxil_mdnode *minor_node = dxil_get_metadata_int32(m, minor);
   const struct dxil_mdnode *version_nodes[] = { major_node, minor_node };
   const struct dxil_mdnode *version = dxil_get_metadata_node(m, version_nodes,
                                                     ARRAY_SIZE(version_nodes));
   return dxil_add_metadata_named_node(m, name, &version, 1);
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

   DXIL_INTR_BUFFER_LOAD = 68,
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
get_dx_resret_i32_type(struct dxil_module *m)
{
   const struct dxil_type *int32_type = dxil_module_get_int_type(m, 32);
   if (!int32_type)
      return NULL;

   const struct dxil_type *resret[] =
      { int32_type, int32_type, int32_type, int32_type, int32_type };

   return dxil_module_get_struct_type(m, "dx.types.ResRet.i32", resret, 5);
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


#define MAX_UAVS 64

struct dxil_def {
   const struct dxil_value *chans[NIR_MAX_VEC_COMPONENTS];
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

   const struct dxil_func *unary_funcs[DXIL_NUM_OVERLOADS],
                          *binary_funcs[DXIL_NUM_OVERLOADS],
                          *threadid_func,
                          *threadidingroup_func,
                          *groupid_func,
                          *bufferload_func,
                          *bufferstore_func,
                          *createhandle_func;

   const struct dxil_func *store_output_func[DXIL_NUM_OVERLOADS];
   const struct dxil_func *load_input_func[DXIL_NUM_OVERLOADS];
};

static const struct dxil_value *
emit_unary_call(struct ntd_context *ctx, enum overload_type overload,
                enum dxil_intr intr,
                const struct dxil_value *op0)
{
   if (!ctx->unary_funcs[overload]) {
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      const struct dxil_type *type = dxil_get_overload_type(&ctx->mod, overload);
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
               dxil_overload_suffix(overload));

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
      const struct dxil_type *type = dxil_get_overload_type(&ctx->mod, overload);
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
               dxil_overload_suffix(overload));

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

static bool
allocate_store_output_func(struct ntd_context *ctx, enum overload_type overload)
{
   const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_type *int8_type = dxil_module_get_int_type(&ctx->mod, 8);
   const struct dxil_type *var_type = dxil_get_overload_type(&ctx->mod, overload);
   const struct dxil_type *void_type = dxil_module_get_void_type(&ctx->mod);
   if (!int32_type || !var_type || !int8_type || !void_type) {
      return false;
   }

   const struct dxil_type *arg_types[] = {
      int32_type,
      int32_type,
      int32_type,
      int8_type,
      var_type
   };

   const struct dxil_type *func_type =
      dxil_module_add_function_type(&ctx->mod, void_type,
                                    arg_types, ARRAY_SIZE(arg_types));
   if (!func_type) {
      fprintf(stderr, "%s: Func type allocation failed\n", __func__);
      return false;
   }

   char name[100];
   snprintf(name, ARRAY_SIZE(name), "dx.op.storeOutput.%s",
            dxil_overload_suffix(overload));

   ctx->store_output_func[overload] = dxil_add_function_decl(&ctx->mod, name,
      func_type, DXIL_ATTR_KIND_NO_UNWIND);
   if (!ctx->store_output_func[overload]) {
      fprintf(stderr, "%s: Func decl failed\n", __func__);
      return false;
   }
   return true;
}

static bool
allocate_load_input_func(struct ntd_context *ctx, enum overload_type overload)
{
   const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_type *int8_type = dxil_module_get_int_type(&ctx->mod, 8);
   const struct dxil_type *return_type = dxil_get_overload_type(&ctx->mod, overload);

   if (!int32_type || !int8_type || !return_type) {
      return false;
   }

   const struct dxil_type *arg_types[] = {
      int32_type,
      int32_type,
      int32_type,
      int8_type,
      int32_type,
   };

   const struct dxil_type *func_type =
      dxil_module_add_function_type(&ctx->mod, return_type,
                                    arg_types, ARRAY_SIZE(arg_types));
   if (!func_type) {
      fprintf(stderr, "%s: Func type allocation failed\n", __func__);
      return false;
   }

   char name[100];
   snprintf(name, ARRAY_SIZE(name), "dx.op.loadInput.%s",
            dxil_overload_suffix(overload));

   ctx->load_input_func[overload] = dxil_add_function_decl(&ctx->mod, name,
      func_type,  DXIL_ATTR_KIND_READ_NONE);
   if (!ctx->load_input_func[overload]) {
      fprintf(stderr, "%s: Func decl failed\n", __func__);
      return false;
   }
   return true;
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

static const struct dxil_value *
emit_bufferload_call(struct ntd_context *ctx,
                     const struct dxil_value *handle,
                     const struct dxil_value *coord[2])
{
   if (!ctx->bufferload_func) {
      const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
      const struct dxil_type *handle_type = get_dx_handle_type(&ctx->mod);
      const struct dxil_type *resret_type = get_dx_resret_i32_type(&ctx->mod);
      if (!int32_type || !handle_type || !resret_type)
         return false;

      const struct dxil_type *arg_types[] = {
         int32_type,
         handle_type,
         int32_type,
         int32_type,
      };

      const struct dxil_type *func_type =
         dxil_module_add_function_type(&ctx->mod, resret_type,
                                       arg_types, ARRAY_SIZE(arg_types));
      if (!func_type)
         return false;

      ctx->bufferload_func = dxil_add_function_decl(&ctx->mod,
                                                    "dx.op.bufferLoad.i32",
                                                    func_type,
                                                    DXIL_ATTR_KIND_READ_ONLY);
      if (!ctx->bufferload_func)
         return false;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
      DXIL_INTR_BUFFER_LOAD);
   const struct dxil_value *args[] = { opcode, handle, coord[0], coord[1] };

   return dxil_emit_call(&ctx->mod, ctx->bufferload_func,
                         args, ARRAY_SIZE(args));
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

   enum dxil_component_type comp_type = dxil_get_comp_type(var->type);
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
   if (ctx->mod.feats.typed_uav_load_additional_formats)
      flags |= (1 << 13);
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

static const struct dxil_mdnode *
emit_entrypoint(struct ntd_context *ctx,
                const struct dxil_func *func, const char *name,
                const struct dxil_mdnode *signatures,
                const struct dxil_mdnode *resources,
                const struct dxil_mdnode *shader_props)
{
   const struct dxil_mdnode *func_md = dxil_get_metadata_func(&ctx->mod, func);
   const struct dxil_mdnode *name_md = dxil_get_metadata_string(&ctx->mod, name);
   const struct dxil_mdnode *nodes[] = {
      func_md,
      name_md,
      signatures,
      resources,
      shader_props
   };
   return dxil_get_metadata_node(&ctx->mod, nodes,
                                 ARRAY_SIZE(nodes));
}

static const struct dxil_mdnode *
emit_resources(struct ntd_context *ctx)
{
   bool emit_resources = false;
   const struct dxil_mdnode *resources_nodes[] = {
      NULL, NULL, NULL, NULL
   };

   if (ctx->num_uavs) {
      resources_nodes[1] = dxil_get_metadata_node(&ctx->mod, ctx->uav_metadata_nodes, ctx->num_uavs);
      emit_resources = true;
   }

   return emit_resources ?
      dxil_get_metadata_node(&ctx->mod, resources_nodes, ARRAY_SIZE(resources_nodes)): NULL;
}

static bool
emit_metadata(struct ntd_context *ctx, nir_shader *s)
{
   if (!emit_llvm_ident(&ctx->mod) ||
       !emit_named_version(&ctx->mod, "dx.version", 1, 0) ||
       !emit_named_version(&ctx->mod, "dx.valver", 1, 4) ||
       !emit_dx_shader_model(&ctx->mod))
      return false;

   const struct dxil_type *void_type = dxil_module_get_void_type(&ctx->mod);
   const struct dxil_type *main_func_type = dxil_module_add_function_type(&ctx->mod, void_type, NULL, 0);
   const struct dxil_func *main_func = dxil_add_function_def(&ctx->mod, "main", main_func_type);
   if (!main_func)
      return false;

   const struct dxil_mdnode *resources_node = emit_resources(ctx);

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

   const struct dxil_mdnode *signatures = get_signatures(&ctx->mod, s);

   const struct dxil_mdnode *dx_entry_point = emit_entrypoint(ctx, main_func,
       "main", signatures, resources_node, shader_properties);
   if (!dx_entry_point)
      return false;

   if (resources_node) {
      const struct dxil_mdnode *dx_resources = resources_node;
      dxil_add_metadata_named_node(&ctx->mod, "dx.resources",
                                       &dx_resources, 1);
   }

   const struct dxil_mdnode *dx_type_annotations[] = { main_type_annotation };
   return dxil_add_metadata_named_node(&ctx->mod, "dx.typeAnnotations",
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
      case 32: return DXIL_I32;
      case 64: return DXIL_I64;
      default:
         unreachable("unexpected bit_size");
      }
   case nir_type_float:
      switch (bit_size) {
      case 32: return DXIL_F32;
      case 64: return DXIL_F64;
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
         store_alu_dest(ctx, alu, i, get_alu_src(ctx, alu, i));
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
   case nir_op_f2f64:
   case nir_op_f2i64:
   case nir_op_f2u64:
   case nir_op_i2f64:
   case nir_op_i2i64:
   case nir_op_u2f64:
   case nir_op_u2u64:
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
emit_load_ssbo(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_type *int32_type =
      dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_value *int32_undef =
      dxil_module_get_undef(&ctx->mod, int32_type);

   const struct dxil_value *coord[2] = {
      get_src(ctx, &intr->src[1], 0, nir_type_uint),
      int32_undef
   };

   nir_const_value *const_ssbo = nir_src_as_const_value(intr->src[0]);
   if (const_ssbo) {
      unsigned idx = const_ssbo->u32;
      assert(ctx->num_uavs > idx);
      assert(intr->dest.is_ssa);

      const struct dxil_value *load =
         emit_bufferload_call(ctx, ctx->uav_handles[idx], coord);
      if (!load)
         return false;

      nir_component_mask_t comps = nir_ssa_def_components_read(&intr->dest.ssa);
      assert(comps != 0);
      for (int i = 0; i < nir_intrinsic_dest_components(intr); i++) {
         if (!(comps & (1 << i)))
            continue;
         const struct dxil_value *val =
            dxil_emit_extractval(&ctx->mod, load, get_dx_resret_i32_type(&ctx->mod), i);
         if (!val)
            return false;
         store_dest_int(ctx, &intr->dest, i, val);
      }
   } else {
      assert("dynamic ssbo addressing not implemented");
      return false;
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
emit_store_deref(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   nir_deref_instr* deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
   nir_variable* output = nir_deref_instr_get_variable(deref);

   assert(output->data.mode == nir_var_shader_out);

   if (!ctx->store_output_func[DXIL_F32] &&
       !allocate_store_output_func(ctx, DXIL_F32)) {
      debug_printf("%s: Unable to allocate storeOutput.%s function\n",
                   __func__, dxil_overload_suffix(DXIL_F32));
      return false;
   }

   const struct dxil_value *dest = dxil_module_get_int32_const(&ctx->mod, DXIL_PSOUTPUT_COLOR0);
   const struct dxil_value *loc = dxil_module_get_int32_const(&ctx->mod, (int)output->data.driver_location);
   const struct dxil_value *unknown = dxil_module_get_int32_const(&ctx->mod, 0);

   bool success = true;
   uint32_t writemask = nir_intrinsic_write_mask(intr);
   for (unsigned i = 0; i < intr->src[1].ssa->num_components && success; ++i) {
      if (writemask & (1 << i)) {
         const struct dxil_value *comp = dxil_module_get_int8_const(&ctx->mod, i);
         const struct dxil_value *value = get_src(ctx, &intr->src[1], i, nir_type_float);
         const struct dxil_value *args[] = {
            dest, loc, unknown, comp, value
         };
         success &= dxil_emit_call_void(&ctx->mod, ctx->store_output_func[DXIL_F32], args, ARRAY_SIZE(args));
      }
   }
   return success;
}

static bool
emit_load_deref(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   nir_deref_instr* deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
   nir_variable* input = nir_deref_instr_get_variable(deref);

   assert(input->data.mode == nir_var_shader_in);

   if (!ctx->load_input_func[DXIL_F32] &&
       !allocate_load_input_func(ctx, DXIL_F32)) {
      debug_printf("%s: Unable to allocate loadInput.%s function\n",
                   __func__, dxil_overload_suffix(DXIL_F32));
      return false;
   }

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, 4);
   const struct dxil_value *input_id = dxil_module_get_int32_const(&ctx->mod, (int)input->data.driver_location);
   const struct dxil_value *row = dxil_module_get_int32_const(&ctx->mod, 0);
   const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_value *vertex_id = dxil_module_get_undef(&ctx->mod, int32_type);

   for (unsigned i = 0; i < intr->dest.ssa.num_components; ++i) {
      const struct dxil_value *comp = dxil_module_get_int8_const(&ctx->mod, i);

      const struct dxil_value *args[] = {
         opcode, input_id, row, comp, vertex_id
      };

      const struct dxil_value *retval = dxil_emit_call(&ctx->mod, ctx->load_input_func[DXIL_F32], args, ARRAY_SIZE(args));
      if (!retval)
         return false;
      store_dest(ctx, &intr->dest, i, retval, nir_type_float);
   }
   return true;
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
   case nir_intrinsic_load_ssbo:
      return emit_load_ssbo(ctx, intr);
   case nir_intrinsic_store_ssbo:
      return emit_store_ssbo(ctx, intr);
   case nir_intrinsic_store_deref:
      return emit_store_deref(ctx, intr);
   case nir_intrinsic_load_deref:
      return emit_load_deref(ctx, intr);

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

static bool
emit_deref(struct ntd_context* ctx, nir_deref_instr* instr)
{
   switch (instr->deref_type) {
   case nir_deref_type_var:
      return true;
   default:
      ;
   }
   NIR_INSTR_UNSUPPORTED(&instr->instr);
   return false;
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
   case nir_instr_type_deref:
      return emit_deref(ctx, nir_instr_as_deref(instr));
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
      TRACE_CONVERSION(instr);

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

/*
 * Transforming CLC into SPIR-V also uses the global memory model for all
 * access, with a hard requirement on pointers. We don't want that, partly
 * because it's unclear how to support pointers in DXIL, and partly because
 * it also requires more work on the runtime side to patch in the pointers
 * in GPU address space.
 *
 * For now, assume that all shader_in global mem accesses will instead be
 * bound sequentially as SSBOs.
 */
static void
lower_global_mem_to_ssbo(struct nir_shader *nir)
{
   /*
    * First, rewrite all our shader_in inputs (global memory model)
    * to be SSBOs instead.
    */
   foreach_list_typed_safe(nir_variable, in, node, &nir->inputs) {
      if (in->data.mode != nir_var_shader_in)
         continue;

      struct nir_variable *ssbo = rzalloc(nir, nir_variable);
      ssbo->data.mode = nir_var_mem_ssbo;
      ssbo->data.read_only = in->data.read_only;
      ssbo->data.location = in->data.location;
      ssbo->name = in->name ? ralloc_strdup(nir, in->name) : NULL;
      ssbo->type = in->type;

      nir_shader_add_variable(nir, ssbo);
      nir->num_shared++;
      exec_node_remove(&in->node);
      nir->num_inputs--;
   }

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      /*
       * Now, find all our function params: turn the load deref (pull the
       * pointer * base address) into an SSBO index which we stash in high
       * bits.
       */
      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
            if (intr->intrinsic != nir_intrinsic_load_kernel_input)
               continue;
            b.cursor = nir_before_instr(instr);

            nir_src *src = &intr->src[0]; /* param location i.e. SSBO slot */
            assert(src->is_ssa);

            nir_ssa_def *ssbo_loc = nir_ishl(&b, src->ssa, nir_imm_int(&b, 28));
            nir_ssa_def_rewrite_uses(&intr->dest.ssa,
                                     nir_src_for_ssa(ssbo_loc));
            nir_instr_remove(instr);
         }
      }

      /*
       * Find the corresponding store_global instructions and replace them
       * with SSBO accesses. This requires widening scalar sources to vec4,
       * and reconstructing the SSBO location + index from the 'pointer'
       * constructed for us by NIR's IO lowering.
       */
      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
            if (intr->intrinsic != nir_intrinsic_store_global)
               continue;
            b.cursor = nir_before_instr(instr);

            assert(intr->src[0].is_ssa);
            nir_ssa_def *val = intr->src[0].ssa; /* value */
            nir_ssa_def *vec4_val = nir_vec4(&b, val, val, val, val);

            assert(intr->src[1].is_ssa);
            nir_ssa_def *ptr = intr->src[1].ssa; /* source 'pointer' */
#if 0
            /*
            * Here's where we would ordinarily do this, and reclaim our SSBO
            * index from the high bits of the pointer:
            *  nir_ssa_def *ssbo_loc = nir_ishr(&b, ptr, nir_imm_int(&b, 28));
            *
            * However, since we don't implement runtime lookup of SSBO
            * binding index -> UAV handle yet
            * (cf. nir_to_dxil.c:emit_store_ssbo), and NIR isn't quite smart
            * enough to see that: ((x & 0x0fffffff) << 28) >> 28 == 0,
            * we ... can't actually do this.
            *
            * So for just now, you get one SSBO, and all of the above is an
            * elaborate exercise in misdirection.
            *
            * Fixing this would likely involve converting our typed UAVs into
            * raw, and pushing the handle value into a runtime array; doing
            * this would bring us closer to the pointer model as we could at
            * least deal in byte rather than element offsets into the
            * RWBuffer.
            */
            nir_ssa_def *ssbo_loc = nir_ishr(&b, ptr, nir_imm_int(&b, 28));
#else
            nir_ssa_def *ssbo_loc = nir_imm_zero(&b, 1, 32);
#endif
            nir_ssa_def *ssbo_idx = nir_iand(&b, ptr,
                                             nir_imm_int(&b, 0x00ffffff));
            ssbo_idx = nir_ishr(&b, ssbo_idx,
                                nir_imm_int(&b, val->bit_size / 16));

            nir_intrinsic_instr *store =
               nir_intrinsic_instr_create(b.shader, nir_intrinsic_store_ssbo);
            store->num_components = 4;
            nir_intrinsic_set_write_mask(store, 0xf);
            nir_intrinsic_set_align(store, 4, 0);
            store->src[0] = nir_src_for_ssa(vec4_val);
            store->src[1] = nir_src_for_ssa(ssbo_loc);
            store->src[2] = nir_src_for_ssa(ssbo_idx);
            nir_builder_instr_insert(&b, &store->instr);
            nir_instr_remove(instr);
         }
      }

      /*
       * Find the corresponding load_global instructions and rewrite them to
       * SSBO accesses.
       */
      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
            if (intr->intrinsic != nir_intrinsic_load_global)
               continue;
            b.cursor = nir_before_instr(instr);

            assert(intr->src[0].is_ssa);
            nir_ssa_def *ptr = intr->src[0].ssa; /* source 'pointer' */
#if 0
            /*
            * Here's where we would ordinarily do this, and reclaim our SSBO
            * index from the high bits of the pointer:
            *  nir_ssa_def *ssbo_loc = nir_ishr(&b, ptr, nir_imm_int(&b, 28));
            *
            * However, since we don't implement runtime lookup of SSBO
            * binding index -> UAV handle yet
            * (cf. nir_to_dxil.c:emit_store_ssbo), and NIR isn't quite smart
            * enough to see that: ((x & 0x0fffffff) << 28) >> 28 == 0,
            * we ... can't actually do this.
            *
            * So for just now, you get one SSBO, and all of the above is an
            * elaborate exercise in misdirection.
            *
            * Fixing this would likely involve converting our typed UAVs into
            * raw, and pushing the handle value into a runtime array; doing
            * this would bring us closer to the pointer model as we could at
            * least deal in byte rather than element offsets into the
            * RWBuffer.
            */
            nir_ssa_def *ssbo_loc = nir_ishr(&b, ptr, nir_imm_int(&b, 28));
#else
            nir_ssa_def *ssbo_loc = nir_imm_zero(&b, 1, 32);
#endif
            nir_ssa_def *ssbo_idx = nir_iand(&b, ptr,
                                             nir_imm_int(&b, 0x00ffffff));
            assert(intr->dest.is_ssa);
            nir_ssa_def *dest = &intr->dest.ssa;
            ssbo_idx = nir_ishr(&b, ssbo_idx,
                                nir_imm_int(&b, dest->bit_size / 16));

            nir_intrinsic_instr *load =
               nir_intrinsic_instr_create(b.shader, nir_intrinsic_load_ssbo);
            load->num_components = 1;
            load->src[0] = nir_src_for_ssa(ssbo_loc);
            load->src[1] = nir_src_for_ssa(ssbo_idx);
            nir_ssa_dest_init(&load->instr, &load->dest, load->num_components,
                              dest->bit_size, dest->name);
            nir_intrinsic_set_align(load, 4, 0);

            nir_builder_instr_insert(&b, &load->instr);
            nir_ssa_def_rewrite_uses(dest, nir_src_for_ssa(&load->dest.ssa));
            nir_instr_remove(instr);
         }
      }
   }
}

static void
optimize_nir(struct nir_shader *s)
{
   bool progress;
   do {
      progress = false;
      NIR_PASS_V(s, nir_lower_vars_to_ssa);
      NIR_PASS(progress, s, nir_lower_alu_to_scalar, NULL, NULL);
      NIR_PASS(progress, s, nir_copy_prop);
      NIR_PASS(progress, s, nir_opt_remove_phis);
      NIR_PASS(progress, s, nir_opt_dce);
      NIR_PASS(progress, s, nir_opt_dead_cf);
      NIR_PASS(progress, s, nir_opt_cse);
      NIR_PASS(progress, s, nir_opt_peephole_select, 8, true, true);
      NIR_PASS(progress, s, nir_opt_algebraic);
      NIR_PASS(progress, s, nir_opt_constant_folding);
      NIR_PASS(progress, s, nir_opt_undef);
      NIR_PASS(progress, s, nir_opt_deref);
      NIR_PASS_V(s, nir_lower_system_values);
      if (s->info.stage == MESA_SHADER_KERNEL)
         NIR_PASS_V(s, nir_lower_explicit_io, nir_var_shader_in | nir_var_mem_global, nir_address_format_32bit_global);
   } while (progress);

   do {
      progress = false;
      NIR_PASS(progress, s, nir_opt_algebraic_late);
   } while (progress);
}

static
void dxil_fill_validation_state(struct ntd_context *ctx,
                                struct dxil_validation_state *state)
{
   state->num_resources = ctx->num_resources;
   state->resources = ctx->resources;
   state->state.psv0.max_expected_wave_lane_count = UINT_MAX;
   state->state.shader_stage = (uint8_t)ctx->mod.shader_kind;
   state->state.sig_input_elements = (uint8_t)ctx->mod.num_sig_inputs;
   state->state.sig_output_elements = (uint8_t)ctx->mod.num_sig_outputs;
   //state->state.sig_patch_const_or_prim_elements = 0;

   switch (ctx->mod.shader_kind) {
   case DXIL_VERTEX_SHADER:
      state->state.psv0.vs.output_position_present = ctx->mod.info.has_out_position;
      break;
   case DXIL_PIXEL_SHADER:
      /* TODO: handle depth outputs */
      state->state.psv0.ps.depth_output = 0;
      /* just guessing */
      state->state.psv0.ps.sample_frequency = 0;
      break;
   case DXIL_COMPUTE_SHADER:
      break;
   case DXIL_GEOMETRY_SHADER:
      /* TODO: fill with info */
      break;
   default:
      assert(0 && "Shader type not (yet) supported");
   }
}

bool
nir_to_dxil(struct nir_shader *s, struct blob *blob)
{
   bool retval = true;
   debug_dxil = (int)debug_get_option_debug_dxil();

   struct ntd_context ctx = { 0 };
   dxil_module_init(&ctx.mod);
   ctx.mod.shader_kind = get_dxil_shader_kind(s);
   ctx.mod.major_version = 6;
   ctx.mod.minor_version = 0;

   NIR_PASS_V(s, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS_V(s, nir_lower_returns);
   NIR_PASS_V(s, nir_inline_functions);
   NIR_PASS_V(s, nir_opt_deref);
   foreach_list_typed_safe(nir_function, func, node, &s->functions) {
      if (!func->is_entrypoint)
         exec_node_remove(&func->node);
   }
   assert(exec_list_length(&s->functions) == 1);
   NIR_PASS_V(s, nir_lower_variable_initializers, ~nir_var_function_temp);

   NIR_PASS_V(s, nir_lower_system_values);

   optimize_nir(s);

   /*
    * See comments on these two passes: rewrite global memory-model usage
    * with pointers into SSBO access. We re-run constant folding and undef
    * removal as these two passes both generate extra constants.
    */
   if (s->info.stage == MESA_SHADER_KERNEL) {
      NIR_PASS_V(s, lower_global_mem_to_ssbo);
      NIR_PASS_V(s, nir_lower_variable_initializers, nir_var_all);
      optimize_nir(s);
   }

   nir_print_shader(s, stderr);

   if (!emit_module(&ctx, s)) {
      debug_printf("D3D12: dxil_container_add_module failed\n");
      retval = false;
      goto fail;
   }

   struct dxil_container container;
   dxil_container_init(&container);
   if (!dxil_container_add_features(&container, &ctx.mod.feats)) {
      debug_printf("D3D12: dxil_container_add_features failed\n");
      retval = false;
      goto fail;
   }

   if (!dxil_container_add_io_signature(&container,
                                        DXIL_ISG1,
                                        ctx.mod.num_sig_inputs,
                                        ctx.mod.inputs)) {
      debug_printf("D3D12: failed to write input signature\n");
      retval = false;
      goto fail;
   }

   if (!dxil_container_add_io_signature(&container,
                                        DXIL_OSG1,
                                        ctx.mod.num_sig_outputs,
                                        ctx.mod.outputs)) {
      debug_printf("D3D12: failed to write output signature\n");
      retval = false;
      goto fail;
   }

   struct dxil_validation_state validation_state;
   memset(&validation_state, 0, sizeof(validation_state));
   dxil_fill_validation_state(&ctx, &validation_state);

   if (!dxil_container_add_state_validation(&container,&ctx.mod,
                                            &validation_state)) {
      debug_printf("D3D12: failed to write state-validation\n");
      retval = false;
      goto fail;
   }

   if (!dxil_container_add_module(&container, &ctx.mod)) {
      debug_printf("D3D12: failed to write module\n");
      retval = false;
      goto fail;
   }

   blob_init(blob);
   if (!dxil_container_write(&container, blob)) {
      debug_printf("D3D12: dxil_container_write failed\n");
      retval = false;
      goto fail;
   }

   if (debug_dxil & DXIL_DEBUG_DUMP_BLOB) {
      static int shader_id = 0;
      char buffer[64];
      snprintf(buffer, sizeof(buffer), "shader_%s_%d.blob",
               get_shader_kind_str(ctx.mod.shader_kind), shader_id++);
      debug_printf("Try to write blob to %s\n", buffer);
      FILE *f = fopen(buffer, "wb");
      if (f) {
         fwrite(blob->data, 1, blob->size, f);
         fclose(f);
      }
   }

fail:
   dxil_module_release(&ctx.mod);
   return retval;
}
