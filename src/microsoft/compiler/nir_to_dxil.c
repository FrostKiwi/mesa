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

#include "nir_to_dxil.h"

#include "dxil_module.h"
#include "dxil_container.h"
#include "dxil_function.h"
#include "dxil_signature.h"
#include "dxil_enums.h"
#include "dxil_dump.h"

#include "util/u_debug.h"
#include "util/u_math.h"
#include "nir/nir_builder.h"

#include "git_sha1.h"

#include <stdint.h>

int debug_dxil = 0;

static const struct debug_named_value
debug_options[] = {
   { "verbose", DXIL_DEBUG_VERBOSE, NULL },
   { "dump_blob",  DXIL_DEBUG_DUMP_BLOB , "Write shader blobs" },
   { "trace",  DXIL_DEBUG_TRACE , "Trace instruction conversion" },
   { "dump_module", DXIL_DEBUG_DUMP_MODULE, "dump module tree to stderr"},
   DEBUG_NAMED_VALUE_END
};

DEBUG_GET_ONCE_FLAGS_OPTION(debug_dxil, "DXIL_DEBUG", debug_options, 0)

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

static const nir_shader_compiler_options
nir_options = {
   .lower_negate = true,
   .lower_inot = true,
   .fuse_ffma = true,
   .lower_isign = true,
   .lower_fsign = true,
   .lower_iabs = true,
   .lower_fmod = true,
   .lower_fpow = true,
   .lower_scmp = true,
   .lower_ldexp = true,
   .lower_flrp16 = true,
   .lower_flrp32 = true,
   .lower_flrp64 = true,
   .lower_extract_word = true,
   .lower_extract_byte = true,
   .lower_all_io_to_elements = true,
   .lower_hadd = true,
   .lower_add_sat = true,
   .lower_uadd_carry = true,
   .lower_mul_high = true,
   .lower_rotate = true
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
   DXIL_INTR_LOAD_INPUT = 4,
   DXIL_INTR_STORE_OUTPUT = 5,
   DXIL_INTR_FABS = 6,
   DXIL_INTR_SATURATE = 7,

   DXIL_INTR_FCOS = 12,
   DXIL_INTR_FSIN = 13,

   DXIL_INTR_FEXP2 = 21,
   DXIL_INTR_FRC = 22,
   DXIL_INTR_FLOG2 = 23,

   DXIL_INTR_SQRT = 24,
   DXIL_INTR_RSQRT = 25,
   DXIL_INTR_ROUND_NE = 26,
   DXIL_INTR_ROUND_NI = 27,
   DXIL_INTR_ROUND_PI = 28,
   DXIL_INTR_ROUND_Z = 29,

   DXIL_INTR_COUNTBITS = 31,
   DXIL_INTR_FIRSTBIT_HI = 33,

   DXIL_INTR_FMAX = 35,
   DXIL_INTR_FMIN = 36,
   DXIL_INTR_IMAX = 37,
   DXIL_INTR_IMIN = 38,
   DXIL_INTR_UMAX = 39,
   DXIL_INTR_UMIN = 40,

   DXIL_INTR_FFMA = 46,

   DXIL_INTR_CREATE_HANDLE = 57,
   DXIL_INTR_CBUFFER_LOAD_LEGACY = 59,

   DXIL_INTR_SAMPLE = 60,
   DXIL_INTR_SAMPLE_BIAS = 61,
   DXIL_INTR_SAMPLE_LEVEL = 62,
   DXIL_INTR_SAMPLE_GRAD = 63,
   DXIL_INTR_SAMPLE_CMP = 64,
   DXIL_INTR_SAMPLE_CMP_LVL_ZERO = 65,

   DXIL_INTR_TEXTURE_LOAD = 66,
   DXIL_INTR_TEXTURE_STORE = 67,

   DXIL_INTR_BUFFER_LOAD = 68,
   DXIL_INTR_BUFFER_STORE = 69,

   DXIL_INTR_DISCARD = 82,
   DXIL_INTR_DDX_COARSE = 83,
   DXIL_INTR_DDY_COARSE = 84,
   DXIL_INTR_DDX_FINE = 85,
   DXIL_INTR_DDY_FINE = 86,

   DXIL_INTR_THREAD_ID = 93,
   DXIL_INTR_GROUP_ID = 94,
   DXIL_INTR_THREAD_ID_IN_GROUP = 95,

   DXIL_INTR_EMIT_STREAM = 97,
   DXIL_INTR_CUT_STREAM = 98,

   DXIL_INTR_ATTRIBUTE_AT_VERTEX = 137,
};

static void
fill_resource_metadata(struct dxil_module *m, const struct dxil_mdnode **fields,
                       const struct dxil_type *struct_type,
                       const char *name, unsigned idx, unsigned binding,
                       unsigned size)
{
   const struct dxil_type *pointer_type = dxil_module_get_pointer_type(m, struct_type);
   const struct dxil_value *pointer_undef = dxil_module_get_undef(m, pointer_type);

   fields[0] = dxil_get_metadata_int32(m, idx); // resource ID
   fields[1] = dxil_get_metadata_value(m, pointer_type, pointer_undef); // global constant symbol
   fields[2] = dxil_get_metadata_string(m, name ? name : ""); // name
   fields[3] = dxil_get_metadata_int32(m, 0); // space ID
   fields[4] = dxil_get_metadata_int32(m, binding); // lower bound
   fields[5] = dxil_get_metadata_int32(m, size); // range size
}

static const struct dxil_mdnode *
emit_srv_metadata(struct dxil_module *m, const struct dxil_type *elem_type,
                  const char *name, unsigned index,
                  enum dxil_component_type comp_type,
                  enum dxil_resource_kind res_kind)
{
   const struct dxil_mdnode *fields[9];

   const struct dxil_mdnode *buffer_element_type_tag = dxil_get_metadata_int32(m, DXIL_TYPED_BUFFER_ELEMENT_TYPE_TAG);
   const struct dxil_mdnode *element_type = dxil_get_metadata_int32(m, comp_type);
   const struct dxil_mdnode *metadata_tag_nodes[] = {
      buffer_element_type_tag, element_type
   };

   fill_resource_metadata(m, fields, elem_type, name, index, index, 1);
   fields[6] = dxil_get_metadata_int32(m, res_kind); // resource shape
   fields[7] = dxil_get_metadata_int1(m, 0); // sample count
   fields[8] = dxil_get_metadata_node(m, metadata_tag_nodes, ARRAY_SIZE(metadata_tag_nodes)); // metadata

   return dxil_get_metadata_node(m, fields, ARRAY_SIZE(fields));
}

static const struct dxil_mdnode *
emit_uav_metadata(struct dxil_module *m, const struct dxil_type *struct_type,
                  const char *name, unsigned index, unsigned size,
                  enum dxil_component_type comp_type,
                  enum dxil_resource_kind res_kind)
{
   const struct dxil_mdnode *fields[11];

   const struct dxil_mdnode *metadata_tag_nodes[2];

   fill_resource_metadata(m, fields, struct_type, name, index, index, size);
   fields[6] = dxil_get_metadata_int32(m, res_kind); // resource shape
   fields[7] = dxil_get_metadata_int1(m, false); // globally-coherent
   fields[8] = dxil_get_metadata_int1(m, false); // has counter
   fields[9] = dxil_get_metadata_int1(m, false); // is ROV
   if (res_kind != DXIL_RESOURCE_KIND_RAW_BUFFER &&
       res_kind != DXIL_RESOURCE_KIND_STRUCTURED_BUFFER) {
      metadata_tag_nodes[0] = dxil_get_metadata_int32(m, DXIL_TYPED_BUFFER_ELEMENT_TYPE_TAG);
      metadata_tag_nodes[1] = dxil_get_metadata_int32(m, comp_type);
      fields[10] = dxil_get_metadata_node(m, metadata_tag_nodes, ARRAY_SIZE(metadata_tag_nodes)); // metadata
   } else
      fields[10] = NULL;

   return dxil_get_metadata_node(m, fields, ARRAY_SIZE(fields));
}

static const struct dxil_mdnode *
emit_cbv_metadata(struct dxil_module *m, const struct dxil_type *struct_type,
                  const char *name, unsigned index, unsigned binding, unsigned size)
{
   const struct dxil_mdnode *fields[8];

   fill_resource_metadata(m, fields, struct_type, name, index, binding, 1);
   fields[6] = dxil_get_metadata_int32(m, size); // constant buffer size
   fields[7] = NULL; // metadata

   return dxil_get_metadata_node(m, fields, ARRAY_SIZE(fields));
}

static const struct dxil_mdnode *
emit_sampler_metadata(struct dxil_module *m, const struct dxil_type *struct_type,
                      nir_variable *var, unsigned index)
{
   const struct dxil_mdnode *fields[8];

   fill_resource_metadata(m, fields, struct_type, var->name, index, index, 1);
   fields[6] = dxil_get_metadata_int32(m, DXIL_SAMPLER_KIND_DEFAULT); // sampler kind
   enum dxil_sampler_kind sampler_kind = glsl_sampler_type_is_shadow(var->type) ?
          DXIL_SAMPLER_KIND_COMPARISON : DXIL_SAMPLER_KIND_DEFAULT;
   fields[6] = dxil_get_metadata_int32(m, sampler_kind); // sampler kind
   fields[7] = NULL; // metadata

   return dxil_get_metadata_node(m, fields, ARRAY_SIZE(fields));
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

   if (glsl_type_is_scalar(type))
      return get_glsl_basetype(m, glsl_get_base_type(type));

   if (glsl_type_is_array(type))
      return dxil_module_get_array_type(m,
         get_glsl_type(m, glsl_get_array_element(type)),
         glsl_get_length(type));

   unreachable("unexpected glsl type");
}


#define MAX_SRVS 64 // ??
#define MAX_UAVS 64
#define MAX_CBVS 64 // ??
#define MAX_SAMPLERS 64 // ??

struct dxil_def {
   const struct dxil_value *chans[NIR_MAX_VEC_COMPONENTS];
};

struct ntd_context {
   void *ralloc_ctx;
   const struct nir_to_dxil_options *opts;

   struct dxil_module mod;

   const struct dxil_mdnode *srv_metadata_nodes[MAX_SRVS];
   const struct dxil_value *srv_handles[MAX_SRVS];
   unsigned num_srvs;

   const struct dxil_mdnode *uav_metadata_node;

   const struct dxil_mdnode *cbv_metadata_nodes[MAX_CBVS];
   const struct dxil_value *cbv_handles[MAX_CBVS];
   unsigned num_cbvs;

   const struct dxil_mdnode *sampler_metadata_nodes[MAX_SAMPLERS];
   const struct dxil_value *sampler_handles[MAX_SAMPLERS];
   unsigned num_samplers;

   struct dxil_resource resources[MAX_UAVS];
   unsigned num_resources;

   const struct dxil_mdnode *shader_property_nodes[6];
   size_t num_shader_property_nodes;

   struct dxil_def *defs;
   unsigned num_defs;
   struct hash_table *phis;

   struct hash_table *locals;
   nir_variable *ps_front_face;
};

static const char*
unary_func_name(enum dxil_intr intr)
{
   switch (intr) {
   case DXIL_INTR_COUNTBITS:
   case DXIL_INTR_FIRSTBIT_HI:
      return "dx.op.unaryBits";
   }

   return "dx.op.unary";
}

static const struct dxil_value *
emit_unary_call(struct ntd_context *ctx, enum overload_type overload,
                enum dxil_intr intr,
                const struct dxil_value *op0)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod,
                                                    unary_func_name(intr),
                                                    overload);
   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, intr);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     op0
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_binary_call(struct ntd_context *ctx, enum overload_type overload,
                 enum dxil_intr intr,
                 const struct dxil_value *op0, const struct dxil_value *op1)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.binary", overload);
   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, intr);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     op0,
     op1
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_tertiary_call(struct ntd_context *ctx, enum overload_type overload,
                   enum dxil_intr intr,
                   const struct dxil_value *op0,
                   const struct dxil_value *op1,
                   const struct dxil_value *op2)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.tertiary", overload);
   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, intr);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     op0,
     op1,
     op2
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_threadid_call(struct ntd_context *ctx, const struct dxil_value *comp)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.threadId", DXIL_I32);
   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
       DXIL_INTR_THREAD_ID);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_threadidingroup_call(struct ntd_context *ctx,
                          const struct dxil_value *comp)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.threadIdInGroup", DXIL_I32);

   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
       DXIL_INTR_THREAD_ID_IN_GROUP);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_groupid_call(struct ntd_context *ctx, const struct dxil_value *comp)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.groupId", DXIL_I32);

   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
       DXIL_INTR_GROUP_ID);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
     opcode,
     comp
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_bufferload_call(struct ntd_context *ctx,
                     const struct dxil_value *handle,
                     const struct dxil_value *coord[2])
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.bufferLoad", DXIL_I32);
   if (!func)
      return NULL;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
      DXIL_INTR_BUFFER_LOAD);
   const struct dxil_value *args[] = { opcode, handle, coord[0], coord[1] };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static bool
emit_bufferstore_call(struct ntd_context *ctx,
                      const struct dxil_value *handle,
                      const struct dxil_value *coord[2],
                      const struct dxil_value *value[4],
                      const struct dxil_value *write_mask)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.bufferStore", DXIL_I32);

   if (!func)
      return false;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod,
      DXIL_INTR_BUFFER_STORE);
   const struct dxil_value *args[] = {
      opcode, handle, coord[0], coord[1],
      value[0], value[1], value[2], value[3],
      write_mask
   };

   return dxil_emit_call_void(&ctx->mod, func,
                              args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_createhandle_call(struct ntd_context *ctx,
                       enum dxil_resource_class resource_class,
                       unsigned resource_range_id,
                       const struct dxil_value *resource_range_index,
                       bool non_uniform_resource_index)
{
   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_CREATE_HANDLE);
   const struct dxil_value *resource_class_value = dxil_module_get_int8_const(&ctx->mod, resource_class);
   const struct dxil_value *resource_range_id_value = dxil_module_get_int32_const(&ctx->mod, resource_range_id);
   const struct dxil_value *non_uniform_resource_index_value = dxil_module_get_int1_const(&ctx->mod, non_uniform_resource_index);
   if (!opcode || !resource_class_value || !resource_range_id_value ||
       !non_uniform_resource_index_value)
      return NULL;

   const struct dxil_value *args[] = {
      opcode,
      resource_class_value,
      resource_range_id_value,
      resource_range_index,
      non_uniform_resource_index_value
   };

   const struct dxil_func *func =
         dxil_get_function(&ctx->mod, "dx.op.createHandle", DXIL_NONE);

   if (!func)
         return NULL;

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_createhandle_call_const_index(struct ntd_context *ctx,
                                   enum dxil_resource_class resource_class,
                                   unsigned resource_range_id,
                                   unsigned resource_range_index,
                                   bool non_uniform_resource_index)
{

   const struct dxil_value *resource_range_index_value = dxil_module_get_int32_const(&ctx->mod, resource_range_index);
   if (!resource_range_index_value)
      return NULL;

   return emit_createhandle_call(ctx, resource_class, resource_range_id,
                                 resource_range_index_value,
                                 non_uniform_resource_index);
}

static void
add_resource(struct ntd_context *ctx, enum dxil_resource_type type, unsigned index, unsigned array_size)
{
   assert(ctx->num_resources < ARRAY_SIZE(ctx->resources));
   ctx->resources[ctx->num_resources].resource_type = type;
   ctx->resources[ctx->num_resources].space = 0;
   ctx->resources[ctx->num_resources].lower_bound = index;
   ctx->resources[ctx->num_resources].upper_bound = index + array_size - 1;
   ctx->num_resources++;
}

static bool
emit_srv(struct ntd_context *ctx, nir_variable *var)
{
   assert(ctx->num_srvs < ARRAY_SIZE(ctx->srv_metadata_nodes));
   assert(ctx->num_srvs < ARRAY_SIZE(ctx->srv_handles));

   unsigned idx = ctx->num_srvs;
   enum dxil_component_type comp_type = DXIL_COMP_TYPE_F32;
   enum dxil_resource_kind res_kind = dxil_get_resource_kind(var->type);
   const struct dxil_type *res_type = dxil_module_get_res_type(&ctx->mod, res_kind);
   const struct dxil_mdnode *srv_meta = emit_srv_metadata(&ctx->mod, res_type, var->name, idx, comp_type, res_kind);

   if (!srv_meta)
      return false;

   ctx->srv_metadata_nodes[ctx->num_srvs] = srv_meta;
   assert(glsl_type_get_sampler_count(var->type) == 1);
   add_resource(ctx, DXIL_RES_SRV_TYPED, idx, glsl_type_get_sampler_count(var->type));

   const struct dxil_value *handle = emit_createhandle_call_const_index(ctx, DXIL_RESOURCE_CLASS_SRV,
                                                                        idx, idx, false);
   if (!handle)
      return false;

   ctx->srv_handles[ctx->num_srvs] = handle;
   ctx->num_srvs++;

   return true;
}

static bool
emit_globals(struct ntd_context *ctx, uint64_t global_inputs)
{
   const struct dxil_type *type = dxil_module_get_int_type(&ctx->mod, 32);
   if (!type)
      return false;

   const struct dxil_type *struct_type =
      dxil_module_get_struct_type(&ctx->mod, NULL, &type, 1);
   if (!struct_type)
      return false;

   unsigned size = util_bitcount64(global_inputs);
   const struct dxil_type *array_type =
      dxil_module_get_array_type(&ctx->mod, struct_type, size);
   if (!array_type)
      return false;

   const struct dxil_mdnode *uav_meta =
      emit_uav_metadata(&ctx->mod, array_type,
                                   "globals", 0, size,
                                   DXIL_COMP_TYPE_INVALID,
                                   DXIL_RESOURCE_KIND_RAW_BUFFER);
   if (!uav_meta)
      return false;

   ctx->uav_metadata_node = uav_meta;
   add_resource(ctx, DXIL_RES_UAV_RAW, 0, size);
   ctx->mod.raw_and_structured_buffers = true;
   return true;
}

static unsigned get_dword_size(const struct glsl_type *type)
{
   unsigned factor = 1;
   if (glsl_type_is_array(type)) {
      factor = glsl_get_aoa_size(type);
      type = glsl_without_array(type);
   }
   return (factor * glsl_get_components(type));
}

static bool
emit_kernel_inputs_cbv(struct ntd_context *ctx, nir_shader *nir)
{
   unsigned size = 0;

   nir_foreach_variable(var, &nir->inputs)
      size = MAX2(size,
                  var->data.driver_location +
                  glsl_get_cl_size(var->type));

   size = align(size, 4);
   if (!size)
      return true;

   const struct dxil_type *int32 = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_type *array_type = dxil_module_get_array_type(&ctx->mod, int32, size / 4);
   const struct dxil_type *buffer_type = dxil_module_get_struct_type(&ctx->mod, "kernel_inputs",
                                                                     &array_type, 1);
   const struct dxil_mdnode *cbv_meta = emit_cbv_metadata(&ctx->mod, buffer_type,
                                                          "kernel_inputs", ctx->num_cbvs,
                                                          ctx->num_cbvs, size);

   if (!cbv_meta)
      return false;

   ctx->cbv_metadata_nodes[ctx->num_cbvs] = cbv_meta;
   add_resource(ctx, DXIL_RES_CBV, ctx->num_cbvs, 1);

   const struct dxil_value *handle = emit_createhandle_call_const_index(ctx, DXIL_RESOURCE_CLASS_CBV,
                                                                        ctx->num_cbvs, ctx->num_cbvs,
                                                                        false);
   if (!handle)
      return false;

   ctx->cbv_handles[ctx->num_cbvs++] = handle;

   return true;
}

static bool
emit_cbv(struct ntd_context *ctx, nir_variable *var)
{
   unsigned idx = ctx->num_cbvs;
   unsigned binding = var->data.binding;
   unsigned size = get_dword_size(var->type);

   assert(idx < ARRAY_SIZE(ctx->cbv_metadata_nodes));
   assert(binding < ARRAY_SIZE(ctx->cbv_handles));

   const struct dxil_type *float32 = dxil_module_get_float_type(&ctx->mod, 32);
   const struct dxil_type *array_type = dxil_module_get_array_type(&ctx->mod, float32, size);
   const struct dxil_type *buffer_type = dxil_module_get_struct_type(&ctx->mod, var->name,
                                                                     &array_type, 1);
   const struct dxil_mdnode *cbv_meta = emit_cbv_metadata(&ctx->mod, buffer_type,
                                                          var->name, idx, binding,
                                                          size * 4);

   if (!cbv_meta)
      return false;

   ctx->cbv_metadata_nodes[ctx->num_cbvs] = cbv_meta;
   add_resource(ctx, DXIL_RES_CBV, binding, 1);

   const struct dxil_value *handle = emit_createhandle_call_const_index(ctx, DXIL_RESOURCE_CLASS_CBV,
                                                                        idx, binding, false);
   if (!handle)
      return false;

   assert(!ctx->cbv_handles[binding]);
   ctx->cbv_handles[binding] = handle;
   ctx->num_cbvs++;

   return true;
}

static bool
emit_sampler(struct ntd_context *ctx, nir_variable *var)
{
   assert(ctx->num_samplers < ARRAY_SIZE(ctx->sampler_metadata_nodes));
   assert(ctx->num_samplers < ARRAY_SIZE(ctx->sampler_handles));

   unsigned idx = ctx->num_samplers;
   const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_type *sampler_type = dxil_module_get_struct_type(&ctx->mod, "struct.SamplerState", &int32_type, 1);
   const struct dxil_mdnode *sampler_meta = emit_sampler_metadata(&ctx->mod, sampler_type, var, idx);

   if (!sampler_meta)
      return false;

   ctx->sampler_metadata_nodes[ctx->num_samplers] = sampler_meta;
   add_resource(ctx, DXIL_RES_SAMPLER, idx, glsl_type_get_sampler_count(var->type));

   const struct dxil_value *handle = emit_createhandle_call_const_index(ctx, DXIL_RESOURCE_CLASS_SAMPLER,
                                                                        idx, idx, false);
   if (!handle)
      return false;

   ctx->sampler_handles[ctx->num_samplers] = handle;
   ctx->num_samplers++;

   return true;
}

static const struct dxil_mdnode *
emit_gs_state(struct ntd_context *ctx, nir_shader *s)
{
   const struct dxil_mdnode *gs_state_nodes[5];

   gs_state_nodes[0] = dxil_get_metadata_int32(&ctx->mod, dxil_get_input_primitive(s->info.gs.input_primitive));
   gs_state_nodes[1] = dxil_get_metadata_int32(&ctx->mod, s->info.gs.vertices_out);
   gs_state_nodes[2] = dxil_get_metadata_int32(&ctx->mod, s->info.gs.active_stream_mask);
   gs_state_nodes[3] = dxil_get_metadata_int32(&ctx->mod, dxil_get_primitive_topology(s->info.gs.output_primitive));
   gs_state_nodes[4] = dxil_get_metadata_int32(&ctx->mod, s->info.gs.invocations);

   for (unsigned i = 0; i < ARRAY_SIZE(gs_state_nodes); ++i) {
      if (!gs_state_nodes[i])
         return NULL;
   }

   return dxil_get_metadata_node(&ctx->mod, gs_state_nodes, ARRAY_SIZE(gs_state_nodes));
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
   if (ctx->mod.raw_and_structured_buffers)
      flags |= (1 << 4);
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

   if (ctx->num_srvs) {
      resources_nodes[0] = dxil_get_metadata_node(&ctx->mod, ctx->srv_metadata_nodes, ctx->num_srvs);
      emit_resources = true;
   }

   if (ctx->uav_metadata_node) {
      resources_nodes[1] = dxil_get_metadata_node(&ctx->mod, &ctx->uav_metadata_node, 1);
      emit_resources = true;
   }

   if (ctx->num_cbvs) {
      resources_nodes[2] = dxil_get_metadata_node(&ctx->mod, ctx->cbv_metadata_nodes, ctx->num_cbvs);
      emit_resources = true;
   }

   if (ctx->num_samplers) {
      resources_nodes[3] = dxil_get_metadata_node(&ctx->mod, ctx->sampler_metadata_nodes, ctx->num_samplers);
      emit_resources = true;
   }

   return emit_resources ?
      dxil_get_metadata_node(&ctx->mod, resources_nodes, ARRAY_SIZE(resources_nodes)): NULL;
}

static boolean
emit_tag(struct ntd_context *ctx, enum dxil_shader_tag tag,
         const struct dxil_mdnode *value_node)
{
   const struct dxil_mdnode *tag_node = dxil_get_metadata_int32(&ctx->mod, tag);
   if (!tag_node || !value_node)
      return false;
   assert(ctx->num_shader_property_nodes <= ARRAY_SIZE(ctx->shader_property_nodes) - 2);
   ctx->shader_property_nodes[ctx->num_shader_property_nodes++] = tag_node;
   ctx->shader_property_nodes[ctx->num_shader_property_nodes++] = value_node;

   return true;
}

static bool
emit_metadata(struct ntd_context *ctx, nir_shader *s)
{
   if (!emit_llvm_ident(&ctx->mod) ||
       !emit_named_version(&ctx->mod, "dx.version", 1, 1) ||
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

   if (ctx->mod.shader_kind == DXIL_GEOMETRY_SHADER) {
      if (!emit_tag(ctx, DXIL_SHADER_TAG_GS_STATE, emit_gs_state(ctx, s)))
         return false;
   } else if (ctx->mod.shader_kind == DXIL_COMPUTE_SHADER) {
      uint64_t flags = get_module_flags(ctx);
      if (flags != 0) {
         if (!emit_tag(ctx, DXIL_SHADER_TAG_FLAGS, dxil_get_metadata_int64(&ctx->mod, flags)))
            return false;
      }
      if (!emit_tag(ctx, DXIL_SHADER_TAG_NUM_THREADS, emit_threads(ctx, s)))
         return false;
   }

   const struct dxil_mdnode *shader_properties = NULL;
   if (ctx->num_shader_property_nodes > 0) {
      shader_properties = dxil_get_metadata_node(&ctx->mod, ctx->shader_property_nodes,
                                                 ctx->num_shader_property_nodes);
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
   if (nir_dest_bit_size(*dest) != 1) {
      switch (nir_alu_type_get_base_type(type)) {
      case nir_type_uint:
      case nir_type_int:
         assert(nir_dest_bit_size(*dest) != 1);
         if (nir_dest_bit_size(*dest) == 16)
            ctx->mod.feats.min_precision = true;
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

      default:
         unreachable("unexpected nir_alu_type");
      }
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

   if (nir_src_bit_size(*src) == 1)
      return value;

   switch (nir_alu_type_get_base_type(type)) {
   case nir_type_int:
   case nir_type_uint:
      assert(nir_src_bit_size(*src) >= 16);
      assert(nir_src_bit_size(*src) != 64 || ctx->mod.feats.int64_ops);
      /* nohing to do */
      return value;

   case nir_type_float:
      assert(nir_src_bit_size(*src) >= 32);
      assert(nir_src_bit_size(*src) != 64 || (ctx->mod.feats.doubles &&
                                              ctx->mod.feats.int64_ops));
      return bitcast_to_float(ctx, nir_src_bit_size(*src), value);

   default:
      unreachable("unexpected nir_alu_type");
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
   bool is_float_op = nir_alu_type_get_base_type(nir_op_infos[alu->op].output_type) == nir_type_float;

   enum dxil_opt_flags flags = 0;
   if (is_float_op && !alu->exact)
      flags |= DXIL_UNSAFE_ALGEBRA;

   const struct dxil_value *v = dxil_emit_binop(&ctx->mod, opcode, op0, op1, flags);
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
   /* bool -> int */
   case nir_op_b2i16:
   case nir_op_b2i32:
   case nir_op_b2i64:
      return DXIL_CAST_ZEXT;

   /* float -> float */
   case nir_op_f2f32:
   case nir_op_f2f64:
      assert(dst_bits != src_bits);
      if (dst_bits < src_bits)
         return DXIL_CAST_FPTRUNC;
      else
         return DXIL_CAST_FPEXT;

   /* int -> int */
   case nir_op_i2i16:
   case nir_op_i2i32:
   case nir_op_i2i64:
      assert(dst_bits != src_bits);
      if (dst_bits < src_bits)
         return DXIL_CAST_TRUNC;
      else
         return DXIL_CAST_SEXT;

   /* uint -> uint */
   case nir_op_u2u16:
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
   case nir_op_f2b1:
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
   case nir_type_bool:
      assert(dst_bits == 1);
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
emit_tertiary_intin(struct ntd_context *ctx, nir_alu_instr *alu,
                    enum dxil_intr intr,
                    const struct dxil_value *op0,
                    const struct dxil_value *op1,
                    const struct dxil_value *op2)
{
   const nir_op_info *info = &nir_op_infos[alu->op];
   assert(info->output_type == info->input_types[0]);
   assert(info->output_type == info->input_types[1]);
   assert(info->output_type == info->input_types[2]);

   unsigned dst_bits = nir_dest_bit_size(alu->dest.dest);
   assert(nir_src_bit_size(alu->src[0].src) == dst_bits);
   assert(nir_src_bit_size(alu->src[1].src) == dst_bits);
   assert(nir_src_bit_size(alu->src[2].src) == dst_bits);

   enum overload_type overload = get_overload(info->output_type, dst_bits);

   const struct dxil_value *v = emit_tertiary_call(ctx, overload, intr,
                                                   op0, op1, op2);
   if (!v)
      return false;
   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static bool emit_select(struct ntd_context *ctx, nir_alu_instr *alu,
                        const struct dxil_value *sel,
                        const struct dxil_value *val_true,
                        const struct dxil_value *val_false)
{
   assert(sel);
   assert(val_true);
   assert(val_false);

   const struct dxil_value *v = dxil_emit_select(&ctx->mod, sel, val_true, val_false);
   if (!v)
      return false;

   store_alu_dest(ctx, alu, 0, v);
   return true;
}

static bool
emit_b2f32(struct ntd_context *ctx, nir_alu_instr *alu, const struct dxil_value *val)
{
   assert(val);

   struct dxil_module *m = &ctx->mod;

   const struct dxil_value *c1 = dxil_module_get_float_const(m, 1.0f);
   const struct dxil_value *c0 = dxil_module_get_float_const(m, 0.0f);

   if (!c0 || !c1)
      return false;

   return emit_select(ctx, alu, val, c1, c0);
}

static bool
emit_ufind_msb(struct ntd_context *ctx, nir_alu_instr *alu,
               const struct dxil_value *val)
{
   const nir_op_info *info = &nir_op_infos[alu->op];
   unsigned dst_bits = nir_dest_bit_size(alu->dest.dest);
   unsigned src_bits = nir_src_bit_size(alu->src[0].src);
   enum overload_type overload = get_overload(info->output_type, dst_bits);

   const struct dxil_value *v = emit_unary_call(ctx, overload,
                                                DXIL_INTR_FIRSTBIT_HI, val);
   if (!v)
      return false;

   const struct dxil_value *size = dxil_module_get_int32_const(&ctx->mod,
      src_bits - 1);
   const struct dxil_value *zero = dxil_module_get_int_const(&ctx->mod, 0,
                                                             src_bits);
   if (!size || !zero)
      return false;

   v = dxil_emit_binop(&ctx->mod, DXIL_BINOP_SUB, size, v, 0);
   const struct dxil_value *cnd = dxil_emit_cmp(&ctx->mod, DXIL_ICMP_NE,
                                                val, zero);
   if (!v || !cnd)
      return false;

   const struct dxil_value *minus_one =
      dxil_module_get_int_const(&ctx->mod, -1, src_bits);
   if (!minus_one)
      return false;

   v = dxil_emit_select(&ctx->mod, cnd, v, minus_one);
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
   case nir_op_umod: return emit_binop(ctx, alu, DXIL_BINOP_UREM, src[0], src[1]);
   case nir_op_ishl: return emit_binop(ctx, alu, DXIL_BINOP_SHL, src[0], src[1]);
   case nir_op_ishr: return emit_binop(ctx, alu, DXIL_BINOP_ASHR, src[0], src[1]);
   case nir_op_ushr: return emit_binop(ctx, alu, DXIL_BINOP_LSHR, src[0], src[1]);
   case nir_op_iand: return emit_binop(ctx, alu, DXIL_BINOP_AND, src[0], src[1]);
   case nir_op_ior:  return emit_binop(ctx, alu, DXIL_BINOP_OR, src[0], src[1]);
   case nir_op_ixor: return emit_binop(ctx, alu, DXIL_BINOP_XOR, src[0], src[1]);
   case nir_op_ieq:  return emit_cmp(ctx, alu, DXIL_ICMP_EQ, src[0], src[1]);
   case nir_op_ine:  return emit_cmp(ctx, alu, DXIL_ICMP_NE, src[0], src[1]);
   case nir_op_ige:  return emit_cmp(ctx, alu, DXIL_ICMP_SGE, src[0], src[1]);
   case nir_op_uge:  return emit_cmp(ctx, alu, DXIL_ICMP_UGE, src[0], src[1]);
   case nir_op_ilt:  return emit_cmp(ctx, alu, DXIL_ICMP_SLT, src[0], src[1]);
   case nir_op_ult:  return emit_cmp(ctx, alu, DXIL_ICMP_ULT, src[0], src[1]);
   case nir_op_feq:  return emit_cmp(ctx, alu, DXIL_FCMP_UEQ, src[0], src[1]);
   case nir_op_fne:  return emit_cmp(ctx, alu, DXIL_FCMP_UNE, src[0], src[1]);
   case nir_op_flt:  return emit_cmp(ctx, alu, DXIL_FCMP_ULT, src[0], src[1]);
   case nir_op_fge:  return emit_cmp(ctx, alu, DXIL_FCMP_UGE, src[0], src[1]);
   case nir_op_bcsel: return emit_select(ctx, alu, src[0], src[1], src[2]);
   case nir_op_ftrunc: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_Z, src[0]);
   case nir_op_fabs: return emit_unary_intin(ctx, alu, DXIL_INTR_FABS, src[0]);
   case nir_op_fcos: return emit_unary_intin(ctx, alu, DXIL_INTR_FCOS, src[0]);
   case nir_op_fsin: return emit_unary_intin(ctx, alu, DXIL_INTR_FSIN, src[0]);
   case nir_op_fceil: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_PI, src[0]);
   case nir_op_fexp2: return emit_unary_intin(ctx, alu, DXIL_INTR_FEXP2, src[0]);
   case nir_op_flog2: return emit_unary_intin(ctx, alu, DXIL_INTR_FLOG2, src[0]);
   case nir_op_ffloor: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_NI, src[0]);
   case nir_op_ffract: return emit_unary_intin(ctx, alu, DXIL_INTR_FRC, src[0]);

   case nir_op_fddx:
   case nir_op_fddx_coarse: return emit_unary_intin(ctx, alu, DXIL_INTR_DDX_COARSE, src[0]);
   case nir_op_fddx_fine: return emit_unary_intin(ctx, alu, DXIL_INTR_DDX_FINE, src[0]);
   case nir_op_fddy:
   case nir_op_fddy_coarse: return emit_unary_intin(ctx, alu, DXIL_INTR_DDY_COARSE, src[0]);
   case nir_op_fddy_fine: return emit_unary_intin(ctx, alu, DXIL_INTR_DDY_FINE, src[0]);

   case nir_op_fround_even: return emit_unary_intin(ctx, alu, DXIL_INTR_ROUND_NE, src[0]);
   case nir_op_frcp: {
         const struct dxil_value *one = dxil_module_get_float_const(&ctx->mod, 1.0f);
         return emit_binop(ctx, alu, DXIL_BINOP_SDIV, one, src[0]);
      }
   case nir_op_fsat: return emit_unary_intin(ctx, alu, DXIL_INTR_SATURATE, src[0]);
   case nir_op_bit_count: return emit_unary_intin(ctx, alu, DXIL_INTR_COUNTBITS, src[0]);
   case nir_op_ufind_msb: return emit_ufind_msb(ctx, alu, src[0]);
   case nir_op_imax: return emit_binary_intin(ctx, alu, DXIL_INTR_IMAX, src[0], src[1]);
   case nir_op_imin: return emit_binary_intin(ctx, alu, DXIL_INTR_IMIN, src[0], src[1]);
   case nir_op_umax: return emit_binary_intin(ctx, alu, DXIL_INTR_UMAX, src[0], src[1]);
   case nir_op_umin: return emit_binary_intin(ctx, alu, DXIL_INTR_UMIN, src[0], src[1]);
   case nir_op_frsq: return emit_unary_intin(ctx, alu, DXIL_INTR_RSQRT, src[0]);
   case nir_op_fsqrt: return emit_unary_intin(ctx, alu, DXIL_INTR_SQRT, src[0]);
   case nir_op_fmax: return emit_binary_intin(ctx, alu, DXIL_INTR_FMAX, src[0], src[1]);
   case nir_op_fmin: return emit_binary_intin(ctx, alu, DXIL_INTR_FMIN, src[0], src[1]);
   case nir_op_ffma: return emit_tertiary_intin(ctx, alu, DXIL_INTR_FFMA, src[0], src[1], src[2]);

   case nir_op_f2b1:
   case nir_op_b2i16:
   case nir_op_i2i16:
   case nir_op_u2u16:
   case nir_op_b2i32:
   case nir_op_f2f32:
   case nir_op_f2i32:
   case nir_op_f2u32:
   case nir_op_i2f32:
   case nir_op_i2i32:
   case nir_op_u2f32:
   case nir_op_u2u32:
   case nir_op_b2i64:
   case nir_op_f2f64:
   case nir_op_f2i64:
   case nir_op_f2u64:
   case nir_op_i2f64:
   case nir_op_i2i64:
   case nir_op_u2f64:
   case nir_op_u2u64:
      return emit_cast(ctx, alu, src[0]);

   case nir_op_b2f32: return emit_b2f32(ctx, alu, src[0]);
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

const struct dxil_value *
load_ubo(struct ntd_context *ctx, const struct dxil_value *handle,
         const struct dxil_value *offset, enum overload_type overload)
{
   assert(handle && offset);

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_CBUFFER_LOAD_LEGACY);
   if (!opcode)
      return NULL;

   const struct dxil_value *args[] = {
      opcode, handle, offset
   };

   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.cbufferLoadLegacy", overload);
   if (!func)
      return NULL;
   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static bool
emit_load_kernel_input(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_value *handle = ctx->cbv_handles[ctx->num_cbvs - 1];
   const struct dxil_value *offset;
   nir_const_value *const_offset = nir_src_as_const_value(intr->src[0]);
   assert(const_offset);
   if (const_offset) {
      offset = dxil_module_get_int32_const(&ctx->mod, const_offset->i32 >> 4);
   } else {
      const struct dxil_value *offset_src = get_src(ctx, &intr->src[0], 0, nir_type_uint);
      const struct dxil_value *c4 = dxil_module_get_int32_const(&ctx->mod, 4);
      offset = dxil_emit_binop(&ctx->mod, DXIL_BINOP_ASHR, offset_src, c4, 0);
   }

   const struct dxil_value *agg = load_ubo(ctx, handle, offset, DXIL_I32);

   if (!agg)
      return false;

   unsigned start = (const_offset->i32 % 16) / 4;
   for (unsigned i = 0; i < intr->dest.ssa.num_components; ++i) {
      const struct dxil_value *retval = dxil_emit_extractval(&ctx->mod, agg, start + i);
      store_dest_int(ctx, &intr->dest, i, retval);
   }
   return true;
}

static const struct dxil_value *
get_int32_undef(struct dxil_module *m)
{
   const struct dxil_type *int32_type =
      dxil_module_get_int_type(m, 32);
   if (!int32_type)
      return NULL;

   return dxil_module_get_undef(m, int32_type);
}

static const struct dxil_value *
ptr_to_buffer(struct dxil_module *m, const struct dxil_value *ptr)
{
   const struct dxil_value *shift = dxil_module_get_int32_const(m, 28);
   if (!shift)
      return NULL;

   return dxil_emit_binop(m, DXIL_BINOP_LSHR, ptr, shift, 0);
}

static const struct dxil_value *
ptr_to_offset(struct dxil_module *m, const struct dxil_value *ptr)
{
   const struct dxil_value *mask =
      dxil_module_get_int32_const(m, (1u << 28) - 1);
   if (!mask)
      return NULL;

   return dxil_emit_binop(m, DXIL_BINOP_AND, ptr, mask, 0);
}

static const struct dxil_value *
offset_to_index(struct dxil_module *m, const struct dxil_value *offset,
                unsigned bit_size)
{
   unsigned shift_amt = util_logbase2(bit_size / 8);
   const struct dxil_value *shift =
      dxil_module_get_int32_const(m, shift_amt);
   if (!shift)
      return NULL;

   return dxil_emit_binop(m, DXIL_BINOP_LSHR, offset, shift, 0);
}

static const struct dxil_value *
index_to_offset(struct dxil_module *m, const struct dxil_value *index,
                unsigned bit_size)
{
   unsigned shift_amt = util_logbase2(bit_size / 8);
   const struct dxil_value *shift =
      dxil_module_get_int32_const(m, shift_amt);
   if (!shift)
      return NULL;

   return dxil_emit_binop(m, DXIL_BINOP_SHL, index, shift, 0);
}

static bool
emit_load_global(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_value *int32_undef = get_int32_undef(&ctx->mod);
   const struct dxil_value *ptr =
      get_src(ctx, &intr->src[0], 0, nir_type_uint);
   if (!int32_undef || !ptr)
      return false;

   const struct dxil_value *buffer = ptr_to_buffer(&ctx->mod, ptr);
   const struct dxil_value *offset = ptr_to_offset(&ctx->mod, ptr);
   if (!buffer || !offset)
      return false;

   const struct dxil_value *handle =
      emit_createhandle_call(ctx, DXIL_RESOURCE_CLASS_UAV, 0, buffer, true);
   if (!handle)
      return false;

   const struct dxil_value *coord[2] = {
      offset,
      int32_undef
   };

   const struct dxil_value *load = emit_bufferload_call(ctx, handle, coord);
   if (!load)
      return false;

   const struct dxil_value *val =
      dxil_emit_extractval(&ctx->mod, load, 0);
   if (!val)
      return false;

   store_dest_int(ctx, &intr->dest, 0, val);
   return true;
}

static bool
emit_store_global(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_value *int32_undef = get_int32_undef(&ctx->mod);
   const struct dxil_value *ptr =
      get_src(ctx, &intr->src[1], 0, nir_type_uint);
   if (!int32_undef || !ptr)
      return false;

   const struct dxil_value *buffer = ptr_to_buffer(&ctx->mod, ptr);
   const struct dxil_value *offset = ptr_to_offset(&ctx->mod, ptr);
   if (!buffer || !offset)
      return false;

   const struct dxil_value *handle =
      emit_createhandle_call(ctx, DXIL_RESOURCE_CLASS_UAV, 0, buffer, true);
   if (!handle)
      return false;

   const struct dxil_value *coord[2] = {
      offset,
      int32_undef
   };

   uint32_t writemask = nir_intrinsic_write_mask(intr);
   assert(writemask == 0x1 || writemask == 0x3 ||
          writemask == 0x7 || writemask == 0xf);

   const struct dxil_value *value[4];
   for (unsigned i = 0; i < 4; ++i) {
      if (writemask & (1 << i))
         value[i] = get_src(ctx, &intr->src[0], i, nir_type_uint);
      else
         value[i] = int32_undef;

      if (!value[i])
         return false;
   }

   const struct dxil_value *write_mask =
      dxil_module_get_int8_const(&ctx->mod, writemask);
   if (!write_mask)
      return false;

   return emit_bufferstore_call(ctx, handle, coord, value, write_mask);
}

static bool
emit_load_ubo(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   nir_const_value *const_block_index = nir_src_as_const_value(intr->src[0]);
   assert(const_block_index); // no dynamic indexing for now
   assert(const_block_index->u32 <= 1); // we only support the default and state vars UBO for now
   const struct dxil_value *handle = ctx->cbv_handles[const_block_index->u32];
   assert(handle);
   const struct dxil_value *offset;
   nir_const_value *const_offset = nir_src_as_const_value(intr->src[1]);
   if (const_offset) {
      offset = dxil_module_get_int32_const(&ctx->mod, const_offset->i32 >> 4);
   } else {
      const struct dxil_value *offset_src = get_src(ctx, &intr->src[1], 0, nir_type_uint);
      const struct dxil_value *c4 = dxil_module_get_int32_const(&ctx->mod, 4);
      offset = dxil_emit_binop(&ctx->mod, DXIL_BINOP_ASHR, offset_src, c4, 0);
   }

   const struct dxil_value *agg = load_ubo(ctx, handle, offset, DXIL_F32);

   if (!agg)
      return false;

   for (unsigned i = 0; i < nir_dest_num_components(intr->dest); ++i) {
      const struct dxil_value *retval = dxil_emit_extractval(&ctx->mod, agg, i);
      store_dest(ctx, &intr->dest, i, retval,
                 nir_dest_bit_size(intr->dest) > 1 ? nir_type_float : nir_type_bool);
   }
   return true;
}

static bool
emit_store_output(struct ntd_context *ctx, nir_intrinsic_instr *intr,
                  nir_variable *output)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.storeOutput", DXIL_F32);

   if (!func)
      return false;

   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_STORE_OUTPUT);
   const struct dxil_value *output_id = dxil_module_get_int32_const(&ctx->mod, (int)output->data.driver_location);
   const struct dxil_value *row = dxil_module_get_int32_const(&ctx->mod, 0);

   bool success = true;
   uint32_t writemask = nir_intrinsic_write_mask(intr);
   for (unsigned i = 0; i < nir_src_num_components(intr->src[1]) && success; ++i) {
      if (writemask & (1 << i)) {
         const struct dxil_value *col = dxil_module_get_int8_const(&ctx->mod, i);
         const struct dxil_value *value = get_src(ctx, &intr->src[1], i, nir_type_float);
         const struct dxil_value *args[] = {
            opcode, output_id, row, col, value
         };
         success &= dxil_emit_call_void(&ctx->mod, func, args, ARRAY_SIZE(args));
      }
   }
   return success;
}

static bool
emit_store_function_temp(struct ntd_context *ctx, nir_intrinsic_instr *intr,
                         nir_variable *var)
{
   const struct dxil_value *value =
      get_src(ctx, &intr->src[1], 0, nir_type_uint);
   const struct dxil_value *ptr =
      get_src(ctx, &intr->src[0], 0, nir_type_uint);

   unsigned align = nir_src_bit_size(intr->src[0]) / 8;
   return dxil_emit_store(&ctx->mod, value, ptr, align, false);
}

static bool
emit_store_deref(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   assert(intr->src[0].is_ssa);
   nir_deref_instr *deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
   nir_variable *var = nir_deref_instr_get_variable(deref);

   switch (var->data.mode) {
   case nir_var_shader_out:
      return emit_store_output(ctx, intr, var);

   case nir_var_function_temp:
      return emit_store_function_temp(ctx, intr, var);

   default:
      unreachable("unsupported nir_variable_mode");
   }
}

static bool
emit_load_input_interpolated(struct ntd_context *ctx, nir_intrinsic_instr *intr, nir_variable *var)
{
   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_LOAD_INPUT);
   const struct dxil_value *input_id = dxil_module_get_int32_const(&ctx->mod, var->data.driver_location);
   const struct dxil_value *row = dxil_module_get_int32_const(&ctx->mod, 0);
   const struct dxil_type *int32_type = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_value *vertex_id = dxil_module_get_undef(&ctx->mod, int32_type);

   nir_alu_type out_type = nir_get_nir_type_for_glsl_base_type(glsl_get_base_type(var->type));
   enum overload_type overload = get_overload(out_type, 32);

   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.loadInput", overload);

   if (!func)
      return false;

   for (unsigned i = 0; i < nir_dest_num_components(intr->dest); ++i) {
      const struct dxil_value *comp = dxil_module_get_int8_const(&ctx->mod, i);

      const struct dxil_value *args[] = {
         opcode, input_id, row, comp, vertex_id
      };

      const struct dxil_value *retval = dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
      if (!retval)
         return false;
      store_dest(ctx, &intr->dest, i, retval, out_type);
   }
   return true;
}

static bool
emit_load_input_flat(struct ntd_context *ctx, nir_intrinsic_instr *intr, nir_variable* var)
{
   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_ATTRIBUTE_AT_VERTEX);
   const struct dxil_value *input_id = dxil_module_get_int32_const(&ctx->mod, (int)var->data.driver_location);
   const struct dxil_value *row = dxil_module_get_int32_const(&ctx->mod, 0);
   const struct dxil_value *vertex_id = dxil_module_get_int8_const(&ctx->mod, 2);

   nir_alu_type out_type = nir_get_nir_type_for_glsl_base_type(glsl_get_base_type(var->type));
   enum overload_type overload = get_overload(out_type, 32);

   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.attributeAtVertex", overload);
   if (!func)
      return false;

   for (unsigned i = 0; i < nir_dest_num_components(intr->dest); ++i) {
      const struct dxil_value *comp = dxil_module_get_int8_const(&ctx->mod, i);
      const struct dxil_value *args[] = {
         opcode, input_id, row, comp, vertex_id
      };

      const struct dxil_value *retval = dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
      if (!retval)
         return false;

      store_dest(ctx, &intr->dest, i, retval, out_type);
   }
   return true;
}

static bool
emit_load_input(struct ntd_context *ctx, nir_intrinsic_instr *intr,
                nir_variable *input)
{
   if (ctx->mod.shader_kind != DXIL_PIXEL_SHADER ||
       input->data.interpolation != INTERP_MODE_FLAT ||
       !ctx->opts->interpolate_at_vertex)
      return emit_load_input_interpolated(ctx, intr, input);
   else
      return emit_load_input_flat(ctx, intr, input);
}

static bool
emit_load_function_temp(struct ntd_context *ctx, nir_intrinsic_instr *intr,
                        nir_variable *var)
{
   const struct dxil_value *ptr =
      get_src(ctx, &intr->src[0], 0, nir_type_uint);

   unsigned align = nir_dest_bit_size(intr->dest) / 8;
   const struct dxil_value *retval =
      dxil_emit_load(&ctx->mod, ptr, align, false);

   store_dest(ctx, &intr->dest, 0, retval, nir_type_uint);
   return true;
}

static bool
emit_load_mem_ubo(struct ntd_context *ctx, nir_intrinsic_instr *intr,
                  nir_variable *var)
{
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   const struct dxil_value *ptr =
      get_src(ctx, &intr->src[0], 0, nir_type_uint);
   const struct dxil_type *type = dxil_module_get_int_type(&ctx->mod,
      bit_size);
   unsigned align = bit_size / 8;

   const struct dxil_value *handle = ctx->cbv_handles[var->data.binding];
   assert(handle);
   const struct dxil_value *index = offset_to_index(&ctx->mod, ptr, bit_size * 4);
   const struct dxil_value *agg = load_ubo(ctx, handle, index, DXIL_I32);
   if (!index || !agg)
      return false;

   /* It seems we can't take that address of a struct (in this case
    * %dx.types.CBufRet.i32)??? And we can only extract values from constant
    * indices using extractval. So here's a glorious hack: we use a three
    * level tree of compares and selects to pick the right component!
    */
   const struct dxil_value *comp = offset_to_index(&ctx->mod, ptr, bit_size);
   const struct dxil_value *comp_mask1 = dxil_module_get_int32_const(&ctx->mod, 0x1);
   const struct dxil_value *comp_mask2 = dxil_module_get_int32_const(&ctx->mod, 0x2);

   const struct dxil_value *comp_x = dxil_emit_extractval(&ctx->mod, agg, 0);
   const struct dxil_value *comp_y = dxil_emit_extractval(&ctx->mod, agg, 1);
   const struct dxil_value *comp_z = dxil_emit_extractval(&ctx->mod, agg, 2);
   const struct dxil_value *comp_w = dxil_emit_extractval(&ctx->mod, agg, 3);

   const struct dxil_value *bit1 = dxil_emit_binop(&ctx->mod, DXIL_BINOP_AND, comp, comp_mask1, 0);
   const struct dxil_value *bit2 = dxil_emit_binop(&ctx->mod, DXIL_BINOP_AND, comp, comp_mask2, 0);
   const struct dxil_value *sel1 = dxil_emit_cmp(&ctx->mod, DXIL_ICMP_NE, bit1, comp_mask1);
   const struct dxil_value *sel2 = dxil_emit_cmp(&ctx->mod, DXIL_ICMP_NE, bit2, comp_mask2);

   const struct dxil_value *comp_xy = dxil_emit_select(&ctx->mod, sel1, comp_x, comp_y);
   const struct dxil_value *comp_zw = dxil_emit_select(&ctx->mod, sel1, comp_z, comp_w);
   const struct dxil_value *retval = dxil_emit_select(&ctx->mod, sel2, comp_xy, comp_zw);

   store_dest(ctx, &intr->dest, 0, retval, nir_type_uint);
   return true;
}

static bool
emit_load_deref(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   assert(intr->src[0].is_ssa);
   nir_deref_instr *deref = nir_instr_as_deref(intr->src[0].ssa->parent_instr);
   nir_variable *var = nir_deref_instr_get_variable(deref);

   switch (var->data.mode) {
   case nir_var_shader_in:
      return emit_load_input(ctx, intr, var);

   case nir_var_function_temp:
      return emit_load_function_temp(ctx, intr, var);

   case nir_var_mem_ubo:
      return emit_load_mem_ubo(ctx, intr, var);

   default:
      unreachable("unsupported nir_variable_mode");
   }
}

static bool
emit_discard_if_with_value(struct ntd_context *ctx, const struct dxil_value *value)
{
   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_DISCARD);
   if (!opcode)
      return false;

   const struct dxil_value *args[] = {
     opcode,
     value
   };

   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.discard", DXIL_NONE);
   if (!func)
      return false;

   return dxil_emit_call_void(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static bool
emit_discard_if(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_value *value = get_src(ctx, &intr->src[0], 0, nir_type_bool);
   return emit_discard_if_with_value(ctx, value);
}

static bool
emit_discard(struct ntd_context *ctx)
{
   const struct dxil_value *value = dxil_module_get_int1_const(&ctx->mod, true);
   return emit_discard_if_with_value(ctx, value);
}

static bool
emit_emit_vertex(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_EMIT_STREAM);
   const struct dxil_value *stream_id = dxil_module_get_int8_const(&ctx->mod, nir_intrinsic_stream_id(intr));
   if (!opcode || !stream_id)
      return false;

   const struct dxil_value *args[] = {
     opcode,
     stream_id
   };

   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.emitStream", DXIL_NONE);
   if (!func)
      return false;

   return dxil_emit_call_void(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static bool
emit_end_primitive(struct ntd_context *ctx, nir_intrinsic_instr *intr)
{
   const struct dxil_value *opcode = dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_CUT_STREAM);
   const struct dxil_value *stream_id = dxil_module_get_int8_const(&ctx->mod, nir_intrinsic_stream_id(intr));
   if (!opcode || !stream_id)
      return false;

   const struct dxil_value *args[] = {
     opcode,
     stream_id
   };

   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.cutStream", DXIL_NONE);
   if (!func)
      return false;

   return dxil_emit_call_void(&ctx->mod, func, args, ARRAY_SIZE(args));
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
   case nir_intrinsic_load_kernel_input:
      return emit_load_kernel_input(ctx, intr);
   case nir_intrinsic_load_global:
      return emit_load_global(ctx, intr);
   case nir_intrinsic_store_global:
      return emit_store_global(ctx, intr);
   case nir_intrinsic_store_deref:
      return emit_store_deref(ctx, intr);
   case nir_intrinsic_load_deref:
      return emit_load_deref(ctx, intr);
   case nir_intrinsic_load_ubo:
      return emit_load_ubo(ctx, intr);
   case nir_intrinsic_load_front_face:
      assert(ctx->ps_front_face);
      return emit_load_input_interpolated(ctx, intr, ctx->ps_front_face);
   case nir_intrinsic_discard_if:
      return emit_discard_if(ctx, intr);
   case nir_intrinsic_discard:
      return emit_discard(ctx);
   case nir_intrinsic_emit_vertex:
      return emit_emit_vertex(ctx, intr);
   case nir_intrinsic_end_primitive:
      return emit_end_primitive(ctx, intr);

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
      case 16:
         ctx->mod.feats.min_precision = true;
         value = dxil_module_get_int16_const(&ctx->mod,
                                             load_const->value[i].u16);
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
emit_deref_array(struct ntd_context *ctx, nir_deref_instr *deref)
{
   assert(deref->deref_type == nir_deref_type_array);
   nir_variable *var = nir_deref_instr_get_variable(deref);

   const struct dxil_value *index =
      get_src(ctx, &deref->arr.index, 0, nir_type_uint);
   if (!index)
      return false;

   const struct dxil_value *ptr;
   switch (var->data.mode) {
   case nir_var_function_temp: {
         struct hash_entry *he =
            _mesa_hash_table_search(ctx->locals, var);
         assert(he != NULL);
         ptr = he->data;
      }
      break;

   case nir_var_mem_ubo: {
         // HACK: force CBV#0
         unsigned bit_size = glsl_get_bit_size(glsl_without_array(var->type));
         ptr = index_to_offset(&ctx->mod, index, bit_size);
         store_dest_int(ctx, &deref->dest, 0, ptr);
         return true;
      }

   default:
      unreachable("unexpected variable-mode");
   }

   const struct dxil_value *zero = dxil_module_get_int32_const(&ctx->mod, 0);
   if (!zero)
      return false;

   const struct dxil_value *ops[] = { ptr, zero, index };
   ptr = dxil_emit_gep_inbounds(&ctx->mod, ops, ARRAY_SIZE(ops));
   if (!ptr)
      return false;

   store_dest_int(ctx, &deref->dest, 0, ptr);
   return true;
}

static bool
emit_deref(struct ntd_context* ctx, nir_deref_instr* instr)
{
   switch (instr->deref_type) {
   case nir_deref_type_var:
      return true;

   case nir_deref_type_array:
      return emit_deref_array(ctx, instr);

   default:
      ;
   }
   NIR_INSTR_UNSUPPORTED(&instr->instr);
   return false;
}

static bool
emit_cond_branch(struct ntd_context *ctx, const struct dxil_value *cond,
                 int true_block, int false_block)
{
   assert(cond);
   assert(true_block >= 0);
   assert(false_block >= 0);
   return dxil_emit_branch(&ctx->mod, cond, true_block, false_block);
}

static bool
emit_branch(struct ntd_context *ctx, int block)
{
   assert(block >= 0);
   return dxil_emit_branch(&ctx->mod, NULL, block, -1);
}

static bool
emit_jump(struct ntd_context *ctx, nir_jump_instr *instr)
{
   switch (instr->type) {
   case nir_jump_break:
   case nir_jump_continue:
      assert(instr->instr.block->successors[0]);
      assert(!instr->instr.block->successors[1]);
      return emit_branch(ctx, instr->instr.block->successors[0]->index);

   default:
      unreachable("Unsupported jump type\n");
   }
}

struct phi_block {
   unsigned num_components;
   struct dxil_instr *comp[4];
};

static bool
emit_phi(struct ntd_context *ctx, nir_phi_instr *instr)
{
   unsigned bit_size = nir_dest_bit_size(instr->dest);
   const struct dxil_type *type = dxil_module_get_int_type(&ctx->mod,
                                                           bit_size);

   struct phi_block *vphi = ralloc(ctx->phis, struct phi_block);
   vphi->num_components = nir_dest_num_components(instr->dest);

   for (unsigned i = 0; i < vphi->num_components; ++i) {
      struct dxil_instr *phi = vphi->comp[i] = dxil_emit_phi(&ctx->mod, type);
      if (!phi)
         return false;
      store_dest_int(ctx, &instr->dest, i, dxil_instr_get_return_value(phi));
   }
   _mesa_hash_table_insert(ctx->phis, instr, vphi);
   return true;
}

static void
fixup_phi(struct ntd_context *ctx, nir_phi_instr *instr,
          struct phi_block *vphi)
{
   const struct dxil_value *values[128];
   unsigned blocks[128];
   for (unsigned i = 0; i < vphi->num_components; ++i) {
      size_t num_incoming = 0;
      nir_foreach_phi_src(src, instr) {
         assert(src->src.is_ssa);
         const struct dxil_value *val = get_src_ssa(ctx, src->src.ssa, i);
         assert(num_incoming < ARRAY_SIZE(values));
         values[num_incoming] = val;
         assert(num_incoming < ARRAY_SIZE(blocks));
         blocks[num_incoming] = src->pred->index;
         ++num_incoming;
      }
      dxil_phi_set_incoming(vphi->comp[i], values, blocks, num_incoming);
   }
}

static unsigned
get_n_src(struct ntd_context *ctx, const struct dxil_value **values,
          unsigned max_components, nir_tex_src *src, nir_alu_type type)
{
   unsigned num_components = nir_src_num_components(src->src);
   unsigned i = 0;

   assert(num_components <= max_components);

   for (i = 0; i < num_components; ++i) {
      values[i] = get_src(ctx, &src->src, i, type);
      assert(values[i] != NULL);
   }

   return num_components;
}

#define PAD_SRC(ctx, array, undef) \
   for (unsigned i = array ## _components; i < ARRAY_SIZE(array); ++i) { \
      array[i] = undef; \
   }

static const struct dxil_value *
emit_sample(struct ntd_context *ctx, const struct dxil_value *tex,
            const struct dxil_value *sampler, const struct dxil_value **coord,
            const struct dxil_value **offset, const struct dxil_value *min_lod)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.sample", DXIL_F32);
   if (!func)
      return NULL;

   const struct dxil_value *args[11] = {
      dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_SAMPLE),
      tex, sampler,
      coord[0], coord[1], coord[2], coord[3],
      offset[0], offset[1], offset[2],
      min_lod
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_sample_bias(struct ntd_context *ctx, const struct dxil_value *tex,
                 const struct dxil_value *sampler, const struct dxil_value **coord,
                 const struct dxil_value **offset, const struct dxil_value *bias,
                 const struct dxil_value *min_lod)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.sampleBias", DXIL_F32);
   if (!func)
      return NULL;

   assert(bias != NULL);

   const struct dxil_value *args[12] = {
      dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_SAMPLE_BIAS),
      tex, sampler,
      coord[0], coord[1], coord[2], coord[3],
      offset[0], offset[1], offset[2],
      bias, min_lod
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_sample_level(struct ntd_context *ctx, const struct dxil_value *tex,
                  const struct dxil_value *sampler, const struct dxil_value **coord,
                  const struct dxil_value **offset, const struct dxil_value *lod)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.sampleLevel", DXIL_F32);
   if (!func)
      return NULL;

   assert(lod != NULL);

   const struct dxil_value *args[11] = {
      dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_SAMPLE_LEVEL),
      tex, sampler,
      coord[0], coord[1], coord[2], coord[3],
      offset[0], offset[1], offset[2],
      lod
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_sample_cmp(struct ntd_context *ctx, const struct dxil_value *tex,
                const struct dxil_value *sampler, const struct dxil_value **coord,
                const struct dxil_value **offset, const struct dxil_value *cmp,
                const struct dxil_value *min_lod)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.sampleCmp", DXIL_F32);
   if (!func)
      return NULL;

   const struct dxil_value *args[12] = {
      dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_SAMPLE_CMP),
      tex, sampler,
      coord[0], coord[1], coord[2], coord[3],
      offset[0], offset[1], offset[2],
      cmp, min_lod
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_sample_grad(struct ntd_context *ctx, const struct dxil_value *tex,
                 const struct dxil_value *sampler, const struct dxil_value **coord,
                 const struct dxil_value **offset, const struct dxil_value **dx,
                 const struct dxil_value **dy, const struct dxil_value *min_lod)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.sampleGrad", DXIL_F32);
   if (!func)
      return false;

   const struct dxil_value *args[17] = {
      dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_SAMPLE_GRAD),
      tex, sampler,
      coord[0], coord[1], coord[2], coord[3],
      offset[0], offset[1], offset[2],
      dx[0], dx[1], dx[2], dy[0], dy[1], dy[2],
      min_lod
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}

static const struct dxil_value *
emit_texel_fetch(struct ntd_context *ctx, const struct dxil_value *tex,
                 const struct dxil_value **coord,
                 const struct dxil_value **offset, const struct dxil_value *lod)
{
   const struct dxil_func *func = dxil_get_function(&ctx->mod, "dx.op.textureLoad", DXIL_F32);
   if (!func)
      return false;

   if (!lod)
      lod = dxil_module_get_undef(&ctx->mod, dxil_module_get_int_type(&ctx->mod, 32));

   const struct dxil_value *args[] = {
      dxil_module_get_int32_const(&ctx->mod, DXIL_INTR_TEXTURE_LOAD),
      tex,
      lod, coord[0], coord[1], coord[2],
      offset[0], offset[1], offset[2]
   };

   return dxil_emit_call(&ctx->mod, func, args, ARRAY_SIZE(args));
}


static bool
emit_tex(struct ntd_context *ctx, nir_tex_instr *instr)
{
   assert(instr->texture_index == instr->sampler_index);
   assert(nir_alu_type_get_base_type(instr->dest_type) == nir_type_float);

   assert(instr->texture_index < ctx->num_srvs);
   const struct dxil_value *tex = ctx->srv_handles[instr->texture_index];
   const struct dxil_value *sampler = ctx->sampler_handles[instr->texture_index];

   const struct dxil_type *int_type = dxil_module_get_int_type(&ctx->mod, 32);
   const struct dxil_type *float_type = dxil_module_get_float_type(&ctx->mod, 32);
   const struct dxil_value *int_undef = dxil_module_get_undef(&ctx->mod, int_type);
   const struct dxil_value *float_undef = dxil_module_get_undef(&ctx->mod, float_type);

   const struct dxil_value *bias = NULL, *lod = NULL, *dref = NULL, *min_lod = NULL;
   const struct dxil_value *coord[4], *offset[3], *dx[3], *dy[3];
   unsigned coord_components = 0, offset_components = 0, dx_components = 0, dy_components = 0;

   for (unsigned i = 0; i < instr->num_srcs; i++) {
      nir_alu_type type = nir_tex_instr_src_type(instr, i);

      switch (instr->src[i].src_type) {
      case nir_tex_src_coord:
         coord_components = get_n_src(ctx, coord, ARRAY_SIZE(coord), &instr->src[i], type);
         break;

      case nir_tex_src_offset:
         offset_components = get_n_src(ctx, offset, ARRAY_SIZE(offset), &instr->src[i],  nir_type_int);
         break;

      case nir_tex_src_bias:
         assert(instr->op == nir_texop_txb);
         assert(nir_src_num_components(instr->src[i].src) == 1);
         bias = get_src(ctx, &instr->src[i].src, 0, nir_type_float);
         assert(bias != NULL);
         break;

      case nir_tex_src_lod:
         assert(nir_src_num_components(instr->src[i].src) == 1);
         lod = get_src(ctx, &instr->src[i].src, 0, type);
         assert(lod != NULL);
         break;

      case nir_tex_src_min_lod:
         assert(nir_src_num_components(instr->src[i].src) == 1);
         min_lod = get_src(ctx, &instr->src[i].src, 0, type);
         assert(min_lod != NULL);
         break;

      case nir_tex_src_comparator:
         assert(nir_src_num_components(instr->src[i].src) == 1);
         dref = get_src(ctx, &instr->src[i].src, 0, nir_type_float);
         assert(dref != NULL);
         break;

      case nir_tex_src_ddx:
         dx_components = get_n_src(ctx, dx, ARRAY_SIZE(dx),
                                   &instr->src[i], nir_type_float);
         assert(dx_components != 0);
         break;

      case nir_tex_src_ddy:
         dy_components = get_n_src(ctx, dy, ARRAY_SIZE(dy),
                                   &instr->src[i], nir_type_float);
         assert(dy_components != 0);
         break;

      case nir_tex_src_projector:
         unreachable("Texture projector should have been lowered");

      default:
         fprintf(stderr, "texture source: %d\n", instr->src[i].src_type);
         unreachable("unknown texture source");
      }
   }

   PAD_SRC(ctx, coord, float_undef);
   PAD_SRC(ctx, offset, int_undef);
   if (!min_lod) min_lod = float_undef;

   const struct dxil_value *sample = NULL;
   switch (instr->op) {
   case nir_texop_tex:
      if (dref != NULL)
         sample = emit_sample_cmp(ctx, tex, sampler, coord, offset, dref, min_lod);
      else
         sample = emit_sample(ctx, tex, sampler, coord, offset, min_lod);
      break;

   case nir_texop_txb:
      sample = emit_sample_bias(ctx, tex, sampler, coord, offset, bias, min_lod);
      break;

   case nir_texop_txl:
      sample = emit_sample_level(ctx, tex, sampler, coord, offset, lod);
      break;

   case nir_texop_txd:
      PAD_SRC(ctx, dx, float_undef);
      PAD_SRC(ctx, dy, float_undef);
      sample = emit_sample_grad(ctx, tex, sampler, coord, offset, dx, dy, min_lod);
      break;

   case nir_texop_txf:
      PAD_SRC(ctx, coord, int_undef);
      sample = emit_texel_fetch(ctx, tex, coord, offset, lod ? lod : int_undef);
      break;
   }

   if (!sample)
      return false;

   for (unsigned i = 0; i < nir_dest_num_components(instr->dest); ++i) {
      const struct dxil_value *retval = dxil_emit_extractval(&ctx->mod, sample, i);
      store_dest(ctx, &instr->dest, i, retval, nir_type_float);
   }

   return true;
}

static bool
emit_undefined(struct ntd_context *ctx, nir_ssa_undef_instr *undef)
{
   for (unsigned i = 0; i < undef->def.num_components; ++i)
      store_ssa_def(ctx, &undef->def, i, dxil_module_get_int32_const(&ctx->mod, 0));
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
   case nir_instr_type_deref:
      return emit_deref(ctx, nir_instr_as_deref(instr));
   case nir_instr_type_jump:
      return emit_jump(ctx, nir_instr_as_jump(instr));
   case nir_instr_type_phi:
      return emit_phi(ctx, nir_instr_as_phi(instr));
   case nir_instr_type_tex:
      return emit_tex(ctx, nir_instr_as_tex(instr));
   case nir_instr_type_ssa_undef:
      return emit_undefined(ctx, nir_instr_as_ssa_undef(instr));
   default:
      NIR_INSTR_UNSUPPORTED(instr);
      assert("Unimplemented instruction type");
      return false;
   }
}


static bool
emit_block(struct ntd_context *ctx, struct nir_block *block)
{
   assert(block->index < ctx->mod.num_basic_block_ids);
   ctx->mod.basic_block_ids[block->index] = ctx->mod.curr_block;

   nir_foreach_instr(instr, block) {
      TRACE_CONVERSION(instr);

      if (!emit_instr(ctx, instr))  {
         return false;
      }
   }
   return true;
}

static bool
emit_cf_list(struct ntd_context *ctx, struct exec_list *list);

static bool
emit_if(struct ntd_context *ctx, struct nir_if *if_stmt)
{
   assert(nir_src_num_components(if_stmt->condition) == 1);
   const struct dxil_value *cond = get_src(ctx, &if_stmt->condition, 0,
                                           nir_type_bool);

   /* prepare blocks */
   nir_block *then_block = nir_if_first_then_block(if_stmt);
   assert(nir_if_last_then_block(if_stmt)->successors[0]);
   assert(!nir_if_last_then_block(if_stmt)->successors[1]);
   int then_succ = nir_if_last_then_block(if_stmt)->successors[0]->index;

   nir_block *else_block = NULL;
   int else_succ = -1;
   if (!exec_list_is_empty(&if_stmt->else_list)) {
      else_block = nir_if_first_else_block(if_stmt);
      assert(nir_if_last_else_block(if_stmt)->successors[0]);
      assert(!nir_if_last_else_block(if_stmt)->successors[1]);
      else_succ = nir_if_last_else_block(if_stmt)->successors[0]->index;
   }

   if (!emit_cond_branch(ctx, cond, then_block->index,
                         else_block ? else_block->index : then_succ))
      return false;

   /* handle then-block */
   if (!emit_cf_list(ctx, &if_stmt->then_list) ||
       (!nir_block_ends_in_jump(nir_if_last_then_block(if_stmt)) &&
        !emit_branch(ctx, then_succ)))
      return false;

   if (else_block) {
      /* handle else-block */
      if (!emit_cf_list(ctx, &if_stmt->else_list) ||
          (!nir_block_ends_in_jump(nir_if_last_else_block(if_stmt)) &&
           !emit_branch(ctx, else_succ)))
         return false;
   }

   return true;
}

static bool
emit_loop(struct ntd_context *ctx, nir_loop *loop)
{
   nir_block *first_block = nir_loop_first_block(loop);

   assert(nir_loop_last_block(loop)->successors[0]);
   assert(!nir_loop_last_block(loop)->successors[1]);

   if (!emit_branch(ctx, first_block->index))
      return false;

   if (!emit_cf_list(ctx, &loop->body))
      return false;

   if (!emit_branch(ctx, first_block->index))
      return false;

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

      case nir_cf_node_if:
         if (!emit_if(ctx, nir_cf_node_as_if(node)))
            return false;
         break;

      case nir_cf_node_loop:
         if (!emit_loop(ctx, nir_cf_node_as_loop(node)))
            return false;
         break;

      default:
         unreachable("unsupported cf-list node");
         break;
      }
   }
   return true;
}

static void insert_sorted_by_binding(struct exec_list *var_list, nir_variable *new_var)
{
   nir_foreach_variable(var, var_list) {
      if (var->data.binding > new_var->data.binding) {
         exec_node_insert_node_before(&var->node, &new_var->node);
         return;
      }
   }
   exec_list_push_tail(var_list, &new_var->node);
}


static void sort_uniforms_by_binding(struct exec_list *uniforms)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   nir_foreach_variable_safe(var, uniforms) {
      exec_node_remove(&var->node);
      insert_sorted_by_binding(&new_list, var);
   }
   exec_list_move_nodes_to(&new_list, uniforms);
}

static bool
emit_module(struct ntd_context *ctx, nir_shader *s)
{
   /* The validator forces us to emit resources in a specific order:
    * CBVs, Samplers, SRVs, UAVs */

   sort_uniforms_by_binding(&s->uniforms);

   /* CBVs */
   nir_foreach_variable(var, &s->uniforms) {
      if (var->data.mode == nir_var_mem_ubo) {
         if (!emit_cbv(ctx, var))
            return false;
      }
   }

   if (s->info.stage == MESA_SHADER_KERNEL)
      if (!emit_kernel_inputs_cbv(ctx, s))
         return false;

   /* Samplers */
   nir_foreach_variable(var, &s->uniforms) {
      if (var->data.mode == nir_var_uniform && glsl_type_get_sampler_count(var->type)) {
            if (!emit_sampler(ctx, var))
               return false;
      }
   }

   /* SRVs */
   nir_foreach_variable(var, &s->uniforms) {
      if (var->data.mode == nir_var_uniform && glsl_type_get_sampler_count(var->type)) {
         if (!emit_srv(ctx, var))
            return false;
      }
   }

   if (s->info.stage == MESA_SHADER_KERNEL) {
      if (s->info.cs.global_inputs &&
          !emit_globals(ctx, s->info.cs.global_inputs))
         return false;
   }

   nir_function_impl *entry = nir_shader_get_entrypoint(s);
   nir_metadata_require(entry, nir_metadata_block_index);

   assert(entry->num_blocks > 0);
   ctx->mod.basic_block_ids = rzalloc_array(ctx->ralloc_ctx, int,
                                            entry->num_blocks);
   if (!ctx->mod.basic_block_ids)
      return false;

   for (int i = 0; i < entry->num_blocks; ++i)
      ctx->mod.basic_block_ids[i] = -1;
   ctx->mod.num_basic_block_ids = entry->num_blocks;

   ctx->defs = rzalloc_array(ctx->ralloc_ctx, struct dxil_def,
                             entry->ssa_alloc);
   if (!ctx->defs)
      return false;
   ctx->num_defs = entry->ssa_alloc;

   ctx->phis = _mesa_pointer_hash_table_create(ctx->ralloc_ctx);
   if (!ctx->phis)
      return false;

   ctx->locals = _mesa_pointer_hash_table_create(ctx->ralloc_ctx);
   if (!ctx->locals)
      return false;

   nir_foreach_variable(var, &entry->locals) {
      if (glsl_type_is_array(var->type)) {
         assert(!glsl_type_is_unsized_array(var->type));
         const struct dxil_type *alloc_type =
            get_glsl_type(&ctx->mod, var->type);

         const struct dxil_type *size_type =
            dxil_module_get_int_type(&ctx->mod, 32);
         const struct dxil_value *size =
            dxil_module_get_int32_const(&ctx->mod,
                                        glsl_get_length(var->type));

         // TODO: find a way of moving CL-semantics out of nir_to_dxil?
         assert(s->info.stage == MESA_SHADER_KERNEL);
         unsigned align =
            glsl_get_cl_alignment(glsl_get_array_element(var->type));

         const struct dxil_value *ptr =
            dxil_emit_alloca(&ctx->mod, alloc_type, size_type, size, align);
         _mesa_hash_table_insert(ctx->locals, var, (void *)ptr);
      }
   }

   if (!emit_cf_list(ctx, &entry->body))
      return false;

   hash_table_foreach(ctx->phis, entry) {
      fixup_phi(ctx, (nir_phi_instr *)entry->key,
                (struct phi_block *)entry->data);
   }

   if (!dxil_emit_ret_void(&ctx->mod))
      return false;

   return emit_metadata(ctx, s) &&
          dxil_emit_module(&ctx->mod);
}

unsigned int
get_dxil_shader_kind(struct nir_shader *s)
{
   switch (s->info.stage) {
   case MESA_SHADER_VERTEX:
      return DXIL_VERTEX_SHADER;
   case MESA_SHADER_GEOMETRY:
      return DXIL_GEOMETRY_SHADER;
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
      NIR_PASS(progress, s, nir_opt_if, true);
      NIR_PASS(progress, s, nir_opt_dead_cf);
      NIR_PASS(progress, s, nir_opt_cse);
      NIR_PASS(progress, s, nir_opt_peephole_select, 8, true, true);
      NIR_PASS(progress, s, nir_opt_algebraic);
      NIR_PASS(progress, s, nir_opt_constant_folding);
      NIR_PASS(progress, s, nir_opt_undef);
      NIR_PASS(progress, s, nir_opt_deref);
      NIR_PASS_V(s, nir_lower_system_values);
   } while (progress);

   do {
      progress = false;
      NIR_PASS(progress, s, nir_opt_algebraic_late);
   } while (progress);
}

static
void dxil_fill_validation_state(struct ntd_context *ctx,
                                nir_shader *s,
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
      state->state.psv0.ps.depth_output =  ctx->mod.info.has_out_depth;
      /* just guessing */
      state->state.psv0.ps.sample_frequency = 0;
      break;
   case DXIL_COMPUTE_SHADER:
      break;
   case DXIL_GEOMETRY_SHADER:
      state->state.max_vertex_count = s->info.gs.vertices_out;
      state->state.psv0.gs.input_primitive = dxil_get_input_primitive(s->info.gs.input_primitive);
      state->state.psv0.gs.output_toplology = dxil_get_primitive_topology(s->info.gs.output_primitive);
      state->state.psv0.gs.output_stream_mask = s->info.gs.active_stream_mask;
      state->state.psv0.gs.output_position_present = ctx->mod.info.has_out_position;
      break;
   default:
      assert(0 && "Shader type not (yet) supported");
   }
}

static nir_variable *
fs_append_front_face_input(nir_shader *s)
{
   nir_foreach_variable(var, &s->inputs) {
      if (var->data.location == VARYING_SLOT_FACE)
         return var;
   }

   nir_variable *ff = rzalloc(s, nir_variable);
   if (!ff)
      return NULL;
   ff->data.driver_location = exec_list_length(&s->inputs);
   ff->data.location = VARYING_SLOT_FACE;
   ff->type = glsl_uint_type();
   ff->name = "IsFrontFace";
   ff->data.mode = nir_var_shader_in;
   ff->data.interpolation = INTERP_MODE_FLAT;
   exec_list_push_tail(&s->inputs, &ff->node);

   return ff;
}

bool
nir_to_dxil(struct nir_shader *s, const struct nir_to_dxil_options *opts,
            struct blob *blob)
{
   assert(opts);
   bool retval = true;
   debug_dxil = (int)debug_get_option_debug_dxil();

   struct ntd_context ctx = { 0 };
   ctx.opts = opts;

   ctx.ralloc_ctx = ralloc_context(NULL);
   if (!ctx.ralloc_ctx)
      return false;

   dxil_module_init(&ctx.mod, ctx.ralloc_ctx);
   ctx.mod.shader_kind = get_dxil_shader_kind(s);
   ctx.mod.major_version = 6;
   ctx.mod.minor_version = 1;

   NIR_PASS_V(s, nir_lower_frexp);

   optimize_nir(s);

   NIR_PASS_V(s, nir_remove_dead_variables, nir_var_function_temp);

   if ((s->info.stage == MESA_SHADER_FRAGMENT) &&
       (s->info.system_values_read & (1ull << SYSTEM_VALUE_FRONT_FACE))) {
      ctx.ps_front_face = fs_append_front_face_input(s);
      if (!ctx.ps_front_face) {
         retval = false;
         goto out;
      }
   }

   if (debug_dxil & DXIL_DEBUG_VERBOSE)
      nir_print_shader(s, stderr);

   if (!emit_module(&ctx, s)) {
      debug_printf("D3D12: dxil_container_add_module failed\n");
      retval = false;
      goto out;
   }

   if (debug_dxil & DXIL_DEBUG_DUMP_MODULE) {
      struct dxil_dumper *dumper = dxil_dump_create();
      dxil_dump_module(dumper, &ctx.mod);
      fprintf(stderr, "\n");
      dxil_dump_buf_to_file(dumper, stderr);
      fprintf(stderr, "\n\n");
      dxil_dump_free(dumper);
   }

   struct dxil_container container;
   dxil_container_init(&container);
   if (!dxil_container_add_features(&container, &ctx.mod.feats)) {
      debug_printf("D3D12: dxil_container_add_features failed\n");
      retval = false;
      goto out;
   }

   if (!dxil_container_add_io_signature(&container,
                                        DXIL_ISG1,
                                        ctx.mod.num_sig_inputs,
                                        ctx.mod.inputs)) {
      debug_printf("D3D12: failed to write input signature\n");
      retval = false;
      goto out;
   }

   if (!dxil_container_add_io_signature(&container,
                                        DXIL_OSG1,
                                        ctx.mod.num_sig_outputs,
                                        ctx.mod.outputs)) {
      debug_printf("D3D12: failed to write output signature\n");
      retval = false;
      goto out;
   }

   struct dxil_validation_state validation_state;
   memset(&validation_state, 0, sizeof(validation_state));
   dxil_fill_validation_state(&ctx, s, &validation_state);

   if (!dxil_container_add_state_validation(&container,&ctx.mod,
                                            &validation_state)) {
      debug_printf("D3D12: failed to write state-validation\n");
      retval = false;
      goto out;
   }

   if (!dxil_container_add_module(&container, &ctx.mod)) {
      debug_printf("D3D12: failed to write module\n");
      retval = false;
      goto out;
   }

   blob_init(blob);
   if (!dxil_container_write(&container, blob)) {
      debug_printf("D3D12: dxil_container_write failed\n");
      retval = false;
      goto out;
   }
   dxil_container_finish(&container);

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

out:
   dxil_module_release(&ctx.mod);
   ralloc_free(ctx.ralloc_ctx);
   return retval;
}

static const char *generics_semantics[] = {
   "GENERICAA", "GENERICAB", "GENERICAC", "GENERIAD",
   "GENERICAE", "GENERICAF", "GENERICAG", "GENERIAG",
   "GENERICBA", "GENERICBB", "GENERICBC", "GENERIBD",
   "GENERICBE", "GENERICBF", "GENERICBG", "GENERIBG",
   "GENERICCA", "GENERICCB", "GENERICCC", "GENERICD",
   "GENERICCE", "GENERICCF", "GENERICCG", "GENERICG",
   "GENERICDA", "GENERICDB", "GENERICDC", "GENERIDD",
   "GENERICDE", "GENERICDF", "GENERICDG", "GENERIDG"
};

const char *
dxil_vs_attr_index_to_name(unsigned index)
{
   assert(index < 32);
   return generics_semantics[index];
}

enum dxil_sysvalue_type
nir_var_to_dxil_sysvalue_type(nir_variable *var)
{
   switch (var->data.location) {
   case VARYING_SLOT_POS:
   case VARYING_SLOT_CLIP_DIST0:
   case VARYING_SLOT_CLIP_DIST1:
      return DXIL_SYSVALUE;
   case VARYING_SLOT_FACE:
      return DXIL_GENERATED_SYSVALUE;
   default:
      return DXIL_NO_SYSVALUE;
   }
}
