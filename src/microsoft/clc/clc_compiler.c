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
   const dxil_value pointer_undef = dxil_module_get_undef(m, pointer_type);

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

static bool
emit_module(struct dxil_module *m)
{
   if (!dxil_module_emit_bits(m, 'B', 8) ||
       !dxil_module_emit_bits(m, 'C', 8) ||
       !dxil_module_emit_bits(m, 0xC0, 8) ||
       !dxil_module_emit_bits(m, 0xDE, 8))
      return false;

   if (!dxil_module_enter_subblock(m, DXIL_MODULE, 3) ||
       !dxil_module_emit_record_int(m, DXIL_MODULE_CODE_VERSION, 1))
      return false;

   const struct dxil_type *int32_type = dxil_module_get_int_type(m, 32);
   const struct dxil_type *rwbuffer_struct_type = dxil_module_get_struct_type(m, "class.RWBuffer<unsigned int>", &int32_type, 1);

   const struct dxil_type *void_type = dxil_module_get_void_type(m);
   const struct dxil_type *main_func_type = dxil_module_add_function_type(m, void_type, NULL, 0);
   const struct dxil_type *main_func_pointer_type = dxil_module_get_pointer_type(m, main_func_type);

   const struct dxil_type *threadid_arg_types[] = { int32_type, int32_type };
   const struct dxil_type *threadid_func_type = dxil_module_add_function_type(m, int32_type, threadid_arg_types, ARRAY_SIZE(threadid_arg_types));

   const struct dxil_type *int8_type = dxil_module_get_int_type(m, 8);
   const struct dxil_type *handle_type = get_dx_handle_type(m);
   const struct dxil_type *bufferstore_arg_types[] = { int32_type, handle_type, int32_type, int32_type, int32_type, int32_type, int32_type, int32_type, int8_type };
   const struct dxil_type *bufferstore_func_type = dxil_module_add_function_type(m, void_type, bufferstore_arg_types, ARRAY_SIZE(bufferstore_arg_types));

   const struct dxil_type *int1_type = dxil_module_get_int_type(m, 1);
   const struct dxil_type *createhandle_arg_types[] = { int32_type, int8_type, int32_type, int32_type, int1_type };
   const struct dxil_type *createhandle_func_type = dxil_module_add_function_type(m, handle_type, createhandle_arg_types, ARRAY_SIZE(createhandle_arg_types));

   const dxil_value output_buffer_gvar = dxil_add_global_var(m, rwbuffer_struct_type, true, 3);
   if (output_buffer_gvar == DXIL_VALUE_INVALID)
      return false;

   const dxil_value main_func = dxil_add_function_def(m, "main", main_func_type);
   const dxil_value threadid_func = dxil_add_function_decl(m, "dx.op.threadId.i32", threadid_func_type, DXIL_ATTR_KIND_READ_NONE);
   const dxil_value bufferstore_func = dxil_add_function_decl(m, "dx.op.bufferStore.i32", bufferstore_func_type, DXIL_ATTR_KIND_NONE);
   const dxil_value createhandle_func = dxil_add_function_decl(m, "dx.op.createHandle", createhandle_func_type, DXIL_ATTR_KIND_READ_ONLY);
   if (main_func == DXIL_VALUE_INVALID ||
       threadid_func == DXIL_VALUE_INVALID ||
       bufferstore_func == DXIL_VALUE_INVALID ||
       createhandle_func == DXIL_VALUE_INVALID)
      return false;

   if (!emit_llvm_ident(m) ||
       !emit_dx_versions(m, 1, 0) ||
       !emit_dx_shader_model(m))
      return false;

   const struct dxil_mdnode *output_buffer_node = emit_uav_metadata(m, rwbuffer_struct_type,
                                                                    "OutputBuffer",
                                                                    DXIL_COMP_TYPE_U32);

   const struct dxil_mdnode *uav_metadata = dxil_get_metadata_node(m, &output_buffer_node, 1);

   const struct dxil_mdnode *resources_nodes[] = {
      NULL, uav_metadata, NULL, NULL
   };
   const struct dxil_mdnode *resources_node = dxil_get_metadata_node(m, resources_nodes,
                                                      ARRAY_SIZE(resources_nodes));

   const struct dxil_mdnode *main_entrypoint = dxil_get_metadata_value(m, main_func_pointer_type, main_func);
   const struct dxil_mdnode *node27 = dxil_get_metadata_node(m, NULL, 0);

   const struct dxil_mdnode *node4 = dxil_get_metadata_int32(m, 0);
   const struct dxil_mdnode *nodes_4_27_27[] = {
      node4, node27, node27
   };
   const struct dxil_mdnode *node28 = dxil_get_metadata_node(m, nodes_4_27_27,
                                                      ARRAY_SIZE(nodes_4_27_27));

   const struct dxil_mdnode *node29 = dxil_get_metadata_node(m, &node28, 1);

   const struct dxil_mdnode *node3 = dxil_get_metadata_int32(m, 1);
   const struct dxil_mdnode *main_type_annotation_nodes[] = {
      node3, main_entrypoint, node29
   };
   const struct dxil_mdnode *main_type_annotation = dxil_get_metadata_node(m, main_type_annotation_nodes,
                                                                           ARRAY_SIZE(main_type_annotation_nodes));

   const struct dxil_mdnode *main_name = dxil_get_metadata_string(m, "main");

   const struct dxil_mdnode *nodes_3_3_3[] = { node3, node3, node3 };
   const struct dxil_mdnode *node32 = dxil_get_metadata_node(m, nodes_3_3_3,
                                                      ARRAY_SIZE(nodes_3_3_3));

   const struct dxil_mdnode *node19 = dxil_get_metadata_int32(m, 4);
   const struct dxil_mdnode *nodes_19_32[] = { node19, node32 };
   const struct dxil_mdnode *node33 = dxil_get_metadata_node(m, nodes_19_32,
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
                     *dx_entry_point = dxil_get_metadata_node(m, main_entrypoint_metadata,
                                                              ARRAY_SIZE(main_entrypoint_metadata));

   if (!dxil_add_metadata_named_node(m, "dx.resources",
                                      &dx_resources, 1) ||
       !dxil_add_metadata_named_node(m, "dx.typeAnnotations",
                                 dx_type_annotations,
                                 ARRAY_SIZE(dx_type_annotations)) ||
       !dxil_add_metadata_named_node(m, "dx.entryPoints",
                                      &dx_entry_point, 1))
      return false;

   const dxil_value int32_57 = dxil_module_get_int32_const(m, 57);
   const dxil_value int8_1 = dxil_module_get_int8_const(m, 1);
   const dxil_value int32_0 = dxil_module_get_int32_const(m, 0);
   const dxil_value int32_93 = dxil_module_get_int32_const(m, 93);
   const dxil_value int32_69 = dxil_module_get_int32_const(m, 69);
   const dxil_value int32_undef = dxil_module_get_undef(m, int32_type);
   const dxil_value int8_15 = dxil_module_get_int8_const(m, 15);
   const dxil_value int1_0 = dxil_module_get_int1_const(m, false);

   const dxil_value createhandle_args[] = {
      int32_57, int8_1, int32_0, int32_0, int1_0
   };
   const dxil_value handle = dxil_emit_call(m, createhandle_func_type,
                                            createhandle_func,
                                            createhandle_args,
                                            ARRAY_SIZE(createhandle_args));
   if (handle == DXIL_VALUE_INVALID)
      return false;

   const dxil_value threadid_args[] = {
     int32_93, int32_0
   };
   const dxil_value threadid = dxil_emit_call(m, threadid_func_type,
                                              threadid_func,
                                              threadid_args,
                                              ARRAY_SIZE(threadid_args));
   if (threadid == DXIL_VALUE_INVALID)
      return false;

   const dxil_value bufferstore_args[] = {
     int32_69, handle, threadid, int32_undef,
     threadid, threadid, threadid, threadid, int8_15
   };

   if (!dxil_emit_call_void(m, bufferstore_func_type, bufferstore_func,
                            bufferstore_args,
                            ARRAY_SIZE(bufferstore_args)) ||
       !dxil_emit_ret_void(m))
      return false;

   if (!dxil_module_emit_blockinfo(m) ||
       !dxil_emit_attrib_group_table(m) ||
       !dxil_emit_attribute_table(m) ||
       !dxil_module_emit_type_table(m) ||
       !dxil_emit_module_info(m) ||
       !dxil_emit_module_consts(m))
      return false;

   return dxil_emit_metadata(m) &&
          dxil_emit_value_symbol_table(m) &&
          dxil_emit_function(m) &&
          dxil_module_exit_block(m);
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

   struct dxil_module mod;
   dxil_module_init(&mod);
   mod.shader_kind = DXIL_COMPUTE_SHADER;
   mod.major_version = 6;
   mod.minor_version = 0;
   if (!emit_module(&mod)) {
      debug_printf("D3D12: dxil_container_add_module failed\n");
      return -1;
   }

   if (!dxil_container_add_module(&container, &mod)) {
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
