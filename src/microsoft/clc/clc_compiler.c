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

#include <stdint.h>

static bool
emit_value_symbol_table(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, DXIL_VALUE_SYMTAB_BLOCK, 4) &&
          dxil_module_emit_symtab_entry(m, 1, "main") &&
          dxil_module_emit_symtab_entry(m, 2, "dx.op.threadId.i32") &&
          dxil_module_emit_symtab_entry(m, 3, "dx.op.bufferStore.i32") &&
          dxil_module_emit_symtab_entry(m, 4, "dx.op.createHandle") &&
          dxil_module_exit_block(m);
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

   struct dxil_attrib attrs1[] = {
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_NO_UNWIND },
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_READ_NONE },
   };
   struct dxil_attrib attrs2[] = {
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_NO_UNWIND },
   };
   struct dxil_attrib attrs3[] = {
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_NO_UNWIND },
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_READ_ONLY },
   };

   struct dxil_attrib *attrs[] = {
      attrs1, attrs2, attrs3
   };
   size_t attr_sizes[] = {
      ARRAY_SIZE(attrs1), ARRAY_SIZE(attrs2), ARRAY_SIZE(attrs3)
   };
   assert(ARRAY_SIZE(attrs) == ARRAY_SIZE(attr_sizes));

   unsigned attr_data[] = {
      1, 2, 3
   };

   const struct dxil_type *int32_type = dxil_module_get_int_type(m, 32);
   const struct dxil_type *rwbuffer_struct_type = dxil_module_add_struct_type(m, "class.RWBuffer<unsigned int>", &int32_type, 1);
   const struct dxil_type *rwbuffer_pointer_type = dxil_module_add_pointer_type(m, rwbuffer_struct_type);

   const struct dxil_type *void_type = dxil_module_get_void_type(m);
   const struct dxil_type *main_func_type = dxil_module_add_function_type(m, void_type, NULL, 0);
   const struct dxil_type *main_func_pointer_type = dxil_module_add_pointer_type(m, main_func_type);

   const struct dxil_type *threadid_arg_types[] = { int32_type, int32_type };
   const struct dxil_type *threadid_func_type = dxil_module_add_function_type(m, int32_type, threadid_arg_types, ARRAY_SIZE(threadid_arg_types));
   const struct dxil_type *threadid_func_pointer_type = dxil_module_add_pointer_type(m, threadid_func_type);

   const struct dxil_type *int8_type = dxil_module_get_int_type(m, 8);
   const struct dxil_type *int8_pointer_type = dxil_module_add_pointer_type(m, int8_type);
   const struct dxil_type *handle_type = dxil_module_add_struct_type(m, "dx.types.Handle", &int8_pointer_type, 1);

   const struct dxil_type *bufferstore_arg_types[] = { int32_type, handle_type, int32_type, int32_type, int32_type, int32_type, int32_type, int32_type, int8_type };
   const struct dxil_type *bufferstore_func_type = dxil_module_add_function_type(m, void_type, bufferstore_arg_types, ARRAY_SIZE(bufferstore_arg_types));
   const struct dxil_type *bufferstore_func_pointer_type = dxil_module_add_pointer_type(m, bufferstore_func_type);

   const struct dxil_type *int1_type = dxil_module_get_int_type(m, 1);

   const struct dxil_type *createhandle_arg_types[] = { int32_type, int8_type, int32_type, int32_type, int1_type };
   const struct dxil_type *createhandle_func_type = dxil_module_add_function_type(m, handle_type, createhandle_arg_types, ARRAY_SIZE(createhandle_arg_types));
   const struct dxil_type *createhandle_func_pointer_type = dxil_module_add_pointer_type(m, createhandle_func_type);

   dxil_add_global_var(m, rwbuffer_struct_type, true, 3);
   const dxil_value main_func = dxil_add_function_def(m, main_func_type, 0);
   const dxil_value threadid_func = dxil_add_function_decl(m, threadid_func_type, 1);
   const dxil_value bufferstore_func = dxil_add_function_decl(m, bufferstore_func_type, 2);
   const dxil_value createhandle_func = dxil_add_function_decl(m, createhandle_func_type, 3);
   if (main_func == DXIL_VALUE_INVALID ||
       threadid_func == DXIL_VALUE_INVALID ||
       bufferstore_func == DXIL_VALUE_INVALID ||
       createhandle_func == DXIL_VALUE_INVALID)
      return false;

   const dxil_value int32_1 = dxil_module_add_int32_const(m, 1);
   const dxil_value int32_0 = dxil_module_add_int32_const(m, 0);
   const dxil_value int32_6 = dxil_module_add_int32_const(m, 6);
   const dxil_value int32_10 = dxil_module_add_int32_const(m, 10);
   const dxil_value int32_5 = dxil_module_add_int32_const(m, 5);
   const dxil_value int32_4 = dxil_module_add_int32_const(m, 4);
   const dxil_value int32_3 = dxil_module_add_int32_const(m, 3);
   const dxil_value int32_7 = dxil_module_add_int32_const(m, 7);
   const dxil_value int1_0 = dxil_module_add_int1_const(m, false);
   const dxil_value rwbuffer_pointer_undef = dxil_module_add_undef(m, rwbuffer_pointer_type);
   const dxil_value rwbuffer_struct_undef = dxil_module_add_undef(m, rwbuffer_struct_type);
   const dxil_value int32_57 = dxil_module_add_int32_const(m, 57);
   const dxil_value int32_93 = dxil_module_add_int32_const(m, 93);
   const dxil_value int32_69 = dxil_module_add_int32_const(m, 69);
   const dxil_value int32_undef = dxil_module_add_undef(m, int32_type);
   const dxil_value int8_1 = dxil_module_add_int8_const(m, 1);
   const dxil_value int8_15 = dxil_module_add_int8_const(m, 15);

   const dxil_value createhandle_args[] = {
      int32_57, int8_1, int32_0, int32_0, int1_0
   };
   const dxil_value threadid_args[] = {
     int32_93, int32_0
   };

   const int FUNC_CODE_DECLAREBLOCKS = 1; // TODO: remove
   const int num_type_bits = 5;

   if (!dxil_module_emit_blockinfo(m, num_type_bits) ||
       !dxil_emit_attrib_group_table(m, attrs, attr_sizes,
                                     ARRAY_SIZE(attrs)) ||
       !dxil_emit_attribute_table(m, attr_data, ARRAY_SIZE(attr_data)) ||
       !dxil_module_emit_type_table(m, num_type_bits) ||
       !dxil_emit_module_info(m) ||
       !dxil_emit_module_consts(m))
      return false;

   if (!dxil_module_enter_subblock(m, DXIL_METADATA_BLOCK, 3) ||
       !dxil_emit_metadata_abbrevs(m))
      return false;

   const dxil_mdnode compiler = dxil_emit_metadata_string(m, "clang version 3.7 (tags/RELEASE_370/final)");
   const dxil_mdnode llvm_ident = dxil_emit_metadata_node(m, &compiler, 1);
   if (!compiler || !llvm_ident)
      return false;

   const dxil_mdnode node3 = dxil_emit_metadata_value(m, int32_type, int32_1);
   const dxil_mdnode node4 = dxil_emit_metadata_value(m, int32_type, int32_0); // 0
   const dxil_mdnode nodes_3_4[] = { node3, node4 };
   const dxil_mdnode node5 = dxil_emit_metadata_node(m, nodes_3_4,
                                                     ARRAY_SIZE(nodes_3_4));
   const dxil_mdnode node6 = dxil_emit_metadata_string(m, "cs");
   const dxil_mdnode node7 = dxil_emit_metadata_value(m, int32_type, int32_6); // 6
   const dxil_mdnode nodes_6_7_4[] = { node6, node7, node4 };
   const dxil_mdnode dx_shader_model = dxil_emit_metadata_node(m, nodes_6_7_4,
                                                               ARRAY_SIZE(nodes_6_7_4));
   const dxil_mdnode node9 = dxil_emit_metadata_value(m, rwbuffer_pointer_type, rwbuffer_pointer_undef);
   const dxil_mdnode node10 = dxil_emit_metadata_string(m, "OutputBuffer");
   const dxil_mdnode node11 = dxil_emit_metadata_value(m, int32_type, int32_10);
   const dxil_mdnode node12 = dxil_emit_metadata_value(m, int1_type, int1_0);
   const dxil_mdnode node13 = dxil_emit_metadata_value(m, int32_type, int32_5);
   const dxil_mdnode nodes_4_13[] = { node4, node13 };
   const dxil_mdnode node14 = dxil_emit_metadata_node(m, nodes_4_13, ARRAY_SIZE(nodes_4_13));

   const dxil_mdnode nodes_many[] = { node4, node9, node10, node4, node4, node3, node11, node12, node12, node12, node14 };
   const dxil_mdnode node15 = dxil_emit_metadata_node(m, nodes_many, ARRAY_SIZE(nodes_many));
   const dxil_mdnode node16 = dxil_emit_metadata_node(m, &node15, 1);

   const dxil_mdnode nodes_0_16_0_0[] = { DXIL_MDNODE_NULL, node16, DXIL_MDNODE_NULL, DXIL_MDNODE_NULL };
   const dxil_mdnode main_resources = dxil_emit_metadata_node(m, nodes_0_16_0_0,
                                                      ARRAY_SIZE(nodes_0_16_0_0));

   const dxil_mdnode node18 = dxil_emit_metadata_value(m, rwbuffer_struct_type, rwbuffer_struct_undef);
   const dxil_mdnode node19 = dxil_emit_metadata_value(m, int32_type, int32_4);
   const dxil_mdnode node20 = dxil_emit_metadata_string(m, "h");
   const dxil_mdnode node21 = dxil_emit_metadata_value(m, int32_type, int32_3);
   const dxil_mdnode node22 = dxil_emit_metadata_value(m, int32_type, int32_7);

   const dxil_mdnode nodes_7_20_21_4_22_13[] = { node7, node20, node21, node4, node22, node13 };
   const dxil_mdnode node23 = dxil_emit_metadata_node(m, nodes_7_20_21_4_22_13,
                                                      ARRAY_SIZE(nodes_7_20_21_4_22_13));

   const dxil_mdnode nodes_19_23[] = { node19, node23 };
   const dxil_mdnode node24 = dxil_emit_metadata_node(m, nodes_19_23,
                                                      ARRAY_SIZE(nodes_19_23));

   const dxil_mdnode nodes_4_18_24[] = { node4, node18, node24 };
   const dxil_mdnode node25 = dxil_emit_metadata_node(m, nodes_4_18_24,
                                                      ARRAY_SIZE(nodes_4_18_24));

   const dxil_mdnode main_entrypoint = dxil_emit_metadata_value(m, main_func_pointer_type, 1);
   const dxil_mdnode node27 = dxil_emit_metadata_node(m, NULL, 0);

   const dxil_mdnode nodes_4_27_27[] = { node4, node27, node27 };
   const dxil_mdnode node28 = dxil_emit_metadata_node(m, nodes_4_27_27,
                                                      ARRAY_SIZE(nodes_4_27_27));

   const dxil_mdnode node29 = dxil_emit_metadata_node(m, &node28, 1);

   const dxil_mdnode nodes_3_26_29[] = { node3, main_entrypoint, node29 };
   const dxil_mdnode node30 = dxil_emit_metadata_node(m, nodes_3_26_29,
                                                      ARRAY_SIZE(nodes_3_26_29));

   const dxil_mdnode main_name = dxil_emit_metadata_string(m, "main");

   const dxil_mdnode nodes_3_3_3[] = { node3, node3, node3 };
   const dxil_mdnode node32 = dxil_emit_metadata_node(m, nodes_3_3_3,
                                                      ARRAY_SIZE(nodes_3_3_3));

   const dxil_mdnode nodes_19_32[] = { node19, node32 };
   const dxil_mdnode node33 = dxil_emit_metadata_node(m, nodes_19_32,
                                                      ARRAY_SIZE(nodes_19_32));
   const dxil_mdnode main_entrypoint_metadata[] = {
      main_entrypoint,
      main_name,
      DXIL_MDNODE_NULL, /* list of signatures */
      main_resources, /* list of resources */
      node33 /* list of caps and other properties */
   };
   const dxil_mdnode dx_entry_point = dxil_emit_metadata_node(m, main_entrypoint_metadata,
                                                      ARRAY_SIZE(main_entrypoint_metadata));

   const dxil_mdnode dx_version = node5, dx_valver = node5,
                     dx_resources = main_resources,
                     dx_type_annotations[] = { node25, node30 };
   if (!dxil_emit_metadata_named_node(m, "llvm.ident", &llvm_ident, 1) ||
       !dxil_emit_metadata_named_node(m, "dx.version", &dx_version, 1) ||
       !dxil_emit_metadata_named_node(m, "dx.valver", &dx_valver, 1) ||
       !dxil_emit_metadata_named_node(m, "dx.shaderModel",
                                      &dx_shader_model, 1) ||
       !dxil_emit_metadata_named_node(m, "dx.resources",
                                      &dx_resources, 1) ||
       !dxil_emit_metadata_named_node(m, "dx.typeAnnotations",
                                 dx_type_annotations,
                                 ARRAY_SIZE(dx_type_annotations)) ||
       !dxil_emit_metadata_named_node(m, "dx.entryPoints",
                                      &dx_entry_point, 1))
      return false;

   if (!dxil_module_exit_block(m) ||
       !emit_value_symbol_table(m) ||
       !dxil_module_enter_subblock(m, DXIL_FUNCTION_BLOCK, 4) ||
       !dxil_module_emit_record_int(m, FUNC_CODE_DECLAREBLOCKS, 1))
      return false;

   const dxil_value handle = dxil_emit_call(m, createhandle_func_type,
                                            createhandle_func,
                                            createhandle_args,
                                            ARRAY_SIZE(createhandle_args));
   if (handle == DXIL_VALUE_INVALID)
      return false;

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
       !dxil_emit_ret_void(m) ||
       !dxil_module_exit_block(m))
      return false;

   return dxil_module_exit_block(m);
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
