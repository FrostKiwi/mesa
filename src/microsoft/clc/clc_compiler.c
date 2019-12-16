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
emit_type_comdats(struct dxil_module *m)
{
   return true; /* nothing for now */
}

static bool
emit_metadata(struct dxil_module *m)
{
   const unsigned root_subnode = 1,
                  subnodes_3_4[] = { 3, 4 },
                  subnodes_6_7_4[] = { 6, 7, 4 },
                  subnodes_4_13[] = { 4, 13 },
                  subnodes_many[] = { 4, 9, 10, 4, 4, 3, 11, 12, 12, 12, 14 },
                  subnode_15 = 15,
                  subnodes_0_16_0_0[] = { 0, 16, 0, 0 },
                  subnodes_7_20_21_4_22_13[] = { 7, 20, 21, 4, 22, 13 },
                  subnodes_19_23[] = { 19, 23 },
                  subnodes_4_18_24[] = { 4, 18, 24 },
                  subnodes_4_27_27[] = { 4, 27, 27 },
                  subnode_28 = 28,
                  subnodes_3_26_29[] = { 3, 26, 29 },
                  subnodes_3_3_3[] = { 3, 3, 3 },
                  subnodes_19_32[] = { 19, 32 },
                  subnodes_26_31_0_17_33[] = { 26, 31, 0, 17, 33 };

   if (!dxil_module_enter_subblock(m, DXIL_METADATA_BLOCK, 3) ||
       !dxil_emit_metadata_abbrevs(m))
      return false;

   if (!dxil_emit_metadata_string(m, "clang version 3.7 (tags/RELEASE_370/final)") ||
       !dxil_emit_metadata_node(m, &root_subnode, 1) ||
       !dxil_emit_metadata_value(m, 0, 5) ||
       !dxil_emit_metadata_value(m, 0, 6) ||
       !dxil_emit_metadata_node(m, subnodes_3_4,
                                ARRAY_SIZE(subnodes_3_4)) ||
       !dxil_emit_metadata_string(m, "cs") ||
       !dxil_emit_metadata_value(m, 0, 7) ||
       !dxil_emit_metadata_node(m, subnodes_6_7_4,
                                ARRAY_SIZE(subnodes_6_7_4)) ||
       !dxil_emit_metadata_value(m, 2, 14) ||
       !dxil_emit_metadata_string(m, "OutputBuffer") ||
       !dxil_emit_metadata_value(m, 0, 8) ||
       !dxil_emit_metadata_value(m, 13, 13) ||
       !dxil_emit_metadata_value(m, 0, 9) ||
       !dxil_emit_metadata_node(m, subnodes_4_13,
                                ARRAY_SIZE(subnodes_4_13)) ||
       !dxil_emit_metadata_node(m, subnodes_many,
                                ARRAY_SIZE(subnodes_many)) ||
       !dxil_emit_metadata_node(m, &subnode_15, 1) ||
       !dxil_emit_metadata_node(m, subnodes_0_16_0_0,
                           ARRAY_SIZE(subnodes_0_16_0_0)) ||
       !dxil_emit_metadata_value(m, 1, 15) ||
       !dxil_emit_metadata_value(m, 0, 10) ||
       !dxil_emit_metadata_string(m, "h") ||
       !dxil_emit_metadata_value(m, 0, 11) ||
       !dxil_emit_metadata_value(m, 0, 12) ||
       !dxil_emit_metadata_node(m, subnodes_7_20_21_4_22_13,
                           ARRAY_SIZE(subnodes_7_20_21_4_22_13)) ||
       !dxil_emit_metadata_node(m, subnodes_19_23,
                                ARRAY_SIZE(subnodes_19_23)) ||
       !dxil_emit_metadata_node(m, subnodes_4_18_24,
                           ARRAY_SIZE(subnodes_4_18_24)) ||
       !dxil_emit_metadata_value(m, 5, 1) ||
       !dxil_emit_metadata_node(m, NULL, 0) ||
       !dxil_emit_metadata_node(m, subnodes_4_27_27,
                           ARRAY_SIZE(subnodes_4_27_27)) ||
       !dxil_emit_metadata_node(m, &subnode_28, 1) ||
       !dxil_emit_metadata_node(m, subnodes_3_26_29,
                           ARRAY_SIZE(subnodes_3_26_29)) ||
       !dxil_emit_metadata_string(m, "main") ||
       !dxil_emit_metadata_node(m, subnodes_3_3_3,
                           ARRAY_SIZE(subnodes_3_3_3)) ||
       !dxil_emit_metadata_node(m, subnodes_19_32,
                           ARRAY_SIZE(subnodes_19_32)) ||
       !dxil_emit_metadata_node(m, subnodes_26_31_0_17_33,
                           ARRAY_SIZE(subnodes_26_31_0_17_33)))
      return false;

   unsigned llvm_ident = 1, dx_version = 4, dx_valver = 4,
            dx_shader_model = 7, dx_resources = 16,
            dx_type_annotations[] = { 24, 29 }, dx_entry_points = 33;
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
                                      &dx_entry_points, 1))
      return false;

   return dxil_module_exit_block(m);
}

static bool
emit_value_symbol_table(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, DXIL_VALUE_SYMTAB_BLOCK, 4) &&
          dxil_module_emit_symtab_entry(m, 0, "\01?OutputBuffer@@3V?$RWBuffer@I@@A") &&
          dxil_module_emit_symtab_entry(m, 3, "dx.op.bufferStore.i32") &&
          dxil_module_emit_symtab_entry(m, 4, "dx.op.createHandle") &&
          dxil_module_emit_symtab_entry(m, 2, "dx.op.threadId.i32") &&
          dxil_module_emit_symtab_entry(m, 1, "main") &&
          dxil_module_exit_block(m);
}

static bool
emit_use_list_block(struct dxil_module *m)
{
   return true; /* nothing for now */
}

static bool
emit_function_value_symtab(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, DXIL_VALUE_SYMTAB_BLOCK, 4) &&
          dxil_module_emit_symtab_entry(m, 22, "OutputBuffer_UAV_buf") &&
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

   uint64_t version = 1;
   if (!dxil_module_enter_subblock(m, DXIL_MODULE, 3) ||
       !dxil_module_emit_record(m, DXIL_MODULE_CODE_VERSION, &version, 1))
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

   const struct dxil_type *int32_type = dxil_module_add_int_type(m, 32);
   const struct dxil_type *rwbuffer_struct_type = dxil_module_add_struct_type(m, "class.RWBuffer<unsigned int>", &int32_type, 1);
   const struct dxil_type *rwbuffer_pointer_type = dxil_module_add_pointer_type(m, rwbuffer_struct_type);

   const struct dxil_type *void_type = dxil_module_add_void_type(m);
   const struct dxil_type *main_func_type = dxil_module_add_function_type(m, void_type, NULL, 0);
   const struct dxil_type *main_func_pointer_type = dxil_module_add_pointer_type(m, main_func_type);

   const struct dxil_type *threadid_arg_types[] = { int32_type, int32_type };
   const struct dxil_type *threadid_func_type = dxil_module_add_function_type(m, int32_type, threadid_arg_types, ARRAY_SIZE(threadid_arg_types));
   const struct dxil_type *threadid_func_pointer_type = dxil_module_add_pointer_type(m, threadid_func_type);

   const struct dxil_type *int8_type = dxil_module_add_int_type(m, 8);
   const struct dxil_type *int8_pointer_type = dxil_module_add_pointer_type(m, int8_type);
   const struct dxil_type *handle_type = dxil_module_add_struct_type(m, "dx.types.Handle", &int8_pointer_type, 1);

   const struct dxil_type *bufferstore_arg_types[] = { int32_type, handle_type, int32_type, int32_type, int32_type, int32_type, int32_type, int32_type, int8_type };
   const struct dxil_type *bufferstore_func_type = dxil_module_add_function_type(m, void_type, bufferstore_arg_types, ARRAY_SIZE(bufferstore_arg_types));
   const struct dxil_type *bufferstore_func_pointer_type = dxil_module_add_pointer_type(m, bufferstore_func_type);

   const struct dxil_type *bool_type = dxil_module_add_int_type(m, 1);

   const struct dxil_type *createhandle_arg_types[] = { int32_type, int8_type, int32_type, int32_type, bool_type };
   const struct dxil_type *createhandle_func_type = dxil_module_add_function_type(m, handle_type, createhandle_arg_types, ARRAY_SIZE(createhandle_arg_types));
   const struct dxil_type *createhandle_func_pointer_type = dxil_module_add_pointer_type(m, createhandle_func_type);

   struct dxil_function_module_info funcs[] = {
      { main_func_type, false, 0 },
      { threadid_func_type, true, 1 },
      { bufferstore_func_type, true, 2 },
      { createhandle_func_type, true, 3 }
   };

   struct dxil_const global_consts[] = {
      { int32_type, .int_value = 1 },
      { int32_type, .int_value = 0 },
      { int32_type, .int_value = 6 },
      { int32_type, .int_value = 10 },
      { int32_type, .int_value = 5 },
      { int32_type, .int_value = 4 },
      { int32_type, .int_value = 3 },
      { int32_type, .int_value = 7 },
      { bool_type, .int_value = 0 },
      { rwbuffer_pointer_type, .undef = true },
      { rwbuffer_struct_type, .undef = true },
   };

   struct dxil_const function_consts[] = {
      { int32_type, .int_value = 57 },
      { int32_type, .int_value = 93 },
      { int32_type, .int_value = 69 },
      { int32_type, .undef = true },
      { int8_type, .int_value = 1 },
      { int8_type, .int_value = 15 },
   };

   const unsigned createhandle_args[] = {
      15, 19, 5, 5, 12
   };
   const unsigned threadid_args[] = {
     16, 5
   };
   const unsigned bufferstore_args[] = {
     17, 21, 22, 18, 22, 22, 22, 22, 20
   };


   const int FUNC_CODE_DECLAREBLOCKS = 1; // TODO: remove
   const int num_type_bits = 5;

   const char *names[] = {
      "dbg", "tbaa", "prof", "fpmath", "range", "tbaa.struct",
      "invariant.load", "alias.scope", "noalias", "nontemporal",
      "llvm.mem.parallel_loop_access", "nonnull",
      "dereferenceable", "dereferenceable_or_null",
      "dx.hl.resource.attribute", "dx.dbg.varlayout"
   };

   if (!dxil_module_emit_blockinfo(m, num_type_bits) ||
       !dxil_emit_attrib_group_table(m, attrs, attr_sizes,
                                     ARRAY_SIZE(attrs)) ||
       !dxil_emit_attribute_table(m, attr_data, ARRAY_SIZE(attr_data)) ||
       !dxil_module_emit_type_table(m, num_type_bits) ||
       !emit_type_comdats(m) ||
       !dxil_emit_module_info(m, funcs, ARRAY_SIZE(funcs)) ||
       !dxil_emit_module_consts(m, global_consts,
                                ARRAY_SIZE(global_consts)) ||
       !emit_metadata(m) ||
       !dxil_emit_metadata_store(m, names, ARRAY_SIZE(names)) ||
       !emit_value_symbol_table(m) ||
       !emit_use_list_block(m) ||
       !dxil_module_enter_subblock(m, DXIL_FUNCTION_BLOCK, 4) ||
       !dxil_module_emit_record_int(m, FUNC_CODE_DECLAREBLOCKS, 1) ||
       !dxil_emit_function_consts(m, function_consts,
                                  ARRAY_SIZE(function_consts)) ||
       !dxil_emit_call(m, createhandle_func_type, 3, createhandle_args,
                       ARRAY_SIZE(createhandle_args)) ||
       !dxil_emit_call(m, threadid_func_type, 1, threadid_args,
                       ARRAY_SIZE(threadid_args)) ||
       !dxil_emit_call(m, bufferstore_func_type, 2, bufferstore_args,
                       ARRAY_SIZE(bufferstore_args)) ||
       !dxil_emit_ret_void(m) ||
       !emit_function_value_symtab(m) ||
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
