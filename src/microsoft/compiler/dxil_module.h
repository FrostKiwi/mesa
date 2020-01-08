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

#ifndef DXIL_MODULE_H
#define DXIL_MODULE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "dxil.h"
#include "dxil_buffer.h"

#include "util/list.h"

enum dxil_attr_kind {
   DXIL_ATTR_KIND_NONE = 0,
   DXIL_ATTR_KIND_NO_UNWIND = 18,
   DXIL_ATTR_KIND_READ_NONE = 20,
   DXIL_ATTR_KIND_READ_ONLY = 21,
};

struct dxil_type;
struct dxil_value;
struct dxil_gvar;
struct dxil_mdnode;

struct dxil_module {
   enum dxil_shader_kind shader_kind;
   unsigned major_version, minor_version;

   struct dxil_buffer buf;
   struct dxil_buffer code;

   struct {
      unsigned abbrev_width;
      intptr_t offset;
   } blocks[16];
   size_t num_blocks;

   struct list_head type_list;
   struct list_head gvar_list;
   struct list_head func_list;
   struct list_head attr_set_list;
   struct list_head instr_list;
   unsigned next_type_id;

   struct list_head const_list;
   unsigned next_value_id;

   struct list_head mdnode_list;
   unsigned next_mdnode_id;
   struct list_head md_named_node_list;

   const struct dxil_type *void_type;
   const struct dxil_type *int1_type;
   const struct dxil_type *int8_type;
   const struct dxil_type *int32_type;
};

void
dxil_module_init(struct dxil_module *m);

const struct dxil_gvar *
dxil_add_global_var(struct dxil_module *m, const struct dxil_type *type,
                    bool constant, int align);

const struct dxil_value *
dxil_add_function_def(struct dxil_module *m, const char *name,
                      const struct dxil_type *type);

const struct dxil_value *
dxil_add_function_decl(struct dxil_module *m, const char *name,
                       const struct dxil_type *type,
                       enum dxil_attr_kind attr);

const struct dxil_type *
dxil_module_get_void_type(struct dxil_module *m);

const struct dxil_type *
dxil_module_get_int_type(struct dxil_module *m, unsigned bit_size);

const struct dxil_type *
dxil_module_get_pointer_type(struct dxil_module *m,
                             const struct dxil_type *target);

const struct dxil_type *
dxil_module_get_struct_type(struct dxil_module *m,
                            const char *name,
                            const struct dxil_type **elem_types,
                            size_t num_elem_types);

const struct dxil_type *
dxil_module_add_function_type(struct dxil_module *m,
                              const struct dxil_type *ret_type,
                              const struct dxil_type **arg_types,
                              size_t num_arg_types);

const struct dxil_value *
dxil_module_get_int1_const(struct dxil_module *m, bool value);

const struct dxil_value *
dxil_module_get_int8_const(struct dxil_module *m, int8_t value);

const struct dxil_value *
dxil_module_get_int32_const(struct dxil_module *m, int32_t value);

const struct dxil_value *
dxil_module_get_undef(struct dxil_module *m, const struct dxil_type *type);

const struct dxil_mdnode *
dxil_get_metadata_string(struct dxil_module *m, const char *str);

const struct dxil_mdnode *
dxil_get_metadata_value(struct dxil_module *m, const struct dxil_type *type,
                        const struct dxil_value *value);

const struct dxil_mdnode *
dxil_get_metadata_int1(struct dxil_module *m, bool value);

const struct dxil_mdnode *
dxil_get_metadata_int32(struct dxil_module *m, int32_t value);

const struct dxil_mdnode *
dxil_get_metadata_node(struct dxil_module *m,
                       const struct dxil_mdnode *subnodes[],
                       size_t num_subnodes);

bool
dxil_add_metadata_named_node(struct dxil_module *m, const char *name,
                             const struct dxil_mdnode *subnodes[],
                             size_t num_subnodes);

const struct dxil_value *
dxil_emit_call(struct dxil_module *m,
               const struct dxil_type *func_type,
               const struct dxil_value *func,
               const struct dxil_value **args, size_t num_args);

bool
dxil_emit_call_void(struct dxil_module *m,
                    const struct dxil_type *func_type,
                    const struct dxil_value *func,
                    const struct dxil_value **args, size_t num_args);

bool
dxil_emit_ret_void(struct dxil_module *m);

bool
dxil_emit_module(struct dxil_module *m);

#ifdef __cplusplus
}
#endif

#endif
