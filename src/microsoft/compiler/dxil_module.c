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

#include "dxil_module.h"

#include "util/macros.h"
#include "util/u_math.h"
#include "util/u_memory.h"

#include <assert.h>

void
dxil_module_init(struct dxil_module *m)
{
   dxil_buffer_init(&m->buf, 2);
   dxil_buffer_init(&m->code, 4);

   m->num_blocks = 0;

   list_inithead(&m->type_list);
   m->next_type_id = 0;

   list_inithead(&m->func_list);
   list_inithead(&m->gvar_list);
   list_inithead(&m->const_list);
   m->next_value_id = 0;

   list_inithead(&m->mdnode_list);
   m->next_mdnode_id = 1; /* zero is reserved for NULL nodes */
   list_inithead(&m->md_named_node_list);

   m->void_type = m->int1_type = m->int8_type = m->int32_type = NULL;
}

bool
emit_bits64(struct dxil_buffer *b, uint64_t data, unsigned width)
{
   if (data > UINT32_MAX) {
      assert(width > 32);
      return dxil_buffer_emit_bits(b, (uint32_t)(data & UINT32_MAX), width) &&
             dxil_buffer_emit_bits(b, (uint32_t)(data >> 32), width - 32);
   } else
      return dxil_buffer_emit_bits(b, (uint32_t)data, width);
}

static bool
emit_enter_subblock(struct dxil_buffer *b, unsigned id,
                    unsigned abbrev_width, intptr_t *size_offset)
{
   assert(size_offset);
   if (!dxil_buffer_emit_abbrev_id(b, DXIL_ENTER_SUBBLOCK) ||
       !dxil_buffer_emit_vbr_bits(b, id, 8) ||
       !dxil_buffer_emit_vbr_bits(b, abbrev_width, 4) ||
       !dxil_buffer_align(b))
      return false;

   b->abbrev_width = abbrev_width;
   *size_offset = blob_reserve_uint32(&b->blob);
   return true;
}

bool
dxil_module_enter_subblock(struct dxil_module *m, unsigned id,
                           unsigned abbrev_width)
{
   assert(m->num_blocks < ARRAY_SIZE(m->blocks));
   m->blocks[m->num_blocks].abbrev_width = m->buf.abbrev_width;

   if (!emit_enter_subblock(&m->buf, id, abbrev_width,
                            &m->blocks[m->num_blocks].offset))
      return false;

   m->num_blocks++;
   return true;
}

static bool
emit_exit_block(struct dxil_buffer *b, intptr_t size_offset)
{
   if (!dxil_buffer_emit_abbrev_id(b, DXIL_END_BLOCK) ||
       !dxil_buffer_align(b))
      return false;

   uint32_t size = (b->blob.size - size_offset - 1) / sizeof(uint32_t);
   if (!blob_overwrite_uint32(&b->blob, size_offset, size))
      return false;

   return true;
}

bool
dxil_module_exit_block(struct dxil_module *m)
{
   assert(m->num_blocks > 0);
   assert(m->num_blocks < ARRAY_SIZE(m->blocks));
   if (!emit_exit_block(&m->buf, m->blocks[m->num_blocks - 1].offset))
      return false;

   m->num_blocks--;
   m->buf.abbrev_width = m->blocks[m->num_blocks].abbrev_width;
   return true;
}

static bool
emit_record_no_abbrev(struct dxil_buffer *b, unsigned code,
                      const uint64_t *data, size_t size)
{
   if (!dxil_buffer_emit_abbrev_id(b, DXIL_UNABBREV_RECORD) ||
       !dxil_buffer_emit_vbr_bits(b, code, 6) ||
       !dxil_buffer_emit_vbr_bits(b, size, 6))
      return false;

   for (size_t i = 0; i < size; ++i)
      if (!dxil_buffer_emit_vbr_bits(b, data[i], 6))
         return false;

   return true;
}

bool
dxil_module_emit_record(struct dxil_module *m, unsigned code,
                        const uint64_t *data, size_t size)
{
   return emit_record_no_abbrev(&m->buf, code, data, size);
}

static bool
is_char6(char ch)
{
   if ((ch >= 'a' && ch <= 'z') ||
       (ch >= 'A' && ch <= 'Z') ||
       (ch >= '0' && ch <= '9'))
     return true;

   switch (ch) {
   case '.':
   case '_':
      return true;

   default:
      return false;
   }
}

static bool
is_char6_string(const char *str)
{
   while (*str != '\0') {
      if (!is_char6(*str++))
         return false;
   }
   return true;
}

static bool
is_char7_string(const char *str)
{
   while (*str != '\0') {
      if (*str++ >= 128)
         return false;
   }
   return true;
}

static unsigned
encode_char6(char ch)
{
   const int letters = 'z' - 'a' + 1;

   if (ch >= 'a' && ch <= 'z')
      return ch - 'a';
   else if (ch >= 'A' && ch <= 'Z')
      return letters + ch - 'A';
   else if (ch >= '0' && ch <= '9')
      return 2 * letters + ch - '0';

   switch (ch) {
   case '.': return 62;
   case '_': return 63;
   default:
      unreachable("invalid char6-character");
   }
}

static bool
emit_fixed(struct dxil_buffer *b, uint64_t data, unsigned width)
{
   if (!width)
      return true;

   return emit_bits64(b, data, width);
}

static bool
emit_vbr(struct dxil_buffer *b, uint64_t data, unsigned width)
{
   if (!width)
      return true;

   return dxil_buffer_emit_vbr_bits(b, data, width);
}

static bool
emit_char6(struct dxil_buffer *b, uint64_t data)
{
   return dxil_buffer_emit_bits(b, encode_char6((char)data), 6);
}

static bool
emit_record_abbrev(struct dxil_buffer *b,
                   unsigned abbrev, const struct dxil_abbrev *a,
                   const uint64_t *data, size_t size)
{
   assert(abbrev >= DXIL_FIRST_APPLICATION_ABBREV);

   if (!dxil_buffer_emit_abbrev_id(b, abbrev))
      return false;

   size_t curr_data = 0;
   for (int i = 0; i < a->num_operands; ++i) {
      switch (a->operands[i].type) {
      case DXIL_OP_LITERAL:
         assert(curr_data < size);
         assert(data[curr_data] == a->operands[i].value);
         curr_data++;
         /* literals are no-ops, because their value is defined in the
            abbrev-definition already */
         break;

      case DXIL_OP_FIXED:
         assert(curr_data < size);
         if (!emit_fixed(b, data[curr_data++], a->operands[i].encoding_data))
            return false;
         break;

      case DXIL_OP_VBR:
         assert(curr_data < size);
         if (!emit_vbr(b, data[curr_data++], a->operands[i].encoding_data))
            return false;
         break;

      case DXIL_OP_ARRAY:
         assert(i == a->num_operands - 2); /* arrays should always be second to last */

         if (!dxil_buffer_emit_vbr_bits(b, size - curr_data, 6))
            return false;

         switch (a->operands[i + 1].type) {
         case DXIL_OP_FIXED:
            while (curr_data < size)
               if (!emit_fixed(b, data[curr_data++], a->operands[i + 1].encoding_data))
                  return false;
            break;

         case DXIL_OP_VBR:
            while (curr_data < size)
               if (!emit_vbr(b, data[curr_data++], a->operands[i + 1].encoding_data))
                  return false;
            break;

         case DXIL_OP_CHAR6:
            while (curr_data < size)
               if (!emit_char6(b, data[curr_data++]))
                  return false;
            break;

         default:
            unreachable("unexpected operand type");
         }
         return true; /* we're done */

      case DXIL_OP_CHAR6:
         assert(curr_data < size);
         if (!emit_char6(b, data[curr_data++]))
            return false;
         break;

      case DXIL_OP_BLOB:
         unreachable("HALP, unplement!");

      default:
         unreachable("unexpected operand type");
      }
   }

   assert(curr_data == size);
   return true;
}

struct dxil_type {
   enum type_type {
      TYPE_VOID,
      TYPE_INTEGER,
      TYPE_POINTER,
      TYPE_STRUCT,
      TYPE_FUNCTION
   } type;

   union {
      unsigned int_bits;
      const struct dxil_type *ptr_target_type;
      struct {
         const char *name;
         struct dxil_type **elem_types;
         size_t num_elem_types;
      } struct_def;
      struct {
         const struct dxil_type *ret_type;
         struct dxil_type **arg_types;
         size_t num_arg_types;
      } function_def;
   };

   struct list_head head;
   unsigned id;
};

static struct dxil_type *
create_type(struct dxil_module *m, enum type_type type)
{
   struct dxil_type *ret = CALLOC_STRUCT(dxil_type);
   if (ret) {
      ret->type = type;
      ret->id = m->next_type_id++;
      list_addtail(&ret->head, &m->type_list);
   }
   return ret;
}

const struct dxil_type *
dxil_module_get_void_type(struct dxil_module *m)
{
   if (!m->void_type)
      m->void_type = create_type(m, TYPE_VOID);
   return m->void_type;
}

static const struct dxil_type *
create_int_type(struct dxil_module *m, unsigned bit_size)
{
   struct dxil_type *type = create_type(m, TYPE_INTEGER);
   if (type)
      type->int_bits = bit_size;
   return type;
}

static const struct dxil_type *
get_int1_type(struct dxil_module *m)
{
   if (!m->int1_type)
      m->int1_type = create_int_type(m, 1);
   return m->int1_type;
}

static const struct dxil_type *
get_int8_type(struct dxil_module *m)
{
   if (!m->int8_type)
      m->int8_type = create_int_type(m, 8);
   return m->int8_type;
}

static const struct dxil_type *
get_int32_type(struct dxil_module *m)
{
   if (!m->int32_type)
      m->int32_type = create_int_type(m, 32);
   return m->int32_type;
}

const struct dxil_type *
dxil_module_get_int_type(struct dxil_module *m, unsigned bit_size)
{
   switch (bit_size) {
   case 1: return get_int1_type(m);
   case 8: return get_int8_type(m);
   case 32: return get_int32_type(m);
   default:
      unreachable("unsupported bit-width");
   }
}

const struct dxil_type *
dxil_module_add_pointer_type(struct dxil_module *m,
                             const struct dxil_type *target)
{
   struct dxil_type *type = create_type(m, TYPE_POINTER);
   if (type)
      type->ptr_target_type = target;
   return type;
}

const struct dxil_type *
dxil_module_add_struct_type(struct dxil_module *m,
                            const char *name,
                            const struct dxil_type **elem_types,
                            size_t num_elem_types)
{
   struct dxil_type *type = create_type(m, TYPE_STRUCT);
   if (type) {
      type->struct_def.name = strdup(name);
      if (!type->struct_def.name) {
         FREE(type);
         return NULL;
      }
      type->struct_def.elem_types = CALLOC(sizeof(struct dxil_type *),
                                           num_elem_types);
      if (!type->struct_def.elem_types) {
         free((void *)type->struct_def.name);
         FREE(type);
         return NULL;
      }
      memcpy(type->struct_def.elem_types, elem_types,
             sizeof(struct dxil_type *) * num_elem_types);
      type->struct_def.num_elem_types = num_elem_types;
   }
   return type;
}

const struct dxil_type *
dxil_module_add_function_type(struct dxil_module *m,
                              const struct dxil_type *ret_type,
                              const struct dxil_type **arg_types,
                              size_t num_arg_types)
{
   struct dxil_type *type = create_type(m, TYPE_FUNCTION);
   if (type) {
      type->function_def.arg_types = CALLOC(sizeof(struct dxil_type *),
                                            num_arg_types);
      if (!type->function_def.arg_types) {
         FREE(type);
         return NULL;
      }
      memcpy(type->function_def.arg_types, arg_types,
             sizeof(struct dxil_type *) * num_arg_types);
      type->function_def.num_arg_types = num_arg_types;
      type->function_def.ret_type = ret_type;
   }
   return type;
}

static bool
emit_type_table_abbrev_record(struct dxil_module *m, unsigned abbrev,
                              const uint64_t *data, size_t size)
{
   assert(abbrev >= DXIL_FIRST_APPLICATION_ABBREV);
   unsigned index = abbrev - DXIL_FIRST_APPLICATION_ABBREV;
   assert(index < ARRAY_SIZE(m->type_table_abbrevs));

   return emit_record_abbrev(&m->buf, abbrev, m->type_table_abbrevs + index,
                             data, size);
}

static bool
emit_const_abbrev_record(struct dxil_module *m, unsigned abbrev,
                         const uint64_t *data, size_t size)
{
   assert(abbrev >= DXIL_FIRST_APPLICATION_ABBREV);
   unsigned index = abbrev - DXIL_FIRST_APPLICATION_ABBREV;
   assert(index < ARRAY_SIZE(m->const_abbrevs));

   return emit_record_abbrev(&m->buf, abbrev, m->const_abbrevs + index,
                             data, size);
}

static bool
emit_func_abbrev_record(struct dxil_module *m, unsigned abbrev,
                        const uint64_t *data, size_t size)
{
   assert(abbrev >= DXIL_FIRST_APPLICATION_ABBREV);
   unsigned index = abbrev - DXIL_FIRST_APPLICATION_ABBREV;
   assert(index < ARRAY_SIZE(m->func_abbrevs));

   return emit_record_abbrev(&m->code, abbrev, m->func_abbrevs + index,
                             data, size);
}

enum value_symtab_code {
  VST_CODE_ENTRY = 1,
  VST_CODE_BBENTRY = 2
};

enum constant_code {
  CST_CODE_SETTYPE = 1,
  CST_CODE_NULL = 2,
  CST_CODE_UNDEF = 3,
  CST_CODE_INTEGER = 4,
  CST_CODE_WIDE_INTEGER = 5,
  CST_CODE_FLOAT = 6,
  CST_CODE_AGGREGATE = 7,
  CST_CODE_STRING = 8,
  CST_CODE_CSTRING = 9,
  CST_CODE_CE_BINOP = 10,
  CST_CODE_CE_CAST = 11,
  CST_CODE_CE_GEP = 12,
  CST_CODE_CE_SELECT = 13,
  CST_CODE_CE_EXTRACTELT = 14,
  CST_CODE_CE_INSERTELT = 15,
  CST_CODE_CE_SHUFFLEVEC = 16,
  CST_CODE_CE_CMP = 17,
  CST_CODE_INLINEASM_OLD = 18,
  CST_CODE_CE_SHUFVEC_EX = 19,
  CST_CODE_CE_INBOUNDS_GEP = 20,
  CST_CODE_BLOCKADDRESS = 21,
  CST_CODE_DATA = 22,
  CST_CODE_INLINEASM = 23
};

enum function_code {
  FUNC_CODE_DECLAREBLOCKS = 1,
  FUNC_CODE_INST_BINOP = 2,
  FUNC_CODE_INST_CAST = 3,
  FUNC_CODE_INST_GEP_OLD = 4,
  FUNC_CODE_INST_SELECT = 5,
  FUNC_CODE_INST_EXTRACTELT = 6,
  FUNC_CODE_INST_INSERTELT = 7,
  FUNC_CODE_INST_SHUFFLEVEC = 8,
  FUNC_CODE_INST_CMP = 9,
  FUNC_CODE_INST_RET = 10,
  FUNC_CODE_INST_BR = 11,
  FUNC_CODE_INST_SWITCH = 12,
  FUNC_CODE_INST_INVOKE = 13,
  /* 14: unused */
  FUNC_CODE_INST_UNREACHABLE = 15,
  FUNC_CODE_INST_PHI = 16,
  /* 17-18: unused */
  FUNC_CODE_INST_ALLOCA = 19,
  FUNC_CODE_INST_LOAD = 20,
  /* 21-22: unused */
  FUNC_CODE_INST_VAARG = 23,
  FUNC_CODE_INST_STORE_OLD = 24,
  /* 25: unused */
  FUNC_CODE_INST_EXTRACTVAL = 26,
  FUNC_CODE_INST_INSERTVAL = 27,
  FUNC_CODE_INST_CMP2 = 28,
  FUNC_CODE_INST_VSELECT = 29,
  FUNC_CODE_INST_INBOUNDS_GEP_OLD = 30,
  FUNC_CODE_INST_INDIRECTBR = 31,
  /* 32: unused */
  FUNC_CODE_DEBUG_LOC_AGAIN = 33,
  FUNC_CODE_INST_CALL = 34,
  FUNC_CODE_DEBUG_LOC = 35,
  FUNC_CODE_INST_FENCE = 36,
  FUNC_CODE_INST_CMPXCHG_OLD = 37,
  FUNC_CODE_INST_ATOMICRMW = 38,
  FUNC_CODE_INST_RESUME = 39,
  FUNC_CODE_INST_LANDINGPAD_OLD = 40,
  FUNC_CODE_INST_LOADATOMIC = 41,
  FUNC_CODE_INST_STOREATOMIC_OLD = 42,
  FUNC_CODE_INST_GEP = 43,
  FUNC_CODE_INST_STORE = 44,
  FUNC_CODE_INST_STOREATOMIC = 45,
  FUNC_CODE_INST_CMPXCHG = 46,
  FUNC_CODE_INST_LANDINGPAD = 47,
};

enum type_codes {
  TYPE_CODE_NUMENTRY = 1,
  TYPE_CODE_VOID = 2,
  TYPE_CODE_FLOAT = 3,
  TYPE_CODE_DOUBLE = 4,
  TYPE_CODE_LABEL = 5,
  TYPE_CODE_OPAQUE = 6,
  TYPE_CODE_INTEGER = 7,
  TYPE_CODE_POINTER = 8,
  TYPE_CODE_FUNCTION_OLD = 9,
  TYPE_CODE_HALF = 10,
  TYPE_CODE_ARRAY = 11,
  TYPE_CODE_VECTOR = 12,
  TYPE_CODE_X86_FP80 = 13,
  TYPE_CODE_FP128 = 14,
  TYPE_CODE_PPC_FP128 = 15,
  TYPE_CODE_METADATA = 16,
  TYPE_CODE_X86_MMX = 17,
  TYPE_CODE_STRUCT_ANON = 18,
  TYPE_CODE_STRUCT_NAME = 19,
  TYPE_CODE_STRUCT_NAMED = 20,
  TYPE_CODE_FUNCTION = 21
};

#define LITERAL(x) { DXIL_OP_LITERAL, (x) }
#define FIXED(x) { DXIL_OP_FIXED, (x) }
#define VBR(x) { DXIL_OP_VBR, (x) }
#define ARRAY() { DXIL_OP_ARRAY, 0 }
#define CHAR6() { DXIL_OP_CHAR6, 0 }
#define BLOB() { DXIL_OP_BLOB, 0 }

static bool
define_abbrev(struct dxil_module *m, const struct dxil_abbrev *a)
{
   if (!dxil_buffer_emit_abbrev_id(&m->buf, DXIL_DEFINE_ABBREV) ||
       !dxil_buffer_emit_vbr_bits(&m->buf, a->num_operands, 5))
      return false;

   for (int i = 0; i < a->num_operands; ++i) {
      unsigned is_literal = a->operands[i].type == DXIL_OP_LITERAL;
      if (!dxil_buffer_emit_bits(&m->buf, is_literal, 1))
         return false;
      if (a->operands[i].type == DXIL_OP_LITERAL) {
         if (!dxil_buffer_emit_vbr_bits(&m->buf, a->operands[i].value, 8))
            return false;
      } else {
         if (!dxil_module_emit_bits(m, a->operands[i].type, 3))
            return false;
         if (a->operands[i].type == DXIL_OP_FIXED ||
             a->operands[i].type == DXIL_OP_VBR) {
            if (!dxil_buffer_emit_vbr_bits(&m->buf,
                                           a->operands[i].encoding_data, 5))
               return false;
         }
      }
   }

   return true;
}

static bool
switch_to_block(struct dxil_module *m, uint32_t block)
{
   return dxil_module_emit_record_int(m, DXIL_BLOCKINFO_CODE_SETBID, block);
}

static struct dxil_abbrev value_symtab_abbrevs[] = {
   { { FIXED(3), VBR(8), ARRAY(), FIXED(8) }, 4 },
   { { LITERAL(VST_CODE_ENTRY), VBR(8), ARRAY(), FIXED(7), }, 4 },
   { { LITERAL(VST_CODE_ENTRY), VBR(8), ARRAY(), CHAR6(), }, 4 },
   { { LITERAL(VST_CODE_BBENTRY), VBR(8), ARRAY(), CHAR6(), }, 4 },
};

static bool
emit_value_symtab_abbrevs(struct dxil_module *m)
{
   if (!switch_to_block(m, DXIL_VALUE_SYMTAB_BLOCK))
      return false;

   for (int i = 0; i < ARRAY_SIZE(value_symtab_abbrevs); ++i) {
      if (!define_abbrev(m, value_symtab_abbrevs + i))
         return false;
   }

   return true;
}

static bool
emit_const_abbrevs(struct dxil_module *m, int type_index_bits)
{
   if (!switch_to_block(m, DXIL_CONST_BLOCK))
      return false;

   struct dxil_abbrev const_abbrevs[] = {
      { { LITERAL(CST_CODE_SETTYPE), FIXED(type_index_bits) }, 2 },
      { { LITERAL(CST_CODE_INTEGER), VBR(8) }, 2 },
      { { LITERAL(CST_CODE_CE_CAST), FIXED(4), FIXED(type_index_bits),
          VBR(8) }, 4 },
      { { LITERAL(CST_CODE_NULL) }, 1 },
   };

   assert(sizeof(const_abbrevs) == sizeof(m->const_abbrevs));
   memcpy(m->const_abbrevs, const_abbrevs, sizeof(const_abbrevs));

   for (int i = 0; i < ARRAY_SIZE(const_abbrevs); ++i) {
      if (!define_abbrev(m, const_abbrevs + i))
         return false;
   }

   return true;
}

static bool
emit_function_abbrevs(struct dxil_module *m, int type_index_bits)
{
   if (!switch_to_block(m, DXIL_FUNCTION_BLOCK))
      return false;

   struct dxil_abbrev func_abbrevs[] = {
      { { LITERAL(FUNC_CODE_INST_LOAD), VBR(6), FIXED(type_index_bits),
          VBR(4), FIXED(1) }, 5 },
      { { LITERAL(FUNC_CODE_INST_BINOP), VBR(6), VBR(6), FIXED(4) }, 4 },
      { { LITERAL(FUNC_CODE_INST_BINOP), VBR(6), VBR(6), FIXED(4),
          FIXED(7) }, 5 },
      { { LITERAL(FUNC_CODE_INST_CAST), VBR(6), FIXED(type_index_bits),
          FIXED(4) }, 4 },
      { { LITERAL(FUNC_CODE_INST_RET) }, 1 },
      { { LITERAL(FUNC_CODE_INST_RET), VBR(6) }, 2 },
      { { LITERAL(FUNC_CODE_INST_UNREACHABLE) }, 1 },

      { { LITERAL(FUNC_CODE_INST_GEP), FIXED(1), FIXED(type_index_bits),
          ARRAY(), VBR(6) }, 5 },
   };

   assert(sizeof(func_abbrevs) == sizeof(m->func_abbrevs));
   memcpy(m->func_abbrevs, func_abbrevs, sizeof(func_abbrevs));

   for (int i = 0; i < ARRAY_SIZE(func_abbrevs); ++i) {
      if (!define_abbrev(m, func_abbrevs + i))
         return false;
   }

   return true;
}

bool
dxil_module_emit_blockinfo(struct dxil_module *m, int type_index_bits)
{
   return dxil_module_enter_subblock(m, DXIL_BLOCKINFO, 2) &&
          emit_value_symtab_abbrevs(m) &&
          emit_const_abbrevs(m, type_index_bits) &&
          emit_function_abbrevs(m, type_index_bits) &&
          dxil_module_exit_block(m);
}

enum attribute_codes {
   PARAMATTR_GRP_CODE_ENTRY = 3,
   PARAMATTR_CODE_ENTRY = 2
};

static bool
emit_attrib_group(struct dxil_module *m, int id, uint32_t slot,
                  const struct dxil_attrib *attrs, size_t num_attrs)
{
   uint64_t record[64];
   record[0] = id;
   record[1] = slot;
   size_t size = 2;

   for (int i = 0; i < num_attrs; ++i) {
      uint64_t kind;
      switch (attrs[i].type) {
      case DXIL_ATTR_ENUM:
         assert(size < ARRAY_SIZE(record) - 2);
         record[size++] = 0;
         record[size++] = attrs[i].kind;
         break;

      default:
         unreachable("unsupported attrib type");
      }
   }

   return dxil_module_emit_record(m, PARAMATTR_GRP_CODE_ENTRY, record, size);
}

bool
dxil_emit_attrib_group_table(struct dxil_module *m,
                             const struct dxil_attrib **attrs,
                             const size_t *sizes, size_t num_attrs)
{
   if (!dxil_module_enter_subblock(m, DXIL_PARAMATTR_GROUP, 3))
      return false;

   for (int i = 0; i < num_attrs; ++i)
      if (!emit_attrib_group(m, 1 + i, UINT32_MAX, attrs[i], sizes[i]))
         return false;

   return dxil_module_exit_block(m);
}

bool
dxil_emit_attribute_table(struct dxil_module *m,
                          const unsigned *attrs, size_t num_attrs)
{
   if (!dxil_module_enter_subblock(m, DXIL_PARAMATTR, 3))
      return false;

   for (int i = 0; i < num_attrs; ++i) {
      if (!dxil_module_emit_record_int(m, PARAMATTR_CODE_ENTRY, attrs[i]))
         return false;
   }

   return dxil_module_exit_block(m);
}

static bool
emit_type_table_abbrevs(struct dxil_module *m, int type_index_bits)
{
   struct dxil_abbrev type_table_abbrevs[] = {
      { { LITERAL(TYPE_CODE_POINTER), FIXED(type_index_bits),
          LITERAL(0) }, 3 },
      { { LITERAL(TYPE_CODE_FUNCTION), FIXED(1), ARRAY(),
          FIXED(type_index_bits) }, 4 },
      { { LITERAL(TYPE_CODE_STRUCT_ANON), FIXED(1), ARRAY(),
          FIXED(type_index_bits) }, 4 },
      { { LITERAL(TYPE_CODE_STRUCT_NAME), ARRAY(), CHAR6() }, 3 },
      { { LITERAL(TYPE_CODE_STRUCT_NAMED), FIXED(1), ARRAY(),
          FIXED(type_index_bits) }, 4 },
      { { LITERAL(TYPE_CODE_ARRAY), VBR(8), FIXED(type_index_bits) }, 3 }
   };

   for (int i = 0; i < ARRAY_SIZE(type_table_abbrevs); ++i) {
      if (!define_abbrev(m, type_table_abbrevs + i))
         return false;
   }

   assert(sizeof(type_table_abbrevs) == sizeof(m->type_table_abbrevs));
   memcpy(m->type_table_abbrevs, type_table_abbrevs, sizeof(type_table_abbrevs));

   return true;
}

static bool
emit_void_type(struct dxil_module *m)
{
   return dxil_module_emit_record(m, TYPE_CODE_VOID, NULL, 0);
}

static bool
emit_integer_type(struct dxil_module *m, int bit_size)
{
   return dxil_module_emit_record_int(m, TYPE_CODE_INTEGER, bit_size);
}

static bool
emit_pointer_type(struct dxil_module *m, int type_index)
{
   uint64_t data[] = { TYPE_CODE_POINTER, type_index, 0 };
   return emit_type_table_abbrev_record(m, 4, data, ARRAY_SIZE(data));
}

static bool
emit_struct_name(struct dxil_module *m, const char *name)
{
   uint64_t temp[256];
   assert(strlen(name) < ARRAY_SIZE(temp));

   for (int i = 0; i < strlen(name); ++i)
      temp[i] = name[i];

   return dxil_module_emit_record(m, TYPE_CODE_STRUCT_NAME, temp, strlen(name));
}

static bool
emit_struct_name_char6(struct dxil_module *m, const char *name)
{
   uint64_t temp[256];
   assert(strlen(name) < ARRAY_SIZE(temp) - 1);

   temp[0] = TYPE_CODE_STRUCT_NAME;
   for (int i = 0; i < strlen(name); ++i)
      temp[i + 1] = name[i];

   return emit_type_table_abbrev_record(m, 7, temp, 1 + strlen(name));
}

static bool
emit_struct_type(struct dxil_module *m, const struct dxil_type *type)
{
   assert(type->struct_def.name);
   if (is_char6_string(type->struct_def.name)) {
      if (!emit_struct_name_char6(m, type->struct_def.name))
         return false;
   } else {
      if (!emit_struct_name(m, type->struct_def.name))
         return false;
   }

   uint64_t temp[256];
   assert(type->struct_def.num_elem_types < ARRAY_SIZE(temp) - 2);
   temp[0] = TYPE_CODE_STRUCT_NAMED;
   temp[1] = 0; /* packed */
   for (int i = 0; i < type->struct_def.num_elem_types; ++i)
      temp[2 + i] = type->struct_def.elem_types[i]->id;

   return emit_type_table_abbrev_record(m, 8, temp, 2 + type->struct_def.num_elem_types);
}

static bool
emit_function_type(struct dxil_module *m, const struct dxil_type *type)
{
   uint64_t temp[256];
   assert(type->function_def.num_arg_types < ARRAY_SIZE(temp) - 3);

   temp[0] = TYPE_CODE_FUNCTION;
   temp[1] = 0; // vararg
   temp[2] = type->function_def.ret_type->id;
   for (int i = 0; i < type->function_def.num_arg_types; ++i)
      temp[3 + i] = type->function_def.arg_types[i]->id;

   return emit_type_table_abbrev_record(m, 5, temp, 3 + type->function_def.num_arg_types);
}

static bool
emit_metadata_type(struct dxil_module *m)
{
   return dxil_module_emit_record(m, TYPE_CODE_METADATA, NULL, 0);
}

bool
dxil_module_emit_type_table(struct dxil_module *m, int type_index_bits)
{
   if (!dxil_module_enter_subblock(m, DXIL_TYPE_BLOCK, 4) ||
       !emit_type_table_abbrevs(m, type_index_bits) ||
       !dxil_module_emit_record_int(m, 1, 1 + list_length(&m->type_list)))
      return false;

   struct dxil_type *type;
   LIST_FOR_EACH_ENTRY(type, &m->type_list, head) {
      switch (type->type) {
      case TYPE_VOID:
         if (!emit_void_type(m))
            return false;
         break;

      case TYPE_INTEGER:
         if (!emit_integer_type(m, type->int_bits))
            return false;
         break;

      case TYPE_POINTER:
         if (!emit_pointer_type(m, type->ptr_target_type->id))
            return false;
         break;

      case TYPE_STRUCT:
         if (!emit_struct_type(m, type))
            return false;
         break;

      case TYPE_FUNCTION:
         if (!emit_function_type(m, type))
            return false;
         break;

      default:
         unreachable("unexpected type->type");
      }
   }

   return emit_metadata_type(m) &&
          dxil_module_exit_block(m);
}

struct dxil_const {
   const struct dxil_type *type;
   dxil_value value;

   bool undef;
   union {
      intmax_t int_value;
   };

   struct list_head head;
};

static struct dxil_const *
create_const(struct dxil_module *m, const struct dxil_type *type, bool undef)
{
   struct dxil_const *ret = CALLOC_STRUCT(dxil_const);
   if (ret) {
      ret->type = type;
      ret->value = m->next_value_id++;
      ret->undef = undef;
      list_addtail(&ret->head, &m->const_list);
   }
   return ret;
}

static const dxil_value
get_int_const(struct dxil_module *m, const struct dxil_type *type,
              intmax_t value)
{
   assert(type && type->type == TYPE_INTEGER);

   struct dxil_const *c;
   LIST_FOR_EACH_ENTRY(c, &m->const_list, head) {
      if (c->type != type)
         continue;

      if (c->int_value == value)
         return c->value;
   }

   c = create_const(m, type, false);
   if (!c)
      return DXIL_VALUE_INVALID;

   c->int_value = value;
   return c->value;
}

const dxil_value
dxil_module_get_int1_const(struct dxil_module *m, bool value)
{
   const struct dxil_type *type = get_int1_type(m);
   if (!type)
      return DXIL_VALUE_INVALID;

   return get_int_const(m, type, value);
}

const dxil_value
dxil_module_get_int8_const(struct dxil_module *m, int8_t value)
{
   const struct dxil_type *type = get_int8_type(m);
   if (!type)
      return DXIL_VALUE_INVALID;

   return get_int_const(m, type, value);
}

const dxil_value
dxil_module_get_int32_const(struct dxil_module *m, int32_t value)
{
   const struct dxil_type *type = get_int32_type(m);
   if (!type)
      return DXIL_VALUE_INVALID;

   return get_int_const(m, type, value);
}

const dxil_value
dxil_module_get_undef(struct dxil_module *m, const struct dxil_type *type)
{
   assert(type != NULL);

   struct dxil_const *c;
   LIST_FOR_EACH_ENTRY(c, &m->const_list, head) {
      if (c->type != type)
         continue;

      if (c->undef)
         return c->value;
   }

   c = create_const(m, type, true);
   return c ? c->value : DXIL_VALUE_INVALID;
}

static bool
emit_target_triple(struct dxil_module *m, const char *triple)
{
   uint64_t temp[256];
   assert(strlen(triple) < ARRAY_SIZE(temp));

   for (int i = 0; i < strlen(triple); ++i)
      temp[i] = triple[i];

   return dxil_module_emit_record(m, DXIL_MODULE_CODE_TRIPLE,
                                  temp, strlen(triple));
}

static bool
emit_datalayout(struct dxil_module *m, const char *datalayout)
{
   uint64_t temp[256];
   assert(strlen(datalayout) < ARRAY_SIZE(temp));

   for (int i = 0; i < strlen(datalayout); ++i)
      temp[i] = datalayout[i];

   return dxil_module_emit_record(m, DXIL_MODULE_CODE_DATALAYOUT,
                                  temp, strlen(datalayout));
}

struct dxil_func {
   const struct dxil_type *type;
   bool decl;
   unsigned attr_set;

   dxil_value value;
   struct list_head head;
};

static const dxil_value
add_function(struct dxil_module *m, const struct dxil_type *type,
             bool decl, unsigned attr_set)
{
   struct dxil_func *func = CALLOC_STRUCT(dxil_func);
   if (!func)
      return DXIL_VALUE_INVALID;

   func->type = type;
   func->decl = decl;
   func->attr_set = attr_set;

   func->value = m->next_value_id++;
   list_addtail(&func->head, &m->func_list);
   return func->value;
}

struct dxil_gvar {
   const struct dxil_type *type;
   bool constant;
   int align;

   dxil_value value;
   struct list_head head;
};

const dxil_value
dxil_add_global_var(struct dxil_module *m, const struct dxil_type *type,
                    bool constant, int align)
{
   struct dxil_gvar *gvar = CALLOC_STRUCT(dxil_gvar);
   if (!gvar)
      return DXIL_VALUE_INVALID;

   gvar->type = type;
   gvar->constant = constant;
   gvar->align = align;

   gvar->value = m->next_value_id++;
   list_addtail(&gvar->head, &m->gvar_list);
   return gvar->value;
}

const dxil_value
dxil_add_function_def(struct dxil_module *m, const struct dxil_type *type,
                      unsigned attr_set)
{
   return add_function(m, type, false, attr_set);
}

const dxil_value
dxil_add_function_decl(struct dxil_module *m, const struct dxil_type *type,
                       unsigned attr_set)
{
   return add_function(m, type, true, attr_set);
}

static bool
emit_module_info_function(struct dxil_module *m, int type, bool declaration,
                          int attr_set_index)
{
   uint64_t data[] = {
      type, 0/* address space */, declaration, 0/* linkage */,
      attr_set_index, 0/* alignment */, 0 /* section */, 0 /* visibility */,
      0 /* GC */, 0 /* unnamed addr */, 0 /* prologue data */,
      0 /* storage class */, 0 /* comdat */, 0 /* prefix-data */,
      0 /* personality */
   };
   return dxil_module_emit_record(m, DXIL_MODULE_CODE_FUNCTION,
                                  data, ARRAY_SIZE(data));
}

static bool
emit_module_info_global(struct dxil_module *m, int type_id, bool constant,
                        int alignment, const struct dxil_abbrev *simple_gvar_abbr)
{
   uint64_t data[] = {
      DXIL_MODULE_CODE_GLOBALVAR,
      type_id,
      2 | constant,
      0, // initializer
      0, // linkage
      alignment,
      0
   };
   return emit_record_abbrev(&m->buf, 4, simple_gvar_abbr,
                             data, ARRAY_SIZE(data));
}

bool
dxil_emit_module_info(struct dxil_module *m)
{
   struct dxil_gvar *gvar;
   int max_global_type = 0;
   LIST_FOR_EACH_ENTRY(gvar, &m->gvar_list, head) {
      max_global_type = MAX2(max_global_type, gvar->type->id);
   }

   struct dxil_abbrev simple_gvar_abbr = {
      { LITERAL(DXIL_MODULE_CODE_GLOBALVAR),
        FIXED(util_logbase2(max_global_type) + 1),
        VBR(6), VBR(6), FIXED(5), FIXED(2), LITERAL(0) }, 7
   };

   if (!emit_target_triple(m, "dxil-ms-dx") ||
       !emit_datalayout(m, "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64") ||
       !define_abbrev(m, &simple_gvar_abbr))
      return false;

   LIST_FOR_EACH_ENTRY(gvar, &m->gvar_list, head) {
      if (!emit_module_info_global(m, gvar->type->id, gvar->constant,
                                   gvar->align, &simple_gvar_abbr))
         return false;
   }

   struct dxil_func *func;
   LIST_FOR_EACH_ENTRY(func, &m->func_list, head) {
      if (!emit_module_info_function(m, func->type->id, func->decl,
                                     func->attr_set))
         return false;
   }

   return true;
}

static bool
emit_module_const_abbrevs(struct dxil_module *m)
{
   /* these are unused for now, so let's not even record them */
   struct dxil_abbrev abbrevs[] = {
      { { LITERAL(CST_CODE_AGGREGATE), ARRAY(), FIXED(5) }, 3 },
      { { LITERAL(CST_CODE_STRING), ARRAY(), FIXED(8) }, 3 },
      { { LITERAL(CST_CODE_CSTRING), ARRAY(), FIXED(7) }, 3 },
      { { LITERAL(CST_CODE_CSTRING), ARRAY(), CHAR6() }, 3 },
   };

   for (int i = 0; i < ARRAY_SIZE(abbrevs); ++i) {
      if (!define_abbrev(m, abbrevs + i))
         return false;
   }

   return true;
}

static bool
emit_set_type(struct dxil_module *m, unsigned type_index)
{
   uint64_t data[] = { CST_CODE_SETTYPE, type_index };
   return emit_const_abbrev_record(m, 4, data, ARRAY_SIZE(data));
}

static bool
emit_null_value(struct dxil_module *m)
{
   return emit_record_no_abbrev(&m->buf, CST_CODE_NULL, NULL, 0);
}

static bool
emit_undef_value(struct dxil_module *m)
{
   return emit_record_no_abbrev(&m->buf, CST_CODE_UNDEF, NULL, 0);
}

static bool
emit_int_value(struct dxil_module *m, int64_t value)
{
   if (!value)
      return emit_null_value(m);

   assert(value > 0); /* no support for signed constants yet */

   uint64_t data[] = { CST_CODE_INTEGER, value << 1 };
   return emit_const_abbrev_record(m, 5, data, ARRAY_SIZE(data));
}

static bool
emit_consts(struct dxil_module *m)
{
   const struct dxil_type *curr_type = NULL;
   struct dxil_const *c;
   LIST_FOR_EACH_ENTRY(c, &m->const_list, head) {
      assert(c->type != NULL);
      if (curr_type != c->type) {
         if (!emit_set_type(m, c->type->id))
            return false;
         curr_type = c->type;
      }

      if (c->undef) {
         emit_undef_value(m);
         continue;
      }

      switch (curr_type->type) {
      case TYPE_INTEGER:
         emit_int_value(m, c->int_value);
         break;

      default:
         unreachable("unsupported constant type");
      }
   }

   return true;
}

bool
dxil_emit_module_consts(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, DXIL_CONST_BLOCK, 4) &&
          emit_module_const_abbrevs(m) &&
          emit_consts(m) &&
          dxil_module_exit_block(m);
}

static bool
emit_value_symtab_abbrev_record(struct dxil_module *m, unsigned abbrev,
                                const uint64_t *data, size_t size)
{
   assert(abbrev >= DXIL_FIRST_APPLICATION_ABBREV);
   unsigned index = abbrev - DXIL_FIRST_APPLICATION_ABBREV;
   assert(index < ARRAY_SIZE(value_symtab_abbrevs));

   return emit_record_abbrev(&m->buf, abbrev, value_symtab_abbrevs + index,
                             data, size);
}

bool
dxil_module_emit_symtab_entry(struct dxil_module *m, unsigned value,
                              const char *name)
{
   uint64_t temp[256];
   assert(strlen(name) < ARRAY_SIZE(temp) - 2);

   temp[0] = VST_CODE_ENTRY;
   temp[1] = value;
   for (int i = 0; i < strlen(name); ++i)
      temp[i + 2] = name[i];

   int abbrev = 4;
   if (is_char6_string(name))
      abbrev = 6;
   else if (is_char7_string(name))
      abbrev = 5;

   return emit_value_symtab_abbrev_record(m, abbrev, temp, 2 + strlen(name));
}

enum metadata_codes {
  METADATA_STRING = 1,
  METADATA_VALUE = 2,
  METADATA_NODE = 3,
  METADATA_NAME = 4,
  METADATA_KIND = 6,
  METADATA_NAMED_NODE = 10
};

static const struct dxil_abbrev metadata_abbrevs[] = {
   { { LITERAL(METADATA_STRING), ARRAY(), FIXED(8) }, 3 },
   { { LITERAL(METADATA_NAME), ARRAY(), FIXED(8) }, 3 },
};

static bool
emit_metadata_abbrevs(struct dxil_module *m)
{
   for (int i = 0; i < ARRAY_SIZE(metadata_abbrevs); ++i) {
      if (!define_abbrev(m, metadata_abbrevs + i))
         return false;
   }
   return true;
}

struct dxil_mdnode {
   enum mdnode_type {
      MD_STRING,
      MD_VALUE,
      MD_NODE
   } type;

   union {
      char *string;

      struct {
         const struct dxil_type *type;
         dxil_value value;
      } value;

      struct {
         struct dxil_mdnode **subnodes;
         size_t num_subnodes;
      } node;
   };

   struct list_head head;
   unsigned id;
};

static struct dxil_mdnode *
create_mdnode(struct dxil_module *m, enum mdnode_type type)
{
   struct dxil_mdnode *ret = CALLOC_STRUCT(dxil_mdnode);
   if (ret) {
      ret->type = type;
      ret->id = m->next_mdnode_id++;
      list_addtail(&ret->head, &m->mdnode_list);
   }
   return ret;
}

const struct dxil_mdnode *
dxil_add_metadata_string(struct dxil_module *m, const char *str)
{
   struct dxil_mdnode *n = create_mdnode(m, MD_STRING);
   if (n) {
      n->string = strdup(str);
      if (!n->string) {
         FREE(n);
         return NULL;
      }
   }
   return n;
}

const struct dxil_mdnode *
dxil_add_metadata_value(struct dxil_module *m, const struct dxil_type *type,
                        const dxil_value value)
{
   struct dxil_mdnode *n = create_mdnode(m, MD_VALUE);
   if (n) {
      n->value.type = type;
      n->value.value = value;
   }
   return n;
}

const struct dxil_mdnode *
dxil_add_metadata_node(struct dxil_module *m,
                       const struct dxil_mdnode *subnodes[],
                       size_t num_subnodes)
{
   struct dxil_mdnode *n = create_mdnode(m, MD_NODE);
   if (n) {
      n->node.subnodes = CALLOC(num_subnodes, sizeof(struct dxil_mdnode *));
      if (!n->node.subnodes) {
         FREE(n);
         return NULL;
      }

      memcpy(n->node.subnodes, subnodes, sizeof(struct dxil_mdnode *) *
             num_subnodes);
      n->node.num_subnodes = num_subnodes;
   }
   return n;
}

const struct dxil_mdnode *
dxil_add_metadata_int32(struct dxil_module *m, int32_t value)
{
   const struct dxil_type *type = get_int32_type(m);
   if (!type)
      return NULL;

   const dxil_value const_value = get_int_const(m, type, value);
   if (!const_value)
      return NULL;

   return dxil_add_metadata_value(m, type, const_value);
}

struct dxil_named_node {
   char *name;
   struct dxil_mdnode **subnodes;
   size_t num_subnodes;
   struct list_head head;
};

bool
dxil_add_metadata_named_node(struct dxil_module *m, const char *name,
                             const struct dxil_mdnode *subnodes[],
                             size_t num_subnodes)
{
   struct dxil_named_node *n = CALLOC_STRUCT(dxil_named_node);
   if (!n)
      return false;

   n->name = strdup(name);
   if (!n->name)
      goto fail;

   n->subnodes = CALLOC(num_subnodes, sizeof(struct dxil_mdnode *));
   if (!n->subnodes)
      goto fail;

   memcpy(n->subnodes, subnodes, sizeof(struct dxil_mdnode *) *
          num_subnodes);
   n->num_subnodes = num_subnodes;

   list_addtail(&n->head, &m->md_named_node_list);
   return true;

fail:
   FREE(n->name);
   FREE(n);
   return false;
}

static bool
emit_metadata_value(struct dxil_module *m, const struct dxil_type *type,
                    const dxil_value value)
{
   uint64_t data[2] = { type->id, value };
   return dxil_module_emit_record(m, METADATA_VALUE, data,
                                  ARRAY_SIZE(data));
}

static bool
emit_metadata_abbrev_record(struct dxil_module *m, unsigned abbrev,
                            const uint64_t *data, size_t size)
{
   assert(abbrev >= DXIL_FIRST_APPLICATION_ABBREV);
   unsigned index = abbrev - DXIL_FIRST_APPLICATION_ABBREV;
   assert(index < ARRAY_SIZE(metadata_abbrevs));

   return emit_record_abbrev(&m->buf, abbrev, metadata_abbrevs + index,
                             data, size);
}

static bool
emit_metadata_string(struct dxil_module *m, const char *str)
{
   uint64_t data[256];
   assert(strlen(str) < ARRAY_SIZE(data) - 1);
   data[0] = METADATA_STRING;
   for (size_t i = 0; i < strlen(str); ++i)
      data[i + 1] = str[i];

   return emit_metadata_abbrev_record(m, 4, data, strlen(str) + 1);
}

static bool
emit_metadata_node(struct dxil_module *m,
                   const struct dxil_mdnode *subnodes[],
                   size_t num_subnodes)
{
   uint64_t data[256];
   assert(num_subnodes < ARRAY_SIZE(data));
   for (size_t i = 0; i < num_subnodes; ++i)
      data[i] = subnodes[i] ? subnodes[i]->id : 0;

   return dxil_module_emit_record(m, METADATA_NODE, data, num_subnodes);
}

static bool
emit_metadata_nodes(struct dxil_module *m)
{
   struct dxil_mdnode *n;
   LIST_FOR_EACH_ENTRY(n, &m->mdnode_list, head) {
      switch (n->type) {
      case MD_STRING:
         if (!emit_metadata_string(m, n->string))
            return false;
         break;

      case MD_VALUE:
         if (!emit_metadata_value(m, n->value.type, n->value.value))
            return false;
         break;

      case MD_NODE:
         if (!emit_metadata_node(m, n->node.subnodes, n->node.num_subnodes))
            return false;
         break;
      }
   }
   return true;
}

static bool
emit_metadata_name(struct dxil_module *m, const char *name)
{
   uint64_t data[256];
   assert(strlen(name) < ARRAY_SIZE(data) - 1);
   data[0] = METADATA_NAME;
   for (size_t i = 0; i < strlen(name); ++i)
      data[i + 1] = name[i];

   return emit_metadata_abbrev_record(m, 5, data, strlen(name) + 1);
}

static bool
emit_metadata_named_node(struct dxil_module *m, const char *name,
                         const struct dxil_mdnode *subnodes[],
                         size_t num_subnodes)
{
   uint64_t data[256];
   assert(num_subnodes < ARRAY_SIZE(data));
   for (size_t i = 0; i < num_subnodes; ++i) {
      assert(subnodes[i]->id > 0); /* NULL nodes not allowed */
      data[i] = subnodes[i]->id - 1;
   }

   return emit_metadata_name(m, name) &&
          dxil_module_emit_record(m, METADATA_NAMED_NODE,
                                  data, num_subnodes);
}

static bool
emit_metadata_named_nodes(struct dxil_module *m)
{
   struct dxil_named_node *n;
   LIST_FOR_EACH_ENTRY(n, &m->md_named_node_list, head) {
      if (!emit_metadata_named_node(m, n->name, n->subnodes,
                                    n->num_subnodes))
         return false;
   }
   return true;
}

bool
dxil_emit_metadata(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, DXIL_METADATA_BLOCK, 3) &&
          emit_metadata_abbrevs(m) &&
          emit_metadata_nodes(m) &&
          emit_metadata_named_nodes(m) &&
          dxil_module_exit_block(m);
}

static bool
emit_call(struct dxil_buffer *b,
          const unsigned next_value_id,
          const struct dxil_type *func_type,
          dxil_value value_id,
          const dxil_value *args, const size_t num_args)
{
   uint64_t data[256];

   int value_id_delta = next_value_id - value_id;
   assert(value_id <= next_value_id);

   data[0] = 0; // attribute id
   data[1] = 1 << 15; // calling convention etc
   data[2] = func_type->id;
   data[3] = value_id_delta;

   assert(num_args < ARRAY_SIZE(data) - 4);
   for (size_t i = 0; i < num_args; ++i)
      data[4 + i] = next_value_id - args[i];

   return emit_record_no_abbrev(b, FUNC_CODE_INST_CALL, data, 4 + num_args);
}

const dxil_value
dxil_emit_call(struct dxil_module *m,
               const struct dxil_type *func_type,
               dxil_value value_id,
               const dxil_value *args, const size_t num_args)
{
   assert(func_type->type == TYPE_FUNCTION &&
          func_type->function_def.ret_type->type != TYPE_VOID);

   if (!emit_call(&m->code, m->next_value_id, func_type, value_id,
                  args, num_args))
      return DXIL_VALUE_INVALID;

   return m->next_value_id++;
}

bool
dxil_emit_call_void(struct dxil_module *m,
                    const struct dxil_type *func_type,
                    dxil_value value_id,
                    const dxil_value *args, const size_t num_args)
{
   assert(func_type->type == TYPE_FUNCTION &&
          func_type->function_def.ret_type->type == TYPE_VOID);
   return emit_call(&m->code, m->next_value_id, func_type, value_id,
                    args, num_args);
}

bool
dxil_emit_ret_void(struct dxil_module *m)
{
   uint64_t data[] = { FUNC_CODE_INST_RET };
   return emit_func_abbrev_record(m, 8, data, ARRAY_SIZE(data));
}

bool
dxil_emit_function(struct dxil_module *m)
{
   if (!dxil_module_enter_subblock(m, DXIL_FUNCTION_BLOCK, 4) ||
       !dxil_module_emit_record_int(m, FUNC_CODE_DECLAREBLOCKS, 1))
      return false;

   for (int i = 0; i < m->code.blob.size; ++i) {
      if (!dxil_buffer_emit_bits(&m->buf, m->code.blob.data[i], 8))
         return false;
   }
   blob_finish(&m->code.blob);

   if (m->code.buf_bits > 0)
      if (!dxil_buffer_emit_bits(&m->buf, m->code.buf, m->code.buf_bits))
         return false;

   return dxil_module_exit_block(m);
}
