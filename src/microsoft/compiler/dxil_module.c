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

#include <assert.h>

void
dxil_module_init(struct dxil_module *m)
{
   blob_init(&m->module);

   m->abbrev_width = 2;

   m->num_blocks = 0;

   m->buf = 0;
   m->buf_bits = 0;
}

static bool
flush_dword(struct dxil_module *m)
{
   assert(m->buf_bits >= 32 && m->buf_bits < 64);

   uint32_t lower_bits = m->buf & UINT32_MAX;
   if (!blob_write_bytes(&m->module, &lower_bits, sizeof(lower_bits)))
      return false;

   m->buf >>= 32;
   m->buf_bits -= 32;

   return true;
}

bool
dxil_module_emit_bits(struct dxil_module *m, uint32_t data, unsigned width)
{
   assert(m->buf_bits < 32);
   assert(width > 0 && width <= 32);
   assert((data & ~((UINT64_C(1) << width) - 1)) == 0);

   m->buf |= ((uint64_t)data) << m->buf_bits;
   m->buf_bits += width;

   if (m->buf_bits >= 32)
      return flush_dword(m);

   return true;
}

bool
dxil_module_emit_vbr_bits(struct dxil_module *m, uint64_t data,
                          unsigned width)
{
   assert(width > 1 && width <= 32);

   uint32_t tag = UINT32_C(1) << (width - 1);
   uint32_t max = tag - 1;
   while (data > max) {
      uint32_t value = (data & max) | tag;
      data >>= width - 1;

      if (!dxil_module_emit_bits(m, value, width))
         return false;
   }

   return dxil_module_emit_bits(m, data, width);
}

bool
dxil_module_align(struct dxil_module *m)
{
   assert(m->buf_bits < 32);

   if (m->buf_bits) {
      m->buf_bits = 32;
      return flush_dword(m);
   }

   return true;
}

bool
dxil_module_enter_subblock(struct dxil_module *m, unsigned id,
                           unsigned abbrev_width)
{
   if (!dxil_module_emit_abbrev_id(m, DXIL_ENTER_SUBBLOCK) ||
       !dxil_module_emit_vbr_bits(m, id, 8) ||
       !dxil_module_emit_vbr_bits(m, abbrev_width, 4) ||
       !dxil_module_align(m))
      return false;

   assert(m->num_blocks < ARRAY_SIZE(m->blocks));
   m->blocks[m->num_blocks].abbrev_width = m->abbrev_width;
   m->blocks[m->num_blocks].offset = blob_reserve_uint32(&m->module);
   m->num_blocks++;

   m->abbrev_width = abbrev_width;

   return true;
}

bool
dxil_module_exit_block(struct dxil_module *m)
{
   assert(m->num_blocks > 0);
   assert(m->num_blocks < ARRAY_SIZE(m->blocks));

   if (!dxil_module_emit_abbrev_id(m, DXIL_END_BLOCK) ||
       !dxil_module_align(m))
      return false;

   m->num_blocks--;
   m->abbrev_width = m->blocks[m->num_blocks].abbrev_width;

   // patch block-length
   intptr_t offset = m->blocks[m->num_blocks].offset;
   assert(m->module.size >= offset + sizeof(uint32_t));
   uint32_t size = (m->module.size - offset - 1) / sizeof(uint32_t);
   if (!blob_overwrite_uint32(&m->module, offset, size))
      return false;

   return true;
}

static bool
emit_record_no_abbrev(struct dxil_module *m, unsigned code,
                      const uint64_t *data, size_t size)
{
   if (!dxil_module_emit_abbrev_id(m, DXIL_UNABBREV_RECORD) ||
       !dxil_module_emit_vbr_bits(m, code, 6) ||
       !dxil_module_emit_vbr_bits(m, size, 6))
      return false;

   for (size_t i = 0; i < size; ++i)
      if (!dxil_module_emit_vbr_bits(m, data[i], 6))
         return false;

   return true;
}

bool
dxil_module_emit_record(struct dxil_module *m, unsigned code,
                        const uint64_t *data, size_t size)
{
   return emit_record_no_abbrev(m, code, data, size);
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

#define LITERAL(x) { DXIL_OP_LITERAL, (x) }
#define FIXED(x) { DXIL_OP_FIXED, (x) }
#define VBR(x) { DXIL_OP_VBR, (x) }
#define ARRAY() { DXIL_OP_ARRAY, 0 }
#define CHAR6() { DXIL_OP_CHAR6, 0 }
#define BLOB() { DXIL_OP_BLOB, 0 }

static bool
define_abbrev(struct dxil_module *m, const struct dxil_abbrev *a)
{
   if (!dxil_module_emit_abbrev_id(m, DXIL_DEFINE_ABBREV) ||
       !dxil_module_emit_vbr_bits(m, a->num_operands, 5))
      return false;

   for (int i = 0; i < a->num_operands; ++i) {
      if (!dxil_module_emit_bits(m, a->operands[i].type == DXIL_OP_LITERAL, 1))
         return false;
      if (a->operands[i].type == DXIL_OP_LITERAL) {
         if (!dxil_module_emit_vbr_bits(m, a->operands[i].value, 8))
            return false;
      } else {
         if (!dxil_module_emit_bits(m, a->operands[i].type, 3))
            return false;
         if (a->operands[i].type == DXIL_OP_FIXED ||
             a->operands[i].type == DXIL_OP_VBR) {
            if (!dxil_module_emit_vbr_bits(m, a->operands[i].encoding_data, 5))
               return false;
         }
      }
   }

   return true;
}

static bool
switch_to_block(struct dxil_module *m, uint32_t block)
{
   uint64_t data = block;
   return dxil_module_emit_record(m, DXIL_BLOCKINFO_CODE_SETBID, &data, 1);
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
      uint64_t data = attrs[i];
      if (!dxil_module_emit_record(m, PARAMATTR_CODE_ENTRY, &data, 1))
         return false;
   }

   return dxil_module_exit_block(m);
}
