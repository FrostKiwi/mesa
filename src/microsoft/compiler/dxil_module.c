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
