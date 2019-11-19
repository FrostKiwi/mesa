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

#include "util/blob.h"

enum dxil_standard_block {
   DXIL_BLOCKINFO = 0,
   DXIL_FIRST_APPLICATION_BLOCK = 8
};

enum dxil_llvm_block {
   DXIL_MODULE = DXIL_FIRST_APPLICATION_BLOCK
};

enum dxil_fixed_abbrev {
   DXIL_END_BLOCK = 0,
   DXIL_ENTER_SUBBLOCK = 1,
   DXIL_DEFINE_ABBREV = 2,
   DXIL_UNABBREV_RECORD = 3
};

enum dxil_module_code {
   DXIL_MODULE_CODE_VERSION = 1,
   DXIL_MODULE_CODE_TRIPLE = 2,
   DXIL_MODULE_CODE_DATALAYOUT = 3,
   DXIL_MODULE_CODE_ASM = 4,
   DXIL_MODULE_CODE_SECTIONNAME = 5,
   DXIL_MODULE_CODE_DEPLIB = 6,
   DXIL_MODULE_CODE_GLOBALVAR = 7,
   DXIL_MODULE_CODE_FUNCTION = 8,
   DXIL_MODULE_CODE_ALIAS = 9,
   DXIL_MODULE_CODE_PURGEVALS = 10,
   DXIL_MODULE_CODE_GCNAME = 11,
   DXIL_MODULE_CODE_COMDAT = 12,
};

struct dxil_module {
   enum dxil_shader_kind shader_kind;
   unsigned major_version, minor_version;
   struct blob module;

   unsigned abbrev_width;

   struct {
      unsigned abbrev_width;
      intptr_t offset;
   } blocks[16];
   size_t num_blocks;

   uint64_t buf;
   unsigned buf_bits;
};

void
dxil_module_init(struct dxil_module *m);

bool
dxil_module_emit_bits(struct dxil_module *m, uint32_t data, unsigned width);

bool
dxil_module_emit_vbr_bits(struct dxil_module *m, uint64_t data,
                          unsigned width);

bool
dxil_module_align(struct dxil_module *m);

static bool
dxil_module_emit_abbrev_id(struct dxil_module *m, uint32_t id)
{
   return dxil_module_emit_bits(m, id, m->abbrev_width);
}

bool
dxil_module_enter_subblock(struct dxil_module *m, unsigned id,
                           unsigned abbrev_width);

bool
dxil_module_exit_block(struct dxil_module *m);

bool
dxil_module_emit_record(struct dxil_module *m, unsigned code,
                        const uint64_t *data, size_t size);

#ifdef __cplusplus
}
#endif

#endif
