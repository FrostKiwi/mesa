/*
 * Copyright © 2018 Intel Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "sir.h"

static sir_reg *
sir_reg_create(sir_shader *shader, enum sir_reg_file file)
{
   sir_reg *reg = rzalloc(shader, sir_reg);

   reg->file = file;
   list_addtail(&reg->link, &shader->regs);

   return reg;
}

sir_reg *
sir_logical_reg_create(sir_shader *shader,
                       uint8_t bit_size, uint8_t num_comps,
                       uint8_t simd_width, uint8_t simd_group)
{
   sir_reg *reg = sir_reg_create(shader, SIR_REG_FILE_LOGICAL);

   reg->logical.bit_size = bit_size;
   reg->logical.num_comps = num_comps;
   reg->logical.simd_width = simd_width;
   reg->logical.simd_group = simd_group;

   return reg;
}

sir_reg *
sir_hw_grf_reg_create(sir_shader *shader,
                      uint16_t byte, uint8_t size, uint8_t align)
{
   sir_reg *reg = sir_reg_create(shader, SIR_REG_FILE_LOGICAL);

   reg->hw_grf.byte = byte;
   reg->hw_grf.size = size;
   reg->hw_grf.align = align;

   return reg;
}

sir_block *
sir_block_create(sir_shader *shader)
{
   sir_block *block = rzalloc(shader, sir_block);

   list_inithead(&block->instrs);

   return block;
}

sir_shader *
sir_shader_create(void *mem_ctx,
                  const struct gen_device_info *devinfo)
{
   sir_shader *shader = rzalloc(mem_ctx, sir_shader);

   shader->devinfo = devinfo,

   list_inithead(&shader->blocks);
   sir_block *first_block = sir_block_create(shader);
   list_add(&first_block->link, &shader->blocks);

   list_inithead(&shader->regs);

   return shader;
}
