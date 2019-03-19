/*
 * Copyright © 2019 Intel Corporation
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

#ifndef SIR_BUILDER_H
#define SIR_BUILDER_H

#include "sir.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct sir_builder {
   sir_shader *shader;
   struct list_head *prev;

   unsigned exec_size;
   unsigned exec_group;
} sir_builder;

static inline void
sir_builder_init(sir_builder *b, sir_shader *shader, unsigned exec_size)
{
   b->shader = shader;
   sir_block *first_block =
      list_first_entry(&shader->blocks, sir_block, link);
   b->prev = &first_block->instrs;

   b->exec_size = exec_size;
   b->exec_group = 0;
}

static inline void
sir_builder_insert_instr(sir_builder *b, sir_instr *instr)
{
   list_add(&instr->link, b->prev);
   b->prev = &instr->link;
}

static inline sir_reg *
sir_builder_new_logical_reg(sir_builder *b,
                            uint8_t bit_size, uint8_t num_comps)
{
   return sir_logical_reg_create(b->shader, bit_size, num_comps,
                                 b->exec_size, b->exec_group);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SIR_BUILDER_H */
