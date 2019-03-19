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

static inline sir_alu_src
sir_alu_isrc(sir_reg *reg)
{
   assert(reg->file == SIR_REG_FILE_LOGICAL);
   return (sir_alu_src) {
      .file = reg->file,
      .type = SIR_TYPE_INT | reg->logical.bit_size,
      .reg = {
         .reg = reg,
      },
   };
}

static inline sir_alu_src
sir_alu_usrc(sir_reg *reg)
{
   assert(reg->file == SIR_REG_FILE_LOGICAL);
   return (sir_alu_src) {
      .file = reg->file,
      .type = SIR_TYPE_UINT | reg->logical.bit_size,
      .reg = {
         .reg = reg,
      },
   };
}

static inline sir_alu_src
sir_alu_fsrc(sir_reg *reg)
{
   assert(reg->file == SIR_REG_FILE_LOGICAL);
   return (sir_alu_src) {
      .file = reg->file,
      .type = SIR_TYPE_FLOAT | reg->logical.bit_size,
      .reg = {
         .reg = reg,
      },
   };
}

static inline sir_reg *
sir_build_ssa_alu(sir_builder *b, enum sir_alu_op op, enum sir_type dest_type,
                  sir_alu_src *src, unsigned num_srcs)
{
   sir_alu_instr *alu = sir_alu_instr_create(b->shader, op,
                                             b->exec_size, b->exec_group);

   unsigned max_bit_size = 0;
   for (unsigned i = 0; i < num_srcs; i++) {
      alu->src[i] = src[i];
      max_bit_size = MAX2(max_bit_size, sir_type_bit_size(src[i].type));
   }

   if (sir_type_bit_size(dest_type) == 0)
      dest_type |= max_bit_size;

   sir_reg *dest_reg =
      sir_builder_new_logical_reg(b, sir_type_bit_size(dest_type), 1);
   dest_reg->logical.ssa = &alu->instr;

   alu->dest = (sir_alu_dest) {
      .file = SIR_REG_FILE_LOGICAL,
      .type = dest_type,
      .reg = {
         .reg = dest_reg,
      },
   };

   sir_builder_insert_instr(b, &alu->instr);

   return dest_reg;
}

#define SIR_BUILDER_DEFINE_ALU1(OP)                                  \
static inline sir_reg *                                              \
sir_##OP(sir_builder *b, enum sir_type dest_type,                    \
         sir_alu_src src0)                                           \
{                                                                    \
   sir_alu_src srcs[] = { src0 };                                    \
   return sir_build_ssa_alu(b, SIR_ALU_OP_##OP, dest_type, srcs, 1); \
}

#define SIR_BUILDER_DEFINE_ALU2(OP)                                  \
static inline sir_reg *                                              \
sir_##OP(sir_builder *b, enum sir_type dest_type,                    \
         sir_alu_src src0, sir_alu_src src1)                         \
{                                                                    \
   sir_alu_src srcs[] = { src0, src1 };                              \
   return sir_build_ssa_alu(b, SIR_ALU_OP_##OP, dest_type, srcs, 1); \
}

#define SIR_BUILDER_DEFINE_ALU3(OP)                                  \
static inline sir_reg *                                              \
sir_##OP(sir_builder *b, enum sir_type dest_type,                    \
         sir_alu_src src0, sir_alu_src src1, sir_alu_src src2)       \
{                                                                    \
   sir_alu_src srcs[] = { src0, src1, src2 };                        \
   return sir_build_ssa_alu(b, SIR_ALU_OP_##OP, dest_type, srcs, 1); \
}

SIR_BUILDER_DEFINE_ALU1(MOV)
SIR_BUILDER_DEFINE_ALU2(AND)
SIR_BUILDER_DEFINE_ALU2(SHR)
SIR_BUILDER_DEFINE_ALU2(SHL)
SIR_BUILDER_DEFINE_ALU2(ADD)

#undef SIR_BUILDER_DEFINE_ALU1
#undef SIR_BUILDER_DEFINE_ALU2
#undef SIR_BUILDER_DEFINE_ALU3

static inline sir_reg *
sir_read_hw_grf(sir_builder *b, uint8_t reg, uint8_t comp,
                enum sir_type type, uint8_t stride)
{
   unsigned type_sz = sir_type_bit_size(type) / 8;
   assert(comp * type_sz < 32);
   uint16_t byte = reg * 32 + comp * type_sz;
   uint8_t size = type_sz + (b->exec_size - 1) * type_sz * stride;
   uint8_t align = type_sz;
   sir_reg *hw_reg = sir_hw_grf_reg_create(b->shader, byte, size, align);

   return sir_MOV(b, type, (sir_alu_src) {
      .file = SIR_REG_FILE_HW_GRF,
      .type = type,
      .reg = {
         .reg = hw_reg,
         .offset = 0,
         .stride = type_sz * stride,
      },
   });
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SIR_BUILDER_H */
