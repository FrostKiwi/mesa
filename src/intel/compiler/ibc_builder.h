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

#ifndef IBC_BUILDER_H
#define IBC_BUILDER_H

#include "ibc.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ibc_builder {
   ibc_shader *shader;
   struct list_head *prev;

   unsigned exec_size;
   unsigned exec_group;
} ibc_builder;

static inline void
ibc_builder_init(ibc_builder *b, ibc_shader *shader, unsigned exec_size)
{
   b->shader = shader;
   ibc_block *first_block =
      list_first_entry(&shader->blocks, ibc_block, link);
   b->prev = &first_block->instrs;

   b->exec_size = exec_size;
   b->exec_group = 0;
}

static inline void
ibc_builder_insert_instr(ibc_builder *b, ibc_instr *instr)
{
   list_add(&instr->link, b->prev);
   b->prev = &instr->link;
}

static inline ibc_reg *
ibc_builder_new_logical_reg(ibc_builder *b,
                            uint8_t bit_size, uint8_t num_comps)
{
   return ibc_logical_reg_create(b->shader, bit_size, num_comps,
                                 b->exec_size, b->exec_group);
}

static inline ibc_alu_src
ibc_alu_isrc(ibc_reg *reg)
{
   assert(reg->file == IBC_REG_FILE_LOGICAL);
   return (ibc_alu_src) {
      .file = reg->file,
      .type = IBC_TYPE_INT | reg->logical.bit_size,
      .reg = {
         .reg = reg,
      },
   };
}

static inline ibc_alu_src
ibc_alu_usrc(ibc_reg *reg)
{
   assert(reg->file == IBC_REG_FILE_LOGICAL);
   return (ibc_alu_src) {
      .file = reg->file,
      .type = IBC_TYPE_UINT | reg->logical.bit_size,
      .reg = {
         .reg = reg,
      },
   };
}

static inline ibc_alu_src
ibc_alu_fsrc(ibc_reg *reg)
{
   assert(reg->file == IBC_REG_FILE_LOGICAL);
   return (ibc_alu_src) {
      .file = reg->file,
      .type = IBC_TYPE_FLOAT | reg->logical.bit_size,
      .reg = {
         .reg = reg,
      },
   };
}

static inline ibc_reg *
ibc_build_ssa_alu(ibc_builder *b, enum ibc_alu_op op, enum ibc_type dest_type,
                  ibc_alu_src *src, unsigned num_srcs)
{
   ibc_alu_instr *alu = ibc_alu_instr_create(b->shader, op,
                                             b->exec_size, b->exec_group);

   unsigned max_bit_size = 0;
   for (unsigned i = 0; i < num_srcs; i++) {
      alu->src[i] = src[i];
      max_bit_size = MAX2(max_bit_size, ibc_type_bit_size(src[i].type));
   }

   if (ibc_type_bit_size(dest_type) == 0)
      dest_type |= max_bit_size;

   ibc_reg *dest_reg =
      ibc_builder_new_logical_reg(b, ibc_type_bit_size(dest_type), 1);
   dest_reg->logical.ssa = &alu->instr;

   alu->dest = (ibc_alu_dest) {
      .file = IBC_REG_FILE_LOGICAL,
      .type = dest_type,
      .reg = {
         .reg = dest_reg,
      },
   };

   ibc_builder_insert_instr(b, &alu->instr);

   return dest_reg;
}

#define IBC_BUILDER_DEFINE_ALU1(OP)                                  \
static inline ibc_reg *                                              \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_alu_src src0)                                           \
{                                                                    \
   ibc_alu_src srcs[] = { src0 };                                    \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 1); \
}

#define IBC_BUILDER_DEFINE_ALU2(OP)                                  \
static inline ibc_reg *                                              \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_alu_src src0, ibc_alu_src src1)                         \
{                                                                    \
   ibc_alu_src srcs[] = { src0, src1 };                              \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 2); \
}

#define IBC_BUILDER_DEFINE_ALU3(OP)                                  \
static inline ibc_reg *                                              \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_alu_src src0, ibc_alu_src src1, ibc_alu_src src2)       \
{                                                                    \
   ibc_alu_src srcs[] = { src0, src1, src2 };                        \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 3); \
}

IBC_BUILDER_DEFINE_ALU1(MOV)
IBC_BUILDER_DEFINE_ALU2(AND)
IBC_BUILDER_DEFINE_ALU2(SHR)
IBC_BUILDER_DEFINE_ALU2(SHL)
IBC_BUILDER_DEFINE_ALU2(ADD)

#undef IBC_BUILDER_DEFINE_ALU1
#undef IBC_BUILDER_DEFINE_ALU2
#undef IBC_BUILDER_DEFINE_ALU3

static inline ibc_reg *
ibc_read_hw_grf(ibc_builder *b, uint8_t reg, uint8_t comp,
                enum ibc_type type, uint8_t stride)
{
   unsigned type_sz = ibc_type_bit_size(type) / 8;
   assert(comp * type_sz < 32);
   uint16_t byte = reg * 32 + comp * type_sz;
   uint8_t size = type_sz + (b->exec_size - 1) * type_sz * stride;
   uint8_t align = type_sz;
   ibc_reg *hw_reg = ibc_hw_grf_reg_create(b->shader, byte, size, align);

   return ibc_MOV(b, type, (ibc_alu_src) {
      .file = IBC_REG_FILE_HW_GRF,
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

#endif /* IBC_BUILDER_H */
