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

#include "util/half_float.h"

#ifdef __cplusplus
extern "C" {
#endif

#define IBC_BUILDER_GROUP_STACK_SIZE 4

struct ibc_builder_simd_group {
   unsigned simd_width;
   unsigned simd_group;
   bool we_all;
};

typedef struct ibc_builder {
   ibc_shader *shader;
   struct list_head *prev;

   unsigned simd_width;
   unsigned simd_group;
   bool we_all;

   unsigned _group_stack_size;
   struct ibc_builder_simd_group _group_stack[4];
} ibc_builder;

static inline void
ibc_builder_init(ibc_builder *b, ibc_shader *shader, unsigned simd_width)
{
   b->shader = shader;
   ibc_block *first_block =
      list_first_entry(&shader->blocks, ibc_block, link);
   b->prev = &first_block->instrs;

   b->simd_width = simd_width;
   b->simd_group = 0;
   b->we_all = false;

   b->_group_stack_size = 0;
}

static inline void
_ibc_builder_push(ibc_builder *b)
{
   assert(b->_group_stack_size < IBC_BUILDER_GROUP_STACK_SIZE);
   b->_group_stack[b->_group_stack_size] =
      (struct ibc_builder_simd_group) {
         .simd_width = b->simd_width,
         .simd_group = b->simd_group,
         .we_all = b->we_all,
      };
   b->_group_stack_size++;
}

static inline void
ibc_builder_pop(ibc_builder *b)
{
   assert(b->_group_stack_size > 0);
   b->_group_stack_size--;
   b->simd_width = b->_group_stack[b->_group_stack_size].simd_width;
   b->simd_group = b->_group_stack[b->_group_stack_size].simd_group;
   b->we_all = b->_group_stack[b->_group_stack_size].we_all;
}

static inline void
ibc_builder_push_group(ibc_builder *b,
                       unsigned simd_width, unsigned simd_group)
{
   /* We're only allowed to restrict the size */
   assert(b->simd_group + simd_group + simd_width <= b->simd_width);
   _ibc_builder_push(b);
   b->simd_width = simd_width;
   b->simd_group += simd_group;
}

static inline void
ibc_builder_push_we_all(ibc_builder *b, unsigned simd_width)
{
   /* We're only allowed to restrict the size */
   _ibc_builder_push(b);
   b->simd_width = simd_width;
   b->simd_group = 0;
   b->we_all = true;
}

static inline void
ibc_builder_push_scalar(ibc_builder *b)
{
   ibc_builder_push_we_all(b, 1);
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
                                 b->simd_width, b->simd_group);
}

static inline ibc_reg_ref
ibc_typed_ref(const ibc_reg *reg, enum ibc_type type)
{
   assert(reg->file == IBC_REG_FILE_LOGICAL);
   if (ibc_type_bit_size(type) == 0)
      type |= reg->logical.bit_size;
   return (ibc_reg_ref) {
      .file = reg->file,
      .reg = reg,
      .type = type,
   };
}

static inline ibc_reg_ref
ibc_ref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_INVALID);
}

static inline ibc_reg_ref
ibc_iref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_INT);
}

static inline ibc_reg_ref
ibc_uref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_UINT);
}

static inline ibc_reg_ref
ibc_fref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_FLOAT);
}

static inline ibc_reg_ref
ibc_imm_ref(enum ibc_type type, char *imm, unsigned imm_size)
{
   ibc_reg_ref ref = {
      .file = IBC_REG_FILE_IMM,
      .type = type,
   };
   memcpy(ref.imm, imm, imm_size);
   return ref;
}

#define IBC_BUILDER_DEFINE_IMM_HELPER(tp, TP, c_type)          \
static inline ibc_reg_ref                                      \
ibc_imm_##tp(c_type x)                                         \
{                                                              \
   return ibc_imm_ref(IBC_TYPE_##TP, (char *)&x, sizeof(x));   \
}

IBC_BUILDER_DEFINE_IMM_HELPER(w, W, int16_t)
IBC_BUILDER_DEFINE_IMM_HELPER(uw, UW, uint16_t)
IBC_BUILDER_DEFINE_IMM_HELPER(v, V, int32_t)
IBC_BUILDER_DEFINE_IMM_HELPER(uv, UV, int32_t)
IBC_BUILDER_DEFINE_IMM_HELPER(d, D, int32_t)
IBC_BUILDER_DEFINE_IMM_HELPER(ud, UD, uint32_t)
IBC_BUILDER_DEFINE_IMM_HELPER(f, F, float)
IBC_BUILDER_DEFINE_IMM_HELPER(q, Q, int64_t)
IBC_BUILDER_DEFINE_IMM_HELPER(uq, UQ, uint64_t)
IBC_BUILDER_DEFINE_IMM_HELPER(df, DF, double)

#undef IBC_BUILDER_DEFINE_IMM_HELPER

static inline ibc_reg_ref
ibc_imm_hf(float x)
{
   uint16_t hf = _mesa_float_to_half(x);
   return ibc_imm_ref(IBC_TYPE_HF, (char *)&hf, sizeof(hf));
}

static inline ibc_alu_instr *
ibc_build_alu(ibc_builder *b, enum ibc_alu_op op, ibc_reg_ref dest,
              ibc_reg_ref *src, unsigned num_srcs)
{
   ibc_alu_instr *alu = ibc_alu_instr_create(b->shader, op,
                                             b->simd_width, b->simd_group);
   alu->instr.we_all = b->we_all;

   for (unsigned i = 0; i < num_srcs; i++)
      alu->src[i].ref = src[i];
   alu->dest.ref = dest;

   ibc_builder_insert_instr(b, &alu->instr);

   return alu;
}

static inline ibc_reg_ref
ibc_build_ssa_alu(ibc_builder *b, enum ibc_alu_op op, enum ibc_type dest_type,
                  ibc_reg_ref *src, unsigned num_srcs)
{
   if (ibc_type_bit_size(dest_type) == 0) {
      unsigned max_bit_size = 0;
      for (unsigned i = 0; i < num_srcs; i++)
         max_bit_size = MAX2(max_bit_size, ibc_type_bit_size(src[i].type));
      dest_type |= max_bit_size;
   }

   ibc_reg *dest_reg =
      ibc_builder_new_logical_reg(b, ibc_type_bit_size(dest_type), 1);
   ibc_reg_ref dest_ref = ibc_typed_ref(dest_reg, dest_type);

   ibc_build_alu(b, op, dest_ref, src, num_srcs);

   return dest_ref;
}

#define IBC_BUILDER_DEFINE_ALU1(OP)                                  \
static inline ibc_reg_ref                                            \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_reg_ref src0)                                           \
{                                                                    \
   ibc_reg_ref srcs[] = { src0 };                                    \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 1); \
}

#define IBC_BUILDER_DEFINE_ALU2(OP)                                  \
static inline ibc_reg_ref                                            \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_reg_ref src0, ibc_reg_ref src1)                         \
{                                                                    \
   ibc_reg_ref srcs[] = { src0, src1 };                              \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 2); \
}

#define IBC_BUILDER_DEFINE_ALU3(OP)                                  \
static inline ibc_reg_ref                                            \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_reg_ref src0, ibc_reg_ref src1, ibc_reg_ref src2)       \
{                                                                    \
   ibc_reg_ref srcs[] = { src0, src1, src2 };                        \
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

static inline ibc_reg_ref
ibc_read_hw_grf(ibc_builder *b, uint8_t reg, uint8_t comp,
                enum ibc_type type, uint8_t stride)
{
   unsigned type_sz = ibc_type_bit_size(type) / 8;
   assert(comp * type_sz < 32);
   uint16_t byte = reg * 32 + comp * type_sz;
   uint8_t size = type_sz + (b->simd_width - 1) * type_sz * stride;
   uint8_t align = type_sz;
   ibc_reg *hw_reg = ibc_hw_grf_reg_create(b->shader, byte, size, align);

   return ibc_MOV(b, type, (ibc_reg_ref) {
      .file = IBC_REG_FILE_HW_GRF,
      .type = type,
      .reg = hw_reg,
      .offset = 0,
      .stride = type_sz * stride,
   });
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_BUILDER_H */
