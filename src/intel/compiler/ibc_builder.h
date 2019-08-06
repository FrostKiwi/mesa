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

#include "util/bitscan.h"
#include "util/half_float.h"

#ifdef __cplusplus
extern "C" {
#endif

#define IBC_BUILDER_GROUP_STACK_SIZE 4

struct ibc_builder_simd_group {
   unsigned simd_group;
   unsigned simd_width;
   bool we_all;
};

typedef struct ibc_builder {
   ibc_shader *shader;

   ibc_cursor cursor;

   unsigned simd_group;
   unsigned simd_width;
   bool we_all;

   ibc_merge_instr *block_start;

   unsigned _group_stack_size;
   struct ibc_builder_simd_group _group_stack[4];
} ibc_builder;

static inline void
ibc_builder_init(ibc_builder *b, ibc_shader *shader)
{
   b->shader = shader;
   b->cursor = ibc_before_shader(shader);

   b->simd_group = 0;
   b->simd_width = shader->simd_width;
   b->we_all = false;

   b->block_start = NULL;

   b->_group_stack_size = 0;
}

static inline void
_ibc_builder_push(ibc_builder *b)
{
   assert(b->_group_stack_size < IBC_BUILDER_GROUP_STACK_SIZE);
   b->_group_stack[b->_group_stack_size] =
      (struct ibc_builder_simd_group) {
         .simd_group = b->simd_group,
         .simd_width = b->simd_width,
         .we_all = b->we_all,
      };
   b->_group_stack_size++;
}

static inline void
ibc_builder_pop(ibc_builder *b)
{
   assert(b->_group_stack_size > 0);
   b->_group_stack_size--;
   b->simd_group = b->_group_stack[b->_group_stack_size].simd_group;
   b->simd_width = b->_group_stack[b->_group_stack_size].simd_width;
   b->we_all = b->_group_stack[b->_group_stack_size].we_all;
}

static inline void
ibc_builder_push_group(ibc_builder *b,
                       unsigned simd_group, unsigned simd_width)
{
   /* We're only allowed to restrict the size */
   assert(simd_group + simd_width <= b->simd_width);
   if (b->we_all)
      assert(simd_group == 0);
   else
      assert(simd_width >= 8);

   _ibc_builder_push(b);
   b->simd_group += simd_group;
   b->simd_width = simd_width;
}

static inline void
ibc_builder_push_we_all(ibc_builder *b, unsigned simd_width)
{
   /* We're only allowed to restrict the size */
   _ibc_builder_push(b);
   b->simd_group = 0;
   b->simd_width = simd_width;
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
   ibc_instr_insert(instr, b->cursor);
   b->cursor = ibc_after_instr(instr);
}

static inline ibc_reg_ref
ibc_hw_grf_ref(uint8_t nr, uint8_t subnr, enum ibc_type type)
{
   return (ibc_reg_ref) {
      .file = IBC_REG_FILE_HW_GRF,
      .type = IBC_TYPE_UD,
      .hw_grf = {
         .byte = (uint16_t)nr * REG_SIZE + subnr * ibc_type_byte_size(type),
         .vstride = 8 * ibc_type_byte_size(type),
         .width = 8,
         .hstride = ibc_type_byte_size(type),
      },
   };
}

static inline ibc_reg_ref
ibc_flag_ref(uint8_t subnr, uint8_t bit)
{
   return (ibc_reg_ref) {
      .file = IBC_REG_FILE_FLAG,
      .type = IBC_TYPE_FLAG,
      .flag = {
         .bit = subnr * 16 + bit,
      },
   };
}

static inline ibc_reg_ref
ibc_typed_ref(const ibc_reg *reg, enum ibc_type type)
{
   ibc_reg_ref ref = {
      .file = reg->file,
      .reg = reg,
      .type = type,
   };
   if (ibc_type_bit_size(type) == 0) {
      switch (reg->file) {
      case IBC_REG_FILE_LOGICAL:
         ref.type |= reg->logical.bit_size;
         break;
      case IBC_REG_FILE_FLAG:
         ref.type = IBC_TYPE_FLAG;
         break;
      default:
         unreachable("Unsupported register file for automatic types");
      }
   }
   switch (reg->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Unsupported register file");

   case IBC_REG_FILE_LOGICAL:
      ref.logical = (ibc_logical_reg_ref) { 0, };
      return ref;

   case IBC_REG_FILE_HW_GRF:
      ref.hw_grf = (ibc_hw_grf_reg_ref) {
         .vstride = 8 * ibc_type_byte_size(ref.type),
         .width = 8,
         .hstride = ibc_type_byte_size(ref.type),
      };
      return ref;

   case IBC_REG_FILE_FLAG:
      assert(ref.type == IBC_TYPE_FLAG);
      return ref;
   }

   unreachable("Unknown register file");
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
ibc_null(enum ibc_type type)
{
   return (ibc_reg_ref) {
      .file = IBC_REG_FILE_NONE,
      .type = type,
   };
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

static inline ibc_reg_ref
ibc_imm_zero(enum ibc_type type)
{
   return (ibc_reg_ref) {
      .file = IBC_REG_FILE_IMM,
      .type = type,
   };
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

static inline ibc_reg_ref
ibc_builder_new_logical_reg(ibc_builder *b, enum ibc_type type,
                            uint8_t num_comps)
{
   ibc_reg *reg = ibc_logical_reg_create(b->shader,
                                         ibc_type_bit_size(type), num_comps,
                                         b->simd_group, b->simd_width);
   return ibc_typed_ref(reg, type);
}

static inline ibc_alu_instr *
ibc_build_alu(ibc_builder *b, enum ibc_alu_op op, ibc_reg_ref dest,
              ibc_reg_ref flag, enum brw_conditional_mod cmod,
              ibc_reg_ref *src, unsigned num_srcs)
{
   ibc_alu_instr *alu = ibc_alu_instr_create(b->shader, op,
                                             b->simd_group, b->simd_width);
   alu->instr.we_all = b->we_all;

   alu->instr.flag = flag;
   alu->cmod = cmod;

   for (unsigned i = 0; i < num_srcs; i++)
      alu->src[i].ref = src[i];
   alu->dest = dest;

   ibc_builder_insert_instr(b, &alu->instr);

   return alu;
}

static inline ibc_alu_instr *
ibc_build_alu1(ibc_builder *b, enum ibc_alu_op op, ibc_reg_ref dest,
               ibc_reg_ref src0)
{
   ibc_reg_ref srcs[] = { src0 };
   return ibc_build_alu(b, op, dest, ibc_null(IBC_TYPE_FLAG),
                        BRW_CONDITIONAL_NONE, srcs, 1);
}

static inline ibc_alu_instr *
ibc_build_alu2(ibc_builder *b, enum ibc_alu_op op, ibc_reg_ref dest,
               ibc_reg_ref src0, ibc_reg_ref src1)
{
   ibc_reg_ref srcs[] = { src0, src1 };
   return ibc_build_alu(b, op, dest, ibc_null(IBC_TYPE_FLAG),
                        BRW_CONDITIONAL_NONE, srcs, 2);
}

static inline enum ibc_type
_ibc_builder_dest_type(enum ibc_type dest_type,
                       ibc_reg_ref *src, unsigned num_srcs)
{
   if (ibc_type_bit_size(dest_type) == 0) {
      unsigned max_bit_size = 0;
      for (unsigned i = 0; i < num_srcs; i++)
         max_bit_size = MAX2(max_bit_size, ibc_type_bit_size(src[i].type));
      dest_type |= max_bit_size;
   }
   return dest_type;
}

static inline ibc_alu_instr *
ibc_build_alu2_cmod(ibc_builder *b, enum ibc_alu_op op, ibc_reg_ref dest,
                    enum brw_conditional_mod cmod,
                    ibc_reg_ref src0, ibc_reg_ref src1)
{
   ibc_reg_ref srcs[] = { src0, src1 };
   if (dest.type == IBC_TYPE_FLAG) {
      enum ibc_type dest_type = _ibc_builder_dest_type(src0.type, srcs,
                                                       ARRAY_SIZE(srcs));
      return ibc_build_alu(b, op, ibc_null(dest_type), dest, cmod, srcs, 2);
   } else if (cmod != BRW_CONDITIONAL_NONE) {
      /* We need a flag register even though the result may never be used */
      ibc_reg_ref flag = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);

      return ibc_build_alu(b, op, dest, flag, cmod, srcs, ARRAY_SIZE(srcs));
   } else {
      return ibc_build_alu2(b, op, dest, src0, src1);
   }
}

static inline ibc_reg_ref
ibc_build_ssa_alu(ibc_builder *b, enum ibc_alu_op op, enum ibc_type dest_type,
                  ibc_reg_ref *src, unsigned num_srcs)
{
   dest_type = _ibc_builder_dest_type(dest_type, src, num_srcs);
   ibc_reg_ref dest_ref = ibc_builder_new_logical_reg(b, dest_type, 1);

   ibc_build_alu(b, op, dest_ref, ibc_null(IBC_TYPE_FLAG),
                 BRW_CONDITIONAL_NONE, src, num_srcs);

   return dest_ref;
}

static inline ibc_reg_ref
ibc_build_ssa_flag_alu(ibc_builder *b, enum ibc_alu_op op,
                       enum ibc_type dest_type,
                       enum brw_conditional_mod cmod,
                       ibc_reg_ref *src, unsigned num_srcs)
{
   ibc_reg_ref flag = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);

   dest_type = _ibc_builder_dest_type(dest_type, src, num_srcs);
   ibc_build_alu(b, op, ibc_null(dest_type), flag, cmod, src, num_srcs);

   return flag;
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
IBC_BUILDER_DEFINE_ALU1(NOT)
IBC_BUILDER_DEFINE_ALU2(AND)
IBC_BUILDER_DEFINE_ALU2(OR)
IBC_BUILDER_DEFINE_ALU2(SHR)
IBC_BUILDER_DEFINE_ALU2(SHL)
IBC_BUILDER_DEFINE_ALU2(ADD)
IBC_BUILDER_DEFINE_ALU3(MAD)

#undef IBC_BUILDER_DEFINE_ALU1
#undef IBC_BUILDER_DEFINE_ALU2
#undef IBC_BUILDER_DEFINE_ALU3

static inline void
ibc_MOV_to(ibc_builder *b, ibc_reg_ref dest, ibc_reg_ref src)
{
   ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
}

static inline ibc_reg_ref
ibc_MOV_to_flag(ibc_builder *b, enum brw_conditional_mod cmod, ibc_reg_ref src)
{
   return ibc_build_ssa_flag_alu(b, IBC_ALU_OP_MOV, src.type, cmod, &src, 1);
}

static inline void
ibc_MOV_raw_vec_to(ibc_builder *b, ibc_reg_ref dest,
                   ibc_reg_ref src, unsigned num_comps)
{
   assert(ibc_type_bit_size(src.type) == ibc_type_bit_size(dest.type));

   if (ibc_type_base_type(src.type) == IBC_TYPE_INVALID)
      src.type |= IBC_TYPE_UINT;
   dest.type = src.type;

   /* TODO: This needs to be adjusted more carefully */
   const unsigned simd_width = MIN2(b->simd_width, 16);
   if (num_comps > 1 || simd_width != b->simd_width) {
      assert(src.file == IBC_REG_FILE_LOGICAL);
      assert(dest.file == IBC_REG_FILE_LOGICAL);
   }

   for (unsigned i = 0; i < num_comps; i++) {
      for (unsigned g = 0; g < b->simd_width; g += simd_width) {
         ibc_builder_push_group(b, g, MIN2(b->simd_width, 16));
         ibc_MOV_to(b, dest, src);
         ibc_builder_pop(b);
      }
      src.logical.comp++;
      dest.logical.comp++;
   }
}

static inline ibc_reg_ref
ibc_MOV_raw(ibc_builder *b, ibc_reg_ref src)
{
   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, src.type, 1);
   ibc_MOV_raw_vec_to(b, dest, src, 1);
   return dest;
}

static inline ibc_reg_ref
ibc_SEL(ibc_builder *b, enum ibc_type dest_type,
        ibc_reg_ref flag, ibc_reg_ref src0, ibc_reg_ref src1)
{
   ibc_reg_ref srcs[] = { src0, src1 };
   ibc_reg_ref dest = ibc_build_ssa_alu(b, IBC_ALU_OP_SEL, dest_type, srcs, 2);
   ibc_alu_instr *sel = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   ibc_instr_set_predicate(&sel->instr, flag, BRW_PREDICATE_NORMAL, false);
   return dest;
}

static inline ibc_reg_ref
ibc_MIN(ibc_builder *b, enum ibc_type dest_type,
        ibc_reg_ref src0, ibc_reg_ref src1)
{
   ibc_reg_ref srcs[] = { src0, src1 };
   ibc_reg_ref dest = ibc_build_ssa_alu(b, IBC_ALU_OP_SEL, dest_type, srcs, 2);
   ibc_alu_instr *sel = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   sel->cmod = BRW_CONDITIONAL_L;
   return dest;
}

static inline ibc_reg_ref
ibc_MAX(ibc_builder *b, enum ibc_type dest_type,
        ibc_reg_ref src0, ibc_reg_ref src1)
{
   ibc_reg_ref srcs[] = { src0, src1 };
   ibc_reg_ref dest = ibc_build_ssa_alu(b, IBC_ALU_OP_SEL, dest_type, srcs, 2);
   ibc_alu_instr *sel = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   sel->cmod = BRW_CONDITIONAL_GE;
   return dest;
}

static inline ibc_reg_ref
ibc_CMP(ibc_builder *b, enum ibc_type dest_type,
        enum brw_conditional_mod cmod,
        ibc_reg_ref src0, ibc_reg_ref src1)
{
   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, dest_type, 1);
   ibc_build_alu2_cmod(b, IBC_ALU_OP_CMP, dest, cmod, src0, src1);
   return dest;
}

static inline ibc_reg_ref
ibc_PLN(ibc_builder *b, ibc_reg_ref vert, ibc_reg_ref bary)
{
   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, IBC_TYPE_F, 1);

   assert(vert.file == IBC_REG_FILE_LOGICAL);
   assert(vert.reg->logical.simd_width == 1);

   for (unsigned g = 0; g < b->simd_width; g += 16) {
      ibc_builder_push_group(b, g, MIN2(16, b->simd_width));

      ibc_intrinsic_instr *pln =
         ibc_intrinsic_instr_create(b->shader, IBC_INTRINSIC_OP_PLN,
                                    b->simd_group, b->simd_width,
                                    1 + (b->simd_width / 8));
      pln->src[0] = (ibc_intrinsic_src) {
         .ref = vert,
         .simd_group = b->simd_group,
         .simd_width = b->simd_width,
         .num_comps = 4,
      };
      assert(b->simd_width >= 8);
      for (unsigned i = 0; i < (b->simd_width / 8); i++) {
         pln->src[1 + i] = (ibc_intrinsic_src) {
            .ref = bary,
            .simd_group = b->simd_group + i * 8,
            .simd_width = 8,
            .num_comps = 2,
         };
      }
      pln->dest = dest;
      pln->num_dest_comps = 1;
      ibc_builder_insert_instr(b, &pln->instr);

      ibc_builder_pop(b);
   }

   return dest;
}

static inline ibc_reg_ref
ibc_SIMD_ZIP(ibc_builder *b, ibc_reg_ref *srcs, unsigned num_srcs,
             unsigned num_comps)
{
   ibc_intrinsic_instr *zip =
      ibc_intrinsic_instr_create(b->shader, IBC_INTRINSIC_OP_SIMD_ZIP,
                                 b->simd_group, b->simd_width, num_srcs);

   assert(num_srcs > 1 && b->simd_width % num_srcs == 0);
   const unsigned src_width = b->simd_width / num_srcs;

   for (unsigned i = 0; i < num_srcs; i++) {
      assert(srcs[i].type == srcs[0].type);
      zip->src[i] = (ibc_intrinsic_src) {
         .ref = srcs[i],
         .simd_group = b->simd_group + src_width * i,
         .simd_width = src_width,
         .num_comps = num_comps,
      };
   }

   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, srcs[0].type, num_comps);
   zip->dest = dest;
   zip->num_dest_comps = num_comps;

   ibc_builder_insert_instr(b, &zip->instr);

   return dest;
}

static inline ibc_reg_ref
ibc_SIMD_ZIP2(ibc_builder *b, ibc_reg_ref src0, ibc_reg_ref src1,
              unsigned num_comps)
{
   ibc_reg_ref srcs[2] = { src0, src1 };
   return ibc_SIMD_ZIP(b, srcs, 2, num_comps);
}

static inline ibc_reg_ref
ibc_VEC(ibc_builder *b, ibc_reg_ref *srcs, unsigned num_comps)
{
   assert(num_comps > 0);
   if (num_comps == 1)
      return ibc_MOV_raw(b, srcs[0]);

   ibc_intrinsic_instr *vec =
      ibc_intrinsic_instr_create(b->shader, IBC_INTRINSIC_OP_VEC,
                                 b->simd_group, b->simd_width, num_comps);
   vec->instr.we_all = b->we_all;

   enum ibc_type bits_type = ibc_type_bit_size(srcs[0].type);
   for (unsigned i = 0; i < num_comps; i++) {
      assert(srcs[i].type == srcs[0].type);
      vec->src[i].ref = srcs[i];
      vec->src[i].ref.type = bits_type;
      vec->src[i].num_comps = 1;
   }

   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, bits_type, num_comps);
   vec->dest = dest;
   vec->num_dest_comps = num_comps;

   ibc_builder_insert_instr(b, &vec->instr);

   return dest;
}

static inline void
ibc_build_alu_scan(ibc_builder *b, enum ibc_alu_op op, ibc_reg_ref tmp,
                   enum brw_conditional_mod cmod,
                   unsigned final_cluster_size)
{
   assert(b->simd_width >= 8);
   const uint8_t scan_simd_width = b->simd_width;
   assert(tmp.file == IBC_REG_FILE_HW_GRF);
   assert(tmp.hw_grf.hstride == ibc_type_byte_size(tmp.type));
   assert(tmp.hw_grf.hstride * tmp.hw_grf.width == tmp.hw_grf.vstride);
   assert(util_is_power_of_two_nonzero(final_cluster_size));

   if (final_cluster_size >= 2) {
      ibc_builder_push_we_all(b, b->simd_width / 2);

      ibc_reg_ref left = tmp;
      left.hw_grf.vstride *= 2;
      left.hw_grf.hstride *= 2;

      ibc_reg_ref right = tmp;
      right.hw_grf.byte += 1 * tmp.hw_grf.hstride;
      right.hw_grf.vstride *= 2;
      right.hw_grf.hstride *= 2;

      ibc_build_alu2_cmod(b, op, right, cmod, left, right);

      ibc_builder_pop(b);
   }

   unsigned cluster_size = 4;
   /* For 32-bit and smaller types, we can be a bit clever with the second
    * step and do it in two strided instructions with a stride of four.  For
    * 64-bit types, we have to fall back to the general case below.
    * Fortunately, that case works out to the same number of instructions for
    * 64-bit types so there's no great loss.
    */
   if (final_cluster_size >= 4 && ibc_type_bit_size(tmp.type) <= 32) {
      ibc_builder_push_we_all(b, b->simd_width / 4);

      ibc_reg_ref left = tmp;
      left.hw_grf.byte += 1 * tmp.hw_grf.hstride;
      left.hw_grf.vstride *= 4;
      left.hw_grf.hstride *= 4;

      ibc_reg_ref right = tmp;
      right.hw_grf.byte += 2 * tmp.hw_grf.hstride;
      right.hw_grf.vstride *= 4;
      right.hw_grf.hstride *= 4;

      ibc_build_alu2_cmod(b, op, right, cmod, left, right);

      right.hw_grf.byte += 1 * tmp.hw_grf.hstride;

      ibc_build_alu2_cmod(b, op, right, cmod, left, right);

      ibc_builder_pop(b);

      cluster_size *= 2;
   }

   for (; cluster_size <= final_cluster_size; cluster_size *= 2) {
      const unsigned half_size = cluster_size / 2;
      ibc_builder_push_we_all(b, half_size);

      for (unsigned g = 0; g < scan_simd_width; g += cluster_size) {
         ibc_reg_ref left = tmp;
         left.hw_grf.byte += (g + half_size - 1) * tmp.hw_grf.hstride;
         left.hw_grf.vstride = 0;
         left.hw_grf.width = 1;
         left.hw_grf.hstride = 0;

         ibc_reg_ref right = tmp;
         right.hw_grf.byte += (g + half_size) * tmp.hw_grf.hstride;

         ibc_build_alu2_cmod(b, op, right, cmod, left, right);
      }

      ibc_builder_pop(b);
   }
}

static inline ibc_reg_ref
ibc_cluster_broadcast(ibc_builder *b, enum ibc_type dest_type,
                      ibc_reg_ref src, unsigned cluster_size)
{
   if (cluster_size * ibc_type_byte_size(src.type) >= REG_SIZE * 2) {
      /* In this case, the distance between clusters is at least 2 GRFs so we
       * don't need to do any weird striding.
       */
      ibc_reg_ref dest = ibc_builder_new_logical_reg(b, src.type, 1);
      const unsigned num_clusters = cluster_size / b->simd_width;
      for (unsigned cluster = 0; cluster < num_clusters; cluster++) {
         ibc_builder_push_group(b, cluster * cluster_size,  cluster_size);

         const unsigned comp = cluster * cluster_size +
                               (cluster_size - 1);
         ibc_reg_ref comp_src = src;
         ibc_hw_grf_slice_simd_group(&comp_src.hw_grf, comp, 1);
         ibc_hw_grf_mul_stride(&comp_src.hw_grf, 0);
         ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, comp_src);

         ibc_builder_pop(b);
      }
      return dest;
   } else {
      ibc_reg_ref strided_src = src;
      strided_src.hw_grf = (ibc_hw_grf_reg_ref) {
         .byte = (cluster_size - 1) * ibc_type_byte_size(src.type),
         .vstride = cluster_size * ibc_type_byte_size(src.type),
         .width = cluster_size,
         .hstride = 0,
      };
      return ibc_MOV(b, dest_type, strided_src);
   }
}

static inline ibc_merge_pred*
ibc_builder_add_merge_pred(ibc_builder *b, struct list_head *pred_list,
                           bool logical, ibc_branch_instr *branch)
{
   ibc_merge_pred *pred = ralloc(b->shader, ibc_merge_pred);
   pred->logical = logical;
   pred->branch = branch;
   list_addtail(&pred->link, pred_list);
   return pred;
}

static inline ibc_merge_instr *
ibc_build_merge(ibc_builder *b, enum ibc_merge_op op,
                struct list_head *preds)
{
   assert(b->simd_group == 0);
   ibc_merge_instr *merge =
      ibc_merge_instr_create(b->shader, op, b->simd_width);

   /* This is the merge instruction so it starts a block */
   assert(b->block_start == NULL);
   b->block_start = merge;

   if (op != IBC_MERGE_OP_START)
      list_splicetail(preds, &merge->preds);

   ibc_builder_insert_instr(b, &merge->instr);

   return merge;
}

static inline ibc_branch_instr *
ibc_build_branch(ibc_builder *b, enum ibc_branch_op op, ibc_reg_ref pred,
                 enum brw_predicate predicate, bool pred_inverse)
{
   assert(b->simd_group == 0);
   ibc_branch_instr *branch =
      ibc_branch_instr_create(b->shader, op, b->simd_width);

   if (predicate != BRW_PREDICATE_NONE) {
      branch->instr.flag = pred;
      branch->instr.predicate = predicate;
      branch->instr.pred_inverse = pred_inverse;
   }

   /* This is the branch instruction, tie up the loose ends */
   assert(b->block_start != NULL);
   branch->block_start = b->block_start;
   b->block_start->block_end = branch;
   b->block_start = NULL;

   ibc_builder_insert_instr(b, &branch->instr);

   return branch;
}

static inline ibc_merge_instr *
ibc_start(ibc_builder *b)
{
   return ibc_build_merge(b, IBC_MERGE_OP_START, NULL);
}

static inline ibc_branch_instr *
ibc_end(ibc_builder *b)
{
   return ibc_build_branch(b, IBC_BRANCH_OP_END, ibc_null(IBC_TYPE_UD),
                           BRW_PREDICATE_NONE, false);
}

static inline ibc_branch_instr *
ibc_if(ibc_builder *b, ibc_reg_ref pred,
       enum brw_predicate predicate, bool pred_inverse)
{
   ibc_branch_instr *_if =
      ibc_build_branch(b, IBC_BRANCH_OP_IF, pred, predicate, pred_inverse);

   /* The then block has to start with a merge instruction */
   struct list_head preds;
   list_inithead(&preds);
   ibc_builder_add_merge_pred(b, &preds, true, _if);
   ibc_build_merge(b, IBC_MERGE_OP_MERGE, &preds);

   return _if;
}

static inline ibc_branch_instr *
ibc_else(ibc_builder *b, ibc_branch_instr *_if)
{
   ibc_branch_instr *_else =
      ibc_build_branch(b, IBC_BRANCH_OP_ELSE, ibc_null(IBC_TYPE_UD),
                       BRW_PREDICATE_NONE, false);

   /* The then block has to start with a merge instruction */
   struct list_head preds;
   list_inithead(&preds);
   ibc_builder_add_merge_pred(b, &preds, true, _if);
   ibc_builder_add_merge_pred(b, &preds, false, _else);
   ibc_build_merge(b, IBC_MERGE_OP_MERGE, &preds);

   return _else;
}

static inline ibc_merge_instr *
ibc_endif(ibc_builder *b, ibc_branch_instr *_if, ibc_branch_instr *_else)
{
   ibc_branch_instr *branch =
      ibc_build_branch(b, IBC_BRANCH_OP_NEXT, ibc_null(IBC_TYPE_UD),
                       BRW_PREDICATE_NONE, false);

   /* The then block has to start with a merge instruction */
   struct list_head preds;
   list_inithead(&preds);
   if (_else == NULL) {
      ibc_builder_add_merge_pred(b, &preds, true, _if);
   } else {
      /* The ELSE instruction actually ends the then block */
      ibc_builder_add_merge_pred(b, &preds, true, _else);
   }
   ibc_builder_add_merge_pred(b, &preds, true, branch);
   ibc_merge_instr *_endif = ibc_build_merge(b, IBC_MERGE_OP_ENDIF, &preds);

   _if->merge = _endif;
   if (_else) {
      ibc_merge_instr *after_else =
         ibc_instr_as_merge(ibc_instr_next(&_else->instr));
      _if->jump = after_else;
      _else->jump = _endif;
      _else->merge = _endif;
   } else {
      _if->jump = _endif;
   }

   return _endif;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_BUILDER_H */
