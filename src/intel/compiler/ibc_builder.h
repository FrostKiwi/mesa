/*
 * Copyright © 2019 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, flow, publish, distribute, sublicense,
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

   ibc_flow_instr *block_start;

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
   assert(util_is_power_of_two_nonzero(simd_width));
   assert(simd_group % simd_width == 0);

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
ibc_builder_push_instr_group(ibc_builder *b, const ibc_instr *instr)
{
   if (instr->we_all) {
      assert(instr->simd_group == 0);
      ibc_builder_push_we_all(b, instr->simd_width);
   } else {
      ibc_builder_push_group(b, instr->simd_group, instr->simd_width);
   }
}

static inline void
ibc_builder_insert_instr(ibc_builder *b, ibc_instr *instr)
{
   ibc_instr_insert(instr, b->cursor);
   b->cursor = ibc_after_instr(instr);
}

static inline ibc_ref
ibc_hw_grf_ref(uint8_t nr, uint8_t subnr, enum ibc_type type)
{
   return (ibc_ref) {
      .file = IBC_FILE_HW_GRF,
      .type = IBC_TYPE_UD,
      .hw_grf = {
         .byte = (uint16_t)nr * REG_SIZE + subnr * ibc_type_byte_size(type),
         .vstride = 8 * ibc_type_byte_size(type),
         .width = 8,
         .hstride = ibc_type_byte_size(type),
      },
   };
}

static inline ibc_ref
ibc_flag_ref(uint8_t bit)
{
   return (ibc_ref) {
      .file = IBC_FILE_FLAG,
      .type = IBC_TYPE_FLAG,
      .flag = {
         .bit = bit,
      },
   };
}

static inline ibc_ref
ibc_typed_ref(const ibc_reg *reg, enum ibc_type type)
{
   ibc_ref ref = {
      .file = reg->file,
      .reg = reg,
      .type = type,
   };
   if (ibc_type_bit_size(type) == 0) {
      switch (reg->file) {
      case IBC_FILE_LOGICAL:
         ref.type |= reg->logical.bit_size;
         break;
      case IBC_FILE_FLAG:
         ref.type = IBC_TYPE_FLAG;
         break;
      default:
         unreachable("Unsupported register file for automatic types");
      }
   }
   switch (reg->file) {
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      unreachable("Unsupported register file");

   case IBC_FILE_LOGICAL:
      ref.logical = (struct ibc_ref_logical) { 0, };
      return ref;

   case IBC_FILE_HW_GRF:
      ref.hw_grf = (struct ibc_ref_hw_grf) {
         .vstride = 8 * ibc_type_byte_size(ref.type),
         .width = 8,
         .hstride = ibc_type_byte_size(ref.type),
      };
      return ref;

   case IBC_FILE_FLAG:
      assert(ibc_type_base_type(ref.type) != IBC_TYPE_FLOAT);
      assert(ibc_type_bit_size(ref.type) == 1 ||
             ibc_type_bit_size(ref.type) == 16 ||
             ibc_type_bit_size(ref.type) == 32);
      return ref;

   case IBC_FILE_ACCUM:
      assert(ref.type == reg->accum.type);
      return ref;
   }

   unreachable("Unknown register file");
}

static inline ibc_ref
ibc_iref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_INT);
}

static inline ibc_ref
ibc_uref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_UINT);
}

static inline ibc_ref
ibc_fref(const ibc_reg *reg)
{
   return ibc_typed_ref(reg, IBC_TYPE_FLOAT);
}

static inline ibc_ref
ibc_null(enum ibc_type type)
{
   return (ibc_ref) {
      .file = IBC_FILE_NONE,
      .type = type,
   };
}

static inline ibc_ref
ibc_comp_ref(ibc_ref ref, unsigned comp)
{
   if (comp == 0)
      return ref;

   assert(ref.file == IBC_FILE_LOGICAL);
   ref.logical.comp += comp;
   return ref;
}

static inline ibc_ref
ibc_imm_ref(enum ibc_type type, char *imm, unsigned imm_size)
{
   ibc_ref ref = {
      .file = IBC_FILE_IMM,
      .type = type,
   };
   memcpy(ref.imm, imm, imm_size);
   return ref;
}

static inline ibc_ref
ibc_imm_zero(enum ibc_type type)
{
   return (ibc_ref) {
      .file = IBC_FILE_IMM,
      .type = type,
   };
}

#define IBC_BUILDER_DEFINE_IMM_HELPER(tp, TP, c_type)          \
static inline ibc_ref                                          \
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

static inline ibc_ref
ibc_imm_hf(float x)
{
   uint16_t hf = _mesa_float_to_half(x);
   return ibc_imm_ref(IBC_TYPE_HF, (char *)&hf, sizeof(hf));
}

static inline ibc_ref
ibc_builder_new_logical_reg(ibc_builder *b, enum ibc_type type,
                            uint8_t num_comps)
{
   ibc_reg *reg = ibc_logical_reg_create(b->shader,
                                         ibc_type_bit_size(type), num_comps,
                                         b->simd_group, b->simd_width);
   return ibc_typed_ref(reg, type);
}

static inline ibc_alu_instr *
ibc_build_alu(ibc_builder *b, enum ibc_alu_op op, ibc_ref dest,
              ibc_ref flag, enum brw_conditional_mod cmod,
              ibc_ref *src, unsigned num_srcs)
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
ibc_build_alu1(ibc_builder *b, enum ibc_alu_op op, ibc_ref dest,
               ibc_ref src0)
{
   ibc_ref srcs[] = { src0 };
   return ibc_build_alu(b, op, dest, ibc_null(IBC_TYPE_FLAG),
                        BRW_CONDITIONAL_NONE, srcs, 1);
}

static inline ibc_alu_instr *
ibc_build_alu2(ibc_builder *b, enum ibc_alu_op op, ibc_ref dest,
               ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[] = { src0, src1 };
   return ibc_build_alu(b, op, dest, ibc_null(IBC_TYPE_FLAG),
                        BRW_CONDITIONAL_NONE, srcs, 2);
}

static inline enum ibc_type
_ibc_builder_dest_type(enum ibc_type dest_type,
                       ibc_ref *src, unsigned num_srcs)
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
ibc_build_alu2_cmod(ibc_builder *b, enum ibc_alu_op op, ibc_ref dest,
                    enum brw_conditional_mod cmod,
                    ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[] = { src0, src1 };
   if (dest.type == IBC_TYPE_FLAG) {
      assert(op != IBC_ALU_OP_SEL);
      enum ibc_type dest_type = _ibc_builder_dest_type(src0.type, srcs,
                                                       ARRAY_SIZE(srcs));
      return ibc_build_alu(b, op, ibc_null(dest_type), dest, cmod, srcs, 2);
   } else if (cmod != BRW_CONDITIONAL_NONE) {
      /* We need a flag register even though the result may never be used.
       * This is not true for SEL, however.
       */
      ibc_ref flag = op == IBC_ALU_OP_SEL ? ibc_null(IBC_TYPE_FLAG) :
                     ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);

      return ibc_build_alu(b, op, dest, flag, cmod, srcs, ARRAY_SIZE(srcs));
   } else {
      return ibc_build_alu2(b, op, dest, src0, src1);
   }
}

static inline ibc_ref
ibc_build_ssa_alu(ibc_builder *b, enum ibc_alu_op op, enum ibc_type dest_type,
                  ibc_ref *src, unsigned num_srcs)
{
   dest_type = _ibc_builder_dest_type(dest_type, src, num_srcs);
   ibc_ref dest_ref = ibc_builder_new_logical_reg(b, dest_type, 1);

   ibc_build_alu(b, op, dest_ref, ibc_null(IBC_TYPE_FLAG),
                 BRW_CONDITIONAL_NONE, src, num_srcs);

   return dest_ref;
}

static inline ibc_ref
ibc_build_ssa_flag_alu(ibc_builder *b, enum ibc_alu_op op,
                       enum ibc_type dest_type,
                       enum brw_conditional_mod cmod,
                       ibc_ref *src, unsigned num_srcs)
{
   ibc_ref flag = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);

   dest_type = _ibc_builder_dest_type(dest_type, src, num_srcs);
   ibc_build_alu(b, op, ibc_null(dest_type), flag, cmod, src, num_srcs);

   return flag;
}

#define IBC_BUILDER_DEFINE_ALU1(OP)                                  \
static inline ibc_ref                                                \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_ref src0)                                               \
{                                                                    \
   ibc_ref srcs[] = { src0 };                                        \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 1); \
}

#define IBC_BUILDER_DEFINE_ALU2(OP)                                  \
static inline ibc_ref                                                \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_ref src0, ibc_ref src1)                                 \
{                                                                    \
   ibc_ref srcs[] = { src0, src1 };                                  \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 2); \
}

#define IBC_BUILDER_DEFINE_ALU3(OP)                                  \
static inline ibc_ref                                                \
ibc_##OP(ibc_builder *b, enum ibc_type dest_type,                    \
         ibc_ref src0, ibc_ref src1, ibc_ref src2)                   \
{                                                                    \
   ibc_ref srcs[] = { src0, src1, src2 };                            \
   return ibc_build_ssa_alu(b, IBC_ALU_OP_##OP, dest_type, srcs, 3); \
}

IBC_BUILDER_DEFINE_ALU1(MOV)
IBC_BUILDER_DEFINE_ALU1(NOT)
IBC_BUILDER_DEFINE_ALU2(AND)
IBC_BUILDER_DEFINE_ALU2(OR)
IBC_BUILDER_DEFINE_ALU2(XOR)
IBC_BUILDER_DEFINE_ALU2(SHR)
IBC_BUILDER_DEFINE_ALU2(SHL)
IBC_BUILDER_DEFINE_ALU1(BFREV)
IBC_BUILDER_DEFINE_ALU3(BFE)
IBC_BUILDER_DEFINE_ALU2(BFI1)
IBC_BUILDER_DEFINE_ALU3(BFI2)
IBC_BUILDER_DEFINE_ALU2(ADD)
IBC_BUILDER_DEFINE_ALU2(MUL)
IBC_BUILDER_DEFINE_ALU1(FRC)
IBC_BUILDER_DEFINE_ALU1(RNDU)
IBC_BUILDER_DEFINE_ALU1(RNDD)
IBC_BUILDER_DEFINE_ALU1(RNDE)
IBC_BUILDER_DEFINE_ALU1(RNDZ)
IBC_BUILDER_DEFINE_ALU1(FBH)
IBC_BUILDER_DEFINE_ALU1(FBL)
IBC_BUILDER_DEFINE_ALU1(CBIT)
IBC_BUILDER_DEFINE_ALU3(MAD)
IBC_BUILDER_DEFINE_ALU3(LRP)

IBC_BUILDER_DEFINE_ALU1(RCP)
IBC_BUILDER_DEFINE_ALU1(LOG2)
IBC_BUILDER_DEFINE_ALU1(EXP2)
IBC_BUILDER_DEFINE_ALU1(SQRT)
IBC_BUILDER_DEFINE_ALU1(RSQ)
IBC_BUILDER_DEFINE_ALU1(SIN)
IBC_BUILDER_DEFINE_ALU1(COS)
IBC_BUILDER_DEFINE_ALU2(POW)
IBC_BUILDER_DEFINE_ALU2(IDIV)
IBC_BUILDER_DEFINE_ALU2(IREM)

#undef IBC_BUILDER_DEFINE_ALU1
#undef IBC_BUILDER_DEFINE_ALU2
#undef IBC_BUILDER_DEFINE_ALU3

static inline ibc_alu_instr *
ibc_MOV_to(ibc_builder *b, ibc_ref dest, ibc_ref src)
{
   return ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
}

static inline ibc_ref
ibc_MOV_scalar(ibc_builder *b, enum ibc_type dest_type, ibc_ref src)
{
   ibc_builder_push_scalar(b);
   ibc_ref dest = ibc_MOV(b, dest_type, src);
   ibc_builder_pop(b);
   return dest;
}

static inline ibc_alu_instr *
ibc_MOV_to_flag(ibc_builder *b, ibc_ref flag,
                enum brw_conditional_mod cmod, ibc_ref src)
{
   assert(flag.type == IBC_TYPE_FLAG);
   assert(ibc_type_base_type(src.type) != IBC_TYPE_FLOAT);
   return ibc_build_alu(b, IBC_ALU_OP_MOV, ibc_null(IBC_TYPE_W),
                        flag, cmod, &src, 1);
}

static inline ibc_ref
ibc_MOV_from_flag(ibc_builder *b, enum ibc_type dest_type,
                  enum ibc_predicate predicate,
                  ibc_ref flag)
{
   assert(ibc_type_base_type(dest_type) == IBC_TYPE_UINT ||
          ibc_type_base_type(dest_type) == IBC_TYPE_INT);
   /* Make it signed */
   dest_type = IBC_TYPE_INT | ibc_type_bit_size(dest_type);

   ibc_ref dest = ibc_builder_new_logical_reg(b, dest_type, 1);
   ibc_MOV_to(b, dest, ibc_imm_w(0));
   ibc_alu_instr *mov = ibc_MOV_to(b, dest, ibc_imm_w(-1));
   ibc_instr_set_predicate(&mov->instr, flag, predicate);

   return dest;
}

static inline void
ibc_MOV_raw_vec_to(ibc_builder *b, ibc_ref dest,
                   ibc_ref src, unsigned num_comps)
{
   assert(ibc_type_bit_size(src.type) == ibc_type_bit_size(dest.type));

   if (ibc_type_base_type(src.type) == IBC_TYPE_INVALID)
      src.type |= IBC_TYPE_UINT;
   dest.type = src.type;

   /* TODO: This needs to be adjusted more carefully */
   const unsigned simd_width = MIN2(b->simd_width, 16);
   if (num_comps > 1 || simd_width != b->simd_width) {
      assert(src.file == IBC_FILE_IMM ||
             src.file == IBC_FILE_LOGICAL);
      assert(dest.file == IBC_FILE_LOGICAL);
   }

   for (unsigned i = 0; i < num_comps; i++) {
      for (unsigned g = 0; g < b->simd_width; g += simd_width) {
         ibc_builder_push_group(b, g, MIN2(b->simd_width, 16));
         ibc_MOV_to(b, dest, src);
         ibc_builder_pop(b);
      }
      if (src.file == IBC_FILE_LOGICAL) {
         src.logical.comp++;
         dest.logical.comp++;
      }
   }
}

static inline ibc_ref
ibc_MOV_raw(ibc_builder *b, ibc_ref src)
{
   ibc_ref dest = ibc_builder_new_logical_reg(b, src.type, 1);
   ibc_MOV_raw_vec_to(b, dest, src, 1);
   return dest;
}

static inline ibc_ref
ibc_restride(ibc_builder *b, ibc_ref src, enum ibc_type type, unsigned offset,
             unsigned vstride, unsigned width, unsigned hstride)
{
   /* To make things easy on the caller, we take offset, vstride and hstride
    * in units of the type size.
    */
   offset *= ibc_type_byte_size(type);
   vstride *= ibc_type_byte_size(type);
   hstride *= ibc_type_byte_size(type);

   ibc_ref ref = src;
   if (src.file != IBC_FILE_HW_GRF) {
      unsigned grf_size = offset;
      if (b->simd_width >= width) {
         assert(b->simd_width % width == 0);
         grf_size += vstride * ((b->simd_width / width) - 1);
      }
      grf_size += hstride * (width - 1);
      grf_size += ibc_type_byte_size(type);
      grf_size = util_next_power_of_two(grf_size);

      ibc_reg *grf = ibc_hw_grf_reg_create(b->shader, grf_size,
                                           MIN2(grf_size, REG_SIZE));
      ref = ibc_typed_ref(grf, src.type);

      ibc_MOV_to(b, ref, src);
   }

   ref.type = type;
   ref.hw_grf.byte += offset;
   ref.hw_grf.vstride = vstride;
   ref.hw_grf.width = width;
   ref.hw_grf.hstride = hstride;
   return ref;
}

static inline ibc_ref
ibc_SEL(ibc_builder *b, enum ibc_type dest_type,
        ibc_ref flag, ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[] = { src0, src1 };
   ibc_ref dest = ibc_build_ssa_alu(b, IBC_ALU_OP_SEL, dest_type, srcs, 2);
   ibc_alu_instr *sel = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   ibc_instr_set_predicate(&sel->instr, flag, IBC_PREDICATE_NORMAL);
   return dest;
}

static inline ibc_ref
ibc_NEG(ibc_builder *b, enum ibc_type dest_type, ibc_ref src)
{
   ibc_ref dest = ibc_MOV(b, dest_type, src);
   ibc_alu_instr *mov = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   mov->src[0].mod = IBC_ALU_SRC_MOD_NEG;
   return dest;
}

static inline ibc_ref
ibc_ABS(ibc_builder *b, enum ibc_type dest_type, ibc_ref src)
{
   ibc_ref dest = ibc_MOV(b, dest_type, src);
   ibc_alu_instr *mov = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   mov->src[0].mod = IBC_ALU_SRC_MOD_ABS;
   return dest;
}

static inline ibc_ref
ibc_MIN(ibc_builder *b, enum ibc_type dest_type, ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[] = { src0, src1 };
   ibc_ref dest = ibc_build_ssa_alu(b, IBC_ALU_OP_SEL, dest_type, srcs, 2);
   ibc_alu_instr *sel = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   sel->cmod = BRW_CONDITIONAL_L;
   return dest;
}

static inline ibc_ref
ibc_MAX(ibc_builder *b, enum ibc_type dest_type, ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[] = { src0, src1 };
   ibc_ref dest = ibc_build_ssa_alu(b, IBC_ALU_OP_SEL, dest_type, srcs, 2);
   ibc_alu_instr *sel = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
   sel->cmod = BRW_CONDITIONAL_GE;
   return dest;
}

static inline ibc_ref
ibc_CMP(ibc_builder *b, enum ibc_type dest_type,
        enum brw_conditional_mod cmod, ibc_ref src0, ibc_ref src1)
{
   ibc_ref dest = ibc_builder_new_logical_reg(b, dest_type, 1);
   ibc_build_alu2_cmod(b, IBC_ALU_OP_CMP, dest, cmod, src0, src1);
   return dest;
}

static inline ibc_intrinsic_instr *
ibc_build_intrinsic(ibc_builder *b, enum ibc_intrinsic_op op,
                    ibc_ref dest, int num_dest_bytes, int num_dest_comps,
                    const ibc_intrinsic_src *srcs, unsigned num_srcs)
{
   ibc_intrinsic_instr *intrin =
      ibc_intrinsic_instr_create(b->shader, op, b->simd_group, b->simd_width,
                                 num_srcs);
   intrin->instr.we_all = b->we_all;

   for (unsigned i = 0; i < num_srcs; i++) {
      intrin->src[i].ref = srcs[i].ref;
      if (srcs[i].simd_width > 0) {
         intrin->src[i].simd_group = srcs[i].simd_group;
         intrin->src[i].simd_width = srcs[i].simd_width;
      }
      intrin->src[i].num_comps =
         srcs[i].num_comps > 0 ? srcs[i].num_comps :
         srcs[i].ref.file == IBC_FILE_NONE ? 0 : num_dest_comps;
   }

   intrin->dest = dest;
   intrin->num_dest_bytes = num_dest_bytes;
   intrin->num_dest_comps = num_dest_comps;
   ibc_builder_insert_instr(b, &intrin->instr);

   return intrin;
}

static inline ibc_ref
ibc_build_ssa_intrinsic(ibc_builder *b, enum ibc_intrinsic_op op,
                        enum ibc_type dest_type, unsigned num_dest_comps,
                        const ibc_intrinsic_src *srcs, unsigned num_srcs)
{
   ibc_ref dest =
      ibc_builder_new_logical_reg(b, dest_type, num_dest_comps);

   ibc_build_intrinsic(b, op, dest, -1, num_dest_comps, srcs, num_srcs);

   return dest;
}

static inline ibc_ref
ibc_UNDEF(ibc_builder *b, enum ibc_type type, unsigned num_comps)
{
   return ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_UNDEF,
                                 type, num_comps, NULL, 0);
}

static inline ibc_ref
ibc_FIND_LIVE_CHANNEL(ibc_builder *b)
{
   ibc_builder_push_scalar(b);
   ibc_ref dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, 1);
   ibc_intrinsic_instr *intrin =
      ibc_build_intrinsic(b, IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL,
                          dest, -1, 1, NULL, 0);
   intrin->can_reorder = false;
   ibc_builder_pop(b);

   return dest;
}

static inline ibc_ref
ibc_SIMD_SHUFFLE(ibc_builder *b, ibc_ref val,
                 uint8_t val_simd_group, uint8_t val_simd_width,
                 ibc_ref chan)
{
   if (ibc_ref_read_is_uniform(val))
      return ibc_MOV(b, val.type, val);

   if (chan.file == IBC_FILE_IMM) {
      if (val.file == IBC_FILE_LOGICAL) {
         assert(!val.logical.broadcast);
         val.logical.broadcast = true;
         val.logical.simd_channel = ibc_ref_as_uint(chan);
      } else {
         assert(val.file == IBC_FILE_HW_GRF);
         assert(val.hw_grf.vstride == val.hw_grf.width * val.hw_grf.hstride);
         val.hw_grf.byte += val.hw_grf.hstride * ibc_ref_as_uint(chan);
         ibc_hw_grf_mul_stride(&val.hw_grf, 0);
      }
      return ibc_MOV(b, val.type, val);
   }

   ibc_intrinsic_src srcs[2] = {
      {
         .ref = val,
         .simd_group = val_simd_group,
         .simd_width = val_simd_width,
      },
      {
         .ref = chan,
      },
   };

   ibc_ref dest =
      ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_SIMD_SHUFFLE,
                              val.type, 1, srcs, 2);

   return dest;
}

static inline ibc_ref
ibc_uniformize(ibc_builder *b, ibc_ref val)
{
   if (ibc_ref_read_is_uniform(val)) {
      if (ibc_ref_read_is_static(val))
         return val;
      else
         return ibc_MOV_scalar(b, val.type, val);
   }

   uint8_t val_simd_group = b->simd_group;
   uint8_t val_simd_width = b->simd_width;

   ibc_builder_push_scalar(b);
   ibc_ref dest = ibc_SIMD_SHUFFLE(b, val, val_simd_group, val_simd_width,
                                   ibc_FIND_LIVE_CHANNEL(b));
   ibc_builder_pop(b);

   return dest;
}

static inline ibc_ref
ibc_SIMD_ZIP(ibc_builder *b, ibc_ref *srcs, unsigned num_srcs,
             unsigned num_comps)
{
   assert(num_srcs > 1 && b->simd_width % num_srcs == 0);
   const unsigned src_width = b->simd_width / num_srcs;

   ibc_intrinsic_src zip_srcs[8];
   assert(num_srcs <= ARRAY_SIZE(zip_srcs));

   for (unsigned i = 0; i < num_srcs; i++) {
      assert(srcs[i].type == srcs[0].type);
      zip_srcs[i] = (ibc_intrinsic_src) {
         .ref = srcs[i],
         .simd_group = b->simd_group + src_width * i,
         .simd_width = src_width,
      };
      zip_srcs[i].ref.type = ibc_type_bit_size(srcs[0].type);
   }

   ibc_ref dest = ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_SIMD_ZIP,
                                          zip_srcs[0].ref.type, num_comps,
                                          zip_srcs, num_srcs);
   dest.type = srcs[0].type;
   return dest;
}

static inline ibc_ref
ibc_SIMD_ZIP2(ibc_builder *b, ibc_ref src0, ibc_ref src1,
              unsigned num_comps)
{
   ibc_ref srcs[2] = { src0, src1 };
   return ibc_SIMD_ZIP(b, srcs, 2, num_comps);
}

static inline ibc_ref
ibc_PACK(ibc_builder *b, ibc_ref *srcs, unsigned num_srcs, unsigned num_comps)
{
   assert(num_srcs > 1);
   unsigned dest_bits = ibc_type_bit_size(srcs[0].type) * num_srcs;
   assert(dest_bits <= 64);

   ibc_intrinsic_src pack_srcs[8];
   assert(num_srcs <= ARRAY_SIZE(pack_srcs));

   for (unsigned i = 0; i < num_srcs; i++) {
      assert(srcs[i].type == srcs[0].type);
      pack_srcs[i] = (ibc_intrinsic_src) {
         .ref = srcs[i],
         .num_comps = num_comps,
      };
      pack_srcs[i].ref.type = ibc_type_bit_size(srcs[0].type);
   }

   return ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_PACK, dest_bits,
                                  num_comps, pack_srcs, num_srcs);
}

static inline ibc_ref
ibc_PACK2(ibc_builder *b, ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[2] = { src0, src1 };
   return ibc_PACK(b, srcs, 2, 1);
}

static inline ibc_ref
ibc_UNPACK(ibc_builder *b, enum ibc_type dest_type,
           ibc_ref src, unsigned elem)
{
   assert(src.file == IBC_FILE_LOGICAL);
   src.type = dest_type;
   src.logical.byte += elem * ibc_type_byte_size(dest_type);
   return ibc_MOV(b, dest_type, src);
}

static inline void
ibc_VEC_to(ibc_builder *b, ibc_ref dest, ibc_ref *srcs, unsigned num_comps)
{
   assert(num_comps > 0);
   if (num_comps == 1) {
      ibc_MOV_raw_vec_to(b, dest, srcs[0], 1);
      return;
   }

   ibc_intrinsic_src vec_srcs[8];
   assert(num_comps <= ARRAY_SIZE(vec_srcs));

   for (unsigned i = 0; i < num_comps; i++) {
      assert(srcs[i].type == dest.type);
      vec_srcs[i] = (ibc_intrinsic_src) {
         .ref = srcs[i],
         .num_comps = 1,
      };
      vec_srcs[i].ref.type = ibc_type_bit_size(srcs[0].type);
   }
   dest.type = ibc_type_bit_size(dest.type);

   ibc_build_intrinsic(b, IBC_INTRINSIC_OP_VEC, dest,
                       -1, num_comps, vec_srcs, num_comps);
}

static inline ibc_ref
ibc_VEC(ibc_builder *b, ibc_ref *srcs, unsigned num_comps)
{
   ibc_ref dest = ibc_builder_new_logical_reg(b, srcs[0].type, num_comps);
   ibc_VEC_to(b, dest, srcs, num_comps);
   return dest;
}

static inline ibc_ref
ibc_VEC2(ibc_builder *b, ibc_ref src0, ibc_ref src1)
{
   ibc_ref srcs[2] = { src0, src1 };
   return ibc_VEC(b, srcs, 2);
}

static inline ibc_ref
ibc_MESSAGE(ibc_builder *b, const ibc_intrinsic_src *src_in,
            unsigned num_srcs, unsigned *mlen_out)
{
   ibc_intrinsic_src src[11];
   assert(num_srcs <= ARRAY_SIZE(src));

   unsigned num_bytes = 0;
   for (unsigned i = 0; i < num_srcs; i++) {
      src[i] = src_in[i];

      if (src[i].simd_width == 0) {
         src[i].simd_group = b->simd_group;
         src[i].simd_width = b->simd_width;
      }

      if (src[i].num_comps == 0)
         src[i].num_comps = 1;

      assert(src[i].simd_width == b->simd_width || src[i].simd_width == 1);
      assert(src[i].simd_width == 1 || src[i].num_comps == 1);
      unsigned src_bytes = ibc_type_byte_size(src[i].ref.type) *
                           src[i].simd_width * src[i].num_comps;
      num_bytes += ALIGN(src_bytes, REG_SIZE);
   }

   ibc_reg *grf = ibc_hw_grf_reg_create(b->shader, num_bytes, REG_SIZE);

   ibc_build_intrinsic(b, IBC_INTRINSIC_OP_MESSAGE,
                       ibc_typed_ref(grf, IBC_TYPE_32_BIT),
                       num_bytes, -1, src, num_srcs);

   assert(num_bytes % REG_SIZE == 0);
   if (mlen_out)
      *mlen_out = num_bytes / REG_SIZE;

   return ibc_typed_ref(grf, IBC_TYPE_32_BIT);
}

static inline ibc_ref
ibc_PLN(ibc_builder *b, ibc_ref vert, ibc_ref bary)
{
   assert(vert.file == IBC_FILE_LOGICAL);
   assert(vert.reg->logical.simd_width == 1);

   ibc_ref comps[2];
   for (unsigned g = 0; g < b->simd_width; g += 16) {
      assert(b->simd_width >= 8);
      ibc_builder_push_group(b, g, MIN2(16, b->simd_width));

      ibc_intrinsic_src pln_srcs[3];
      pln_srcs[0] = (ibc_intrinsic_src) {
         .ref = vert,
         .num_comps = 4,
      };
      for (unsigned i = 0; i < (b->simd_width / 8); i++) {
         pln_srcs[1 + i] = (ibc_intrinsic_src) {
            .ref = bary,
            .simd_group = b->simd_group + i * 8,
            .simd_width = 8,
            .num_comps = 2,
         };
      }

      comps[g / 16] = ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_PLN,
                                              IBC_TYPE_F, 1, pln_srcs,
                                              1 + (b->simd_width / 8));
      ibc_builder_pop(b);
   }

   if (b->simd_width <= 16) {
      return comps[0];
   } else {
      assert(b->simd_width % 16 == 0);
      return ibc_SIMD_ZIP(b, comps, b->simd_width / 16, 1);
   }
}

static inline void
ibc_WAIT(ibc_builder *b)
{
   ibc_builder_push_scalar(b);
   ibc_intrinsic_instr *wait =
      ibc_build_intrinsic(b, IBC_INTRINSIC_OP_WAIT,
                          ibc_null(IBC_TYPE_UD), 0, 0, NULL, 0);
   wait->can_reorder = false;
   wait->has_side_effects = true;
   ibc_builder_pop(b);
}

static inline void
ibc_build_alu_scan(ibc_builder *b, enum ibc_alu_op op, ibc_ref tmp,
                   enum brw_conditional_mod cmod,
                   unsigned final_cluster_size)
{
   assert(b->simd_width >= 8);
   const uint8_t scan_simd_width = b->simd_width;
   assert(tmp.file == IBC_FILE_HW_GRF);
   assert(tmp.hw_grf.hstride == ibc_type_byte_size(tmp.type));
   assert(tmp.hw_grf.hstride * tmp.hw_grf.width == tmp.hw_grf.vstride);
   assert(util_is_power_of_two_nonzero(final_cluster_size));

   if (final_cluster_size >= 2) {
      ibc_builder_push_we_all(b, b->simd_width / 2);

      ibc_ref left = tmp;
      left.hw_grf.vstride *= 2;
      left.hw_grf.hstride *= 2;

      ibc_ref right = tmp;
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

      ibc_ref left = tmp;
      left.hw_grf.byte += 1 * tmp.hw_grf.hstride;
      left.hw_grf.vstride *= 4;
      left.hw_grf.hstride *= 4;

      ibc_ref right = tmp;
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
         ibc_ref left = tmp;
         left.hw_grf.byte += (g + half_size - 1) * tmp.hw_grf.hstride;
         left.hw_grf.vstride = 0;
         left.hw_grf.width = 1;
         left.hw_grf.hstride = 0;

         ibc_ref right = tmp;
         right.hw_grf.byte += (g + half_size) * tmp.hw_grf.hstride;

         ibc_build_alu2_cmod(b, op, right, cmod, left, right);
      }

      ibc_builder_pop(b);
   }
}

static inline ibc_ref
ibc_cluster_broadcast(ibc_builder *b, enum ibc_type dest_type,
                      ibc_ref src, unsigned cluster_size)
{
   if (cluster_size == 0 || cluster_size == b->simd_width) {
      ibc_ref scalar_src = src;
      scalar_src.hw_grf = (struct ibc_ref_hw_grf) {
         .byte = (b->simd_width - 1) * ibc_type_byte_size(src.type),
         .vstride = 0,
         .width = 1,
         .hstride = 0,
      };
      ibc_builder_push_scalar(b);
      ibc_ref dest = ibc_MOV(b, dest_type, scalar_src);
      ibc_builder_pop(b);
      return dest;
   } else if (cluster_size * ibc_type_byte_size(src.type) >= REG_SIZE * 2) {
      /* In this case, the distance between clusters is at least 2 GRFs so we
       * don't need to do any weird striding.
       */
      ibc_ref dest = ibc_builder_new_logical_reg(b, src.type, 1);
      const unsigned num_clusters = b->simd_width / cluster_size;
      for (unsigned cluster = 0; cluster < num_clusters; cluster++) {
         ibc_builder_push_group(b, cluster * cluster_size,  cluster_size);

         const unsigned comp = cluster * cluster_size +
                               (cluster_size - 1);
         ibc_ref comp_src = src;
         ibc_hw_grf_simd_slice(&comp_src.hw_grf, comp);
         ibc_hw_grf_mul_stride(&comp_src.hw_grf, 0);
         ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, comp_src);

         ibc_builder_pop(b);
      }
      return dest;
   } else {
      ibc_ref strided_src = src;
      strided_src.hw_grf = (struct ibc_ref_hw_grf) {
         .byte = (cluster_size - 1) * ibc_type_byte_size(src.type),
         .vstride = cluster_size * ibc_type_byte_size(src.type),
         .width = cluster_size,
         .hstride = 0,
      };
      return ibc_MOV(b, dest_type, strided_src);
   }
}

static inline ibc_flow_instr *
ibc_build_flow(ibc_builder *b, enum ibc_flow_op op, ibc_ref pred,
               enum ibc_predicate predicate)
{
   assert(b->simd_group == 0);
   ibc_flow_instr *flow =
      ibc_flow_instr_create(b->shader, op, b->simd_width);

   if (predicate != IBC_PREDICATE_NONE) {
      flow->instr.flag = pred;
      flow->instr.predicate = predicate;
   }

   /* If a flow instruction falls through, it is its own predecessor */
   if (ibc_flow_instr_falls_through(flow))
      ibc_flow_instr_add_pred(flow, flow);

   ibc_builder_insert_instr(b, &flow->instr);

   if (b->block_start == NULL) {
      assert(op == IBC_FLOW_OP_START);
      assert(list_is_empty(&b->shader->flow_instrs));
      list_addtail(&flow->flow_link, &b->shader->flow_instrs);
   } else {
      assert(op != IBC_FLOW_OP_START);
      list_add(&flow->flow_link, &b->block_start->flow_link);
   }
   b->block_start = flow;

   return flow;
}

static inline ibc_flow_instr *
ibc_START(ibc_builder *b)
{
   return ibc_build_flow(b, IBC_FLOW_OP_START, ibc_null(IBC_TYPE_FLAG),
                            IBC_PREDICATE_NONE);
}

static inline ibc_flow_instr *
ibc_END(ibc_builder *b)
{
   return ibc_build_flow(b, IBC_FLOW_OP_END, ibc_null(IBC_TYPE_FLAG),
                            IBC_PREDICATE_NONE);
}

static inline ibc_flow_instr *
ibc_IF(ibc_builder *b, ibc_ref pred,
       enum ibc_predicate predicate)
{
   return ibc_build_flow(b, IBC_FLOW_OP_IF, pred, predicate);
}

static inline ibc_flow_instr *
ibc_ELSE(ibc_builder *b, ibc_flow_instr *_if)
{
   ibc_flow_instr *_else =
      ibc_build_flow(b, IBC_FLOW_OP_ELSE, ibc_null(IBC_TYPE_FLAG),
                        IBC_PREDICATE_NONE);

   assert(_if->op == IBC_FLOW_OP_IF);
   ibc_flow_instr_set_jump(_if, _else);

   return _else;
}

static inline ibc_flow_instr *
ibc_ENDIF(ibc_builder *b, ibc_flow_instr *_if, ibc_flow_instr *_else)
{
   ibc_flow_instr *_endif =
      ibc_build_flow(b, IBC_FLOW_OP_ENDIF, ibc_null(IBC_TYPE_FLAG),
                        IBC_PREDICATE_NONE);

   assert(_if->op == IBC_FLOW_OP_IF);
   if (_else == NULL) {
      ibc_flow_instr_set_jump(_if, _endif);
   } else {
      assert(_else->op == IBC_FLOW_OP_ELSE);
      ibc_flow_instr_set_jump(_else, _endif);
      _else->merge = _endif;
   }
   _if->merge = _endif;

   return _endif;
}

static inline ibc_flow_instr *
ibc_DO(ibc_builder *b)
{
   return ibc_build_flow(b, IBC_FLOW_OP_DO, ibc_null(IBC_TYPE_FLAG),
                            IBC_PREDICATE_NONE);
}

static inline ibc_flow_instr *
ibc_BREAK(ibc_builder *b, ibc_ref pred,
          enum ibc_predicate predicate,
          struct list_head *breaks)
{
   ibc_flow_instr *_break =
      ibc_build_flow(b, IBC_FLOW_OP_BREAK, pred, predicate);

   list_addtail(&_break->builder_link, breaks);

   return _break;
}

static inline ibc_flow_instr *
ibc_CONT(ibc_builder *b, ibc_ref pred,
         enum ibc_predicate predicate,
         ibc_flow_instr *_do)

{
   ibc_flow_instr *_cont =
      ibc_build_flow(b, IBC_FLOW_OP_CONT, pred, predicate);

   assert(_do->op == IBC_FLOW_OP_DO);
   ibc_flow_instr_set_jump(_cont, _do);

   return _cont;
}

static inline ibc_flow_instr *
ibc_WHILE(ibc_builder *b, ibc_ref pred,
          enum ibc_predicate predicate,
          ibc_flow_instr *_do, struct list_head *breaks)
{
   ibc_flow_instr *_while =
      ibc_build_flow(b, IBC_FLOW_OP_WHILE, pred, predicate);

   assert(_do->op == IBC_FLOW_OP_DO);
   ibc_flow_instr_set_jump(_while, _do);
   _do->merge = _while;

   /* Link up all the jumps for continus */
   ibc_foreach_flow_pred(pred, _do) {
      if (pred->instr == _while || pred->instr == _do)
         continue;

      ibc_flow_instr *_cont = pred->instr;
      assert(_cont->op == IBC_FLOW_OP_CONT);
      _cont->merge = _while;
   }

   /* Link up all the jumps for breaks.  We stashed them in a list in
    * ibc_BREAK.
    */
   list_for_each_entry(ibc_flow_instr, _break, breaks, builder_link) {
      ibc_flow_instr_set_jump(_break, _while);
      _break->merge = _while;
   }
   list_inithead(breaks);

   return _while;
}

static inline ibc_flow_instr *
ibc_HALT_JUMP(ibc_builder *b, ibc_ref pred,
              enum ibc_predicate predicate,
              struct list_head *halt_jumps)
{
   ibc_flow_instr *_halt_jump =
      ibc_build_flow(b, IBC_FLOW_OP_HALT_JUMP, pred, predicate);

   list_addtail(&_halt_jump->builder_link, halt_jumps);

   return _halt_jump;
}

static inline ibc_flow_instr *
ibc_HALT_MERGE(ibc_builder *b, struct list_head *halt_jumps)
{
   ibc_flow_instr *_halt_merge =
      ibc_build_flow(b, IBC_FLOW_OP_HALT_MERGE,
                     ibc_null(IBC_TYPE_FLAG), IBC_PREDICATE_NONE);

   /* Link up all the jumps for halt jumps.  We stashed them in a list in
    * ibc_HALT_JUMP.
    */
   list_for_each_entry(ibc_flow_instr, _halt_jump, halt_jumps, builder_link) {
      assert(_halt_jump->op == IBC_FLOW_OP_HALT_JUMP);
      ibc_flow_instr_set_jump(_halt_jump, _halt_merge);
   }
   list_inithead(halt_jumps);

   return _halt_merge;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_BUILDER_H */
