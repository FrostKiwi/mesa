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

#include "ibc.h"
#include "ibc_builder.h"

static inline ibc_ref
ibc_builder_new_accum_reg(ibc_builder *b, enum ibc_type type)
{
   ibc_reg *reg = ibc_accum_reg_create(b->shader, type, b->simd_width);
   reg->is_wlr = false;
   return ibc_typed_ref(reg, type);
}

static void
build_MACH(ibc_builder *b, ibc_ref accum, ibc_ref dest,
           ibc_ref src0, ibc_ref src1)
{
   ibc_alu_instr *mach =
      ibc_build_alu2(b, IBC_ALU_OP_MACH, dest, src0, src1);
   ibc_alu_instr_set_accum(mach, accum, true);
}

/**
 * If this ref is an immediate that fits in 16-bits, return a new
 * immediate converted to UW/W type.
 */
static ibc_ref
ref_as_16bit_imm(ibc_ref ref)
{
   if (ref.file == IBC_FILE_IMM) {
      uint64_t u = ibc_ref_as_uint(ref);

      if (ref.type == IBC_TYPE_UD && u <= UINT16_MAX)
         return ibc_imm_uw(u);

      int64_t i = ibc_ref_as_int(ref);

      if (ref.type == IBC_TYPE_D && i <= INT16_MAX && i >= INT16_MIN)
         return ibc_imm_w(i);
   }

   return (ibc_ref) {};
}

static bool
lower_mul_dword(ibc_builder *b, ibc_alu_instr *alu)
{
   b->cursor = ibc_after_instr(&alu->instr);

   ibc_builder_push_instr_group(b, &alu->instr);

   ibc_ref src1_imm_word = ref_as_16bit_imm(alu->src[1].ref);
   if (src1_imm_word.file == IBC_FILE_IMM) {
      alu->src[1].ref = src1_imm_word;
   } else {
      /* Most of our hardware cannot do 32-bit integer multiplication in a
       * single instruction, but instead must do a sequence (which actually
       * calculations a 64-bit result):
       *
       *    mul(8)  acc0<1>D   g3<8,8,1>D      g4<8,8,1>D
       *    mach(8) null       g3<8,8,1>D      g4<8,8,1>D
       *    mov(8)  g2<1>D     acc0<8,8,1>D
       *
       * However, the fixed accumulator access prevents scheduling, and
       * also has issues in higher SIMD widths.
       *
       * Since we only want the low 32-bits of the result, we can do two
       * 32-bit x 16-bit multiplies (like the mul and mach are doing), and
       * adjust the high result and add them (like the mach is doing):
       *
       *    mul(8)  g7<1>D     g3<8,8,1>D      g4.0<8,8,1>UW
       *    mul(8)  g8<1>D     g3<8,8,1>D      g4.1<8,8,1>UW
       *    shl(8)  g9<1>D     g8<8,8,1>D      16D
       *    add(8)  g2<1>D     g7<8,8,1>D      g8<8,8,1>D
       *
       * We avoid the shl instruction by realizing that we only want to add
       * the low 16-bits of the "high" result to the high 16-bits of the
       * "low" result and using proper regioning on the add:
       *
       *    mul(8)  g7<1>D     g3<8,8,1>D      g4.0<16,8,2>UW
       *    mul(8)  g8<1>D     g3<8,8,1>D      g4.1<16,8,2>UW
       *    add(8)  g7.1<2>UW  g7.1<16,8,2>UW  g8<16,8,2>UW
       */
      ibc_ref x = alu->src[0].ref;
      ibc_ref y = alu->src[1].ref;

      ibc_ref y_lo, y_hi;
      if (y.file == IBC_FILE_IMM) {
         uint32_t src1_imm = ibc_ref_as_uint(y);
         y_lo = ibc_imm_uw(src1_imm & 0xffff);
         y_hi = ibc_imm_uw(src1_imm >> 16);
      } else {
         y_lo = ibc_subscript_ref(y, IBC_TYPE_UW, 0);
         y_hi = ibc_subscript_ref(y, IBC_TYPE_UW, 1);
      }

      ibc_reg *low_reg =
         ibc_hw_grf_reg_create(b->shader, 4 * alu->instr.simd_width, 4);
      ibc_reg *high_reg =
         ibc_hw_grf_reg_create(b->shader, 4 * alu->instr.simd_width, 4);
      ibc_ref low = ibc_typed_ref(low_reg, IBC_TYPE_D);
      ibc_ref high = ibc_typed_ref(high_reg, IBC_TYPE_D);

      /* Our construction of "low" violates WLR rules; don't claim it. */
      low_reg->is_wlr = false;

      ibc_build_alu2(b, IBC_ALU_OP_IMUL, low,  x, y_lo);
      ibc_build_alu2(b, IBC_ALU_OP_IMUL, high, x, y_hi);
      ibc_build_alu2(b, IBC_ALU_OP_ADD,
                     ibc_subscript_ref(low, IBC_TYPE_UW, 1),
                     ibc_subscript_ref(low, IBC_TYPE_UW, 1),
                     ibc_subscript_ref(high, IBC_TYPE_UW, 0));

      ibc_MOV_to(b, alu->dest, low);
      ibc_instr_remove(&alu->instr);
   }

   ibc_builder_pop(b);

   return true;
}

static bool
lower_mulh(ibc_builder *b, ibc_alu_instr *alu)
{
   b->cursor = ibc_after_instr(&alu->instr);

   assert(alu->src[1].ref.type == IBC_TYPE_D ||
          alu->src[1].ref.type == IBC_TYPE_UD);

   ibc_ref y;

   if (alu->src[1].ref.file == IBC_FILE_IMM) {
      y = ibc_imm_uw(ibc_ref_as_uint(alu->src[1].ref));
   } else {
      y = ibc_subscript_ref(alu->src[1].ref, IBC_TYPE_UW, 0);
   }

   ibc_reg *temp_reg = ibc_logical_reg_create(b->shader, 32, 1,
                                              alu->instr.simd_group,
                                              alu->instr.simd_width);
   temp_reg->logical.align = REG_SIZE;
   ibc_ref temp_ref = ibc_typed_ref(temp_reg, IBC_TYPE_UD);

   ibc_builder_push_instr_group(b, &alu->instr);

   ibc_ref acc = ibc_builder_new_accum_reg(b, alu->dest.type);
   ibc_build_alu2(b, IBC_ALU_OP_IMUL, acc, alu->src[0].ref, y);
   build_MACH(b, acc, temp_ref, alu->src[0].ref, alu->src[1].ref);

   ibc_MOV_to(b, alu->dest, temp_ref);

   ibc_instr_remove(&alu->instr);

   ibc_builder_pop(b);

   return true;
}

static bool
lower_mul_qword(ibc_builder *b, ibc_alu_instr *alu)
{
   b->cursor = ibc_after_instr(&alu->instr);

   /* Considering two 64-bit integers ab and cd where each letter        ab
    * corresponds to 32 bits, we get a 128-bit result WXYZ. We         * cd
    * only need to provide the YZ part of the result.               -------
    *                                                                    BD
    *  Only BD needs to be 64 bits. For AD and BC we only care       +  AD
    *  about the lower 32 bits (since they are part of the upper     +  BC
    *  32 bits of our result). AC is not needed since it starts      + AC
    *  on the 65th bit of the result.                               -------
    *                                                                  WXYZ
    */

   /* TODO: We can't assume this on Apollolake */
   assert(b->shader->devinfo->has_integer_dword_mul);

   /* Here we need the full 64 bit result for 32b * 32b. */
   ibc_ref bd = ibc_IMUL(b, IBC_TYPE_UQ,
                         ibc_subscript_ref(alu->src[0].ref, IBC_TYPE_UD, 0),
                         ibc_subscript_ref(alu->src[1].ref, IBC_TYPE_UD, 0));

   ibc_ref ad = ibc_IMUL(b, IBC_TYPE_UD,
                         ibc_subscript_ref(alu->src[0].ref, IBC_TYPE_UD, 1),
                         ibc_subscript_ref(alu->src[1].ref, IBC_TYPE_UD, 0));

   ibc_ref bc = ibc_IMUL(b, IBC_TYPE_UD,
                         ibc_subscript_ref(alu->src[0].ref, IBC_TYPE_UD, 0),
                         ibc_subscript_ref(alu->src[1].ref, IBC_TYPE_UD, 1));

   ibc_ref ad_plus_bc = ibc_ADD(b, IBC_TYPE_UD, ad, bc);
   ibc_ref bd_1_plus_ad = ibc_ADD(b, IBC_TYPE_UD,
                                  ibc_subscript_ref(bd, IBC_TYPE_UD, 1),
                                  ad_plus_bc);

   ibc_ref result =
      ibc_PACK2(b, bd_1_plus_ad, ibc_subscript_ref(bd, IBC_TYPE_UD, 0));

   ibc_MOV_to(b, alu->dest, result);

   return true;
}

/** Lowers integer multiplication operations where necessary */
bool
ibc_lower_integer_multiplication(ibc_shader *shader)
{
   const struct gen_device_info *devinfo = shader->devinfo;
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_ALU)
         continue;

      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      switch (alu->op) {
      case IBC_ALU_OP_IMUL:
         if (alu->dest.type & IBC_TYPE_VECTOR)
            break;

         if (ibc_type_bit_size(alu->dest.type) == 64 &&
             ibc_type_bit_size(alu->src[0].ref.type) == 64 &&
             ibc_type_bit_size(alu->src[1].ref.type) == 64) {
            progress |= lower_mul_qword(&b, alu);
         } else if (!devinfo->has_integer_dword_mul &&
                    ibc_type_bit_size(alu->src[0].ref.type) == 32 &&
                    ibc_type_bit_size(alu->src[1].ref.type) == 32) {
            progress |= lower_mul_dword(&b, alu);
         }
         break;
      case IBC_ALU_OP_IMULH:
         progress |= lower_mulh(&b, alu);
         break;
      default:
         break;
      }
   }

   if (progress)
      ibc_repair_wlr_order(shader);

   return progress;
}