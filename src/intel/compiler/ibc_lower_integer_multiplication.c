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
   return ibc_typed_ref(reg, type);
}

static void
build_MACH(ibc_builder *b, enum ibc_type dest_type, ibc_ref accum,
           ibc_ref src0, ibc_ref src1)
{
   ibc_alu_instr *mach =
      ibc_build_alu2(b, IBC_ALU_OP_MACH, ibc_null(dest_type), src0, src1);
   ibc_alu_instr_set_accum(mach, accum, true);
}

static bool
lower_mul(ibc_builder *b, ibc_alu_instr *alu)
{
   b->cursor = ibc_after_instr(&alu->instr);
   bool progress = false;

   ibc_builder_push_instr_group(b, &alu->instr);

   if ((alu->dest.type == IBC_TYPE_Q ||
        alu->dest.type == IBC_TYPE_UQ) &&
       (alu->src[0].ref.type == IBC_TYPE_Q ||
        alu->src[0].ref.type == IBC_TYPE_UQ) &&
       (alu->src[1].ref.type == IBC_TYPE_Q ||
        alu->src[1].ref.type == IBC_TYPE_UQ)) {
      assert(!"unimplemented");
   } else if ((alu->dest.type == IBC_TYPE_D ||
               alu->dest.type == IBC_TYPE_UD) &&
              b->shader->devinfo->has_integer_dword_mul) {
      ibc_ref src1_uw = ibc_MOV(b, IBC_TYPE_UW, alu->src[1].ref);

      if (alu->instr.simd_width == 1)
         ibc_builder_push_we_all(b, 8);

      ibc_ref acc = ibc_builder_new_accum_reg(b, alu->dest.type);
      ibc_build_alu2(b, IBC_ALU_OP_MUL, acc, alu->src[0].ref, src1_uw);
      build_MACH(b, alu->dest.type, acc, alu->src[0].ref, alu->src[1].ref);

      if (alu->instr.simd_width == 1)
         ibc_builder_pop(b);

      ibc_MOV_to(b, alu->dest, acc);

      ibc_instr_remove(&alu->instr);
      progress = true;
   }

   ibc_builder_pop(b);

   return progress;
}

/** Lowers integer multiplication operations where necessary */
bool
ibc_lower_integer_multiplication(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_ALU)
         continue;

      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      switch (alu->op) {
      case IBC_ALU_OP_MUL:
         progress |= lower_mul(&b, alu);
         break;
      default:
         break;
      }
   }

   return progress;
}
