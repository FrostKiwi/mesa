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

#include "ibc.h"

static enum ibc_alu_src_mod
compose_alu_src_mods(enum ibc_alu_src_mod outer, enum ibc_alu_src_mod inner)
{
   enum ibc_alu_src_mod mod = outer;

   if (!(mod & IBC_ALU_SRC_MOD_ABS))
      mod ^= inner & IBC_ALU_SRC_MOD_NEG;

   mod |= inner & IBC_ALU_SRC_MOD_ABS;
   mod ^= inner & IBC_ALU_SRC_MOD_NOT;

   return mod;
}

static bool
try_copy_prop_reg_ref(ibc_reg_ref *ref, ibc_alu_src *alu_src,
                      uint8_t simd_group, uint8_t simd_width)
{
   if (ref->file != IBC_REG_FILE_LOGICAL)
      return false;

   if (ref->reg->logical.ssa == NULL)
      return false;

   switch (ref->reg->logical.ssa->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *mov = ibc_instr_as_alu(ref->reg->logical.ssa);

      /* Must be a non-saturating MOV */
      if (mov->op != IBC_ALU_OP_MOV)
         return false;

      /* The source must also be SSA */
      if (mov->src[0].ref.file != IBC_REG_FILE_LOGICAL ||
          mov->src[0].ref.reg->logical.ssa == NULL)
         return false;

      /* Cannot saturate or type convert */
      if (mov->src[0].ref.type != mov->dest.ref.type || mov->saturate)
         return false;

      if (alu_src) {
         alu_src->mod = compose_alu_src_mods(alu_src->mod, mov->src[0].mod);
      } else if (mov->src[0].mod != IBC_ALU_SRC_MOD_NONE) {
         return false;
      }

      ref->reg = mov->src[0].ref.reg;
      ref->comp += mov->src[0].ref.comp;
      ref->byte += mov->src[0].ref.byte;

      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin =
         ibc_instr_as_intrinsic(ref->reg->logical.ssa);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_SIMD_ZIP:
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            if (simd_group < intrin->src[i].simd_group ||
                simd_group + simd_width >
                  intrin->src[i].simd_group + intrin->src[i].simd_width)
               continue;

            ref->reg = intrin->src[i].ref.reg;
            ref->comp += intrin->src[i].ref.comp;
            ref->byte += intrin->src[i].ref.byte;
            return true;
         }
         return false;

      default:
         return false;
      }
   }

   default:
      return false;
   }
}

bool
ibc_opt_copy_prop(ibc_shader *shader)
{
   bool progress = false;

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr_safe(instr, block) {

         /* TODO: Handle predicate flag? */

         switch (instr->type) {
         case IBC_INSTR_TYPE_ALU: {
            ibc_alu_instr *alu = ibc_instr_as_alu(instr);

            unsigned num_srcs = 3; /* TODO */
            for (unsigned i = 0; i < num_srcs; i++) {
               if (try_copy_prop_reg_ref(&alu->src[i].ref, &alu->src[i],
                                         alu->instr.simd_group,
                                         alu->instr.simd_width))
                  progress = true;
            }
         }

         case IBC_INSTR_TYPE_SEND:
            /* TODO */
            continue;

         case IBC_INSTR_TYPE_INTRINSIC: {
            ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
            for (unsigned i = 0; i < intrin->num_srcs; i++) {
               if (try_copy_prop_reg_ref(&intrin->src[i].ref, NULL,
                                         intrin->src[i].simd_group,
                                         intrin->src[i].simd_width))
                  progress = true;
            }
         }

         case IBC_INSTR_TYPE_JUMP:
            continue;
         }
         unreachable("Unsupported IBC instruction type");
      }
   }

   return progress;
}
