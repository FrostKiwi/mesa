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

/** Composes an ibc_logical_reg_ref with an ibc_reg_ref
 *
 * Specifically, this is the composition outer(inner(reg))
 */
static inline ibc_reg_ref
compose_reg_refs(ibc_reg_ref outer, ibc_reg_ref inner,
                 unsigned outer_simd_group,
                 unsigned outer_simd_width,
                 unsigned inner_simd_group,
                 unsigned inner_simd_width)
{
   assert(outer.file == IBC_REG_FILE_LOGICAL);
   assert(ibc_type_bit_size(outer.type) <= ibc_type_bit_size(inner.type));

   ibc_reg_ref ref = inner;
   ref.type = outer.type;

   switch (ref.file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      return ref;

   case IBC_REG_FILE_LOGICAL:
      ref.logical.byte += outer.logical.byte;
      ref.logical.comp += outer.logical.comp;
      if (outer.logical.broadcast && !inner.logical.broadcast) {
         /* If the outer ref wants to broadcast and the inner ref is not
          * already broadcasting, we broadcast based on the outer ref.
          */
         ref.logical.broadcast = true;
         ref.logical.simd_channel = outer.logical.simd_channel;
      }
      return ref;

   case IBC_REG_FILE_HW_GRF:
      /* Components aren't well-defined for HW grfs */
      assert(outer.logical.comp == 0);
      if (outer.logical.broadcast) {
         assert(outer.logical.simd_channel >= inner_simd_group);
         assert(outer.logical.simd_channel < inner_simd_group +
                                             inner_simd_width);
         unsigned rel_channel = outer.logical.simd_channel - inner_simd_group;
         ibc_hw_grf_slice_simd_group(&ref.hw_grf, rel_channel, 1);
         ibc_hw_grf_mul_stride(&ref.hw_grf, 0);
      } else {
         assert(outer_simd_group >= inner_simd_group);
         assert(outer_simd_group + outer_simd_width <=
                inner_simd_group + inner_simd_width);
         unsigned rel_simd_group = outer_simd_group - inner_simd_group;
         ibc_hw_grf_slice_simd_group(&ref.hw_grf, rel_simd_group,
                                     outer_simd_width);
      }
      ibc_hw_grf_add_byte_offset(&ref.hw_grf, outer.logical.byte);
      return ref;

   case IBC_REG_FILE_FLAG:
      assert(outer.logical.byte == 0 &&
             outer.logical.comp == 0 &&
             !outer.logical.broadcast);
      return ref;
   }

   unreachable("Invalid IBC register file");
}

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

   ibc_instr *ssa_instr = ibc_reg_ssa_instr(ref->reg);
   if (ssa_instr == NULL)
      return false;

   switch (ssa_instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *mov = ibc_instr_as_alu(ssa_instr);

      /* Must be a non-saturating MOV */
      if (mov->op != IBC_ALU_OP_MOV)
         return false;

      /* Is this even possible? */
      assert(mov->src[0].ref.file != IBC_REG_FILE_NONE);

      if (mov->src[0].ref.file == IBC_REG_FILE_IMM)
         return false; /* TODO */

      /* The source must be static */
      if (!ibc_reg_ref_read_is_static(mov->src[0].ref))
         return false;

      /* Cannot saturate or type convert */
      if (mov->src[0].ref.type != mov->dest.type || mov->saturate)
         return false;

      if (alu_src) {
         alu_src->mod = compose_alu_src_mods(alu_src->mod, mov->src[0].mod);
      } else if (mov->src[0].mod != IBC_ALU_SRC_MOD_NONE) {
         return false;
      }

      *ref = compose_reg_refs(*ref, mov->src[0].ref,
                              simd_group, simd_width,
                              mov->instr.simd_group,
                              mov->instr.simd_width);

      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(ssa_instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_SIMD_ZIP:
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            if (ref->logical.broadcast) {
               if (ref->logical.simd_channel < intrin->src[i].simd_group ||
                   ref->logical.simd_channel >
                     intrin->src[i].simd_group + intrin->src[i].simd_width)
                  continue;
            } else {
               if (simd_group < intrin->src[i].simd_group ||
                   simd_group + simd_width >
                     intrin->src[i].simd_group + intrin->src[i].simd_width)
                  continue;
            }

            /* Is this even possible? */
            assert(intrin->src[i].ref.file != IBC_REG_FILE_NONE);

            if (intrin->src[i].ref.file == IBC_REG_FILE_IMM)
               return false; /* TODO */

            /* The source must be static */
            if (!ibc_reg_ref_read_is_static(intrin->src[i].ref))
               return false;

            *ref = compose_reg_refs(*ref, intrin->src[i].ref,
                                    simd_group, simd_width,
                                    intrin->src[i].simd_group,
                                    intrin->src[i].simd_width);
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

   ibc_foreach_instr_safe(instr, shader) {

      if (try_copy_prop_reg_ref(&instr->flag, NULL,
                                instr->simd_group, instr->simd_width))
         progress = true;

      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU: {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);

         for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
            if (try_copy_prop_reg_ref(&alu->src[i].ref, &alu->src[i],
                                      alu->instr.simd_group,
                                      alu->instr.simd_width))
               progress = true;
         }
         continue;
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
         continue;
      }

      case IBC_INSTR_TYPE_BRANCH:
      case IBC_INSTR_TYPE_MERGE:
         continue;
      }
      unreachable("Unsupported IBC instruction type");
   }

   return progress;
}
