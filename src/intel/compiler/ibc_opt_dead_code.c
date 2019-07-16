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

static bool
reg_ref_is_alive(ibc_reg_ref *ref)
{
   switch (ref->file) {
   case IBC_REG_FILE_NONE:
      return false;

   case IBC_REG_FILE_IMM:
      unreachable("Invalid destination register file");

   case IBC_REG_FILE_LOGICAL:
      return ref->reg->index;

   case IBC_REG_FILE_HW_GRF:
      /* If it's a fixed HW reg, we consider it live */
      return ref->reg == NULL || ref->reg->index;

   case IBC_REG_FILE_FLAG:
      /* If it's a fixed HW reg, we consider it live */
      return ref->reg->flag.subnr != IBC_FLAG_REG_UNASSIGNED ||
             ref->reg->index;
   }
   unreachable("Invalid register file");
}

static bool
mark_ref(ibc_reg_ref *ref,
         UNUSED int8_t num_comps,
         UNUSED uint8_t simd_group,
         UNUSED uint8_t simd_width,
         void *_progress)
{
   bool *progress = _progress;

   if (ref->file == IBC_REG_FILE_NONE ||
       ref->file == IBC_REG_FILE_IMM)
      return true;

   if (ref->reg && ref->reg->index == 0) {
      ((ibc_reg *)ref->reg)->index = 1;
      *progress = true;
   }

   return true;
}

static bool
instr_is_alive(ibc_instr *instr) {
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      return reg_ref_is_alive(&alu->dest) ||
             (alu->cmod && reg_ref_is_alive(&alu->instr.flag));
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      return send->has_side_effects || reg_ref_is_alive(&send->dest);
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      return intrin->has_side_effects || reg_ref_is_alive(&intrin->dest);
   }

   case IBC_INSTR_TYPE_BRANCH:
   case IBC_INSTR_TYPE_MERGE:
      return true;

   case IBC_INSTR_TYPE_PHI:
      return reg_ref_is_alive(&ibc_instr_as_phi(instr)->dest);
   }

   unreachable("Invalid instruction type");
}

bool
ibc_opt_dead_code(ibc_shader *shader)
{
   /* We're going to use the index to mark stuff as used */
   ibc_foreach_reg(reg, shader)
      reg->index = 0;

   bool progress;
   do {
      progress = false;

      ibc_foreach_instr_reverse(instr, shader) {
         if (!instr_is_alive(instr))
            continue;

         ibc_instr_foreach_read(instr, mark_ref, &progress);
         ibc_instr_foreach_write(instr, mark_ref, &progress);
      }
   } while (progress);

   ibc_foreach_instr_safe(instr, shader) {
      if (!instr_is_alive(instr)) {
         ibc_instr_remove(instr);
         ralloc_free(instr);
         progress = true;
      }
   }

   unsigned num_regs = 0;
   ibc_foreach_reg_safe(reg, shader) {
      if (reg->index) {
         reg->index = num_regs++;
      } else {
         list_del(&reg->link);
         ralloc_free(reg);
         progress = true;
      }
   }

   return progress;
}
