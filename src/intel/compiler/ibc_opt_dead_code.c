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
mark_ref(ibc_ref *ref,
         UNUSED int num_bytes,
         UNUSED int num_comps,
         UNUSED uint8_t simd_group,
         UNUSED uint8_t simd_width,
         void *_progress)
{
   bool *progress = _progress;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg && ref->reg->index == 0) {
      ((ibc_reg *)ref->reg)->index = 1;
      *progress = true;
   }

   return true;
}

static bool
ref_is_dead(ibc_ref *ref,
            UNUSED int num_bytes,
            UNUSED int num_comps,
            UNUSED uint8_t simd_group,
            UNUSED uint8_t simd_width,
            UNUSED void *_state)
{
   switch (ref->file) {
   case IBC_FILE_NONE:
      return true;

   case IBC_FILE_IMM:
      unreachable("Invalid destination register file");

   default:
      return ref->reg != NULL && ref->reg->index == 0;
   }
}

static bool
instr_is_alive(ibc_instr *instr)
{
   if (!ibc_instr_foreach_write(instr, ref_is_dead, NULL))
      return true;

   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      return false;

   case IBC_INSTR_TYPE_SEND:
      return ibc_instr_as_send(instr)->has_side_effects;

   case IBC_INSTR_TYPE_INTRINSIC:
      return ibc_instr_as_intrinsic(instr)->has_side_effects;

   case IBC_INSTR_TYPE_FLOW:
      return true;
   }

   unreachable("Invalid instruction type");
}

/**
 * Some instructions (like CMP) may write both a flag and a destination
 * register.  That destination may be dead, even if we still need the
 * flag result.  Eliminating it will save us a register and eliminate
 * post-RA scheduling barriers.
 */
static bool
try_removing_dest_write(ibc_instr *instr)
{
   bool progress = false;

   if (instr->type == IBC_INSTR_TYPE_ALU) {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);

      if (alu->dest.file != IBC_FILE_NONE &&
          ref_is_dead(&alu->dest, 0, 0, 0, 0, NULL)) {
         ibc_ref nullref = { .type = alu->dest.type };
         ibc_instr_set_ref(instr, &alu->dest, nullref);
         progress = true;
      }
   }

   /* TODO: also atomic intrinsic destinations */

   return progress;
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
      progress |= try_removing_dest_write(instr);

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
