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

static unsigned
reg_size(ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_HW_GRF:
      return reg->hw_grf.size;
   case IBC_REG_FILE_LOGICAL:
      return reg->logical.simd_width *
             reg->logical.num_comps * (reg->logical.bit_size / 8);
   }
   unreachable("Unsupported register file");
}

static unsigned
reg_align(ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_HW_GRF:
      return reg->hw_grf.align;
   case IBC_REG_FILE_LOGICAL:
      return MIN2(reg->logical.simd_width * (reg->logical.bit_size / 8), 32);
   }
   unreachable("Unsupported register file");
}

static void
rewrite_reg_ref(ibc_reg_ref *ref, ibc_hw_grf_reg *logical_grfs, bool is_src)
{
   if (ref->reg == NULL || ref->reg->file == IBC_REG_FILE_HW_GRF)
      return;

   assert(ref->reg->file == IBC_REG_FILE_LOGICAL);

   const uint8_t comp = ref->comp;
   ref->file = IBC_REG_FILE_HW_GRF;
   if (ref->reg->logical.simd_width == 1 && is_src)
      ref->stride = 0;
   else
      ref->stride = ref->reg->logical.bit_size / 8;
   ref->offset = comp * ref->reg->logical.simd_width * ref->stride;
}

void
ibc_assign_regs(ibc_shader *shader)
{
   void *dead_ctx = ralloc_context(shader);

   unsigned num_logical = 0;
   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_LOGICAL)
         reg->index = num_logical++;
   }

   ibc_hw_grf_reg *logical_grfs =
      ralloc_array(dead_ctx, ibc_hw_grf_reg, num_logical);
   for (unsigned i = 0; i < num_logical; i++)
      logical_grfs[i].byte = IBC_HW_GRF_REG_UNASSIGNED;

   /* Assign a high register number to the final EOT send */
   {
      ibc_block *last_block = list_last_entry(&shader->blocks,
                                              ibc_block, link);
      ibc_instr *last_instr = list_last_entry(&last_block->instrs,
                                              ibc_instr, link);
      assert(last_instr->type == IBC_INSTR_TYPE_SEND);
      ibc_send_instr *last_send = ibc_instr_as_send(last_instr);

      assert(last_send->eot);
      /* TODO: Support SENDS */
      assert(last_send->payload[1].reg == NULL);
      assert(last_send->payload[0].reg->file == IBC_REG_FILE_HW_GRF);
      ibc_hw_grf_reg *grf = &last_send->payload[0].reg->hw_grf;

      /* Just place it as far up in the file as it will go */
      assert(grf->size % 32 == 0);
      grf->byte = (128 * 32) - grf->size;
   }

   unsigned byte = 128; /* Leave room for headers */

   /* Assign scalars first so they get packed */
   ibc_foreach_reg(reg, shader) {
      if (reg->file != IBC_REG_FILE_LOGICAL ||
          reg->logical.simd_width > 1)
         continue;

      ibc_hw_grf_reg *grf = &logical_grfs[reg->index];

      *grf = (ibc_hw_grf_reg) {
         .size = reg_size(reg),
         .align = reg_align(reg),
      };
      grf->byte = ALIGN(byte, grf->align);
      byte = grf->byte + grf->size;
   }

   ibc_foreach_reg(reg, shader) {
      assert(reg->file == IBC_REG_FILE_LOGICAL ||
             reg->file == IBC_REG_FILE_HW_GRF);
      ibc_hw_grf_reg *grf = reg->file == IBC_REG_FILE_LOGICAL ?
                            &logical_grfs[reg->index] : &reg->hw_grf;

      if (grf->byte != IBC_HW_GRF_REG_UNASSIGNED)
         continue;

      if (reg->file != IBC_REG_FILE_HW_GRF) {
         /* Set up the initial HW GRF */
         *grf = (ibc_hw_grf_reg) {
            .size = reg_size(reg),
            .align = reg_align(reg),
         };
      }
      grf->byte = ALIGN(byte, grf->align);
      byte = grf->byte + grf->size;
   }

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr(instr, block) {
         /* Right now, only ALU instructions use logical regs */
         switch (instr->type) {
         case IBC_INSTR_TYPE_ALU: {
            ibc_alu_instr *alu = ibc_instr_as_alu(instr);

            if (alu->dest.ref.file == IBC_REG_FILE_LOGICAL)
               rewrite_reg_ref(&alu->dest.ref, logical_grfs, false);

            unsigned num_srcs = 3; /* TODO */
            for (unsigned i = 0; i < num_srcs; i++) {
               if (alu->src[i].ref.file == IBC_REG_FILE_LOGICAL)
                  rewrite_reg_ref(&alu->src[i].ref, logical_grfs, true);
            }
            continue;
         }

         case IBC_INSTR_TYPE_SEND: {
            ibc_send_instr *send = ibc_instr_as_send(instr);
            rewrite_reg_ref(&send->dest, logical_grfs, false);
            rewrite_reg_ref(&send->payload[0], logical_grfs, false);
            rewrite_reg_ref(&send->payload[1], logical_grfs, false);
            rewrite_reg_ref(&send->desc, logical_grfs, false);
            rewrite_reg_ref(&send->ex_desc, logical_grfs, false);
            continue;
         }

         case IBC_INSTR_TYPE_INTRINSIC:
            unreachable("These should no longer exist");
         }
         unreachable("Invalid instruction type");
      }
   }

   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_LOGICAL) {
         reg->file = IBC_REG_FILE_HW_GRF;
         reg->hw_grf = logical_grfs[reg->index];
      }
   }

   ralloc_free(dead_ctx);
}
