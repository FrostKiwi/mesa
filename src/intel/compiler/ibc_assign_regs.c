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
ibc_alu_instr_is_raw_mov(const ibc_alu_instr *alu)
{
   return alu->op == IBC_ALU_OP_MOV &&
          alu->dest.type == alu->src[0].ref.type &&
          alu->src[0].mod == IBC_ALU_SRC_MOD_NONE &&
          !alu->saturate;
}

static unsigned
logical_reg_stride(const ibc_reg *reg)
{
   assert(reg->file == IBC_REG_FILE_LOGICAL);
   assert(reg->logical.bit_size >= 8);

   ibc_instr *ssa_instr = ibc_reg_ssa_instr(reg);
   if (!ssa_instr || ssa_instr->type != IBC_INSTR_TYPE_ALU)
      return reg->logical.bit_size / 8;

   ibc_alu_instr *alu = ibc_instr_as_alu(ssa_instr);
   assert(alu->dest.reg == reg);

   unsigned stride = ibc_type_byte_size(alu->dest.type);
   for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++)
      stride = MAX2(stride, ibc_type_byte_size(alu->src[i].ref.type));

   /* Only raw MOV supports a packed-byte destination */
   if (stride == 1 && !ibc_alu_instr_is_raw_mov(alu))
      stride = 2;

   return stride;
}

static unsigned
reg_size(ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_HW_GRF:
      return reg->hw_grf.size;
   case IBC_REG_FILE_LOGICAL:
      return reg->logical.simd_width *
             reg->logical.num_comps * logical_reg_stride(reg);
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
      return MIN2(reg->logical.simd_width *
                  logical_reg_stride(reg), 32);
   }
   unreachable("Unsupported register file");
}

static void
rewrite_reg_ref(ibc_reg_ref *ref, unsigned ref_simd_group,
                ibc_hw_grf_reg *logical_grfs, bool is_src)
{
   if (ref->reg == NULL || ref->reg->file == IBC_REG_FILE_HW_GRF)
      return;

   assert(ref->reg->file == IBC_REG_FILE_LOGICAL);

   ibc_logical_reg_ref logical = ref->logical;
   ref->file = IBC_REG_FILE_HW_GRF;
   if (ref->reg->logical.simd_width == 1 && is_src)
      ref->hw_grf.stride = 0;
   else
      ref->hw_grf.stride = logical_reg_stride(ref->reg);
   ref->hw_grf.offset = ref->hw_grf.stride * logical.comp *
                        ref->reg->logical.simd_width;
   if (logical.broadcast) {
      ref->hw_grf.offset += ref->hw_grf.stride * logical.simd_channel;
      ref->hw_grf.stride = 0;
   } else {
      ref->hw_grf.offset += ref->hw_grf.stride *
                            (ref_simd_group - ref->reg->logical.simd_group);
   }
   ref->hw_grf.offset += logical.byte;
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
      if (reg->file == IBC_REG_FILE_FLAG)
         continue;

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
         /* Flags should already have been lowered */
         assert(instr->flag.file == IBC_REG_FILE_NONE ||
                instr->flag.file == IBC_REG_FILE_FLAG);

         /* Right now, only ALU instructions use logical regs */
         switch (instr->type) {
         case IBC_INSTR_TYPE_ALU: {
            ibc_alu_instr *alu = ibc_instr_as_alu(instr);

            if (alu->dest.file == IBC_REG_FILE_LOGICAL) {
               rewrite_reg_ref(&alu->dest, alu->instr.simd_group,
                               logical_grfs, false);
            }

            for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
               if (alu->src[i].ref.file == IBC_REG_FILE_LOGICAL) {
                  rewrite_reg_ref(&alu->src[i].ref, alu->instr.simd_group,
                                  logical_grfs, true);
               }
            }
            continue;
         }

         case IBC_INSTR_TYPE_SEND: {
            ibc_send_instr *send = ibc_instr_as_send(instr);
            rewrite_reg_ref(&send->dest, send->instr.simd_group,
                            logical_grfs, false);
            rewrite_reg_ref(&send->payload[0], send->instr.simd_group,
                            logical_grfs, false);
            rewrite_reg_ref(&send->payload[1], send->instr.simd_group,
                            logical_grfs, false);
            rewrite_reg_ref(&send->desc, send->instr.simd_group,
                            logical_grfs, false);
            rewrite_reg_ref(&send->ex_desc, send->instr.simd_group,
                            logical_grfs, false);
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
         if (reg->logical.bit_size == 1) {
            reg->file = IBC_REG_FILE_FLAG;
            reg->flag = (ibc_flag_reg) {
               .subnr = 0,
               .bits = reg->logical.simd_width,
            };
         } else {
            reg->file = IBC_REG_FILE_HW_GRF;
            reg->hw_grf = logical_grfs[reg->index];
         }
      }
   }

   ralloc_free(dead_ctx);
}
