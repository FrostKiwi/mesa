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

static ibc_reg *
ibc_reg_create(ibc_shader *shader, enum ibc_reg_file file)
{
   ibc_reg *reg = rzalloc(shader, ibc_reg);

   reg->file = file;
   list_addtail(&reg->link, &shader->regs);

   return reg;
}

ibc_reg *
ibc_logical_reg_create(ibc_shader *shader,
                       uint8_t bit_size, uint8_t num_comps,
                       uint8_t simd_group, uint8_t simd_width)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_REG_FILE_LOGICAL);

   reg->logical.bit_size = bit_size;
   reg->logical.num_comps = num_comps;
   reg->logical.simd_group = simd_group;
   reg->logical.simd_width = simd_width;

   return reg;
}

ibc_reg *
ibc_hw_grf_reg_create(ibc_shader *shader,
                      uint16_t byte, uint8_t size, uint8_t align)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_REG_FILE_HW_GRF);

   reg->hw_grf.byte = byte;
   reg->hw_grf.size = size;
   reg->hw_grf.align = align;

   return reg;
}

ibc_reg *
ibc_flag_reg_create(ibc_shader *shader,
                    uint8_t subnr, uint8_t bits)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_REG_FILE_FLAG);

   reg->flag.subnr = subnr;
   reg->flag.bits = bits;

   return reg;
}

static void
ibc_reg_ref_init(ibc_reg_ref *ref)
{
}

static void
ibc_instr_init(ibc_instr *instr, enum ibc_instr_type type,
               uint8_t simd_group, uint8_t simd_width)
{
   assert(simd_width > 0);
   instr->type = type;
   instr->simd_group = simd_group;
   instr->simd_width = simd_width;

   ibc_reg_ref_init(&instr->flag);
}

bool
ibc_instr_foreach_read(ibc_instr *instr, ibc_reg_ref_cb cb, void *state)
{
   if (instr->predicate) {
      if (!cb(&instr->flag, state))
         return false;
   }

   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
         if (!cb(&alu->src[i].ref, state))
            return false;
      }
      return true;
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      if (send->desc.file != IBC_REG_FILE_NONE &&
          !cb(&send->desc, state))
         return false;

      if (send->ex_desc.file != IBC_REG_FILE_NONE &&
          !cb(&send->ex_desc, state))
         return false;

      if (!cb(&send->payload[0], state))
         return false;

      if (send->ex_mlen > 0 && !cb(&send->payload[1], state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      for (unsigned i = 0; i < intrin->num_srcs; i++) {
         if (!cb(&intrin->src[i].ref, state))
            return false;
      }
      return true;
   }

   case IBC_INSTR_TYPE_JUMP:
      return true;
   }

   unreachable("Invalid IBC instruction type");
}

bool
ibc_instr_foreach_write(ibc_instr *instr, ibc_reg_ref_cb cb, void *state)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      if (alu->cmod && !cb(&instr->flag, state))
         return false;

      if (!cb(&alu->dest, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      if (send->rlen > 0 && !cb(&send->dest, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      if (intrin->dest.file != IBC_REG_FILE_NONE &&
          cb(&intrin->dest, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_JUMP:
      return true;
   }

   unreachable("Invalid IBC instruction type");
}

#define IBC_ALU_OP_DECL(OP, _num_srcs, _src_mods)        \
   {                                                     \
      .name = #OP,                                       \
      .num_srcs = _num_srcs,                             \
      .supported_src_mods = IBC_ALU_SRC_MOD_##_src_mods, \
   },

const ibc_alu_op_info ibc_alu_op_infos[IBC_ALU_NUM_OPS] = {
#include "ibc_alu_ops.h"
};

#undef IBC_ALU_OP_DECL

ibc_alu_instr *
ibc_alu_instr_create(struct ibc_shader *shader, enum ibc_alu_op op,
                     uint8_t simd_group, uint8_t simd_width)
{
   const unsigned num_srcs = ibc_alu_op_infos[op].num_srcs;

   ibc_alu_instr *alu = rzalloc_size(shader, sizeof(ibc_alu_instr) +
                                             sizeof(ibc_alu_src) * num_srcs);

   ibc_instr_init(&alu->instr, IBC_INSTR_TYPE_ALU, simd_group, simd_width);

   alu->op = op;

   ibc_reg_ref_init(&alu->dest);

   for (unsigned i = 0; i < num_srcs; i++)
      ibc_reg_ref_init(&alu->src[i].ref);

   return alu;
}

ibc_send_instr *
ibc_send_instr_create(struct ibc_shader *shader,
                      uint8_t simd_group,
                      uint8_t simd_width)
{
   ibc_send_instr *send = rzalloc(shader, ibc_send_instr);

   ibc_instr_init(&send->instr, IBC_INSTR_TYPE_SEND, simd_group, simd_width);

   ibc_reg_ref_init(&send->desc);
   ibc_reg_ref_init(&send->ex_desc);

   ibc_reg_ref_init(&send->dest);

   ibc_reg_ref_init(&send->payload[0]);
   ibc_reg_ref_init(&send->payload[1]);

   return send;
}

ibc_intrinsic_instr *
ibc_intrinsic_instr_create(struct ibc_shader *shader,
                           enum ibc_intrinsic_op op,
                           uint8_t simd_group, uint8_t simd_width,
                           unsigned num_srcs)
{
   ibc_intrinsic_instr *intrin =
      rzalloc_size(shader, sizeof(ibc_intrinsic_instr) +
                           sizeof(ibc_intrinsic_src) * num_srcs);

   ibc_instr_init(&intrin->instr, IBC_INSTR_TYPE_INTRINSIC,
                  simd_group, simd_width);

   intrin->op = op;

   ibc_reg_ref_init(&intrin->dest);

   intrin->num_srcs = num_srcs;
   for (unsigned i = 0; i < num_srcs; i++) {
      intrin->src[i].simd_group = simd_group;
      intrin->src[i].simd_width = simd_width;
      ibc_reg_ref_init(&intrin->src[i].ref);
   }

   return intrin;
}

ibc_block *
ibc_block_create(ibc_shader *shader)
{
   ibc_block *block = rzalloc(shader, ibc_block);

   list_inithead(&block->instrs);

   return block;
}

ibc_shader *
ibc_shader_create(void *mem_ctx,
                  const struct gen_device_info *devinfo)
{
   ibc_shader *shader = rzalloc(mem_ctx, ibc_shader);

   shader->devinfo = devinfo,

   list_inithead(&shader->blocks);
   ibc_block *first_block = ibc_block_create(shader);
   list_add(&first_block->link, &shader->blocks);

   list_inithead(&shader->regs);

   return shader;
}

void
ibc_instr_insert(ibc_instr *instr, ibc_cursor cursor)
{
   list_add(&instr->link, cursor.prev);
}

void
ibc_instr_remove(ibc_instr *instr)
{
   list_del(&instr->link);
}
