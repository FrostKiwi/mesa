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
   list_inithead(&reg->writes);

   return reg;
}

ibc_reg *
ibc_logical_reg_create(ibc_shader *shader,
                       uint8_t bit_size, uint8_t num_comps,
                       uint8_t simd_group, uint8_t simd_width)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_REG_FILE_LOGICAL);

   reg->is_wlr = true;
   reg->logical.bit_size = bit_size;
   reg->logical.num_comps = num_comps;
   reg->logical.simd_group = simd_group;
   reg->logical.simd_width = simd_width;

   return reg;
}

ibc_reg *
ibc_hw_grf_reg_create(ibc_shader *shader, uint16_t size, uint8_t align)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_REG_FILE_HW_GRF);

   reg->is_wlr = true;
   reg->hw_grf.size = size;
   reg->hw_grf.align = align;

   return reg;
}

ibc_reg *
ibc_flag_reg_create(ibc_shader *shader, uint8_t bits)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_REG_FILE_FLAG);

   reg->is_wlr = true;
   reg->flag.bits = bits;
   reg->flag.align_mul = MAX2(bits, 16);
   reg->flag.align_offset = 0;

   return reg;
}

ibc_instr *
ibc_reg_ssa_instr(const ibc_reg *reg)
{
   if (!reg->is_wlr)
      return NULL;

   if (!list_is_singular(&reg->writes))
      return NULL;

   return LIST_ENTRY(ibc_ref, reg->writes.next, write_link)->write_instr;
}

static void
ibc_ref_init(ibc_ref *ref)
{
}

static void
ibc_ref_link_write(ibc_ref *ref, ibc_instr *instr)
{
   if (ref->file == IBC_REG_FILE_NONE || ref->file == IBC_REG_FILE_IMM)
      return;

   if (ref->reg) {
      ref->write_instr = instr;
      list_addtail(&ref->write_link, &((ibc_reg *)ref->reg)->writes);
   }
}

static void
ibc_ref_unlink_write(ibc_ref *ref, ibc_instr *instr)
{
   if (ref->write_instr) {
      assert(ref->write_instr == instr);
      assert(ref->file != IBC_REG_FILE_NONE);
      assert(ref->file != IBC_REG_FILE_IMM);
      list_del(&ref->write_link);
      ref->write_instr = NULL;
   }
}

static void
ibc_instr_init(ibc_instr *instr, enum ibc_instr_type type,
               uint8_t simd_group, uint8_t simd_width)
{
   assert(simd_width > 0);
   instr->type = type;
   instr->simd_group = simd_group;
   instr->simd_width = simd_width;

   ibc_ref_init(&instr->flag);
}

static int8_t
num_comps_for_reg_count(ibc_ref ref, unsigned reg_count,
                        unsigned simd_width)
{
   if (ref.file != IBC_REG_FILE_LOGICAL)
      return -1;

   /* We assume here that the register is tightly packed and works out to a
    * integer number of registers per component.  If not, we have to bail.
    */
   assert(ref.reg->logical.bit_size >= 8);
   unsigned comp_size_B = (ref.reg->logical.bit_size / 8) * simd_width;
   assert(comp_size_B % 32 == 0);
   unsigned comp_size_regs = comp_size_B / 32;
   assert(reg_count % comp_size_regs == 0);
   return reg_count / comp_size_regs;
}

bool
ibc_instr_foreach_read(ibc_instr *instr, ibc_ref_cb cb, void *state)
{
   if (instr->predicate) {
      if (!cb(&instr->flag, -1, 1,
              instr->simd_group, instr->simd_width, state))
         return false;
   }

   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
         if (!cb(&alu->src[i].ref, -1, 1,
                 instr->simd_group, instr->simd_width, state))
            return false;
      }
      return true;
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      if (send->desc.file != IBC_REG_FILE_NONE &&
          !cb(&send->desc, sizeof(uint32_t), 1, 0, 1, state))
         return false;

      if (send->ex_desc.file != IBC_REG_FILE_NONE &&
          !cb(&send->ex_desc, sizeof(uint32_t), 1, 0, 1, state))
         return false;

      if (!cb(&send->payload[0], send->mlen * REG_SIZE,
              num_comps_for_reg_count(send->payload[0], send->mlen,
                                      instr->simd_width),
              instr->simd_group, instr->simd_width, state))
         return false;

      if (send->ex_mlen > 0 &&
          !cb(&send->payload[1], send->ex_mlen * REG_SIZE,
              num_comps_for_reg_count(send->payload[1], send->ex_mlen,
                                      instr->simd_width),
              instr->simd_group, instr->simd_width, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      for (unsigned i = 0; i < intrin->num_srcs; i++) {
         if (!cb(&intrin->src[i].ref, -1, intrin->src[i].num_comps,
                 intrin->src[i].simd_group, intrin->src[i].simd_width, state))
            return false;
      }
      return true;
   }

   case IBC_INSTR_TYPE_FLOW:
      return true;
   }

   unreachable("Invalid IBC instruction type");
}

bool
ibc_instr_foreach_write(ibc_instr *instr, ibc_ref_cb cb, void *state)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      if (alu->cmod && !cb(&instr->flag, -1, 1, instr->simd_group,
                           instr->simd_width, state))
         return false;

      if (!cb(&alu->dest, -1, 1, instr->simd_group, instr->simd_width, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      if (send->rlen > 0 &&
          !cb(&send->dest, send->rlen * REG_SIZE,
              num_comps_for_reg_count(send->dest, send->rlen,
                                      instr->simd_width),
              instr->simd_group, instr->simd_width, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      if (intrin->dest.file != IBC_REG_FILE_NONE &&
          !cb(&intrin->dest, -1, intrin->num_dest_comps,
              instr->simd_group, instr->simd_width, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_FLOW:
      return true;
   }

   unreachable("Invalid IBC instruction type");
}

static void
ibc_instr_set_write_ref(ibc_instr *instr, ibc_ref *write_ref,
                        ibc_ref new_ref)
{
   ibc_ref_unlink_write(write_ref, instr);
   *write_ref = new_ref;
   ibc_ref_link_write(write_ref, instr);
}

static bool
refs_not_equal(ibc_ref *ref,
               UNUSED int num_bytes,
               UNUSED int num_comps,
               UNUSED uint8_t simd_group,
               UNUSED uint8_t simd_width,
               void *_other_ref)
{
   return ref != (ibc_ref *)_other_ref;
}

void
ibc_instr_set_ref(ibc_instr *instr, ibc_ref *ref, ibc_ref new_ref)
{
   if (instr == NULL || ibc_instr_foreach_write(instr, refs_not_equal, ref)) {
      assert(ref->write_instr == NULL);
      *ref = new_ref;
   } else {
      assert(instr && (ref->reg == NULL || ref->write_instr == instr));
      ibc_instr_set_write_ref(instr, ref, new_ref);
   }
}

void
ibc_instr_set_predicate(ibc_instr *instr, ibc_ref flag,
                        enum brw_predicate predicate,
                        bool pred_inverse)
{
   /* We don't do any use tracking right now so we can just assign this */
   instr->flag = flag;
   instr->predicate = predicate,
   instr->pred_inverse = pred_inverse;
}

void
ibc_alu_instr_set_cmod(ibc_alu_instr *alu, ibc_ref flag,
                       enum brw_conditional_mod cmod)
{
   alu->cmod = cmod;
   ibc_instr_set_write_ref(&alu->instr, &alu->instr.flag, flag);
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

   ibc_ref_init(&alu->dest);

   for (unsigned i = 0; i < num_srcs; i++)
      ibc_ref_init(&alu->src[i].ref);

   return alu;
}

ibc_send_instr *
ibc_send_instr_create(struct ibc_shader *shader,
                      uint8_t simd_group,
                      uint8_t simd_width)
{
   ibc_send_instr *send = rzalloc(shader, ibc_send_instr);

   ibc_instr_init(&send->instr, IBC_INSTR_TYPE_SEND, simd_group, simd_width);

   ibc_ref_init(&send->desc);
   ibc_ref_init(&send->ex_desc);

   ibc_ref_init(&send->dest);

   ibc_ref_init(&send->payload[0]);
   ibc_ref_init(&send->payload[1]);

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
   intrin->can_reorder = true;
   intrin->has_side_effects = false;

   ibc_ref_init(&intrin->dest);

   intrin->num_srcs = num_srcs;
   for (unsigned i = 0; i < num_srcs; i++) {
      intrin->src[i].simd_group = simd_group;
      intrin->src[i].simd_width = simd_width;
      ibc_ref_init(&intrin->src[i].ref);
   }

   return intrin;
}

ibc_flow_instr *
ibc_flow_instr_create(struct ibc_shader *shader,
                      enum ibc_flow_op op,
                      uint8_t simd_width)
{
   ibc_flow_instr *flow = rzalloc(shader, ibc_flow_instr);

   ibc_instr_init(&flow->instr, IBC_INSTR_TYPE_FLOW, 0, simd_width);

   flow->op = op;
   list_inithead(&flow->preds);

   return flow;
}

void
ibc_flow_instr_add_pred(struct ibc_flow_instr *flow,
                        struct ibc_flow_instr *pred_instr)
{
   ibc_flow_pred *pred = rzalloc(flow, ibc_flow_pred);
   pred->instr = pred_instr;
   list_addtail(&pred->link, &flow->preds);
}

ibc_shader *
ibc_shader_create(void *mem_ctx,
                  const struct gen_device_info *devinfo,
                  uint8_t simd_width)
{
   ibc_shader *shader = rzalloc(mem_ctx, ibc_shader);

   shader->devinfo = devinfo,
   shader->simd_width = simd_width;

   list_inithead(&shader->instrs);
   list_inithead(&shader->flow_instrs);
   list_inithead(&shader->regs);

   return shader;
}

static bool
link_write_cb(ibc_ref *ref,
              UNUSED int num_bytes,
              UNUSED int num_comps,
              UNUSED uint8_t simd_group,
              UNUSED uint8_t simd_width,
              void *_instr)
{
   ibc_ref_link_write(ref, _instr);
   return true;
}

void
ibc_instr_insert(ibc_instr *instr, ibc_cursor cursor)
{
   list_add(&instr->link, cursor.prev);
   ibc_instr_foreach_write(instr, link_write_cb, instr);
}

static bool
unlink_write_cb(ibc_ref *ref,
                UNUSED int num_bytes,
                UNUSED int num_comps,
                UNUSED uint8_t simd_group,
                UNUSED uint8_t simd_width,
                void *_instr)
{
   ibc_ref_unlink_write(ref, _instr);
   return true;
}

void
ibc_instr_remove(ibc_instr *instr)
{
   /* Flow instructions are complex to delete because of the way they're
    * linked together.  Just disallow control-flow manipulation for now.
    */
   assert(instr->type != IBC_INSTR_TYPE_FLOW);

   ibc_instr_foreach_write(instr, unlink_write_cb, instr);
   list_del(&instr->link);
}

#define OPT(pass)                            \
   do {                                      \
      if (pass(ibc)) {                       \
         if (print) {                        \
            fprintf(stderr, "%s\n", #pass);  \
            ibc_print_shader(ibc, stderr);   \
            fprintf(stderr, "\n\n");         \
         }                                   \
         ibc_validate_shader(ibc);           \
      }                                      \
   } while(0)

void
ibc_lower_and_optimize(ibc_shader *ibc, bool print)
{
   OPT(ibc_lower_simd_width);
   OPT(ibc_split_logical_regs);
   OPT(ibc_opt_copy_prop);
   OPT(ibc_opt_cse);
   OPT(ibc_opt_dead_code);
   OPT(ibc_lower_io_to_sends);
   OPT(ibc_opt_copy_prop);
   OPT(ibc_opt_dead_code);
   OPT(ibc_lower_gather_ops);
   OPT(ibc_assign_and_lower_flags);
   OPT(ibc_lower_simd_width);
   OPT(ibc_opt_copy_prop);
   OPT(ibc_opt_dead_code);
   OPT(ibc_lower_gather_ops);
   OPT(ibc_assign_regs);
}
