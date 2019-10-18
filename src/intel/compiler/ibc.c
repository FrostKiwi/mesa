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
#include "ibc_compile.h"

#include "dev/gen_debug.h"

void
ibc_init_compiler(struct brw_compiler *compiler)
{
   ibc_assign_regs_init(compiler);
}

static ibc_reg *
ibc_reg_create(ibc_shader *shader, enum ibc_file file)
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
   ibc_reg *reg = ibc_reg_create(shader, IBC_FILE_LOGICAL);

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
   ibc_reg *reg = ibc_reg_create(shader, IBC_FILE_HW_GRF);

   reg->is_wlr = true;
   reg->hw_grf.size = size;
   reg->hw_grf.align = align;

   return reg;
}

ibc_reg *
ibc_flag_reg_create(ibc_shader *shader, uint8_t bits)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_FILE_FLAG);

   reg->is_wlr = true;
   reg->flag.bits = bits;
   reg->flag.align_mul = MAX2(bits, 16);
   reg->flag.align_offset = 0;

   return reg;
}

ibc_reg *
ibc_accum_reg_create(ibc_shader *shader, enum ibc_type type, uint8_t channels)
{
   ibc_reg *reg = ibc_reg_create(shader, IBC_FILE_ACCUM);

   reg->is_wlr = true;
   reg->accum.type = type;
   reg->accum.channels = channels;
   reg->accum.align_mul = channels;
   reg->accum.align_offset = 0;

   return reg;
}

ibc_instr *
ibc_reg_ssa_instr(const ibc_reg *reg)
{
   if (!reg->is_wlr)
      return NULL;

   if (!list_is_singular(&reg->writes))
      return NULL;

   return LIST_ENTRY(ibc_reg_write, reg->writes.next, link)->instr;
}

static void
ibc_ref_init(ibc_ref *ref)
{
}

static bool
ibc_reg_write_link(ibc_reg_write *write, ibc_ref *ref, void *_instr)
{
   ibc_instr *instr = _instr;
   assert(write->instr == NULL);

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg) {
      write->instr = instr;
      list_addtail(&write->link, &((ibc_reg *)ref->reg)->writes);
   }

   return true;
}

static bool
ibc_reg_write_unlink(ibc_reg_write *write, UNUSED ibc_ref *ref, void *_instr)
{
   ibc_instr *instr = _instr;
   if (write->instr) {
      assert(write->instr == instr);
      assert(ref->file != IBC_FILE_NONE);
      assert(ref->file != IBC_FILE_IMM);
      assert(ref->reg != NULL);
      list_del(&write->link);
      write->instr = NULL;
   } else {
      assert(ref->reg == NULL);
      assert(write->instr == NULL);
   }
   return true;
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
   if (ref.file != IBC_FILE_LOGICAL)
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
      unsigned pred_simd_width = ibc_predicate_simd_width(instr->predicate);
      assert(util_is_power_of_two_nonzero(pred_simd_width));
      unsigned ref_simd_group = instr->simd_group & ~(pred_simd_width - 1);
      unsigned ref_simd_width = MAX2(instr->simd_width, pred_simd_width);

      if (!cb(&instr->flag, -1, 1, ref_simd_group, ref_simd_width, state))
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

      if ((ibc_alu_op_infos[alu->op].props & IBC_ALU_OP_PROP_READS_ACCUM) &&
          !cb(&alu->accum, -1, 1,
              instr->simd_group, instr->simd_width, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      if (send->desc.file != IBC_FILE_NONE &&
          !cb(&send->desc, sizeof(uint32_t), 1, 0, 1, state))
         return false;

      if (send->ex_desc.file != IBC_FILE_NONE &&
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

      if (alu->accum_wr_en && !cb(&alu->accum, -1, 1, instr->simd_group,
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
      if ((intrin->num_dest_comps > 0 || intrin->num_dest_bytes > 0) &&
          !cb(&intrin->dest, intrin->num_dest_bytes, intrin->num_dest_comps,
              instr->simd_group, instr->simd_width, state))
         return false;

      return true;
   }

   case IBC_INSTR_TYPE_FLOW:
      return true;
   }

   unreachable("Invalid IBC instruction type");
}

bool
ibc_instr_foreach_reg_write(ibc_instr *instr, ibc_reg_write_cb cb, void *_data)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      if (alu->cmod && !cb(&alu->cmod_write, &instr->flag, _data))
         return false;
      if (alu->accum_wr_en && !cb(&alu->accum_write, &alu->accum, _data))
         return false;
      if (!cb(&alu->dest_write, &alu->dest, _data))
         return false;
      return true;
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      return send->rlen == 0 ||
             cb(&send->dest_write, &send->dest, _data);
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      return intrin->num_dest_comps == 0 ||
              cb(&intrin->dest_write, &intrin->dest, _data);
   }

   case IBC_INSTR_TYPE_FLOW:
      return true;
   }

   unreachable("Invalid IBC instruction type");
}

ibc_ref *
ibc_reg_write_get_ref(ibc_reg_write *write)
{
   switch (write->instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(write->instr);
      if (write == &alu->cmod_write) {
         assert(alu->cmod != BRW_CONDITIONAL_NONE);
         return &alu->instr.flag;
      }
      assert(write == &alu->dest_write);
      return &alu->dest;
   }

   case IBC_INSTR_TYPE_SEND:
      assert(write == &ibc_instr_as_send(write->instr)->dest_write);
      return &ibc_instr_as_send(write->instr)->dest;

   case IBC_INSTR_TYPE_INTRINSIC:
      assert(write == &ibc_instr_as_intrinsic(write->instr)->dest_write);
      return &ibc_instr_as_intrinsic(write->instr)->dest;

   case IBC_INSTR_TYPE_FLOW:
      unreachable("Flow instructions never have writes");
   }

   unreachable("Invalid IBC instruction type");
}

static void
ibc_instr_set_write_ref(ibc_instr *instr, ibc_ref *ref,
                        ibc_reg_write *write, ibc_ref new_ref)
{
   ibc_reg_write_unlink(write, ref, instr);
   *ref = new_ref;
   ibc_reg_write_link(write, ref, instr);
}

struct set_write_ref_cb_data {
   ibc_instr *instr;
   ibc_ref *ref;
   const ibc_ref *new_ref;
};

static bool
set_write_ref_cb(ibc_reg_write *write, ibc_ref *ref, void *_data)
{
   struct set_write_ref_cb_data *data = _data;
   if (ref == data->ref) {
      ibc_instr_set_write_ref(data->instr, ref, write, *data->new_ref);
      return false;
   } else {
      return true;
   }
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
   if (instr == NULL) {
      *ref = new_ref;
      return;
   }

   struct set_write_ref_cb_data cbd = {
      .instr = instr,
      .ref = ref,
      .new_ref = &new_ref,
   };
   bool not_found = ibc_instr_foreach_reg_write(instr, set_write_ref_cb, &cbd);
   if (not_found) {
      assert(!ibc_instr_foreach_read(instr, refs_not_equal, ref));
      *ref = new_ref;
   }
}

void
ibc_instr_set_predicate(ibc_instr *instr, ibc_ref flag,
                        enum ibc_predicate predicate)
{
   /* We don't do any use tracking right now so we can just assign this */
   instr->flag = flag;
   instr->predicate = predicate;
}

void
ibc_alu_instr_set_cmod(ibc_alu_instr *alu, ibc_ref flag,
                       enum brw_conditional_mod cmod)
{
   alu->cmod = cmod;
   ibc_instr_set_write_ref(&alu->instr, &alu->instr.flag,
                           &alu->cmod_write, flag);
}

void
ibc_alu_instr_set_accum(ibc_alu_instr *alu, ibc_ref accum,
                        bool accum_wr_en)
{
   if (alu->accum_wr_en)
      ibc_reg_write_unlink(&alu->accum_write, &alu->accum, &alu->instr);
   alu->accum_wr_en = accum_wr_en;
   alu->accum = accum;
   if (alu->accum_wr_en)
      ibc_reg_write_link(&alu->accum_write, &alu->accum, &alu->instr);
}

#define IBC_ALU_OP_DECL(OP, _num_srcs, _src_mods, _props)\
   {                                                     \
      .name = #OP,                                       \
      .num_srcs = _num_srcs,                             \
      .supported_src_mods = IBC_ALU_SRC_MOD_##_src_mods, \
      .props = _props,                                   \
   },
#define READS_ACCUM IBC_ALU_OP_PROP_READS_ACCUM

const ibc_alu_op_info ibc_alu_op_infos[IBC_ALU_NUM_OPS] = {
#include "ibc_alu_ops.h"
};

#undef READS_ACCUM
#undef IBC_ALU_OP_DECL

static enum ibc_type
ibc_type_exec_type(enum ibc_type type)
{
   switch (type) {
   case IBC_TYPE_B:
   case IBC_TYPE_V:
      return IBC_TYPE_W;
   case IBC_TYPE_UB:
   case IBC_TYPE_UV:
      return IBC_TYPE_UW;
   case IBC_TYPE_VF:
      return IBC_TYPE_F;
   default:
      return type;
   }
}

enum ibc_type
ibc_alu_instr_exec_type(ibc_alu_instr *alu)
{
   enum ibc_type exec_type = IBC_TYPE_INVALID;
   for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
      enum ibc_type t = ibc_type_exec_type(alu->src[i].ref.type);
      if (ibc_type_bit_size(t) > ibc_type_bit_size(exec_type) ||
          (ibc_type_bit_size(t) == ibc_type_bit_size(exec_type) &&
           ibc_type_base_type(t) == IBC_TYPE_FLOAT))
         exec_type = t;
   }

   if (exec_type == IBC_TYPE_INVALID)
      exec_type = alu->dest.type;

   /* Promotion of the execution type to 32-bit for conversions from or to
    * half-float seems to be consistent with the following text from the
    * Cherryview PRM Vol. 7, "Execution Data Type":
    *
    * "When single precision and half precision floats are mixed between
    *  source operands or between source and destination operand [..] single
    *  precision float is the execution datatype."
    *
    * and from "Register Region Restrictions":
    *
    * "Conversion between Integer and HF (Half Float) must be DWord aligned
    *  and strided by a DWord on the destination."
    */
   if (ibc_type_bit_size(exec_type) == 16 &&
       alu->dest.type != exec_type) {
      if (exec_type == IBC_TYPE_HF)
         exec_type = IBC_TYPE_F;
      else if (alu->dest.type == IBC_TYPE_HF)
         exec_type = IBC_TYPE_D;
   }

   return exec_type;
}

ibc_alu_instr *
ibc_alu_instr_create(struct ibc_shader *shader, enum ibc_alu_op op,
                     uint8_t simd_group, uint8_t simd_width)
{
   const unsigned num_srcs = ibc_alu_op_infos[op].num_srcs;

   ibc_alu_instr *alu = rzalloc_size(shader, sizeof(ibc_alu_instr) +
                                             sizeof(ibc_alu_src) * num_srcs);

   ibc_instr_init(&alu->instr, IBC_INSTR_TYPE_ALU, simd_group, simd_width);

   alu->op = op;

   ibc_ref_init(&alu->accum);
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

   send->can_reorder = true;
   send->has_side_effects = false;

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
                  gl_shader_stage stage,
                  uint8_t simd_width)
{
   ibc_shader *shader = rzalloc(mem_ctx, ibc_shader);

   shader->devinfo = devinfo;
   shader->stage = stage;
   shader->simd_width = simd_width;

   list_inithead(&shader->instrs);
   list_inithead(&shader->flow_instrs);
   list_inithead(&shader->regs);

   return shader;
}

void
ibc_instr_insert(ibc_instr *instr, ibc_cursor cursor)
{
   list_add(&instr->link, cursor.prev);

   ibc_instr_foreach_reg_write(instr, ibc_reg_write_link, instr);
}

void
ibc_instr_remove(ibc_instr *instr)
{
   /* Flow instructions are complex to delete because of the way they're
    * linked together.  Just disallow control-flow manipulation for now.
    */
   assert(instr->type != IBC_INSTR_TYPE_FLOW);

   ibc_instr_foreach_reg_write(instr, ibc_reg_write_unlink, instr);

   list_del(&instr->link);
}

bool
ibc_should_print_shader(const ibc_shader *ibc)
{
   return INTEL_DEBUG & intel_debug_flag_for_shader_stage(ibc->stage);
}

static bool
repair_wlr_write_cb(ibc_reg_write *write, ibc_ref *ref,
                    UNUSED void *_state)
{
   assert(ref->file != IBC_FILE_IMM);
   if (ref->file == IBC_FILE_NONE || ref->reg == NULL)
      return true;

   assert(write->instr != NULL);
   list_del(&write->link);
   list_addtail(&write->link, &ref->reg->writes);

   return true;
}

void
ibc_repair_wlr_order(ibc_shader *shader)
{
   ibc_foreach_instr(instr, shader)
      ibc_instr_foreach_reg_write(instr, repair_wlr_write_cb, NULL);
}

static bool
ibc_optimize(ibc_shader *ibc)
{
   bool progress, any_progress = false;
   do {
      progress = false;

      IBC_PASS(progress, ibc, ibc_lower_simd_width);
      IBC_PASS(progress, ibc, ibc_split_regs);
      IBC_PASS(progress, ibc, ibc_opt_copy_prop);
      IBC_PASS(progress, ibc, ibc_opt_cse);
      IBC_PASS(progress, ibc, ibc_opt_dead_code);

      any_progress |= progress;
   } while(progress);

   return any_progress;
}

void
ibc_lower_and_optimize(ibc_shader *ibc)
{
   ibc_optimize(ibc);
   IBC_PASS_V(ibc, ibc_lower_io_to_sends);
   ibc_optimize(ibc);
   IBC_PASS_V(ibc, ibc_lower_gather_ops);
   ibc_optimize(ibc);
   IBC_PASS_V(ibc, ibc_schedule_instructions);
   IBC_PASS_V(ibc, ibc_lower_integer_multiplication);
   IBC_PASS_V(ibc, ibc_assign_and_lower_flags);
   ibc_optimize(ibc);
   IBC_PASS_V(ibc, ibc_lower_gather_ops);
   IBC_PASS_V(ibc, ibc_lower_overlapping_send_payloads);
   IBC_PASS_V(ibc, ibc_assign_and_lower_accums);
}
