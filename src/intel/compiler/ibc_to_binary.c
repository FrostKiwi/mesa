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
#include "brw_eu.h"

static enum brw_reg_type
brw_reg_type_for_ibc_type(enum ibc_type type)
{
   switch (type) {
   case IBC_TYPE_INVALID:
   case IBC_TYPE_INT:
   case IBC_TYPE_UINT:
   case IBC_TYPE_FLOAT:
   case IBC_TYPE_FLAG:
   case IBC_TYPE_8_BIT:
   case IBC_TYPE_16_BIT:
   case IBC_TYPE_32_BIT:
   case IBC_TYPE_64_BIT:
   case IBC_TYPE_VECTOR:
      unreachable("Partial IBC type");

   case IBC_TYPE_V:  return BRW_REGISTER_TYPE_V;
   case IBC_TYPE_UV: return BRW_REGISTER_TYPE_UV;
   case IBC_TYPE_VF: return BRW_REGISTER_TYPE_VF;
   case IBC_TYPE_B:  return BRW_REGISTER_TYPE_B;
   case IBC_TYPE_UB: return BRW_REGISTER_TYPE_UB;
   case IBC_TYPE_W:  return BRW_REGISTER_TYPE_W;
   case IBC_TYPE_UW: return BRW_REGISTER_TYPE_UW;
   case IBC_TYPE_HF: return BRW_REGISTER_TYPE_HF;
   case IBC_TYPE_D:  return BRW_REGISTER_TYPE_D;
   case IBC_TYPE_UD: return BRW_REGISTER_TYPE_UD;
   case IBC_TYPE_F:  return BRW_REGISTER_TYPE_F;
   case IBC_TYPE_Q:  return BRW_REGISTER_TYPE_Q;
   case IBC_TYPE_UQ: return BRW_REGISTER_TYPE_UQ;
   case IBC_TYPE_DF: return BRW_REGISTER_TYPE_DF;
   }

   unreachable("Invalid IBC type");
}

static struct brw_reg
brw_reg_for_ibc_reg_ref(const struct gen_device_info *devinfo,
                        const ibc_reg_ref *ref,
                        unsigned simd_width, bool compressed)
{
   /* Default the type to UINT if no base type is specified */
   enum ibc_type type = ref->type;
   if (type == IBC_TYPE_INVALID)
      type = IBC_TYPE_UD;
   else if (ibc_type_base_type(type) == IBC_TYPE_INVALID)
      type |= IBC_TYPE_UINT;

   switch (ref->file) {
   case IBC_REG_FILE_NONE:
      assert(ref->reg == NULL);
      return retype(brw_null_reg(), brw_reg_type_for_ibc_type(type));

   case IBC_REG_FILE_IMM:
      /* Immediates had better have a real type */
      assert(type == ref->type);

      switch (type) {
      case IBC_TYPE_INVALID:
      case IBC_TYPE_INT:
      case IBC_TYPE_UINT:
      case IBC_TYPE_FLOAT:
      case IBC_TYPE_FLAG:
      case IBC_TYPE_8_BIT:
      case IBC_TYPE_16_BIT:
      case IBC_TYPE_32_BIT:
      case IBC_TYPE_64_BIT:
      case IBC_TYPE_B:
      case IBC_TYPE_UB:
      case IBC_TYPE_VECTOR:
         unreachable("Invalid immediate types");

      case IBC_TYPE_W:
      case IBC_TYPE_UW:
      case IBC_TYPE_HF:
         return retype(brw_imm_uw(*(uint16_t *)ref->imm),
                       brw_reg_type_for_ibc_type(type));

      case IBC_TYPE_D:
      case IBC_TYPE_UD:
      case IBC_TYPE_F:
      case IBC_TYPE_V:
      case IBC_TYPE_UV:
      case IBC_TYPE_VF:
         return retype(brw_imm_ud(*(uint32_t *)ref->imm),
                       brw_reg_type_for_ibc_type(type));

      case IBC_TYPE_Q:
      case IBC_TYPE_UQ:
      case IBC_TYPE_DF:
         return retype(brw_imm_uq(*(uint64_t *)ref->imm),
                       brw_reg_type_for_ibc_type(type));
      }
      unreachable("Invalid IBC type");
      break;

   case IBC_REG_FILE_HW_GRF: {
      assert(ref->reg == NULL);
      unsigned nr = ref->hw_grf.byte / REG_SIZE;
      unsigned subnr = ref->hw_grf.byte % REG_SIZE;
      const unsigned elem_sz_B = ibc_type_byte_size(type);
      struct brw_reg brw_reg;
      if (ref->hw_grf.hstride == 0 && ref->hw_grf.vstride == 0) {
         brw_reg = brw_vec1_grf(nr, 0);
      } else if (ref->hw_grf.hstride * ref->hw_grf.width ==
                 ref->hw_grf.vstride ) {
         /* This is a regular stride where the split between vstride and
          * hstride doesn't matter.  We'll sanitize the stride so that it's
          * something that will work in the HW.
          */
         const unsigned stride_B = ref->hw_grf.hstride;
         assert(stride_B % elem_sz_B == 0);
         unsigned stride_elem = stride_B / elem_sz_B;

         /* From the Haswell PRM:
          *
          *  "VertStride must be used to cross GRF register boundaries. This
          *   rule implies that elements within a 'Width' cannot cross GRF
          *   boundaries."
          *
          * The maximum width value that could satisfy this restriction is:
          */
         const unsigned reg_width = REG_SIZE / stride_B;

         /* Because the hardware can only split source regions at a whole
          * multiple of width during decompression (i.e. vertically), clamp
          * the value obtained above to the physical execution size of a
          * single decompressed chunk of the instruction:
          */
         const unsigned phys_width = compressed ? simd_width / 2 : simd_width;

         /* XXX - The equation above is strictly speaking not correct on
          *       hardware that supports unbalanced GRF writes -- On Gen9+
          *       each decompressed chunk of the instruction may have a
          *       different execution size when the number of components
          *       written to each destination GRF is not the same.
          */
         if (stride_elem > 4) {
            assert(stride_B <= REG_SIZE);
            brw_reg = stride(brw_vecn_grf(1, nr, 0), stride_elem, 1, 0);
         } else {
            const unsigned width = MIN2(reg_width, phys_width);
            brw_reg = stride(brw_vecn_grf(width, nr, 0),
                             width * stride_elem, width, stride_elem);
         }
      } else {
         /* In this case, we just have to trust that whoever set up the
          * register region knows what they're doing.
          */
         brw_reg = stride(brw_vecn_grf(ref->hw_grf.width, nr, 0),
                          ref->hw_grf.vstride / elem_sz_B,
                          ref->hw_grf.width,
                          ref->hw_grf.hstride / elem_sz_B);
      }

      brw_reg = byte_offset(brw_reg, subnr);
      brw_reg = retype(brw_reg, brw_reg_type_for_ibc_type(type));

      return brw_reg;
   }

   case IBC_REG_FILE_FLAG: {
      assert(ref->reg == NULL);
      uint8_t subnr = ref->flag.bit / 16;
      return retype(brw_flag_reg(subnr / 2, subnr % 2),
                    brw_reg_type_for_ibc_type(type));
   }

   case IBC_REG_FILE_LOGICAL:
      unreachable("Logical registers should not exist in codegen");
   }

   unreachable("Unknown register file");
}

static void
generate_alu(struct brw_codegen *p, const ibc_alu_instr *alu)
{
   /* If the instruction writes to more than one register, it needs to
    * be explicitly marked as compressed on Gen <= 5.  On Gen >= 6 the
    * hardware figures out by itself what the right compression mode is,
    * but we still need to know whether the instruction is compressed to
    * set up the source register regions appropriately.
    *
    * XXX - This is wrong for instructions that write a single register
    *       but read more than one which should strictly speaking be
    *       treated as compressed.  For instructions that don't write
    *       any registers it relies on the destination being a null
    *       register of the correct type and regioning so the
    *       instruction is considered compressed or not accordingly.
    */
   bool compressed;
   if (alu->dest.file == IBC_REG_FILE_NONE) {
      /* TODO: Is this correct? */
      unsigned bytes_written = ibc_type_byte_size(alu->dest.type) *
                               alu->instr.simd_width;
      compressed = bytes_written > REG_SIZE;
   } else if (alu->dest.file == IBC_REG_FILE_FLAG) {
      assert(alu->instr.we_all && alu->instr.simd_width == 1);
      compressed = false;
   } else {
      assert(alu->dest.file == IBC_REG_FILE_HW_GRF);
      unsigned dest_byte = alu->dest.hw_grf.byte;
      unsigned bytes_written =
         alu->dest.hw_grf.hstride * (alu->instr.simd_width %
                                     alu->dest.hw_grf.width) +
         alu->dest.hw_grf.vstride * (alu->instr.simd_width /
                                     alu->dest.hw_grf.width);
      compressed = (dest_byte % REG_SIZE) + bytes_written > REG_SIZE;
   }

   struct brw_reg src[3], dest;
   assert(ibc_alu_op_infos[alu->op].num_srcs <= ARRAY_SIZE(src));
   for (unsigned int i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
      src[i] = brw_reg_for_ibc_reg_ref(p->devinfo, &alu->src[i].ref,
                                       alu->instr.simd_width,
                                       compressed);
      src[i].abs = (alu->src[i].mod & IBC_ALU_SRC_MOD_ABS) != 0;
      src[i].negate = (alu->src[i].mod & (IBC_ALU_SRC_MOD_NEG |
                                          IBC_ALU_SRC_MOD_NOT)) != 0;
   }
   dest = brw_reg_for_ibc_reg_ref(p->devinfo, &alu->dest,
                                  alu->instr.simd_width,
                                  compressed);

   brw_set_default_saturate(p, alu->saturate);
   brw_set_default_acc_write_control(p, false /* TODO */);

   const unsigned int last_insn_offset = p->next_insn_offset;

   switch (alu->op) {
   case IBC_ALU_OP_MOV:
      brw_MOV(p, dest, src[0]);
      break;

   case IBC_ALU_OP_SEL:
      brw_SEL(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_NOT:
      brw_NOT(p, dest, src[0]);
      break;

   case IBC_ALU_OP_AND:
      brw_AND(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_OR:
      brw_OR(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_SHR:
      brw_SHR(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_SHL:
      brw_SHL(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_CMP:
      brw_CMP(p, dest, alu->cmod, src[0], src[1]);
      break;

   case IBC_ALU_OP_ADD:
      brw_ADD(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_MUL:
      brw_MUL(p, dest, src[0], src[1]);
      break;

   case IBC_ALU_OP_MAD:
      if (p->devinfo->gen < 10)
         brw_set_default_access_mode(p, BRW_ALIGN_16);
      brw_MAD(p, dest, src[0], src[1], src[2]);
      break;

   case IBC_ALU_OP_RCP:
      gen6_math(p, dest, BRW_MATH_FUNCTION_INV, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_LOG2:
      gen6_math(p, dest, BRW_MATH_FUNCTION_LOG, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_EXP2:
      gen6_math(p, dest, BRW_MATH_FUNCTION_EXP, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_SQRT:
      gen6_math(p, dest, BRW_MATH_FUNCTION_SQRT, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_RSQ:
      gen6_math(p, dest, BRW_MATH_FUNCTION_RSQ, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_SIN:
      gen6_math(p, dest, BRW_MATH_FUNCTION_SIN, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_COS:
      gen6_math(p, dest, BRW_MATH_FUNCTION_COS, src[0], brw_null_reg());
      break;

   case IBC_ALU_OP_POW:
      gen6_math(p, dest, BRW_MATH_FUNCTION_POW, src[0], src[1]);
      break;

   default:
      unreachable("Invalid instruction");
   }

   if (alu->cmod) {
      assert(p->next_insn_offset == last_insn_offset + 16 ||
             !"conditional_mod set for IR emitting more than 1 "
              "instruction");
      brw_inst_set_cond_modifier(p->devinfo, brw_last_inst, alu->cmod);
   }
}

static void
generate_send(struct brw_codegen *p, const ibc_send_instr *send)
{
   struct brw_reg dst =
      brw_reg_for_ibc_reg_ref(p->devinfo, &send->dest,
                              send->instr.simd_width, false);

   struct brw_reg payload0 =
      brw_reg_for_ibc_reg_ref(p->devinfo, &send->payload[0],
                              send->instr.simd_width, false);
   struct brw_reg payload1 =
      brw_reg_for_ibc_reg_ref(p->devinfo, &send->payload[1],
                              send->instr.simd_width, false);

   struct brw_reg desc;
   if (send->desc.file == IBC_REG_FILE_NONE) {
      desc = brw_imm_ud(0);
   } else {
      assert(send->desc.type == IBC_TYPE_UD);
      desc = brw_reg_for_ibc_reg_ref(p->devinfo, &send->desc,
                                     send->instr.simd_width, false);
   }
   uint32_t desc_imm = send->desc_imm |
      brw_message_desc(p->devinfo, send->mlen, send->rlen, send->has_header);

   struct brw_reg ex_desc;
   if (send->ex_desc.file == IBC_REG_FILE_NONE) {
      ex_desc = brw_imm_ud(0);
   } else {
      assert(send->ex_desc.type == IBC_TYPE_UD);
      ex_desc = brw_reg_for_ibc_reg_ref(p->devinfo, &send->ex_desc,
                                        send->instr.simd_width, false);
   }
   uint32_t ex_desc_imm = send->ex_desc_imm |
      brw_message_ex_desc(p->devinfo, send->ex_mlen);

   if (ex_desc.file != BRW_IMMEDIATE_VALUE || ex_desc_imm) {
      /* If we have any sort of extended descriptor, then we need SENDS.  This
       * also covers the dual-payload case because ex_mlen goes in ex_desc.
       */
      brw_send_indirect_split_message(p, send->sfid, dst, payload0, payload1,
                                      desc, desc_imm, ex_desc, ex_desc_imm,
                                      send->eot);
      if (send->check_tdr)
         brw_inst_set_opcode(p->devinfo, brw_last_inst, BRW_OPCODE_SENDSC);
   } else {
      brw_send_indirect_message(p, send->sfid, dst, payload0, desc, desc_imm,
                                   send->eot);
      if (send->check_tdr)
         brw_inst_set_opcode(p->devinfo, brw_last_inst, BRW_OPCODE_SENDC);
   }
}

static void
generate_pln(struct brw_codegen *p, const ibc_intrinsic_instr *intrin,
             struct brw_reg dest,
             struct brw_reg interp, struct brw_reg bary_xy)
{
   assert(intrin->instr.simd_width == 8 ||
          intrin->instr.simd_width == 16);
   assert(intrin->num_srcs == 1 + (intrin->instr.simd_width / 8));

   /* The interpolant should be a scalar on an oword boundary.  The PLN
    * instruction reads it as a SIMD1 vec4.
    */
   assert(intrin->src[0].ref.file == IBC_REG_FILE_HW_GRF);
   assert(intrin->src[0].ref.reg == NULL);
   assert(intrin->src[0].ref.hw_grf.byte % 16 == 0);
   assert(intrin->src[0].ref.hw_grf.hstride == 0);

   /* The first barycentric source should be register-aligned */
   assert(intrin->src[1].ref.file == IBC_REG_FILE_HW_GRF);
   assert(intrin->src[1].ref.reg == NULL);
   assert(intrin->src[1].ref.hw_grf.byte % 32 == 0);
   assert(intrin->src[1].ref.hw_grf.hstride == 4);

   if (intrin->instr.simd_width > 8) {
      /* If we're a SIMD16 PLN instruction, the second source contains the
       * second SIMD8 vec2.  Even though we don't have to pass it into the
       * HW PLN instruction directly, we still verify that it's in the right
       * spot.  Register allocation and payload placement should take care of
       * this for us as long as we only use PLN for FS input interpolation.
       */
      assert(intrin->src[2].ref.file == IBC_REG_FILE_HW_GRF);
      assert(intrin->src[2].ref.reg == NULL);
      assert(intrin->src[2].ref.hw_grf.byte ==
             intrin->src[1].ref.hw_grf.byte + 64);
   }

   brw_PLN(p, dest, interp, bary_xy);
}

static void
generate_intrinsic(struct brw_codegen *p, const ibc_shader *shader,
                   const ibc_intrinsic_instr *intrin)
{
   /* We use a very simple heuristic for intrinsics */
   const bool compressed = intrin->instr.simd_width > 8;

   struct brw_reg src[3], dest;
   for (unsigned int i = 0; i < intrin->num_srcs; i++) {
      src[i] = brw_reg_for_ibc_reg_ref(p->devinfo, &intrin->src[i].ref,
                                       intrin->src[i].simd_width,
                                       compressed);
   }
   dest = brw_reg_for_ibc_reg_ref(p->devinfo, &intrin->dest,
                                  intrin->instr.simd_width,
                                  compressed);

   switch (intrin->op) {
   case IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL: {
      const struct brw_reg mask =
         shader->has_packed_dispatch ? brw_imm_ud(~0u) :
         shader->use_vmask ? brw_vmask_reg() : brw_dmask_reg();
      brw_find_live_channel(p, dest, mask);
      break;
   }

   case IBC_INTRINSIC_OP_SIMD_BROADCAST:
      assert(intrin->instr.simd_width == 1);
      assert(intrin->instr.we_all);
      brw_broadcast(p, dest, src[0], src[1]);
      break;

   case IBC_INTRINSIC_OP_PLN:
      generate_pln(p, intrin, dest, src[0], src[1]);
      break;

   default:
      unreachable("Intrinsic should have been lowered");
   }
}

static void
generate_flow(struct brw_codegen *p, const ibc_flow_instr *flow)
{
   switch (flow->op) {
   case IBC_FLOW_OP_START:
   case IBC_FLOW_OP_END:
      return; /* Nothing to do */

   case IBC_FLOW_OP_IF:
      brw_IF(p, brw_get_default_exec_size(p));
      return;

   case IBC_FLOW_OP_ELSE:
      brw_ELSE(p);
      return;

   case IBC_FLOW_OP_ENDIF:
      brw_ENDIF(p);
      return;

   case IBC_FLOW_OP_DO:
      brw_DO(p, brw_get_default_exec_size(p));
      return;

   case IBC_FLOW_OP_BREAK:
      brw_BREAK(p);
      return;

   case IBC_FLOW_OP_CONT:
      brw_CONT(p);
      return;

   case IBC_FLOW_OP_WHILE:
      brw_WHILE(p);
      return;
   }

   unreachable("Invalid branch op");
}

const unsigned *
ibc_to_binary(const ibc_shader *shader, void *mem_ctx, unsigned *program_size,
              bool print)
{
   const struct gen_device_info *devinfo = shader->devinfo;

   const unsigned start_offset = 0;
   struct brw_codegen *p = ralloc(mem_ctx, struct brw_codegen);
   brw_init_codegen(devinfo, p, mem_ctx);
   p->automatic_exec_sizes = false;

   struct disasm_info *disasm_info = disasm_initialize(devinfo, NULL);
   disasm_new_inst_group(disasm_info, 0);

   ibc_foreach_instr(instr, shader) {
      brw_set_default_access_mode(p, BRW_ALIGN_1);

      assert(instr->we_all || instr->simd_width >= 4);
      assert(instr->we_all || instr->simd_group % instr->simd_width == 0);
      brw_set_default_exec_size(p, cvt(instr->simd_width) - 1);
      brw_set_default_group(p, instr->simd_group);

      brw_set_default_predicate_control(p, instr->predicate);
      brw_set_default_predicate_inverse(p, instr->pred_inverse);
      if (instr->flag.file == IBC_REG_FILE_FLAG) {
         assert(instr->flag.reg == NULL);
         /* The hardware "helpfully" adds our simd_group to the subnr that we
          * provide so we need to decrement to account for it.
          */
         int subnr = (instr->flag.flag.bit - instr->simd_group) / 16;
         brw_set_default_flag_reg(p, subnr / 2, subnr % 2);
      } else {
         assert(instr->flag.file == IBC_REG_FILE_NONE);
         brw_set_default_flag_reg(p, 0, 0); /* TODO */
      }
      brw_set_default_mask_control(p, instr->we_all);

      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU:
         generate_alu(p, ibc_instr_as_alu(instr));
         continue;

      case IBC_INSTR_TYPE_SEND:
         generate_send(p, ibc_instr_as_send(instr));
         continue;

      case IBC_INSTR_TYPE_INTRINSIC:
         generate_intrinsic(p, shader, ibc_instr_as_intrinsic(instr));
         continue;

      case IBC_INSTR_TYPE_FLOW:
         generate_flow(p, ibc_instr_as_flow(instr));
         continue;
      }
      unreachable("Invalid instruction type");
   }

   brw_set_uip_jip(p, start_offset);

   disasm_new_inst_group(disasm_info, p->next_insn_offset);

   brw_validate_instructions(devinfo, p->store,
                             start_offset,
                             p->next_insn_offset,
                             disasm_info);

   brw_compact_instructions(p, start_offset, disasm_info);

   if (print) {
      dump_assembly(p->store, start_offset, p->next_insn_offset,
                    disasm_info, NULL);
   }

   ralloc_free(disasm_info);

   return brw_get_program(p, program_size);
}
