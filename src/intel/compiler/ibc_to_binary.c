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
#include "brw_compiler.h"
#include "brw_eu.h"

#include "dev/gen_debug.h"
#include "compiler/shader_info.h"
#include "nir.h"

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
brw_reg_for_ibc_ref(const struct gen_device_info *devinfo,
                    const ibc_ref *ref,
                    unsigned simd_width, bool compressed)
{
   /* Default the type to UINT if no base type is specified */
   enum ibc_type type = ref->type;
   if (type == IBC_TYPE_INVALID)
      type = IBC_TYPE_UD;
   else if (ibc_type_base_type(type) == IBC_TYPE_INVALID)
      type |= IBC_TYPE_UINT;

   switch (ref->file) {
   case IBC_FILE_NONE:
      assert(ref->reg == NULL);
      return retype(brw_null_reg(), brw_reg_type_for_ibc_type(type));

   case IBC_FILE_IMM:
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

   case IBC_FILE_HW_GRF: {
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

   case IBC_FILE_FLAG: {
      assert(ref->reg == NULL);
      uint8_t subnr = ref->flag.bit / 16;
      return retype(brw_flag_reg(subnr / 2, subnr % 2),
                    brw_reg_type_for_ibc_type(type));
   }

   case IBC_FILE_ACCUM: {
      assert(ref->reg == NULL);
      unsigned acc0_channels = 32 / ibc_type_byte_size(ref->type);
      uint8_t nr = ref->accum.chan / acc0_channels;
      uint8_t subnr = ref->accum.chan % acc0_channels;
      struct brw_reg brw_reg = brw_vecn_reg(8, BRW_ARCHITECTURE_REGISTER_FILE,
                                            BRW_ARF_ACCUMULATOR + nr, subnr);

      brw_reg = retype(brw_reg, brw_reg_type_for_ibc_type(type));
      if (simd_width == 1)
         brw_reg = stride(brw_reg, 0, 1, 0);

      return brw_reg;
   }

   case IBC_FILE_LOGICAL:
      unreachable("Logical registers should not exist in codegen");
   }

   unreachable("Unknown register file");
}

static void
set_byte_mask(unsigned byte, void *_data)
{
   uint64_t *mask = _data;
   *mask |= (1u << byte);
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
   if (alu->dest.file == IBC_FILE_NONE) {
      /* TODO: Is this correct? */
      unsigned bytes_written = ibc_type_byte_size(alu->dest.type) *
                               alu->instr.simd_width;
      compressed = bytes_written > REG_SIZE;
   } else if (alu->dest.file == IBC_FILE_FLAG) {
      assert(alu->instr.we_all && alu->instr.simd_width == 1);
      compressed = false;
   } else if (alu->dest.file == IBC_FILE_ACCUM) {
      /* TODO: Is this correct? */
      unsigned bytes_written = ibc_type_byte_size(alu->dest.type) *
                               alu->instr.simd_width;
      compressed = bytes_written > REG_SIZE;
   } else {
      assert(alu->dest.file == IBC_FILE_HW_GRF);
      ibc_ref dest_rebased = alu->dest;
      dest_rebased.hw_grf.byte = dest_rebased.hw_grf.byte % 32;
      uint64_t byte_mask = 0;
      ibc_hw_grf_ref_foreach_byte(dest_rebased, 1, alu->instr.simd_width,
                                  set_byte_mask, &byte_mask);
      compressed = (byte_mask >> 32) != 0;
   }

   struct brw_reg src[3], dest;
   assert(ibc_alu_op_infos[alu->op].num_srcs <= ARRAY_SIZE(src));
   for (unsigned int i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
      src[i] = brw_reg_for_ibc_ref(p->devinfo, &alu->src[i].ref,
                                   alu->instr.simd_width,
                                   compressed);
      src[i].abs = (alu->src[i].mod & IBC_ALU_SRC_MOD_ABS) != 0;
      src[i].negate = (alu->src[i].mod & (IBC_ALU_SRC_MOD_NEG |
                                          IBC_ALU_SRC_MOD_NOT)) != 0;
   }
   dest = brw_reg_for_ibc_ref(p->devinfo, &alu->dest,
                              alu->instr.simd_width,
                              compressed);

   brw_set_default_saturate(p, alu->saturate);
   brw_set_default_acc_write_control(p, alu->accum_wr_en);

   if (p->devinfo->gen < 10 && ibc_alu_op_infos[alu->op].num_srcs == 3) {
      /* On gen9 and earlier, we don't have 3-src ALIGN1 instructions so they
       * have to be implemented using ALIGN16.
       */
      brw_set_default_access_mode(p, BRW_ALIGN_16);
      if (alu->instr.simd_width == 1) {
         /* If we're SIMD1 things get interesting because we can't just use a
          * SIMD1 instruction and expect the destination to work properly.
          * Instead, we have to use a higher SIMD width and an ALIGN16
          * write-mask.
          */
         assert(type_sz(dest.type) >= 4);
         const unsigned width = 16 / type_sz(dest.type);
         brw_set_default_exec_size(p, cvt(width) - 1);
         const unsigned dest_comp =
            (dest.subnr % 16) / type_sz(dest.type);

         switch (type_sz(dest.type)) {
         case 2:
            unreachable("TODO");
            break;
         case 4:
            dest.writemask = 1 << dest_comp;
            dest.subnr = (dest.subnr / 16) * 16;
            break;
         case 8:
            /* Writemasks act on 32-bit components in ALIGN16 */
            dest.writemask = 3 << (dest_comp * 2);
            dest.subnr = (dest.subnr / 16) * 16;

            /* We have even more fun with DF instructions because <0;1,0>
             * regions turn into SWIZZLE_XXXX which replicates 32-bit
             * components, not 64-bit components.  In order for things to work
             * properly, we need to use SWIZZLE_XYXY or SWIZZLE_ZWZW.
             */
            for (unsigned i = 0; i < 3; i++) {
               assert(type_sz(src[i].type) == 8);
               const unsigned src_comp =  (src[i].subnr % 16) / 8;
               src[i].subnr = (src[i].subnr / 16) * 16;
               src[i] = vec8(src[i]);
               src[i].swizzle = src_comp == 0 ?
                                BRW_SWIZZLE_XYXY : BRW_SWIZZLE_ZWZW;
            }
            break;
         default:
            unreachable("Invalid type size for a 3-src instruction");
         }
      }
   }

   const unsigned int last_insn_offset = p->next_insn_offset;

#define UNOP_CASE(OP)            \
   case IBC_ALU_OP_##OP:         \
      brw_##OP(p, dest, src[0]); \
      break;

#define BINOP_CASE(OP)                    \
   case IBC_ALU_OP_##OP:                  \
      brw_##OP(p, dest, src[0], src[1]);  \
      break;

#define TRIOP_CASE(OP)                             \
   case IBC_ALU_OP_##OP:                           \
      brw_##OP(p, dest, src[0], src[1], src[2]);   \
      break;

#define UNOP_MATH_CASE(OP, MATH)                            \
   case IBC_ALU_OP_##OP:                                    \
      gen6_math(p, dest, BRW_MATH_FUNCTION_##MATH, src[0],  \
                retype(brw_null_reg(), src[0].type));       \
      break;

#define BINOP_MATH_CASE(OP, MATH)                                    \
   case IBC_ALU_OP_##OP:                                             \
      gen6_math(p, dest, BRW_MATH_FUNCTION_##MATH, src[0], src[1]);  \
      break;

   switch (alu->op) {
   UNOP_CASE(MOV)
   BINOP_CASE(SEL)
   UNOP_CASE(NOT)
   BINOP_CASE(AND)
   BINOP_CASE(OR)
   BINOP_CASE(XOR)
   BINOP_CASE(SHR)
   BINOP_CASE(SHL)
   BINOP_CASE(ROR)
   BINOP_CASE(ROL)

   case IBC_ALU_OP_CMP:
      brw_CMP(p, dest, alu->cmod, src[0], src[1]);
      break;

   case IBC_ALU_OP_IMUL:
   case IBC_ALU_OP_FMUL:
      brw_MUL(p, dest, src[0], src[1]);
      break;

   UNOP_CASE(BFREV)
   TRIOP_CASE(BFE)
   BINOP_CASE(BFI1)
   TRIOP_CASE(BFI2)

   BINOP_CASE(ADD)
   BINOP_CASE(AVG)
   UNOP_CASE(FRC)
   UNOP_CASE(RNDU)
   UNOP_CASE(RNDD)
   UNOP_CASE(RNDE)
   UNOP_CASE(RNDZ)
   BINOP_CASE(MACH)
   UNOP_CASE(LZD)
   UNOP_CASE(FBH)
   UNOP_CASE(FBL)
   UNOP_CASE(CBIT)
   TRIOP_CASE(MAD)
   TRIOP_CASE(LRP)

   UNOP_MATH_CASE(RCP, INV)
   UNOP_MATH_CASE(LOG2, LOG)
   UNOP_MATH_CASE(EXP2, EXP)
   UNOP_MATH_CASE(SQRT, SQRT)
   UNOP_MATH_CASE(RSQ, RSQ)
   UNOP_MATH_CASE(SIN, SIN)
   UNOP_MATH_CASE(COS, COS)

   BINOP_MATH_CASE(POW, POW);
   BINOP_MATH_CASE(IDIV, INT_DIV_QUOTIENT);
   BINOP_MATH_CASE(IREM, INT_DIV_REMAINDER);

   default:
      unreachable("Invalid instruction");
   }

#undef UNOP_CASE
#undef BINOP_CASE
#undef TRIOP_CASE
#undef UNOP_MATH_CASE

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
      brw_reg_for_ibc_ref(p->devinfo, &send->dest,
                          send->instr.simd_width, false);

   struct brw_reg payload0 =
      brw_reg_for_ibc_ref(p->devinfo, &send->payload[0],
                          send->instr.simd_width, false);
   struct brw_reg payload1 =
      brw_reg_for_ibc_ref(p->devinfo, &send->payload[1],
                          send->instr.simd_width, false);

   struct brw_reg desc;
   if (send->desc.file == IBC_FILE_NONE) {
      desc = brw_imm_ud(0);
   } else {
      assert(send->desc.type == IBC_TYPE_UD);
      desc = brw_reg_for_ibc_ref(p->devinfo, &send->desc,
                                 send->instr.simd_width, false);
   }
   uint32_t desc_imm = send->desc_imm |
      brw_message_desc(p->devinfo, send->mlen, send->rlen, send->has_header);

   struct brw_reg ex_desc;
   if (send->ex_desc.file == IBC_FILE_NONE) {
      ex_desc = brw_imm_ud(0);
   } else {
      assert(send->ex_desc.type == IBC_TYPE_UD);
      ex_desc = brw_reg_for_ibc_ref(p->devinfo, &send->ex_desc,
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
   assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
   assert(intrin->src[0].ref.reg == NULL);
   assert(intrin->src[0].ref.hw_grf.byte % 16 == 0);
   assert(intrin->src[0].ref.hw_grf.hstride == 0);

   /* The first barycentric source should be register-aligned */
   assert(intrin->src[1].ref.file == IBC_FILE_HW_GRF);
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
      assert(intrin->src[2].ref.file == IBC_FILE_HW_GRF);
      assert(intrin->src[2].ref.reg == NULL);
      assert(intrin->src[2].ref.hw_grf.byte ==
             intrin->src[1].ref.hw_grf.byte + 64);
   }

   brw_PLN(p, dest, interp, bary_xy);
}

static void
generate_mov_indirect(struct brw_codegen *p, const ibc_intrinsic_instr *intrin,
                      struct brw_reg dest, struct brw_reg val,
                      struct brw_reg offset, unsigned offset_mul)
{
   /* Because we're using the address register, we're limited to 16-wide
    * exection and 8-wide for 64-bit types.  We could try and make this
    * instruction splittable higher up in the compiler but that gets weird
    * because it reads all of the channels regardless of execution size.  It's
    * easier just to split it here.
    */
   const unsigned max_width = type_sz(val.type) > 4 ? 8 : 16;
   const unsigned lower_width = MIN2(max_width, intrin->instr.simd_width);

   const unsigned dest_stride = (1u << dest.hstride) >> 1;
   const unsigned offset_stride = (1u << offset.hstride) >> 1;

   brw_set_default_exec_size(p, cvt(lower_width) - 1);
   for (unsigned g = 0; g < intrin->instr.simd_width; g += lower_width) {
      brw_set_default_group(p, intrin->instr.simd_group + g);

      struct brw_reg gdest = suboffset(dest, g * dest_stride);

      if (offset_mul == 0 || offset.file == BRW_IMMEDIATE_VALUE) {
         /* Trivial, the source is already uniform or the index is a constant.
          * We will typically not get here if the optimizer is doing its job,
          * but asserting would be mean.
          */
         unsigned offset_B = offset.ud * offset_mul;
         brw_MOV(p, gdest, stride(byte_offset(val, offset_B), 0, 1, 0));
      } else {
         /* We use VxH indirect addressing, clobbering a0.0 through a0.7. */
         struct brw_reg addr = vec8(brw_address_reg(0));
         if (intrin->instr.simd_width == 1)
            addr = vec1(addr);

         struct brw_reg goffset = suboffset(offset, g * offset_stride);

         if (lower_width == 8 && goffset.width == BRW_WIDTH_16) {
            /* Things get grumpy if the register is too wide. */
            goffset.width--;
            goffset.vstride--;
         }

         assert(type_sz(goffset.type) <= 4);
         if (type_sz(goffset.type) == 4) {
            /* The destination stride of an instruction (in bytes) must be
             * greater than or equal to the size of the rest of the
             * instruction.  Since the address register is of type UW, we
             * can't use a D-type instruction.  In order to get around this,
             * re retype to UW and use a stride.
             */
            goffset = retype(spread(goffset, 2), BRW_REGISTER_TYPE_W);
         }

         /* Take into account the component size and horizontal stride. */
         if (offset_mul > 1) {
            brw_SHL(p, addr, goffset, brw_imm_uw(util_logbase2(offset_mul)));

            /* Add on the register start offset */
            brw_ADD(p, addr, addr, brw_imm_uw(val.nr * REG_SIZE + val.subnr));
         } else {
            assert(offset_mul == 1);
            /* Add on the register start offset */
            brw_ADD(p, addr, goffset,
                    brw_imm_uw(val.nr * REG_SIZE + val.subnr));
         }

         if (type_sz(val.type) > 4 &&
             (p->devinfo->is_cherryview ||
              gen_device_info_is_9lp(p->devinfo))) {
            /* From the Cherryview PRM Vol 7. "Register Region Restrictions":
             *
             *    "When source or destination datatype is 64b or operation is
             *    integer DWord multiply, indirect addressing must not be
             *    used."
             *
             * To work around both of these, we do two integer MOVs insead of
             * one 64-bit MOV.  Because no double value should ever cross a
             * register boundary, it's safe to use the immediate offset in the
             * indirect here to handle adding 4 bytes to the offset and avoid
             * the extra ADD to the register file.
             */
            struct brw_reg dest_d = retype(spread(gdest, 2),
                                           BRW_REGISTER_TYPE_D);
            assert(dest.hstride == 1);
            brw_MOV(p, dest_d,
                    retype(brw_VxH_indirect(0, 0), BRW_REGISTER_TYPE_D));
            brw_MOV(p, byte_offset(dest_d, 4),
                    retype(brw_VxH_indirect(0, 4), BRW_REGISTER_TYPE_D));
         } else {
            brw_MOV(p, gdest, retype(brw_VxH_indirect(0, 0), val.type));
         }
      }
   }

}

static void
generate_intrinsic(struct brw_codegen *p, const ibc_shader *shader,
                   const ibc_intrinsic_instr *intrin)
{
   /* We use a very simple heuristic for intrinsics */
   const bool compressed = intrin->instr.simd_width > 8;

   struct brw_reg src[3], dest;
   for (unsigned int i = 0; i < intrin->num_srcs; i++) {
      src[i] = brw_reg_for_ibc_ref(p->devinfo, &intrin->src[i].ref,
                                   intrin->src[i].simd_width,
                                   compressed);
   }
   dest = brw_reg_for_ibc_ref(p->devinfo, &intrin->dest,
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

   case IBC_INTRINSIC_OP_SIMD_SHUFFLE:
      assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
      assert(intrin->src[0].ref.hw_grf.hstride *
             intrin->src[0].ref.hw_grf.width ==
             intrin->src[0].ref.hw_grf.vstride);
      generate_mov_indirect(p, intrin, dest, src[0], src[1],
                            intrin->src[0].ref.hw_grf.hstride);
      break;

   case IBC_INTRINSIC_OP_MOV_INDIRECT:
      assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
      generate_mov_indirect(p, intrin, dest, src[0], src[1], 1);
      break;

   case IBC_INTRINSIC_OP_PLN:
      generate_pln(p, intrin, dest, src[0], src[1]);
      break;

   case IBC_INTRINSIC_OP_ALIGN16_DDX_FINE: {
      assert(src[0].type == BRW_REGISTER_TYPE_F);
      assert(dest.type == BRW_REGISTER_TYPE_F);

      struct brw_reg src0 = stride(src[0], 4, 4, 1);
      struct brw_reg src1 = stride(src[0], 4, 4, 1);
      src0.swizzle = BRW_SWIZZLE_XYXY;
      src1.swizzle = BRW_SWIZZLE_ZWZW;

      brw_push_insn_state(p);
      brw_set_default_access_mode(p, BRW_ALIGN_16);
      brw_ADD(p, dest, negate(src0), src1);
      brw_pop_insn_state(p);
      break;
   }

   case IBC_INTRINSIC_OP_WAIT:
      brw_WAIT(p);
      break;

   case IBC_INTRINSIC_OP_STALL_REG:
      brw_MOV(p, retype(brw_null_reg(), src[0].type), src[0]);
      break;

   case IBC_INTRINSIC_OP_FLOAT_CONTROL_MODE:
      assert(src[0].file == BRW_IMMEDIATE_VALUE);
      assert(src[1].file == BRW_IMMEDIATE_VALUE);
      brw_float_controls_mode(p, src[0].d, src[1].d);
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

   case IBC_FLOW_OP_HALT_JUMP:
      brw_HALT(p);
      return;

   case IBC_FLOW_OP_HALT_MERGE:
      if (!list_is_singular(&flow->preds)) {
         int scale = brw_jump_scale(p->devinfo);

         /* There is a somewhat strange undocumented requirement of using
          * HALT, according to the simulator.  If some channel has HALTed to
          * a particular UIP, then by the end of the program, every channel
          * must have HALTed to that UIP.  Furthermore, the tracking is a
          * stack, so you can't do the final halt of a UIP after starting
          * halting to a new UIP.
          *
          * Symptoms of not emitting this instruction on actual hardware
          * included GPU hangs and sparkly rendering on the piglit discard
          * tests.
          */
         brw_inst *merge = brw_HALT(p);
         brw_inst_set_uip(p->devinfo, merge, 1 * scale);
         brw_inst_set_jip(p->devinfo, merge, 1 * scale);

         int merge_ip = p->nr_insn;

         ibc_foreach_flow_pred(pred, flow) {
            /* FLOW_MERGE instructions are their own predecessors */
            if (pred->instr == flow)
               continue;

            assert(pred->instr->op == IBC_FLOW_OP_HALT_JUMP);
            int jump_ip = pred->instr->instr.index;
            brw_inst *jump = &p->store[jump_ip];
            assert(brw_inst_opcode(p->devinfo, jump) == BRW_OPCODE_HALT);

            /* HALT takes a half-instruction distance from the
             * pre-incremented IP.
             */
            brw_inst_set_uip(p->devinfo, jump, (merge_ip - jump_ip) * scale);
         }
      } else {
         ibc_foreach_flow_pred(pred, flow)
            assert(pred->instr == flow);
      }
      return;
   }

   unreachable("Invalid branch op");
}

const unsigned *
ibc_to_binary(ibc_shader *shader, const shader_info *info,
              const struct brw_compiler *compiler, void *log_data,
              void *mem_ctx, unsigned *program_size)
{
   const struct gen_device_info *devinfo = shader->devinfo;

   const unsigned start_offset = 0;
   struct brw_codegen *p = ralloc(mem_ctx, struct brw_codegen);
   brw_init_codegen(devinfo, p, mem_ctx);
   p->automatic_exec_sizes = false;

   struct disasm_info *disasm_info = disasm_initialize(devinfo, NULL);
   disasm_new_inst_group(disasm_info, 0);

   unsigned nop_count = 0; /* TODO: Gen8-9 POW/FDIV workaround */

   ibc_foreach_instr(instr, shader) {
      brw_push_insn_state(p);

      brw_set_default_access_mode(p, BRW_ALIGN_1);

      assert(instr->we_all || instr->simd_width >= 4);
      assert(instr->we_all || instr->simd_group % instr->simd_width == 0);
      brw_set_default_exec_size(p, cvt(instr->simd_width) - 1);
      brw_set_default_group(p, instr->simd_group);

      brw_set_default_predicate_control(p, ibc_predicate_control(instr->predicate));
      brw_set_default_predicate_inverse(p, ibc_predicate_is_inverted(instr->predicate));
      if (instr->flag.file == IBC_FILE_FLAG) {
         assert(instr->flag.reg == NULL);
         /* The hardware "helpfully" adds our simd_group to the subnr that we
          * provide so we need to decrement to account for it.
          */
         int subnr = (instr->flag.flag.bit - instr->simd_group) / 16;
         brw_set_default_flag_reg(p, subnr / 2, subnr % 2);
      } else {
         assert(instr->flag.file == IBC_FILE_NONE);
         brw_set_default_flag_reg(p, 0, 0); /* TODO */
      }
      brw_set_default_mask_control(p, instr->we_all);

      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU:
         generate_alu(p, ibc_instr_as_alu(instr));
         break;

      case IBC_INSTR_TYPE_SEND:
         generate_send(p, ibc_instr_as_send(instr));
         shader->stats.sends++;
         break;

      case IBC_INSTR_TYPE_INTRINSIC:
         generate_intrinsic(p, shader, ibc_instr_as_intrinsic(instr));
         break;

      case IBC_INSTR_TYPE_FLOW:
         generate_flow(p, ibc_instr_as_flow(instr));
         if (ibc_instr_as_flow(instr)->op == IBC_FLOW_OP_WHILE)
            shader->stats.loops++;
         break;

      default:
         unreachable("Invalid instruction type");
      }

      /* Stash the physical IP of the last instruction in the index */
      instr->index = p->nr_insn - 1;

      brw_pop_insn_state(p);
   }

   brw_set_uip_jip(p, start_offset);

   disasm_new_inst_group(disasm_info, p->next_insn_offset);

   ASSERTED bool validated = brw_validate_instructions(devinfo, p->store,
                                                       start_offset,
                                                       p->next_insn_offset,
                                                       disasm_info);

   unsigned before_size = p->next_insn_offset - start_offset;
   brw_compact_instructions(p, start_offset, disasm_info);

   unsigned after_size = p->next_insn_offset - start_offset;

   shader->stats.instructions = before_size / 16 - nop_count;
   shader->stats.cycles = shader->cycles;
   shader->stats.sends -= shader->stats.spills + shader->stats.fills;

   if (INTEL_DEBUG & intel_debug_flag_for_shader_stage(shader->stage)) {
      fprintf(stderr,
              "Native code for %s %s shader %s\n"
              "SIMD%d shader: %u instructions. %u loops. %u cycles. "
              "%u:%u spills:fills. %u sends. "
              "Compacted %u to %u bytes (%.0f%%)\n",
               info->label ? info->label : "unnamed",
               _mesa_shader_stage_to_string(shader->stage), info->name,
              shader->simd_width, shader->stats.instructions,
              shader->stats.loops, shader->cycles,
              shader->stats.spills, shader->stats.fills,
              shader->stats.sends,
              before_size, after_size,
              100.0f * (before_size - after_size) / before_size);
      dump_assembly(p->store, start_offset, p->next_insn_offset,
                    disasm_info, NULL);
   }

   assert(validated);

   compiler->shader_debug_log(log_data,
                              "%s SIMD%d shader: %d inst, %d loops, %u cycles, "
                              "%d:%d spills:fills, %u sends, "
                              "scheduled with mode %s, "
                              "Promoted %u constants, "
                              "compacted %d to %d bytes.",
                              _mesa_shader_stage_to_abbrev(shader->stage),
                              shader->simd_width, shader->stats.instructions,
                              shader->stats.loops, shader->cycles,
                              shader->stats.spills, shader->stats.fills,
                              shader->stats.sends,
                              "unknown",
                              0,
                              before_size, after_size);

   ralloc_free(disasm_info);

   return brw_get_program(p, program_size);
}

const void *
ibc_append_nir_constant_data(const struct nir_shader *nir,
                             const void *assembly,
                             struct brw_stage_prog_data *prog_data)
{
   if (nir->constant_data_size == 0)
      return assembly;

   void *mem_ctx = ralloc_parent(assembly);
   void *combined =
      ralloc_size(mem_ctx, prog_data->program_size + nir->constant_data_size);

   memcpy(combined, assembly, prog_data->program_size);
   memcpy(combined + prog_data->program_size,
          nir->constant_data, nir->constant_data_size);

   prog_data->const_data_offset = prog_data->program_size;
   prog_data->const_data_size = nir->constant_data_size;
   prog_data->program_size += nir->constant_data_size;

   return combined;
}
