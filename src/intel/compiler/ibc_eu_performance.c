/*
 * Copyright © 2020 Intel Corporation
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

#include "gen_eu_performance.h"
#include "ibc.h"

#undef OP
#undef ALU
#define OP(type, op) (type << 29 | op)
#define ALU(x)    OP(IBC_INSTR_TYPE_ALU, IBC_ALU_OP_##x)
#define FLOW(x)   OP(IBC_INSTR_TYPE_FLOW, IBC_FLOW_OP_##x)
#define INTRIN(x) OP(IBC_INSTR_TYPE_INTRINSIC, IBC_INTRINSIC_OP_##x)
#define OP_SEND   OP(IBC_INSTR_TYPE_SEND, 0)

/**
 * Compute the timing information of an instruction based on any relevant
 * information from the IR and a number of linear approximation parameters
 * hard-coded for each IR instruction.
 *
 * Most timing parameters are obtained from the multivariate linear
 * regression of a sample of empirical timings measured using the tm0
 * register (as can be done today by using the shader_time debugging
 * option).  The Gen4-5 math timings are obtained from BSpec Volume 5c.3
 * "Shared Functions - Extended Math", Section 3.2 "Performance".
 * Parameters marked XXX shall be considered low-quality, they're possibly
 * high variance or completely guessed in cases where experimental data was
 * unavailable.
 */
static struct gen_eu_perf_desc
instruction_desc(const struct gen_eu_instruction_info *info)
{
   const struct gen_device_info *devinfo = info->devinfo;

   switch ((unsigned) info->op) {
   //case ALU(SYNC):
   case ALU(SEL):
   case ALU(NOT):
   case ALU(AND):
   case ALU(OR):
   case ALU(XOR):
   case ALU(SHR):
   case ALU(SHL):
   //case ALU(ASR):
   //case ALU(CMPN):
   case ALU(BFREV):
   case ALU(BFI1):
   case ALU(AVG):
   case ALU(FRC):
   case ALU(RNDU):
   case ALU(RNDD):
   case ALU(RNDE):
   case ALU(RNDZ):
   //case ALU(MAC):
   case ALU(MACH):
   case ALU(LZD):
   case ALU(FBH):
   case ALU(FBL):
   case ALU(CBIT):
   //case ALU(ADDC):
   case ALU(ROR):
   case ALU(ROL):
   //case ALU(SUBB):
   //case ALU(SAD2):
   //case ALU(SADA2):
   //case ALU(NOP):
   case INTRIN(ALIGN16_DDX_FINE):
   case INTRIN(STALL_REG): /* XXX: maybe wrong for STALL_REG */
      if (devinfo->gen >= 11) {
         return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                               0, 10, 6 /* XXX */, 14, 0, 0);
      } else {
         if (ibc_type_bit_size(info->tx) > 32)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 8, 4, 12, 0, 0);
      }

   case ALU(MOV):
   case ALU(CMP):
   case ALU(ADD):
   case ALU(FMUL):
   case ALU(IMUL):
      if (devinfo->gen >= 11) {
         return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                               0, 10, 6, 14, 0, 0);
      } else {
         if (ibc_type_bit_size(info->tx) > 32)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 8, 4, 12, 0, 0);
      }

   case ALU(BFE):
   case ALU(BFI2):
   //case ALU(CSEL):
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                               0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                               0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);

   case ALU(MAD):
      if (devinfo->gen >= 11) {
         return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                               0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
      } else {
         if (ibc_type_bit_size(info->tx) > 32)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 1, 0, 4,
                                  0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
      }

   case ALU(RCP):
   case ALU(RSQ):
   case ALU(SQRT):
   case ALU(EXP2):
   case ALU(LOG2):
   case ALU(SIN):
   case ALU(COS):
      return calculate_desc(info, GEN_UNIT_EM, -2, 4, 0, 0, 4,
                            0, 16, 0, 0, 0, 0);
   case ALU(POW):
      return calculate_desc(info, GEN_UNIT_EM, -2, 4, 0, 0, 8,
                            0, 24, 0, 0, 0, 0);
   case ALU(IDIV):
   case ALU(IREM):
      return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 26, 0,
                            0, 28 /* XXX */, 0, 0, 0, 0);

   case FLOW(IF):
   case FLOW(ELSE):
   case FLOW(ENDIF):
   case FLOW(DO):
   case FLOW(BREAK):
   case FLOW(CONT):
   case FLOW(WHILE):
   case FLOW(HALT_JUMP):
      return calculate_desc(info, GEN_UNIT_NULL, 8, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0);

   case ALU(LRP):
      return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 1, 0, 4,
                            0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);

   case INTRIN(PLN):
      return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                            0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);

   case INTRIN(MOV_INDIRECT):
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 34, 0, 0, 34, 0,
                               0, 10 /* XXX */, 6 /* XXX */,
                               14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 34, 0, 0, 34, 0,
                               0, 8 /* XXX */, 4 /* XXX */,
                               12 /* XXX */, 0, 0);

#if 0
   case SHADER_OPCODE_BROADCAST:
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 20 /* XXX */, 0, 0, 4, 0,
                               0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 18, 0, 0, 4, 0,
                               0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
#endif

   case INTRIN(FIND_LIVE_CHANNEL):
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 2, 0, 0, 2, 0,
                               0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 2, 0, 0, 2, 0,
                               0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);

   case INTRIN(FLOAT_CONTROL_MODE):
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 24 /* XXX */, 0, 0,
                               4 /* XXX */, 0,
                               0, 0, 0, 0, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 20 /* XXX */, 0, 0,
                               4 /* XXX */, 0,
                               0, 0, 0, 0, 0, 0);

   case INTRIN(SIMD_SHUFFLE):
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 44 /* XXX */, 0, 0,
                               44 /* XXX */, 0,
                               0, 10 /* XXX */, 6 /* XXX */,
                               14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 42 /* XXX */, 0, 0,
                               42 /* XXX */, 0,
                               0, 8 /* XXX */, 4 /* XXX */,
                               12 /* XXX */, 0, 0);

#if 0
   case SHADER_OPCODE_SEL_EXEC:
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 10 /* XXX */, 4 /* XXX */, 0,
                               0, 4 /* XXX */,
                               0, 10 /* XXX */, 6 /* XXX */,
                               14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 8 /* XXX */, 4 /* XXX */, 0,
                               0, 4 /* XXX */,
                               0, 8 /* XXX */, 4 /* XXX */,
                               12 /* XXX */, 0, 0);

   case SHADER_OPCODE_QUAD_SWIZZLE:
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 0 /* XXX */, 8 /* XXX */, 0,
                               0, 8 /* XXX */,
                               0, 10 /* XXX */, 6 /* XXX */,
                               14 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 0 /* XXX */, 8 /* XXX */, 0,
                               0, 8 /* XXX */,
                               0, 8 /* XXX */, 4 /* XXX */,
                               12 /* XXX */, 0, 0);

   case FS_OPCODE_LOAD_LIVE_CHANNELS:
      if (devinfo->gen >= 11)
         return calculate_desc(info, GEN_UNIT_FPU, 2 /* XXX */, 0, 0,
                               2 /* XXX */, 0,
                               0, 0, 0, 10 /* XXX */, 0, 0);
      else
         return calculate_desc(info, GEN_UNIT_FPU, 0, 2 /* XXX */, 0,
                               0, 2 /* XXX */,
                               0, 0, 0, 8 /* XXX */, 0, 0);
#endif

   case INTRIN(TEX):
   case INTRIN(TXB):
   case INTRIN(TXL):
   case INTRIN(TXD):
   case INTRIN(TXF):
   case INTRIN(TXF_MS):
   case INTRIN(TXF_MCS):
   case INTRIN(TXS):
   case INTRIN(LOD):
   case INTRIN(TG4):
   case INTRIN(TG4_OFFSET):
   case INTRIN(SAMPLEINFO):
      return calculate_desc(info, GEN_UNIT_SAMPLER, 2, 0, 0, 0, 16 /* XXX */,
                            8 /* XXX */, 750 /* XXX */, 0, 0,
                            2 /* XXX */, 0);

   case INTRIN(URB_READ):
   case INTRIN(URB_WRITE):
      return calculate_desc(info, GEN_UNIT_URB, 2, 0, 0, 0, 6 /* XXX */,
                            32 /* XXX */, 200 /* XXX */, 0, 0, 0, 0);

   case INTRIN(FB_WRITE):
      return calculate_desc(info, GEN_UNIT_DP_RC, 2, 0, 0, 0, 450 /* XXX */,
                            10 /* XXX */, 300 /* XXX */, 0, 0, 0, 0);

#if 0
   case SHADER_OPCODE_MEMORY_FENCE:
   case SHADER_OPCODE_INTERLOCK:
      switch (info->sfid) {
      case GEN6_SFID_DATAPORT_RENDER_CACHE:
         return calculate_desc(info, GEN_UNIT_DP_RC, 2, 0, 0, 30 /* XXX */, 0,
                               10 /* XXX */, 300 /* XXX */, 0, 0, 0, 0);

      case GEN7_SFID_DATAPORT_DATA_CACHE:
      case HSW_SFID_DATAPORT_DATA_CACHE_1:
         return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0, 30 /* XXX */, 0,
                               10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);

      default:
         abort();
      }

   case SHADER_OPCODE_GEN4_SCRATCH_READ:
   case SHADER_OPCODE_GEN4_SCRATCH_WRITE:
   case SHADER_OPCODE_GEN7_SCRATCH_READ:
      return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0, 0, 8 /* XXX */,
                            10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);

   case FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD:
   case FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD_GEN7:
      return calculate_desc(info, GEN_UNIT_DP_CC, 2, 0, 0, 0, 16 /* XXX */,
                            10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);

   case FS_OPCODE_INTERPOLATE_AT_SAMPLE:
   case FS_OPCODE_INTERPOLATE_AT_SHARED_OFFSET:
   case FS_OPCODE_INTERPOLATE_AT_PER_SLOT_OFFSET:
      return calculate_desc(info, GEN_UNIT_PI, 2, 0, 0, 14 /* XXX */, 0,
                            0, 90 /* XXX */, 0, 0, 0, 0);

   case CS_OPCODE_CS_TERMINATE:
      return calculate_desc(info, GEN_UNIT_SPAWNER, 2, 0, 0, 0 /* XXX */, 0,
                            10 /* XXX */, 0, 0, 0, 0, 0);
#endif

   case OP_SEND:
      switch (info->sfid) {
      case BRW_SFID_URB:
         return calculate_desc(info, GEN_UNIT_URB, 2, 0, 0, 0, 6 /* XXX */,
                               32 /* XXX */, 200 /* XXX */, 0, 0, 0, 0);

      case BRW_SFID_THREAD_SPAWNER:
         /* Assume it's the compute shader EOT message */
         assert(info->desc == brw_ts_eot_desc(devinfo));
         return calculate_desc(info, GEN_UNIT_SPAWNER, 2, 0, 0, 0 /* XXX */, 0,
                               10 /* XXX */, 0, 0, 0, 0, 0);

      case BRW_SFID_MESSAGE_GATEWAY:
         return calculate_desc(info, GEN_UNIT_GATEWAY, 90 /* XXX */, 0, 0,
                               0 /* XXX */, 0, 0, 0, 0, 0, 0, 0);

      case GEN6_SFID_DATAPORT_CONSTANT_CACHE:
         return calculate_desc(info, GEN_UNIT_DP_CC, 2, 0, 0, 0, 16 /* XXX */,
                               10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);

      case GEN6_SFID_DATAPORT_RENDER_CACHE:
         switch (brw_dp_desc_msg_type(devinfo, info->desc)) {
         case GEN7_DATAPORT_RC_TYPED_ATOMIC_OP:
            return calculate_desc(info, GEN_UNIT_DP_RC, 2, 0, 0,
                                  30 /* XXX */, 450 /* XXX */,
                                  10 /* XXX */, 100 /* XXX */,
                                  0, 0, 0, 400 /* XXX */);
         default:
            return calculate_desc(info, GEN_UNIT_DP_RC, 2, 0, 0,
                                  0, 450 /* XXX */,
                                  10 /* XXX */, 300 /* XXX */, 0, 0,
                                  0, 0);
         }
      case BRW_SFID_SAMPLER:
         return calculate_desc(info, GEN_UNIT_SAMPLER, 2, 0, 0, 0, 16,
                               8, 750, 0, 0, 2, 0);

      case GEN7_SFID_DATAPORT_DATA_CACHE:
      case HSW_SFID_DATAPORT_DATA_CACHE_1:
         switch (brw_dp_desc_msg_type(devinfo, info->desc)) {
#if 0
         case GEN7_DATAPORT_DC_MEMORY_FENCE:
            return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0, 30 /* XXX */,
                                  0, 10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);
#endif
         case HSW_DATAPORT_DC_PORT1_UNTYPED_ATOMIC_OP:
         case HSW_DATAPORT_DC_PORT1_UNTYPED_ATOMIC_OP_SIMD4X2:
         case HSW_DATAPORT_DC_PORT1_TYPED_ATOMIC_OP_SIMD4X2:
         case HSW_DATAPORT_DC_PORT1_TYPED_ATOMIC_OP:
            return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0,
                                  30 /* XXX */, 400 /* XXX */,
                                  10 /* XXX */, 100 /* XXX */, 0, 0,
                                  0, 400 /* XXX */);

         default:
            return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0,
                                  0, 20 /* XXX */,
                                  10 /* XXX */, 100 /* XXX */, 0, 0,
                                  0, 0);
         }

      case GEN7_SFID_PIXEL_INTERPOLATOR:
         return calculate_desc(info, GEN_UNIT_PI, 2, 0, 0, 14 /* XXX */, 0,
                               0, 90 /* XXX */, 0, 0, 0, 0);

      default:
         abort();
      }

   case FLOW(START):
   case FLOW(END):
   case FLOW(HALT_MERGE):
   case INTRIN(UNDEF):
   case INTRIN(LOAD_PAYLOAD):
   case INTRIN(WAIT): /* XXX: wrong for WAIT */
      return calculate_desc(info, GEN_UNIT_NULL, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0);

   default:
      abort();
   }
}

struct byte_range_cb_state {
   unsigned min;
   unsigned max;
};

static void
byte_range_cb(unsigned byte, void *data)
{
   struct byte_range_cb_state *state = data;
   state->min = MIN2(state->min, byte);
   state->max = MAX2(state->max, byte + 1);
}

void
ibc_calc_hw_grf_range(ibc_ref *ref,
                      int num_bytes, int num_comps, uint8_t simd_width,
                      unsigned *min, unsigned *max)
{
   assert(ref->file == IBC_FILE_HW_GRF);

   if (num_comps < 0) {
      assert(num_bytes >= 0);
      *min = ref->hw_grf.byte / REG_SIZE;
      *max = DIV_ROUND_UP(ref->hw_grf.byte + num_bytes, REG_SIZE);
   } else {
      struct byte_range_cb_state t = { ~0, 0 };
      ibc_hw_grf_ref_foreach_byte(*ref, num_comps, simd_width, byte_range_cb, &t);
      *min = t.min / REG_SIZE;
      *max = DIV_ROUND_UP(t.max, REG_SIZE);
   }
}

static unsigned
hw_grf_count(ibc_ref *ref, int num_bytes, int num_comps, uint8_t simd_width)
{
   unsigned min, max;
   ibc_calc_hw_grf_range(ref, num_bytes, num_comps, simd_width, &min, &max);
   return max - min;
}

static ibc_ref
dest_ref(const ibc_instr *instr)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      return ibc_instr_as_alu(instr)->dest;
   case IBC_INSTR_TYPE_SEND:
      return ibc_instr_as_send(instr)->dest;
   case IBC_INSTR_TYPE_INTRINSIC:
      return ibc_instr_as_intrinsic(instr)->dest;
   default:
      return (struct ibc_ref) { .file = IBC_FILE_NONE };
   }
}

static unsigned
regs_written(const ibc_instr *instr)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_SEND:
      return ibc_instr_as_send(instr)->rlen;
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      if (alu->dest.file != IBC_FILE_HW_GRF)
         return 0;
      return hw_grf_count(&alu->dest, -1, 1, instr->simd_width);
   }
   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      /* XXX: num_dest_comps */
      return DIV_ROUND_UP(intrin->num_dest_bytes, REG_SIZE);
   }
   default:
      return 0;
   }
}

static struct gen_eu_instruction_info
inst_info(const struct gen_device_info *devinfo, const ibc_instr *instr)
{
   enum ibc_type tx, td;
   unsigned op, sd, ss = 0, desc = 0, sfid = 0;

   tx = ibc_instr_exec_type(instr);
   td = dest_ref(instr).type;
   sd = regs_written(instr);

   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      op = OP(instr->type, alu->op);

      for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
         if (alu->src[i].ref.file == IBC_FILE_HW_GRF) {
            unsigned ith_size =
               hw_grf_count(&alu->src[i].ref, -1, 1, instr->simd_width);
            ss = MAX2(ss, ith_size);
         }
      }

      /* 32x32 integer multiplication has half the usual ALU throughput.
       * Treat it as double-precision.
       */
      if ((alu->op == IBC_ALU_OP_IMUL || alu->op == IBC_ALU_OP_MAD) &&
          ibc_type_bit_size(tx) == 32 &&
          ibc_type_bit_size(alu->src[0].ref.type) ==
             ibc_type_bit_size(alu->src[1].ref.type)) {
         tx = ibc_type_base_type(tx) | IBC_TYPE_64_BIT;
      }
      break;
   }
   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);

      /* We typically want the maximum source size, except for split send
       * messages which require the total size.
       */
      op = OP_SEND;
      ss = send->mlen + send->ex_mlen;
      desc = send->desc_imm;
      sfid = send->sfid;
      break;
   }
   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      op = OP(instr->type, intrin->op);

      for (unsigned i = 0; i < intrin->num_srcs; i++) {
         if (intrin->src[i].ref.file == IBC_FILE_HW_GRF) {
            unsigned ith_size =
               hw_grf_count(&intrin->src[i].ref, -1, 1, instr->simd_width);
            ss = MAX2(ss, ith_size);
         }
      }
      break;
   }
   case IBC_INSTR_TYPE_FLOW: {
      ibc_flow_instr *flow = ibc_instr_as_flow(instr);
      op = OP(instr->type, flow->op);
      break;
   }
   }

   assert(ibc_type_bit_size(tx) != 0 || instr->type == IBC_INSTR_TYPE_FLOW);

   /* Convert the execution size to GRF units. */
   unsigned sx =
      DIV_ROUND_UP(instr->simd_width * ibc_type_byte_size(tx), REG_SIZE);

   struct gen_eu_instruction_info info = {
      .devinfo = devinfo,
      .op = op,
      .td = td,
      .sd = sd,
      .tx = tx,
      .sx = sx,
      .ss = ss,
      .sc = 0, // XXX: has_bank_conflict(devinfo, inst) ? info.sd : 0;
      .desc = desc,
      .sfid = sfid,
   };

   return info;
}

static bool
stall_for_ref_callback(ibc_ref *ref,
                       int num_bytes, int num_comps,
                       uint8_t simd_group, uint8_t simd_width,
                       void *state)
{
   struct gen_eu_performance_state *st = state;

   switch (ref->file) {
   case IBC_FILE_HW_GRF: {
      assert(!ref->reg); /* We don't handle virtual registers yet. */

      unsigned min, max;
      ibc_calc_hw_grf_range(ref, num_bytes, num_comps, simd_width, &min, &max);

      for (unsigned i = min; i < max; i++)
         stall_on_dependency(st, grf_dependency_id(i));
      break;
   }
   case IBC_FILE_FLAG:
      assert(!ref->reg); /* We don't handle virtual registers yet. */
      assert(num_comps == 1);

      for (unsigned i = ref->flag.bit / 32;
           i < DIV_ROUND_UP(ref->flag.bit + simd_width, 32); i++) {
         stall_on_dependency(st, flag_dependency_id(i));
      }
      break;
   case IBC_FILE_ACCUM: {
      assert(!ref->reg); /* We don't handle virtual registers yet. */
      assert(num_comps == 1);

      unsigned ref_byte_size = ibc_type_byte_size(ref->type);
      unsigned start_byte = ref->accum.chan * ref_byte_size;
      unsigned end_byte = (ref->accum.chan + simd_width) * ref_byte_size;

      for (unsigned i = start_byte / 4; i < DIV_ROUND_UP(end_byte, 4); i++) {
         enum gen_eu_dependency_id dep = GEN_DEPENDENCY_ID_ACCUM0 + i;
         assert(dep < GEN_DEPENDENCY_ID_FLAG0);
         stall_on_dependency(st, dep);
      }
      break;
   }
   case IBC_FILE_LOGICAL:
      assert(!"EU performance doesn't handle IBC logical registers yet.");
      abort();
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      /* No stalls for immediates or null sources. */
      break;
   }

   return true;
}

struct mark_write_dep_cb_state {
   struct gen_eu_performance_state *st;
   const struct gen_eu_perf_desc *perf;
};

static bool
mark_write_dependency_for_ref_callback(ibc_ref *ref,
                                       int num_bytes, int num_comps,
                                       uint8_t simd_group, uint8_t simd_width,
                                       void *state)
{
   struct mark_write_dep_cb_state *tuple = state;
   struct gen_eu_performance_state *st = tuple->st;
   const struct gen_eu_perf_desc *perf = tuple->perf;

   switch (ref->file) {
   case IBC_FILE_HW_GRF: {
      assert(!ref->reg); /* We don't handle virtual registers yet. */

      unsigned min, max;
      ibc_calc_hw_grf_range(ref, num_bytes, num_comps, simd_width, &min, &max);

      for (unsigned i = min; i < max; i++)
         mark_write_dependency(st, perf, grf_dependency_id(i));
      break;
   }
   case IBC_FILE_FLAG:
      assert(!ref->reg); /* We don't handle virtual registers yet. */
      assert(num_comps == 1);

      for (unsigned i = ref->flag.bit / 32;
           i < DIV_ROUND_UP(ref->flag.bit + simd_width, 32); i++) {
         mark_write_dependency(st, perf, flag_dependency_id(i));
      }
      break;
   case IBC_FILE_ACCUM: {
      assert(!ref->reg); /* We don't handle virtual registers yet. */
      assert(num_comps == 1);

      unsigned ref_byte_size = ibc_type_byte_size(ref->type);
      unsigned start_byte = ref->accum.chan * ref_byte_size;
      unsigned end_byte = (ref->accum.chan + simd_width) * ref_byte_size;

      for (unsigned i = start_byte / 4; i < DIV_ROUND_UP(end_byte, 4); i++) {
         enum gen_eu_dependency_id dep = GEN_DEPENDENCY_ID_ACCUM0 + i;
         assert(dep < GEN_DEPENDENCY_ID_FLAG0);
         mark_write_dependency(st, perf, dep);
      }
      break;
   }
   case IBC_FILE_LOGICAL:
      assert(!"EU performance doesn't handle IBC logical registers yet.");
      abort();
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      /* No stalls for immediates or null sources. */
      break;
   }

   return true;
}


/**
 * Model the performance behavior of an IBC instruction.
 */
static void
issue_instruction(struct gen_eu_performance_state *st,
                  const struct gen_device_info *devinfo,
                  ibc_instr *instr)
{
   const struct gen_eu_instruction_info info = inst_info(devinfo, instr);
   const struct gen_eu_perf_desc perf = instruction_desc(&info);

   /* Stall on any source dependencies. */
   ibc_instr_foreach_read(instr, stall_for_ref_callback, st);

   /* Stall on any write dependencies. */
   ibc_instr_foreach_write(instr, stall_for_ref_callback, st);

#if 0
   /* Stall on any SBID dependencies. */
   if (inst->sched.mode & (TGL_SBID_SET | TGL_SBID_DST))
      stall_on_dependency(st, tgl_swsb_wr_dependency_id(inst->sched));
   else if (inst->sched.mode & TGL_SBID_SRC)
      stall_on_dependency(st, tgl_swsb_rd_dependency_id(inst->sched));
#endif

   /* Execute the instruction. */
   execute_instruction(st, &perf);

   /* Mark any source dependencies. */
   if (instr->type == IBC_INSTR_TYPE_SEND) {
      /* XXX: logical intrinsics that turn into sends */
      ibc_send_instr *send = ibc_instr_as_send(instr);
      for (unsigned i = 0; i < send->mlen; i++) {
         assert(send->payload[0].file == IBC_FILE_HW_GRF);
         unsigned grf = send->payload[0].hw_grf.byte / REG_SIZE + i;
         mark_read_dependency(st, &perf, grf_dependency_id(grf));
      }
      for (unsigned i = 0; i < send->ex_mlen; i++) {
         assert(send->payload[1].file == IBC_FILE_HW_GRF);
         unsigned grf = send->payload[1].hw_grf.byte / REG_SIZE + i;
         mark_read_dependency(st, &perf, grf_dependency_id(grf));
      }
   }

   /* Mark any destination dependencies. */
   struct mark_write_dep_cb_state t = { .st = st, .perf = &perf };
   ibc_instr_foreach_write(instr, mark_write_dependency_for_ref_callback, &t);

#if 0
   /* Mark any SBID dependencies. */
   if (inst->sched.mode & TGL_SBID_SET) {
      mark_read_dependency(st, &perf, tgl_swsb_rd_dependency_id(inst->sched));
      mark_write_dependency(st, &perf, tgl_swsb_wr_dependency_id(inst->sched));
   }
#endif
}

/**
 * Update block index and return the number of blocks.
 */
static unsigned
label_blocks(ibc_shader *shader)
{
   unsigned num_blocks = 0;
   ibc_foreach_flow_instr(flow, shader)
      flow->block_index = num_blocks++;

   /* Disregard the END flow instruction. */
   return num_blocks - 1;
}

/**
 * Estimate the performance of the specified shader.
 */
struct ibc_eu_performance *
ibc_estimate_performance(ibc_shader *shader)
{
   unsigned num_blocks = label_blocks(shader);

   struct ibc_eu_performance *p = ralloc(NULL, struct ibc_eu_performance);
   p->block_latency = rzalloc_array(p, unsigned, num_blocks);

   /* XXX - Plumbing the trip counts from NIR loop analysis would allow us
    *       to do a better job regarding the loop weights.  And some branch
    *       divergence analysis would allow us to do a better job with
    *       branching weights.
    *
    *       In the meantime use values that roughly match the control flow
    *       weights used elsewhere in the compiler back-end -- Main
    *       difference is the worst-case scenario branch_weight used for
    *       SIMD32 which accounts for the possibility of a dynamically
    *       uniform branch becoming divergent in SIMD32.
    */
   const float branch_weight = (shader->simd_width > 16 ? 1.0 : 0.5);
   const float loop_weight = 10;
   unsigned elapsed = 0, elapsed0 = 0;
   struct gen_eu_performance_state st;

   gen_eu_performance_state_init(&st);

   ibc_foreach_instr(instr, shader) {
      ibc_flow_instr *flow =
         instr->type == IBC_INSTR_TYPE_FLOW ? ibc_instr_as_flow(instr) : NULL;

      const unsigned clock0 = st.unit_ready[GEN_UNIT_FE];

      issue_instruction(&st, shader->devinfo, instr);

      if (flow && flow->op == IBC_FLOW_OP_ENDIF)
         st.weight /= branch_weight;

      elapsed += (st.unit_ready[GEN_UNIT_FE] - clock0) * st.weight;

      if (flow) {
         if (flow->op == IBC_FLOW_OP_IF)
            st.weight *= branch_weight;
         else if (flow->op == IBC_FLOW_OP_DO)
            st.weight *= loop_weight;
         else if (flow->op == IBC_FLOW_OP_WHILE)
            st.weight /= loop_weight;

         if (flow->op != IBC_FLOW_OP_START) {
            p->block_latency[flow->block_index - 1] = elapsed - elapsed0;
            elapsed0 = elapsed;
         }
      }
   }

   p->latency = elapsed;
   p->throughput =
      shader->simd_width * calculate_thread_throughput(&st, elapsed);

   return p;
}
