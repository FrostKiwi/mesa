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
#include "brw_fs.h"
#include "brw_vec4.h"
#include "brw_cfg.h"

using namespace brw;

namespace {
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
   const struct gen_eu_perf_desc
   instruction_desc(const struct gen_eu_instruction_info *info)
   {
      const struct gen_device_info *devinfo = info->devinfo;

      switch (info->op) {
      case BRW_OPCODE_SYNC:
      case BRW_OPCODE_SEL:
      case BRW_OPCODE_NOT:
      case BRW_OPCODE_AND:
      case BRW_OPCODE_OR:
      case BRW_OPCODE_XOR:
      case BRW_OPCODE_SHR:
      case BRW_OPCODE_SHL:
      case BRW_OPCODE_DIM:
      case BRW_OPCODE_ASR:
      case BRW_OPCODE_CMPN:
      case BRW_OPCODE_F16TO32:
      case BRW_OPCODE_BFREV:
      case BRW_OPCODE_BFI1:
      case BRW_OPCODE_AVG:
      case BRW_OPCODE_FRC:
      case BRW_OPCODE_RNDU:
      case BRW_OPCODE_RNDD:
      case BRW_OPCODE_RNDE:
      case BRW_OPCODE_RNDZ:
      case BRW_OPCODE_MAC:
      case BRW_OPCODE_MACH:
      case BRW_OPCODE_LZD:
      case BRW_OPCODE_FBH:
      case BRW_OPCODE_FBL:
      case BRW_OPCODE_CBIT:
      case BRW_OPCODE_ADDC:
      case BRW_OPCODE_ROR:
      case BRW_OPCODE_ROL:
      case BRW_OPCODE_SUBB:
      case BRW_OPCODE_SAD2:
      case BRW_OPCODE_SADA2:
      case BRW_OPCODE_LINE:
      case BRW_OPCODE_NOP:
      case SHADER_OPCODE_CLUSTER_BROADCAST:
      case SHADER_OPCODE_SCRATCH_HEADER:
      case FS_OPCODE_DDX_COARSE:
      case FS_OPCODE_DDX_FINE:
      case FS_OPCODE_DDY_COARSE:
      case FS_OPCODE_PIXEL_X:
      case FS_OPCODE_PIXEL_Y:
      case FS_OPCODE_SET_SAMPLE_ID:
      case VEC4_OPCODE_MOV_BYTES:
      case VEC4_OPCODE_UNPACK_UNIFORM:
      case VEC4_OPCODE_DOUBLE_TO_F32:
      case VEC4_OPCODE_DOUBLE_TO_D32:
      case VEC4_OPCODE_DOUBLE_TO_U32:
      case VEC4_OPCODE_TO_DOUBLE:
      case VEC4_OPCODE_PICK_LOW_32BIT:
      case VEC4_OPCODE_PICK_HIGH_32BIT:
      case VEC4_OPCODE_SET_LOW_32BIT:
      case VEC4_OPCODE_SET_HIGH_32BIT:
      case GS_OPCODE_SET_DWORD_2:
      case GS_OPCODE_SET_WRITE_OFFSET:
      case GS_OPCODE_SET_VERTEX_COUNT:
      case GS_OPCODE_PREPARE_CHANNEL_MASKS:
      case GS_OPCODE_SET_CHANNEL_MASKS:
      case GS_OPCODE_GET_INSTANCE_ID:
      case GS_OPCODE_SET_PRIMITIVE_ID:
      case GS_OPCODE_SVB_SET_DST_INDEX:
      case TCS_OPCODE_SRC0_010_IS_ZERO:
      case TCS_OPCODE_GET_PRIMITIVE_ID:
      case TES_OPCODE_GET_PRIMITIVE_ID:
         if (devinfo->gen >= 11) {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 10, 6 /* XXX */, 14, 0, 0);
         } else if (devinfo->gen >= 8) {
            if (type_sz(info->tx) > 4)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                     0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                     0, 8, 4, 12, 0, 0);
         } else if (devinfo->is_haswell) {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 10, 6 /* XXX */, 16, 0, 0);
         } else {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 12, 8 /* XXX */, 18, 0, 0);
         }

      case BRW_OPCODE_MOV:
      case BRW_OPCODE_CMP:
      case BRW_OPCODE_ADD:
      case BRW_OPCODE_MUL:
      case SHADER_OPCODE_MOV_RELOC_IMM:
         if (devinfo->gen >= 11) {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 10, 6, 14, 0, 0);
         } else if (devinfo->gen >= 8) {
            if (type_sz(info->tx) > 4)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                     0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                     0, 8, 4, 12, 0, 0);
         } else if (devinfo->is_haswell) {
            if (info->tx == BRW_REGISTER_TYPE_F)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                     0, 12, 8 /* XXX */, 18, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                     0, 10, 6 /* XXX */, 16, 0, 0);
         } else if (devinfo->gen >= 7) {
            if (info->tx == BRW_REGISTER_TYPE_F)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                     0, 14, 10 /* XXX */, 20, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                     0, 12, 8 /* XXX */, 18, 0, 0);
         } else {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2 /* XXX */, 0,
                                  0, 2 /* XXX */,
                                  0, 12 /* XXX */, 8 /* XXX */, 18 /* XXX */,
                                  0, 0);
         }

      case BRW_OPCODE_BFE:
      case BRW_OPCODE_BFI2:
      case BRW_OPCODE_CSEL:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);
         else
            abort();

      case BRW_OPCODE_MAD:
         if (devinfo->gen >= 11) {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
         } else if (devinfo->gen >= 8) {
            if (type_sz(info->tx) > 4)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 1, 0, 4,
                                     0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                     0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
         } else if (devinfo->is_haswell) {
            if (info->tx == BRW_REGISTER_TYPE_F)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                     0, 12, 8 /* XXX */, 18, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                     0, 10, 6 /* XXX */, 16, 0, 0);
         } else if (devinfo->gen >= 7) {
            if (info->tx == BRW_REGISTER_TYPE_F)
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                     0, 14, 10 /* XXX */, 20, 0, 0);
            else
               return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                     0, 12, 8 /* XXX */, 18, 0, 0);
         } else if (devinfo->gen >= 6) {
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2 /* XXX */, 1 /* XXX */,
                                  0, 2 /* XXX */,
                                  0, 12 /* XXX */, 8 /* XXX */, 18 /* XXX */,
                                  0, 0);
         } else {
            abort();
         }

      case BRW_OPCODE_F32TO16:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);
         else
            abort();

      case BRW_OPCODE_DP4:
      case BRW_OPCODE_DPH:
      case BRW_OPCODE_DP3:
      case BRW_OPCODE_DP2:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);

      case SHADER_OPCODE_RCP:
      case SHADER_OPCODE_RSQ:
      case SHADER_OPCODE_SQRT:
      case SHADER_OPCODE_EXP2:
      case SHADER_OPCODE_LOG2:
      case SHADER_OPCODE_SIN:
      case SHADER_OPCODE_COS:
      case SHADER_OPCODE_POW:
      case SHADER_OPCODE_INT_QUOTIENT:
      case SHADER_OPCODE_INT_REMAINDER:
         if (devinfo->gen >= 6) {
            switch (info->op) {
            case SHADER_OPCODE_RCP:
            case SHADER_OPCODE_RSQ:
            case SHADER_OPCODE_SQRT:
            case SHADER_OPCODE_EXP2:
            case SHADER_OPCODE_LOG2:
            case SHADER_OPCODE_SIN:
            case SHADER_OPCODE_COS:
               if (devinfo->gen >= 8)
                  return calculate_desc(info, GEN_UNIT_EM, -2, 4, 0, 0, 4,
                                        0, 16, 0, 0, 0, 0);
               else if (devinfo->is_haswell)
                  return calculate_desc(info, GEN_UNIT_EM, 0, 2, 0, 0, 2,
                                        0, 12, 0, 0, 0, 0);
               else
                  return calculate_desc(info, GEN_UNIT_EM, 0, 2, 0, 0, 2,
                                        0, 14, 0, 0, 0, 0);

            case SHADER_OPCODE_POW:
               if (devinfo->gen >= 8)
                  return calculate_desc(info, GEN_UNIT_EM, -2, 4, 0, 0, 8,
                                        0, 24, 0, 0, 0, 0);
               else if (devinfo->is_haswell)
                  return calculate_desc(info, GEN_UNIT_EM, 0, 2, 0, 0, 4,
                                        0, 20, 0, 0, 0, 0);
               else
                  return calculate_desc(info, GEN_UNIT_EM, 0, 2, 0, 0, 4,
                                        0, 22, 0, 0, 0, 0);

            case SHADER_OPCODE_INT_QUOTIENT:
            case SHADER_OPCODE_INT_REMAINDER:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 26, 0,
                                     0, 28 /* XXX */, 0, 0, 0, 0);

            default:
               abort();
            }
         } else {
            switch (info->op) {
            case SHADER_OPCODE_RCP:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 0, 8,
                                     0, 22, 0, 0, 0, 8);

            case SHADER_OPCODE_RSQ:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 0, 16,
                                     0, 44, 0, 0, 0, 8);

            case SHADER_OPCODE_INT_QUOTIENT:
            case SHADER_OPCODE_SQRT:
            case SHADER_OPCODE_LOG2:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 0, 24,
                                     0, 66, 0, 0, 0, 8);

            case SHADER_OPCODE_INT_REMAINDER:
            case SHADER_OPCODE_EXP2:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 0, 32,
                                     0, 88, 0, 0, 0, 8);

            case SHADER_OPCODE_SIN:
            case SHADER_OPCODE_COS:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 0, 48,
                                     0, 132, 0, 0, 0, 8);

            case SHADER_OPCODE_POW:
               return calculate_desc(info, GEN_UNIT_EM, 2, 0, 0, 0, 64,
                                     0, 176, 0, 0, 0, 8);

            default:
               abort();
            }
         }

      case BRW_OPCODE_DO:
         if (devinfo->gen >= 6)
            return calculate_desc(info, GEN_UNIT_NULL, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_NULL, 2 /* XXX */, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0);

      case BRW_OPCODE_IF:
      case BRW_OPCODE_ELSE:
      case BRW_OPCODE_ENDIF:
      case BRW_OPCODE_WHILE:
      case BRW_OPCODE_BREAK:
      case BRW_OPCODE_CONTINUE:
      case FS_OPCODE_DISCARD_JUMP:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_NULL, 8, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_NULL, 6, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_NULL, 2, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0);

      case FS_OPCODE_LINTERP:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 0, 0, 4,
                                  0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);

      case BRW_OPCODE_LRP:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 4, 1, 0, 4,
                                  0, 12, 8 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 6)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 1, 0, 2,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);
         else
            abort();

      case FS_OPCODE_PACK_HALF_2x16_SPLIT:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 20, 6, 0, 0, 6,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 16, 6, 0, 0, 6,
                                  0, 8 /* XXX */, 4 /* XXX */,
                                  12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 20, 6, 0, 0, 6,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_FPU, 24, 6, 0, 0, 6,
                                  0, 12 /* XXX */, 8 /* XXX */,
                                  18 /* XXX */, 0, 0);
         else
            abort();

      case SHADER_OPCODE_MOV_INDIRECT:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 34, 0, 0, 34, 0,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 34, 0, 0, 34, 0,
                                  0, 8 /* XXX */, 4 /* XXX */,
                                  12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 34, 0, 0, 34, 0,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 34, 0, 0, 34, 0,
                                  0, 12 /* XXX */, 8 /* XXX */,
                                  18 /* XXX */, 0, 0);

      case SHADER_OPCODE_BROADCAST:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 20 /* XXX */, 0, 0, 4, 0,
                                  0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 18, 0, 0, 4, 0,
                                  0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 18, 0, 0, 4, 0,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_FPU, 20, 0, 0, 4, 0,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);
         else
            abort();

      case SHADER_OPCODE_FIND_LIVE_CHANNEL:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 2, 0, 0, 2, 0,
                                  0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 2, 0, 0, 2, 0,
                                  0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 36, 0, 0, 6, 0,
                                  0, 10, 6 /* XXX */, 16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_FPU, 40, 0, 0, 6, 0,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);
         else
            abort();

      case SHADER_OPCODE_RND_MODE:
      case SHADER_OPCODE_FLOAT_CONTROL_MODE:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 24 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 0, 0, 0, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 20 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 0, 0, 0, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 24 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 0, 0, 0, 0, 0);
         else if (devinfo->gen >= 6)
            return calculate_desc(info, GEN_UNIT_FPU, 28 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 0, 0, 0, 0, 0);
         else
            abort();

      case SHADER_OPCODE_SHUFFLE:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 44 /* XXX */, 0, 0,
                                  44 /* XXX */, 0,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 42 /* XXX */, 0, 0,
                                  42 /* XXX */, 0,
                                  0, 8 /* XXX */, 4 /* XXX */,
                                  12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 44 /* XXX */, 0,
                                  0, 44 /* XXX */,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  16 /* XXX */, 0, 0);
         else if (devinfo->gen >= 6)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 46 /* XXX */, 0,
                                  0, 46 /* XXX */,
                                  0, 12 /* XXX */, 8 /* XXX */,
                                  18 /* XXX */, 0, 0);
         else
            abort();

      case SHADER_OPCODE_SEL_EXEC:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 10 /* XXX */, 4 /* XXX */, 0,
                                  0, 4 /* XXX */,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 8 /* XXX */, 4 /* XXX */, 0,
                                  0, 4 /* XXX */,
                                  0, 8 /* XXX */, 4 /* XXX */,
                                  12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 10 /* XXX */, 4 /* XXX */, 0,
                                  0, 4 /* XXX */,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 12 /* XXX */, 4 /* XXX */, 0,
                                  0, 4 /* XXX */,
                                  0, 12 /* XXX */, 8 /* XXX */,
                                  18 /* XXX */, 0, 0);

      case SHADER_OPCODE_QUAD_SWIZZLE:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 0 /* XXX */, 8 /* XXX */, 0,
                                  0, 8 /* XXX */,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0 /* XXX */, 8 /* XXX */, 0,
                                  0, 8 /* XXX */,
                                  0, 8 /* XXX */, 4 /* XXX */,
                                  12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0 /* XXX */, 8 /* XXX */, 0,
                                  0, 8 /* XXX */,
                                  0, 10 /* XXX */, 6 /* XXX */,
                                  16 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0 /* XXX */, 8 /* XXX */, 0,
                                  0, 8 /* XXX */,
                                  0, 12 /* XXX */, 8 /* XXX */,
                                  18 /* XXX */, 0, 0);

      case FS_OPCODE_DDY_FINE:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 14, 0, 0, 4,
                                  0, 10, 6 /* XXX */, 14 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 8, 4 /* XXX */, 12 /* XXX */, 0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 12, 8 /* XXX */, 18 /* XXX */, 0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2, 0, 0, 2,
                                  0, 14, 10 /* XXX */, 20 /* XXX */, 0, 0);

      case FS_OPCODE_LOAD_LIVE_CHANNELS:
         if (devinfo->gen >= 11)
            return calculate_desc(info, GEN_UNIT_FPU, 2 /* XXX */, 0, 0,
                                  2 /* XXX */, 0,
                                  0, 0, 0, 10 /* XXX */, 0, 0);
         else if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 0, 2 /* XXX */, 0,
                                  0, 2 /* XXX */,
                                  0, 0, 0, 8 /* XXX */, 0, 0);
         else
            abort();

      case VEC4_OPCODE_PACK_BYTES:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 4 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 8 /* XXX */, 4 /* XXX */, 12 /* XXX */,
                                  0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 4 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 10 /* XXX */, 6 /* XXX */, 16 /* XXX */,
                                  0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 4 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 12 /* XXX */, 8 /* XXX */, 18 /* XXX */,
                                  0, 0);

      case VS_OPCODE_UNPACK_FLAGS_SIMD4X2:
      case TCS_OPCODE_GET_INSTANCE_ID:
      case TCS_OPCODE_SET_INPUT_URB_OFFSETS:
      case TCS_OPCODE_SET_OUTPUT_URB_OFFSETS:
      case TES_OPCODE_CREATE_INPUT_READ_HEADER:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 22 /* XXX */, 0, 0,
                                  6 /* XXX */, 0,
                                  0, 8 /* XXX */, 4 /* XXX */, 12 /* XXX */,
                                  0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 26 /* XXX */, 0, 0,
                                  6 /* XXX */, 0,
                                  0, 10 /* XXX */, 6 /* XXX */, 16 /* XXX */,
                                  0, 0);
         else
            return calculate_desc(info, GEN_UNIT_FPU, 30 /* XXX */, 0, 0,
                                  6 /* XXX */, 0,
                                  0, 12 /* XXX */, 8 /* XXX */, 18 /* XXX */,
                                  0, 0);

      case GS_OPCODE_FF_SYNC_SET_PRIMITIVES:
      case TCS_OPCODE_CREATE_BARRIER_HEADER:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 32 /* XXX */, 0, 0,
                                  8 /* XXX */, 0,
                                  0, 8 /* XXX */, 4 /* XXX */, 12 /* XXX */,
                                  0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 38 /* XXX */, 0, 0,
                                  8 /* XXX */, 0,
                                  0, 10 /* XXX */, 6 /* XXX */, 16 /* XXX */,
                                  0, 0);
         else if (devinfo->gen >= 6)
            return calculate_desc(info, GEN_UNIT_FPU, 44 /* XXX */, 0, 0,
                                  8 /* XXX */, 0,
                                  0, 12 /* XXX */, 8 /* XXX */, 18 /* XXX */,
                                  0, 0);
         else
            abort();

      case TES_OPCODE_ADD_INDIRECT_URB_OFFSET:
         if (devinfo->gen >= 8)
            return calculate_desc(info, GEN_UNIT_FPU, 12 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 8 /* XXX */, 4 /* XXX */, 12 /* XXX */,
                                  0, 0);
         else if (devinfo->is_haswell)
            return calculate_desc(info, GEN_UNIT_FPU, 14 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 10 /* XXX */, 6 /* XXX */, 16 /* XXX */,
                                  0, 0);
         else if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_FPU, 16 /* XXX */, 0, 0,
                                  4 /* XXX */, 0,
                                  0, 12 /* XXX */, 8 /* XXX */, 18 /* XXX */,
                                  0, 0);
         else
            abort();

      case SHADER_OPCODE_TEX:
      case FS_OPCODE_TXB:
      case SHADER_OPCODE_TXD:
      case SHADER_OPCODE_TXF:
      case SHADER_OPCODE_TXF_LZ:
      case SHADER_OPCODE_TXL:
      case SHADER_OPCODE_TXL_LZ:
      case SHADER_OPCODE_TXF_CMS:
      case SHADER_OPCODE_TXF_CMS_W:
      case SHADER_OPCODE_TXF_UMS:
      case SHADER_OPCODE_TXF_MCS:
      case SHADER_OPCODE_TXS:
      case SHADER_OPCODE_LOD:
      case SHADER_OPCODE_GET_BUFFER_SIZE:
      case SHADER_OPCODE_TG4:
      case SHADER_OPCODE_TG4_OFFSET:
      case SHADER_OPCODE_SAMPLEINFO:
      case FS_OPCODE_VARYING_PULL_CONSTANT_LOAD_GEN4:
         return calculate_desc(info, GEN_UNIT_SAMPLER, 2, 0, 0, 0, 16 /* XXX */,
                               8 /* XXX */, 750 /* XXX */, 0, 0,
                               2 /* XXX */, 0);

      case SHADER_OPCODE_URB_READ_SIMD8:
      case SHADER_OPCODE_URB_READ_SIMD8_PER_SLOT:
      case SHADER_OPCODE_URB_WRITE_SIMD8:
      case SHADER_OPCODE_URB_WRITE_SIMD8_PER_SLOT:
      case SHADER_OPCODE_URB_WRITE_SIMD8_MASKED:
      case SHADER_OPCODE_URB_WRITE_SIMD8_MASKED_PER_SLOT:
      case VEC4_OPCODE_URB_READ:
      case VS_OPCODE_URB_WRITE:
      case GS_OPCODE_URB_WRITE:
      case GS_OPCODE_URB_WRITE_ALLOCATE:
      case GS_OPCODE_THREAD_END:
      case GS_OPCODE_FF_SYNC:
      case TCS_OPCODE_URB_WRITE:
      case TCS_OPCODE_RELEASE_INPUT:
      case TCS_OPCODE_THREAD_END:
         return calculate_desc(info, GEN_UNIT_URB, 2, 0, 0, 0, 6 /* XXX */,
                               32 /* XXX */, 200 /* XXX */, 0, 0, 0, 0);

      case SHADER_OPCODE_MEMORY_FENCE:
      case SHADER_OPCODE_INTERLOCK:
         switch (info->sfid) {
         case GEN6_SFID_DATAPORT_RENDER_CACHE:
            if (devinfo->gen >= 7)
               return calculate_desc(info, GEN_UNIT_DP_RC, 2, 0, 0, 30 /* XXX */, 0,
                                     10 /* XXX */, 300 /* XXX */, 0, 0, 0, 0);
            else
               abort();

         case GEN7_SFID_DATAPORT_DATA_CACHE:
         case HSW_SFID_DATAPORT_DATA_CACHE_1:
            if (devinfo->gen >= 7)
               return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0, 30 /* XXX */, 0,
                                     10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);
            else
               abort();

         default:
            abort();
         }

      case SHADER_OPCODE_GEN4_SCRATCH_READ:
      case SHADER_OPCODE_GEN4_SCRATCH_WRITE:
      case SHADER_OPCODE_GEN7_SCRATCH_READ:
         return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0, 0, 8 /* XXX */,
                               10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);

      case VEC4_OPCODE_UNTYPED_ATOMIC:
         if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0,
                                  30 /* XXX */, 400 /* XXX */,
                                  10 /* XXX */, 100 /* XXX */, 0, 0,
                                  0, 400 /* XXX */);
         else
            abort();

      case VEC4_OPCODE_UNTYPED_SURFACE_READ:
      case VEC4_OPCODE_UNTYPED_SURFACE_WRITE:
         if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0,
                                  0, 20 /* XXX */,
                                  10 /* XXX */, 100 /* XXX */, 0, 0,
                                  0, 0);
         else
            abort();

      case FS_OPCODE_FB_WRITE:
      case FS_OPCODE_FB_READ:
      case FS_OPCODE_REP_FB_WRITE:
         return calculate_desc(info, GEN_UNIT_DP_RC, 2, 0, 0, 0, 450 /* XXX */,
                               10 /* XXX */, 300 /* XXX */, 0, 0, 0, 0);

      case GS_OPCODE_SVB_WRITE:
         if (devinfo->gen >= 6)
            return calculate_desc(info, GEN_UNIT_DP_RC, 2 /* XXX */, 0, 0,
                                  0, 450 /* XXX */,
                                  10 /* XXX */, 300 /* XXX */, 0, 0,
                                  0, 0);
         else
            abort();

      case FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD:
      case FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD_GEN7:
         return calculate_desc(info, GEN_UNIT_DP_CC, 2, 0, 0, 0, 16 /* XXX */,
                               10 /* XXX */, 100 /* XXX */, 0, 0, 0, 0);

      case VS_OPCODE_PULL_CONSTANT_LOAD:
      case VS_OPCODE_PULL_CONSTANT_LOAD_GEN7:
         return calculate_desc(info, GEN_UNIT_SAMPLER, 2, 0, 0, 0, 16,
                               8, 750, 0, 0, 2, 0);

      case FS_OPCODE_INTERPOLATE_AT_SAMPLE:
      case FS_OPCODE_INTERPOLATE_AT_SHARED_OFFSET:
      case FS_OPCODE_INTERPOLATE_AT_PER_SLOT_OFFSET:
         if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_PI, 2, 0, 0, 14 /* XXX */, 0,
                                  0, 90 /* XXX */, 0, 0, 0, 0);
         else
            abort();

      case SHADER_OPCODE_BARRIER:
         if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_GATEWAY, 90 /* XXX */, 0, 0,
                                  0 /* XXX */, 0,
                                  0, 0, 0, 0, 0, 0);
         else
            abort();

      case CS_OPCODE_CS_TERMINATE:
         if (devinfo->gen >= 7)
            return calculate_desc(info, GEN_UNIT_SPAWNER, 2, 0, 0, 0 /* XXX */, 0,
                                  10 /* XXX */, 0, 0, 0, 0, 0);
         else
            abort();

      case SHADER_OPCODE_SEND:
         switch (info->sfid) {
         case GEN6_SFID_DATAPORT_RENDER_CACHE:
            if (devinfo->gen >= 7) {
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
            } else if (devinfo->gen >= 6)  {
               return calculate_desc(info, GEN_UNIT_DP_RC, 2 /* XXX */, 0, 0,
                                     0, 450 /* XXX */,
                                     10 /* XXX */, 300 /* XXX */, 0, 0, 0, 0);
            } else {
               abort();
            }
         case BRW_SFID_SAMPLER: {
            if (devinfo->gen >= 6)
               return calculate_desc(info, GEN_UNIT_SAMPLER, 2, 0, 0, 0, 16,
                                     8, 750, 0, 0, 2, 0);
            else
               abort();
         }
         case GEN7_SFID_DATAPORT_DATA_CACHE:
         case HSW_SFID_DATAPORT_DATA_CACHE_1:
            if (devinfo->gen >= 8 || devinfo->is_haswell) {
               switch (brw_dp_desc_msg_type(devinfo, info->desc)) {
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
            } else if (devinfo->gen >= 7) {
               switch (brw_dp_desc_msg_type(devinfo, info->desc)) {
               case GEN7_DATAPORT_DC_UNTYPED_ATOMIC_OP:
                  return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0,
                                        30 /* XXX */, 400 /* XXX */,
                                        10 /* XXX */, 100 /* XXX */,
                                        0, 0, 0, 400 /* XXX */);
               default:
                  return calculate_desc(info, GEN_UNIT_DP_DC, 2, 0, 0,
                                        0, 20 /* XXX */,
                                        10 /* XXX */, 100 /* XXX */, 0, 0,
                                        0, 0);
               }
            } else {
               abort();
            }
         default:
            abort();
         }

      case SHADER_OPCODE_UNDEF:
      case FS_OPCODE_PLACEHOLDER_HALT:
      case FS_OPCODE_SCHEDULING_FENCE:
         return calculate_desc(info, GEN_UNIT_NULL, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0);

      default:
         abort();
      }
   }

   /**
    * Return the dependency ID of a backend_reg, offset by \p delta GRFs.
    */
   enum gen_eu_dependency_id
   reg_dependency_id(const gen_device_info *devinfo, const backend_reg &r,
                     const int delta)
   {
      if (r.file == VGRF) {
         const unsigned i = r.nr + r.offset / REG_SIZE + delta;
         return grf_dependency_id(i);

      } else if (r.file == FIXED_GRF) {
         const unsigned i = r.nr + delta;
         assert(i < GEN_DEPENDENCY_ID_MRF0 - GEN_DEPENDENCY_ID_GRF0);
         return gen_eu_dependency_id(GEN_DEPENDENCY_ID_GRF0 + i);

      } else if (r.file == MRF && devinfo->gen >= 7) {
         const unsigned i = GEN7_MRF_HACK_START +
                            r.nr + r.offset / REG_SIZE + delta;
         assert(i < GEN_DEPENDENCY_ID_MRF0 - GEN_DEPENDENCY_ID_GRF0);
         return gen_eu_dependency_id(GEN_DEPENDENCY_ID_GRF0 + i);

      } else if (r.file == MRF && devinfo->gen < 7) {
         const unsigned i = (r.nr & ~BRW_MRF_COMPR4) +
                            r.offset / REG_SIZE + delta;
         assert(i < GEN_DEPENDENCY_ID_ADDR0 - GEN_DEPENDENCY_ID_MRF0);
         return gen_eu_dependency_id(GEN_DEPENDENCY_ID_MRF0 + i);

      } else if (r.file == ARF && r.nr >= BRW_ARF_ADDRESS &&
                 r.nr < BRW_ARF_ACCUMULATOR) {
         assert(delta == 0);
         return GEN_DEPENDENCY_ID_ADDR0;

      } else if (r.file == ARF && r.nr >= BRW_ARF_ACCUMULATOR &&
                 r.nr < BRW_ARF_FLAG) {
         const unsigned i = r.nr - BRW_ARF_ACCUMULATOR + delta;
         assert(i < GEN_DEPENDENCY_ID_FLAG0 - GEN_DEPENDENCY_ID_ACCUM0);
         return gen_eu_dependency_id(GEN_DEPENDENCY_ID_ACCUM0 + i);

      } else {
         return GEN_NUM_DEPENDENCY_IDS;
      }
   }

   /**
    * Return the implicit accumulator register accessed by channel \p i of the
    * instruction.
    */
   unsigned
   accum_reg_of_channel(const gen_device_info *devinfo,
                        const backend_instruction *inst,
                        brw_reg_type tx, unsigned i)
   {
      assert(inst->reads_accumulator_implicitly() ||
             inst->writes_accumulator_implicitly(devinfo));
      const unsigned offset = (inst->group + i) * type_sz(tx) *
         (devinfo->gen < 7 || brw_reg_type_is_floating_point(tx) ? 1 : 2);
      return offset / REG_SIZE % 2;
   }

   struct gen_eu_instruction_info
   inst_info(const gen_device_info *devinfo, const fs_inst *inst)
   {
      enum brw_reg_type tx = get_exec_type(inst);

      /* We typically want the maximum source size, except for split send
       * messages which require the total size.
       */
      unsigned ss = 0;
      if (inst->opcode == SHADER_OPCODE_SEND) {
         ss = DIV_ROUND_UP(inst->size_read(2), REG_SIZE) +
              DIV_ROUND_UP(inst->size_read(3), REG_SIZE);
      } else {
         for (unsigned i = 0; i < inst->sources; i++)
            ss = MAX2(ss, DIV_ROUND_UP(inst->size_read(i), REG_SIZE));
      }

      /* Convert the execution size to GRF units. */
      unsigned sx = DIV_ROUND_UP(inst->exec_size * type_sz(tx), REG_SIZE);

      /* 32x32 integer multiplication has half the usual ALU throughput.
       * Treat it as double-precision.
       */
      if ((inst->opcode == BRW_OPCODE_MUL || inst->opcode == BRW_OPCODE_MAD) &&
          !brw_reg_type_is_floating_point(tx) && type_sz(tx) == 4 &&
          type_sz(inst->src[0].type) == type_sz(inst->src[1].type))
         tx = brw_int_type(8, tx == BRW_REGISTER_TYPE_D);

      struct gen_eu_instruction_info info;

      info.devinfo = devinfo;
      info.op = inst->opcode;
      info.td = inst->dst.type;
      info.sd = DIV_ROUND_UP(inst->size_written, REG_SIZE);
      info.tx = tx;
      info.sx = sx;
      info.ss = ss;
      info.sc = has_bank_conflict(devinfo, inst) ? info.sd : 0;
      info.desc = inst->desc;
      info.sfid = inst->sfid;

      return info;
   }

   /**
    * Model the performance behavior of an FS back-end instruction.
    */
   void
   issue_fs_inst(struct gen_eu_performance_state *st,
                 const gen_device_info *devinfo,
                 const backend_instruction *be_inst)
   {
      const fs_inst *inst = static_cast<const fs_inst *>(be_inst);
      const struct gen_eu_instruction_info info = inst_info(devinfo, inst);
      const gen_eu_perf_desc perf = instruction_desc(&info);

      /* Stall on any source dependencies. */
      for (unsigned i = 0; i < inst->sources; i++) {
         for (unsigned j = 0; j < regs_read(inst, i); j++)
            stall_on_dependency(
               st, reg_dependency_id(devinfo, inst->src[i], j));
      }

      if (inst->reads_accumulator_implicitly()) {
         for (unsigned j = accum_reg_of_channel(devinfo, inst, info.tx, 0);
              j <= accum_reg_of_channel(devinfo, inst, info.tx,
                                        inst->exec_size - 1); j++)
            stall_on_dependency(
               st, reg_dependency_id(devinfo, brw_acc_reg(8), j));
      }

      if (is_send(inst) && inst->base_mrf != -1) {
         for (unsigned j = 0; j < inst->mlen; j++)
            stall_on_dependency(
               st, reg_dependency_id(
                  devinfo, brw_uvec_mrf(8, inst->base_mrf, 0), j));
      }

      if (const unsigned mask = inst->flags_read(devinfo)) {
         for (unsigned i = 0; i < sizeof(mask) * CHAR_BIT; i++) {
            if (mask & (1 << i))
               stall_on_dependency(st, flag_dependency_id(i));
         }
      }

      /* Stall on any write dependencies. */
      if (!inst->no_dd_check) {
         if (inst->dst.file != BAD_FILE && !inst->dst.is_null()) {
            for (unsigned j = 0; j < regs_written(inst); j++)
               stall_on_dependency(
                  st, reg_dependency_id(devinfo, inst->dst, j));
         }

         if (inst->writes_accumulator_implicitly(devinfo)) {
            for (unsigned j = accum_reg_of_channel(devinfo, inst, info.tx, 0);
                 j <= accum_reg_of_channel(devinfo, inst, info.tx,
                                           inst->exec_size - 1); j++)
               stall_on_dependency(
                  st, reg_dependency_id(devinfo, brw_acc_reg(8), j));
         }

         if (const unsigned mask = inst->flags_written()) {
            for (unsigned i = 0; i < sizeof(mask) * CHAR_BIT; i++) {
               if (mask & (1 << i))
                  stall_on_dependency(st, flag_dependency_id(i));
            }
         }
      }

      /* Stall on any SBID dependencies. */
      if (inst->sched.mode & (TGL_SBID_SET | TGL_SBID_DST))
         stall_on_dependency(st, tgl_swsb_wr_dependency_id(inst->sched));
      else if (inst->sched.mode & TGL_SBID_SRC)
         stall_on_dependency(st, tgl_swsb_rd_dependency_id(inst->sched));

      /* Execute the instruction. */
      execute_instruction(st, &perf);

      /* Mark any source dependencies. */
      if (inst->is_send_from_grf()) {
         for (unsigned i = 0; i < inst->sources; i++) {
            if (inst->is_payload(i)) {
               for (unsigned j = 0; j < regs_read(inst, i); j++)
                  mark_read_dependency(
                     st, &perf, reg_dependency_id(devinfo, inst->src[i], j));
            }
         }
      }

      if (is_send(inst) && inst->base_mrf != -1) {
         for (unsigned j = 0; j < inst->mlen; j++)
            mark_read_dependency(st, &perf,
               reg_dependency_id(devinfo, brw_uvec_mrf(8, inst->base_mrf, 0), j));
      }

      /* Mark any destination dependencies. */
      if (inst->dst.file != BAD_FILE && !inst->dst.is_null()) {
         for (unsigned j = 0; j < regs_written(inst); j++) {
            mark_write_dependency(st, &perf,
                                  reg_dependency_id(devinfo, inst->dst, j));
         }
      }

      if (inst->writes_accumulator_implicitly(devinfo)) {
         for (unsigned j = accum_reg_of_channel(devinfo, inst, info.tx, 0);
              j <= accum_reg_of_channel(devinfo, inst, info.tx,
                                        inst->exec_size - 1); j++)
            mark_write_dependency(st, &perf,
                                  reg_dependency_id(devinfo, brw_acc_reg(8), j));
      }

      if (const unsigned mask = inst->flags_written()) {
         for (unsigned i = 0; i < sizeof(mask) * CHAR_BIT; i++) {
            if (mask & (1 << i))
               mark_write_dependency(st, &perf, flag_dependency_id(i));
         }
      }

      /* Mark any SBID dependencies. */
      if (inst->sched.mode & TGL_SBID_SET) {
         mark_read_dependency(st, &perf, tgl_swsb_rd_dependency_id(inst->sched));
         mark_write_dependency(st, &perf, tgl_swsb_wr_dependency_id(inst->sched));
      }
   }

   struct gen_eu_instruction_info
   inst_info(const gen_device_info *devinfo, const vec4_instruction *inst)
   {
      enum brw_reg_type tx = get_exec_type(inst);

      /* Compute the maximum source size. */
      unsigned ss = 0;
      for (unsigned i = 0; i < ARRAY_SIZE(inst->src); i++)
         ss = MAX2(ss, DIV_ROUND_UP(inst->size_read(i), REG_SIZE));

      /* Convert the execution size to GRF units. */
      unsigned sx = DIV_ROUND_UP(inst->exec_size * type_sz(tx), REG_SIZE);

      /* 32x32 integer multiplication has half the usual ALU throughput.
       * Treat it as double-precision.
       */
      if ((inst->opcode == BRW_OPCODE_MUL || inst->opcode == BRW_OPCODE_MAD) &&
          !brw_reg_type_is_floating_point(tx) && type_sz(tx) == 4 &&
          type_sz(inst->src[0].type) == type_sz(inst->src[1].type))
         tx = brw_int_type(8, tx == BRW_REGISTER_TYPE_D);

      struct gen_eu_instruction_info info;

      info.devinfo = devinfo;
      info.op = inst->opcode;
      info.td = inst->dst.type;
      info.sd = DIV_ROUND_UP(inst->size_written, REG_SIZE);
      info.tx = tx;
      info.sx = sx;
      info.ss = ss;
      info.sc = 0;
      info.desc = inst->desc;
      info.sfid = inst->sfid;

      return info;
   }

   /**
    * Model the performance behavior of a VEC4 back-end instruction.
    */
   void
   issue_vec4_instruction(struct gen_eu_performance_state *st,
                          const gen_device_info *devinfo,
                          const backend_instruction *be_inst)
   {
      const vec4_instruction *inst =
         static_cast<const vec4_instruction *>(be_inst);
      const struct gen_eu_instruction_info info = inst_info(devinfo, inst);
      const gen_eu_perf_desc perf = instruction_desc(&info);

      /* Stall on any source dependencies. */
      for (unsigned i = 0; i < ARRAY_SIZE(inst->src); i++) {
         for (unsigned j = 0; j < regs_read(inst, i); j++)
            stall_on_dependency(
               st, reg_dependency_id(devinfo, inst->src[i], j));
      }

      if (inst->reads_accumulator_implicitly()) {
         for (unsigned j = accum_reg_of_channel(devinfo, inst, info.tx, 0);
              j <= accum_reg_of_channel(devinfo, inst, info.tx,
                                        inst->exec_size - 1); j++)
            stall_on_dependency(
               st, reg_dependency_id(devinfo, brw_acc_reg(8), j));
      }

      if (inst->base_mrf != -1) {
         for (unsigned j = 0; j < inst->mlen; j++)
            stall_on_dependency(
               st, reg_dependency_id(
                  devinfo, brw_uvec_mrf(8, inst->base_mrf, 0), j));
      }

      if (inst->reads_flag())
         stall_on_dependency(st, GEN_DEPENDENCY_ID_FLAG0);

      /* Stall on any write dependencies. */
      if (!inst->no_dd_check) {
         if (inst->dst.file != BAD_FILE && !inst->dst.is_null()) {
            for (unsigned j = 0; j < regs_written(inst); j++)
               stall_on_dependency(
                  st, reg_dependency_id(devinfo, inst->dst, j));
         }

         if (inst->writes_accumulator_implicitly(devinfo)) {
            for (unsigned j = accum_reg_of_channel(devinfo, inst, info.tx, 0);
                 j <= accum_reg_of_channel(devinfo, inst, info.tx,
                                           inst->exec_size - 1); j++)
               stall_on_dependency(
                  st, reg_dependency_id(devinfo, brw_acc_reg(8), j));
         }

         if (inst->writes_flag())
            stall_on_dependency(st, GEN_DEPENDENCY_ID_FLAG0);
      }

      /* Execute the instruction. */
      execute_instruction(st, &perf);

      /* Mark any source dependencies. */
      if (inst->is_send_from_grf()) {
         for (unsigned i = 0; i < ARRAY_SIZE(inst->src); i++) {
            for (unsigned j = 0; j < regs_read(inst, i); j++)
               mark_read_dependency(
                  st, &perf, reg_dependency_id(devinfo, inst->src[i], j));
         }
      }

      if (inst->base_mrf != -1) {
         for (unsigned j = 0; j < inst->mlen; j++)
            mark_read_dependency(st, &perf,
               reg_dependency_id(devinfo, brw_uvec_mrf(8, inst->base_mrf, 0), j));
      }

      /* Mark any destination dependencies. */
      if (inst->dst.file != BAD_FILE && !inst->dst.is_null()) {
         for (unsigned j = 0; j < regs_written(inst); j++) {
            mark_write_dependency(st, &perf,
                                  reg_dependency_id(devinfo, inst->dst, j));
         }
      }

      if (inst->writes_accumulator_implicitly(devinfo)) {
         for (unsigned j = accum_reg_of_channel(devinfo, inst, info.tx, 0);
              j <= accum_reg_of_channel(devinfo, inst, info.tx,
                                        inst->exec_size - 1); j++)
            mark_write_dependency(st, &perf,
                                  reg_dependency_id(devinfo, brw_acc_reg(8), j));
      }

      if (inst->writes_flag())
         mark_write_dependency(st, &perf, GEN_DEPENDENCY_ID_FLAG0);
   }

   /**
    * Estimate the performance of the specified shader.
    */
   void
   calculate_performance(performance &p, const backend_shader *s,
                         void (*issue_instruction)(
                            struct gen_eu_performance_state *,
                            const gen_device_info *,
                            const backend_instruction *),
                         unsigned dispatch_width)
   {
      /* XXX - Note that the previous version of this code used worst-case
       *       scenario estimation of branching divergence for SIMD32 shaders,
       *       but this heuristic was removed to improve performance in common
       *       scenarios. Wider shader variants are less optimal when divergence
       *       is high, e.g. when application renders complex scene on a small
       *       surface. It is assumed that such renders are short, so their
       *       time doesn't matter and when it comes to the overall performance,
       *       they are dominated by more optimal larger renders.
       *
       *       It's possible that we could do better with divergence analysis
       *       by isolating branches which are 100% uniform.
       *
       *       Plumbing the trip counts from NIR loop analysis would allow us
       *       to do a better job regarding the loop weights.
       *
       *       In the meantime use values that roughly match the control flow
       *       weights used elsewhere in the compiler back-end.
       *
       *       Note that we provide slightly more pessimistic weights on
       *       Gen12+ for SIMD32, since the effective warp size on that
       *       platform is 2x the SIMD width due to EU fusion, which increases
       *       the likelihood of divergent control flow in comparison to
       *       previous generations, giving narrower SIMD modes a performance
       *       advantage in several test-cases with non-uniform discard jumps.
       */
      const float discard_weight = (dispatch_width > 16 || s->devinfo->gen < 12 ?
                                    1.0 : 0.5);
      const float loop_weight = 10;
      unsigned discard_count = 0;
      unsigned elapsed = 0;
      struct gen_eu_performance_state st;

      gen_eu_performance_state_init(&st);

      foreach_block(block, s->cfg) {
         const unsigned elapsed0 = elapsed;

         foreach_inst_in_block(backend_instruction, inst, block) {
            const unsigned clock0 = st.unit_ready[GEN_UNIT_FE];

            issue_instruction(&st, s->devinfo, inst);

            if (inst->opcode == FS_OPCODE_PLACEHOLDER_HALT && discard_count)
               st.weight /= discard_weight;

            elapsed += (st.unit_ready[GEN_UNIT_FE] - clock0) * st.weight;

            if (inst->opcode == BRW_OPCODE_DO)
               st.weight *= loop_weight;
            else if (inst->opcode == BRW_OPCODE_WHILE)
               st.weight /= loop_weight;
            else if (inst->opcode == FS_OPCODE_DISCARD_JUMP && !discard_count++)
               st.weight *= discard_weight;
         }

         p.block_latency[block->num] = elapsed - elapsed0;
      }

      p.latency = elapsed;
      p.throughput = dispatch_width * calculate_thread_throughput(&st, elapsed);
   }
}

brw::performance::performance(const fs_visitor *v) :
   block_latency(new unsigned[v->cfg->num_blocks])
{
   calculate_performance(*this, v, issue_fs_inst, v->dispatch_width);
}

brw::performance::performance(const vec4_visitor *v) :
   block_latency(new unsigned[v->cfg->num_blocks])
{
   calculate_performance(*this, v, issue_vec4_instruction, 8);
}

brw::performance::~performance()
{
   delete[] block_latency;
}
