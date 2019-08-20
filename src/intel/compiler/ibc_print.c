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
#include "util/half_float.h"

#include <stdio.h>
#include <inttypes.h>

static const char *
ibc_type_suffix(enum ibc_type type)
{
   switch (type) {
   case IBC_TYPE_INVALID:
   case IBC_TYPE_INT:
   case IBC_TYPE_UINT:
   case IBC_TYPE_FLOAT:
   case IBC_TYPE_VECTOR:
      unreachable("Invalid types when printing");

   case IBC_TYPE_FLAG:     return "1b";

   case IBC_TYPE_8_BIT:    return "8b";
   case IBC_TYPE_16_BIT:   return "16b";
   case IBC_TYPE_32_BIT:   return "32b";
   case IBC_TYPE_64_BIT:   return "64b";

   case IBC_TYPE_V:  return "v";
   case IBC_TYPE_UV: return "uv";
   case IBC_TYPE_VF: return "vf";

   case IBC_TYPE_B:  return "b";
   case IBC_TYPE_UB: return "ub";
   case IBC_TYPE_W:  return "w";
   case IBC_TYPE_UW: return "uw";
   case IBC_TYPE_HF: return "hf";
   case IBC_TYPE_D:  return "d";
   case IBC_TYPE_UD: return "ud";
   case IBC_TYPE_F:  return "f";
   case IBC_TYPE_Q:  return "q";
   case IBC_TYPE_UQ: return "uq";
   case IBC_TYPE_DF: return "df";
   }
   unreachable("Unknown type");
}

static void
print_ref(FILE *fp, const ibc_ref *ref, bool print_type)
{
   switch (ref->file) {
   case IBC_REG_FILE_NONE:
      fprintf(fp, "(null)");
      if (ref->type != IBC_TYPE_INVALID)
         fprintf(fp, ":%s", ibc_type_suffix(ref->type));
      return;

   case IBC_REG_FILE_IMM:
      switch (ref->type) {
      case IBC_TYPE_INVALID:
      case IBC_TYPE_INT:
      case IBC_TYPE_UINT:
      case IBC_TYPE_FLOAT:
      case IBC_TYPE_FLAG:
      case IBC_TYPE_8_BIT:
      case IBC_TYPE_B:
      case IBC_TYPE_UB:
      case IBC_TYPE_VECTOR:
         unreachable("Invalid immediate types");

      case IBC_TYPE_16_BIT:
      case IBC_TYPE_W:
      case IBC_TYPE_UW:
         fprintf(fp, "0x%04" PRIx16 ":%s", *(uint16_t *)ref->imm,
                 ibc_type_suffix(ref->type));
         return;

      case IBC_TYPE_HF:
         fprintf(fp, "%f:hf", _mesa_half_to_float(*(uint16_t *)ref->imm));
         return;

      case IBC_TYPE_VF:
         unreachable("TODO");

      case IBC_TYPE_32_BIT:
      case IBC_TYPE_D:
      case IBC_TYPE_UD:
      case IBC_TYPE_V:
      case IBC_TYPE_UV:
         fprintf(fp, "0x%08" PRIx32 ":%s", *(uint32_t *)ref->imm,
                 ibc_type_suffix(ref->type));
         return;

      case IBC_TYPE_F:
         fprintf(fp, "%f:f", *(float *)ref->imm);
         return;

      case IBC_TYPE_64_BIT:
      case IBC_TYPE_Q:
      case IBC_TYPE_UQ:
         fprintf(fp, "0x%016" PRIx64 ":%s", *(uint64_t *)ref->imm,
                 ibc_type_suffix(ref->type));
         return;

      case IBC_TYPE_DF:
         fprintf(fp, "%f:f", *(double *)ref->imm);
         return;
      }
      unreachable("Invalid IBC type");
      return;

   case IBC_REG_FILE_LOGICAL:
      fprintf(fp, "lg%u", ref->reg->index);
      if (ref->reg->logical.num_comps > 1)
         fprintf(fp, ".%u", ref->logical.comp);
      if (ref->logical.broadcast)
         fprintf(fp, "<%u>", ref->logical.simd_channel);
      if (print_type)
         fprintf(fp, ":%s", ibc_type_suffix(ref->type));
      return;

   case IBC_REG_FILE_HW_GRF: {
      unsigned type_sz_B = ibc_type_byte_size(ref->type);
      if (ref->reg != NULL) {
         fprintf(fp, "hw%u", ref->reg->index);
         if (ref->hw_grf.byte) {
            assert(type_sz_B > 0);
            fprintf(fp, ".%u", ref->hw_grf.byte / type_sz_B);
         }
      } else {
         unsigned byte = ref->hw_grf.byte;
         fprintf(fp, "g%u", byte / 32);
         if (byte % 32) {
            assert(type_sz_B > 0);
            fprintf(fp, ".%u", (byte % 32) / type_sz_B);
         }
      }
      if (type_sz_B) {
         if (ref->hw_grf.hstride * ref->hw_grf.width == ref->hw_grf.vstride) {
            fprintf(fp, "<%u>", ref->hw_grf.hstride / type_sz_B);
         } else {
            fprintf(fp, "<%u,%u,%u>", ref->hw_grf.vstride / type_sz_B,
                    ref->hw_grf.width, ref->hw_grf.hstride / type_sz_B);
         }
      }
      if (print_type && ref->type != IBC_TYPE_INVALID)
         fprintf(fp, ":%s", ibc_type_suffix(ref->type));
      return;
   }

   case IBC_REG_FILE_FLAG:
      /* TODO: Take simd_group into account? */
      if (ref->reg != NULL) {
         fprintf(fp, "flag%u.%u", ref->reg->index, ref->flag.bit / 16);
      } else {
         uint8_t subnr = ref->flag.bit / 16;
         fprintf(fp, "f%u.%u", subnr / 2, subnr % 2);
      }
      if (ref->type != IBC_TYPE_FLAG)
         fprintf(fp, ":%s", ibc_type_suffix(ref->type));
      return;
   }
   unreachable("Unknown register file");
}

static void
print_instr_group(FILE *fp, const ibc_instr *instr)
{
   if (instr->we_all)
      fprintf(fp, "(%u)", instr->simd_width);
   else
      fprintf(fp, "(%u:%u)", instr->simd_group, instr->simd_width);
}

static void
print_instr_predicate(FILE *fp, const ibc_instr *instr)
{
   if (instr->predicate) {
      assert(instr->predicate == BRW_PREDICATE_NORMAL); /* TODO */
      fprintf(fp, "(%s", instr->pred_inverse ? "-" : "+");
      assert(instr->flag.type == IBC_TYPE_FLAG);
      print_ref(fp, &instr->flag, false);
      fprintf(fp, ") ");
   }
}

static void
print_instr(FILE *fp, const ibc_instr *instr, const char *name,
            unsigned indent)
{
   for (unsigned i = 0; i < indent; i++)
      fprintf(fp, "    ");
   print_instr_predicate(fp, instr);
   fprintf(fp, "%s", name);
   print_instr_group(fp, instr);
}

static const char *
conditional_mod_name(enum brw_conditional_mod cmod)
{
   switch (cmod) {
   case BRW_CONDITIONAL_NONE:
   case BRW_CONDITIONAL_R:
   case BRW_CONDITIONAL_O:
   case BRW_CONDITIONAL_U:
      unreachable("Invalid conditional modifier for printing");

   case BRW_CONDITIONAL_Z:    return "z";
   case BRW_CONDITIONAL_NZ:   return "nz";
   case BRW_CONDITIONAL_G:    return "g";
   case BRW_CONDITIONAL_GE:   return "ge";
   case BRW_CONDITIONAL_L:    return "l";
   case BRW_CONDITIONAL_LE:   return "le";
   }
   unreachable("Invalid conditional modifier");
}

static void
print_alu_instr(FILE *fp, const ibc_alu_instr *alu)
{
   fprintf(fp, "    ");
   print_instr_predicate(fp, &alu->instr);

   char alu_op_name[16];
   strncpy(alu_op_name, ibc_alu_op_infos[alu->op].name, sizeof(alu_op_name));
   for (char *c = alu_op_name; *c; c++)
      if (*c >= 'A' && *c <= 'Z')
         *c += 'a' - 'A';
   fprintf(fp, "%s", alu_op_name);

   if (alu->cmod) {
      fprintf(fp, ".%s", conditional_mod_name(alu->cmod));
      assert(alu->instr.flag.file == IBC_REG_FILE_NONE ||
             alu->instr.flag.type == IBC_TYPE_FLAG);
      if (alu->instr.flag.file != IBC_REG_FILE_NONE) {
         fprintf(fp, ".");
         print_ref(fp, &alu->instr.flag, false);
      }
   }
   print_instr_group(fp, &alu->instr);

   fprintf(fp, "   ");

   print_ref(fp, &alu->dest, true);

   for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
      fprintf(fp, "   ");
      switch (alu->src[i].mod) {
      case IBC_ALU_SRC_MOD_NONE:
         break;
      case IBC_ALU_SRC_MOD_NEG:
         fprintf(fp, "-");
         break;
      case IBC_ALU_SRC_MOD_ABS:
         fprintf(fp, "|");
         break;
      case IBC_ALU_SRC_MOD_NEG_ABS:
         fprintf(fp, "-|");
         break;
      case IBC_ALU_SRC_MOD_NOT:
         fprintf(fp, "~");
         break;
      default:
         unreachable("Invalid source modifier");
      }

      print_ref(fp, &alu->src[i].ref, true);

      switch (alu->src[i].mod) {
      case IBC_ALU_SRC_MOD_NONE:
      case IBC_ALU_SRC_MOD_NEG:
         break;
      case IBC_ALU_SRC_MOD_NOT:
      case IBC_ALU_SRC_MOD_ABS:
      case IBC_ALU_SRC_MOD_NEG_ABS:
         fprintf(fp, "|");
         break;
      default:
         unreachable("Invalid source modifier");
      }
   }

   fprintf(fp, "\n");
}

static void
print_send_instr(FILE *fp, const ibc_send_instr *send)
{
   print_instr(fp, &send->instr, "send", 1);

   fprintf(fp, "   ");
   print_ref(fp, &send->dest, true);

   fprintf(fp, "   ");
   print_ref(fp, &send->payload[0], true);

   fprintf(fp, "   ");
   print_ref(fp, &send->payload[1], true);

   fprintf(fp, "   ");
   if (send->desc.file == IBC_REG_FILE_NONE) {
      fprintf(fp, "0x%08" PRIx32, send->desc_imm);
   } else {
      fprintf(fp, "(");
      print_ref(fp, &send->desc, true);
      fprintf(fp, " | 0x%08" PRIx32 ")", send->desc_imm);
   }

   fprintf(fp, "   ");
   if (send->desc.file == IBC_REG_FILE_NONE) {
      fprintf(fp, "0x%08" PRIx32, send->ex_desc_imm);
   } else {
      fprintf(fp, "(");
      print_ref(fp, &send->ex_desc, true);
      fprintf(fp, " | 0x%08" PRIx32 ")", send->ex_desc_imm);
   }

   fprintf(fp, "\n");
}

static const char *
intrinsic_op_name(enum ibc_intrinsic_op op)
{
   switch (op) {
   case IBC_INTRINSIC_OP_INVALID: break;
   case IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL:  return "find_live_channel";
   case IBC_INTRINSIC_OP_SIMD_BROADCAST:     return "simd_broadcast";
   case IBC_INTRINSIC_OP_SIMD_ZIP:           return "simd_zip";
   case IBC_INTRINSIC_OP_VEC:                return "vec";
   case IBC_INTRINSIC_OP_BTI_CONST_BLOCK_READ:  return "bti_const_block_read";
   case IBC_INTRINSIC_OP_BTI_UNTYPED_READ:   return "bti_untyped_read";
   case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:  return "bti_untyped_write";
   case IBC_INTRINSIC_OP_LOAD_PAYLOAD:       return "load_payload";
   case IBC_INTRINSIC_OP_PLN:                return "pln";
   case IBC_INTRINSIC_OP_FB_WRITE:           return "fb_write";
   case IBC_INTRINSIC_OP_URB_WRITE:          return "urb_write";
   case IBC_INTRINSIC_OP_TEX:                return "tex";
   case IBC_INTRINSIC_OP_TXB:                return "txb";
   case IBC_INTRINSIC_OP_TXL:                return "txl";
   case IBC_INTRINSIC_OP_TXD:                return "txd";
   case IBC_INTRINSIC_OP_TXF:                return "txf";
   case IBC_INTRINSIC_OP_TXF_MS:             return "txf_ms";
   case IBC_INTRINSIC_OP_TXF_MCS:            return "txf_mcs";
   case IBC_INTRINSIC_OP_TXS:                return "txs";
   case IBC_INTRINSIC_OP_LOD:                return "lod";
   case IBC_INTRINSIC_OP_TG4_OFFSET:         return "tg4_offset";
   case IBC_INTRINSIC_OP_TG4:                return "tg4";
   case IBC_INTRINSIC_OP_SAMPLEINFO:         return "sampleinfo";
   }
   unreachable("Invalid IBC intrinsic op");
}

static void
print_intrinsic_instr(FILE *fp, const ibc_intrinsic_instr *intrin)
{
   print_instr(fp, &intrin->instr, intrinsic_op_name(intrin->op), 1);

   fprintf(fp, "   ");
   print_ref(fp, &intrin->dest, true);

   for (unsigned i = 0; i < intrin->num_srcs; i++) {
      fprintf(fp, "   ");
      if (intrin->src[i].num_comps > 1)
         fprintf(fp, "(vec%u)", intrin->src[i].num_comps);
      print_ref(fp, &intrin->src[i].ref, true);
   }
   fprintf(fp, "\n");
}

static const char *
flow_op_name(enum ibc_flow_op op)
{
   switch (op) {
   case IBC_FLOW_OP_START:       return "start";
   case IBC_FLOW_OP_END:         return "end";
   case IBC_FLOW_OP_IF:          return "if";
   case IBC_FLOW_OP_ELSE:        return "else";
   case IBC_FLOW_OP_ENDIF:       return "endif";
   case IBC_FLOW_OP_DO:          return "do";
   case IBC_FLOW_OP_BREAK:       return "break";
   case IBC_FLOW_OP_CONT:        return "cont";
   case IBC_FLOW_OP_WHILE:       return "while";
   case IBC_FLOW_OP_HALT_JUMP:   return "halt_jump";
   case IBC_FLOW_OP_HALT_MERGE:  return "halt_merge";
   }
   unreachable("Unknown ALU opcode");
}

static void
print_flow_instr(FILE *fp, ibc_flow_instr *flow)
{
   if (flow->op != IBC_FLOW_OP_START) {
      fprintf(fp, "/* BLOCK %u END:", flow->block_index - 1);
      if (flow->jump)
         fprintf(fp, " jump: %u", flow->jump->block_index);
      if (flow->merge)
         fprintf(fp, " merge: %u", flow->merge->block_index);
      fprintf(fp, " */\n");
   }

   print_instr(fp, &flow->instr, flow_op_name(flow->op), 0);
   fprintf(fp, "\n");

   if (flow->op != IBC_FLOW_OP_END) {
      fprintf(fp, "/* BLOCK %u START: preds: [", flow->block_index);

      bool first_pred = true;
      ibc_foreach_flow_pred(pred, flow) {
         fprintf(fp, "%s%u", first_pred ? "" : ", ",
                 pred->instr->block_index - 1);
         first_pred = false;
      }
      fprintf(fp, "] */\n");
   }
}

static void
ibc_print_instr(FILE *fp, const ibc_instr *instr)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      print_alu_instr(fp, ibc_instr_as_alu(instr));
      return;
   case IBC_INSTR_TYPE_SEND:
      print_send_instr(fp, ibc_instr_as_send(instr));
      return;
   case IBC_INSTR_TYPE_INTRINSIC:
      print_intrinsic_instr(fp, ibc_instr_as_intrinsic(instr));
      return;
   case IBC_INSTR_TYPE_FLOW:
      print_flow_instr(fp, ibc_instr_as_flow(instr));
      return;
   }
   unreachable("Invalid instruction type");
}

void
ibc_print_shader(const ibc_shader *shader, FILE *fp)
{
   uint32_t num_regs = 0;
   ibc_foreach_reg(reg, shader)
      reg->index = num_regs++;

   uint32_t num_blocks = 0;
   ibc_foreach_flow_instr(flow, shader)
      flow->block_index = num_blocks++;

   ibc_foreach_instr(instr, shader) {
      ibc_print_instr(fp, instr);
   }
}
