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
      unreachable("Invalid types when printing");

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
print_reg_ref(FILE *fp, const ibc_reg_ref *ref, unsigned type_sz_B)
{
   switch (ref->reg->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Invalid files for a ibc_reg");

   case IBC_REG_FILE_LOGICAL:
      fprintf(fp, "lg%u.%u", ref->reg->index, ref->comp);
      return;

   case IBC_REG_FILE_HW_GRF: {
      if (ref->reg->hw_grf.byte == IBC_HW_REG_UNASSIGNED) {
         fprintf(fp, "hw%u", ref->reg->index);
         if (ref->offset) {
            assert(type_sz_B > 0);
            fprintf(fp, ".%u", ref->offset / type_sz_B);
         }
      } else {
         unsigned byte = ref->reg->hw_grf.byte + ref->offset;
         fprintf(fp, "g%u", byte / 32);
         if (byte % 32) {
            assert(type_sz_B > 0);
            fprintf(fp, ".%u", (byte % 32) / type_sz_B);
         }
      }
      if (type_sz_B)
         fprintf(fp, "<%u>", ref->stride / type_sz_B);
      return;
   }

   }
   unreachable("Unknown register file");
}

static void
print_instr(FILE *fp, const ibc_instr *instr, const char *name)
{
   fprintf(fp, "%s", name);
   if (instr->we_all)
      fprintf(fp, "(%u)", instr->simd_width);
   else
      fprintf(fp, "(%u:%u)", instr->simd_group, instr->simd_width);
}

static void
print_imm(FILE *fp, const char imm[8], enum ibc_type type)
{
   switch (type) {
   case IBC_TYPE_INVALID:
   case IBC_TYPE_INT:
   case IBC_TYPE_UINT:
   case IBC_TYPE_FLOAT:
   case IBC_TYPE_B:
   case IBC_TYPE_UB:
      unreachable("Invalid immediate types");

   case IBC_TYPE_W:
   case IBC_TYPE_UW:
      fprintf(fp, "0x%04" PRIx16 ":%s", *(uint16_t *)imm,
              ibc_type_suffix(type));
      return;

   case IBC_TYPE_HF:
      fprintf(fp, "%f:hf", _mesa_half_to_float(*(uint16_t *)imm));
      return;

   case IBC_TYPE_VF:
      unreachable("TODO");

   case IBC_TYPE_D:
   case IBC_TYPE_UD:
   case IBC_TYPE_V:
   case IBC_TYPE_UV:
      fprintf(fp, "0x%08" PRIx32 ":%s", *(uint32_t *)imm,
              ibc_type_suffix(type));
      return;

   case IBC_TYPE_F:
      fprintf(fp, "%f:f", *(float *)imm);
      return;

   case IBC_TYPE_Q:
   case IBC_TYPE_UQ:
      fprintf(fp, "0x%016" PRIx64 ":%s", *(uint64_t *)imm,
              ibc_type_suffix(type));
      return;

   case IBC_TYPE_DF:
      fprintf(fp, "%f:f", *(double *)imm);
      return;
   }

   unreachable("Invalid type");
}

static const char *
alu_op_name(enum ibc_alu_op op)
{
   switch (op) {
   case IBC_ALU_OP_MOV:  return "mov";
   case IBC_ALU_OP_AND:  return "and";
   case IBC_ALU_OP_SHR:  return "shr";
   case IBC_ALU_OP_SHL:  return "shl";
   case IBC_ALU_OP_ADD:  return "add";
   }
   unreachable("Unknown ALU opcode");
}

static void
print_alu_instr(FILE *fp, const ibc_alu_instr *alu)
{
   print_instr(fp, &alu->instr, alu_op_name(alu->op));

   fprintf(fp, "   ");

   if (alu->dest.file == IBC_REG_FILE_NONE) {
      fprintf(fp, "null");
   } else {
      print_reg_ref(fp, &alu->dest.reg, ibc_type_bit_size(alu->dest.type) / 8);
   }
   fprintf(fp, ":%s", ibc_type_suffix(alu->dest.type));

   const unsigned num_srcs = 3; /* TODO */
   for (unsigned i = 0; i < num_srcs; i++) {
      if (alu->src[i].file == IBC_REG_FILE_NONE)
         break;

      fprintf(fp, "   ");
      switch (alu->src[i].file) {
      case IBC_REG_FILE_NONE:
         unreachable("Handled above");
         break;

      case IBC_REG_FILE_IMM:
         print_imm(fp, alu->src[i].imm, alu->src[i].type);
         break;

      case IBC_REG_FILE_LOGICAL:
      case IBC_REG_FILE_HW_GRF:
         print_reg_ref(fp, &alu->src[i].reg,
                       ibc_type_bit_size(alu->src[i].type) / 8);
         fprintf(fp, ":%s", ibc_type_suffix(alu->src[i].type));
         break;

      default:
         unreachable("TODO");
      }
   }

   fprintf(fp, "\n");
}

static void
print_send_instr(FILE *fp, const ibc_send_instr *send)
{
   print_instr(fp, &send->instr, "send");

   fprintf(fp, "   ");

   if (send->dest.reg) {
      print_reg_ref(fp, &send->dest, 0);
   } else {
      fprintf(fp, "null");
   }

   fprintf(fp, "   ");

   print_reg_ref(fp, &send->payload[0], 0);

   fprintf(fp, "   ");

   if (send->payload[1].reg) {
      print_reg_ref(fp, &send->payload[0], 0);
   } else {
      fprintf(fp, "null");
   }

   fprintf(fp, "   ");

   if (send->desc.reg) {
      fprintf(fp, "(");
      print_reg_ref(fp, &send->desc, 0);
      fprintf(fp, " | 0x%08" PRIx32 ")", send->desc_imm);
   } else {
      fprintf(fp, "0x%08" PRIx32, send->desc_imm);
   }

   fprintf(fp, "   ");

   if (send->ex_desc.reg) {
      fprintf(fp, "(");
      print_reg_ref(fp, &send->ex_desc, 0);
      fprintf(fp, " | 0x%08" PRIx32 ")", send->ex_desc_imm);
   } else {
      fprintf(fp, "0x%08" PRIx32, send->ex_desc_imm);
   }

   fprintf(fp, "\n");
}

void
ibc_print_shader(const ibc_shader *shader, FILE *fp)
{
   uint32_t num_regs = 0;
   ibc_foreach_reg(reg, shader)
      reg->index = num_regs++;

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr(instr, block) {
         switch (instr->type) {
         case IBC_INSTR_TYPE_ALU:
            print_alu_instr(fp, ibc_instr_as_alu(instr));
            continue;
         case IBC_INSTR_TYPE_SEND:
            print_send_instr(fp, ibc_instr_as_send(instr));
            continue;
         }
         unreachable("Invalid instruction type");
      }
   }
}
