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

#include "sir.h"
#include "util/half_float.h"

#include <stdio.h>

static const char *
sir_type_suffix(enum sir_type type)
{
   switch (type) {
   case SIR_TYPE_INVALID:
   case SIR_TYPE_INT:
   case SIR_TYPE_UINT:
   case SIR_TYPE_FLOAT:
      unreachable("Invalid types when printing");

   case SIR_TYPE_V:  return "v";
   case SIR_TYPE_UV: return "uv";
   case SIR_TYPE_VF: return "vf";

   case SIR_TYPE_B:  return "b";
   case SIR_TYPE_UB: return "ub";
   case SIR_TYPE_W:  return "w";
   case SIR_TYPE_UW: return "uw";
   case SIR_TYPE_HF: return "hf";
   case SIR_TYPE_D:  return "d";
   case SIR_TYPE_UD: return "ud";
   case SIR_TYPE_F:  return "f";
   case SIR_TYPE_Q:  return "q";
   case SIR_TYPE_UQ: return "uq";
   case SIR_TYPE_DF: return "df";
   }
   unreachable("Unknown type");
}

static void
print_reg_ref(FILE *fp, const sir_reg_ref *ref, unsigned type_sz_B)
{
   switch (ref->reg->file) {
   case SIR_REG_FILE_NONE:
   case SIR_REG_FILE_IMM:
      unreachable("Invalid files for a sir_reg");

   case SIR_REG_FILE_LOGICAL:
      fprintf(fp, "lg%u.%u", ref->reg->index, ref->comp);
      return;

   case SIR_REG_FILE_HW_GRF: {
      if (ref->reg->hw_grf.byte == SIR_HW_REG_UNASSIGNED) {
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
print_imm(FILE *fp, const char imm[8], enum sir_type type)
{
   switch (type) {
   case SIR_TYPE_INVALID:
   case SIR_TYPE_INT:
   case SIR_TYPE_UINT:
   case SIR_TYPE_FLOAT:
   case SIR_TYPE_B:
   case SIR_TYPE_UB:
      unreachable("Invalid immediate types");

   case SIR_TYPE_W:
   case SIR_TYPE_UW:
      fprintf(fp, "0x%04" PRIx16 ":%s", *(uint16_t *)imm,
              sir_type_suffix(type));
      return;

   case SIR_TYPE_HF:
      fprintf(fp, "%f:hf", _mesa_half_to_float(*(uint16_t *)imm));
      return;

   case SIR_TYPE_VF:
      unreachable("TODO");

   case SIR_TYPE_D:
   case SIR_TYPE_UD:
   case SIR_TYPE_V:
   case SIR_TYPE_UV:
      fprintf(fp, "0x%08" PRIx32 ":%s", *(uint32_t *)imm,
              sir_type_suffix(type));
      return;

   case SIR_TYPE_F:
      fprintf(fp, "%f:f", *(float *)imm);
      return;

   case SIR_TYPE_Q:
   case SIR_TYPE_UQ:
      fprintf(fp, "0x%016" PRIx64 ":%s", *(uint64_t *)imm,
              sir_type_suffix(type));
      return;

   case SIR_TYPE_DF:
      fprintf(fp, "%f:f", *(double *)imm);
      return;
   }

   unreachable("Invalid type");
}

static const char *
alu_op_name(enum sir_alu_op op)
{
   switch (op) {
   case SIR_ALU_OP_MOV:  return "mov";
   case SIR_ALU_OP_AND:  return "and";
   case SIR_ALU_OP_SHR:  return "shr";
   case SIR_ALU_OP_SHL:  return "shl";
   case SIR_ALU_OP_ADD:  return "add";
   }
   unreachable("Unknown ALU opcode");
}

static void
print_alu_instr(FILE *fp, const sir_alu_instr *alu)
{
   fprintf(fp, "%s(%u:%u)", alu_op_name(alu->op),
           alu->instr.simd_group, alu->instr.simd_width);

   fprintf(fp, "   ");

   if (alu->dest.file == SIR_REG_FILE_NONE) {
      fprintf(fp, "null");
   } else {
      print_reg_ref(fp, &alu->dest.reg, sir_type_bit_size(alu->dest.type) / 8);
   }
   fprintf(fp, ":%s", sir_type_suffix(alu->dest.type));

   const unsigned num_srcs = 3; /* TODO */
   for (unsigned i = 0; i < num_srcs; i++) {
      if (alu->src[i].file == SIR_REG_FILE_NONE)
         break;

      fprintf(fp, "   ");
      switch (alu->src[i].file) {
      case SIR_REG_FILE_NONE:
         unreachable("Handled above");
         break;

      case SIR_REG_FILE_IMM:
         print_imm(fp, alu->src[i].imm, alu->src[i].type);
         break;

      case SIR_REG_FILE_LOGICAL:
      case SIR_REG_FILE_HW_GRF:
         print_reg_ref(fp, &alu->src[i].reg,
                       sir_type_bit_size(alu->src[i].type) / 8);
         fprintf(fp, ":%s", sir_type_suffix(alu->src[i].type));
         break;

      default:
         unreachable("TODO");
      }
   }

   fprintf(fp, "\n");
}

static void
print_send_instr(FILE *fp, const sir_send_instr *send)
{
}

void
sir_print_shader(const sir_shader *shader, FILE *fp)
{
   uint32_t num_regs = 0;
   sir_foreach_reg(reg, shader)
      reg->index = num_regs++;

   sir_foreach_block(block, shader) {
      sir_foreach_instr(instr, block) {
         switch (instr->type) {
         case SIR_INSTR_TYPE_ALU:
            print_alu_instr(fp, sir_instr_as_alu(instr));
            continue;
         case SIR_INSTR_TYPE_SEND:
            print_send_instr(fp, sir_instr_as_send(instr));
            continue;
         }
         unreachable("Invalid instruction type");
      }
   }
}
