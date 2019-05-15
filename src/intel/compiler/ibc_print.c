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
print_reg_ref(FILE *fp, const ibc_reg_ref *ref, bool print_type)
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
      fprintf(fp, ":%s", ibc_type_suffix(ref->type));
      return;

   case IBC_REG_FILE_HW_GRF: {
      unsigned type_sz_B = ibc_type_byte_size(ref->type);
      if (ref->reg->hw_grf.byte == IBC_HW_GRF_REG_UNASSIGNED) {
         fprintf(fp, "hw%u", ref->reg->index);
         if (ref->hw_grf.offset) {
            assert(type_sz_B > 0);
            fprintf(fp, ".%u", ref->hw_grf.offset / type_sz_B);
         }
      } else {
         unsigned byte = ref->reg->hw_grf.byte + ref->hw_grf.offset;
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
      if (ref->type != IBC_TYPE_INVALID)
         fprintf(fp, ":%s", ibc_type_suffix(ref->type));
      return;
   }

   case IBC_REG_FILE_FLAG:
      /* TODO: Take simd_group into account? */
      fprintf(fp, "f%u.%u", ref->reg->flag.subnr / 2,
              ref->reg->flag.subnr % 2);
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
      print_reg_ref(fp, &instr->flag, false);
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
alu_op_name(enum ibc_alu_op op)
{
   switch (op) {
   case IBC_ALU_OP_MOV:  return "mov";
   case IBC_ALU_OP_SEL:  return "sel";
   case IBC_ALU_OP_AND:  return "and";
   case IBC_ALU_OP_OR:   return "or";
   case IBC_ALU_OP_SHR:  return "shr";
   case IBC_ALU_OP_SHL:  return "shl";
   case IBC_ALU_OP_CMP:  return "cmp";
   case IBC_ALU_OP_ADD:  return "add";
   }
   unreachable("Unknown ALU opcode");
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
   fprintf(fp, "%s", alu_op_name(alu->op));
   if (alu->cmod) {
      fprintf(fp, ".%s.", conditional_mod_name(alu->cmod));
      assert(alu->instr.flag.type == IBC_TYPE_FLAG);
      print_reg_ref(fp, &alu->instr.flag, false);
   }
   print_instr_group(fp, &alu->instr);

   fprintf(fp, "   ");

   print_reg_ref(fp, &alu->dest, true);

   for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
      fprintf(fp, "   ");
      print_reg_ref(fp, &alu->src[i].ref, true);
   }

   fprintf(fp, "\n");
}

static void
print_send_instr(FILE *fp, const ibc_send_instr *send)
{
   print_instr(fp, &send->instr, "send", 1);

   fprintf(fp, "   ");
   print_reg_ref(fp, &send->dest, true);

   fprintf(fp, "   ");
   print_reg_ref(fp, &send->payload[0], true);

   fprintf(fp, "   ");
   print_reg_ref(fp, &send->payload[1], true);

   fprintf(fp, "   ");
   if (send->desc.file == IBC_REG_FILE_NONE) {
      fprintf(fp, "0x%08" PRIx32, send->desc_imm);
   } else {
      fprintf(fp, "(");
      print_reg_ref(fp, &send->desc, true);
      fprintf(fp, " | 0x%08" PRIx32 ")", send->desc_imm);
   }

   fprintf(fp, "   ");
   if (send->desc.file == IBC_REG_FILE_NONE) {
      fprintf(fp, "0x%08" PRIx32, send->ex_desc_imm);
   } else {
      fprintf(fp, "(");
      print_reg_ref(fp, &send->ex_desc, true);
      fprintf(fp, " | 0x%08" PRIx32 ")", send->ex_desc_imm);
   }

   fprintf(fp, "\n");
}

static const char *
intrinsic_op_name(enum ibc_intrinsic_op op)
{
   switch (op) {
   case IBC_INTRINSIC_OP_INVALID: break;
   case IBC_INTRINSIC_OP_SIMD_ZIP:           return "simd_zip";
   case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:  return "bti_untyped_write";
   }
   unreachable("Invalid IBC intrinsic op");
}

static void
print_intrinsic_instr(FILE *fp, const ibc_intrinsic_instr *intrin)
{
   print_instr(fp, &intrin->instr, intrinsic_op_name(intrin->op), 1);

   fprintf(fp, "   ");
   print_reg_ref(fp, &intrin->dest, true);

   for (unsigned i = 0; i < intrin->num_srcs; i++) {
      fprintf(fp, "   ");
      if (intrin->src[i].num_comps > 1)
         fprintf(fp, "(vec%u)", intrin->src[i].num_comps);
      print_reg_ref(fp, &intrin->src[i].ref, true);
   }
   fprintf(fp, "\n");
}

static const char *
merge_op_name(enum ibc_merge_op op)
{
   switch (op) {
   case IBC_MERGE_OP_MERGE:   return "merge";
   case IBC_MERGE_OP_ENDIF:   return "endif";
   case IBC_MERGE_OP_DO:      return "do";
   case IBC_MERGE_OP_START:   return "start";
   }
   unreachable("Unknown ALU opcode");
}

static void
print_merge_instr(FILE *fp, ibc_merge_instr *merge)
{
   fprintf(fp, "/* Block %u, preds: [", merge->block_index);
   bool first_pred = true;
   list_for_each_entry(ibc_merge_pred, pred, &merge->preds, link) {
      fprintf(fp, "%s%u", first_pred ? "" : ", ",
              pred->branch->block_start->block_index);
      first_pred = false;
   }
   fprintf(fp, "] */\n");

   print_instr(fp, &merge->instr, merge_op_name(merge->op), 0);

   fprintf(fp, "\n");
}

static const char *
branch_op_name(enum ibc_branch_op op)
{
   switch (op) {
   case IBC_BRANCH_OP_NEXT:      return "next";
   case IBC_BRANCH_OP_IF:        return "if";
   case IBC_BRANCH_OP_ELSE:      return "else";
   case IBC_BRANCH_OP_WHILE:     return "while";
   case IBC_BRANCH_OP_BREAK:     return "break";
   case IBC_BRANCH_OP_CONTINUE:  return "continue";
   case IBC_BRANCH_OP_END:       return "end";
   }
   unreachable("Unknown ALU opcode");
}

static void
print_branch_instr(FILE *fp, ibc_branch_instr *branch)
{
   print_instr(fp, &branch->instr, branch_op_name(branch->op), 0);
   if (branch->jump)
      fprintf(fp, " jump: %u", branch->jump->block_index);
   if (branch->merge)
      fprintf(fp, " merge: %u", branch->merge->block_index);

   fprintf(fp, "\n");
}

static void
print_phi_instr(FILE *fp, ibc_phi_instr *phi)
{
   print_instr(fp, &phi->instr, "phi", 1);

   fprintf(fp, "   ");
   print_reg_ref(fp, &phi->dest, true);

   ibc_foreach_phi_src(src, phi) {
      fprintf(fp, "   %u -> ", src->pred->block_start->block_index);
      print_reg_ref(fp, &src->ref, true);
   }

   fprintf(fp, "\n");
}

void
ibc_print_shader(const ibc_shader *shader, FILE *fp)
{
   uint32_t num_regs = 0;
   ibc_foreach_reg(reg, shader)
      reg->index = num_regs++;

   uint32_t num_blocks = 0;
   ibc_foreach_instr(instr, shader) {
      if (instr->type == IBC_INSTR_TYPE_MERGE)
         ibc_instr_as_merge(instr)->block_index = num_blocks++;
   }

   ibc_foreach_instr(instr, shader) {
      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU:
         print_alu_instr(fp, ibc_instr_as_alu(instr));
         continue;
      case IBC_INSTR_TYPE_SEND:
         print_send_instr(fp, ibc_instr_as_send(instr));
         continue;
      case IBC_INSTR_TYPE_INTRINSIC:
         print_intrinsic_instr(fp, ibc_instr_as_intrinsic(instr));
         continue;
      case IBC_INSTR_TYPE_MERGE:
         print_merge_instr(fp, ibc_instr_as_merge(instr));
         continue;
      case IBC_INSTR_TYPE_BRANCH:
         print_branch_instr(fp, ibc_instr_as_branch(instr));
         continue;
      case IBC_INSTR_TYPE_PHI:
         print_phi_instr(fp, ibc_instr_as_phi(instr));
         continue;
      }
      unreachable("Invalid instruction type");
   }
}
