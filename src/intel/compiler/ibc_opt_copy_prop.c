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

/** Tries to compose an ibc_logical_ref with an ibc_ref
 *
 * Specifically, this is the composition outer(inner(reg)).
 * try_copy_prop_ref has already figured out how the SIMD channels map.
 * This function figures out if the composition is possible and, if it is,
 * returns the composition in ref_out and returns true.
 */
static inline bool
try_compose_refs(ibc_ref *ref_out,
                 ibc_ref outer, ibc_ref inner,
                 uint8_t num_comps,
                 unsigned outer_simd_group,
                 unsigned outer_simd_width,
                 unsigned inner_simd_group,
                 unsigned inner_simd_width,
                 bool supports_imm)
{
   assert(outer.file == IBC_FILE_LOGICAL);
   assert(inner.file != IBC_FILE_NONE);
   assert(ibc_type_bit_size(outer.type) <= ibc_type_bit_size(inner.type));

   /* The source must be static */
   if (!ibc_ref_read_is_static(inner))
      return false;

   if (inner.file == IBC_FILE_IMM && !supports_imm)
      return false;

   ibc_ref ref = inner;
   ref.type = outer.type;

   switch (ref.file) {
   case IBC_FILE_NONE:
      break;

   case IBC_FILE_IMM:
      assert(ref.logical.comp == 0);
      if (ref.logical.byte) {
         memmove(ref.imm, ref.imm + ref.logical.byte,
                 ibc_type_byte_size(ref.type));
      }
      break;

   case IBC_FILE_LOGICAL:
      ref.logical.byte += outer.logical.byte;
      ref.logical.comp += outer.logical.comp;
      if (outer.logical.broadcast && !inner.logical.broadcast) {
         /* If the outer ref wants to broadcast and the inner ref is not
          * already broadcasting, we broadcast based on the outer ref.
          */
         ref.logical.broadcast = true;
         ref.logical.simd_channel = outer.logical.simd_channel;
      }
      break;

   case IBC_FILE_HW_GRF:
      /* TODO: We could probably do this in a well-defined way in some cases
       * but it's easier to just avoid for now.
       */
      if (outer.logical.comp > 0 || num_comps > 1)
         return false;

      if (outer.logical.broadcast) {
         assert(outer.logical.simd_channel >= inner_simd_group);
         assert(outer.logical.simd_channel < inner_simd_group +
                                             inner_simd_width);
         unsigned rel_channel = outer.logical.simd_channel - inner_simd_group;
         ibc_hw_grf_simd_slice(&ref.hw_grf, rel_channel);
         ibc_hw_grf_mul_stride(&ref.hw_grf, 0);
      } else if (ref.hw_grf.vstride > 0 || ref.hw_grf.hstride > 0) {
         assert(outer_simd_group >= inner_simd_group);
         assert(outer_simd_group + outer_simd_width <=
                inner_simd_group + inner_simd_width);
         unsigned rel_simd_group = outer_simd_group - inner_simd_group;
         ibc_hw_grf_simd_slice(&ref.hw_grf, rel_simd_group);
      }
      ibc_ref_byte_offset(&ref, outer.logical.byte);
      break;

   case IBC_FILE_FLAG:
      /* Registers in IBC_FILE_FLAG do not have components */
      assert(outer.logical.comp == 0);

      if (inner.type == IBC_TYPE_FLAG) {
         if (outer.type != IBC_TYPE_FLAG)
            return false;

         if (outer.logical.broadcast)
            return false;

         unsigned rel_channel = outer.logical.simd_channel - inner_simd_group;
         ref.flag.bit += rel_channel;
      } else {
         /* Flags can only be accessed as a flag or as 16 or 32-bit */
         assert(ibc_type_bit_size(inner.type) == 16 ||
                ibc_type_bit_size(inner.type) == 32);
         assert(ibc_type_bit_size(outer.type) != 64);
         if (ibc_type_bit_size(outer.type) == 8)
            return false;

         assert(!outer.logical.broadcast);

         if (outer.logical.byte % 2)
            return false;

         ref.flag.bit += outer.logical.byte * 8;
      }
      break;

   case IBC_FILE_ACCUM:
      /* TODO: Figure out how to copy-prop accumulators */
      return false;

   default:
      unreachable("Invalid IBC register file");
   }

   *ref_out = ref;
   return true;
}

static void
ibc_imm_neg(ibc_ref *imm)
{
   switch (ibc_type_base_type(imm->type)) {
   case IBC_TYPE_INT:
   case IBC_TYPE_UINT:
      *(int64_t *)imm->imm = -*(int64_t *)imm->imm;
      memset(imm->imm + ibc_type_byte_size(imm->type), 0,
             sizeof(imm->imm) - ibc_type_byte_size(imm->type));
      break;

   case IBC_TYPE_FLOAT:
      imm->imm[ibc_type_byte_size(imm->type) - 1] ^= 0x80;
      break;

   default:
      unreachable("Invalid immediate type");
   }
}

static void
ibc_imm_not(ibc_ref *imm)
{
   *(int64_t *)imm->imm = ~*(int64_t *)imm->imm;
   memset(imm->imm + ibc_type_byte_size(imm->type), 0,
          sizeof(imm->imm) - ibc_type_byte_size(imm->type));
}

static void
ibc_imm_abs(ibc_ref *imm)
{
   if (imm->imm[ibc_type_byte_size(imm->type) - 1] & 0x80)
      ibc_imm_neg(imm);
}

static void
ibc_imm_apply_mod(ibc_ref *imm, enum ibc_alu_src_mod mod)
{
   if (mod & IBC_ALU_SRC_MOD_ABS)
      ibc_imm_abs(imm);

   if (mod & IBC_ALU_SRC_MOD_NEG)
      ibc_imm_neg(imm);

   if (mod & IBC_ALU_SRC_MOD_NOT)
      ibc_imm_not(imm);
}

static enum ibc_alu_src_mod
compose_alu_src_mods(enum ibc_alu_src_mod outer, enum ibc_alu_src_mod inner)
{
   enum ibc_alu_src_mod mod = outer;

   if (!(mod & IBC_ALU_SRC_MOD_ABS))
      mod ^= inner & IBC_ALU_SRC_MOD_NEG;

   mod |= inner & IBC_ALU_SRC_MOD_ABS;
   mod ^= inner & IBC_ALU_SRC_MOD_NOT;

   return mod;
}

static bool
ref_fills_reg(ibc_ref *ref,
              UNUSED int num_bytes, int num_comps,
              uint8_t simd_group, uint8_t simd_width,
              UNUSED void *_state)
{
   if (ref->file != IBC_FILE_LOGICAL || num_comps < 0)
      return false;

   assert(!ref->logical.broadcast);
   return ref->reg->logical.bit_size == ibc_type_bit_size(ref->type) &&
          ref->reg->logical.num_comps == num_comps &&
          ref->reg->logical.simd_group == simd_group &&
          ref->reg->logical.simd_width == simd_width;
}

static bool
try_copy_prop_ref(ibc_ref *ref, ibc_alu_instr *alu, int alu_src_idx,
                  uint8_t num_comps,
                  uint8_t simd_group, uint8_t simd_width,
                  bool supports_imm)
{
   if (ref->file != IBC_FILE_LOGICAL)
      return false;

   ibc_instr *ssa_instr = ibc_reg_ssa_instr(ref->reg);
   if (ssa_instr == NULL)
      return false;

   if (!ibc_instr_foreach_write(ssa_instr, ref_fills_reg, NULL))
      return false;

   switch (ssa_instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *mov = ibc_instr_as_alu(ssa_instr);
      assert(num_comps == 1);

      /* Must be a non-saturating MOV */
      if (mov->op != IBC_ALU_OP_MOV)
         return false;

      if (mov->src[0].ref.file == IBC_FILE_IMM)
         assert(mov->src[0].mod == IBC_ALU_SRC_MOD_NONE);

      /* Cannot saturate or type convert */
      if (mov->src[0].ref.type != mov->dest.type || mov->saturate)
         return false;

      /* We can only propagate modifiers to ALU sources
       *
       * TODO: We probably need to be a bit more careful here.
       */
      if (!alu && mov->src[0].mod != IBC_ALU_SRC_MOD_NONE)
         return false;

      /* We can't propagate modifiers if the types don't match.
       *
       * TODO: In i965 we have a concept of opcodes which can have their
       * types rewritten.  We should do something similar here.
       */
      if (mov->src[0].mod != IBC_ALU_SRC_MOD_NONE &&
          ref->type != mov->src[0].ref.type)
         return false;

      ibc_ref new_ref;
      if (!try_compose_refs(&new_ref, *ref, mov->src[0].ref,
                            num_comps, simd_group, simd_width,
                            mov->instr.simd_group,
                            mov->instr.simd_width,
                            supports_imm))
         return false;

      if (alu) {
         enum ibc_alu_src_mod new_mods =
            compose_alu_src_mods(alu->src[alu_src_idx].mod, mov->src[0].mod);
         if (new_mods & ~ibc_alu_op_infos[alu->op].supported_src_mods)
            return false;

         if (new_ref.file == IBC_FILE_IMM) {
            ibc_imm_apply_mod(&new_ref, new_mods);
            alu->src[alu_src_idx].mod = IBC_ALU_SRC_MOD_NONE;
         } else {
            alu->src[alu_src_idx].mod = new_mods;
         }
      }

      *ref = new_ref;
      return true;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(ssa_instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_SIMD_ZIP:
         assert(num_comps <= ref->reg->logical.num_comps);
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            if (ref->logical.broadcast) {
               if (ref->logical.simd_channel < intrin->src[i].simd_group ||
                   ref->logical.simd_channel >
                     intrin->src[i].simd_group + intrin->src[i].simd_width)
                  continue;
            } else {
               if (simd_group < intrin->src[i].simd_group ||
                   simd_group + simd_width >
                     intrin->src[i].simd_group + intrin->src[i].simd_width)
                  continue;
            }

            return try_compose_refs(ref, *ref, intrin->src[i].ref,
                                    num_comps, simd_group, simd_width,
                                    intrin->src[i].simd_group,
                                    intrin->src[i].simd_width,
                                    supports_imm);
         }
         return false;

      case IBC_INTRINSIC_OP_VEC: {
         unsigned comp = intrin->dest.logical.comp;
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            if (comp <= ref->logical.comp &&
                (ref->logical.comp + num_comps <=
                 comp + intrin->src[i].num_comps)) {
               assert(intrin->src[i].simd_group == intrin->instr.simd_group);
               assert(intrin->src[i].simd_width == intrin->instr.simd_width);

               ibc_ref comp_ref = *ref;
               comp_ref.logical.comp -= comp;
               return try_compose_refs(ref, comp_ref, intrin->src[i].ref,
                                       num_comps, simd_group, simd_width,
                                       intrin->src[i].simd_group,
                                       intrin->src[i].simd_width,
                                       supports_imm);
            }
            comp += intrin->src[i].num_comps;
         }
      }

      default:
         return false;
      }
   }

   default:
      return false;
   }
}

static bool
can_flip_alu_instr(const ibc_alu_instr *alu)
{
   if (ibc_alu_op_infos[alu->op].num_srcs != 2)
      return false;

   switch (alu->op) {
   case IBC_ALU_OP_SEL:
   case IBC_ALU_OP_AND:
   case IBC_ALU_OP_OR:
   case IBC_ALU_OP_CMP:
   case IBC_ALU_OP_ADD:
      return true;
   default:
      return false;
   }
}

static bool
alu_instr_src_supports_imm(const ibc_alu_instr *alu, unsigned src_idx)
{
   assert(src_idx < ibc_alu_op_infos[alu->op].num_srcs);

   switch (ibc_alu_op_infos[alu->op].num_srcs) {
   case 1:
      return true;
   case 2:
      if (ibc_type_bit_size(alu->src[src_idx].ref.type) > 32)
         return false;

      return src_idx == 1 || (alu->src[1].ref.file != IBC_FILE_IMM &&
                              can_flip_alu_instr(alu));
   default:
      return false;
   }
}

static void
flip_alu_instr_if_needed(ibc_alu_instr *alu)
{
   if (ibc_alu_op_infos[alu->op].num_srcs != 2)
      return;

   if (alu->src[0].ref.file != IBC_FILE_IMM)
      return;

   assert(can_flip_alu_instr(alu));

   ibc_alu_src tmp = alu->src[0];
   alu->src[0] = alu->src[1];
   alu->src[1] = tmp;

   if (alu->op == IBC_ALU_OP_SEL) {
      alu->instr.predicate = ibc_predicate_invert(alu->instr.predicate);
   } else if (alu->op == IBC_ALU_OP_CMP) {
      /* For CMP, we have to flip the comparison around. */
      switch (alu->cmod) {
      case BRW_CONDITIONAL_Z:
      case BRW_CONDITIONAL_NZ:
         break;
      case BRW_CONDITIONAL_G:    alu->cmod = BRW_CONDITIONAL_L;   break;
      case BRW_CONDITIONAL_GE:   alu->cmod = BRW_CONDITIONAL_LE;  break;
      case BRW_CONDITIONAL_L:    alu->cmod = BRW_CONDITIONAL_G;   break;
      case BRW_CONDITIONAL_LE:   alu->cmod = BRW_CONDITIONAL_GE;  break;
      default:
         unreachable("Invalid conditional modifier for CMP");
      }
   }
}

static bool
try_copy_prop_message(ibc_ref *ref, ibc_send_instr *send, uint16_t num_bytes)
{
   ibc_ref new_ref = *ref;
   unsigned num_comps;
   switch (ref->file) {
   case IBC_FILE_LOGICAL: {
      if (ref->reg->logical.stride == 0)
         return false;

      assert(ref->reg->logical.bit_size >= 8);
      assert(ref->reg->logical.stride == ref->reg->logical.bit_size / 8);
      unsigned comp_size_B = (ref->reg->logical.bit_size / 8) *
                             send->instr.simd_width;
      assert(num_bytes % comp_size_B == 0);
      num_comps = num_bytes / comp_size_B;

      if (!try_copy_prop_ref(&new_ref, NULL, 0, num_comps,
                             send->instr.simd_group,
                             send->instr.simd_width,
                             false /* supports_imm */))
         return false;
      break;
   }

   case IBC_FILE_HW_GRF: {
      if (ref->reg == NULL)
         return false;

      ibc_instr *ssa_instr = ibc_reg_ssa_instr(ref->reg);
      if (ssa_instr == NULL)
         return false;

      if (ssa_instr->type != IBC_INSTR_TYPE_INTRINSIC)
         return false;

      ibc_intrinsic_instr *msg = ibc_instr_as_intrinsic(ssa_instr);
      if (msg->op != IBC_INTRINSIC_OP_MESSAGE)
         return false;

      if (ref->hw_grf.byte > 0 || num_bytes != msg->num_dest_bytes)
         return false;

      if (msg->num_srcs != 1)
         return false;

      if (msg->src[0].simd_group != send->instr.simd_group ||
          msg->src[0].simd_width != send->instr.simd_width)
         return false;

      /* There's no interesting composition to do here */
      new_ref = msg->src[0].ref;
      num_comps = msg->src[0].num_comps;
      break;
   }

   default:
      unreachable("Invalid file for SEND message");
   }

   if (ibc_type_bit_size(new_ref.type) < 8)
      return false;

   switch (new_ref.file) {
   case IBC_FILE_LOGICAL:
      if (new_ref.logical.broadcast ||
          new_ref.reg->logical.simd_width == 1 ||
          new_ref.reg->logical.stride != ibc_type_byte_size(new_ref.type) ||
          new_ref.reg->logical.stride != new_ref.reg->logical.bit_size / 8)
         return false;

      if (num_comps > 1 &&
          (new_ref.reg->logical.simd_group != send->instr.simd_group ||
           new_ref.reg->logical.simd_width != send->instr.simd_width))
         return false;
      break;

   case IBC_FILE_HW_GRF:
      if (new_ref.hw_grf.hstride != ibc_type_byte_size(new_ref.type) ||
          new_ref.hw_grf.vstride != new_ref.hw_grf.hstride *
                                    new_ref.hw_grf.width)
         return false;
      break;

   default:
      return false;
   }

   *ref = new_ref;
   return true;
}

bool
ibc_opt_copy_prop(ibc_shader *shader)
{
   bool progress = false;

   ibc_assign_logical_reg_strides(shader);

   ibc_foreach_instr_safe(instr, shader) {

      while (try_copy_prop_ref(&instr->flag, NULL, -1, 1,
                               instr->simd_group, instr->simd_width,
                               false)) {
         progress = true;
      }

      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU: {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);

         for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
            while (try_copy_prop_ref(&alu->src[i].ref, alu, i, 1,
                                     alu->instr.simd_group,
                                     alu->instr.simd_width,
                                     alu_instr_src_supports_imm(alu, i))) {
               /* Immediates may come from things that aren't MOV instructions
                * so we need to handle them here.
                */
               if (alu->src[i].ref.file == IBC_FILE_IMM &&
                   alu->src[i].mod != IBC_ALU_SRC_MOD_NONE) {
                  ibc_imm_apply_mod(&alu->src[i].ref, alu->src[i].mod);
                  alu->src[i].mod = IBC_ALU_SRC_MOD_NONE;
               }
               progress = true;
            }
            flip_alu_instr_if_needed(alu);
         }
         continue;
      }

      case IBC_INSTR_TYPE_SEND: {
         ibc_send_instr *send = ibc_instr_as_send(instr);
         while (try_copy_prop_message(&send->payload[0], send,
                                      send->mlen * 32)) {
            progress = true;
         }

         if (send->ex_mlen > 0) {
            while (try_copy_prop_message(&send->payload[1], send,
                                         send->ex_mlen * 32)) {
               progress = true;
            }
         }
         continue;
      }

      case IBC_INSTR_TYPE_INTRINSIC: {
         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            while (try_copy_prop_ref(&intrin->src[i].ref, NULL, -1,
                                     intrin->src[i].num_comps,
                                     intrin->src[i].simd_group,
                                     intrin->src[i].simd_width,
                                     true)) {
               progress = true;
            }
         }
         continue;
      }

      case IBC_INSTR_TYPE_FLOW:
         continue;
      }
      unreachable("Unsupported IBC instruction type");
   }

   return progress;
}
