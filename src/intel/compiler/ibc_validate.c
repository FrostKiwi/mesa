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

#include <stdio.h>
#include <stdlib.h>

#include "ibc.h"

#include <util/bitscan.h>
#include <util/hash_table.h>
#include <util/set.h>

struct ibc_validate_state {
   void *mem_ctx;

   /* Pointer set for various things to use to check for uniqueness.  We just
    * always keep one around so we can avoid re-allocating it all the time.
    */
   struct set *tmp_set;

   const ibc_shader *shader;
   const ibc_merge_instr *block_start;
   const ibc_instr *instr;

   /* ibc_reg* -> reg_validate_state* */
   struct hash_table *reg_state;
};

struct reg_validate_state {
   struct set *writes;
   const ibc_merge_instr *write_block_start;
   struct list_head *next_write_link;
};

static bool
_ibc_assert(struct ibc_validate_state *s, int line,
            const char *expr, bool value)
{
   if (likely(value))
      return true;

   fprintf(stderr, "ibc_validate:%d Assertion failed: %s", line, expr);
   abort();

   /* For when this does something more interesting */
   return false;
}
#define ibc_assert(state, expr) _ibc_assert(state, __LINE__, #expr, expr)

static void
ibc_validate_null_reg_ref(struct ibc_validate_state *s,
                          const ibc_reg_ref *ref)
{
   ibc_assert(s, ref->file == IBC_REG_FILE_NONE);
   ibc_assert(s, ref->reg == NULL);
}

static bool
ref_is_none_or_reg(ibc_reg_ref *ref,
                   UNUSED int8_t num_bytes,
                   UNUSED int8_t num_comps,
                   UNUSED uint8_t simd_group,
                   UNUSED uint8_t simd_width,
                   void *_reg)
{
   if (ref->file == IBC_REG_FILE_NONE)
      return true;

   if (ref->file == IBC_REG_FILE_IMM)
      return false;

   return ref->reg == _reg;
}

static void
ibc_validate_reg_ref(struct ibc_validate_state *s,
                     const ibc_reg_ref *ref, bool is_write,
                     unsigned num_bytes, unsigned num_comps,
                     unsigned ref_simd_group, unsigned ref_simd_width)
{
   switch (ref->file) {
   case IBC_REG_FILE_NONE:
      ibc_assert(s, num_comps == 0 && num_bytes == 0);
      ibc_validate_null_reg_ref(s, ref);
      return;

   case IBC_REG_FILE_IMM:
      ibc_assert(s, !is_write);
      ibc_assert(s, num_comps == 1 && num_bytes == 0);
      return;

   case IBC_REG_FILE_HW_GRF:
   case IBC_REG_FILE_FLAG:
      ibc_assert(s, ref->reg == NULL || ref->file == ref->reg->file);
      break;

   case IBC_REG_FILE_LOGICAL:
      if (!ibc_assert(s, ref->reg))
         return;

      ibc_assert(s, ref->file == ref->reg->file);
      break;
   }

   struct reg_validate_state *reg_state = NULL;
   if (ref->reg) {
      struct hash_entry *state_entry =
         _mesa_hash_table_search(s->reg_state, ref->reg);
      if (ibc_assert(s, state_entry))
         reg_state = state_entry->data;
   }

   if (is_write) {
      if (ref->reg) {
         ibc_assert(s, ref->write_instr == s->instr);
         _mesa_set_add(reg_state->writes, ref);
         if (ref->reg->is_wlr) {
            if (reg_state->write_block_start == NULL) {
               reg_state->write_block_start = s->block_start;
            } else {
               ibc_assert(s, reg_state->write_block_start == s->block_start);
            }
            ibc_assert(s, reg_state->next_write_link == &ref->write_link);
            reg_state->next_write_link = ref->write_link.next;
         }
      }
   } else {
      ibc_assert(s, ref->write_instr == NULL);
      if (ref->reg && ref->reg->is_wlr) {
         /* All instructions which read the value must satisfy one of the
          * following:
          *  a. The instruction's only output is a write to the value (i.e.
          *     x |= 7 is ok.)
          *  b. The instruction is dominated by the final write to the value
          *
          * The first condition we check directly.  The second is a bit
          * trickier because we don't actually have dominance information.
          * However, because we validate destinations after sources in
          * instructions, we can get a reasonable approximation by asserting
          * that we've see the final write.  We do this by checking if the
          * next write is the write list sentinel.
          */
         ibc_assert(s, (reg_state &&
                        reg_state->next_write_link == &ref->reg->writes) ||
                       ibc_instr_foreach_write((ibc_instr *)s->instr,
                                               ref_is_none_or_reg,
                                               (ibc_reg *)ref->reg));
      }
   }

   switch (ref->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Handled above with an early return");

   case IBC_REG_FILE_LOGICAL: {
      const ibc_logical_reg *lreg = &ref->reg->logical;
      const ibc_logical_reg_ref *lref = &ref->logical;

      if (num_comps == 0) {
         /* Byte-wise access of logical registers is allowed but it has strict
          * restrictions and assumes the register is tightly packed.
          */
         ibc_assert(s, ref_simd_group == lreg->simd_group);
         ibc_assert(s, ref_simd_width == lreg->simd_width);
         ibc_assert(s, ibc_type_bit_size(ref->type) >= 8);
         ibc_assert(s, ibc_type_bit_size(ref->type) == lreg->bit_size);
         unsigned comp_size_B = (lreg->bit_size / 8) * lreg->simd_width;
         ibc_assert(s, num_bytes % comp_size_B == 0);
         num_comps = num_bytes / comp_size_B;
      } else {
         ibc_assert(s, num_bytes == 0);
      }

      if (lreg->bit_size < 8)
         ibc_assert(s, lref->byte == 0);
      else
         ibc_assert(s, (lref->byte + 1) * 8 <= lreg->bit_size);

      ibc_assert(s, num_comps > 0);
      ibc_assert(s, lref->comp + num_comps <= lreg->num_comps);

      if (lref->broadcast) {
         ibc_assert(s, lref->simd_channel >= lreg->simd_group);
         ibc_assert(s, lref->simd_channel <=
                       lreg->simd_group + lreg->simd_width);
      } else {
         ibc_assert(s, lref->simd_channel == 0);
         if (lreg->simd_width == 1) {
            /* If the register is a scalar (only one SIMD channel), the
             * broadcast is implicit with channel 0.  There's nothing to
             * validate for SIMD channels in this case.
             */
         } else {
            ibc_assert(s, ref_simd_group >= lreg->simd_group);
            ibc_assert(s, ref_simd_group + ref_simd_width <=
                          lreg->simd_group + lreg->simd_width);
         }
      }
      return;
   }

   case IBC_REG_FILE_HW_GRF: {
      const ibc_hw_grf_reg_ref *hw_ref = &ref->hw_grf;

      ibc_assert(s, hw_ref->hstride % ibc_type_byte_size(ref->type) == 0);
      ibc_assert(s, hw_ref->vstride % ibc_type_byte_size(ref->type) == 0);
      if (is_write)
         ibc_assert(s, hw_ref->hstride > 0);

      if (num_bytes == 0) {
         num_bytes = hw_ref->hstride * ((ref_simd_width - 1) % hw_ref->width) +
                     hw_ref->vstride * ((ref_simd_width - 1) / hw_ref->width) +
                     ibc_type_byte_size(ref->type);
         if (num_comps > 1) {
            ibc_assert(s, hw_ref->vstride == hw_ref->hstride * hw_ref->width);
            unsigned arr_stride = MAX2(hw_ref->hstride * ref_simd_width,
                                       ibc_type_byte_size(ref->type));
            num_bytes += arr_stride * (num_comps - 1);
         }
      } else {
         ibc_assert(s, num_comps == 0);
         ibc_assert(s, hw_ref->vstride == hw_ref->hstride * hw_ref->width);
      }
      ibc_assert(s, num_bytes > 0);
      if (ref->reg) {
         const ibc_hw_grf_reg *hw_reg = &ref->reg->hw_grf;
         ibc_assert(s, hw_ref->byte + num_bytes <= hw_reg->size);
         ibc_assert(s, hw_reg->align >= ibc_type_byte_size(ref->type));
      } else {
         ibc_assert(s, hw_ref->byte + num_bytes <= 4096);
      }
      return;
   }

   case IBC_REG_FILE_FLAG:
      ibc_assert(s, num_comps == 1 && num_bytes == 0);
      if (ref->reg) {
         const ibc_flag_reg *flag = &ref->reg->flag;
         ibc_assert(s, ref_simd_group + ref_simd_width <= flag->bits);
         ibc_assert(s, ref->flag.bit == ref_simd_group);
      } else {
         ibc_assert(s, ref->flag.bit >= ref_simd_group);
         ibc_assert(s, ref->flag.bit % ref_simd_width == 0);
         uint8_t base_bit = ref->flag.bit - ref_simd_group;

         /* Assert we're properly aligned */
         ibc_assert(s, base_bit % 8 == 0);
         ibc_assert(s, ref_simd_group <= 8 || base_bit % 16 == 0);
         ibc_assert(s, ref_simd_group <= 16 || base_bit % 32 == 0);
      }
      return;
   }

   unreachable("Invalid register file");
}

static unsigned
brw_predicate_bits(enum brw_predicate pred)
{
   switch (pred) {
   case BRW_PREDICATE_NONE:            return 1;
   case BRW_PREDICATE_NORMAL:          return 1;
   case BRW_PREDICATE_ALIGN1_ANYV:     return 1;
   case BRW_PREDICATE_ALIGN1_ALLV:     return 1;
   case BRW_PREDICATE_ALIGN1_ANY2H:    return 2;
   case BRW_PREDICATE_ALIGN1_ALL2H:    return 2;
   case BRW_PREDICATE_ALIGN1_ANY4H:    return 4;
   case BRW_PREDICATE_ALIGN1_ALL4H:    return 4;
   case BRW_PREDICATE_ALIGN1_ANY8H:    return 8;
   case BRW_PREDICATE_ALIGN1_ALL8H:    return 8;
   case BRW_PREDICATE_ALIGN1_ANY16H:   return 16;
   case BRW_PREDICATE_ALIGN1_ALL16H:   return 16;
   case BRW_PREDICATE_ALIGN1_ANY32H:   return 32;
   case BRW_PREDICATE_ALIGN1_ALL32H:   return 32;
   }

   unreachable("Invalid predicate");
}

static void
ibc_validate_alu_instr(struct ibc_validate_state *s, const ibc_alu_instr *alu)
{
   if (!ibc_assert(s, alu->op < IBC_ALU_NUM_OPS))
      return;

   const ibc_alu_op_info *alu_info = &ibc_alu_op_infos[alu->op];

   for (unsigned i = 0; i < alu_info->num_srcs; i++) {
      ibc_assert(s, alu->src[i].ref.file != IBC_REG_FILE_NONE);
      ibc_assert(s, alu->src[i].ref.type == IBC_TYPE_FLAG ||
                    ibc_type_base_type(alu->src[i].ref.type) != IBC_TYPE_INVALID);
      ibc_validate_reg_ref(s, &alu->src[i].ref, false, 0, 1,
                           alu->instr.simd_group,
                           alu->instr.simd_width);
      ibc_assert(s, (alu->src[i].mod & ~alu_info->supported_src_mods) == 0);
   }

   if (alu->cmod != BRW_CONDITIONAL_NONE) {
      if (alu->instr.flag.file == IBC_REG_FILE_NONE) {
         ibc_validate_null_reg_ref(s, &alu->instr.flag);
         ibc_assert(s, alu->op == IBC_ALU_OP_SEL);
      } else {
         ibc_assert(s, brw_predicate_bits(alu->instr.predicate) == 1);
         ibc_assert(s, alu->instr.flag.file != IBC_REG_FILE_NONE);
         ibc_assert(s, alu->instr.flag.type == IBC_TYPE_FLAG);
         ibc_validate_reg_ref(s, &alu->instr.flag, true, 0, 1,
                              alu->instr.simd_group,
                              alu->instr.simd_width);
      }
   }

   ibc_assert(s, !alu->saturate ||
                 ibc_type_base_type(alu->dest.type) == IBC_TYPE_FLOAT);

   ibc_validate_reg_ref(s, &alu->dest, true,
                        0, alu->dest.file == IBC_REG_FILE_NONE ? 0 : 1,
                        alu->instr.simd_group,
                        alu->instr.simd_width);
}

static void
ibc_validate_send_instr(struct ibc_validate_state *s,
                        const ibc_send_instr *send)
{
   switch (send->desc.file) {
   case IBC_REG_FILE_NONE:
      ibc_validate_null_reg_ref(s, &send->desc);
      break;

   case IBC_REG_FILE_IMM:
      ibc_validate_reg_ref(s, &send->desc, false, 0, 1,
                           send->instr.simd_group,
                           send->instr.simd_width);
      break;

   default:
      ibc_assert(s, send->desc.type == IBC_TYPE_UD);
      ibc_validate_reg_ref(s, &send->desc, false,
                           ibc_type_byte_size(IBC_TYPE_UD), 0,
                           send->instr.simd_group,
                           send->instr.simd_width);
      break;
   }

   switch (send->ex_desc.file) {
   case IBC_REG_FILE_NONE:
      ibc_validate_null_reg_ref(s, &send->ex_desc);
      break;

   case IBC_REG_FILE_IMM:
      ibc_validate_reg_ref(s, &send->ex_desc, false, 0, 1,
                           send->instr.simd_group,
                           send->instr.simd_width);
      break;

   default:
      ibc_assert(s, send->ex_desc.type == IBC_TYPE_UD);
      ibc_validate_reg_ref(s, &send->ex_desc, false,
                           ibc_type_byte_size(IBC_TYPE_UD), 0,
                           send->instr.simd_group,
                           send->instr.simd_width);
   }

   ibc_assert(s, send->mlen > 0);
   ibc_validate_reg_ref(s, &send->payload[0], false,
                        send->mlen * 32, 0,
                        send->instr.simd_group,
                        send->instr.simd_width);

   if (send->ex_mlen > 0) {
      ibc_validate_reg_ref(s, &send->payload[1], false,
                           send->ex_mlen * 32, 0,
                           send->instr.simd_group,
                           send->instr.simd_width);
   } else {
      ibc_validate_null_reg_ref(s, &send->payload[1]);
   }

   if (send->rlen > 0) {
      ibc_validate_reg_ref(s, &send->dest, true,
                           send->rlen * 32, 0,
                           send->instr.simd_group,
                           send->instr.simd_width);
   } else {
      ibc_validate_null_reg_ref(s, &send->dest);
   }
}

static void
ibc_validate_intrinsic_instr(struct ibc_validate_state *s,
                             const ibc_intrinsic_instr *intrin)
{
   if (intrin->has_side_effects)
      ibc_assert(s, !intrin->can_reorder);

   for (unsigned i = 0; i < intrin->num_srcs; i++) {
      ibc_assert(s, intrin->src[i].simd_group >= intrin->instr.simd_group);
      ibc_assert(s, intrin->src[i].simd_group + intrin->src[i].simd_width <=
                    intrin->instr.simd_group + intrin->instr.simd_width);
      ibc_validate_reg_ref(s, &intrin->src[i].ref, false,
                           0, intrin->src[i].num_comps,
                           intrin->src[i].simd_group,
                           intrin->src[i].simd_width);
   }

   ibc_validate_reg_ref(s, &intrin->dest, true,
                        0, intrin->num_dest_comps,
                        intrin->instr.simd_group,
                        intrin->instr.simd_width);

   switch (intrin->op) {
   case IBC_INTRINSIC_OP_SIMD_ZIP: {
      assert(intrin->instr.simd_width % intrin->num_srcs == 0);
      const unsigned src_width = intrin->instr.simd_width / intrin->num_srcs;
      for (unsigned i = 0; i < intrin->num_srcs; i++) {
         ibc_assert(s, intrin->src[i].simd_group ==
                       intrin->instr.simd_group + i * src_width);
         ibc_assert(s, intrin->src[i].simd_width == src_width);
         ibc_assert(s, intrin->src[i].num_comps == intrin->num_dest_comps);
      }
      break;
   }

   default:
      break;
   }
}

static void
ibc_validate_merge_instr(struct ibc_validate_state *s,
                         const ibc_merge_instr *merge)
{
   ibc_assert(s, s->block_start == NULL);
   s->block_start = merge;

   ibc_assert(s, merge->instr.simd_width == s->shader->simd_width);

   if (ibc_assert(s, merge->block_end))
      ibc_assert(s, merge->block_end->block_start == merge);

   assert(s->tmp_set->entries == 0);
   list_validate(&merge->preds);
   list_for_each_entry(ibc_merge_pred, pred, &merge->preds, link) {
      if (!ibc_assert(s, pred->branch != NULL))
         continue;

      /* Check for uniqueness of the predecessors */
      bool already_seen = false;
      _mesa_set_search_and_add(s->tmp_set, pred->branch, &already_seen);
      ibc_assert(s, !already_seen);

      ibc_assert(s, merge == pred->branch->jump ||
                    merge == pred->branch->merge ||
                    (ibc_branch_instr_falls_through(pred->branch) &&
                     &merge->instr == ibc_instr_next(&pred->branch->instr)));
   }
   _mesa_set_clear(s->tmp_set, NULL);

   switch (merge->op) {
   case IBC_MERGE_OP_MERGE:
      break;

   case IBC_MERGE_OP_ENDIF:
      list_for_each_entry(const ibc_merge_pred, pred, &merge->preds, link) {
         ibc_assert(s, pred->branch &&
                       (pred->branch->op == IBC_BRANCH_OP_IF ||
                        pred->branch->op == IBC_BRANCH_OP_ELSE ||
                        &pred->branch->instr == ibc_instr_prev(&merge->instr)));
      }
      break;

   case IBC_MERGE_OP_START:
      ibc_assert(s, list_is_empty(&merge->preds));
      break;

   default:
      ibc_assert(s, !"TODO: Finish merge instruction validation");
   }
}

static void
ibc_validate_branch_instr(struct ibc_validate_state *s,
                          const ibc_branch_instr *branch)
{
   ibc_assert(s, branch->instr.simd_width == s->shader->simd_width);

   if (ibc_assert(s, branch->block_start))
      ibc_assert(s, branch->block_start->block_end == branch);

   switch (branch->op) {
   case IBC_BRANCH_OP_NEXT:
      ibc_assert(s, branch->jump == NULL);
      ibc_assert(s, branch->merge == NULL);
      break;

   case IBC_BRANCH_OP_IF:
      /* IF instructions must be predicated */
      ibc_assert(s, branch->instr.predicate != BRW_PREDICATE_NONE);

      /* IF instructions can jump to an ENDIF or an ELSE */
      if (ibc_assert(s, branch->jump) &&
          branch->jump->op != IBC_MERGE_OP_ENDIF) {
         /* The else is actually the branch instruction at the end of the
          * previous block.
          */
         const ibc_branch_instr *jump_prev =
            ibc_instr_as_branch(ibc_instr_prev(&branch->jump->instr));
         ibc_assert(s, jump_prev->op == IBC_BRANCH_OP_ELSE);
      }
      /* Must merge at an endif */
      ibc_assert(s, branch->merge && branch->merge->op == IBC_MERGE_OP_ENDIF);
      break;

   case IBC_BRANCH_OP_ELSE: {
      ibc_assert(s, branch->jump && branch->jump->op == IBC_MERGE_OP_ENDIF);
      ibc_assert(s, branch->merge && branch->merge->op == IBC_MERGE_OP_ENDIF);
      const ibc_merge_instr *next_merge =
         ibc_instr_as_merge(ibc_instr_next(&branch->instr));
      ibc_assert(s, next_merge->op == IBC_MERGE_OP_MERGE);
      list_for_each_entry(ibc_merge_pred, pred, &next_merge->preds, link) {
         ibc_assert(s, pred->branch &&
                       (pred->branch == branch ||
                        pred->branch->op == IBC_BRANCH_OP_IF));
      }
      break;
   }

   case IBC_BRANCH_OP_END:
      ibc_assert(s, branch->jump == NULL);
      ibc_assert(s, branch->merge == NULL);
      break;

   default:
      ibc_assert(s, !"TODO: Finish branch instruction validation");
   }

   ibc_assert(s, s->block_start == branch->block_start);
   s->block_start = NULL;
}

static void
ibc_validate_phi_instr(struct ibc_validate_state *s,
                       const ibc_phi_instr *phi)
{
   ibc_assert(s, phi->instr.predicate == BRW_PREDICATE_NONE);
   ibc_assert(s, phi->dest.file == IBC_REG_FILE_LOGICAL);

   ibc_validate_reg_ref(s, &phi->dest, true, 0, phi->num_comps,
                        phi->instr.simd_group, phi->instr.simd_width);

   const ibc_merge_instr *merge = s->block_start;

   assert(s->tmp_set->entries == 0);
   list_validate(&phi->srcs);
   ibc_foreach_phi_src(src, phi) {
      ibc_validate_reg_ref(s, &src->ref, false, 0, phi->num_comps,
                           phi->instr.simd_group, phi->instr.simd_width);

      /* Check for uniqueness of the predecessors */
      bool already_seen = false;
      _mesa_set_search_and_add(s->tmp_set, src->pred, &already_seen);
      ibc_assert(s, !already_seen);

      /* Check to make sure we're one of the merge's logical predecessors */
      bool found = false;
      list_for_each_entry(const ibc_merge_pred, pred, &merge->preds, link) {
         if (pred->branch == src->pred) {
            ibc_assert(s, pred->logical);
            found = true;
            break;
         }
      }
      ibc_assert(s, found);
   }

   /* Check that we got all the logical predecessors */
   list_for_each_entry(ibc_merge_pred, pred, &merge->preds, link) {
      if (pred->logical)
         ibc_assert(s, _mesa_set_search(s->tmp_set, pred->branch));
   }

   /* Clear out the tmp set now that we're done with it */
   _mesa_set_clear(s->tmp_set, NULL);
}

static void
ibc_validate_instr(struct ibc_validate_state *s, const ibc_instr *instr)
{
   s->instr = instr;

   ibc_assert(s, instr->simd_group < s->shader->simd_width);
   ibc_assert(s, instr->simd_width <= s->shader->simd_width);
   ibc_assert(s, instr->simd_group + instr->simd_width <= s->shader->simd_width);
   ibc_assert(s, instr->simd_group % instr->simd_width == 0);

   /* We technically don't need to but we require all WE_all instructions to
    * have a SIMD group of 0.
    */
   ibc_assert(s, !instr->we_all || instr->simd_group == 0);

   if (instr->predicate != BRW_PREDICATE_NONE) {
      /* The ANY*H or ALL*H predicate group threads into groups so we need to
       * align the instruction bits accordingly.
       */
      unsigned pred_bits = brw_predicate_bits(instr->predicate);
      assert(util_is_power_of_two_nonzero(pred_bits));
      unsigned pred_simd_group = instr->simd_group & ~(pred_bits - 1);
      unsigned pred_simd_width = MAX2(instr->simd_width, pred_bits);

      ibc_assert(s, instr->flag.file != IBC_REG_FILE_NONE);
      ibc_assert(s, instr->flag.type == IBC_TYPE_FLAG);
      ibc_validate_reg_ref(s, &instr->flag, false, 0, 1,
                           pred_simd_group, pred_simd_width);
   } else if (instr->type != IBC_INSTR_TYPE_ALU ||
              ibc_instr_as_alu(instr)->cmod == BRW_CONDITIONAL_NONE) {
      ibc_validate_null_reg_ref(s, &instr->flag);
   }

   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      ibc_validate_alu_instr(s, ibc_instr_as_alu(instr));
      return;

   case IBC_INSTR_TYPE_SEND:
      ibc_validate_send_instr(s, ibc_instr_as_send(instr));
      return;

   case IBC_INSTR_TYPE_INTRINSIC:
      ibc_validate_intrinsic_instr(s, ibc_instr_as_intrinsic(instr));
      return;

   case IBC_INSTR_TYPE_MERGE:
      ibc_validate_merge_instr(s, ibc_instr_as_merge(instr));
      return;

   case IBC_INSTR_TYPE_BRANCH:
      ibc_validate_branch_instr(s, ibc_instr_as_branch(instr));
      return;

   case IBC_INSTR_TYPE_PHI:
      ibc_validate_phi_instr(s, ibc_instr_as_phi(instr));
      return;
   }

   unreachable("Invalid instruction type");
}

static void
ibc_validate_reg_pre(struct ibc_validate_state *s, const ibc_reg *reg)
{
   list_validate(&reg->writes);

   struct reg_validate_state *reg_state =
      rzalloc(s->mem_ctx, struct reg_validate_state);

   reg_state->writes = _mesa_pointer_set_create(s->mem_ctx);
   if (reg->is_wlr)
      reg_state->next_write_link = reg->writes.next;

   _mesa_hash_table_insert(s->reg_state, reg, reg_state);

   switch (reg->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      ibc_assert(s, !"Invalid register file for an actual register");
      return;

   case IBC_REG_FILE_LOGICAL:
      ibc_assert(s, reg->logical.bit_size == 1 ||
                    reg->logical.bit_size == 8 ||
                    reg->logical.bit_size == 16 ||
                    reg->logical.bit_size == 32 ||
                    reg->logical.bit_size == 64);

      ibc_assert(s, reg->logical.num_comps >= 1 &&
                    reg->logical.num_comps <= 16);

      ibc_assert(s, reg->logical.simd_group < 32);
      ibc_assert(s, reg->logical.simd_width <= 32);
      ibc_assert(s, util_is_power_of_two_nonzero(reg->logical.simd_width));
      ibc_assert(s, reg->logical.simd_group % reg->logical.simd_width == 0);
      return;

   case IBC_REG_FILE_HW_GRF:
      ibc_assert(s, util_is_power_of_two_or_zero(reg->hw_grf.align));
      ibc_assert(s, reg->hw_grf.align > 0);
      return;

   case IBC_REG_FILE_FLAG:
      ibc_assert(s, reg->flag.bits <= 32);
      ibc_assert(s, reg->flag.align_mul >= 1 && reg->flag.align_mul <= 2);
      ibc_assert(s, reg->flag.bits <= reg->flag.align_mul * 16);
      ibc_assert(s, reg->flag.align_offset < reg->flag.align_mul);
      return;
   }

   unreachable("Invalid register file");
}

static void
ibc_validate_reg_post(struct ibc_validate_state *s, const ibc_reg *reg)
{
   struct reg_validate_state *reg_state =
      _mesa_hash_table_search(s->reg_state, reg)->data;

   ibc_reg_foreach_write(ref, reg) {
      struct set_entry *entry = _mesa_set_search(reg_state->writes, ref);
      ibc_assert(s, entry);
      _mesa_set_remove(reg_state->writes, entry);
   }

   if (reg_state->writes->entries != 0) {
      fprintf(stderr, "extra entries in register writes:\n");
      set_foreach(reg_state->writes, entry)
         fprintf(stderr, "%p\n", entry->key);

      abort();
   }

   if (reg->is_wlr)
      ibc_assert(s, reg_state->next_write_link == &reg->writes);
}

void
ibc_validate_shader(const ibc_shader *shader)
{
   struct ibc_validate_state s = {
      .mem_ctx = ralloc_context(NULL),
      .shader = shader,
   };
   s.tmp_set = _mesa_pointer_set_create(s.mem_ctx);

   ibc_assert(&s, shader->simd_width <= 32);

   s.reg_state = _mesa_pointer_hash_table_create(s.mem_ctx);
   ibc_foreach_reg(reg, shader)
      ibc_validate_reg_pre(&s, reg);

   list_validate(&shader->instrs);

   const ibc_instr *start_instr =
      LIST_ENTRY(const ibc_instr, shader->instrs.next, link);
   ibc_assert(&s, start_instr->type == IBC_INSTR_TYPE_MERGE &&
                  ibc_instr_as_merge(start_instr)->op == IBC_MERGE_OP_START);

   const ibc_instr *end_instr =
      LIST_ENTRY(const ibc_instr, shader->instrs.prev, link);
   ibc_assert(&s, end_instr->type == IBC_INSTR_TYPE_BRANCH &&
                  ibc_instr_as_branch(end_instr)->op == IBC_BRANCH_OP_END);

   ibc_foreach_instr(instr, shader)
      ibc_validate_instr(&s, instr);

   ibc_foreach_reg(reg, shader)
      ibc_validate_reg_post(&s, reg);

   ralloc_free(s.mem_ctx);
}
