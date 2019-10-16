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
#include <util/debug.h>
#include <util/hash_table.h>
#include <util/set.h>

/* Since this file is just a pile of asserts, don't bother compiling it if
 * we're not building a debug build.
 */
#ifndef NDEBUG

struct ibc_validate_state {
   void *mem_ctx;

   /* Pointer set for various things to use to check for uniqueness.  We just
    * always keep one around so we can avoid re-allocating it all the time.
    */
   struct set *tmp_set;

   const ibc_shader *shader;
   const ibc_flow_instr *block_start;
   const ibc_instr *instr;

   /* ibc_reg* -> reg_validate_state* */
   struct hash_table *reg_state;
};

struct reg_validate_state {
   struct set *writes;
   const ibc_flow_instr *write_block_start;
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
ibc_validate_null_ref(struct ibc_validate_state *s,
                      const ibc_ref *ref)
{
   ibc_assert(s, ref->file == IBC_FILE_NONE);
   ibc_assert(s, ref->reg == NULL);
}

static bool
ref_is_none_or_reg(ibc_ref *ref,
                   UNUSED int num_bytes,
                   UNUSED int num_comps,
                   UNUSED uint8_t simd_group,
                   UNUSED uint8_t simd_width,
                   void *_reg)
{
   if (ref->file == IBC_FILE_NONE)
      return true;

   if (ref->file == IBC_FILE_IMM)
      return false;

   return ref->reg == _reg;
}

static void
ibc_validate_ref(struct ibc_validate_state *s,
                 const ibc_ref *ref, const ibc_reg_write *write,
                 int num_bytes, int num_comps,
                 unsigned ref_simd_group, unsigned ref_simd_width)
{
   switch (ref->file) {
   case IBC_FILE_NONE:
      ibc_assert(s, num_comps == 0 && num_bytes == 0);
      ibc_validate_null_ref(s, ref);
      return;

   case IBC_FILE_IMM:
      ibc_assert(s, write == NULL);
      ibc_assert(s, num_comps == 1 && num_bytes == -1);
      return;

   case IBC_FILE_HW_GRF:
   case IBC_FILE_FLAG:
   case IBC_FILE_ACCUM:
      ibc_assert(s, ref->reg == NULL || ref->file == ref->reg->file);
      break;

   case IBC_FILE_LOGICAL:
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

   if (write) {
      if (ref->reg) {
         ibc_assert(s, write->instr == s->instr);
         _mesa_set_add(reg_state->writes, write);
         if (ref->reg->is_wlr) {
            if (reg_state->write_block_start == NULL) {
               reg_state->write_block_start = s->block_start;
            } else {
               ibc_assert(s, reg_state->write_block_start == s->block_start);
            }
            ibc_assert(s, reg_state->next_write_link == &write->link);
            reg_state->next_write_link = write->link.next;
         }
      }
   } else {
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
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      unreachable("Handled above with an early return");

   case IBC_FILE_LOGICAL: {
      const ibc_logical_reg *lreg = &ref->reg->logical;
      const struct ibc_ref_logical *lref = &ref->logical;

      if (num_comps == -1) {
         /* Byte-wise access of logical registers is allowed but it has strict
          * restrictions and assumes the register is tightly packed.
          */
         ibc_assert(s, ibc_type_bit_size(ref->type) >= 8);
         ibc_assert(s, ibc_type_bit_size(ref->type) == lreg->bit_size);
         ibc_assert(s, !lref->broadcast);
         unsigned comp_size_B = (lreg->bit_size / 8) * lreg->simd_width;
         ibc_assert(s, num_bytes % comp_size_B == 0);
         num_comps = num_bytes / comp_size_B;
         if (num_comps > 1) {
            /* If we are reading more than one component, we have to be packed
             * and have an exactly matching SIMD group.
             */
            ibc_assert(s, ref_simd_group == lreg->simd_group);
            ibc_assert(s, ref_simd_width == lreg->simd_width);
         }
      } else {
         ibc_assert(s, num_bytes == -1);
      }

      if (lreg->bit_size < 8)
         ibc_assert(s, lref->byte == 0);
      else
         ibc_assert(s, (lref->byte + 1) * 8 <= lreg->bit_size);

      ibc_assert(s, num_comps > 0);
      ibc_assert(s, lref->comp + num_comps <= lreg->num_comps);

      if (lref->broadcast) {
         ibc_assert(s, !write);
         ibc_assert(s, lref->simd_channel >= lreg->simd_group);
         ibc_assert(s, lref->simd_channel <=
                       lreg->simd_group + lreg->simd_width);
      } else {
         ibc_assert(s, lref->simd_channel == 0);
         if (lreg->simd_width == 1) {
            /* If the register is a scalar (only one SIMD channel), the
             * broadcast is implicit with channel 0.  There's nothing to
             * validate for reads for SIMD channels in this case.
             */
            if (write) {
               ibc_assert(s, ref_simd_group == 0);
               ibc_assert(s, ref_simd_width == 1);
               ibc_assert(s, s->instr->we_all);
            }
         } else {
            ibc_assert(s, ref_simd_group >= lreg->simd_group);
            ibc_assert(s, ref_simd_group + ref_simd_width <=
                          lreg->simd_group + lreg->simd_width);
            if (write)
               ibc_assert(s, !s->instr->we_all);
         }
      }
      return;
   }

   case IBC_FILE_HW_GRF: {
      const struct ibc_ref_hw_grf *hw_ref = &ref->hw_grf;

      ibc_assert(s, hw_ref->hstride % ibc_type_byte_size(ref->type) == 0);
      ibc_assert(s, hw_ref->vstride % ibc_type_byte_size(ref->type) == 0);
      if (write)
         ibc_assert(s, hw_ref->hstride > 0);

      if (num_bytes == -1) {
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
         ibc_assert(s, num_comps == -1);
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

   case IBC_FILE_FLAG:
      ibc_assert(s, num_comps == 1 && num_bytes == -1);
      if (ref->type == IBC_TYPE_FLAG) {
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
      } else {
         ibc_assert(s, ibc_type_bit_size(ref->type) == 16 ||
                       ibc_type_bit_size(ref->type) == 32);
         ibc_assert(s, ref->flag.bit % ibc_type_bit_size(ref->type) == 0);
         if (ref->reg) {
            ibc_assert(s, ref->flag.bit + ibc_type_bit_size(ref->type) <=
                          ref->reg->flag.bits);
         } else {
            ibc_assert(s, ref->flag.bit + ibc_type_bit_size(ref->type) <= 64);
         }
      }
      return;

   case IBC_FILE_ACCUM:
      ibc_assert(s, num_comps == 1 && num_bytes == -1);
      if (ref->reg) {
         ibc_assert(s, ref->type == ref->reg->accum.type);
         ibc_assert(s, ref->accum.chan % ref_simd_width == 0);
         ibc_assert(s, ref->accum.chan + ref_simd_width <=
                       ref->reg->accum.channels);

         /* Any given accumulator can only be accessed from one block */
         if (reg_state->write_block_start == NULL) {
            reg_state->write_block_start = s->block_start;
         } else {
            ibc_assert(s, reg_state->write_block_start == s->block_start);
         }
      } else {
         ibc_assert(s, ref->accum.chan + ref_simd_width <=
                       ibc_hw_accum_reg_width(ref->type));
      }
      return;
   }

   unreachable("Invalid register file");
}

static void
ibc_validate_alu_instr(struct ibc_validate_state *s, const ibc_alu_instr *alu)
{
   if (!ibc_assert(s, alu->op < IBC_ALU_NUM_OPS))
      return;

   const ibc_alu_op_info *alu_info = &ibc_alu_op_infos[alu->op];

   for (unsigned i = 0; i < alu_info->num_srcs; i++) {
      ibc_assert(s, alu->src[i].ref.file != IBC_FILE_NONE);
      ibc_assert(s, alu->src[i].ref.type == IBC_TYPE_FLAG ||
                    ibc_type_base_type(alu->src[i].ref.type) != IBC_TYPE_INVALID);
      ibc_validate_ref(s, &alu->src[i].ref, NULL, -1, 1,
                       alu->instr.simd_group,
                       alu->instr.simd_width);
      ibc_assert(s, (alu->src[i].mod & ~alu_info->supported_src_mods) == 0);
   }

   if ((ibc_alu_op_infos[alu->op].props & IBC_ALU_OP_PROP_READS_ACCUM) ||
       alu->accum_wr_en) {
      unsigned hw_accum_reg_width = ibc_hw_accum_reg_width(alu->accum.type);
      unsigned offset = alu->accum.reg ?
                        alu->accum.reg->accum.align_offset : 0;
      /* The actual accumulator used by implicit accumulator access is
       * entirely determined by the SIMD group of the instruction combined
       * with the size of the hardware accumulator.
       */
      ibc_assert(s, offset + alu->accum.accum.chan ==
                    alu->instr.simd_group % hw_accum_reg_width);
   } else {
      ibc_validate_null_ref(s, &alu->accum);
   }

   if (ibc_alu_op_infos[alu->op].props & IBC_ALU_OP_PROP_READS_ACCUM) {
      ibc_assert(s, alu->accum.file == IBC_FILE_ACCUM);
      ibc_validate_ref(s, &alu->accum, NULL, -1, 1,
                       alu->instr.simd_group,
                       alu->instr.simd_width);
   }

   if (alu->accum_wr_en) {
      ibc_assert(s, alu->accum.file == IBC_FILE_ACCUM);
      ibc_validate_ref(s, &alu->accum, &alu->accum_write, -1, 1,
                       alu->instr.simd_group,
                       alu->instr.simd_width);
   }

   if (alu->cmod != BRW_CONDITIONAL_NONE) {
      if (alu->instr.flag.file == IBC_FILE_NONE) {
         ibc_validate_null_ref(s, &alu->instr.flag);
         ibc_assert(s, alu->op == IBC_ALU_OP_SEL);
      } else {
         ibc_assert(s, ibc_predicate_simd_width(alu->instr.predicate) == 1);
         ibc_assert(s, alu->instr.flag.file != IBC_FILE_NONE);
         ibc_assert(s, alu->instr.flag.type == IBC_TYPE_FLAG);
         ibc_validate_ref(s, &alu->instr.flag, &alu->cmod_write, -1, 1,
                          alu->instr.simd_group,
                          alu->instr.simd_width);
      }
   }

   ibc_assert(s, !alu->saturate ||
                 ibc_type_base_type(alu->dest.type) == IBC_TYPE_FLOAT);

   if (alu->dest.file == IBC_FILE_NONE) {
      ibc_validate_null_ref(s, &alu->dest);
   } else {
      ibc_validate_ref(s, &alu->dest, &alu->dest_write,
                       -1, 1,
                       alu->instr.simd_group,
                       alu->instr.simd_width);
   }
}

static void
ibc_validate_send_instr(struct ibc_validate_state *s,
                        const ibc_send_instr *send)
{
   if (send->has_side_effects)
      ibc_assert(s, !send->can_reorder);

   if (send->eot)
      ibc_assert(s, send->has_side_effects);

   switch (send->desc.file) {
   case IBC_FILE_NONE:
      ibc_validate_null_ref(s, &send->desc);
      break;

   case IBC_FILE_IMM:
      ibc_validate_ref(s, &send->desc, NULL, -1, 1,
                       0 /* simd_group */, 1 /* simd_width */);
      break;

   default:
      ibc_assert(s, send->desc.type == IBC_TYPE_UD);
      ibc_validate_ref(s, &send->desc, NULL,
                       ibc_type_byte_size(IBC_TYPE_UD), -1,
                       0 /* simd_group */, 1 /* simd_width */);
      break;
   }

   switch (send->ex_desc.file) {
   case IBC_FILE_NONE:
      ibc_validate_null_ref(s, &send->ex_desc);
      break;

   case IBC_FILE_IMM:
      ibc_validate_ref(s, &send->ex_desc, NULL, -1, 1,
                       0 /* simd_group */, 1 /* simd_width */);
      break;

   default:
      ibc_assert(s, send->ex_desc.type == IBC_TYPE_UD);
      ibc_validate_ref(s, &send->ex_desc, NULL,
                       ibc_type_byte_size(IBC_TYPE_UD), -1,
                       0 /* simd_group */, 1 /* simd_width */);
   }

   ibc_assert(s, send->mlen > 0);
   ibc_validate_ref(s, &send->payload[0], NULL,
                    send->mlen * 32, -1,
                    send->instr.simd_group,
                    send->instr.simd_width);

   if (send->ex_mlen > 0) {
      ibc_validate_ref(s, &send->payload[1], NULL,
                       send->ex_mlen * 32, -1,
                       send->instr.simd_group,
                       send->instr.simd_width);
   } else {
      ibc_validate_null_ref(s, &send->payload[1]);
   }

   if (send->rlen > 0) {
      ibc_validate_ref(s, &send->dest, &send->dest_write,
                       send->rlen * 32, -1,
                       send->instr.simd_group,
                       send->instr.simd_width);
   } else {
      ibc_validate_null_ref(s, &send->dest);
   }
}

static void
ibc_validate_intrinsic_instr(struct ibc_validate_state *s,
                             const ibc_intrinsic_instr *intrin)
{
   if (intrin->has_side_effects)
      ibc_assert(s, !intrin->can_reorder);

   for (unsigned i = 0; i < intrin->num_srcs; i++) {
      if (intrin->src[i].ref.file == IBC_FILE_NONE) {
         ibc_validate_null_ref(s, &intrin->src[i].ref);
      } else {
         ibc_validate_ref(s, &intrin->src[i].ref, NULL,
                          -1, intrin->src[i].num_comps,
                          intrin->src[i].simd_group,
                          intrin->src[i].simd_width);
      }
   }

   if (intrin->dest.file == IBC_FILE_NONE) {
      ibc_validate_null_ref(s, &intrin->dest);
   } else {
      ibc_validate_ref(s, &intrin->dest, &intrin->dest_write,
                       intrin->num_dest_bytes,
                       intrin->num_dest_comps,
                       intrin->instr.simd_group,
                       intrin->instr.simd_width);
   }

   switch (intrin->op) {
   case IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL:
      ibc_assert(s, intrin->instr.simd_width == 1);
      ibc_assert(s, intrin->num_srcs == 0);
      break;

   case IBC_INTRINSIC_OP_SIMD_BROADCAST:
      ibc_assert(s, intrin->instr.simd_width == 1);
      ibc_assert(s, intrin->num_srcs == 2);
      ibc_assert(s, intrin->src[0].num_comps == intrin->num_dest_comps);
      ibc_assert(s, intrin->src[1].simd_width == 1);
      ibc_assert(s, intrin->src[1].num_comps == intrin->num_dest_comps);
      break;

   case IBC_INTRINSIC_OP_SIMD_ZIP: {
      ibc_assert(s, intrin->instr.simd_width % intrin->num_srcs == 0);
      const unsigned src_width = intrin->instr.simd_width / intrin->num_srcs;
      for (unsigned i = 0; i < intrin->num_srcs; i++) {
         ibc_assert(s, intrin->src[i].simd_group ==
                       intrin->instr.simd_group + i * src_width);
         ibc_assert(s, intrin->src[i].simd_width == src_width);
         ibc_assert(s, intrin->src[i].num_comps == intrin->num_dest_comps);
      }
      break;
   }

   case IBC_INTRINSIC_OP_MESSAGE: {
      unsigned num_bytes = 0;
      for (unsigned i = 0; i < intrin->num_srcs; i++) {
         ibc_assert(s, intrin->src[i].simd_width == intrin->instr.simd_width ||
                       intrin->src[i].simd_width == 1);
         ibc_assert(s, intrin->src[i].simd_width == 1 ||
                       intrin->src[i].num_comps == 1);
         unsigned src_bytes = ibc_type_byte_size(intrin->src[i].ref.type) *
                              intrin->src[i].simd_width *
                              intrin->src[i].num_comps;
         num_bytes += ALIGN(src_bytes, REG_SIZE);
      }
      ibc_assert(s, intrin->num_dest_bytes == num_bytes);
      ibc_assert(s, intrin->num_dest_comps == -1);
      break;
   }

   default:
      break;
   }
}

static void
ibc_validate_flow_successor(struct ibc_validate_state *s,
                            const ibc_flow_instr *flow,
                            const ibc_flow_instr *successor)
{
   bool found_in_preds = false;
   ibc_foreach_flow_pred(pred, successor) {
      if (pred->instr == flow) {
         found_in_preds = true;
         break;
      }
   }
   ibc_assert(s, found_in_preds);
}

static void
ibc_validate_flow_instr(struct ibc_validate_state *s,
                        const ibc_flow_instr *flow)
{
   if (flow->op == IBC_FLOW_OP_START)
      ibc_assert(s, s->block_start == NULL);
   else
      ibc_assert(s, s->block_start == ibc_flow_instr_prev(flow));
   s->block_start = flow;

   ibc_assert(s, flow->instr.simd_width == s->shader->simd_width);

   assert(s->tmp_set->entries == 0);
   list_validate(&flow->preds);
   ibc_foreach_flow_pred(pred, flow) {
      if (!ibc_assert(s, pred->instr != NULL))
         continue;

      /* Check for uniqueness of the predecessors */
      bool already_seen = false;
      _mesa_set_search_and_add(s->tmp_set, pred->instr, &already_seen);
      ibc_assert(s, !already_seen);

      ibc_assert(s, flow == pred->instr->jump ||
                    (ibc_flow_instr_falls_through(pred->instr) &&
                     flow == pred->instr));
   }
   _mesa_set_clear(s->tmp_set, NULL);

   if (ibc_flow_instr_falls_through(flow))
      ibc_validate_flow_successor(s, flow, flow);
   if (flow->jump)
      ibc_validate_flow_successor(s, flow, flow->jump);

   switch (flow->op) {
   case IBC_FLOW_OP_START:
   case IBC_FLOW_OP_END:
      /* START and END instructions must not be predicated */
      ibc_assert(s, flow->instr.predicate == IBC_PREDICATE_NONE);

      ibc_assert(s, flow->jump == NULL);
      ibc_assert(s, flow->merge == NULL);

      ibc_assert(s, list_is_empty(&flow->preds));
      return;

   case IBC_FLOW_OP_IF:
      /* IF instructions must be predicated */
      ibc_assert(s, flow->instr.predicate != IBC_PREDICATE_NONE);

      /* IF instructions can jump to an ENDIF or an ELSE */
      if (ibc_assert(s, flow->jump)) {
         ibc_assert(s, flow->jump->op == IBC_FLOW_OP_ELSE ||
                       flow->jump->op == IBC_FLOW_OP_ENDIF);
      }
      /* Must merge at an endif */
      ibc_assert(s, flow->merge && flow->merge->op == IBC_FLOW_OP_ENDIF);

      /* We can only have one predecessor and its the IF itself */
      ibc_foreach_flow_pred(pred, flow)
         ibc_assert(s, pred->instr == flow);
      return;

   case IBC_FLOW_OP_ELSE:
      /* ELSE instructions must not be predicated */
      ibc_assert(s, flow->instr.predicate == IBC_PREDICATE_NONE);

      ibc_assert(s, flow->jump && flow->jump->op == IBC_FLOW_OP_ENDIF);
      ibc_assert(s, flow->merge && flow->merge->op == IBC_FLOW_OP_ENDIF);

      ibc_foreach_flow_pred(pred, flow) {
         if (!ibc_assert(s, pred->instr != NULL))
            continue;

         ibc_assert(s, pred->instr->op == IBC_FLOW_OP_IF &&
                       pred->instr->jump == flow);
      }
      return;

   case IBC_FLOW_OP_ENDIF:
      /* ENDIF instructions must not be predicated */
      ibc_assert(s, flow->instr.predicate == IBC_PREDICATE_NONE);

      ibc_assert(s, flow->jump == NULL);
      ibc_assert(s, flow->merge == NULL);

      ibc_foreach_flow_pred(pred, flow) {
         if (!ibc_assert(s, pred->instr != NULL))
            continue;

         switch (pred->instr->op) {
         case IBC_FLOW_OP_IF:
            ibc_assert(s, pred->instr->jump == flow);
            ibc_assert(s, pred->instr->merge == flow);
            break;

         case IBC_FLOW_OP_ELSE:
            ibc_assert(s, pred->instr->jump == flow);
            ibc_assert(s, pred->instr->merge == flow);
            break;

         case IBC_FLOW_OP_ENDIF:
            ibc_assert(s, pred->instr == flow);
            break;

         default:
            ibc_assert(s, !"Invalid ENDIF predecessor");
         }
      }
      return;

   case IBC_FLOW_OP_DO:
      /* DO instructions must not be predicated */
      ibc_assert(s, flow->instr.predicate == IBC_PREDICATE_NONE);

      ibc_assert(s, flow->jump == NULL);
      ibc_assert(s, flow->merge && flow->merge->op == IBC_FLOW_OP_WHILE);

      ibc_foreach_flow_pred(pred, flow) {
         if (!ibc_assert(s, pred->instr != NULL))
            continue;

         ibc_assert(s, pred->instr == flow ||
                       (pred->instr->op == IBC_FLOW_OP_CONT &&
                        pred->instr->jump == flow) ||
                       (pred->instr->op == IBC_FLOW_OP_WHILE &&
                        pred->instr->jump == flow));
      }
      return;

   case IBC_FLOW_OP_BREAK:
      ibc_assert(s, flow->jump == flow->merge);
      ibc_assert(s, flow->jump && flow->jump->op == IBC_FLOW_OP_WHILE);

      ibc_foreach_flow_pred(pred, flow)
         ibc_assert(s, pred->instr == flow);
      return;

   case IBC_FLOW_OP_CONT:
      ibc_assert(s, flow->jump && flow->jump->op == IBC_FLOW_OP_DO);
      ibc_assert(s, flow->merge && flow->merge->op == IBC_FLOW_OP_WHILE);

      ibc_foreach_flow_pred(pred, flow)
         ibc_assert(s, pred->instr == flow);
      return;

   case IBC_FLOW_OP_WHILE:
      ibc_assert(s, flow->jump && flow->jump->op == IBC_FLOW_OP_DO);
      ibc_assert(s, flow->merge == NULL);

      ibc_foreach_flow_pred(pred, flow) {
         if (!ibc_assert(s, pred->instr != NULL))
            continue;

         ibc_assert(s, pred->instr == flow ||
                       (pred->instr->op == IBC_FLOW_OP_BREAK &&
                        pred->instr->jump == flow));
      }
      return;

   case IBC_FLOW_OP_HALT_JUMP:
      ibc_assert(s, flow->jump && flow->jump->op == IBC_FLOW_OP_HALT_MERGE);
      ibc_assert(s, flow->merge == NULL);

      ibc_assert(s, list_is_singular(&flow->preds));
      ibc_foreach_flow_pred(pred, flow)
         ibc_assert(s, pred->instr == flow);
      return;

   case IBC_FLOW_OP_HALT_MERGE:
      ibc_assert(s, flow->jump == NULL);
      ibc_assert(s, flow->merge == NULL);

      ibc_foreach_flow_pred(pred, flow) {
         if (!ibc_assert(s, pred->instr != NULL))
            continue;

         ibc_assert(s, pred->instr == flow ||
                       (pred->instr->op == IBC_FLOW_OP_HALT_JUMP &&
                        pred->instr->jump == flow));
      }
      return;
   }

   unreachable("Invalid flow instruction opcode");
}

static void
ibc_validate_instr(struct ibc_validate_state *s, const ibc_instr *instr)
{
   s->instr = instr;

   if (instr->we_all) {
      /* We technically don't need to but we require all WE_all instructions
       * to have a SIMD group of 0.
       */
      ibc_assert(s, !instr->we_all || instr->simd_group == 0);
   } else {
      ibc_assert(s, instr->simd_width >= 4);
      ibc_assert(s, instr->simd_group < s->shader->simd_width);
      ibc_assert(s, instr->simd_width <= s->shader->simd_width);
      ibc_assert(s, instr->simd_group + instr->simd_width <=
                    s->shader->simd_width);
      ibc_assert(s, instr->simd_group % instr->simd_width == 0);
   }

   if (instr->predicate != IBC_PREDICATE_NONE) {
      /* The ANY*H or ALL*H predicate group threads into groups so we need to
       * align the instruction bits accordingly.
       */
      unsigned pred_simd_width = ibc_predicate_simd_width(instr->predicate);
      assert(util_is_power_of_two_nonzero(pred_simd_width));
      unsigned ref_simd_group = instr->simd_group & ~(pred_simd_width - 1);
      unsigned ref_simd_width = MAX2(instr->simd_width, pred_simd_width);

      ibc_assert(s, instr->flag.file != IBC_FILE_NONE);
      ibc_assert(s, instr->flag.type == IBC_TYPE_FLAG);
      ibc_validate_ref(s, &instr->flag, NULL, -1, 1,
                       ref_simd_group, ref_simd_width);
   } else if (!ibc_instr_writes_flag(instr)) {
      ibc_validate_null_ref(s, &instr->flag);
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

   case IBC_INSTR_TYPE_FLOW:
      ibc_validate_flow_instr(s, ibc_instr_as_flow(instr));
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
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      ibc_assert(s, !"Invalid register file for an actual register");
      return;

   case IBC_FILE_LOGICAL:
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
      if (reg->logical.simd_width == 1) {
         ibc_assert(s, reg->logical.stride == 0);
      } else {
         ibc_assert(s, reg->logical.stride == 0 ||
                       reg->logical.stride >= reg->logical.bit_size / 8);
      }
      if (reg->logical.packed) {
         assert(reg->logical.bit_size >= 8);
         assert(reg->logical.stride == reg->logical.bit_size / 8);
      }
      return;

   case IBC_FILE_HW_GRF:
      ibc_assert(s, util_is_power_of_two_or_zero(reg->hw_grf.align));
      ibc_assert(s, reg->hw_grf.align > 0);
      return;

   case IBC_FILE_FLAG:
      ibc_assert(s, reg->flag.bits <= 32);
      ibc_assert(s, util_is_power_of_two_nonzero(reg->flag.bits));
      ibc_assert(s, util_is_power_of_two_nonzero(reg->flag.align_mul));
      ibc_assert(s, reg->flag.align_mul >= 8 && reg->flag.align_mul <= 32);
      ibc_assert(s, reg->flag.bits <= reg->flag.align_mul);
      ibc_assert(s, reg->flag.align_offset < reg->flag.align_mul);
      return;

   case IBC_FILE_ACCUM:
      ibc_assert(s, ibc_hw_accum_reg_width(reg->accum.type) > 0);
      ibc_assert(s, reg->accum.align_offset + reg->accum.channels <=
                    ibc_hw_accum_reg_width(reg->accum.type));
      return;
   }

   unreachable("Invalid register file");
}

static void
ibc_validate_reg_post(struct ibc_validate_state *s, const ibc_reg *reg)
{
   struct reg_validate_state *reg_state =
      _mesa_hash_table_search(s->reg_state, reg)->data;

   ibc_reg_foreach_write(write, reg) {
      struct set_entry *entry = _mesa_set_search(reg_state->writes, write);
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
   static int should_validate = -1;
   if (should_validate < 0)
      should_validate = env_var_as_boolean("IBC_VALIDATE", true);
   if (!should_validate)
      return;

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
   list_validate(&shader->flow_instrs);

   ibc_foreach_flow_instr(flow, shader)
      ibc_assert(&s, flow->instr.type == IBC_INSTR_TYPE_FLOW);

   const ibc_instr *start_instr =
      LIST_ENTRY(const ibc_instr, shader->instrs.next, link);
   ibc_assert(&s, start_instr->type == IBC_INSTR_TYPE_FLOW &&
                  ibc_instr_as_flow(start_instr)->op == IBC_FLOW_OP_START);

   const ibc_instr *end_instr =
      LIST_ENTRY(const ibc_instr, shader->instrs.prev, link);
   ibc_assert(&s, end_instr->type == IBC_INSTR_TYPE_FLOW &&
                  ibc_instr_as_flow(end_instr)->op == IBC_FLOW_OP_END);

   ibc_foreach_instr(instr, shader)
      ibc_validate_instr(&s, instr);

   ibc_foreach_reg(reg, shader)
      ibc_validate_reg_post(&s, reg);

   ralloc_free(s.mem_ctx);
}

#endif /* NDEBUG */
