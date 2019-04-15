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

   const ibc_shader *shader;
   const ibc_instr *instr;

   /* ibc_reg* -> reg_validate_state* */
   struct hash_table *reg_state;
};

struct reg_validate_state {
   struct set *writes;
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
ibc_validate_reg_ref(struct ibc_validate_state *s,
                     const ibc_reg_ref *ref, bool is_write,
                     unsigned num_comps,
                     unsigned ref_simd_group, unsigned ref_simd_width)
{
   switch (ref->file) {
   case IBC_REG_FILE_NONE:
      ibc_assert(s, ref->reg == NULL);
      return;

   case IBC_REG_FILE_IMM:
      /* TODO */
      return;

   case IBC_REG_FILE_LOGICAL:
   case IBC_REG_FILE_HW_GRF:
   case IBC_REG_FILE_FLAG:
      if (!ibc_assert(s, ref->reg))
         return;

      ibc_assert(s, ref->file == ref->reg->file);
      break;
   }

   struct reg_validate_state *reg_state = NULL;
   {
      struct hash_entry *state_entry =
         _mesa_hash_table_search(s->reg_state, ref->reg);
      if (ibc_assert(s, state_entry))
         reg_state = state_entry->data;
   }

   if (is_write) {
      ibc_assert(s, ref->write_instr == s->instr);
      _mesa_set_add(reg_state->writes, ref);
   } else {
      ibc_assert(s, ref->write_instr == NULL);
   }

   switch (ref->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Handled above with an early return");

   case IBC_REG_FILE_LOGICAL: {
      const ibc_logical_reg *lreg = &ref->reg->logical;
      const ibc_logical_reg_ref *lref = &ref->logical;
      if (lreg->bit_size < 8)
         ibc_assert(s, lref->byte == 0);
      else
         ibc_assert(s, (lref->byte + 1) * 8 <= lreg->bit_size);
      ibc_assert(s, lref->comp + num_comps <= lreg->num_comps);
      if (lref->broadcast) {
         ibc_assert(s, lref->simd_channel >= lreg->simd_group);
         ibc_assert(s, lref->simd_channel <=
                       lreg->simd_group + lreg->simd_width);
      } else if (lreg->simd_width == 1) {
         /* TODO: Do we want to require broadcast to be set? */
      } else {
         ibc_assert(s, ref_simd_group >= lreg->simd_group);
         ibc_assert(s, ref_simd_group + ref_simd_width <=
                       lreg->simd_group + lreg->simd_width);
      }
      return;
   }

   case IBC_REG_FILE_HW_GRF:
      /* TODO */
      return;

   case IBC_REG_FILE_FLAG: {
      const ibc_flag_reg *reg = &ref->reg->flag;

      unsigned reg_simd_group = (reg->subnr % 2) * 16;
      unsigned reg_simd_width = reg->bits;

      ibc_assert(s, ref_simd_group >= reg_simd_group);
      ibc_assert(s, ref_simd_group + ref_simd_width <=
                    reg_simd_group + reg_simd_width);
      return;
   }
   }

   unreachable("Invalid register file");
}

static void
ibc_validate_alu_src(struct ibc_validate_state *s,
                     const ibc_alu_instr *alu, const ibc_alu_src *src)
{
   ibc_assert(s, src->ref.file != IBC_REG_FILE_NONE);
   ibc_validate_reg_ref(s, &src->ref, false, 1,
                        alu->instr.simd_group,
                        alu->instr.simd_width);
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
   if (alu->cmod != BRW_CONDITIONAL_NONE) {
      ibc_assert(s, brw_predicate_bits(alu->instr.predicate) == 1);
      ibc_assert(s, alu->instr.flag.file != IBC_REG_FILE_NONE);
      ibc_validate_reg_ref(s, &alu->instr.flag, true, 1,
                           alu->instr.simd_group,
                           alu->instr.simd_width);
   }

   ibc_validate_reg_ref(s, &alu->dest, true, 1,
                        alu->instr.simd_group,
                        alu->instr.simd_width);

   for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++)
      ibc_validate_alu_src(s, alu, &alu->src[i]);
}

static void
ibc_validate_intrinsic_instr(struct ibc_validate_state *s,
                             const ibc_intrinsic_instr *intrin)
{
   ibc_validate_reg_ref(s, &intrin->dest, true,
                        intrin->num_dest_comps,
                        intrin->instr.simd_group,
                        intrin->instr.simd_width);

   for (unsigned i = 0; i < intrin->num_srcs; i++) {
      ibc_assert(s, intrin->src[i].simd_group >= intrin->instr.simd_group);
      ibc_assert(s, intrin->src[i].simd_group + intrin->src[i].simd_width <=
                    intrin->instr.simd_group + intrin->instr.simd_width);
      ibc_validate_reg_ref(s, &intrin->src[i].ref, false,
                           intrin->src[i].num_comps,
                           intrin->src[i].simd_group,
                           intrin->src[i].simd_width);
   }
}

static void
ibc_validate_instr(struct ibc_validate_state *s, const ibc_instr *instr)
{
   s->instr = instr;

   if (instr->predicate != BRW_PREDICATE_NONE) {
      /* The ANY*H or ALL*H predicate group threads into groups so we need to
       * align the instruction bits accordingly.
       *
       * TODO: This should go in a helper somewhere.
       */
      unsigned pred_bits = brw_predicate_bits(instr->predicate);
      assert(util_is_power_of_two_nonzero(pred_bits));
      ibc_validate_reg_ref(s, &instr->flag, false, 1,
                           instr->simd_group & (pred_bits - 1),
                           MAX2(instr->simd_width, pred_bits));
   }

   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      ibc_validate_alu_instr(s, ibc_instr_as_alu(instr));
      return;

   case IBC_INSTR_TYPE_SEND:
      /* TODO */
      return;

   case IBC_INSTR_TYPE_INTRINSIC:
      ibc_validate_intrinsic_instr(s, ibc_instr_as_intrinsic(instr));
      return;
   }

   unreachable("Invalid instruction type");
}

static void
ibc_validate_block(struct ibc_validate_state *s, const ibc_block *block)
{
   list_validate(&block->instrs);
   ibc_foreach_instr(instr, block)
      ibc_validate_instr(s, instr);

#if 0 /* TODO */
   /* The last instruction in the block must be a jump */
   ibc_assert(s, !list_is_empty(&block->instrs));
   ibc_foreach_instr_reverse(instr, block) {
      ibc_assert(s, instr->type == IBC_INSTR_TYPE_JUMP);
      break;
   }
#endif
}

static void
ibc_validate_reg_pre(struct ibc_validate_state *s, const ibc_reg *reg)
{
   list_validate(&reg->writes);

   struct reg_validate_state *reg_state =
      ralloc(s->mem_ctx, struct reg_validate_state);

   reg_state->writes = _mesa_pointer_set_create(s->mem_ctx);

   _mesa_hash_table_insert(s->reg_state, reg, reg_state);
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
}

void
ibc_validate_shader(const ibc_shader *shader)
{
   struct ibc_validate_state s = {
      .mem_ctx = ralloc_context(NULL),
      .shader = shader,
   };

   s.reg_state = _mesa_pointer_hash_table_create(s.mem_ctx);
   ibc_foreach_reg(reg, shader)
      ibc_validate_reg_pre(&s, reg);

   list_validate(&shader->blocks);
   ibc_foreach_block(block, shader)
      ibc_validate_block(&s, block);

   ibc_foreach_reg(reg, shader)
      ibc_validate_reg_post(&s, reg);

   ralloc_free(s.mem_ctx);
}
