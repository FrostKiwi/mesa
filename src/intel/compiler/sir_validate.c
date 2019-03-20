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

#include "sir.h"

#include <util/bitscan.h>

struct sir_validate_state {
   const sir_shader *shader;
};

static bool
_sir_assert(struct sir_validate_state *s, int line,
            const char *expr, bool value)
{
   if (likely(value))
      return true;

   fprintf(stderr, "sir_validate:%d Assertion failed: %s", line, expr);
   abort();

   /* For when this does something more interesting */
   return false;
}
#define sir_assert(state, expr) _sir_assert(state, __LINE__, #expr, expr)

static void
sir_validate_logical_reg_ref(struct sir_validate_state *s,
                             const sir_reg_ref *ref,
                             unsigned bit_size,
                             unsigned num_comps,
                             unsigned src_simd_width,
                             unsigned src_simd_group)
{
   const sir_logical_reg *reg = &ref->reg->logical;

   sir_assert(s, reg->bit_size == bit_size);
   sir_assert(s, ref->comp + num_comps <= reg->num_comps);
   sir_assert(s, src_simd_group >= reg->simd_group);
   sir_assert(s, src_simd_group + src_simd_width <=
                 reg->simd_group + reg->simd_width);
}

static void
sir_validate_flag_reg_ref(struct sir_validate_state *s,
                          const sir_reg_ref *ref,
                          unsigned src_simd_width,
                          unsigned src_simd_group)
{
   const sir_flag_reg *reg = &ref->reg->flag;

   unsigned reg_simd_group = (reg->subnr % 2) * 16;
   unsigned reg_simd_width = reg->bits;

   sir_assert(s, src_simd_group >= reg_simd_group);
   sir_assert(s, src_simd_group + src_simd_width <=
                 reg_simd_group + reg_simd_width);
}

static void
sir_validate_alu_src(struct sir_validate_state *s,
                     const sir_alu_instr *alu, const sir_alu_src *src)
{
   switch (src->file) {
   case SIR_REG_FILE_NONE:
      sir_assert(s, src->reg.reg == NULL);
      return;

   case SIR_REG_FILE_IMM:
      /* TODO */
      return;

   case SIR_REG_FILE_LOGICAL:
      if (sir_assert(s, src->reg.reg)) {
         sir_assert(s, src->reg.reg->file == SIR_REG_FILE_LOGICAL);
         sir_validate_logical_reg_ref(s, &src->reg,
                                      sir_type_bit_size(src->type), 1,
                                      alu->instr.simd_width,
                                      alu->instr.simd_group);
      }
      return;

   case SIR_REG_FILE_HW_GRF:
      /* TODO */
      return;
   }

   unreachable("Invalid register file");
}

static void
sir_validate_alu_dst(struct sir_validate_state *s,
                     const sir_alu_instr *alu, const sir_alu_dest *dest)
{
   switch (dest->file) {
   case SIR_REG_FILE_NONE:
      sir_assert(s, dest->reg.reg == NULL);
      return;

   case SIR_REG_FILE_IMM:
      sir_assert(s, !"Immediates are not allowed in destinations");
      return;

   case SIR_REG_FILE_LOGICAL:
      sir_assert(s, !alu->instr.we_all);
      if (sir_assert(s, dest->reg.reg)) {
         sir_assert(s, dest->reg.reg->file == SIR_REG_FILE_LOGICAL);
         sir_validate_logical_reg_ref(s, &dest->reg,
                                      sir_type_bit_size(dest->type), 1,
                                      alu->instr.simd_width,
                                      alu->instr.simd_group);
      }
      return;

   case SIR_REG_FILE_HW_GRF:
      /* TODO */
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
sir_validate_alu_instr(struct sir_validate_state *s, const sir_alu_instr *alu)
{
   if (alu->cmod != BRW_CONDITIONAL_NONE) {
      sir_assert(s, brw_predicate_bits(alu->instr.predicate) == 1);
      sir_validate_flag_reg_ref(s, &alu->instr.flag, alu->instr.simd_width,
                                alu->instr.simd_group);
   }

   sir_validate_alu_dst(s, alu, &alu->dest);

   for (unsigned i = 0; i < 3 /* TODO */; i++)
      sir_validate_alu_src(s, alu, &alu->src[i]);
}

static void
sir_validate_instr(struct sir_validate_state *s, const sir_instr *instr)
{
   if (instr->predicate != BRW_PREDICATE_NONE) {
      /* The ANY*H or ALL*H predicate group threads into groups so we need to
       * align the instruction bits accordingly.
       *
       * TODO: This should go in a helper somewhere.
       */
      unsigned pred_bits = brw_predicate_bits(instr->predicate);
      assert(util_is_power_of_two_nonzero(pred_bits));
      sir_validate_flag_reg_ref(s, &instr->flag,
                                MAX2(instr->simd_width, pred_bits),
                                instr->simd_group & (pred_bits - 1));
   }

   switch (instr->type) {
   case SIR_INSTR_TYPE_ALU:
      sir_validate_alu_instr(s, sir_instr_as_alu(instr));
      return;

   case SIR_INSTR_TYPE_SEND:
      /* TODO */
      return;

   case SIR_INSTR_TYPE_INTRINSIC:
      /* TODO */
      return;
   }

   unreachable("Invalid instruction type");
}

static void
sir_validate_block(struct sir_validate_state *s, const sir_block *block)
{
   sir_foreach_instr(instr, block)
      sir_validate_instr(s, instr);

#if 0 /* TODO */
   /* The last instruction in the block must be a jump */
   sir_assert(s, !list_is_empty(&block->instrs));
   sir_foreach_instr_reverse(instr, block) {
      sir_assert(s, instr->type == SIR_INSTR_TYPE_JUMP);
      break;
   }
#endif
}

void
sir_validate_shader(const sir_shader *shader)
{
   struct sir_validate_state s = {
      .shader = shader,
   };

   sir_foreach_block(block, shader)
      sir_validate_block(&s, block);
}
