/*
 * Copyright © 2019 Intel Corporation
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

#include "util/u_math.h"

struct split_reg {
   struct ibc_reg *reg;
   uint8_t start_comp;
};

struct reg_split_info {
   ibc_reg *reg;

   /* Each bit indicates that that component or SIMD channel and the next one
    * must remain together.
    */
   uint32_t simd_joint;
   uint8_t comp_joint;

   uint8_t num_regs;
   struct split_reg *split_regs;
};

struct split_regs_state {
   ibc_instr *instr;
   struct reg_split_info *splits;
};

static bool
mark_splits(ibc_reg_ref *ref,
            int8_t num_comps,
            uint8_t simd_group,
            uint8_t simd_width,
            void *_state)
{
   if (ref->file != IBC_REG_FILE_LOGICAL)
      return true;

   struct split_regs_state *state = _state;
   struct reg_split_info *split = &state->splits[ref->reg->index];
   assert(split->reg == ref->reg);

   if (ref->logical.broadcast) {
      simd_group = ref->logical.simd_channel;
      simd_width = 1;
   } else if (ref->reg->logical.simd_width == 1) {
      simd_group = 0;
      simd_width = 1;
   }
   assert(ref->reg->logical.simd_group <= simd_group &&
          simd_group + simd_width <= ref->reg->logical.simd_group +
                                     ref->reg->logical.simd_width);

   if (simd_width > 1)
      split->simd_joint |= ((1 << (simd_width - 1)) - 1) << simd_group;
   if (num_comps > 1)
      split->comp_joint |= ((1 << (num_comps - 1)) - 1) << ref->logical.comp;

   return true;
}

static bool
rewrite_ref_if_split(ibc_reg_ref *ref,
                     int8_t num_comps,
                     uint8_t simd_group,
                     uint8_t simd_width,
                     void *_state)
{
   if (ref->file != IBC_REG_FILE_LOGICAL)
      return true;

   struct split_regs_state *state = _state;
   struct reg_split_info *split = &state->splits[ref->reg->index];
   assert(split->reg == ref->reg);
   if (split->split_regs == NULL)
      return true;

   if (ref->logical.broadcast) {
      simd_group = ref->logical.simd_channel;
      simd_width = 1;
   } else if (ref->reg->logical.simd_width == 1) {
      simd_group = 0;
      simd_width = 1;
   }

   for (unsigned i = 0; i < split->num_regs; i++) {
      ibc_reg *reg = split->split_regs[i].reg;
      unsigned reg_start_comp = split->split_regs[i].start_comp;
      if (reg->logical.simd_group <= simd_group &&
          simd_group + simd_width <= reg->logical.simd_group +
                                     reg->logical.simd_width &&
          reg_start_comp <= ref->logical.comp &&
          ref->logical.comp + num_comps <= reg_start_comp +
                                           reg->logical.num_comps) {
         ibc_reg_ref new_ref = *ref;
         new_ref.reg = reg;
         new_ref.logical.comp -= reg_start_comp;
         if (ref->write_instr)
            ibc_instr_set_write_ref(state->instr, ref, new_ref);
         else
            *ref = new_ref;
         return true;
      }
   }

   unreachable("Failed to find split reg");
}

/** Splits logical registers into consecutively used chunks
 *
 * There are many cases with SIMD splitting or vector splitting operations
 * where a register can be split up into smaller pieces.  This pass looks over
 * all the uses of a logical register and determines the smallest chunks into
 * which the register can be split so that every read or write of that
 * register remains consecutive in all the dimensions.
 *
 * TODO: We need to special-case SIMD ZIP and VEC instructions and let the
 * pass split them as well.
 */
bool
ibc_split_logical_regs(ibc_shader *shader)
{
   bool progress = false;

   struct split_regs_state state = {};

   unsigned num_logical = 0;
   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_LOGICAL)
         reg->index = num_logical++;
   }

   state.splits = rzalloc_array(NULL, struct reg_split_info, num_logical);

   unsigned reg_idx = 0;
   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_LOGICAL) {
         assert(reg_idx == reg->index);
         state.splits[reg_idx++].reg = reg;
      }
   }
   assert(reg_idx == num_logical);

   ibc_foreach_instr_safe(instr, shader) {
      state.instr = instr;
      ibc_instr_foreach_read(instr, mark_splits, &state);
      ibc_instr_foreach_write(instr, mark_splits, &state);
   }

   for (unsigned i = 0; i < num_logical; i++) {
      struct reg_split_info *split = &state.splits[i];
      ibc_reg *reg = split->reg;
      assert(reg->index == i);

      const uint32_t simd_splits =
         ~split->simd_joint &
         (BITFIELD_MASK(reg->logical.simd_width) << reg->logical.simd_group);
      const uint32_t comp_splits =
         ~split->comp_joint & BITFIELD_MASK(reg->logical.num_comps);

      /* We have a "fake" split for the last entry */
      assert(simd_splits &
             (1 << (reg->logical.simd_group + reg->logical.simd_width - 1)));
      assert(comp_splits & (1 << (reg->logical.num_comps - 1)));

      split->num_regs = util_bitcount(simd_splits) *
                        util_bitcount(comp_splits);
      if (split->num_regs == 1)
         continue;

      progress = true;

      split->split_regs =
         ralloc_array(state.splits, struct split_reg, split->num_regs);

      unsigned split_idx = 0;
      unsigned start_comp = 0;
      uint32_t comp_split_tmp = comp_splits;
      while (comp_split_tmp) {
         const unsigned next_start_comp = u_bit_scan(&comp_split_tmp) + 1;
         const unsigned num_comps = next_start_comp - start_comp;

         unsigned simd_group = reg->logical.simd_group;
         uint32_t simd_split_tmp = simd_splits;
         while (simd_split_tmp) {
            const unsigned next_simd_group = u_bit_scan(&simd_split_tmp) + 1;
            const unsigned simd_width = next_simd_group - simd_group;

            split->split_regs[split_idx++] = (struct split_reg) {
               .reg = ibc_logical_reg_create(shader,
                                             reg->logical.bit_size, num_comps,
                                             simd_group, simd_width),
               .start_comp = start_comp,
            };

            simd_group = next_simd_group;
         }
         start_comp = next_start_comp;
      }
      assert(split_idx == split->num_regs);
   }

   ibc_foreach_instr_safe(instr, shader) {
      state.instr = instr;
      ibc_instr_foreach_read(instr, rewrite_ref_if_split, &state);
      ibc_instr_foreach_write(instr, rewrite_ref_if_split, &state);
   }

   return progress;
}
