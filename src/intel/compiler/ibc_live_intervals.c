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
#include "ibc_live_intervals.h"

#include "util/bitscan.h"

static bool
instr_is_predicated(ibc_instr *instr)
{
   if (instr->type == IBC_INSTR_TYPE_ALU &&
       ibc_instr_as_alu(instr)->op == IBC_ALU_OP_SEL)
      return false;

   return instr->predicate != BRW_PREDICATE_NONE;
}

static bool
reg_file_is_tracked(enum ibc_reg_file file)
{
   switch (file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
   case IBC_REG_FILE_FLAG:
      return false;
   case IBC_REG_FILE_LOGICAL:
   case IBC_REG_FILE_HW_GRF:
      return true;
   }
   unreachable("Invalid register file");
}

static bool
record_reg_write_sizes(ibc_reg_ref *ref,
                       UNUSED int8_t num_bytes, UNUSED int8_t num_comps,
                       uint8_t simd_group, uint8_t simd_width,
                       void *_state)
{
   ibc_live_intervals *live = _state;

   if (!reg_file_is_tracked(ref->file) || !ref->reg)
      return true;

   ibc_reg_live_intervals *rli = &live->regs[ref->reg->index];
   const unsigned byte_size = DIV_ROUND_UP(ibc_type_bit_size(ref->type), 8);

   rli->chunk_simd_width = MIN2(rli->chunk_simd_width, simd_width);
   rli->chunk_byte_size = MIN2(rli->chunk_byte_size, byte_size);

   return true;
}

static unsigned
reg_num_chunks(const ibc_reg *reg, ibc_live_intervals *live)
{
   assert(reg_file_is_tracked(reg->file));
   ibc_reg_live_intervals *rli = &live->regs[reg->index];

   if (rli->chunk_simd_width > 32) {
      /* In this case it's never written.  Just track reads with one chunk */
      assert(rli->chunk_byte_size == UINT8_MAX);
      assert(rli->chunk_simd_width == UINT8_MAX);
      return 1;
   }

   assert(util_is_power_of_two_nonzero(rli->chunk_byte_size));
   assert(util_is_power_of_two_nonzero(rli->chunk_simd_width));
   const unsigned byte_divisor = rli->chunk_byte_size;
   const unsigned simd_divisor = rli->chunk_simd_width;
   const unsigned byte_shift = ffs(byte_divisor) - 1;
   const unsigned simd_shift = ffs(simd_divisor) - 1;

   switch (reg->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Not an allocatable register file");
   case IBC_REG_FILE_LOGICAL: {
      const unsigned reg_byte_size = DIV_ROUND_UP(reg->logical.bit_size, 8);
      assert(reg_byte_size % byte_divisor == 0);
      const unsigned chunk_stride = reg_byte_size >> byte_shift;

      assert(reg->logical.simd_width % simd_divisor == 0);
      const unsigned reg_simd_width_chunks =
         reg->logical.simd_width >> simd_shift;

      return chunk_stride * reg_simd_width_chunks * reg->logical.num_comps;
   }
   case IBC_REG_FILE_HW_GRF:
      assert(reg->hw_grf.size % byte_divisor == 0);
      return reg->hw_grf.size >> byte_shift;
   case IBC_REG_FILE_FLAG:
      unreachable("TODO");
   }
   unreachable("Invalid register file");
}

void
ibc_live_intervals_reg_ref_chunks(const ibc_live_intervals *live,
                                  const ibc_reg_ref *ref,
                                  int8_t num_bytes, int8_t num_comps,
                                  uint8_t simd_group, uint8_t simd_width,
                                  BITSET_WORD *chunks)
{
   assert(reg_file_is_tracked(ref->file));
   const ibc_reg *reg = ref->reg;
   ibc_reg_live_intervals *rli = &live->regs[reg->index];

   if (rli->chunk_simd_width > 32) {
      /* In this case it's never written.  Just track reads with one chunk */
      assert(rli->chunk_byte_size == UINT8_MAX);
      assert(rli->chunk_simd_width == UINT8_MAX);
      BITSET_SET(chunks, 0);
      return;
   }

   assert(util_is_power_of_two_nonzero(rli->chunk_byte_size));
   assert(util_is_power_of_two_nonzero(rli->chunk_simd_width));
   const unsigned byte_divisor = rli->chunk_byte_size;
   const unsigned simd_divisor = rli->chunk_simd_width;
   const unsigned byte_shift = ffs(byte_divisor) - 1;
   const unsigned simd_shift = ffs(simd_divisor) - 1;

   const unsigned reg_byte_size = DIV_ROUND_UP(reg->logical.bit_size, 8);
   assert(util_is_power_of_two_nonzero(reg_byte_size));
   const unsigned ref_byte_size =
      DIV_ROUND_UP(ibc_type_bit_size(ref->type), 8);

   const unsigned ref_chunks =
      (ref_byte_size + byte_divisor - 1) >> byte_shift;

   switch (ref->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Not an allocatable register file");

   case IBC_REG_FILE_LOGICAL: {
      assert(num_comps >= 0);
      assert(ref->logical.byte % ref_byte_size == 0);
      assert(ref->logical.byte + ref_byte_size <= reg_byte_size);

      const unsigned ref_chunk = ref->logical.byte >> byte_shift;
      assert(reg_byte_size % byte_divisor == 0);
      const unsigned chunk_stride = reg_byte_size >> byte_shift;

      if (reg->logical.simd_width == 1) {
         for (unsigned c = 0; c < num_comps; c++) {
            const unsigned idx = ref_chunk + c * chunk_stride;
            for (unsigned i = 0; i < ref_chunks; i++)
               BITSET_SET(chunks, idx + i);
         }
      } else if (ref->logical.broadcast) {
         assert(ref->logical.simd_channel >= reg->logical.simd_group);
         assert(ref->logical.simd_channel <
                reg->logical.simd_group + reg->logical.simd_width);
         const unsigned rel_channel =
            ref->logical.simd_channel - reg->logical.simd_group;
         const unsigned rel_channel_chunk = rel_channel >> simd_shift;

         assert(num_comps == 1);
         const unsigned idx = ref_chunk + rel_channel_chunk * chunk_stride;
         for (unsigned i = 0; i < ref_chunks; i++)
            BITSET_SET(chunks, idx + i);
      } else {
         assert(simd_group >= reg->logical.simd_group);
         assert(simd_group + simd_width <=
                reg->logical.simd_group + reg->logical.simd_width);
         const unsigned rel_group = simd_group - reg->logical.simd_group;

         const unsigned simd_width_chunks =
            (simd_width + simd_divisor - 1) >> simd_shift;
         const unsigned rel_group_chunk = rel_group >> simd_shift;

         assert(reg->logical.simd_width % simd_divisor == 0);
         const unsigned reg_simd_width_chunks =
            reg->logical.simd_width >> simd_shift;

         assert(num_comps <= reg->logical.num_comps);
         for (unsigned c = 0; c < num_comps; c++) {
            for (unsigned s = 0; s < simd_width_chunks; s++) {
               const unsigned idx = ref_chunk +
                                    c * reg_simd_width_chunks * chunk_stride +
                                    (s + rel_group_chunk) * chunk_stride;

               for (unsigned i = 0; i < ref_chunks; i++)
                  BITSET_SET(chunks, idx + i);
            }
         }
      }
      break;
   }

   case IBC_REG_FILE_HW_GRF: {
      if (num_comps < 0) {
         assert(ref->hw_grf.hstride * ref->hw_grf.width == ref->hw_grf.vstride);
         assert(num_bytes % byte_divisor == 0);
         const unsigned num_chunks = num_bytes >> byte_shift;
         for (unsigned i = 0; i < num_chunks; i++)
            BITSET_SET(chunks, i);
      } else if (ref->hw_grf.hstride == 0 && ref->hw_grf.vstride == 0) {
         assert(num_comps == 1);
         const unsigned idx = ref->hw_grf.byte >> byte_shift;
         for (unsigned i = 0; i < ref_chunks; i++)
            BITSET_SET(chunks, idx + i);
      } else {
         assert(num_comps == 1);
         unsigned offset = ref->hw_grf.byte;
         unsigned horiz_offset = 0;
         for (unsigned c = 0; c < num_comps; c++) {
            for (unsigned s = 0; s < simd_width;) {
               assert(offset + horiz_offset + ref_byte_size <= reg->hw_grf.size);
               assert((offset + horiz_offset) % byte_divisor == 0);
               const unsigned idx = (offset + horiz_offset) >> byte_shift;
               for (unsigned i = 0; i < ref_chunks; i++)
                  BITSET_SET(chunks, idx + i);

               s++;
               assert(util_is_power_of_two_nonzero(ref->hw_grf.width));
               if (s == simd_width || (s & (ref->hw_grf.width - 1)) == 0) {
                  offset += ref->hw_grf.vstride;
                  horiz_offset = 0;
               } else {
                  horiz_offset += ref->hw_grf.hstride;
               }
            }
         }
      }
      break;
   }

   case IBC_REG_FILE_FLAG:
      unreachable("TODO");
   }
}

static ibc_live_intervals *
alloc_live_intervals(ibc_shader *shader, void *mem_ctx)
{
   ibc_live_intervals *live = ralloc(mem_ctx, ibc_live_intervals);

   live->num_regs = 0;
   ibc_foreach_reg(reg, shader) {
      if (!reg_file_is_tracked(reg->file)) {
         reg->index = UINT32_MAX;
         continue;
      }

      reg->index = live->num_regs++;
   }

   live->regs = ralloc_array(live, ibc_reg_live_intervals, live->num_regs);
   ibc_foreach_reg(reg, shader) {
      if (!reg_file_is_tracked(reg->file))
         continue;

      live->regs[reg->index] = (ibc_reg_live_intervals) {
         .chunk_simd_width = UINT8_MAX,
         .chunk_byte_size = UINT8_MAX,
         .physical_start = 0,
         .physical_end = 0,
      };
   }

   uint32_t num_instrs = 0;
   uint32_t num_blocks = 0;
   ibc_foreach_instr(instr, shader) {
      instr->index = num_instrs++;
      if (instr->type == IBC_INSTR_TYPE_MERGE)
         ibc_instr_as_merge(instr)->block_index = num_blocks++;

      ibc_instr_foreach_write(instr, record_reg_write_sizes, live);
   }

   live->num_chunks = 0;
   ibc_foreach_reg(reg, shader) {
      if (!reg_file_is_tracked(reg->file))
         continue;

      ibc_reg_live_intervals *rli = &live->regs[reg->index];
      rli->chunk_idx = live->num_chunks;
      rli->num_chunks = reg_num_chunks(reg, live);
      assert(rli->num_chunks < IBC_REG_LIVE_MAX_CHUNKS);
      live->num_chunks += rli->num_chunks;
   }

   live->chunk_live =
      rzalloc_array(live, struct interval_set *, live->num_chunks);
   for (uint32_t i = 0; i < live->num_regs; i++)
      live->regs[i].chunk_live = live->chunk_live + live->regs[i].chunk_idx;

   live->blocks = ralloc_array(live, ibc_block_live_sets, num_blocks);
   const uint32_t bitset_words = BITSET_WORDS(live->num_chunks);
   for (unsigned i = 0; i < num_blocks; i++) {
      live->blocks[i].def = rzalloc_array(live, BITSET_WORD, bitset_words);
      live->blocks[i].use = rzalloc_array(live, BITSET_WORD, bitset_words);
      live->blocks[i].livein = rzalloc_array(live, BITSET_WORD, bitset_words);
      live->blocks[i].liveout = rzalloc_array(live, BITSET_WORD, bitset_words);
      live->blocks[i].defin = rzalloc_array(live, BITSET_WORD, bitset_words);
      live->blocks[i].defout = rzalloc_array(live, BITSET_WORD, bitset_words);
   }

   return live;
}

struct setup_use_def_state {
   ibc_instr *instr;
   ibc_live_intervals *live;
   uint32_t block_index;
};

static bool
setup_block_use_def_for_read(ibc_reg_ref *ref,
                             int8_t num_bytes, int8_t num_comps,
                             uint8_t simd_group, uint8_t simd_width,
                             void *_state)
{
   BITSET_DECLARE(read, IBC_REG_LIVE_MAX_CHUNKS);
   struct setup_use_def_state *state = _state;

   if (!reg_file_is_tracked(ref->file) || !ref->reg)
      return true;

   ibc_block_live_sets *bls = &state->live->blocks[state->block_index];
   const unsigned num_chunks = state->live->regs[ref->reg->index].num_chunks;
   const unsigned chunk_idx = state->live->regs[ref->reg->index].chunk_idx;

   memset(read, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_reg_ref_chunks(state->live, ref, num_bytes, num_comps,
                                     simd_group, simd_width, read);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (!BITSET_TEST(read, i))
         continue;

      if (!BITSET_TEST(bls->def, chunk_idx + i))
         BITSET_SET(bls->use, chunk_idx + i);
   }

   return true;
}

static bool
setup_block_use_def_for_write(ibc_reg_ref *ref,
                              int8_t num_bytes, int8_t num_comps,
                              uint8_t simd_group, uint8_t simd_width,
                              void *_state)
{
   BITSET_DECLARE(written, IBC_REG_LIVE_MAX_CHUNKS);
   struct setup_use_def_state *state = _state;

   if (!reg_file_is_tracked(ref->file) || !ref->reg)
      return true;

   ibc_block_live_sets *bls = &state->live->blocks[state->block_index];
   const unsigned num_chunks = state->live->regs[ref->reg->index].num_chunks;
   const unsigned chunk_idx = state->live->regs[ref->reg->index].chunk_idx;

   memset(written, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_reg_ref_chunks(state->live, ref, num_bytes, num_comps,
                                     simd_group, simd_width, written);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (!BITSET_TEST(written, i))
         continue;

      if (!instr_is_predicated(state->instr) &&
          !BITSET_TEST(bls->use, chunk_idx + i))
         BITSET_SET(bls->def, chunk_idx + i);

      BITSET_SET(bls->defout, chunk_idx + i);
   }

   return true;
}

static void
compute_live_sets(ibc_shader *shader, ibc_live_intervals *live)
{
   struct setup_use_def_state state = {
      .live = live,
   };

   ibc_foreach_instr(instr, shader) {
      if (instr->type == IBC_INSTR_TYPE_MERGE)
         state.block_index = ibc_instr_as_merge(instr)->block_index;

      state.instr = instr;
      ibc_instr_foreach_read(instr, setup_block_use_def_for_read, &state);
      ibc_instr_foreach_write(instr, setup_block_use_def_for_write, &state);
   }

   const unsigned bitset_words = BITSET_WORDS(live->num_chunks);

   bool progress;
   do {
      progress = false;

      ibc_foreach_branch_instr_reverse(branch, shader) {
         ibc_merge_instr *merge = branch->block_start;
         ibc_block_live_sets *bls = &live->blocks[merge->block_index];

         /* Update livein */
         for (int i = 0; i < bitset_words; i++) {
            BITSET_WORD new_livein = (bls->use[i] |
                                      (bls->liveout[i] &
                                       ~bls->def[i]));
            if (new_livein & ~bls->livein[i]) {
               bls->livein[i] |= new_livein;
               progress = true;
            }
         }

         /* Update the liveout of our predecessors */
         list_for_each_entry(ibc_merge_pred, pred, &merge->preds, link) {
            if (!pred->logical)
               continue;

            ibc_block_live_sets *pred_bls =
               &live->blocks[pred->branch->block_start->block_index];

	    for (int i = 0; i < bitset_words; i++) {
               BITSET_WORD new_liveout = (bls->livein[i] &
                                          ~pred_bls->liveout[i]);
               if (new_liveout) {
                  pred_bls->liveout[i] |= new_liveout;
                  progress = true;
               }
	    }
         }
      }
   } while (progress);

   /* Propagate defin and defout down the CFG to calculate the union of live
    * variables potentially defined along any possible control flow path.
    */
   do {
      progress = false;
      ibc_foreach_merge_instr(merge, shader) {
         ibc_block_live_sets *bls = &live->blocks[merge->block_index];

         list_for_each_entry(ibc_merge_pred, pred, &merge->preds, link) {
            if (!pred->logical)
               continue;

            ibc_block_live_sets *pred_bls =
               &live->blocks[pred->branch->block_start->block_index];

	    for (int i = 0; i < bitset_words; i++) {
               BITSET_WORD new_def = pred_bls->defout[i] & ~bls->defin[i];
               if (new_def) {
                  bls->defin[i] |= new_def;
                  bls->defout[i] |= new_def;
                  progress = true;
               }
            }
         }
      }
   } while (progress);
}

struct extend_live_interval_state {
   ibc_instr *instr;
   ibc_live_intervals *live;
   BITSET_WORD *def;
};

static bool
extend_live_interval_for_read(ibc_reg_ref *ref,
                              int8_t num_bytes, int8_t num_comps,
                              uint8_t simd_group, uint8_t simd_width,
                              void *_state)
{
   BITSET_DECLARE(read, IBC_REG_LIVE_MAX_CHUNKS);
   struct extend_live_interval_state *state = _state;

   if (!reg_file_is_tracked(ref->file) || !ref->reg)
      return true;

   ibc_reg_live_intervals *rli = &state->live->regs[ref->reg->index];
   const unsigned num_chunks = rli->num_chunks;
   const unsigned chunk_idx = rli->chunk_idx;

   memset(read, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_reg_ref_chunks(state->live, ref, num_bytes, num_comps,
                                     simd_group, simd_width, read);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (!BITSET_TEST(read, i))
         continue;

      if (BITSET_TEST(state->def, chunk_idx + i)) {
         interval_set_extend_to(rli->chunk_live[i], state->instr->index + 1);

         assert(rli->physical_end > 0);
         rli->physical_end = MAX2(rli->physical_end, state->instr->index + 1);
      }
   }

   return true;
}

static bool
extend_live_interval_for_write(ibc_reg_ref *ref,
                               int8_t num_bytes, int8_t num_comps,
                               uint8_t simd_group, uint8_t simd_width,
                               void *_state)
{
   BITSET_DECLARE(write, IBC_REG_LIVE_MAX_CHUNKS);
   struct extend_live_interval_state *state = _state;

   if (!reg_file_is_tracked(ref->file) || !ref->reg)
      return true;

   ibc_reg_live_intervals *rli = &state->live->regs[ref->reg->index];
   const unsigned num_chunks = rli->num_chunks;
   const unsigned chunk_idx = rli->chunk_idx;

   memset(write, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_reg_ref_chunks(state->live, ref, num_bytes, num_comps,
                                     simd_group, simd_width, write);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (!BITSET_TEST(write, i))
         continue;

      if (BITSET_TEST(state->def, chunk_idx + i) &&
          instr_is_predicated(state->instr)) {
         /* This is a predicated re-definition of something that's already
          * defined, just extend liveness.
          */
         interval_set_extend_to(rli->chunk_live[i], state->instr->index + 1);
      } else {
         /* This is a (possibly predicated) brand new definition.  Start a new
          * live interval here.
          */
         rli->chunk_live[i] = interval_set_add_end(state->live,
                                                   rli->chunk_live[i],
                                                   state->instr->index,
                                                   state->instr->index + 1);
      }

      BITSET_SET(state->def, chunk_idx + i);
   }

   if (rli->physical_end == 0) {
      rli->physical_start = state->instr->index;
      rli->physical_end = state->instr->index + 1;
   } else {
      rli->physical_end = MAX2(rli->physical_end, state->instr->index + 1);
   }

   return true;
}

static void
compute_live_intervals(ibc_shader *shader, ibc_live_intervals *live)
{
   const unsigned bitset_words = BITSET_WORDS(live->num_chunks);
   struct extend_live_interval_state state = {
      .live = live,
      .def = rzalloc_array(live, BITSET_WORD, bitset_words),
   };

   ibc_foreach_instr(instr, shader) {
      if (instr->type == IBC_INSTR_TYPE_MERGE) {
         ibc_merge_instr *merge = ibc_instr_as_merge(instr);
         ibc_block_live_sets *bls = &live->blocks[merge->block_index];

         for (uint32_t w = 0; w < bitset_words; w++) {
            /* Set up def for this block */
            state.def[w] = bls->defin[w];

            /* We consider all defined and live-in instructions to be defined
             * by the merge instruction.
             */
            BITSET_WORD in = bls->livein[w] & bls->defin[w];
            while (in) {
               int b = u_bit_scan(&in);
               const uint32_t chunk_idx = w * BITSET_WORDBITS + b;
               live->chunk_live[chunk_idx] =
                  interval_set_add_end(live, live->chunk_live[chunk_idx],
                                       instr->index, instr->index + 1);
            }
         }

         /* If this merge is a DO instruction, we consider anything live-out
          * from the branch to the loop or anything live-in to the loop's
          * merge block to be physically live for the entire loop.
          */
         if (merge->op == IBC_MERGE_OP_DO) {
            ibc_merge_instr *_do = merge;
            ibc_branch_instr *branch_to_loop =
               ibc_instr_as_branch(ibc_instr_next(&merge->instr));
            assert(branch_to_loop->op == IBC_BRANCH_OP_NEXT);
            ibc_merge_instr *loop_merge = branch_to_loop->merge;
            ibc_branch_instr *_while =
               ibc_instr_as_branch(ibc_instr_prev(&loop_merge->instr));
            assert(_while->op == IBC_BRANCH_OP_WHILE);
            assert(_do->instr.index < _while->instr.index);

            ibc_block_live_sets *enter_bls =
               &live->blocks[branch_to_loop->block_start->block_index];
            ibc_block_live_sets *merge_bls =
               &live->blocks[loop_merge->block_index];

            for (uint32_t r = 0; r < live->num_regs; r++) {
               ibc_reg_live_intervals *rli = &live->regs[r];

               bool loop_live = false;
               for (uint32_t i = 0; i < rli->num_chunks; i++) {
                  if (BITSET_TEST(enter_bls->liveout, rli->chunk_idx + i) &&
                      BITSET_TEST(enter_bls->defout, rli->chunk_idx + i)) {
                     loop_live = true;
                     break;
                  }

                  if (BITSET_TEST(merge_bls->livein, rli->chunk_idx + i) &&
                      BITSET_TEST(merge_bls->defin, rli->chunk_idx + i)) {
                     loop_live = true;
                     break;
                  }
               }

               if (loop_live) {
                  if (rli->physical_end == 0) {
                     rli->physical_start = _do->instr.index;
                     rli->physical_end = _while->instr.index + 1;
                  } else {
                     assert(rli->physical_start < _do->instr.index);
                     rli->physical_end = MAX2(rli->physical_end,
                                              _while->instr.index + 1);
                  }
               }
            }
         }
      }

      state.instr = instr;
      ibc_instr_foreach_read(instr, extend_live_interval_for_read, &state);
      ibc_instr_foreach_write(instr, extend_live_interval_for_write, &state);

      if (instr->type == IBC_INSTR_TYPE_BRANCH) {
         ibc_branch_instr *branch = ibc_instr_as_branch(instr);
         ibc_merge_instr *merge = branch->block_start;
         ibc_block_live_sets *bls = &live->blocks[merge->block_index];

         for (uint32_t w = 0; w < bitset_words; w++) {
            /* We consider all defined and live-out instructions to be used
             * by the merge instruction.
             */
            BITSET_WORD in = bls->liveout[w] & state.def[w];
            while (in) {
               int b = u_bit_scan(&in);
               const uint32_t chunk_idx = w * BITSET_WORDBITS + b;
               interval_set_extend_to(live->chunk_live[chunk_idx],
                                      instr->index + 1);
            }
         }
      }
   }
}

ibc_live_intervals *
ibc_compute_live_intervals(ibc_shader *shader, void *mem_ctx)
{
   ibc_live_intervals *live = alloc_live_intervals(shader, mem_ctx);
   compute_live_sets(shader, live);
   compute_live_intervals(shader, live);

   for (uint32_t r = 0; r < live->num_regs; r++) {
      fprintf(stderr, "Reg %u (%u chunks), physical: [%u, %u)\n", r,
              live->regs[r].num_chunks,
              live->regs[r].physical_start,
              live->regs[r].physical_end);
      for (uint32_t c = 0; c < live->regs[r].num_chunks; c++) {
         if (live->regs[r].chunk_live[c] == NULL)
            continue;

         fprintf(stderr, "    chunk %u:", c);
         for (uint32_t i = 0; i < live->regs[r].chunk_live[c]->count; i++) {
            fprintf(stderr, " [%u, %u)",
                    live->regs[r].chunk_live[c]->intervals[i].start,
                    live->regs[r].chunk_live[c]->intervals[i].end);
         }
         fprintf(stderr, "\n");
      }
   }

   return live;
}
