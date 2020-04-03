/*
 * Copyright © 2015 Intel Corporation
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

#include "brw_nir.h"
#include "compiler/nir/nir.h"
#include "util/u_dynarray.h"

/**
 * \file brw_nir_analyze_ubo_ranges.c
 *
 * This pass decides which portions of UBOs to upload as push constants,
 * so shaders can access them as part of the thread payload, rather than
 * having to issue expensive memory reads to pull the data.
 *
 * The 3DSTATE_CONSTANT_* mechanism can push data from up to 4 different
 * buffers, in GRF (256-bit/32-byte) units.
 *
 * To do this, we examine NIR load_ubo intrinsics, recording the number of
 * loads at each offset.  We track offsets at a 32-byte granularity, so even
 * fields with a bit of padding between them tend to fall into contiguous
 * ranges.  We build a list of these ranges, tracking their "cost" (number
 * of registers required) and "benefit" (number of pull loads eliminated
 * by pushing the range).  We then sort the list to obtain the four best
 * ranges (most benefit for the least cost).
 */

struct ubo_range_entry
{
   struct brw_ubo_range range;
   uint16_t uses;
};

static int
cmp_ubo_range_entry_uses(const void *va, const void *vb)
{
   const struct ubo_range_entry *a = va;
   const struct ubo_range_entry *b = vb;

   return (int)b->uses - (int)a->uses;
}

struct ubo_block_info
{
   uint32_t index;

   /* Each bit in the offsets bitfield represents a 32-byte section of data.
    * If it's set to one, there is interesting UBO data at that offset.  If
    * not, there's a "hole" - padding between data - or just nothing at all.
    */
   uint64_t offsets;
   uint8_t uses[64];
};

static int
cmp_ubo_block_info(const void *va, const void *vb)
{
   const struct ubo_block_info *a = va;
   const struct ubo_block_info *b = vb;

   return (int)b->index - (int)a->index;
}

struct ubo_analysis_state
{
   struct hash_table *blocks;
   bool uses_regular_uniforms;
};

static struct ubo_block_info *
get_block_info(struct ubo_analysis_state *state, uint32_t block)
{
   assert(block < BRW_MAX_BINDING_TABLE_SIZE);
   uint32_t hash = block + 1;
   void *key = (void *) (uintptr_t) hash;

   struct hash_entry *entry =
      _mesa_hash_table_search_pre_hashed(state->blocks, hash, key);

   if (entry)
      return (struct ubo_block_info *) entry->data;

   struct ubo_block_info *info =
      rzalloc(state->blocks, struct ubo_block_info);
   info->index = block;
   _mesa_hash_table_insert_pre_hashed(state->blocks, hash, key, info);

   return info;
}

static void
analyze_ubos_block(struct ubo_analysis_state *state, nir_block *block)
{
   nir_foreach_instr(instr, block) {
      if (instr->type != nir_instr_type_intrinsic)
         continue;

      nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
      switch (intrin->intrinsic) {
      case nir_intrinsic_load_uniform:
      case nir_intrinsic_image_deref_load:
      case nir_intrinsic_image_deref_store:
      case nir_intrinsic_image_deref_atomic_add:
      case nir_intrinsic_image_deref_atomic_imin:
      case nir_intrinsic_image_deref_atomic_umin:
      case nir_intrinsic_image_deref_atomic_imax:
      case nir_intrinsic_image_deref_atomic_umax:
      case nir_intrinsic_image_deref_atomic_and:
      case nir_intrinsic_image_deref_atomic_or:
      case nir_intrinsic_image_deref_atomic_xor:
      case nir_intrinsic_image_deref_atomic_exchange:
      case nir_intrinsic_image_deref_atomic_comp_swap:
      case nir_intrinsic_image_deref_size:
         state->uses_regular_uniforms = true;
         continue;

      case nir_intrinsic_load_ubo:
         break; /* Fall through to the analysis below */

      default:
         continue; /* Not a uniform or UBO intrinsic */
      }

      if (nir_src_is_const(intrin->src[0]) &&
          nir_src_is_const(intrin->src[1])) {
         const uint32_t block = nir_src_as_uint(intrin->src[0]);
         const unsigned byte_offset = nir_src_as_uint(intrin->src[1]);
         const int offset = byte_offset / 32;

         /* Avoid shifting by larger than the width of our bitfield, as this
          * is undefined in C.  Even if we require multiple bits to represent
          * the entire value, it's OK to record a partial value - the backend
          * is capable of falling back to pull loads for later components of
          * vectors, as it has to shrink ranges for other reasons anyway.
          */
         if (offset >= 64)
            continue;

         /* The value might span multiple 32-byte chunks. */
         const int bytes = nir_intrinsic_dest_components(intrin) *
                           (nir_dest_bit_size(intrin->dest) / 8);
         const int start = ROUND_DOWN_TO(byte_offset, 32);
         const int end = ALIGN(byte_offset + bytes, 32);
         const int chunks = (end - start) / 32;

         /* TODO: should we count uses in loops as higher benefit? */

         struct ubo_block_info *info = get_block_info(state, block);
         info->offsets |= BITFIELD64_RANGE(offset, chunks);
         info->uses[offset]++;
      }
   }
}

static bool
brw_ubo_ranges_overlap(struct brw_ubo_range a, struct brw_ubo_range b)
{
   return a.block == b.block &&
          ((a.start >= b.start && a.start < b.start + b.length) ||
           (b.start >= a.start && b.start < a.start + a.length));
}

static bool
brw_ubo_ranges_adjacent(struct brw_ubo_range a, struct brw_ubo_range b)
{
   return a.block == b.block &&
          (a.start == b.start + b.length || b.start == a.start + a.length);
}

static struct brw_ubo_range
brw_ubo_ranges_union(struct brw_ubo_range a, struct brw_ubo_range b)
{
   assert(a.block == b.block);
   unsigned start = MIN2(a.start, b.start);
   unsigned end = MAX2(a.start + a.length, b.start + b.length);
   return (struct brw_ubo_range) {
      .block = a.block,
      .start = start,
      .length = end - start,
   };
}

static void
search_for_better_range(const struct ubo_block_info *block, unsigned block_idx,
                        float *best_metric, struct ubo_range_entry *best_range,
                        unsigned start_min, unsigned start_max,
                        unsigned end_min, unsigned end_max,
                        unsigned max_range_length)
{
   for (unsigned start = start_min; start <= start_max; start++) {
      uint32_t uses = 0;
      for (unsigned end = start; end <= end_max; end++) {
         const unsigned length = end - start + 1;
         if (length > max_range_length)
            break;

         uses += block->uses[end];
         if (end < end_min)
            continue;

         float metric = (float)(uses * uses) / (float)length;
         if (metric > *best_metric) {
            *best_metric = metric;
            *best_range = (struct ubo_range_entry) {
               .range = {
                  .block = block_idx,
                  .start = start,
                  .length = length,
               },
               .uses = uses,
            };
         }
      }
   }
}

/* Select the "best" range from the given list of blocks.  We have a few
 * different metrics we could choose from.  The most obvious two are `metric =
 * uses` which will always give us full UBOs because it doesn't take length
 * into account and `metric = uses / length` which will tend to yield single
 * elements because the average is always less than the maximum.  In order to
 * split the difference, we choose `metric = uses / sqrt(length)`.
 *
 * Because square roots are expensive to calculate, we instead use the metric
 * `metric = uses^2 / length` which has an equivalent ordering.
 */
static struct ubo_range_entry
select_best_range(const struct ubo_block_info *blocks, unsigned nr_blocks,
                  struct ubo_range_entry *adj_ranges,
                  unsigned nr_adj_ranges,
                  unsigned max_range_length)
{
   float best_metric = 0;
   struct ubo_range_entry best_range = { };
   if (adj_ranges) {
      for (unsigned r = 0; r < nr_adj_ranges; r++) {
         const unsigned block_idx = adj_ranges[r].range.block;
         const struct ubo_block_info *block = &blocks[block_idx];
         if (block->offsets == 0)
            continue;

         int first_bit = ffsll(block->offsets) - 1;
         int last_bit = util_last_bit64(block->offsets) - 1;

         unsigned range_start = adj_ranges[r].range.start;
         unsigned range_end = adj_ranges[r].range.start +
                              adj_ranges[r].range.length - 1;

         if (range_start > first_bit) {
            /* Try to find a range before this range */
            search_for_better_range(block, block_idx, &best_metric, &best_range,
                                    first_bit, last_bit,
                                    range_start - 1, range_start - 1,
                                    max_range_length);
         }

         if (range_end < last_bit) {
            /* Try to find a range after this range */
            search_for_better_range(block, block_idx, &best_metric, &best_range,
                                    range_end + 1, range_end + 1,
                                    first_bit, last_bit,
                                    max_range_length);
         }
      }
   } else {
      for (unsigned block_idx = 0; block_idx < nr_blocks; block_idx++) {
         const struct ubo_block_info *block = &blocks[block_idx];
         if (block->offsets == 0)
            continue;

         int first_bit = ffsll(block->offsets) - 1;
         int last_bit = util_last_bit64(block->offsets) - 1;

         /* Sanity check */
         assert(block->offsets & BITFIELD64_BIT(first_bit));
         assert(block->offsets & BITFIELD64_BIT(last_bit));

         search_for_better_range(block, block_idx, &best_metric, &best_range,
                                 first_bit, last_bit, first_bit, last_bit,
                                 max_range_length);
      }
   }

   return best_range;
}

static void
remove_range_from_blocks_arr(struct ubo_block_info *blocks, unsigned nr_blocks,
                             struct brw_ubo_range range)
{
   assert(range.block < nr_blocks);
   struct ubo_block_info *block = &blocks[range.block];

   block->offsets &= ~BITFIELD64_RANGE(range.start, range.length);
   for (unsigned i = 0; i < range.length; i++)
      block->uses[range.start + i] = 0;
}

void
brw_nir_analyze_ubo_ranges(const struct brw_compiler *compiler,
                           nir_shader *nir,
                           const struct brw_vs_prog_key *vs_key,
                           struct brw_ubo_range out_ranges[4])
{
   const struct gen_device_info *devinfo = compiler->devinfo;

   memset(out_ranges, 0, 4 * sizeof(struct brw_ubo_range));

   if ((devinfo->gen <= 7 && !devinfo->is_haswell) ||
       !compiler->scalar_stage[nir->info.stage]) {
      return;
   }

   void *mem_ctx = ralloc_context(NULL);

   struct ubo_analysis_state state = {
      .uses_regular_uniforms = false,
      .blocks =
         _mesa_hash_table_create(mem_ctx, NULL, _mesa_key_pointer_equal),
   };

   switch (nir->info.stage) {
   case MESA_SHADER_VERTEX:
      if (vs_key && vs_key->nr_userclip_plane_consts > 0)
         state.uses_regular_uniforms = true;
      break;

   case MESA_SHADER_COMPUTE:
      /* Compute shaders use push constants to get the subgroup ID so it's
       * best to just assume some system values are pushed.
       */
      state.uses_regular_uniforms = true;
      break;

   default:
      break;
   }

   /* Walk the IR, recording how many times each UBO block/offset is used. */
   nir_foreach_function(function, nir) {
      if (function->impl) {
         nir_foreach_block(block, function->impl) {
            analyze_ubos_block(&state, block);
         }
      }
   }

   if (state.blocks->entries == 0)
      goto done; /* No constant UBO access */

   /* Return the top 4 or so.  We drop by one if regular uniforms are in
    * use, assuming one push buffer will be dedicated to those.  We may
    * also only get 3 on Haswell if we can't write INSTPM.
    *
    * The backend may need to shrink these ranges to ensure that they
    * don't exceed the maximum push constant limits.  It can simply drop
    * the tail of the list, as that's the least valuable portion.  We
    * unfortunately can't truncate it here, because we don't know what
    * the backend is planning to do with regular uniforms.
    */
   const int max_ubos = (compiler->constant_buffer_0_is_relative ? 3 : 4) -
                        state.uses_regular_uniforms;

   const unsigned max_push_regs = 64;

   /* Turn our set of blocks into an array sorted by block index.  This
    * ensures that our algorithms are nicely deterministic.
    */
   const unsigned nr_blocks = state.blocks->entries;
   const struct ubo_block_info *blocks;
   {
      struct ubo_block_info *blocks_rw =
         ralloc_array(mem_ctx, struct ubo_block_info, nr_blocks);

      unsigned b = 0;
      hash_table_foreach(state.blocks, entry)
         blocks_rw[b++] = *(struct ubo_block_info *)entry->data;
      assert(b == nr_blocks);

      qsort(blocks_rw, nr_blocks, sizeof(*blocks_rw), cmp_ubo_block_info);

      blocks = blocks_rw;
   }

   /* First, we try to get a trivial solution */
   if (nr_blocks <= max_ubos) {
      unsigned total_len = 0;
      struct ubo_range_entry ranges[4] = {};
      for (unsigned b = 0; b < nr_blocks; b++) {
         const struct ubo_block_info *block = &blocks[b];

         assert(block->offsets);
         int first_bit = ffsll(block->offsets) - 1;
         int last_bit = util_last_bit64(block->offsets) - 1;

         ranges[b] = (struct ubo_range_entry) {
            .range = {
               .block = b,
               .start = first_bit,
               .length = last_bit - first_bit + 1,
            },
            .uses = 0,
         };
         for (unsigned i = first_bit; i <= last_bit; i++)
            ranges[b].uses += block->uses[i];

         total_len += ranges[b].range.length;
      }

      if (total_len <= max_push_regs) {
         qsort(ranges, nr_blocks, sizeof(*ranges), cmp_ubo_range_entry_uses);
         for (unsigned b = 0; b < nr_blocks; b++) {
            out_ranges[b] = ranges[b].range;
            out_ranges[b].block = blocks[ranges[b].range.block].index;
         }
         goto done;
      }
   }

   /* Start by choosing the max_ubos "best" ranges */
   struct ubo_block_info *tmp_blocks =
      ralloc_array(mem_ctx, struct ubo_block_info, nr_blocks);
   memcpy(tmp_blocks, blocks, nr_blocks * sizeof(*blocks));

   assert(max_ubos <= 4);
   unsigned nr_regs = 0;
   unsigned nr_ranges = 0;
   struct ubo_range_entry ranges[8] = {}; /* A few extra in our work stack */
   while (true) {
      struct ubo_range_entry range =
         select_best_range(tmp_blocks, nr_blocks, NULL, 0,
                           nr_regs - max_push_regs);
      if (range.range.length == 0)
         break; /* We can't find anything to push */

      remove_range_from_blocks_arr(tmp_blocks, nr_blocks, range.range);

      /* If we hit the end of the stack, make room */
      if (nr_ranges == ARRAY_SIZE(ranges))
         nr_ranges--;

      ranges[nr_ranges++] = range;

      /* Now we compact down and de-duplicate the list of ranges */
      for (unsigned i = 0; i < nr_ranges; i++) {
         for (unsigned j = i + 1; j < nr_ranges; j++) {
            if (brw_ubo_ranges_overlap(ranges[i].range, ranges[j].range) ||
                brw_ubo_ranges_adjacent(ranges[i].range, ranges[j].range)) {
               ranges[i].range = brw_ubo_ranges_union(ranges[i].range,
                                                      ranges[j].range);
               ranges[i].uses += ranges[j].uses;

               /* Mark as unused */
               ranges[j].uses = 0;
            }
         }
      }

      /* Remove unused ranges and compact the stack */
      nr_regs = 0;
      for (unsigned i = 0, j = 0; i < nr_ranges; i++) {
         if (ranges[i].uses) {
            ranges[j++] = ranges[i];
            /* Only consider the UBOs we actually have room for */
            if (j < (unsigned)max_ubos)
               nr_regs += ranges[i].range.length;
         }
      }

      if (nr_regs == max_push_regs)
         break;
   }

   /* We allowed some extra ranges above so that we could keep a bit of
    * history and compact things.  At this point, we only want to consider
    * at most the number of UBOs we're allowed to push.
    */
   if (nr_ranges > max_ubos)
      nr_ranges = max_ubos;

   if (nr_regs < max_push_regs) {
      /* Only looking at consecutive blocks didn't fill our available push
       * space.  Try to expand ranges in the hopes of picking up more
       * constants.
       */
      memcpy(tmp_blocks, blocks, nr_blocks * sizeof(*blocks));

      /* Remove what's covered by our chosen ranges */
      for (unsigned r = 0; r < nr_ranges; r++)
         remove_range_from_blocks_arr(tmp_blocks, nr_blocks, ranges[r].range);

      while (true) {
         struct ubo_range_entry range =
            select_best_range(tmp_blocks, nr_blocks, ranges, nr_ranges,
                              nr_regs - max_push_regs);
         if (range.range.length == 0)
            break; /* We can't find anything to push */

         remove_range_from_blocks_arr(tmp_blocks, nr_blocks, range.range);

         ASSERTED bool found = false;
         for (unsigned r = 0; r < nr_ranges; r++) {
            assert(!brw_ubo_ranges_overlap(range.range, ranges[r].range));
            if (brw_ubo_ranges_adjacent(range.range, ranges[r].range)) {
               found = true;
               ranges[r].range = brw_ubo_ranges_union(ranges[r].range,
                                                      range.range);
               ranges[r].uses += range.uses;
            }
         }
         assert(found);
      }
   }

   for (unsigned r = 0; r < MIN2(nr_ranges, (unsigned)max_ubos); r++) {
      out_ranges[r] = ranges[r].range;
      out_ranges[r].block = blocks[ranges[r].range.block].index;
   }

done:
   ralloc_free(mem_ctx);
}
