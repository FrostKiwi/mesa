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
#include "ibc_builder.h"
#include "ibc_live_intervals.h"

struct coalesce_ref {
   ibc_ref ref;
   uint8_t simd_group;
   uint8_t simd_width;
};

struct opt_coalesce_state {
   void *mem_ctx;

   const ibc_live_intervals *live;
   ibc_flow_instr *block_start;
   uint32_t *last_chunk_access;
   uint32_t *prev_chunk_access;
   struct coalesce_ref *reg_coalesce_ref;

   ibc_instr *iter_instr;
};

static bool
reg_filter(const ibc_reg *reg)
{
   return reg->file == IBC_FILE_LOGICAL || reg->file == IBC_FILE_HW_GRF;
}

static bool
mark_ref_access(ibc_ref *ref,
                int num_bytes, int num_comps,
                uint8_t simd_group, uint8_t simd_width,
                void *_state)
{
   BITSET_DECLARE(used, IBC_REG_LIVE_MAX_CHUNKS);
   struct opt_coalesce_state *state = _state;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg == NULL || ref->reg->index >= state->live->num_regs)
      return true;

   const unsigned num_chunks = state->live->regs[ref->reg->index].num_chunks;
   const unsigned chunk_idx = state->live->regs[ref->reg->index].chunk_idx;

   memset(used, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_ref_chunks(state->live, ref, num_bytes, num_comps,
                                 simd_group, simd_width, used);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (!BITSET_TEST(used, i))
         continue;

      assert(state->prev_chunk_access[chunk_idx + i] <=
             state->iter_instr->index);
      state->prev_chunk_access[chunk_idx + i] = state->iter_instr->index;
   }

   return true;
}

static bool
remap_ref(ibc_ref *ref,
          int num_bytes, int num_comps,
          uint8_t simd_group, uint8_t simd_width,
          void *_state)
{
   struct opt_coalesce_state *state = _state;

   if (ref->file != IBC_FILE_LOGICAL)
      return true;

   while (1) {
      struct coalesce_ref coalesce = state->reg_coalesce_ref[ref->reg->index];
      if (coalesce.simd_width == 0)
         return true;

      ibc_ref new_ref = coalesce.ref;
      ibc_ref_compose_ref(&new_ref, coalesce.simd_group, coalesce.simd_width,
                          *ref, simd_group, simd_width);
      ibc_instr_set_ref(state->iter_instr, ref, new_ref);
   }
}

static bool
ibc_instr_is_load_payload(const ibc_instr *instr)
{
   if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
      return false;

   ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
   return intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD ||
          (intrin->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO &&
           intrin->src[0].ref.file != IBC_FILE_NONE);
}

static bool
try_coalesce_refs(ibc_instr *copy_instr, ibc_ref dest, ibc_ref src,
                  unsigned num_comps,
                  uint8_t simd_group, uint8_t simd_width,
                  struct opt_coalesce_state *state)
{
   /* For now, coalesce only works on logical and HW_GRF destinations */
   if (dest.file != IBC_FILE_LOGICAL && dest.file != IBC_FILE_HW_GRF)
      return false;

   /* For now, this only works on logical */
   if (src.file != IBC_FILE_LOGICAL)
      return false;

   /* We can only coalesce a register once */
   if (state->reg_coalesce_ref[src.reg->index].simd_width > 0)
      return false;

   /* Our source has to read the whole register */
   if (src.logical.byte > 0 ||
       src.logical.comp > 0 ||
       src.logical.broadcast ||
       ibc_type_bit_size(src.type) != src.reg->logical.bit_size ||
       num_comps != src.reg->logical.num_comps ||
       simd_group != src.reg->logical.simd_group ||
       simd_width != src.reg->logical.simd_width)
      return false;

   /* Source needs to be SSA */
   ibc_instr *src_ssa_instr = ibc_reg_ssa_instr(src.reg);
   if (src_ssa_instr == NULL)
      return false;

   /* We don't want to coalesce into LOAD_PAYLOAD and BTI_BLOCK_LOAD_UBO
    * instructions because RA is going to make them no-ops.
    */
   if (ibc_instr_is_load_payload(src_ssa_instr))
      return false;

   switch (dest.file) {
   case IBC_FILE_LOGICAL:
      if (dest.reg->logical.stride != src.reg->logical.stride)
         return false;

      /* If the source is packed then it needs to either have a single
       * component or else have the same SIMD group as the destination so that
       * it can be packed the same way.
       */
      if (src.reg->logical.packed &&
          src.reg->logical.num_comps > 1 &&
          (src.reg->logical.simd_group != dest.reg->logical.simd_group ||
           src.reg->logical.simd_width != dest.reg->logical.simd_width))
         return false;

      if (src.reg->logical.align > 0) {
         if (dest.reg->logical.align < src.reg->logical.align)
            return false;

         if (dest.logical.byte > 0)
            return false;
      }

      break;

   case IBC_FILE_HW_GRF:
      /* We have to have a simple stride on the destination */
      if (dest.hw_grf.vstride != dest.hw_grf.hstride * dest.hw_grf.width)
         return false;

      /* And it has to match the source */
      if (dest.hw_grf.hstride != src.reg->logical.stride)
         return false;

      if (src.reg->logical.align > 0) {
         if (dest.reg->hw_grf.align < src.reg->logical.align)
            return false;

         if (dest.hw_grf.byte % src.reg->logical.align != 0)
            return false;
      }
      break;

   default:
      unreachable("Unhandled register file");
   }

   /* The source needs to be defined in this block */
   if (src_ssa_instr->index < state->block_start->instr.index)
      return false;

   const ibc_live_intervals *live = state->live;

   /* We don't require that this be the only use of the source but we do
    * require that it be the last use.  If it's used after the MOV instruction
    * we're coalescing, we can't guarantee that the destination isn't written
    * again between now and then which would break things.
    *
    * This shouldn't have a substantial negative impact, however, because most
    * of the MOVs we're coalescing are for phis so they come at the end of the
    * block anyway.
    */
   {
      const unsigned num_src_chunks = live->regs[src.reg->index].num_chunks;
      const unsigned src_chunk_idx = live->regs[src.reg->index].chunk_idx;

      for (unsigned i = 0; i < num_src_chunks; i++) {
         if (state->last_chunk_access[src_chunk_idx + i] > copy_instr->index)
            return false;
      }
   }

   /* If the destination is accessed in any way between the source SSA
    * instruction and this MOV, then we can't move the write up to the source
    * SSA instruction and we have to bail.
    */
   {
      BITSET_DECLARE(written, IBC_REG_LIVE_MAX_CHUNKS);
      const unsigned num_dest_chunks = live->regs[dest.reg->index].num_chunks;
      const unsigned dest_chunk_idx = live->regs[dest.reg->index].chunk_idx;

      memset(written, 0, BITSET_WORDS(num_dest_chunks) * sizeof(BITSET_WORD));
      ibc_live_intervals_ref_chunks(live, &dest, -1, num_comps,
                                    simd_group, simd_width, written);

      for (unsigned i = 0; i < num_dest_chunks; i++) {
         if (!BITSET_TEST(written, i))
            continue;

         if (state->prev_chunk_access[dest_chunk_idx + i] > src_ssa_instr->index)
            return false;
      }
   }

   state->reg_coalesce_ref[src.reg->index] = (struct coalesce_ref) {
      .ref = dest,
      .simd_group = simd_group,
      .simd_width = simd_width,
   };

   /* We may have broken WLR. */
   ((ibc_reg *)dest.reg)->is_wlr = false;

   return true;
}

static bool
try_coalesce_instr(ibc_instr *instr, struct opt_coalesce_state *state)
{
   /* If it's predicated, we'd have to predicate the thing we're coalescing
    * and that could have weird effects.  Best to just avoid it.
    */
   if (instr->predicate != IBC_PREDICATE_NONE)
      return false;

   if (instr->type != IBC_INSTR_TYPE_ALU)
      return false;

   ibc_alu_instr *mov = ibc_instr_as_alu(instr);

   /* Has to be a raw mov */
   if (mov->op != IBC_ALU_OP_MOV ||
       mov->dest.type != mov->src[0].ref.type ||
       mov->saturate ||
       mov->src[0].mod != IBC_ALU_SRC_MOD_NONE)
      return false;

   /* We don't know what to do with a cmod */
   if (mov->cmod != BRW_CONDITIONAL_NONE)
      return false;

   /* This would be a weird case to actually come up in practice but if we
    * have a we_all mov and we try to coalesce it into a non-we_all
    * instruction, we would have to weirdly modify the instruction in order
    * for it to be correct.  Better to just bail.
    */
   if (mov->instr.we_all && mov->instr.simd_width != 1)
      return false;

   return try_coalesce_refs(&mov->instr, mov->dest, mov->src[0].ref, 1,
                            mov->instr.simd_group, mov->instr.simd_width,
                            state);
}

bool
ibc_opt_coalesce(ibc_shader *shader)
{
   struct opt_coalesce_state state = {
      .mem_ctx = ralloc_context(shader),
   };

   state.live = ibc_compute_reg_chunks(shader, reg_filter, state.mem_ctx);
   state.prev_chunk_access = rzalloc_array(state.mem_ctx, uint32_t,
                                           state.live->num_chunks);
   state.reg_coalesce_ref = rzalloc_array(state.mem_ctx, struct coalesce_ref,
                                          state.live->num_regs);

   /* Figure out where the live ranges of variables end */
   ibc_foreach_instr_safe(instr, shader) {
      state.iter_instr = instr;
      ibc_instr_foreach_read(instr, mark_ref_access, &state);
      ibc_instr_foreach_write(instr, mark_ref_access, &state);
   }

   state.last_chunk_access = state.prev_chunk_access;
   state.prev_chunk_access = rzalloc_array(state.mem_ctx, uint32_t,
                                           state.live->num_chunks);

   bool progress = false;

   ibc_foreach_instr_safe(instr, shader) {
      if (try_coalesce_instr(instr, &state)) {
         ibc_instr_remove(instr);
         progress = true;
      } else {
         state.iter_instr = instr;
         ibc_instr_foreach_read(instr, mark_ref_access, &state);
         ibc_instr_foreach_write(instr, mark_ref_access, &state);
      }

      if (instr->type == IBC_INSTR_TYPE_FLOW)
         state.block_start = ibc_instr_as_flow(instr);
   }

   if (progress) {
      ibc_foreach_instr(instr, shader) {
         state.iter_instr = instr;
         ibc_instr_foreach_read(instr, remap_ref, &state);
         ibc_instr_foreach_write(instr, remap_ref, &state);
      }
   }

   ralloc_free(state.mem_ctx);

   return progress;
}
