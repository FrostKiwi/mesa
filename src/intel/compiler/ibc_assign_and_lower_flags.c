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

#define TOTAL_FLAG_SUBNRS 4

enum flag_rep {
   FLAG_REP_FLAG   = 0x1,
   FLAG_REP_VECTOR = 0x2,
   FLAG_REP_SCALAR = 0x4,
   FLAG_REP_ALL    = 0x7,
};

struct flag_reg_state {
   bool read_as_flag;

   uint32_t last_write_ip;

   /** Scalar form of the register.  This is a scalar with UW or UD type. */
   struct ibc_ref scalar;

   /** Vector form of the register.  This is a SIMD vector with W type. */
   struct ibc_ref vector;
};

struct ibc_assign_flags_state {
   void *mem_ctx;

   ibc_builder builder;

   const ibc_live_intervals *live;
   struct flag_reg_state *regs;

   const ibc_reg *assign[TOTAL_FLAG_SUBNRS];
   enum flag_rep valid[TOTAL_FLAG_SUBNRS];
};

static bool
update_reg_last_write_ip(ibc_reg_write *write, ibc_ref *ref, void *_state)
{
   struct ibc_assign_flags_state *state = _state;
   if (ref->reg == NULL || ref->reg->index >= state->live->num_regs)
      return true;

   if (state->regs[ref->reg->index].last_write_ip < write->instr->index)
      state->regs[ref->reg->index].last_write_ip = write->instr->index;

   return true;
}

static uint8_t
cmp_bit_size(ibc_alu_instr *cmp)
{
   assert(cmp->op == IBC_ALU_OP_CMP);
   return MAX2(ibc_type_bit_size(cmp->src[0].ref.type),
               ibc_type_bit_size(cmp->src[1].ref.type));
}

static int8_t
logical_flag_reg_bit_size(const ibc_reg *reg)
{
   assert(reg->file == IBC_REG_FILE_LOGICAL);
   assert(reg->logical.bit_size == 1);
   assert(reg->is_wlr);

   int8_t bit_size = -1;
   ibc_reg_foreach_write(write, reg) {
      if (write->instr->type != IBC_INSTR_TYPE_ALU)
         return -1;

      ibc_alu_instr *cmp = ibc_instr_as_alu(write->instr);
      if (cmp->op != IBC_ALU_OP_CMP)
         return -1;

      if (bit_size < 0) {
         bit_size = cmp_bit_size(cmp);
      } else if (bit_size != cmp_bit_size(cmp)) {
         return -1;
      }
   }

   return bit_size;
}

static bool
logical_reg_has_unique_alu_writes(const ibc_reg *reg,
                                  const ibc_live_intervals *live)
{
   if (!reg->is_wlr)
      return false;

   if (list_is_singular(&reg->writes))
      return true;

   assert(reg->index < live->num_regs);
   const ibc_reg_live_intervals *rli = &live->regs[reg->index];

   BITSET_DECLARE(written, IBC_REG_LIVE_MAX_CHUNKS);
   const unsigned chunks_words = BITSET_WORDS(rli->num_chunks);
   memset(written, 0, chunks_words * sizeof(BITSET_WORD));

   ibc_reg_foreach_write(write, reg) {
      if (write->instr->type != IBC_INSTR_TYPE_ALU)
         return false;

      ibc_alu_instr *write_alu = ibc_instr_as_alu(write->instr);

      BITSET_DECLARE(ref_written, IBC_REG_LIVE_MAX_CHUNKS);
      memset(ref_written, 0, chunks_words * sizeof(BITSET_WORD));

      ibc_live_intervals_ref_chunks(live, ibc_reg_write_get_ref(write), -1, 1,
                                    write_alu->instr.simd_group,
                                    write_alu->instr.simd_width,
                                    ref_written);

      for (unsigned w = 0; w < chunks_words; w++) {
         if (written[w] & ref_written[w])
            return false;
         written[w] |= ref_written[w];
      }
   }

   return true;
}

static void
free_dead_flags(uint32_t ip, struct ibc_assign_flags_state *state)
{
   for (uint8_t subnr = 0; subnr < TOTAL_FLAG_SUBNRS; subnr++) {
      if (state->assign[subnr] == NULL)
         continue;

      const ibc_reg *reg = state->assign[subnr];
      const ibc_reg_live_intervals *rli = &state->live->regs[reg->index];

      if (rli->physical_end <= ip) {
         state->assign[subnr] = NULL;
         continue;
      }

      if (!(state->valid[subnr] & (FLAG_REP_VECTOR | FLAG_REP_SCALAR)))
         continue;

      uint32_t end = rli->physical_end;
      if (!state->regs[reg->index].read_as_flag) {
         /* If this register is never read as a flag, we can potentially
          * significantly shorten its live range.
          */
         end = 0;
         ibc_reg_foreach_write(write, reg)
            end = MAX2(end, write->instr->index);
      }

      if (end <= ip)
         state->assign[subnr] = NULL;
   }
}

static void
ensure_flag_spill_grf(const ibc_reg *flag,
                      struct ibc_assign_flags_state *state)
{
   if (state->regs[flag->index].vector.file != IBC_REG_FILE_NONE ||
       state->regs[flag->index].scalar.file != IBC_REG_FILE_NONE)
      return;

   assert(flag->file == IBC_REG_FILE_LOGICAL ||
          flag->file == IBC_REG_FILE_FLAG);
   const unsigned bits = flag->file == IBC_REG_FILE_LOGICAL ?
      flag->logical.simd_width : flag->flag.bits;

   ibc_reg *scalar = ibc_logical_reg_create(state->builder.shader,
                                            MIN2(16, bits), 1,
                                            0 /* simd_group */,
                                            1 /* simd_width */);
   state->regs[flag->index].scalar = ibc_typed_ref(scalar, IBC_TYPE_UINT);
}

static void
evict_flag(uint8_t subnr, struct ibc_assign_flags_state *state)
{
   if (state->assign[subnr] == NULL)
      return;

   ibc_builder *b = &state->builder;

   const ibc_reg *flag = state->assign[subnr];
   ensure_flag_spill_grf(flag, state);

   if (state->regs[flag->index].vector.file != IBC_REG_FILE_NONE &&
       !(state->valid[subnr] & FLAG_REP_VECTOR)) {
      assert(flag->file == IBC_REG_FILE_LOGICAL);

      ibc_ref vector = state->regs[flag->index].vector;
      assert(vector.file == IBC_REG_FILE_LOGICAL);

      ibc_builder_push_scalar(b);
      ibc_ref zero = ibc_MOV(b, IBC_TYPE_W, ibc_imm_w(0));
      ibc_builder_pop(b);

      ibc_builder_push_group(b, vector.reg->logical.simd_group,
                                vector.reg->logical.simd_width);
      ibc_alu_instr *sel =
         ibc_build_alu2(b, IBC_ALU_OP_SEL, vector, zero, ibc_imm_w(-1));
      ibc_instr_set_predicate(&sel->instr, ibc_flag_ref(subnr, 0),
                              BRW_PREDICATE_NORMAL, true);
      ibc_builder_pop(b);
   }

   if (state->regs[flag->index].scalar.file != IBC_REG_FILE_NONE &&
       !(state->valid[subnr] & FLAG_REP_SCALAR)) {
      ibc_ref scalar = state->regs[flag->index].scalar;
      ibc_builder_push_scalar(b);
      ibc_ref flag_ref = ibc_flag_ref(subnr, 0);
      flag_ref.type = scalar.type;
      ibc_MOV_to(b, scalar, flag_ref);
      ibc_builder_pop(b);
   }

   /* If subnr is even, it's possible that the same register occupies two
    * subnumbers.  Clear both if this is the case.
    */
   if (subnr % 2 == 0 && state->assign[subnr] == state->assign[subnr + 1])
      state->assign[subnr + 1] = NULL;
   state->assign[subnr] = NULL;
}

static void
evict_all_flags(uint32_t ip, struct ibc_assign_flags_state *state)
{
   free_dead_flags(ip, state);

   for (uint8_t subnr = 0; subnr < TOTAL_FLAG_SUBNRS; subnr++)
      evict_flag(subnr, state);
}

static void
load_flag_if_needed(uint8_t subnr, uint32_t ip,
                    struct ibc_assign_flags_state *state)
{
   assert(state->assign[subnr] != NULL);
   const ibc_reg *flag = state->assign[subnr];
   const ibc_reg_live_intervals *rli = &state->live->regs[flag->index];

   /* Detect if this flag register is live right before the current
    * instruction.  ip = 0 should only be true for the START instruction
    */
   assert(ip > 0);
   bool live = false;
   for (unsigned i= 0; i < rli->num_chunks; i++) {
      if (interval_set_contains(rli->chunk_live[i], ip - 1)) {
         live = true;
         break;
      }
   }
   if (!live)
      return;

   ibc_builder *b = &state->builder;
   ensure_flag_spill_grf(flag, state);

   /* Prefer the scalar if we have it as the load is only one instruction and
    * it contributes less to register pressure so extending its live range
    * costs less.
    */
   if (state->regs[flag->index].scalar.file != IBC_REG_FILE_NONE) {
      ibc_ref scalar = state->regs[flag->index].scalar;
      ibc_builder_push_scalar(b);
      ibc_ref flag_ref = ibc_flag_ref(subnr, 0);
      flag_ref.type = scalar.type;
      ibc_MOV_to(b, flag_ref, scalar);
      ibc_builder_pop(b);
      return;
   }

   if (state->regs[flag->index].vector.file != IBC_REG_FILE_NONE) {
      ibc_ref vector = state->regs[flag->index].vector;
      assert(vector.file == IBC_REG_FILE_LOGICAL);

      ibc_builder_push_group(b, vector.reg->logical.simd_group,
                                vector.reg->logical.simd_width);

      ibc_build_alu(b, IBC_ALU_OP_MOV, ibc_null(vector.type),
                    ibc_flag_ref(subnr, 0), BRW_CONDITIONAL_NZ, &vector, 1);
      ibc_builder_pop(b);
      return;
   }

   unreachable("Must have some sort of GRF");
}

static int8_t
find_flag(const ibc_reg *reg, struct ibc_assign_flags_state *state)
{
   for (uint8_t subnr = 0; subnr < TOTAL_FLAG_SUBNRS; subnr++) {
      if (state->assign[subnr] == reg)
         return subnr;
   }

   return -1;
}

static int8_t
find_or_assign_flag(const ibc_reg *reg, uint32_t ip,
                    bool load_from_grf,
                    bool opportunistic,
                    struct ibc_assign_flags_state *state)
{
   /* First, try and look it up */
   int8_t subnr = find_flag(reg, state);
   if (subnr >= 0)
      return subnr;

   uint8_t align_mul, align_offset, num_subnrs;
   if (reg->file == IBC_REG_FILE_LOGICAL) {
      align_mul =
         DIV_ROUND_UP(reg->logical.simd_group + reg->logical.simd_width, 16);
      align_offset = reg->logical.simd_group >= 16;
      num_subnrs = DIV_ROUND_UP(reg->logical.simd_width, 16);
   } else {
      assert(reg->file == IBC_REG_FILE_FLAG);
      align_mul = DIV_ROUND_UP(reg->flag.align_mul, 16);
      align_offset = reg->flag.align_offset / 16;
      num_subnrs = DIV_ROUND_UP(reg->flag.bits, 16);
   }

   /* Next, try and find a free slot */
   for (uint8_t s = align_offset; s < TOTAL_FLAG_SUBNRS; s += align_mul) {
      assert(num_subnrs == 1 || num_subnrs == 2);
      if (state->assign[s] == NULL &&
          (num_subnrs == 1 || state->assign[s + 1] == NULL)) {
         subnr = s;
         goto assigned;
      }
   }

   /* If we don't have any free slots, get rid of the register with the
    * longest remaining live range.  The hope here is that any shorter live
    * ranges will go away by themselves before we need to re-allocate them.
    * It's a heuristic.
    */
   uint32_t max_end = 0;
   for (uint8_t s = align_offset; s < TOTAL_FLAG_SUBNRS; s += align_mul) {
      for (unsigned i = 0; i < num_subnrs; i++) {
         if (state->assign[s + i]) {
            const ibc_reg_live_intervals *iter_rli =
               &state->live->regs[state->assign[s + i]->index];
            if (iter_rli->physical_end > max_end) {
               subnr = s;
               max_end = iter_rli->physical_end;
            }
         }
      }
   }

   /* If this is an opportunistic allocation, we only want to evict a register
    * if its live range is shorter than the register we're opportunistically
    * trying to allocation.  The idea here is that if the live range of this
    * register is longer, we would be evicted before we would be able to take
    * advantage of the opportunistic flag allocation.  It's a heuristic.
    */
   if (opportunistic && max_end < state->live->regs[reg->index].physical_end)
      return -1;

   /* If we got here, we should have found a register to use */
   assert(subnr >= 0);

   for (unsigned i = 0; i < num_subnrs; i++)
      evict_flag(subnr + i, state);

assigned:

   for (unsigned i = 0; i < num_subnrs; i++)
      state->assign[subnr + i] = reg;

   if (load_from_grf)
      load_flag_if_needed(subnr, ip, state);

   for (unsigned i = 0; i < num_subnrs; i++)
      state->valid[subnr + i] = FLAG_REP_ALL;

   return subnr;
}

static void
instr_set_flag_ref(ibc_instr *instr, ibc_ref *ref, uint8_t subnr,
                   enum flag_rep preserved,
                   struct ibc_assign_flags_state *state)
{
   assert(subnr < TOTAL_FLAG_SUBNRS);
   const ibc_reg *reg = state->assign[subnr];

   uint8_t num_subnrs;
   ibc_ref flag_ref = ibc_flag_ref(subnr, 0);
   if (reg->file == IBC_REG_FILE_LOGICAL) {
      assert(ref->type == IBC_TYPE_FLAG);
      assert(reg->logical.bit_size == 1);
      assert(reg->logical.simd_group <= instr->simd_group);
      flag_ref.flag.bit += instr->simd_group - reg->logical.simd_group;
      num_subnrs = DIV_ROUND_UP(reg->logical.simd_width, 16);
   } else {
      flag_ref.type = ref->type;
      num_subnrs = DIV_ROUND_UP(reg->flag.bits, 16);
   }

   ibc_instr_set_ref(instr, ref, flag_ref);

   for (unsigned i = 0; i < num_subnrs; i++)
      state->valid[subnr] &= preserved;
}

static void
rewrite_flag_ref(ibc_ref *ref, ibc_instr *write_instr,
                 struct ibc_assign_flags_state *state)
{
   if (ref->file == IBC_REG_FILE_LOGICAL && ref->type == IBC_TYPE_FLAG) {
      assert(ref->reg->index < state->live->num_regs);
      ibc_ref vector = state->regs[ref->reg->index].vector;
      vector.logical = ref->logical;
      ibc_instr_set_ref(write_instr, ref, vector);
   } else if (ref->file == IBC_REG_FILE_FLAG && ref->reg) {
      /* We should only get here for regular sources and destinations that
       * access flags.
       */
      assert(ref->type == IBC_TYPE_W || ref->type == IBC_TYPE_UW ||
             ref->type == IBC_TYPE_D || ref->type == IBC_TYPE_UD);
      if (write_instr) {
         /* We assume that if someone is writing to a flag directly, they
          * probably intend to immediately follow that by another instruction
          * which accesses the flag so go ahead and allocate a flag reg for
          * real.
          */
         assert(ibc_type_bit_size(ref->type) == ref->reg->flag.bits);
         int8_t subnr = find_or_assign_flag((ibc_reg *)ref->reg,
                                            write_instr->index,
                                            false, /* load_from_grf */
                                            true, /* opportunistic */
                                            state);
         instr_set_flag_ref(write_instr, ref, subnr,
                            FLAG_REP_FLAG, state);
      } else {
         int8_t subnr = find_flag(ref->reg, state);
         if (subnr >= 0) {
            instr_set_flag_ref(NULL, ref, subnr, FLAG_REP_ALL, state);
         } else {
            /* If we're a read and it's not currently in a flag, pull from the
             * scalar GRF.  We should have one.
             */
            ibc_ref scalar = state->regs[ref->reg->index].scalar;
            assert(scalar.file != IBC_REG_FILE_NONE);
            scalar.type = ref->type;
            *ref = scalar;
         }
      }
   }
}

static bool
rewrite_flag_read_cb(ibc_ref *ref,
                     UNUSED int num_bytes,
                     UNUSED int num_comps,
                     UNUSED uint8_t simd_group,
                     UNUSED uint8_t simd_width,
                     void *_state)
{
   rewrite_flag_ref(ref, NULL, _state);
   return true;
}

static bool
rewrite_flag_write_cb(ibc_reg_write *write, ibc_ref *ref, void *_state)
{
   rewrite_flag_ref(ref, write->instr, _state);
   return true;
}

static bool
is_flag_reg(const ibc_reg *reg)
{
   return (reg->file == IBC_REG_FILE_LOGICAL && reg->logical.bit_size == 1) ||
          reg->file == IBC_REG_FILE_FLAG;
}

bool
ibc_assign_and_lower_flags(ibc_shader *shader)
{
   struct ibc_assign_flags_state state = {
      .mem_ctx = ralloc_context(NULL),
   };

   state.live = ibc_compute_live_intervals(shader, is_flag_reg, state.mem_ctx);
   if (state.live->num_regs == 0) {
      ralloc_free(state.mem_ctx);
      return false;
   }

   ibc_builder_init(&state.builder, shader);
   ibc_builder *b = &state.builder;

   /* Figure out what registers are read as a flag register */
   state.regs = rzalloc_array(state.mem_ctx, struct flag_reg_state,
                              state.live->num_regs);
   ibc_foreach_instr(instr, shader) {
      if (instr->predicate != BRW_PREDICATE_NONE && instr->flag.reg != NULL) {
         assert(instr->flag.reg->index < state.live->num_regs);
         state.regs[instr->flag.reg->index].read_as_flag = true;
      }
      ibc_instr_foreach_reg_write(instr, update_reg_last_write_ip, &state);
   }

   ibc_foreach_reg(reg, shader) {
      if (!is_flag_reg(reg))
         continue;

      if (reg->file == IBC_REG_FILE_FLAG) {
         state.regs[reg->index].read_as_flag = true;
         continue;
      }

      assert(reg->file == IBC_REG_FILE_LOGICAL);
      assert(reg->logical.num_comps == 1);

      if (!reg->is_wlr) {
         ibc_reg *vec_reg = ibc_logical_reg_create(b->shader, 16, 1,
                                                   reg->logical.simd_group,
                                                   reg->logical.simd_width);
         vec_reg->is_wlr = false;
         state.regs[reg->index].vector = ibc_typed_ref(vec_reg, IBC_TYPE_W);
         continue;
      }

      int8_t bit_size = logical_flag_reg_bit_size(reg);
      if (bit_size < 0)
         bit_size = 16;

      ibc_builder_push_group(b, reg->logical.simd_group,
                                reg->logical.simd_width);
      state.regs[reg->index].vector =
         ibc_builder_new_logical_reg(b, IBC_TYPE_INT | bit_size, 1);
      ibc_builder_pop(b);
   }

   ibc_foreach_instr_safe(instr, shader) {
      /* Start by freeing any newly dead flag registers */
      free_dead_flags(instr->index, &state);

      /* Set the cursor to before the current instruction in case
       * find_or_assign_flag needs to load a flag value.
       */
      b->cursor = ibc_before_instr(instr);

      if (instr->type == IBC_INSTR_TYPE_ALU) {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);
         if (alu->op == IBC_ALU_OP_CMP) {
            ibc_reg *logical = (ibc_reg *)instr->flag.reg;
            /* CMP destinations that are never read as flags are forever used
             * as vectors and the flag value is discarded.  There's no need
             * for us to load from the GRF to ensure the flag is consistent.
             */
            const bool load_from_grf =
               state.regs[logical->index].read_as_flag;
            int8_t subnr = find_or_assign_flag(logical, instr->index,
                                               load_from_grf,
                                               false, /* opportunistic */
                                               &state);

            enum flag_rep preserved = FLAG_REP_FLAG;

            /* If the vector version matches, we can write to it directly and
             * avoid getting it out-of-sync.
             */
            ibc_ref vector = state.regs[logical->index].vector;
            if (vector.file == IBC_REG_FILE_LOGICAL &&
                vector.reg->logical.bit_size == cmp_bit_size(alu)) {
               vector.type = ibc_type_base_type(alu->src[0].ref.type) |
                             ibc_type_bit_size(vector.type);
               ibc_instr_set_ref(&alu->instr, &alu->dest, vector);
               preserved |= FLAG_REP_VECTOR;
            }

            instr_set_flag_ref(instr, &instr->flag, subnr, preserved, &state);
         } else if (alu->dest.file == IBC_REG_FILE_LOGICAL &&
                    alu->dest.reg->logical.bit_size == 1 &&
                    logical_reg_has_unique_alu_writes(alu->dest.reg,
                                                      state.live) &&
                    state.regs[alu->dest.reg->index].read_as_flag) {
            /* In this case, we're some ALU instruction which is producing a
             * 1-bit destination.  This is something like an AND or OR which
             * we are using on logical things.  We're going to turn its
             * destination into a W type but we can also, if we have a free
             * reg, get a flag write for free.
             */
            ibc_reg *logical = (ibc_reg *)alu->dest.reg;
            int8_t subnr = find_or_assign_flag(logical, instr->index,
                                               true, /* load_from_grf */
                                               true, /* opportunistic */
                                               &state);
            if (subnr >= 0) {
               alu->cmod = BRW_CONDITIONAL_NZ;
               instr_set_flag_ref(instr, &instr->flag, subnr,
                                  FLAG_REP_FLAG | FLAG_REP_VECTOR, &state);
            }
         }
      }

      if (instr->flag.file != IBC_REG_FILE_NONE && instr->flag.reg) {
         int8_t subnr = find_or_assign_flag(instr->flag.reg, instr->index,
                                            true, /* load_from_grf */
                                            false, /* opportunistic */
                                            &state);
         enum flag_rep preserved =
            ibc_instr_writes_flag(instr) ? FLAG_REP_FLAG : FLAG_REP_ALL;
         instr_set_flag_ref(instr, &instr->flag, subnr, preserved, &state);
      }

      ibc_instr_foreach_read(instr, rewrite_flag_read_cb, &state);
      ibc_instr_foreach_reg_write(instr, rewrite_flag_write_cb, &state);

      /* When we cross block boundaries, reset the allocator */
      if (instr->type == IBC_INSTR_TYPE_FLOW)
         evict_all_flags(instr->index, &state);
   }

   ibc_foreach_reg_safe(reg, shader) {
      if (is_flag_reg(reg))
         list_del(&reg->link);
   }

   ralloc_free(state.mem_ctx);

   return true;
}
