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

#define TOTAL_FLAG_BITS 64

/* We allocate at the nibble granularity */
#define FLAG_CHUNK_BITS 4
#define TOTAL_FLAG_CHUNKS (TOTAL_FLAG_BITS / FLAG_CHUNK_BITS)

enum flag_rep {
   FLAG_REP_NONE   = 0x0,
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

   const ibc_reg *assign[TOTAL_FLAG_CHUNKS];
   enum flag_rep valid[TOTAL_FLAG_CHUNKS];
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

   /* This can happen when we have a CMP of booleans */
   if (bit_size == 1)
      bit_size = 16;

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

static unsigned
flag_reg_num_bits(const ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_LOGICAL:
      assert(reg->logical.bit_size == 1);
      return reg->logical.simd_width;
   case IBC_REG_FILE_FLAG:
      return reg->flag.bits;
   default:
      unreachable("Invalid flag register type");
   }
}

static unsigned
flag_reg_num_chunks(const ibc_reg *reg)
{
   return DIV_ROUND_UP(flag_reg_num_bits(reg), FLAG_CHUNK_BITS);
}

static void
free_dead_flags(uint32_t ip, struct ibc_assign_flags_state *state)
{
   for (unsigned chunk = 0; chunk < TOTAL_FLAG_CHUNKS; chunk++) {
      if (state->assign[chunk] == NULL)
         continue;

      const ibc_reg *reg = state->assign[chunk];
      const ibc_reg_live_intervals *rli = &state->live->regs[reg->index];

      if (rli->physical_end <= ip) {
         state->assign[chunk] = NULL;
         continue;
      }

      if (!(state->valid[chunk] & (FLAG_REP_VECTOR | FLAG_REP_SCALAR)))
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

      if (end <= ip) {
         const unsigned num_chunks = flag_reg_num_chunks(reg);
         for (unsigned c = 0; c < num_chunks; c++)
            state->assign[chunk + c] = NULL;

         for (unsigned c = 0; c < TOTAL_FLAG_CHUNKS; c++)
            assert(state->assign[c] != reg);
      }
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

static unsigned
max_pow2_divisor(unsigned x)
{
   /* Anything divides 0 */
   if (x == 0)
      return 1u << 31;

   const unsigned pow2_divisor = 1 << (ffs(x) - 1);
   assert(x % pow2_divisor == 0);
   assert(x % (pow2_divisor * 2) != 0);
   return pow2_divisor;
}

static void
load_scalar_if_needed(unsigned chunk, struct ibc_assign_flags_state *state)
{
   if (state->assign[chunk] == NULL)
      return;

   for (unsigned c = 0; c < chunk; c++)
      assert(state->assign[c] != state->assign[chunk]);

   const ibc_reg *flag = state->assign[chunk];
   const unsigned num_chunks = flag_reg_num_chunks(flag);

   ibc_builder *b = &state->builder;
   ensure_flag_spill_grf(flag, state);

   ibc_ref scalar = state->regs[flag->index].scalar;
   if (scalar.file == IBC_REG_FILE_NONE)
      return;

   enum flag_rep all_valid = FLAG_REP_ALL;
   for (unsigned c = 0; c < num_chunks; c++)
      all_valid &= state->valid[chunk + c];

   if (all_valid & FLAG_REP_VECTOR)
      return;

   ibc_builder_push_scalar(b);
   ibc_ref flag_ref = ibc_flag_ref(chunk * FLAG_CHUNK_BITS);
   flag_ref.type = scalar.type;
   ibc_MOV_to(b, scalar, flag_ref);
   ibc_builder_pop(b);
}

static void
load_vector_if_needed(unsigned chunk,
                      unsigned start_chunk, unsigned num_chunks,
                      struct ibc_assign_flags_state *state)
{
   if (state->assign[chunk] == NULL)
      return;

   for (unsigned c = 0; c < chunk; c++)
      assert(state->assign[c] != state->assign[chunk]);

   const ibc_reg *flag = state->assign[chunk];

   ibc_builder *b = &state->builder;
   ensure_flag_spill_grf(flag, state);

   ibc_ref vector = state->regs[flag->index].vector;
   if (vector.file == IBC_REG_FILE_NONE)
      return;

   assert(vector.file == IBC_REG_FILE_LOGICAL);
   if (flag->logical.simd_width < FLAG_CHUNK_BITS) {
      assert(start_chunk == 0 && num_chunks == 1);
      if (!(state->valid[chunk] & FLAG_REP_VECTOR)) {
         ibc_builder_push_we_all(b, flag->logical.simd_width);
         ibc_build_alu(b, IBC_ALU_OP_MOV, ibc_null(vector.type),
                       ibc_flag_ref(chunk * FLAG_CHUNK_BITS),
                       BRW_CONDITIONAL_NZ, &vector, 1);
         ibc_builder_pop(b);
      }
   } else {
      const unsigned max_width =
         (2 * REG_SIZE) / ibc_type_byte_size(vector.type);
      assert(max_width % FLAG_CHUNK_BITS == 0);
      const unsigned max_width_chunks = max_width / FLAG_CHUNK_BITS;
         const unsigned end_chunk = start_chunk + num_chunks - 1;

      int copy_start = -1;
      for (unsigned c = start_chunk; c <= end_chunk; c++) {
         if (copy_start < 0 && !(state->valid[chunk + c] & FLAG_REP_VECTOR))
            copy_start = c;

         if (copy_start < 0)
            continue;

         if (c == end_chunk ||
             (state->valid[chunk + c] & FLAG_REP_VECTOR) ||
             (c - copy_start) == max_width_chunks ||
             (c - copy_start) == max_pow2_divisor(copy_start)) {
            unsigned copy_bit = copy_start * FLAG_CHUNK_BITS;
            unsigned copy_width = (c - copy_start) * FLAG_CHUNK_BITS;
            if (c == end_chunk)
               copy_width += FLAG_CHUNK_BITS;

            ibc_builder_push_scalar(b);
            ibc_ref zero = ibc_MOV(b, IBC_TYPE_W, ibc_imm_w(0));
            ibc_builder_pop(b);

            ibc_builder_push_group(b, flag->logical.simd_group + copy_bit,
                                      copy_width);
            ibc_alu_instr *sel =
               ibc_build_alu2(b, IBC_ALU_OP_SEL, vector, zero, ibc_imm_w(-1));
            ibc_instr_set_predicate(&sel->instr,
               ibc_flag_ref(chunk * FLAG_CHUNK_BITS + copy_bit),
               BRW_PREDICATE_NORMAL, true);
            ibc_builder_pop(b);
         }
      }
   }
}

static void
evict_flag(unsigned chunk, struct ibc_assign_flags_state *state)
{
   if (state->assign[chunk] == NULL)
      return;

   UNUSED const ibc_reg *flag = state->assign[chunk];

   const unsigned num_chunks = flag_reg_num_chunks(state->assign[chunk]);
   load_vector_if_needed(chunk, 0, num_chunks, state);
   load_scalar_if_needed(chunk, state);

   for (unsigned c = 0; c < num_chunks; c++)
      state->assign[chunk + c] = NULL;

   for (unsigned c = 0; c < TOTAL_FLAG_CHUNKS; c++)
      assert(state->assign[c] != flag);
}

static void
load_from_scalar(unsigned chunk, struct ibc_assign_flags_state *state)
{
   ibc_builder *b = &state->builder;
   const ibc_reg *flag = state->assign[chunk];
   ibc_ref scalar = state->regs[flag->index].scalar;
   assert(scalar.file == IBC_REG_FILE_LOGICAL);

   const unsigned num_chunks = flag_reg_num_chunks(flag);
   for (unsigned c = 0; c < num_chunks; c++) {
      assert(state->assign[chunk + c] == flag);
      assert(state->valid[chunk + c] & FLAG_REP_SCALAR);
   }

   ibc_builder_push_scalar(b);
   ibc_ref flag_ref = ibc_flag_ref(chunk * FLAG_CHUNK_BITS);
   flag_ref.type = scalar.type;
   ibc_MOV_to(b, flag_ref, scalar);
   ibc_builder_pop(b);

   for (unsigned c = 0; c < num_chunks; c++)
      state->valid[chunk + c] |= FLAG_REP_FLAG;
}

static void
load_flag_if_needed(unsigned chunk, unsigned start_chunk, unsigned num_chunks,
                    bool is_write, struct ibc_assign_flags_state *state)
{
   assert(state->assign[chunk] != NULL);
   for (unsigned c = 0; c < chunk; c++)
      assert(state->assign[c] != state->assign[chunk]);

   const ibc_reg *flag = state->assign[chunk];
   assert(start_chunk + num_chunks <= flag_reg_num_chunks(flag));

   ibc_builder *b = &state->builder;
   ensure_flag_spill_grf(flag, state);

   enum flag_rep all_valid = FLAG_REP_ALL;
   for (unsigned c = 0; c < flag_reg_num_chunks(flag); c++)
      all_valid &= state->valid[chunk + c];

   if (is_write) {
      if ((start_chunk > 0 ||
           start_chunk + num_chunks < flag_reg_num_chunks(flag)) &&
          state->regs[flag->index].scalar.file != IBC_REG_FILE_NONE &&
          !(all_valid & FLAG_REP_FLAG)) {
         /* If we're a partial write and we don't have a scalar representation
          * and not all of the flag values are valid then we have to do a load
          * because we can't easily do a partial load from a scalar later.
          */
         load_from_scalar(chunk, state);
      }
      /* No other flag writes require a load. */
      return;
   }

   /* Prefer the scalar if we have it as the load is only one instruction and
    * it contributes less to register pressure so extending its live range
    * costs less.
    */
   if (state->regs[flag->index].scalar.file != IBC_REG_FILE_NONE &&
       (all_valid & FLAG_REP_SCALAR)) {
      load_from_scalar(chunk, state);
      return;
   }

   if (state->regs[flag->index].vector.file != IBC_REG_FILE_NONE) {
      ibc_ref vector = state->regs[flag->index].vector;
      assert(vector.file == IBC_REG_FILE_LOGICAL);

      if (flag_reg_num_bits(flag) < FLAG_CHUNK_BITS) {
         assert(start_chunk == 0 && num_chunks == 1);
         assert(flag_reg_num_chunks(flag) == 1);
         if (!(state->valid[chunk] & FLAG_REP_FLAG)) {
            ibc_builder_push_we_all(b, flag_reg_num_bits(flag));
            ibc_build_alu(b, IBC_ALU_OP_MOV, ibc_null(vector.type),
                          ibc_flag_ref(chunk * FLAG_CHUNK_BITS),
                          BRW_CONDITIONAL_NZ, &vector, 1);
            ibc_builder_pop(b);
            state->valid[chunk] |= FLAG_REP_FLAG;
         }
      } else {
         const unsigned max_width =
            (2 * REG_SIZE) / ibc_type_byte_size(vector.type);
         assert(max_width % FLAG_CHUNK_BITS == 0);
         const unsigned max_width_chunks = max_width / FLAG_CHUNK_BITS;
         const unsigned end_chunk = start_chunk + num_chunks - 1;

         int copy_start = -1;
         for (unsigned c = start_chunk; c <= end_chunk; c++) {
            if (copy_start < 0 && !(state->valid[chunk + c] & FLAG_REP_FLAG))
               copy_start = c;

            if (copy_start < 0)
               continue;

            if (c == end_chunk ||
                (state->valid[chunk + c] & FLAG_REP_FLAG) ||
                (c - copy_start) == max_width_chunks ||
                (c - copy_start) == max_pow2_divisor(copy_start)) {
               unsigned copy_bit = copy_start * FLAG_CHUNK_BITS;
               unsigned copy_width = (c - copy_start) * FLAG_CHUNK_BITS;
               if (c == end_chunk)
                  copy_width += FLAG_CHUNK_BITS;
               ibc_builder_push_group(b, flag->logical.simd_group + copy_bit,
                                         copy_width);
               ibc_build_alu(b, IBC_ALU_OP_MOV, ibc_null(vector.type),
                             ibc_flag_ref(chunk * FLAG_CHUNK_BITS + copy_bit),
                             BRW_CONDITIONAL_NZ, &vector, 1);
               ibc_builder_pop(b);
            }

            /* We've either done a copy or set copy_group to ensure this chunk
             * gets copied.
             */
            state->valid[chunk + c] |= FLAG_REP_FLAG;
         }
      }
      return;
   }

   if (state->regs[flag->index].scalar.file != IBC_REG_FILE_NONE) {
      load_from_scalar(chunk, state);
      return;
   }

   unreachable("Must have some sort of GRF");
}

static void
evict_all_flags(uint32_t ip, struct ibc_assign_flags_state *state)
{
   free_dead_flags(ip, state);

   for (unsigned chunk = 0; chunk < TOTAL_FLAG_CHUNKS; chunk++)
      evict_flag(chunk, state);
}

static int
find_flag(const ibc_reg *reg, struct ibc_assign_flags_state *state)
{
   for (unsigned chunk = 0; chunk < TOTAL_FLAG_CHUNKS; chunk++) {
      if (state->assign[chunk] == reg)
         return chunk;
   }

   return -1;
}

static int
find_or_assign_flag(const ibc_reg *reg, uint32_t ip, bool opportunistic,
                    struct ibc_assign_flags_state *state)
{
   /* First, try and look it up */
   int chunk = find_flag(reg, state);
   if (chunk >= 0)
      return chunk;

   const unsigned num_chunks = flag_reg_num_chunks(reg);
   unsigned align_mul, align_offset;
   if (reg->file == IBC_REG_FILE_LOGICAL) {
      unsigned align_mul_bits =
         MAX2(16, reg->logical.simd_group + reg->logical.simd_width);
      assert(align_mul_bits % FLAG_CHUNK_BITS == 0);
      assert(reg->logical.simd_group % FLAG_CHUNK_BITS == 0);
      align_mul = align_mul_bits / FLAG_CHUNK_BITS;
      align_offset = (reg->logical.simd_group / FLAG_CHUNK_BITS) % align_mul;
   } else {
      assert(reg->file == IBC_REG_FILE_FLAG);
      assert(reg->flag.align_mul >= 16);
      assert(reg->flag.align_offset % FLAG_CHUNK_BITS == 0);
      align_mul = reg->flag.align_mul / FLAG_CHUNK_BITS;
      align_offset = reg->flag.align_offset / FLAG_CHUNK_BITS;
   }

   /* Next, try and find a free slot */
   for (chunk = align_offset; chunk < TOTAL_FLAG_CHUNKS; chunk += align_mul) {
      if (state->assign[chunk] != NULL)
         continue;

      bool all_chunks_free = true;
      for (unsigned c = 1; c < num_chunks; c++) {
         if (state->assign[chunk + c] != NULL) {
            all_chunks_free = false;
            break;
         }
      }
      if (all_chunks_free)
         goto assigned;
   }

   /* If we don't have any free slots, get rid of the register with the
    * longest remaining live range.  The hope here is that any shorter live
    * ranges will go away by themselves before we need to re-allocate them.
    * It's a heuristic.
    */
   uint32_t max_end = 0;
   int max_end_chunk = -1;
   for (chunk = align_offset; chunk < TOTAL_FLAG_CHUNKS; chunk += align_mul) {
      for (unsigned c = 1; c < num_chunks; c++) {
         if (state->assign[chunk + c]) {
            const ibc_reg_live_intervals *iter_rli =
               &state->live->regs[state->assign[chunk + c]->index];
            if (iter_rli->physical_end > max_end) {
               max_end = iter_rli->physical_end;
               max_end_chunk = chunk;
            }
         }
      }
   }
   assert(max_end_chunk >= 0);
   chunk = max_end_chunk;

   /* If this is an opportunistic allocation, we only want to evict a register
    * if its live range is shorter than the register we're opportunistically
    * trying to allocation.  The idea here is that if the live range of this
    * register is longer, we would be evicted before we would be able to take
    * advantage of the opportunistic flag allocation.  It's a heuristic.
    */
   if (opportunistic && max_end < state->live->regs[reg->index].physical_end)
      return -1;

   for (unsigned c = 0; c < num_chunks; c++)
      evict_flag(chunk + c, state);

assigned:

   {
      enum flag_rep valid = FLAG_REP_ALL;

      /* Detect if this flag register is live right before the current
       * instruction.  If it's not then we can assume the flag register starts
       * off valid.  If it is even somewhat live, we assume the whole thing is
       * valid.  In theory, we could use a finer granularity for this but it
       * doesn't seem worth the bother.
       *
       * ip = 0 should only be true for the START instruction.
       */
      assert(ip > 0);
      const ibc_reg_live_intervals *rli = &state->live->regs[reg->index];
      for (unsigned i= 0; i < rli->num_chunks; i++) {
         if (interval_set_contains(rli->chunk_live[i], ip - 1)) {
            valid &= FLAG_REP_FLAG;
            break;
         }
      }

      for (unsigned c = 0; c < num_chunks; c++) {
         state->assign[chunk + c] = reg;
         state->valid[chunk + c] = FLAG_REP_VECTOR | FLAG_REP_SCALAR;
      }
   }

   return chunk;
}

static void
flag_ref_chunk_range(const ibc_ref *ref,
                     uint8_t simd_group, uint8_t simd_width,
                     unsigned *start_chunk, unsigned *num_chunks)
{
   if (ref->type == IBC_TYPE_FLAG) {
      if (flag_reg_num_bits(ref->reg) < FLAG_CHUNK_BITS) {
         assert(flag_reg_num_bits(ref->reg) == 1);
         if (ref->file == IBC_REG_FILE_LOGICAL)
            assert(ref->reg->logical.simd_group == 0);
         assert(simd_group == 0 && simd_width == 1);
         *start_chunk = 0;
         *num_chunks = 1;
      } else {
         unsigned start_bit;
         if (ref->file == IBC_REG_FILE_LOGICAL) {
            start_bit = simd_group - ref->reg->logical.simd_group;
         } else {
            assert(ref->file == IBC_REG_FILE_FLAG);
            start_bit = ref->flag.bit;
         }
         assert(start_bit % FLAG_CHUNK_BITS == 0);
         assert(simd_width % FLAG_CHUNK_BITS == 0);
         *start_chunk = start_bit / FLAG_CHUNK_BITS;
         *num_chunks = simd_width / FLAG_CHUNK_BITS;
      }
   } else {
      assert(ref->file == IBC_REG_FILE_FLAG);
      assert(ref->flag.bit % FLAG_CHUNK_BITS == 0);
      *start_chunk = ref->flag.bit / FLAG_CHUNK_BITS;
      *num_chunks = ibc_type_bit_size(ref->type) / FLAG_CHUNK_BITS;
   }
}

static bool
flag_ref_valid(const ibc_instr *instr, const ibc_ref *ref, unsigned chunk,
               struct ibc_assign_flags_state *state)
{
   unsigned start_chunk, num_chunks;
   flag_ref_chunk_range(ref, instr->simd_group, instr->simd_width,
                        &start_chunk, &num_chunks);

   enum flag_rep valid = FLAG_REP_ALL;
   for (unsigned c = 0; c < num_chunks; c++) {
      assert(state->assign[chunk + start_chunk + c] == ref->reg);
      valid &= state->valid[chunk + start_chunk + c];
   }

   return valid & FLAG_REP_FLAG;
}

static void
instr_set_flag_ref(ibc_instr *instr, ibc_ref *ref, unsigned chunk,
                   enum flag_rep written,
                   struct ibc_assign_flags_state *state)
{
   unsigned start_chunk, num_chunks;
   flag_ref_chunk_range(ref, instr->simd_group, instr->simd_width,
                        &start_chunk, &num_chunks);

   load_flag_if_needed(chunk, start_chunk, num_chunks, written, state);

   ibc_ref flag_ref = ibc_flag_ref((chunk + start_chunk) * FLAG_CHUNK_BITS);
   ibc_instr_set_ref(instr, ref, flag_ref);

   if (written) {
      for (unsigned c = 0; c < num_chunks; c++)
         state->valid[chunk + start_chunk + c] = written;
   }
}

static void
rewrite_flag_ref(ibc_ref *ref, ibc_instr *write_instr,
                 uint8_t simd_group, uint8_t simd_width,
                 struct ibc_assign_flags_state *state)
{
   if (ref->file == IBC_REG_FILE_LOGICAL && ref->type == IBC_TYPE_FLAG) {
      assert(ref->reg->index < state->live->num_regs);

      unsigned start_chunk, num_chunks;
      flag_ref_chunk_range(ref, simd_group, simd_width,
                           &start_chunk, &num_chunks);

      int chunk = find_flag(ref->reg, state);
      if (chunk >= 0 && !write_instr)
         load_vector_if_needed(chunk, start_chunk, num_chunks, state);

      ibc_ref vector = state->regs[ref->reg->index].vector;
      vector.logical = ref->logical;
      ibc_instr_set_ref(write_instr, ref, vector);

      if (chunk >= 0 && write_instr) {
         for (unsigned c = 0; c < num_chunks; c++)
            state->valid[chunk + start_chunk + c] = FLAG_REP_VECTOR;
      }
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
         int chunk = find_or_assign_flag((ibc_reg *)ref->reg,
                                         write_instr->index,
                                         true, /* opportunistic */
                                         state);
         instr_set_flag_ref(write_instr, ref, chunk, FLAG_REP_FLAG, state);
      } else {
         int chunk = find_flag(ref->reg, state);
         if (chunk >= 0 && flag_ref_valid(NULL, ref, chunk, state)) {
            instr_set_flag_ref(NULL, ref, chunk, FLAG_REP_NONE, state);
         } else {
            /* If we're a read and it's not currently in a flag, pull from the
             * scalar GRF.  We should have one.
             */
            load_scalar_if_needed(chunk, state);
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
   rewrite_flag_ref(ref, NULL, simd_group, simd_width, _state);
   return true;
}

static bool
rewrite_flag_write_cb(ibc_reg_write *write, ibc_ref *ref, void *_state)
{
   if (write->instr) {
      rewrite_flag_ref(ref, write->instr, write->instr->simd_group,
                       write->instr->simd_width, _state);
   }
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

      /* TODO: Implement simultaneous flag read/write. */
      assert(instr->predicate == BRW_PREDICATE_NONE ||
             !ibc_instr_writes_flag(instr));

      if (instr->type == IBC_INSTR_TYPE_ALU) {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);
         if (alu->op == IBC_ALU_OP_CMP) {
            ibc_reg *logical = (ibc_reg *)instr->flag.reg;
            int chunk = find_or_assign_flag(logical, instr->index,
                                            false, /* opportunistic */
                                            &state);

            enum flag_rep written = FLAG_REP_FLAG;

            /* If the vector version matches, we can write to it directly and
             * avoid getting it out-of-sync.
             */
            ibc_ref vector = state.regs[logical->index].vector;
            if (vector.file == IBC_REG_FILE_LOGICAL &&
                vector.reg->logical.bit_size == cmp_bit_size(alu)) {
               vector.type = ibc_type_base_type(alu->src[0].ref.type) |
                             ibc_type_bit_size(vector.type);
               ibc_instr_set_ref(&alu->instr, &alu->dest, vector);
               written |= FLAG_REP_VECTOR;
            }

            instr_set_flag_ref(instr, &instr->flag, chunk, written, &state);
         } else if (alu->dest.file == IBC_REG_FILE_LOGICAL &&
                    alu->dest.reg->logical.bit_size == 1 &&
                    alu->op != IBC_ALU_OP_SEL &&
                    alu->instr.predicate == BRW_PREDICATE_NONE &&
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
            int chunk = find_or_assign_flag(logical, instr->index,
                                            true, /* opportunistic */
                                            &state);
            if (chunk >= 0) {
               ibc_alu_instr_set_cmod(alu, alu->dest, BRW_CONDITIONAL_NZ);
               instr_set_flag_ref(instr, &instr->flag, chunk,
                                  FLAG_REP_FLAG | FLAG_REP_VECTOR, &state);
            }
         }
      }

      if (instr->flag.file != IBC_REG_FILE_NONE && instr->flag.reg) {
         int chunk = find_or_assign_flag(instr->flag.reg, instr->index,
                                         false, /* opportunistic */
                                         &state);
         enum flag_rep written =
            ibc_instr_writes_flag(instr) ? FLAG_REP_FLAG : FLAG_REP_NONE;
         instr_set_flag_ref(instr, &instr->flag, chunk, written, &state);
      }

      ibc_instr_foreach_read(instr, rewrite_flag_read_cb, &state);
      ibc_instr_foreach_reg_write(instr, rewrite_flag_write_cb, &state);

      if (instr->type == IBC_INSTR_TYPE_ALU) {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);
         if (alu->dest.file == IBC_REG_FILE_NONE &&
             alu->dest.type == IBC_TYPE_FLAG)
            alu->dest.type = alu->src[0].ref.type;
      }

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
