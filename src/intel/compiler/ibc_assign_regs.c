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

#include "util/rb_tree.h"
#include "util/vma.h"

static bool
ibc_alu_instr_is_raw_mov(const ibc_alu_instr *alu)
{
   return alu->op == IBC_ALU_OP_MOV &&
          alu->dest.type == alu->src[0].ref.type &&
          alu->src[0].mod == IBC_ALU_SRC_MOD_NONE &&
          !alu->saturate;
}

void
ibc_assign_logical_reg_strides(ibc_shader *shader)
{
   ibc_foreach_reg(reg, shader) {
      if (reg->file != IBC_REG_FILE_LOGICAL)
         continue;

      if (reg->logical.stride > 0)
         continue;

      /* Scalars and booleans won't be mapped to HW regs so they don't need to
       * have assigned strides.
       */
      if (reg->logical.bit_size == 1)
         continue;

      /* At the very least, we want it to be the size of the register */
      assert(reg->logical.bit_size >= 8);
      reg->logical.stride = reg->logical.bit_size / 8;

      if (reg->logical.simd_width == 1)
         continue;

      ibc_instr *ssa_instr = ibc_reg_ssa_instr(reg);
      if (!ssa_instr || ssa_instr->type != IBC_INSTR_TYPE_ALU)
         continue;

      ibc_alu_instr *alu = ibc_instr_as_alu(ssa_instr);
      assert(alu->dest.reg == reg);

      reg->logical.stride = MAX2(reg->logical.stride,
                                 ibc_type_byte_size(alu->dest.type));
      for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
         reg->logical.stride = MAX2(reg->logical.stride,
                                    ibc_type_byte_size(alu->src[i].ref.type));
      }

      /* Only raw MOV supports a packed-byte destination */
      if (reg->logical.stride == 1 && !ibc_alu_instr_is_raw_mov(alu))
         reg->logical.stride = 2;
   }
}

struct ibc_phys_reg {
   /** Byte offset into the 4K register file */
   uint16_t byte;

   /** Size (in bytes) of this register */
   uint16_t size;

   /** Start (inclusive) of the live range of this register */
   uint32_t start;

   /** End (exclusive) of the live range of this register */
   uint32_t end;

   /** Node in ibc_phys_reg_alloc::regs */
   struct rb_node node;
};

static int
ibc_phys_reg_rb_cmp(const struct rb_node *an, const struct rb_node *bn)
{
   const struct ibc_phys_reg *a, *b;
   a = rb_node_data(struct ibc_phys_reg, an, node);
   b = rb_node_data(struct ibc_phys_reg, bn, node);

   if (a->end > b->end)
      return -1;
   if (a->end < b->end)
      return 1;
   return 0;
}

struct ibc_phys_reg_alloc {
   struct util_vma_heap heap;

   /** Node in the red-black tree of ibc_phys_reg
    *
    * This tree is sorted by physical register end so that we can easily
    * return physical registers to the physical allocator.
    */
   struct rb_tree regs;
};

static void
ibc_phys_reg_alloc_init(struct ibc_phys_reg_alloc *alloc)
{
   util_vma_heap_init(&alloc->heap, 4096, 4096);
   rb_tree_init(&alloc->regs);
}

static void
ibc_phys_reg_alloc_finish(struct ibc_phys_reg_alloc *alloc)
{
   util_vma_heap_finish(&alloc->heap);
}

static bool
ibc_phys_reg_alloc(struct ibc_phys_reg_alloc *alloc,
                   int16_t fixed_hw_grf_byte,
                   uint16_t size, uint16_t align,
                   uint32_t start, uint32_t end,
                   struct ibc_phys_reg *reg_out)
{
   uint16_t byte;
   if (fixed_hw_grf_byte < 0) {
      uint64_t addr = util_vma_heap_alloc(&alloc->heap, size, align);
      if (addr == 0)
         return false;
      assert(addr >= 4096 && addr + size <= 8192);
      byte = 8192 - size - addr;
   } else {
      assert(byte + size <= 4096);
      uint64_t addr = 8192 - size - fixed_hw_grf_byte;
      assert(addr >= 4096 && addr + size <= 8192);
      if (!util_vma_heap_alloc_addr(&alloc->heap, addr, size))
         return false;
      byte = fixed_hw_grf_byte;
   }

   *reg_out = (struct ibc_phys_reg) {
      .byte = byte,
      .size = size,
      .start = start,
      .end = end,
   };
   rb_tree_insert(&alloc->regs, &reg_out->node, ibc_phys_reg_rb_cmp);

   return true;
}

static void
ibc_phys_reg_extend_live_range(struct ibc_phys_reg_alloc *alloc,
                               struct ibc_phys_reg *reg,
                               uint32_t new_end)
{
   if (new_end <= reg->end)
      return;

   reg->end = new_end;
   rb_tree_remove(&alloc->regs, &reg->node);
   rb_tree_insert(&alloc->regs, &reg->node, ibc_phys_reg_rb_cmp);
}

/** Frees all registers that end at or before the given ip */
static void
ibc_phys_reg_alloc_free_regs(struct ibc_phys_reg_alloc *alloc,
                             uint32_t ip)
{
   while (!rb_tree_is_empty(&alloc->regs)) {
      struct rb_node *n = rb_tree_first(&alloc->regs);
      struct ibc_phys_reg *reg = rb_node_data(struct ibc_phys_reg, n, node);
      if (reg->end > ip)
         break;

      util_vma_heap_free(&alloc->heap, 8192 - reg->size - reg->byte, reg->size);
      rb_tree_remove(&alloc->regs, &reg->node);
   }
}

struct ibc_strided_reg_chunk {
   /* The start and end of the next hole */
   uint32_t hole_start;
   uint32_t hole_end;

   struct interval_set *live_range;
};

#define STRIDED_REG_SIMD_GRANULARITY 8

struct ibc_strided_reg {
   struct ibc_phys_reg phys;

   /* Link in either ibc_strided_reg_alloc::free or ::busy. */
   struct list_head link;

   /* byte_size will always be the same as the base strided reg allocator's
    * stride.  We repeat it here for convenience and symmetry.
    */
   uint8_t byte_size;
   uint8_t num_comps;
   uint8_t simd_group;
   uint8_t simd_width;

   uint32_t hole_start;
   uint32_t hole_end;

   struct ibc_strided_reg_chunk chunks[0];
};

static unsigned
ibc_strided_reg_num_chunks(const struct ibc_strided_reg *reg)
{
   unsigned num_simd_groups =
      DIV_ROUND_UP(reg->simd_width, STRIDED_REG_SIMD_GRANULARITY);

   return reg->byte_size * num_simd_groups * reg->num_comps;
}

static struct ibc_strided_reg_chunk *
ibc_strided_reg_chunk(struct ibc_strided_reg *reg,
                      uint8_t byte,
                      uint8_t comp,
                      uint8_t simd_group)
{
   assert(simd_group >= reg->simd_group);
   unsigned simd_idx = (simd_group - reg->simd_group) /
                       STRIDED_REG_SIMD_GRANULARITY;
   unsigned num_simd_groups =
      DIV_ROUND_UP(reg->simd_width, STRIDED_REG_SIMD_GRANULARITY);

   unsigned idx = byte +
                  simd_idx * reg->byte_size +
                  comp * reg->byte_size * num_simd_groups;
   return &reg->chunks[idx];
}

static void
ibc_strided_reg_update_holes(struct ibc_strided_reg *reg,
                             uint32_t ip)
{
   unsigned num_chunks = ibc_strided_reg_num_chunks(reg);

   reg->hole_start = UINT32_MAX;
   reg->hole_end = 0;
   for (unsigned c = 0; c < num_chunks; c++) {
      /* Only re-compute holes when the instruction index goes past the end or
       * when it's been explicitly reset (indicated by hole_end == 0).
       */
      if (ip >= reg->chunks[c].hole_end) {
         struct interval_set *chunk_live = reg->chunks[c].live_range;
         for (uint32_t i = 0; i < chunk_live->count; i++) {
            if (ip < chunk_live->intervals[i].end) {
               reg->chunks[c].hole_start =
                  i == 0 ? 0 : chunk_live->intervals[i - 1].end;
               reg->chunks[c].hole_end = chunk_live->intervals[i].start;
               goto found_hole;
            }
         }

         assert(ip >= interval_set_end(chunk_live));
         reg->chunks[c].hole_start = interval_set_end(chunk_live);
         reg->chunks[c].hole_end = UINT32_MAX;
      }

   found_hole:
      reg->hole_start = MIN2(reg->hole_start, reg->chunks[c].hole_start);
      reg->hole_end = MAX2(reg->hole_end, reg->chunks[c].hole_end);
   }
}

static inline bool
ibc_strided_reg_is_busy(struct ibc_strided_reg *reg,
                        uint32_t ip)
{
   unsigned num_chunks = ibc_strided_reg_num_chunks(reg);

   for (unsigned c = 0; c < num_chunks; c++) {
      assert(ip < reg->chunks[c].hole_end);
      if (ip >= reg->chunks[c].hole_start)
         return false;
   }

   return true;
}

static struct ibc_strided_reg *
ibc_strided_reg_create(void *mem_ctx,
                       uint32_t ip,
                       uint8_t byte_size, uint8_t num_comps,
                       uint8_t simd_group, uint8_t simd_width,
                       struct interval_set **live,
                       uint8_t simd_stride, uint8_t comp_stride)
{
   struct ibc_strided_reg base = {
      .byte_size = byte_size,
      .num_comps = num_comps,
      .simd_group = simd_group,
      .simd_width = simd_width,
   };
   unsigned num_chunks = ibc_strided_reg_num_chunks(&base);

   const size_t size = sizeof(struct ibc_strided_reg) +
                       sizeof(struct ibc_strided_reg_chunk) * num_chunks;
   struct ibc_strided_reg *reg = rzalloc_size(mem_ctx, size);

   *reg = base;

   /* We'll let the caller fill out the physical properties: byte, size,
    * start, end.
    */

   for (unsigned c = 0; c < num_comps; c++) {
      for (unsigned s = 0; s < simd_width; s += STRIDED_REG_SIMD_GRANULARITY) {
         for (unsigned b = 0; b < byte_size; b++) {
            struct ibc_strided_reg_chunk *chunk =
               ibc_strided_reg_chunk(reg, b, c, simd_group + s);
            chunk->live_range =
               live[b +
                    (s / STRIDED_REG_SIMD_GRANULARITY) * simd_stride +
                    c * comp_stride];
         }
      }
   }

   ibc_strided_reg_update_holes(reg, ip);

   return reg;
}

static bool
ibc_strided_reg_try_find_hole(struct ibc_strided_reg *reg,
                              uint32_t ip,
                              uint8_t byte_size, uint8_t num_comps,
                              uint8_t simd_group, uint8_t simd_width,
                              struct interval_set **live,
                              uint8_t simd_stride, uint8_t comp_stride,
                              uint8_t *alloc_byte, uint8_t *alloc_comp)
{
   if (simd_group < reg->simd_group ||
       simd_group + simd_width > reg->simd_group + reg->simd_width)
      return false;

   assert(byte_size <= reg->byte_size);
   if (num_comps > reg->num_comps)
      return false;

   /* From the SKL PRM Vol. 7 "Register Region Restrictions":
    *
    *    "There is a relaxed alignment rule for byte destinations. When the
    *    destination type is byte (UB or B), destination data types can be
    *    aligned to either the lowest byte or the second lowest byte of the
    *    execution channel. For example, if one of the source operands is in
    *    word mode (a signed or unsigned word integer), the execution data
    *    type will be signed word integer. In this case the destination data
    *    bytes can be either all in the even byte locations or all in the odd
    *    byte locations."
    *
    * and
    *
    *    "There is a relaxed alignment rule for word destinations. When the
    *    destination type is word (UW, W, HF), destination data types can be
    *    aligned to either the lowest word or the second lowest word of the
    *    execution channel. This means the destination data words can be
    *    either all in the even word locations or all in the odd word
    *    locations."
    *
    * If we assume that the only reason why a register would be strided out
    * wider than its base type is because of region restrictions due to
    * execution types, this means that we can only scan the first two chunks
    * for byte and word registers and have to keep things aligned for dword
    * and qword registers.
    */
   unsigned scan_bytes =
      byte_size <= 2 ? MIN2(reg->byte_size, byte_size * 2) : byte_size;

   unsigned byte, comp;
   for (byte = 0; byte < scan_bytes; byte += byte_size) {
      for (comp = 0; comp <= reg->num_comps - num_comps; comp++) {
         for (unsigned c = 0; c < num_comps; c++) {
            for (unsigned s = 0; s < simd_width; s += STRIDED_REG_SIMD_GRANULARITY) {
               for (unsigned b = 0; b < byte_size; b++) {
                  struct ibc_strided_reg_chunk *chunk =
                     ibc_strided_reg_chunk(reg, byte + b, comp + c,
                                                simd_group + s);
                  struct interval_set *byte_live =
                     live[b +
                          (s / STRIDED_REG_SIMD_GRANULARITY) * simd_stride +
                          c * comp_stride];
                  if (chunk->hole_start > interval_set_start(byte_live) ||
                      interval_set_end(byte_live) > chunk->hole_end)
                     goto next_byte;
               }
            }
         }
         goto found;

      next_byte:
         continue;
      }
   }

   return false;

found:
   for (unsigned c = 0; c < num_comps; c++) {
      for (unsigned s = 0; s < simd_width; s += STRIDED_REG_SIMD_GRANULARITY) {
         for (unsigned b = 0; b < byte_size; b++) {
            struct ibc_strided_reg_chunk *chunk =
               ibc_strided_reg_chunk(reg, byte + b, comp + c,
                                          simd_group + s);
            struct interval_set *byte_live =
               live[b +
                    (s / STRIDED_REG_SIMD_GRANULARITY) * simd_stride +
                    c * comp_stride];

            /* Union the live intervals and flag the hole as ending at 0 so
             * that it gets reset when we call ibc_strided_reg_update_holes.
             */
            chunk->live_range =
               interval_set_from_union(reg, chunk->live_range, byte_live);
            chunk->hole_end = 0;
         }
      }
   }

   ibc_strided_reg_update_holes(reg, ip);

   *alloc_byte = byte;
   *alloc_comp = comp;
   return true;
}

struct ibc_strided_reg_alloc {
   struct ibc_phys_reg_alloc *phys_alloc;
   const ibc_live_intervals *live;
   uint8_t stride;

   void *mem_ctx;

   struct ibc_strided_reg *regs;

   /** List of registers which are at lest partially free at the current ip */
   struct list_head free;

   /** List of registers which are fully busy at the current ip */
   struct list_head busy;
};

static void
ibc_strided_reg_alloc_init(struct ibc_strided_reg_alloc *alloc,
                           const ibc_live_intervals *live,
                           struct ibc_phys_reg_alloc *phys_alloc,
                           uint8_t stride, void *mem_ctx)
{
   *alloc = (struct ibc_strided_reg_alloc) {
      .phys_alloc = phys_alloc,
      .live = live,
      .stride = stride,
      .mem_ctx = mem_ctx,
   };

   alloc->regs = rzalloc_array(mem_ctx, struct ibc_strided_reg,
                               live->num_regs);

   list_inithead(&alloc->free);
   list_inithead(&alloc->busy);
}

static void
ibc_strided_reg_alloc_finish(struct ibc_strided_reg_alloc *alloc)
{
}

static struct ibc_strided_reg *
ibc_strided_reg_alloc(struct ibc_strided_reg_alloc *alloc,
                      const ibc_live_intervals *live,
                      const ibc_reg *reg,
                      int16_t fixed_hw_grf_byte,
                      uint8_t *alloc_byte, uint8_t *alloc_comp)
{
   const ibc_reg_live_intervals *rli = &live->regs[reg->index];
   const uint32_t ip = rli->physical_start;

   assert(reg->file == IBC_REG_FILE_LOGICAL);
   const ibc_logical_reg *lreg = &reg->logical;
   assert(lreg->bit_size % 8 == 0);
   unsigned byte_size = lreg->bit_size / 8;
   unsigned num_simd_groups =
      DIV_ROUND_UP(lreg->simd_width, STRIDED_REG_SIMD_GRANULARITY);

   assert(byte_size <= 8);
   assert(lreg->num_comps <= 4);
   assert(num_simd_groups <= 4);
   const uint8_t simd_stride = byte_size;
   const uint8_t comp_stride = simd_stride * num_simd_groups;
   struct interval_set *live_ranges[8 * 4 * 4];

   /* Compute the RA liveness sets from the ones given by liveness analysis.
    * They may be at different granularities and this lets us keep the two
    * passes decoupled a bit.  Yes, it looks like a giant loop but for most
    * registers, it'll run only a couple of times and do almost zero work.
    */
   for (unsigned c = 0; c < lreg->num_comps; c++) {
      for (unsigned s = 0; s < lreg->simd_width; s += STRIDED_REG_SIMD_GRANULARITY) {
         for (unsigned b = 0; b < byte_size; b++) {
            BITSET_DECLARE(chunks, IBC_REG_LIVE_MAX_CHUNKS);
            const unsigned chunks_words = BITSET_WORDS(rli->num_chunks);
            memset(chunks, 0, chunks_words * sizeof(BITSET_WORD));

            const ibc_reg_ref ref = {
               .file = IBC_REG_FILE_LOGICAL,
               .type = IBC_TYPE_8_BIT,
               .logical = {
                  .byte = b,
                  .comp = c,
               },
               .reg = reg,
            };
            const uint8_t ref_simd_group = lreg->simd_group + s;
            const uint8_t ref_simd_width =
               MIN2(lreg->simd_width, STRIDED_REG_SIMD_GRANULARITY);
            ibc_live_intervals_reg_ref_chunks(live, &ref, 1,
                                              ref_simd_group, ref_simd_width,
                                              chunks);

            struct interval_set *byte_live = NULL;
            for (unsigned w = 0; w < chunks_words; w++) {
               BITSET_WORD mask = chunks[w];
               while (mask) {
                  int b = u_bit_scan(&mask);
                  assert(b >= 0 && b < BITSET_WORDBITS);
                  struct interval_set *chunk_live =
                     rli->chunk_live[w * BITSET_WORDBITS + b];

                  if (chunk_live) {
                     if (byte_live) {
                        byte_live = interval_set_from_union(alloc->mem_ctx,
                                                            chunk_live,
                                                            byte_live);
                     } else {
                        byte_live = chunk_live;
                     }
                  }
               }
            }
            live_ranges[b +
                        (s / STRIDED_REG_SIMD_GRANULARITY) * simd_stride +
                        c * comp_stride] = byte_live;
         }
      }
   }


   if (fixed_hw_grf_byte < 0) {
      /* First try searching through the list of free registers and see if we
       * can re-use one of them.
       */
      list_for_each_entry_safe(struct ibc_strided_reg, sreg, &alloc->free, link) {
         /* If we come across a register whose physical range has already
          * ended, we can't use it and we'll never be able to use it again.
          * Just remove it from the list.
          */
         if (sreg->phys.end <= ip) {
            list_del(&sreg->link);
            continue;
         }

         if (ibc_strided_reg_try_find_hole(sreg, ip,
                                           byte_size, lreg->num_comps,
                                           lreg->simd_group,
                                           lreg->simd_width,
                                           live_ranges,
                                           simd_stride, comp_stride,
                                           alloc_byte, alloc_comp)) {

            /* If the first hole now starts after the current instruction, we
             * need to move it to the busy list.
             */
            if (sreg->hole_start > ip) {
               list_del(&sreg->link);
               list_addtail(&sreg->link, &alloc->busy);
            }

            /* If this allocation ends up growing the physical register, we
             * need to update it and move it to its new location in the RB
             * tree.
             */
            ibc_phys_reg_extend_live_range(alloc->phys_alloc, &sreg->phys,
                                           rli->physical_end);

            return sreg;
         }
      }
   }

   /* If we got here, we didn't find any preexisting registers to allocate
    * from so we need to get ourselves a new one.
    */
   struct ibc_strided_reg *sreg =
      ibc_strided_reg_create(alloc->mem_ctx, ip,
                             alloc->stride, lreg->num_comps,
                             lreg->simd_group, lreg->simd_width,
                             live_ranges, simd_stride, comp_stride);

   uint16_t size = alloc->stride * lreg->simd_width * lreg->num_comps;
   uint16_t align = MIN2(alloc->stride * lreg->simd_width, 32);
   if (!ibc_phys_reg_alloc(alloc->phys_alloc,
                           fixed_hw_grf_byte, size, align,
                           rli->physical_start, rli->physical_end,
                           &sreg->phys)) {
      ralloc_free(sreg);
      return NULL;
   }

   if (sreg->hole_start > ip) {
      list_addtail(&sreg->link, &alloc->busy);
   } else {
      list_addtail(&sreg->link, &alloc->free);
   }

   *alloc_byte = 0;
   *alloc_comp = 0;
   return sreg;
}

static void
ibc_strided_reg_alloc_update_reg_holes(struct ibc_strided_reg_alloc *alloc,
                                       uint32_t ip)
{
   list_for_each_entry_safe(struct ibc_strided_reg, reg, &alloc->free, link) {
      /* If we come across a register whose physical range has already ended,
       * we'll never be able to use it again.  Might as well clean it up.
       */
      if (reg->phys.end <= ip) {
         list_del(&reg->link);
         continue;
      }

      ibc_strided_reg_update_holes(reg, ip);
   }

   list_for_each_entry(struct ibc_strided_reg, reg, &alloc->busy, link)
      assert(ibc_strided_reg_is_busy(reg, ip));
}

struct ibc_reg_assignment {
   ibc_reg *reg;

   uint32_t physical_start;

   /** Node in ibc_assign_regs_state::alloc_order */
   struct rb_node node;

   struct ibc_phys_reg *preg;

   struct ibc_strided_reg *sreg;
   uint8_t sreg_byte;
   uint8_t sreg_comp;
};

static int
ibc_reg_assignment_rb_cmp(const struct rb_node *an, const struct rb_node *bn)
{
   const struct ibc_reg_assignment *a, *b;
   a = rb_node_data(struct ibc_reg_assignment, an, node);
   b = rb_node_data(struct ibc_reg_assignment, bn, node);

   if (a->physical_start > b->physical_start)
      return -1;
   if (a->physical_start < b->physical_start)
      return 1;
   return 0;
}

struct ibc_assign_regs_state {
   void *mem_ctx;

   const ibc_live_intervals *live;

   /** Current instruction index */
   uint32_t ip;

   /** True if we're currently processing a read */
   bool is_read;

   /** Physical register allocator */
   struct ibc_phys_reg_alloc phys_alloc;

   /** Strided allocators for 1, 2, 4, and 8 byte strides */
   struct ibc_strided_reg_alloc strided_alloc[4];

   struct ibc_reg_assignment *assign;

   /** Node in the red-black tree of ibc_reg_assignment
    *
    * This tree is sorted by physical register start so that we can easily
    * figure out which physical register to allocate next.
    */
   struct rb_tree alloc_order;
};

static bool
should_assign_reg(const ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_HW_GRF:
      return reg != NULL;
   case IBC_REG_FILE_LOGICAL:
      return true;
   default:
      return false;
   }
}

static bool
rewrite_ref_and_update_reg(ibc_reg_ref *ref, int8_t num_comps,
                           uint8_t simd_group, uint8_t simd_width,
                           void *_state)
{
   struct ibc_assign_regs_state *state = _state;
   if (ref->file != IBC_REG_FILE_LOGICAL &&
       ref->file != IBC_REG_FILE_HW_GRF)
      return true;

   if (ref->reg == NULL) {
      assert(ref->file == IBC_REG_FILE_HW_GRF);
      return true;
   }

   const ibc_reg *reg = ref->reg;
   assert(reg->index < state->live->num_regs);
   struct ibc_reg_assignment *assign = &state->assign[reg->index];

   switch (ref->file) {
   case IBC_REG_FILE_HW_GRF:
      ref->hw_grf.byte += assign->preg->byte;
      break;

   case IBC_REG_FILE_LOGICAL: {
      assert(reg->logical.bit_size % 8 == 0);
      assert(reg->logical.stride >= reg->logical.bit_size / 8);
      unsigned stride = ref->reg->logical.stride;

      /* Stash this so we can access it unchanged */
      const ibc_logical_reg_ref logical = ref->logical;

      ref->file = IBC_REG_FILE_HW_GRF;
      if (assign->sreg) {
         ibc_strided_reg_update_holes(assign->sreg, state->ip);

         ref->hw_grf.byte = assign->sreg->phys.byte;
         ref->hw_grf.byte += (assign->sreg_comp + logical.comp) *
                             ref->reg->logical.simd_width * stride;
         ref->hw_grf.byte += assign->sreg_byte + logical.byte;
      } else {
         assert(reg->logical.simd_width == 1);
         assert(reg->logical.stride == reg->logical.bit_size / 8);
         ref->hw_grf.byte = assign->preg->byte;
         ref->hw_grf.byte += logical.comp * (reg->logical.bit_size / 8);
         ref->hw_grf.byte += logical.byte;
      }

      if (logical.broadcast) {
         ref->hw_grf.byte += logical.simd_channel * stride;
      } else if (ref->reg->logical.simd_width > 1) {
         ref->hw_grf.byte +=
            (simd_group - assign->sreg->simd_group) * stride;
      }

      if (logical.broadcast ||
          (ref->reg->logical.simd_width == 1 && state->is_read)) {
         ref->hw_grf.vstride = 0;
         ref->hw_grf.width = 1;
         ref->hw_grf.hstride = 0;
      } else {
         ref->hw_grf.hstride = stride;
         ref->hw_grf.width = 8;
         ref->hw_grf.vstride = stride * ref->hw_grf.width;
      }
      break;
   }

   default:
      unreachable("Unhandled register file");
   }

   if (ref->write_instr) {
      list_del(&ref->write_link);
      ref->write_instr = NULL;
   }

   ref->reg = NULL;

   return true;
}

void
ibc_assign_regs(ibc_shader *shader)
{
   struct ibc_assign_regs_state state = {
      .mem_ctx = ralloc_context(NULL),
   };

   state.live = ibc_compute_live_intervals(shader, state.mem_ctx);

   ibc_phys_reg_alloc_init(&state.phys_alloc);
   for (unsigned i = 0; i < ARRAY_SIZE(state.strided_alloc); i++) {
      ibc_strided_reg_alloc_init(&state.strided_alloc[i],
                                 state.live,
                                 &state.phys_alloc,
                                 1 << i, /* stride in bytes */
                                 state.mem_ctx);
   }

   state.assign = ralloc_array(state.mem_ctx,
                               struct ibc_reg_assignment,
                               state.live->num_regs);
   rb_tree_init(&state.alloc_order);

   ibc_foreach_reg(reg, shader) {
      if (!should_assign_reg(reg))
         continue;

      assert(reg->index < state.live->num_regs);
      struct ibc_reg_assignment *assign = &state.assign[reg->index];
      *assign = (struct ibc_reg_assignment) {
         .reg = reg,
         .physical_start = state.live->regs[reg->index].physical_start,
      };
      rb_tree_insert(&state.alloc_order, &assign->node,
                     ibc_reg_assignment_rb_cmp);
   }

   ibc_foreach_instr_safe(instr, shader) {
      state.ip = instr->index;

      ibc_phys_reg_alloc_free_regs(&state.phys_alloc, state.ip);

      while (!rb_tree_is_empty(&state.alloc_order)) {
         struct rb_node *assign_node = rb_tree_first(&state.alloc_order);
         struct ibc_reg_assignment *assign =
            rb_node_data(struct ibc_reg_assignment, assign_node, node);

         if (assign->physical_start > state.ip)
            break;

         ibc_reg *reg = assign->reg;
         const ibc_reg_live_intervals *rli = &state.live->regs[reg->index];

         ibc_instr *ssa_instr = ibc_reg_ssa_instr(reg);
         int16_t fixed_hw_grf_byte = -1;
         if (ssa_instr && ssa_instr->type == IBC_INSTR_TYPE_INTRINSIC) {
            ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(ssa_instr);
            if (intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD) {
               assert(intrin->src[0].ref.file == IBC_REG_FILE_HW_GRF);
               assert(intrin->src[0].ref.reg == NULL);
               fixed_hw_grf_byte = intrin->src[0].ref.hw_grf.byte;
            }
         }

         bool success;
         switch (assign->reg->file) {
         case IBC_REG_FILE_HW_GRF:
            assign->preg = rzalloc(state.mem_ctx, struct ibc_phys_reg);
            success = ibc_phys_reg_alloc(&state.phys_alloc,
                                         fixed_hw_grf_byte,
                                         reg->hw_grf.size,
                                         reg->hw_grf.align,
                                         rli->physical_start,
                                         rli->physical_end,
                                         assign->preg);
            assert(success);
            break;

         case IBC_REG_FILE_LOGICAL:
            if (reg->logical.simd_width == 1) {
               assert(reg->logical.bit_size % 8 == 0);
               assign->preg = rzalloc(state.mem_ctx, struct ibc_phys_reg);
               uint16_t size = (reg->logical.bit_size / 8) *
                               reg->logical.num_comps;
               uint16_t align = reg->logical.bit_size / 8;
               success = ibc_phys_reg_alloc(&state.phys_alloc,
                                            fixed_hw_grf_byte,
                                            size, align,
                                            rli->physical_start,
                                            rli->physical_end,
                                            assign->preg);
               assert(success);
            } else {
               assert(reg->logical.bit_size % 8 == 0);
               assert(reg->logical.stride >= reg->logical.bit_size / 8);
               assert(reg->logical.stride > 0 && reg->logical.stride <= 8);
               /* If we've been provided with a specific HW GRF reg, then it
                * must be tightly packed.
                */
               assert(fixed_hw_grf_byte < 0 ||
                      reg->logical.stride == reg->logical.bit_size / 8);
               struct ibc_strided_reg_alloc *strided_alloc =
                  &state.strided_alloc[ffs(reg->logical.stride) - 1];

               assign->sreg = ibc_strided_reg_alloc(strided_alloc,
                                                    state.live, reg,
                                                    fixed_hw_grf_byte,
                                                    &assign->sreg_byte,
                                                    &assign->sreg_comp);
               assert(assign->sreg);
            }
            break;

         default:
            unreachable("Unhandled register file");
         }

         rb_tree_remove(&state.alloc_order, &assign->node);
      }

      if (instr->type == IBC_INSTR_TYPE_INTRINSIC) {
         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
         if (intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD) {
            assert(intrin->dest.reg);
            assert(ibc_reg_ssa_instr(intrin->dest.reg) == instr);
            ibc_instr_remove(instr);
            continue;
         }
      }

      state.is_read = true;
      ibc_instr_foreach_read(instr, rewrite_ref_and_update_reg, &state);
      state.is_read = false;
      ibc_instr_foreach_write(instr, rewrite_ref_and_update_reg, &state);

      if (instr->type == IBC_INSTR_TYPE_MERGE) {
         /* The merge instruction doesn't actually have any sources but it's
          * none-the-less a transition point for register live ranges and we
          * need to update strided register holes.
          */
         for (unsigned i = 0; i < ARRAY_SIZE(state.strided_alloc); i++) {
            ibc_strided_reg_alloc_update_reg_holes(&state.strided_alloc[i],
                                                   state.ip);
         }
      }
   }

   ibc_phys_reg_alloc_finish(&state.phys_alloc);
   for (unsigned i = 0; i < ARRAY_SIZE(state.strided_alloc); i++)
      ibc_strided_reg_alloc_finish(&state.strided_alloc[i]);
}

static unsigned
reg_size(ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_HW_GRF:
      return reg->hw_grf.size;
   case IBC_REG_FILE_LOGICAL:
      return reg->logical.num_comps *
             reg->logical.simd_width * reg->logical.stride;
   }
   unreachable("Unsupported register file");
}

static unsigned
reg_align(ibc_reg *reg)
{
   switch (reg->file) {
   case IBC_REG_FILE_HW_GRF:
      return reg->hw_grf.align;
   case IBC_REG_FILE_LOGICAL:
      return MIN2(reg->logical.simd_width * reg->logical.stride, 32);
   }
   unreachable("Unsupported register file");
}

static void
rewrite_reg_ref(ibc_reg_ref *ref, unsigned ref_simd_group,
                uint16_t *reg_byte, bool is_src)
{
   if (ref->reg == NULL || ref->reg->file == IBC_REG_FILE_HW_GRF)
      return;

   assert(ref->reg->file == IBC_REG_FILE_LOGICAL);

   ibc_logical_reg_ref logical = ref->logical;
   ref->file = IBC_REG_FILE_HW_GRF;

   unsigned stride = ref->reg->logical.stride;
   ref->hw_grf.byte = reg_byte[ref->reg->index];
   ref->hw_grf.byte += logical.comp * ref->reg->logical.simd_width * stride;
   if (logical.broadcast) {
      ref->hw_grf.byte += logical.simd_channel * stride;
      ref->hw_grf.vstride = 0;
      ref->hw_grf.width = 1;
      ref->hw_grf.hstride = 0;
   } else if (ref->reg->logical.simd_width == 1 && is_src) {
      ref->hw_grf.vstride = 0;
      ref->hw_grf.width = 1;
      ref->hw_grf.hstride = 0;
   } else {
      ref->hw_grf.byte +=
         (ref_simd_group - ref->reg->logical.simd_group) * stride;
      ref->hw_grf.hstride = stride;
      ref->hw_grf.width = 8;
      ref->hw_grf.vstride = stride * ref->hw_grf.width;
   }
   ref->hw_grf.byte += logical.byte;

   if (ref->write_instr) {
      list_del(&ref->write_link);
      ref->write_instr = NULL;
   }

   ref->reg = NULL;
}

void
ibc_assign_regs_trivial(ibc_shader *shader)
{
   void *dead_ctx = ralloc_context(shader);

   unsigned num_logical = 0;
   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_LOGICAL)
         reg->index = num_logical++;
   }

   uint16_t *reg_byte = ralloc_array(dead_ctx, uint16_t, num_logical);

   /* Assign a high register number to the final EOT send */
   {
      ibc_instr *last_instr = list_last_entry(&shader->instrs,
                                              ibc_instr, link);
      assert(ibc_instr_as_branch(last_instr)->op == IBC_BRANCH_OP_END);
      last_instr = LIST_ENTRY(ibc_instr, last_instr->link.prev, link);
      ibc_send_instr *last_send = ibc_instr_as_send(last_instr);

      assert(last_send->eot);
      /* TODO: Support SENDS */
      assert(last_send->payload[1].reg == NULL);
      assert(last_send->payload[0].reg->file == IBC_REG_FILE_HW_GRF);
      ibc_hw_grf_reg *grf = &last_send->payload[0].reg->hw_grf;

      /* Just place it as far up in the file as it will go */
      assert(grf->size % 32 == 0);
      last_send->payload[0].hw_grf.byte += (128 * 32) - grf->size;
      last_send->payload[0].reg = NULL;
   }

   unsigned byte = 128; /* Leave room for headers */

   /* Assign scalars first so they get packed */
   ibc_foreach_reg(reg, shader) {
      if (reg->file != IBC_REG_FILE_LOGICAL ||
          reg->logical.simd_width > 1)
         continue;

      unsigned size = reg_size(reg);
      unsigned align = reg_align(reg);

      reg_byte[reg->index] = ALIGN(byte, align);
      byte = reg_byte[reg->index] + size;
   }

   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_FLAG)
         continue;

      assert(reg->file == IBC_REG_FILE_LOGICAL ||
             reg->file == IBC_REG_FILE_HW_GRF);

      unsigned size = reg_size(reg);
      unsigned align = reg_align(reg);

      reg_byte[reg->index] = ALIGN(byte, align);
      byte = reg_byte[reg->index] + size;
   }

   ibc_foreach_instr(instr, shader) {
      /* Flags should already have been lowered */
      assert(instr->flag.file == IBC_REG_FILE_NONE ||
             instr->flag.file == IBC_REG_FILE_FLAG);

      /* Right now, only ALU instructions use logical regs */
      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU: {
         ibc_alu_instr *alu = ibc_instr_as_alu(instr);

         if (alu->dest.file == IBC_REG_FILE_LOGICAL) {
            rewrite_reg_ref(&alu->dest, alu->instr.simd_group,
                            reg_byte, false);
         }

         for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
            if (alu->src[i].ref.file == IBC_REG_FILE_LOGICAL) {
               rewrite_reg_ref(&alu->src[i].ref, alu->instr.simd_group,
                               reg_byte, true);
            }
         }
         continue;
      }

      case IBC_INSTR_TYPE_SEND: {
         ibc_send_instr *send = ibc_instr_as_send(instr);
         rewrite_reg_ref(&send->dest, send->instr.simd_group,
                         reg_byte, false);
         rewrite_reg_ref(&send->payload[0], send->instr.simd_group,
                         reg_byte, false);
         rewrite_reg_ref(&send->payload[1], send->instr.simd_group,
                         reg_byte, false);
         rewrite_reg_ref(&send->desc, send->instr.simd_group,
                         reg_byte, false);
         rewrite_reg_ref(&send->ex_desc, send->instr.simd_group,
                         reg_byte, false);
         continue;
      }

      case IBC_INSTR_TYPE_INTRINSIC:
         unreachable("These should no longer exist");

      case IBC_INSTR_TYPE_MERGE:
      case IBC_INSTR_TYPE_BRANCH:
         /* Nothing interesting to do here */
         continue;
      }
      unreachable("Invalid instruction type");
   }

   ralloc_free(dead_ctx);
}
