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

#include "brw_compiler.h"

#include "util/rb_tree.h"
#include "util/register_allocate.h"
#include "util/vma.h"

static bool
ibc_alu_instr_is_raw_mov(const ibc_alu_instr *alu)
{
   return alu->op == IBC_ALU_OP_MOV &&
          alu->dest.type == alu->src[0].ref.type &&
          alu->src[0].mod == IBC_ALU_SRC_MOD_NONE &&
          !alu->saturate;
}

static void
ref_set_reg_packed(ibc_ref ref)
{
   if (ref.file == IBC_FILE_HW_GRF) {
      assert(ref.hw_grf.hstride == ibc_type_byte_size(ref.type));
      assert(ref.hw_grf.vstride == ref.hw_grf.hstride * ref.hw_grf.width);
      return;
   }

   assert(ref.file == IBC_FILE_LOGICAL);

   ibc_reg *reg = (ibc_reg *)ref.reg;
   if (reg->logical.packed)
      return;

   if (reg->logical.stride == 0)
      reg->logical.stride = ibc_type_byte_size(ref.type);
   assert(reg->logical.stride == ibc_type_byte_size(ref.type));
   reg->logical.packed = true;
}

bool
ibc_assign_logical_reg_strides(ibc_shader *shader)
{
   bool progress = false;

   ibc_foreach_instr(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_SEND)
         continue;

      ibc_send_instr *send = ibc_instr_as_send(instr);

      if (send->rlen > 0)
         ref_set_reg_packed(send->dest);

      if (send->mlen > 0)
         ref_set_reg_packed(send->payload[0]);

      if (send->ex_mlen > 0)
         ref_set_reg_packed(send->payload[1]);
   }

   ibc_foreach_reg(reg, shader) {
      if (reg->file != IBC_FILE_LOGICAL)
         continue;

      if (reg->logical.stride > 0)
         continue;

      /* Scalars and booleans won't be mapped to HW regs so they don't need to
       * have assigned strides.
       */
      if (reg->logical.bit_size == 1)
         continue;

      /* Uniform values don't have a stride across SIMD channels */
      if (reg->logical.simd_width == 1)
         continue;

      /* At the very least, we want it to be the size of the register */
      assert(reg->logical.bit_size >= 8);
      unsigned stride = reg->logical.bit_size / 8;

      ibc_reg_foreach_write(write, reg) {
         if (write->instr->type != IBC_INSTR_TYPE_ALU)
            continue;

         ibc_alu_instr *alu = ibc_instr_as_alu(write->instr);

         /* This can't be a flag write */
         assert(write == &alu->dest_write);

         for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
            if (alu->src[i].ref.type == IBC_TYPE_FLAG)
               stride = MAX2(stride, ibc_type_byte_size(IBC_TYPE_W));
            else
               stride = MAX2(stride, ibc_type_byte_size(alu->src[i].ref.type));
         }

         /* Only raw MOV supports a packed-byte destination */
         if (stride == 1 && !ibc_alu_instr_is_raw_mov(alu))
            stride = 2;
      }

      reg->logical.stride = stride;
      progress = true;
   }

   return progress;
}

struct ibc_phys_reg {
   /** Byte offset into the 4K register file */
   uint16_t byte;

   /** Size (in bytes) of this register */
   uint16_t size;

   /** End (exclusive) of the live range of this register */
   uint32_t reg_live_end;

   /** Node in ibc_phys_reg_alloc::regs */
   struct rb_node node;

   /** True if this this register has per-byte live end data */
   bool has_byte_live_end;

   /** First per-byte live_end for a live byte
    *
    * This must equal reg_live_end if has_byte_live_end is false.  For any
    * given byte b, it's still allocated if and only if
    *
    *       byte_live_end[b] >= first_live_byte_end.
    */
   uint32_t first_live_byte_end;

   /** Per-byte live end of the given byte is already free */
   uint32_t byte_live_end[0];
};

static int
ibc_phys_reg_rb_cmp(const struct rb_node *an, const struct rb_node *bn)
{
   const struct ibc_phys_reg *a, *b;
   a = rb_node_data(struct ibc_phys_reg, an, node);
   b = rb_node_data(struct ibc_phys_reg, bn, node);

   if (!a->has_byte_live_end)
      assert(a->first_live_byte_end == a->reg_live_end);
   if (!b->has_byte_live_end)
      assert(b->first_live_byte_end == b->reg_live_end);

   if (a->first_live_byte_end < b->first_live_byte_end)
      return -1;
   if (a->first_live_byte_end > b->first_live_byte_end)
      return 1;
   return 0;
}

struct ibc_phys_reg_alloc {
   struct util_vma_heap heap;

   void *mem_ctx;

   /** Red-black tree of ibc_phys_reg
    *
    * This tree is sorted by physical register end so that we can easily
    * return physical registers to the physical allocator.
    */
   struct rb_tree regs;
};

static void
ibc_phys_reg_alloc_init(struct ibc_phys_reg_alloc *alloc, void *mem_ctx)
{
   util_vma_heap_init(&alloc->heap, 4096, 4096);
   alloc->heap.alloc_high = false;
   alloc->mem_ctx = mem_ctx;
   rb_tree_init(&alloc->regs);
}

static void
ibc_phys_reg_alloc_finish(struct ibc_phys_reg_alloc *alloc)
{
   util_vma_heap_finish(&alloc->heap);
}

static inline void
ibc_phys_reg_alloc_print(FILE *fp, struct ibc_phys_reg_alloc *alloc)
{
   fprintf(fp, "ibc_phys_reg_alloc:\n");
   util_vma_heap_print(&alloc->heap, fp, "    ", 4096);
}

static bool
ibc_phys_reg_alloc_raw(struct ibc_phys_reg_alloc *alloc,
                       int16_t fixed_hw_grf_byte,
                       uint16_t size, uint16_t align,
                       uint32_t reg_end, uint32_t *per_byte_end,
                       struct ibc_phys_reg *reg_out)
{
   uint16_t byte;
   if (fixed_hw_grf_byte < 0) {
      uint64_t addr = util_vma_heap_alloc(&alloc->heap, size, align);
      if (addr == 0)
         return false;
      assert(addr >= 4096 && addr + size <= 8192);
      byte = addr - 4096;
   } else {
      assert(fixed_hw_grf_byte + size <= 4096);
      uint64_t addr = fixed_hw_grf_byte + 4096;
      if (!util_vma_heap_alloc_addr(&alloc->heap, addr, size))
         return false;
      byte = fixed_hw_grf_byte;
   }

   reg_out->byte = byte;
   reg_out->size = size;
   reg_out->reg_live_end = reg_end;
   if (per_byte_end) {
      reg_out->has_byte_live_end = true;
      reg_out->first_live_byte_end = UINT32_MAX;

      /* The one caller that gets here just passes in our byte_live_end so we
       * don't have to do anything to fill it out.
       */
      assert(per_byte_end == reg_out->byte_live_end);
      for (unsigned b = 0; b < size; b++) {
         reg_out->first_live_byte_end = MIN2(reg_out->first_live_byte_end,
                                             reg_out->byte_live_end[b]);
      }
   } else {
      reg_out->has_byte_live_end = false;
      reg_out->first_live_byte_end = reg_end;
   }
   rb_tree_insert(&alloc->regs, &reg_out->node, ibc_phys_reg_rb_cmp);

   return true;
}

static void
ibc_phys_reg_extend_live_range(struct ibc_phys_reg_alloc *alloc,
                               struct ibc_phys_reg *reg,
                               uint32_t new_end)
{
   assert(!reg->has_byte_live_end);
   if (new_end <= reg->reg_live_end)
      return;

   reg->reg_live_end = new_end;
   reg->first_live_byte_end = new_end;
   rb_tree_remove(&alloc->regs, &reg->node);
   rb_tree_insert(&alloc->regs, &reg->node, ibc_phys_reg_rb_cmp);
}

static bool
ibc_phys_reg_free_bytes(struct ibc_phys_reg_alloc *alloc,
                        struct ibc_phys_reg *reg,
                        uint32_t ip)
{
   if (reg->first_live_byte_end > ip)
      return false;

   if (!reg->has_byte_live_end) {
      /* If it doesn't have per-byte liveness and we got here, it's dead. */
      util_vma_heap_free(&alloc->heap, reg->byte + 4096, reg->size);
      rb_tree_remove(&alloc->regs, &reg->node);
      return true;
   }

   int first_dead_byte = -1;
   uint32_t first_end_gt_ip = UINT32_MAX;
   for (unsigned b = 0 ; b < reg->size; b++) {
      if (reg->byte_live_end[b] > ip) {
         assert(reg->byte_live_end[b] < UINT32_MAX);
         first_end_gt_ip = MIN2(first_end_gt_ip, reg->byte_live_end[b]);
      }

      /* If byte_live_end[b] < first_live_byte_end then the register has
       * already been freed and if byte_live_end[b] then the register is
       * still live.  If either of these is true, this byte is not free so
       * we should free everything before it.
       */
      if (reg->byte_live_end[b] < reg->first_live_byte_end ||
          reg->byte_live_end[b] > ip) {
         /* If first_dead is non-negative, we have something to free */
         if (first_dead_byte >= 0) {
            assert(b > first_dead_byte);
            util_vma_heap_free(&alloc->heap,
                               reg->byte + first_dead_byte + 4096,
                               b - first_dead_byte);
            first_dead_byte = -1;
         }
      } else {
         if (first_dead_byte < 0)
            first_dead_byte = b;
      }
   }

   if (first_dead_byte >= 0) {
      assert(reg->size > first_dead_byte);
      util_vma_heap_free(&alloc->heap,
                         reg->byte + first_dead_byte + 4096,
                         reg->size - first_dead_byte);
   }

   if (first_end_gt_ip == UINT32_MAX) {
      /* Nothing is still live.  Delete it */
      rb_tree_remove(&alloc->regs, &reg->node);
   } else {
      /* Something is still live, adjust and re-sort */
      assert(first_end_gt_ip > reg->first_live_byte_end);
      reg->first_live_byte_end = first_end_gt_ip;
      rb_tree_remove(&alloc->regs, &reg->node);
      rb_tree_insert(&alloc->regs, &reg->node, ibc_phys_reg_rb_cmp);
   }

   return true;
}

/** Frees all registers that end at or before the given ip */
static void
ibc_phys_reg_alloc_free_regs(struct ibc_phys_reg_alloc *alloc,
                             uint32_t ip)
{
   while (!rb_tree_is_empty(&alloc->regs)) {
      struct rb_node *n = rb_tree_first(&alloc->regs);
      struct ibc_phys_reg *reg = rb_node_data(struct ibc_phys_reg, n, node);

      /* Registers in the red-black tree are sorted by first live byte end.
       * The moment one of them fails to free, they all will.
       */
      if (!ibc_phys_reg_free_bytes(alloc, reg, ip))
         break;
   }
}

static struct ibc_phys_reg *
ibc_phys_reg_alloc(struct ibc_phys_reg_alloc *alloc,
                   int16_t fixed_hw_grf_byte,
                   uint16_t size, uint16_t align,
                   const ibc_reg_live_intervals *rli)
{
   assert(size == rli->num_chunks * rli->chunk_byte_size);

   const size_t struct_size = sizeof(struct ibc_phys_reg) +
                              sizeof(uint32_t) * size;
   struct ibc_phys_reg *reg = rzalloc_size(alloc->mem_ctx, struct_size);

   for (unsigned c = 0; c < rli->num_chunks; c++) {
      for (unsigned b = 0; b < rli->chunk_byte_size; b++) {
         reg->byte_live_end[c * rli->chunk_byte_size + b] =
            rli->chunks[c].physical_end;
      }
   }

   bool success = ibc_phys_reg_alloc_raw(alloc, fixed_hw_grf_byte, size, align,
                                         rli->physical_end, reg->byte_live_end,
                                         reg);
   if (!success) {
      ralloc_free(reg);
      return NULL;
   }

   /* Immediately clean up any 100% dead bytes */
   ibc_phys_reg_free_bytes(alloc, reg, 0);

   return reg;
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
         if (chunk_live == NULL) {
            /* We should only get here if this chunk of the register is never
             * written.
             */
            reg->chunks[c].hole_start = 0;
            reg->chunks[c].hole_end = UINT32_MAX;
            goto found_hole;
         }

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
                       uint32_t ip, uint8_t byte_size,
                       uint8_t num_bytes, uint8_t num_comps,
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
         for (unsigned b = 0; b < num_bytes; b++) {
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
                  if (byte_live == NULL)
                     continue;

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
            if (chunk->live_range && byte_live) {
               chunk->live_range =
                  interval_set_from_union(reg, chunk->live_range, byte_live);
            } else if (byte_live) {
               chunk->live_range = byte_live;
            }
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

   list_inithead(&alloc->free);
   list_inithead(&alloc->busy);
}

static void
ibc_strided_reg_alloc_finish(struct ibc_strided_reg_alloc *alloc)
{
}

static inline void
ibc_strided_reg_print(FILE *fp, struct ibc_strided_reg *sreg, uint32_t ip)
{
   fprintf(fp, "(%u B, %u comps, %u:%u): ",
           sreg->byte_size, sreg->num_comps,
           sreg->simd_group, sreg->simd_width);

   unsigned num_chunks = ibc_strided_reg_num_chunks(sreg);
   unsigned chunks_free = 0;
   for (unsigned i = 0; i < num_chunks; i++) {
      if (sreg->chunks[i].hole_start > ip ||
          sreg->chunks[i].hole_end <= ip)
         chunks_free++;
   }

   fprintf(fp, "%.2f%% used", ((float)chunks_free / (float)num_chunks) * 100);
}

static inline void
ibc_strided_reg_alloc_print(FILE *fp, struct ibc_strided_reg_alloc *alloc,
                            uint32_t ip)
{
   fprintf(fp, "ibc_strided_reg_alloc(stride = %u):\n", alloc->stride);

   fprintf(fp, "    Free registers:\n");
   if (list_empty(&alloc->free))
      fprintf(fp, "        None\n");
   uint32_t total_bytes = 0;
   list_for_each_entry(struct ibc_strided_reg, sreg, &alloc->free, link) {
      fprintf(fp, "        ");
      ibc_strided_reg_print(fp, sreg, ip);
      fprintf(fp, "\n");
      total_bytes += (uint32_t)sreg->byte_size *
                     (uint32_t)sreg->num_comps *
                     (uint32_t)sreg->simd_width;
   }
   fprintf(fp, "    %u B total in free registers\n", total_bytes);
   fprintf(fp, "\n");

   fprintf(fp, "    Busy registers:\n");
   if (list_empty(&alloc->busy))
      fprintf(fp, "        None\n");
   total_bytes = 0;
   list_for_each_entry(struct ibc_strided_reg, sreg, &alloc->busy, link) {
      fprintf(fp, "        ");
      ibc_strided_reg_print(fp, sreg, ip);
      fprintf(fp, "\n");
      total_bytes += (uint32_t)sreg->byte_size *
                     (uint32_t)sreg->num_comps *
                     (uint32_t)sreg->simd_width;
   }
   fprintf(fp, "    %u B total in busy registers\n", total_bytes);
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

   assert(reg->file == IBC_FILE_LOGICAL);
   const ibc_logical_reg *lreg = &reg->logical;
   assert(lreg->bit_size % 8 == 0);
   unsigned byte_size = lreg->bit_size / 8;
   unsigned num_simd_groups =
      DIV_ROUND_UP(lreg->simd_width, STRIDED_REG_SIMD_GRANULARITY);

   assert(byte_size <= 8);
   assert(num_simd_groups <= 4);
   assert(num_simd_groups * lreg->num_comps <= 32);
   const uint8_t simd_stride = byte_size;
   const uint8_t comp_stride = simd_stride * num_simd_groups;
   struct interval_set *live_ranges[8 * 32];
#ifndef NDEBUG
   memset(live_ranges, 137, sizeof(live_ranges));
#endif

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

            const ibc_ref ref = {
               .file = IBC_FILE_LOGICAL,
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
            ibc_live_intervals_ref_chunks(live, &ref, -1, 1,
                                              ref_simd_group, ref_simd_width,
                                              chunks);

            struct interval_set *byte_live = NULL;
            for (unsigned w = 0; w < chunks_words; w++) {
               BITSET_WORD mask = chunks[w];
               while (mask) {
                  int b = u_bit_scan(&mask);
                  assert(b >= 0 && b < BITSET_WORDBITS);
                  ibc_reg_chunk_live_interval *chunk =
                     &rli->chunks[w * BITSET_WORDBITS + b];

                  if (chunk->logical && byte_live) {
                     byte_live = interval_set_from_union(alloc->mem_ctx,
                                                         chunk->logical,
                                                         byte_live);
                  } else if (chunk->logical) {
                     byte_live = chunk->logical;
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
         if (sreg->phys.reg_live_end <= ip) {
            list_del(&sreg->link);
            continue;
         }

         /* If we have a packed vector, then it has to have exactly the same
          * SIMD group as the strided reg from which it's being allocated.
          */
         if (lreg->packed && lreg->num_comps > 1 &&
             (lreg->simd_group != sreg->simd_group ||
              lreg->simd_width != sreg->simd_width))
            continue;

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
                             alloc->stride, byte_size, lreg->num_comps,
                             lreg->simd_group, lreg->simd_width,
                             live_ranges, simd_stride, comp_stride);

   uint16_t size = alloc->stride * lreg->simd_width * lreg->num_comps;
   uint16_t align = MIN2(alloc->stride * lreg->simd_width, 32);
   if (!ibc_phys_reg_alloc_raw(alloc->phys_alloc,
                               fixed_hw_grf_byte, size, align,
                               rli->physical_end, NULL, &sreg->phys)) {
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
      if (reg->phys.reg_live_end <= ip) {
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

   if (a->physical_start < b->physical_start)
      return -1;
   if (a->physical_start > b->physical_start)
      return 1;
   return 0;
}

struct ibc_assign_regs_state {
   void *mem_ctx;

   bool allow_spilling;

   const ibc_live_intervals *live;

   /** Current instruction */
   ibc_instr *instr;

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
   case IBC_FILE_HW_GRF:
      return reg != NULL;
   case IBC_FILE_LOGICAL:
      return true;
   default:
      return false;
   }
}

static bool
rewrite_ref_and_update_reg(ibc_ref *_ref,
                           int num_bytes, int num_comps,
                           uint8_t simd_group, uint8_t simd_width,
                           void *_state)
{
   const ibc_ref *ref = _ref;
   struct ibc_assign_regs_state *state = _state;
   if (ref->file != IBC_FILE_LOGICAL &&
       ref->file != IBC_FILE_HW_GRF)
      return true;

   const ibc_reg *reg = ref->reg;
   assert(reg->index < state->live->num_regs);
   struct ibc_reg_assignment *assign = &state->assign[reg->index];

   ibc_ref new_ref = {
      .file = IBC_FILE_HW_GRF,
      .type = ref->type,
   };
   switch (ref->file) {
   case IBC_FILE_HW_GRF:
      new_ref.hw_grf = ref->hw_grf;
      new_ref.hw_grf.byte += assign->preg->byte;
      break;

   case IBC_FILE_LOGICAL: {
      assert(reg->logical.bit_size % 8 == 0);

      /* Stash this so we can access it unchanged */
      unsigned stride;
      if (assign->sreg) {
         assert(reg->logical.simd_width > 1);
         assert(reg->logical.stride >= reg->logical.bit_size / 8);
         assert(reg->logical.stride == assign->sreg->byte_size);
         ibc_strided_reg_update_holes(assign->sreg, state->ip);

         new_ref.hw_grf.byte = assign->sreg->phys.byte;
         new_ref.hw_grf.byte += (assign->sreg_comp + ref->logical.comp) *
                                 assign->sreg->simd_width *
                                 assign->sreg->byte_size;
         new_ref.hw_grf.byte += assign->sreg_byte + ref->logical.byte;
         stride = assign->sreg->byte_size;
      } else {
         assert(reg->logical.simd_width == 1);
         assert(reg->logical.stride == 0);
         new_ref.hw_grf.byte = assign->preg->byte;
         new_ref.hw_grf.byte += ref->logical.comp *
                                (reg->logical.bit_size / 8);
         new_ref.hw_grf.byte += ref->logical.byte;
         /* This is needed for the !state->is_read case below */
         stride = reg->logical.bit_size / 8;
      }

      if (ref->logical.broadcast) {
         new_ref.hw_grf.byte += ref->logical.simd_channel * stride;
      } else if (reg->logical.simd_width > 1) {
         new_ref.hw_grf.byte +=
            (simd_group - assign->sreg->simd_group) * stride;
      }

      if (ref->logical.broadcast ||
          (reg->logical.simd_width == 1 && state->is_read)) {
         new_ref.hw_grf.vstride = 0;
         new_ref.hw_grf.width = 1;
         new_ref.hw_grf.hstride = 0;
      } else {
         new_ref.hw_grf.hstride = stride;
         new_ref.hw_grf.width = 8;
         new_ref.hw_grf.vstride = stride * new_ref.hw_grf.width;
      }
      break;
   }

   default:
      unreachable("Unhandled register file");
   }

   ibc_instr_set_ref(state->instr, _ref, new_ref);

   return true;
}

static bool
assign_reg(struct ibc_reg_assignment *assign,
           int16_t fixed_hw_grf_byte,
           struct ibc_assign_regs_state *state)
{
   ibc_reg *reg = assign->reg;
   const ibc_reg_live_intervals *rli = &state->live->regs[reg->index];

   switch (assign->reg->file) {
   case IBC_FILE_HW_GRF:
      assign->preg = ibc_phys_reg_alloc(&state->phys_alloc,
                                        fixed_hw_grf_byte,
                                        reg->hw_grf.size,
                                        reg->hw_grf.align,
                                        rli);
      if (!assign->preg && !state->allow_spilling)
         return false;
#ifndef NDEBUG
      if (!assign->preg) {
         ibc_phys_reg_alloc_print(stderr, &state->phys_alloc);
         fprintf(stderr, "\n");
      }
      assert(assign->preg);
#endif
      break;

   case IBC_FILE_LOGICAL:
      if (reg->logical.simd_width == 1) {
         uint16_t size = (reg->logical.bit_size / 8) *
                         reg->logical.num_comps;
         uint16_t align = reg->logical.bit_size / 8;
         assign->preg = ibc_phys_reg_alloc(&state->phys_alloc,
                                           fixed_hw_grf_byte,
                                           size, align, rli);
         if (!assign->preg && !state->allow_spilling)
            return false;
#ifndef NDEBUG
         if (!assign->preg) {
            ibc_phys_reg_alloc_print(stderr, &state->phys_alloc);
            fprintf(stderr, "\n");
         }
         assert(assign->preg);
#endif
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
            &state->strided_alloc[ffs(reg->logical.stride) - 1];

         assign->sreg = ibc_strided_reg_alloc(strided_alloc,
                                              state->live, reg,
                                              fixed_hw_grf_byte,
                                              &assign->sreg_byte,
                                              &assign->sreg_comp);
         if (!assign->sreg && !state->allow_spilling)
            return false;
#ifndef NDEBUG
         if (!assign->sreg) {
            ibc_phys_reg_alloc_print(stderr, &state->phys_alloc);
            fprintf(stderr, "\n");
            for (unsigned i = 0; i < ARRAY_SIZE(state->strided_alloc); i++) {
               ibc_strided_reg_alloc_print(stderr, &state->strided_alloc[i],
                                           rli->physical_start);
               fprintf(stderr, "\n");
            }
         }
         assert(assign->sreg);
#endif
      }
      break;

   default:
      unreachable("Unhandled register file");
   }

   rb_tree_remove(&state->alloc_order, &assign->node);
   return true;
}

static bool
live_reg_filter_cb(const ibc_reg *reg)
{
   return reg->file == IBC_FILE_LOGICAL ||
          reg->file == IBC_FILE_HW_GRF;
}

static void
rewrite_instr_reg_reads(ibc_instr *instr, struct ibc_assign_regs_state *state)
{
   state->is_read = true;
   ibc_instr_foreach_read(instr, rewrite_ref_and_update_reg, state);
}

static void
rewrite_instr_reg_writes(ibc_instr *instr, struct ibc_assign_regs_state *state)
{
   state->is_read = false;
   ibc_instr_foreach_write(instr, rewrite_ref_and_update_reg, state);
}

static bool
ibc_assign_regs_linear_scan(ibc_shader *shader, bool allow_spilling)
{
   struct ibc_builder b;
   ibc_builder_init(&b, shader);

   struct ibc_assign_regs_state state = {
      .mem_ctx = ralloc_context(NULL),
      .allow_spilling = allow_spilling,
   };

   ibc_assign_logical_reg_strides(shader);

   state.live = ibc_compute_live_intervals(shader, live_reg_filter_cb,
                                           state.mem_ctx);

   ibc_phys_reg_alloc_init(&state.phys_alloc, state.mem_ctx);
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

   bool allocated = false;

   ibc_foreach_instr_reverse(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_SEND)
         continue;

      ibc_send_instr *send = ibc_instr_as_send(instr);
      assert(send->eot);

      /* Sends with EOT must use high registers (g112..g127) */
      unsigned src1 = BRW_MAX_GRF * REG_SIZE - send->ex_mlen * REG_SIZE;
      unsigned src0 = src1 - send->mlen * REG_SIZE;

      if (send->ex_mlen > 0) {
         assert(send->payload[1].file == IBC_FILE_LOGICAL ||
                send->payload[1].file == IBC_FILE_HW_GRF);
         if (!assign_reg(&state.assign[send->payload[1].reg->index],
                         src1, &state))
            goto fail;
      }

      assert(send->payload[0].file == IBC_FILE_LOGICAL ||
             send->payload[0].file == IBC_FILE_HW_GRF);
      if (!assign_reg(&state.assign[send->payload[0].reg->index],
                      src0, &state))
         goto fail;
      break;
   }

   ibc_foreach_instr_safe(instr, shader) {
      state.instr = instr;
      state.ip = instr->index;

      ibc_phys_reg_alloc_free_regs(&state.phys_alloc, state.ip);

      if (instr->type == IBC_INSTR_TYPE_SEND) {
         /* We can't validate this in ibc_validate because .packed gets set as
          * a late-binding thing.  However, before we RA, we should assert
          * that things are packed.
          */
         ibc_send_instr *send = ibc_instr_as_send(instr);
         if (send->rlen > 0 && send->dest.file == IBC_FILE_LOGICAL)
            assert(send->dest.reg->logical.packed);
         if (send->payload[0].file == IBC_FILE_LOGICAL)
            assert(send->payload[0].reg->logical.packed);
         if (send->ex_mlen > 0 && send->payload[1].file == IBC_FILE_LOGICAL)
            assert(send->payload[1].reg->logical.packed);
      }

      if (instr->type == IBC_INSTR_TYPE_INTRINSIC) {
         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
         if (intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD) {
            assert(intrin->dest.reg);
            assert(ibc_reg_ssa_instr(intrin->dest.reg) == instr);
            assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
            assert(intrin->src[0].ref.reg == NULL);

            struct ibc_reg_assignment *assign =
               &state.assign[intrin->dest.reg->index];

            if (assign->preg || assign->sreg) {
               rewrite_instr_reg_writes(instr, &state);

               /* Some constraint (such as high GRFs for sends with EOT)
                * has already caused our load_payload destination to be
                * allocated.  So we can't just allocate our destination
                * to the payload register.  Emit a MOV to copy it.
                */
               b.cursor = ibc_before_instr(instr);

               ibc_MOV_raw_vec_to(&b, intrin->dest, intrin->src[0].ref,
                                  intrin->num_dest_comps);

            } else {
               if (!assign_reg(assign, intrin->src[0].ref.hw_grf.byte, &state))
                  goto fail;
            }

            ibc_instr_remove(instr);
            continue;
         } else if (intrin->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO &&
                    intrin->src[0].ref.file != IBC_FILE_NONE) {
            /* This is a LOAD_UBO that actually just represents a push
             * constant block.  Assign it the right GRF.
             */
            assert(intrin->dest.reg);
            assert(ibc_reg_ssa_instr(intrin->dest.reg) == instr);
            assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
            assert(intrin->src[0].ref.reg == NULL);

            UNUSED struct ibc_reg_assignment *assign =
               &state.assign[intrin->dest.reg->index];
            assert(assign->preg == NULL && assign->sreg == NULL);

            if (!assign_reg(assign, intrin->src[0].ref.hw_grf.byte, &state))
               goto fail;
            ibc_instr_remove(instr);
            continue;
         } else if (intrin->op == IBC_INTRINSIC_OP_UNDEF) {
            ibc_instr_remove(instr);
            continue;
         }
      }

      while (!rb_tree_is_empty(&state.alloc_order)) {
         struct rb_node *assign_node = rb_tree_first(&state.alloc_order);
         struct ibc_reg_assignment *assign =
            rb_node_data(struct ibc_reg_assignment, assign_node, node);

         if (assign->physical_start > state.ip)
            break;

         if (!assign_reg(assign, -1, &state))
            goto fail;
      }

      rewrite_instr_reg_reads(instr, &state);
      rewrite_instr_reg_writes(instr, &state);

      if (instr->type == IBC_INSTR_TYPE_FLOW) {
         /* The flow instruction doesn't actually have any non-flag sources
          * but it's none-the-less a transition point for register live ranges
          * and we need to update strided register holes.
          */
         for (unsigned i = 0; i < ARRAY_SIZE(state.strided_alloc); i++) {
            ibc_strided_reg_alloc_update_reg_holes(&state.strided_alloc[i],
                                                   state.ip);
         }
      }
   }

   ibc_foreach_reg_safe(reg, shader) {
      if (should_assign_reg(reg))
         list_del(&reg->link);
   }

   /* If we got here, we were successful at allocating */
   allocated = true;

fail:

   ibc_phys_reg_alloc_finish(&state.phys_alloc);
   for (unsigned i = 0; i < ARRAY_SIZE(state.strided_alloc); i++)
      ibc_strided_reg_alloc_finish(&state.strided_alloc[i]);

   ralloc_free(state.mem_ctx);

   return allocated;
}

#define MAX_HW_GRF_SIZE (16 * 32)
#define TOTAL_GRF_BYTES 4096

struct ibc_reg_class {
   uint8_t nr;
   union {
      uint8_t reg_align;
      uint8_t grf_stride;
   };
   uint16_t reg_size;
   int reg_start;
   int reg_end;
};

struct ibc_reg_set {
   struct ra_regs *regs;

   unsigned simd_width;

   /** Classes for sub-32-byte physical registers */
   struct ibc_reg_class phys_small[5];

   /** Classes for 32-byte and larger physical registers */
   struct ibc_reg_class phys_large[MAX_HW_GRF_SIZE / 32];

   /** Classes for strided registers
    *
    * Strided registers are handled as a bit of a special case.  We only
    * bother for logical registers whose SIMD width is equal to the dispatch
    * width of the shader and only when the whole register SIMD width fits
    * inside of two registers.  We do this for up to a vec4 size register.
    */
   struct ibc_reg_class strided[4][4];

   uint16_t *ra_reg_to_grf;
};

static const struct ibc_reg_class *
ibc_reg_set_get_physical_class(const struct ibc_reg_set *set, uint16_t size)
{
   if (size < 32) {
      /* This is a scalar */
      int idx = ffs(size) - 1;
      assert(idx >= 0 && idx < ARRAY_SIZE(set->phys_small));
      return &set->phys_small[idx];
   } else {
      assert(size < MAX_HW_GRF_SIZE);
      return &set->phys_large[(size / 32) - 1];
   }
}

static struct ibc_reg_class *
ibc_reg_set_get_strided_class_mut(struct ibc_reg_set *set,
                                  unsigned stride, unsigned num_comps,
                                  UNUSED unsigned simd_width)
{
   assert(simd_width == set->simd_width);
   assert(stride * simd_width <= 64);

   int stride_idx = ffs(stride) - 1;
   assert(stride_idx >= 0 && stride_idx < ARRAY_SIZE(set->strided));
   assert(num_comps > 0 && num_comps <= ARRAY_SIZE(set->strided[0]));
   return &set->strided[stride_idx][num_comps - 1];
}

static const struct ibc_reg_class *
ibc_reg_set_get_strided_class(const struct ibc_reg_set *set,
                              unsigned stride, unsigned num_comps,
                              unsigned simd_width)
{
   return ibc_reg_set_get_strided_class_mut((struct ibc_reg_set *)set,
                                            stride, num_comps, simd_width);
}

static int
ibc_reg_set_grf_to_reg(const struct ibc_reg_set *set,
                       uint16_t grf_byte, uint16_t size)
{
   const struct ibc_reg_class *c = ibc_reg_set_get_physical_class(set, size);
   unsigned grf_stride = c->reg_align;
   assert(grf_byte % grf_stride == 0);
   return c->reg_start + (grf_byte / grf_stride);
}

static void
ibc_reg_set_init(struct ibc_reg_set *set, unsigned simd_width,
                 void *mem_ctx)
{
   set->simd_width = simd_width;

   unsigned class_count = 0;
   struct ibc_reg_class *classes[37];

   unsigned ra_reg_count = 0;

   /* Start with scalars.  This way 1B regs and HW regs line up. */
   for (unsigned i = 0; i < ARRAY_SIZE(set->phys_small); i++) {
      set->phys_small[i].reg_align = (1 << i);
      set->phys_small[i].reg_size = (1 << i);
      set->phys_small[i].reg_start = ra_reg_count;
      ra_reg_count += TOTAL_GRF_BYTES >> i;
      set->phys_small[i].reg_end = ra_reg_count;

      classes[class_count++] = &set->phys_small[i];
   }

   for (unsigned i = 0; i < ARRAY_SIZE(set->phys_large); i++) {
      set->phys_large[i].reg_align = 32;
      set->phys_large[i].reg_size = (i + 1) * 32;
      set->phys_large[i].reg_start = ra_reg_count;
      ra_reg_count += (TOTAL_GRF_BYTES / 32) - i;
      set->phys_large[i].reg_end = ra_reg_count;

      classes[class_count++] = &set->phys_large[i];
   }

   for (unsigned stride = 1; stride <= 8; stride *= 2) {
      if (stride * simd_width > 64)
         continue;

      for (unsigned num_comps = 1; num_comps <= 4; num_comps++) {
         struct ibc_reg_class *class =
            ibc_reg_set_get_strided_class_mut(set, stride, num_comps,
                                              simd_width);
         class->reg_align = stride * simd_width;
         class->reg_size = stride * simd_width * num_comps;
         class->reg_start = ra_reg_count;
         ra_reg_count +=
            (TOTAL_GRF_BYTES / (stride * simd_width)) - (num_comps - 1);
         class->reg_end = ra_reg_count;

         classes[class_count++] = class;
      }
   }
   assert(class_count <= ARRAY_SIZE(classes));

   set->ra_reg_to_grf = ralloc_array(mem_ctx, uint16_t, ra_reg_count);
   uint16_t *ra_reg_to_grf_end = ralloc_array(mem_ctx, uint16_t, ra_reg_count);
   set->regs = ra_alloc_reg_set(mem_ctx, ra_reg_count, false);
   ra_set_allocate_round_robin(set->regs);

   for (unsigned i = 0; i < class_count; i++) {
      classes[i]->nr = ra_alloc_reg_class(set->regs);
      assert(classes[i]->nr == i);
   }

   int reg = 0;
   for (unsigned i = 0; i < class_count; i++) {
      assert(classes[i]->nr == i);
      assert(classes[i]->reg_start == reg);

      /* Assume that every class strides by its alignment */
      const unsigned grf_stride = classes[i]->grf_stride;
      const unsigned reg_size = classes[i]->reg_size;
      assert(reg_size < TOTAL_GRF_BYTES);
      for (unsigned grf = 0; grf <= TOTAL_GRF_BYTES - reg_size;
           grf += grf_stride) {
         ra_class_add_reg(set->regs, classes[i]->nr, reg);
         set->ra_reg_to_grf[reg] = grf;
         ra_reg_to_grf_end[reg] = grf + classes[i]->reg_size;
         reg++;
      }
      assert(classes[i]->reg_end == reg);
   }
   assert(reg == ra_reg_count);

   /* From register_allocate.c:
    *
    * q(B,C) (indexed by C, B is this register class) in Runeson/Nyström
    * paper.  This is "how many registers of B could the worst choice
    * register from C conflict with".
    *
    * If we just let the register allocation algorithm compute these values,
    * it's extremely expensive.  However, since all of our registers are laid
    * out fairly explicitly, we can very easily compute them ourselves.
    */
   unsigned **q_values = ralloc_array(mem_ctx, unsigned *, class_count);
   for (unsigned c = 0; c < class_count; c++)
      q_values[c] = ralloc_array(q_values, unsigned, class_count);

   for (unsigned b = 0; b < class_count; b++) {
      for (unsigned c = 0; c < class_count; c++) {
         assert(util_is_power_of_two_nonzero(classes[b]->reg_align));
         assert(util_is_power_of_two_nonzero(classes[c]->reg_align));

         /* Save these off for later */
         const unsigned b_start = classes[b]->reg_start;
         const unsigned c_start = classes[c]->reg_start;
         const unsigned b_num_regs = classes[b]->reg_end -
                                     classes[b]->reg_start;
         const unsigned c_num_regs = classes[c]->reg_end -
                                     classes[c]->reg_start;

         if (classes[c]->reg_align == classes[c]->reg_size &&
             classes[b]->reg_align == classes[b]->reg_size &&
             classes[b]->reg_size >= classes[c]->reg_size) {
            /* If B's and C's registers are both aligned to a register and C's
             * fit inside B's size and alignment then one register in C always
             * maps to one register in B.
             */
            q_values[b][c] = 1;

            const unsigned c_per_b = classes[b]->reg_size /
                                     classes[c]->reg_size;
            assert(c_num_regs == b_num_regs * c_per_b);
            for (unsigned i = 0; i < c_num_regs; i++) {
               unsigned c_reg = c_start + i;
               unsigned b_reg = b_start + i / c_per_b;
               assert(i / c_per_b < b_num_regs);
               ra_add_reg_conflict_non_reflexive(set->regs, c_reg, b_reg);
            }
         } else if (classes[b]->reg_align == classes[b]->reg_size &&
                    classes[c]->reg_align >= classes[b]->reg_align &&
                    classes[c]->reg_size >= classes[b]->reg_size) {
            /* IF B's registers are aligned to a register and fit inside C's
             * alignment then one register in C always maps to an integer
             * number of registers in B and there's no weird overlap.
             */
            q_values[b][c] = classes[c]->reg_size / classes[b]->reg_size;

            const unsigned b_per_c = classes[c]->reg_size /
                                     classes[b]->reg_size;
            const unsigned c_stride_b = classes[c]->reg_align /
                                        classes[b]->reg_size;
            for (unsigned i = 0; i < c_num_regs; i++) {
               unsigned c_reg = c_start + i;
               for (unsigned j = 0; j < b_per_c; j++) {
                  unsigned b_reg = b_start + i * c_stride_b + j;
                  assert(i * c_stride_b + j < b_num_regs);
                  ra_add_reg_conflict_non_reflexive(set->regs, c_reg, b_reg);
               }
            }
         } else {
            /*
             * View the register from C as fixed starting at GRF n somwhere in
             * the middle, and the register from B as sliding back and forth.
             * Then the first register to conflict from B is the one starting
             * at n - classes[b].reg_size + 1 and the last register to
             * conflict will start at n + class[b].reg_size - 1.  Therefore,
             * the number of conflicts from B is
             *
             *    classes[b].reg_size + classes[c].reg_size - 1.
             *
             *   +-+-+-+-+-+-+     +-+-+-+-+-+-+
             * B | | | | | |n| --> | | | | | | |
             *   +-+-+-+-+-+-+     +-+-+-+-+-+-+
             *             +-+-+-+-+-+
             * C           |n| | | | |
             *             +-+-+-+-+-+
             */
            q_values[b][c] = classes[b]->reg_size + classes[c]->reg_size - 1;

            for (unsigned i = 0; i < c_num_regs; i++) {
               for (unsigned j = 0; j < b_num_regs; j++) {
                  unsigned c_reg = c_start + i;
                  unsigned b_reg = b_start + j;
                  if (!(ra_reg_to_grf_end[b_reg] <= set->ra_reg_to_grf[c_reg] ||
                        ra_reg_to_grf_end[c_reg] <= set->ra_reg_to_grf[b_reg]))
                     ra_add_reg_conflict_non_reflexive(set->regs, c_reg, b_reg);
               }
            }
         }
      }
   }

   ra_set_finalize(set->regs, q_values);

   ralloc_free(ra_reg_to_grf_end);
   ralloc_free(q_values);
}

struct ibc_ra_singleton {
   /* One for each SIMD width */
   struct ibc_reg_set reg_sets[3];
};

void
ibc_assign_regs_init(struct brw_compiler *compiler)
{
   struct ibc_ra_singleton *ra = rzalloc(compiler, struct ibc_ra_singleton);
   for (unsigned i = 0; i < 3; i++)
      ibc_reg_set_init(&ra->reg_sets[i], 8 << i, ra);
   compiler->ibc_data = ra;
}

static const struct ibc_reg_set *
brw_compiler_get_ibc_reg_set(const struct brw_compiler *compiler,
                             uint8_t simd_width)
{
   const struct ibc_ra_singleton *ra = compiler->ibc_data;

   assert(simd_width == 8 || simd_width == 16 || simd_width == 32);
   int idx = ffs(simd_width) - 4;
   assert(idx >= 0 && idx < ARRAY_SIZE(ra->reg_sets));
   return &ra->reg_sets[idx];
}

static bool
ibc_reg_has_strided_class(const ibc_reg *reg, const struct ibc_reg_set *set)
{
   if (reg->file != IBC_FILE_LOGICAL)
      return false;

   if (reg->logical.simd_width < 4)
      return false;

   if (reg->logical.num_comps > 4)
      return false;

   assert(reg->logical.stride > 0);
   uint16_t comp_size = (uint16_t)reg->logical.stride *
                        (uint16_t)reg->logical.simd_width;
   return reg->logical.simd_width == set->simd_width && comp_size <= 64;
}

static const struct ibc_reg_class *
ibc_reg_to_class(const ibc_reg *reg, const struct ibc_reg_set *set)
{
   switch (reg->file) {
   case IBC_FILE_LOGICAL:
      assert(reg->logical.bit_size % 8 == 0);
      if (reg->logical.simd_width < 4) {
         assert(reg->logical.simd_width == 1);
         uint16_t phys_size = (reg->logical.bit_size / 8) *
                              reg->logical.num_comps;
         return ibc_reg_set_get_physical_class(set, phys_size);
      } else {
         assert(reg->logical.simd_width >= 8);
         assert(reg->logical.stride > 0);
         if (ibc_reg_has_strided_class(reg, set)) {
            return ibc_reg_set_get_strided_class(set, reg->logical.stride,
                                                 reg->logical.num_comps,
                                                 reg->logical.simd_width);
         } else {
            uint16_t phys_size = (uint16_t)reg->logical.stride *
                                 (uint16_t)reg->logical.simd_width *
                                 (uint16_t)reg->logical.num_comps;
            return ibc_reg_set_get_physical_class(set, phys_size);
         }
      }

   case IBC_FILE_HW_GRF:
      assert(reg->hw_grf.align <= 32);
      return ibc_reg_set_get_physical_class(set, ALIGN(reg->hw_grf.size,
                                                       reg->hw_grf.align));

   default:
      unreachable("Unsupported register fil");
   }
}

struct ibc_assign_regs_gc_state {
   void *mem_ctx;

   /* Current instruction when iterating */
   ibc_instr *iter_instr;

   /* True if we're currently walking instruction reads */
   bool is_read;

   const struct ibc_reg_set *reg_set;
   const ibc_live_intervals *live;

   struct ra_graph *g;
   int grf127_send_hack_node;
   unsigned num_hack_nodes;
};

static bool
rewrite_ref_from_gc_graph(ibc_ref *_ref,
                          int num_bytes, int num_comps,
                          uint8_t simd_group, uint8_t simd_width,
                          void *_state)
{
   const ibc_ref *ref = _ref;
   struct ibc_assign_regs_gc_state *state = _state;
   if (ref->file != IBC_FILE_LOGICAL &&
       ref->file != IBC_FILE_HW_GRF)
      return true;

   if (ref->reg == NULL)
      return true;

   const ibc_reg *reg = ref->reg;
   const unsigned node = state->num_hack_nodes + reg->index;
   const unsigned ra_reg = ra_get_node_reg(state->g, node);
   assert(ra_reg != ~0U);
   const unsigned grf_byte = state->reg_set->ra_reg_to_grf[ra_reg];

   ibc_ref new_ref = {
      .file = IBC_FILE_HW_GRF,
      .type = ref->type,
   };
   switch (ref->file) {
   case IBC_FILE_HW_GRF:
      new_ref.hw_grf = ref->hw_grf;
      new_ref.hw_grf.byte += grf_byte;
      break;

   case IBC_FILE_LOGICAL: {
      assert(reg->logical.bit_size % 8 == 0);

      /* Stash this so we can access it unchanged */
      unsigned stride;
      if (reg->logical.simd_width == 1) {
         assert(reg->logical.stride == 0);
         new_ref.hw_grf.byte = grf_byte;
         new_ref.hw_grf.byte += ref->logical.comp *
                                (reg->logical.bit_size / 8);
         new_ref.hw_grf.byte += ref->logical.byte;
         /* This is needed for the !state->is_read case below */
         stride = reg->logical.bit_size / 8;
      } else {
         assert(reg->logical.stride >= reg->logical.bit_size / 8);
         new_ref.hw_grf.byte = grf_byte;
         new_ref.hw_grf.byte += ref->logical.comp *
                                reg->logical.simd_width *
                                reg->logical.stride;
         new_ref.hw_grf.byte += ref->logical.byte;
         stride = reg->logical.stride;
      }

      if (ref->logical.broadcast) {
         new_ref.hw_grf.byte += ref->logical.simd_channel * stride;
      } else if (reg->logical.simd_width > 1) {
         new_ref.hw_grf.byte +=
            (simd_group - reg->logical.simd_group) * stride;
      }

      if (ref->logical.broadcast ||
          (reg->logical.simd_width == 1 && state->is_read)) {
         new_ref.hw_grf.vstride = 0;
         new_ref.hw_grf.width = 1;
         new_ref.hw_grf.hstride = 0;
      } else {
         new_ref.hw_grf.hstride = stride;
         new_ref.hw_grf.width = 8;
         new_ref.hw_grf.vstride = stride * new_ref.hw_grf.width;
      }
      break;
   }

   default:
      unreachable("Unhandled register file");
   }

   ibc_instr_set_ref(state->iter_instr, _ref, new_ref);

   return true;
}

static bool
ibc_assign_regs_graph_color(ibc_shader *shader,
                            const struct brw_compiler *compiler,
                            bool allow_spilling)
{
   ibc_assign_logical_reg_strides(shader);

   struct ibc_assign_regs_gc_state state = {
      .mem_ctx = ralloc_context(shader),
      .reg_set = brw_compiler_get_ibc_reg_set(compiler, shader->simd_width),
   };

   state.live = ibc_compute_live_intervals(shader, live_reg_filter_cb,
                                           state.mem_ctx);

   unsigned node_count = 0;
   state.grf127_send_hack_node = node_count++;
   state.num_hack_nodes = node_count;
   node_count += state.live->num_regs;

   struct ra_graph *g = state.g =
      ra_alloc_interference_graph(state.reg_set->regs, node_count);
   ralloc_steal(state.mem_ctx, state.g);

   ra_set_node_reg(g, state.grf127_send_hack_node,
                   ibc_reg_set_grf_to_reg(state.reg_set, 127 * 32, 32));

   /* Liveness analysis gives us one live set per chunk.  We need a live set
    * per reg because graph coloring doesn't have any finer granularity.
    */
   struct interval_set **reg_logical_live =
      rzalloc_array(state.mem_ctx, struct interval_set *, state.live->num_regs);
   for (unsigned i = 0; i < state.live->num_regs; i++) {
      struct interval_set *reg_live = NULL;
      for (unsigned c = 0; c < state.live->regs[i].num_chunks; c++) {
         struct interval_set *chunk_live =
            state.live->regs[i].chunks[c].logical;
         if (reg_live != NULL && chunk_live != NULL) {
            reg_live = interval_set_from_union(state.mem_ctx,
                                               reg_live, chunk_live);
         } else if (chunk_live != NULL) {
            reg_live = chunk_live;
         }
      }
      reg_logical_live[i] = reg_live;
   }

   ibc_foreach_reg(reg, shader) {
      if (!should_assign_reg(reg))
         continue;

      const struct ibc_reg_class *c = ibc_reg_to_class(reg, state.reg_set);
      ra_set_node_class(g, state.num_hack_nodes + reg->index, c->nr);

      bool reg_has_strided_class =
         ibc_reg_has_strided_class(reg, state.reg_set);

      ibc_foreach_reg(other, shader) {
         if (!should_assign_reg(other))
            continue;

         /* We only need to look at registers before this one as reflexivity
          * of interference will take care of the rest.
          */
         if (other == reg)
            break;

         if (reg_has_strided_class &&
             other->file == IBC_FILE_LOGICAL &&
             reg->logical.stride == other->logical.stride &&
             ibc_reg_has_strided_class(other, state.reg_set)) {
            /* If both registers have a strided class an have the same stride
             * then we can use a logical interference model rather than a
             * physical one.
             */
            if (reg_logical_live[reg->index] != NULL &&
                reg_logical_live[other->index] != NULL &&
                interval_sets_intersect(reg_logical_live[reg->index],
                                        reg_logical_live[other->index]))
               ra_add_node_interference(g, state.num_hack_nodes + reg->index,
                                        state.num_hack_nodes + other->index);
         } else {
            const ibc_reg_live_intervals *rli = &state.live->regs[reg->index];
            const ibc_reg_live_intervals *oli = &state.live->regs[other->index];
            if (!(rli->physical_end <= oli->physical_start ||
                  oli->physical_end <= rli->physical_start))
               ra_add_node_interference(g, state.num_hack_nodes + reg->index,
                                        state.num_hack_nodes + other->index);
         }
      }
   }

   /* Walk backwards so that we hit the EOT send before any LOAD_PAYLOAD */
   ibc_foreach_instr_reverse(instr, shader) {
      switch (instr->type) {
      case IBC_INSTR_TYPE_SEND: {
         ibc_send_instr *send = ibc_instr_as_send(instr);

         /* We can't validate this in ibc_validate because .packed gets set as
          * a late-binding thing.  However, before we RA, we should assert
          * that things are packed.
          */
         if (send->rlen > 0 && send->dest.file == IBC_FILE_LOGICAL)
            assert(send->dest.reg->logical.packed);
         if (send->payload[0].file == IBC_FILE_LOGICAL)
            assert(send->payload[0].reg->logical.packed);
         if (send->ex_mlen > 0 && send->payload[1].file == IBC_FILE_LOGICAL)
            assert(send->payload[1].reg->logical.packed);

         if (send->eot) {
            unsigned byte = TOTAL_GRF_BYTES;
            if (send->ex_mlen > 0) {
               assert(send->payload[1].file == IBC_FILE_LOGICAL ||
                      send->payload[1].file == IBC_FILE_HW_GRF);

               const struct ibc_reg_class *c =
                  ibc_reg_to_class(send->payload[1].reg, state.reg_set);
               byte -= c->reg_size;
               ra_set_node_reg(g, state.num_hack_nodes +
                                  send->payload[1].reg->index,
                               c->reg_start + (byte / c->grf_stride));
            }

            const struct ibc_reg_class *c =
               ibc_reg_to_class(send->payload[0].reg, state.reg_set);
            byte -= c->reg_size;
            ra_set_node_reg(g, state.num_hack_nodes +
                               send->payload[0].reg->index,
                            c->reg_start + (byte / c->grf_stride));
         }
         break;
      }

      case IBC_INSTR_TYPE_INTRINSIC: {
         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
         if (intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD ||
             (intrin->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO &&
              intrin->src[0].ref.file != IBC_FILE_NONE)) {
            assert(intrin->dest.reg);
            assert(ibc_reg_ssa_instr(intrin->dest.reg) == instr);
            assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
            assert(intrin->src[0].ref.reg == NULL);

            const struct ibc_reg_class *c =
               ibc_reg_to_class(intrin->dest.reg, state.reg_set);
            unsigned byte = intrin->src[0].ref.hw_grf.byte;
            unsigned node = state.num_hack_nodes + intrin->dest.reg->index;
            if (ra_get_node_reg(g, node) == ~0U)
               ra_set_node_reg(g, node, c->reg_start + (byte / c->grf_stride));
         }
         break;
      }

      default:
         break;
      }
   }

   if (!ra_allocate(g)) {
      if (allow_spilling) {
         ralloc_free(state.mem_ctx);
         return false;
      }
      abort();
   }

   struct ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      state.iter_instr = instr;

      state.is_read = true;
      ibc_instr_foreach_read(instr, rewrite_ref_from_gc_graph, &state);
      state.is_read = false;
      ibc_instr_foreach_write(instr, rewrite_ref_from_gc_graph, &state);

      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_UNDEF:
         ibc_instr_remove(instr);
         break;

      case IBC_INTRINSIC_OP_LOAD_PAYLOAD:
      case IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO:
         if (intrin->src[0].ref.file == IBC_FILE_NONE)
            continue;

         assert(intrin->dest.file == IBC_FILE_HW_GRF);
         assert(intrin->dest.reg == NULL);
         assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
         assert(intrin->src[0].ref.reg == NULL);

         if (intrin->src[0].ref.hw_grf.byte != intrin->dest.hw_grf.byte) {
            /* Some constraint (such as high GRFs for sends with EOT) may
             * cause the source and destination to get allocated to different
             * registers.  In this case, we need to replace the LOAD_PAYLOAD
             * with a MOV.
             */
            b.cursor = ibc_before_instr(instr);
            ibc_MOV_raw_vec_to(&b, intrin->dest, intrin->src[0].ref,
                               intrin->num_dest_comps);
         }
         ibc_instr_remove(instr);
         break;

      default:
         break;
      }
   }

   ralloc_free(state.mem_ctx);

   return true;
}

bool
ibc_assign_regs(ibc_shader *shader, const struct brw_compiler *compiler,
                bool allow_spilling)
{
   return ibc_assign_regs_graph_color(shader, compiler, allow_spilling);
//   return ibc_assign_regs_linear_scan(shader, allow_spilling);
}
