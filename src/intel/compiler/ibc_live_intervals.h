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

#ifndef IBC_LIVE_INTERVALS_H
#define IBC_LIVE_INTERVALS_H

#include "ibc.h"

#include "util/bitset.h"
#include "util/interval_set.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
   /**
    * Which chunks are defined before being used in the block.
    *
    * Note that for our purposes, "defined" means unconditionally, completely
    * defined.
    */
   BITSET_WORD *def;

   /**
    * Which chunks are used before being defined in the block.
    */
   BITSET_WORD *use;

   /** Which defs reach the entry point of the block. */
   BITSET_WORD *livein;

   /** Which defs reach the exit point of the block. */
   BITSET_WORD *liveout;

   /**
    * Chunks such that the entry point of the block may be reached from any
    * of their definitions.
    */
   BITSET_WORD *defin;

   /**
    * Chunks such that the exit point of the block may be reached from any
    * of their definitions.
    */
   BITSET_WORD *defout;
} ibc_block_live_sets;

/* A SIMD32 dvec4 is 16 registers and the maximum message size is 11 */
#define IBC_REG_LIVE_MAX_CHUNKS (4 * 8 * 32)

typedef struct {
   /** Chunk index of this register */
   uint32_t chunk_idx;

   /** Number of chunks tracked for this register
    *
    * This governs how many bits this reg consumes in the per-block bitsets
    * and how many per_comp_live sets it has.
    */
   uint16_t num_chunks;

   uint8_t chunk_simd_width;
   uint8_t chunk_byte_size;

   uint32_t physical_start;
   uint32_t physical_end;

   struct interval_set **chunk_live;
} ibc_reg_live_intervals;

typedef struct {
   /** Number of registers for which we have live intervals */
   uint32_t num_regs;

   /** Total number of chunks in all registers */
   uint32_t num_chunks;

   ibc_block_live_sets *blocks;
   ibc_reg_live_intervals *regs;
   struct interval_set **chunk_live;
} ibc_live_intervals;

void
ibc_live_intervals_reg_ref_chunks(const ibc_live_intervals *live,
                                  const ibc_reg_ref *ref,
                                  int8_t num_bytes, int8_t num_comps,
                                  uint8_t simd_group, uint8_t simd_width,
                                  BITSET_WORD *chunks);

ibc_live_intervals *
ibc_compute_live_intervals(ibc_shader *shader, void *mem_ctx);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_LIVE_INTERVALS_H */
