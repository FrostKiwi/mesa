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

#ifndef _UTIL_INTERVAL_SET_H
#define _UTIL_INTERVAL_SET_H

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Represents a set of disjoint half-open intervals */
struct interval_set {
   /** Number of intervals in this set */
   uint32_t count;

   /** Number of intervals with which set was allocated */
   uint32_t _alloc;

   struct {
      /** Start (inclusive) of this interval */
      uint32_t start;

      /** End (exclusive) of this interval */
      uint32_t end;
   } intervals[0];
};

struct interval_set *
interval_set_create(void *mem_ctx);

static inline uint32_t
interval_set_start(const struct interval_set *set)
{
   assert(set->count > 0);
   return set->intervals[0].start;
}

static inline uint32_t
interval_set_end(const struct interval_set *set)
{
   assert(set->count > 0);
   return set->intervals[set->count - 1].end;
}

static inline bool
interval_set_contains(const struct interval_set *set, uint32_t value)
{
   for (unsigned i = 0; i < set->count; i++) {
      if (set->intervals[i].end <= value)
         continue;

      /* This is the first interval with value < end */
      return set->intervals[i].start <= value;
   }

   return false;
}

/** Add an interval at the end of the given set, possibly re-allocating it
 *
 * If the set is NULL a new set will be allocated containing the one interval.
 * If the set needs to be reallocated to accommodate the new interval, the
 * new set will be returned and the original will be freed.  Otherwise, the
 * original set will be modified and returned.
 */
struct interval_set *
interval_set_add_end(void *mem_ctx,
                     struct interval_set *set,
                     uint32_t start, uint32_t end);
void
interval_set_extend_to(struct interval_set *set,
                       uint32_t end);

struct interval_set *
interval_set_from_union(void *mem_ctx,
                        const struct interval_set *a,
                        const struct interval_set *b);

bool
interval_sets_intersect(const struct interval_set *a,
                        const struct interval_set *b);

#ifdef __cplusplus
} /* extern C */
#endif

#endif /* _UTIL_INTERVAL_SET_H */
