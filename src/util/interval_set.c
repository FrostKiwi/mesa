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

#include "interval_set.h"

#include "bitscan.h"
#include "ralloc.h"

static struct interval_set *
interval_set_alloc(void *mem_ctx, uint32_t alloc)
{
   /* We use ralloc for our interval sets and a ralloc header already burns
    * six pointers worth of memory.  We figure it won't hurt pressure too much
    * if we always allocate at least three intervals (8 bytes for the whole
    * data structure) and this lets us avoid a lot of re-allocation when we
    * have to grow things.
    */
   assert(alloc >= 4 && util_is_power_of_two_or_zero(alloc));

   struct interval_set *set =
      ralloc_size(mem_ctx, sizeof(*set) + alloc * sizeof(set->intervals[0]));

   set->count = 0;
   set->_alloc = alloc;

   return set;
}

struct interval_set *
interval_set_create(void *mem_ctx)
{
   return interval_set_alloc(mem_ctx, 4);
}

static struct interval_set *
interval_set_grow(void *mem_ctx,
                  struct interval_set *set,
                  uint32_t alloc_min)
{
   if (set->_alloc >= alloc_min)
      return set;

   uint32_t alloc = set->_alloc * 2;
   while (alloc < alloc_min)
      alloc *= 2;

   struct interval_set *new_set =
      interval_set_alloc(mem_ctx, alloc);

   new_set->count = set->count;
   memcpy(new_set->intervals, set->intervals,
          set->count * sizeof(set->intervals[0]));

   ralloc_free(set);

   return new_set;
}

struct interval_set *
interval_set_add_end(void *mem_ctx,
                     struct interval_set *set,
                     uint32_t start, uint32_t end)
{
   assert(start < end);

   if (set != NULL && set->count > 0) {
      /* Try to just extend the last interval if we can */
      assert(set->intervals[set->count - 1].start <= start);
      if (set->intervals[set->count - 1].end >= start) {
         if (set->intervals[set->count - 1].end < end)
            set->intervals[set->count - 1].end = end;
         return set;
      }
   }

   if (set == NULL)
      set = interval_set_create(mem_ctx);

   set = interval_set_grow(mem_ctx, set, set->count + 1);
   set->intervals[set->count].start = start;
   set->intervals[set->count].end = end;
   set->count++;

   return set;
}

void
interval_set_extend_to(struct interval_set *set,
                       uint32_t end)
{
   assert(set->count > 0);
   assert(set->intervals[set->count - 1].end <= end);
   set->intervals[set->count - 1].end = end;
}

struct interval_set *
interval_set_from_union(void *mem_ctx,
                        const struct interval_set *a,
                        const struct interval_set *b)
{
   struct interval_set *u = interval_set_create(mem_ctx);

   uint32_t ai = 0, bi = 0;
   while (1) {
      if (ai < a->count) {
         if (bi < b->count) {
            /* They both have intervals.  Add the one that starts first. */
            if (a->intervals[ai].start < b->intervals[bi].start) {
               u = interval_set_add_end(mem_ctx, u,
                                        a->intervals[ai].start,
                                        a->intervals[ai].end);
               ai++;
            } else {
               u = interval_set_add_end(mem_ctx, u,
                                        b->intervals[bi].start,
                                        b->intervals[bi].end);
               bi++;
            }
         } else {
            /* b is out of intervals */
            u = interval_set_add_end(mem_ctx, u,
                                     a->intervals[ai].start,
                                     a->intervals[ai].end);
            ai++;
         }
      } else {
         if (bi < b->count) {
            /* a is out of intervals */
            u = interval_set_add_end(mem_ctx, u,
                                     b->intervals[bi].start,
                                     b->intervals[bi].end);
            bi++;
         } else {
            /* They're both out of intervals */ return u;
         }
      }
   }
}

bool
interval_sets_intersect(const struct interval_set *a,
                        const struct interval_set *b)
{
   uint32_t ai = 0, bi = 0;
   while (1) {
      while (a->intervals[ai].end <= b->intervals[bi].start) {
         ai++;
         if (ai >= a->count)
            return false;
      }
      if (a->intervals[ai].start <= b->intervals[bi].end)
         return true;
      while (b->intervals[bi].end <= a->intervals[ai].start) {
         bi++;
         if (bi >= b->count)
            return false;
      }
      if (b->intervals[bi].start <= a->intervals[ai].end)
         return true;
   }
}
