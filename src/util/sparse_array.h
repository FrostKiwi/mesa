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

#ifndef _UTIL_SPARSE_ARRAY_H
#define _UTIL_SPARSE_ARRAY_H

#include <stdint.h>

#include "c11/threads.h"
#include "macros.h"
#include "u_atomic.h"
#include "u_math.h"

#ifdef __cplusplus
extern "C" {
#endif

struct util_sparse_array_node;

/** A thread-safe automatically growing sparse array data structure
 *
 * This data structure has the following very nice properties:
 *
 *  1. Accessing an element is basically constant time.  Technically, it's
 *     O(log_b n) where the base b is the node size and n is the maximum
 *     index.  However, node sizes are expected to be fairly large and the
 *     index is a uint64_t so, if your node size is 256, it's O(8).
 *
 *  2. The data stored in the array is never moved in memory.  Instead, the
 *     data structure only ever grows and new nodes are added as-needed.  This
 *     means it's safe to store a pointer to something stored in the sparse
 *     array without worrying about a realloc invalidating it.
 *
 *  3. The data structure is thread-safe.  No guarantees are made about the
 *     data stored in the sparse array but it is safe to call
 *     util_sparse_array_get(arr, idx) from as many threads as you'd like and
 *     we guarantee that two calls to util_sparse_array_get(arr, idx) with the
 *     same array and index will always return the same pointer regardless
 *     contention between threads.
 *
 *  4. The data structure is lock-free.  All manipulations of the tree are
 *     done by a careful use of atomics to maintain thread safety and no locks
 *     are ever taken other than those taken implicitly by calloc().  If no
 *     allocation is required, util_sparse_array_get(arr, idx) does a simple
 *     walk over the tree should be efficient even in the case where many
 *     threads are accessing the sparse array at once.
 */
struct util_sparse_array {
   size_t elem_size;
   unsigned node_size_log2;

   struct util_sparse_array_node *root;
};

static inline void
util_sparse_array_init(struct util_sparse_array *arr,
                       size_t elem_size, size_t node_size);

static inline void
util_sparse_array_finish(struct util_sparse_array *arr);

static inline void *
util_sparse_array_get(struct util_sparse_array *arr, uint64_t idx);


/*============ Everything below this line is implementation ============*/

struct util_sparse_array_node {
   uint32_t level;
   uint32_t _pad;
   uint64_t max_idx;
};

static inline void
util_sparse_array_init(struct util_sparse_array *arr,
                       size_t elem_size, size_t node_size)
{
   memset(arr, 0, sizeof(*arr));
   arr->elem_size = elem_size;
   arr->node_size_log2 = util_logbase2_64(node_size);
   assert(node_size >= 2 && node_size == (1ull << arr->node_size_log2));
}

static inline void *
_util_sparse_array_node_data(struct util_sparse_array_node *node)
{
   return node + 1;
}

static inline void
_util_sparse_array_node_finish(struct util_sparse_array *arr,
                               struct util_sparse_array_node *node)
{
   if (node->level > 0) {
      struct util_sparse_array_node **children =
         _util_sparse_array_node_data(node);
      size_t node_size = 1ull << arr->node_size_log2;
      for (size_t i = 0; i < node_size; i++) {
         if (children[i] != NULL)
            _util_sparse_array_node_finish(arr, children[i]);
      }
   }

   free(node);
}

static inline void
util_sparse_array_finish(struct util_sparse_array *arr)
{
   if (arr->root)
      _util_sparse_array_node_finish(arr, arr->root);
}

static inline struct util_sparse_array_node *
_util_sparse_array_alloc_node(struct util_sparse_array *arr,
                              unsigned level)
{
   size_t size = sizeof(struct util_sparse_array_node);
   if (level == 0) {
      size += arr->elem_size << arr->node_size_log2;
   } else {
      size += sizeof(struct util_sparse_array_node *) << arr->node_size_log2;
   }

   struct util_sparse_array_node *node = calloc(1, size);
   node->level = level;

   return node;
}

static inline struct util_sparse_array_node *
_util_sparse_array_set_or_free_node(struct util_sparse_array_node **node_ptr,
                                    struct util_sparse_array_node *cmp_node,
                                    struct util_sparse_array_node *node)
{
   struct util_sparse_array_node *prev_node =
      p_atomic_cmpxchg(node_ptr, cmp_node, node);

   if (prev_node != cmp_node) {
      /* We lost the race.  Free this one and return the one that was already
       * allocated.
       */
      free(node);
      return prev_node;
   } else {
      return node;
   }
}

static inline void *
util_sparse_array_get(struct util_sparse_array *arr, uint64_t idx)
{
   struct util_sparse_array_node *root = p_atomic_read(&arr->root);
   if (unlikely(root == NULL)) {
      unsigned root_level = 0;
      uint64_t idx_iter = idx >> arr->node_size_log2;
      while (idx_iter) {
         idx_iter >>= arr->node_size_log2;
         root_level++;
      }
      struct util_sparse_array_node *new_root =
         _util_sparse_array_alloc_node(arr, root_level);
      root = _util_sparse_array_set_or_free_node(&arr->root, NULL, new_root);
   }

   while (1) {
      uint64_t root_idx = idx >> (root->level * arr->node_size_log2);
      if (likely(root_idx < (1ull << arr->node_size_log2)))
         break;

      /* In this case, we have a root but its level is low enough that the
       * requested index is out-of-bounds.
       */
      struct util_sparse_array_node *new_root =
         _util_sparse_array_alloc_node(arr, root->level + 1);

      struct util_sparse_array_node **new_root_children =
         _util_sparse_array_node_data(new_root);
      new_root_children[0] = root;

      /* We only add one at a time instead of the whole tree because it's
       * easier to ensure correctness of both the tree building and the
       * clean-up path.  Because we're only adding one node we never have to
       * worry about trying to free multiple things without freeing the old
       * things.
       */
      root = _util_sparse_array_set_or_free_node(&arr->root, root, new_root);
   }

   struct util_sparse_array_node *node = root;
   while (node->level > 0) {
      uint64_t child_idx = (idx >> (node->level * arr->node_size_log2)) &
                           ((1ull << arr->node_size_log2) - 1);

      struct util_sparse_array_node **children =
         _util_sparse_array_node_data(node);
      struct util_sparse_array_node *child =
         p_atomic_read(&children[child_idx]);

      if (unlikely(child == NULL)) {
         child = _util_sparse_array_alloc_node(arr, node->level - 1);
         child = _util_sparse_array_set_or_free_node(&children[child_idx],
                                                     NULL, child);
      }

      node = child;
   }

   uint64_t elem_idx = idx & ((1ull << arr->node_size_log2) - 1);
   return _util_sparse_array_node_data(node) + (elem_idx * arr->elem_size);
}

#ifdef __cplusplus
} /* extern C */
#endif

#endif /* _UTIL_SPARSE_ARRAY_H */
