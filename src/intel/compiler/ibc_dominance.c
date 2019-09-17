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

/*
 * Implements the algorithms for computing the dominance tree from "A Simple,
 * Fast Dominance Algorithm" by Cooper, Harvey, and Kennedy.
 */

static ibc_dominance_block *
intersect(ibc_dominance_block *b1, ibc_dominance_block *b2)
{
   while (b1 != b2) {
      /* Note, the comparisons here are the opposite of what the paper says
       * because we index blocks from beginning -> end (i.e. reverse
       * post-order) instead of post-order like they assume.
       */
      while (b1 > b2)
         b1 = b1->imm_dom;
      while (b2 > b1)
         b2 = b2->imm_dom;
   }
   return b1;
}

ibc_dominance *
ibc_compute_dominance(ibc_shader *shader, void *mem_ctx)
{
   ibc_dominance *dominance = rzalloc(mem_ctx, ibc_dominance);

   ibc_foreach_flow_instr(flow, shader)
      flow->block_index = dominance->num_blocks++;

   dominance->blocks = rzalloc_array(mem_ctx, ibc_dominance_block,
                                     dominance->num_blocks);
   ibc_dominance_block *blocks = dominance->blocks;

   ibc_foreach_flow_instr(flow, shader)
      blocks[flow->block_index].flow = flow;

   /* Discard the END. */
   dominance->num_blocks--;

   blocks[0].imm_dom = &blocks[0];

   bool progress = true;
   while (progress) {
      progress = false;
      for (int i = 1; i < dominance->num_blocks; i++) {
         ibc_dominance_block *block = &blocks[i];

         ibc_dominance_block *new_idom = NULL;
         ibc_foreach_flow_pred(pred, block->flow) {
            /* The pred points to the flow instruction at the end of the
             * predecessor block but blocks are indexed based on the flow at
             * the start of the block.
             */
            assert(pred->instr->block_index > 0);
            ibc_dominance_block *pred_block = &blocks[pred->instr->block_index - 1];
            if (pred_block->imm_dom) {
               if (new_idom)
                  new_idom = intersect(pred_block, new_idom);
               else
                  new_idom = pred_block;
            }
         }

         if (block->imm_dom != new_idom) {
            block->imm_dom = new_idom;
            progress = true;
         }
      }
   }

   /* It was convenient to have the start block immediate dominate itself for
    * the algorithm above, but not for the one below.
    */
   blocks[0].imm_dom = NULL;

   /* Compute the dominance tree.  First set up enough space for the children
    * pointers (first two passes), then walk each block adding it to its
    * immediate dominator list of children.
    */
   for (int i = 0; i < dominance->num_blocks; i++) {
      ibc_dominance_block *idom = blocks[i].imm_dom;
      if (idom)
         idom->num_dom_children++;
   }

   for (int i = 0; i < dominance->num_blocks; i++) {
      ibc_dominance_block *block = &blocks[i];
      block->dom_children = rzalloc_array(shader, ibc_dominance_block *,
                                          block->num_dom_children);
      block->num_dom_children = 0;
   }

   for (int i = 0; i < dominance->num_blocks; i++) {
      ibc_dominance_block *idom = blocks[i].imm_dom;
      if (idom)
         idom->dom_children[idom->num_dom_children++] = &blocks[i];
   }

   return dominance;
}
