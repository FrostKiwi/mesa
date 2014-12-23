/*
 * Copyright © 2014 Intel Corporation
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
 *
 * Authors:
 *    Jason Ekstrand (jason@jlekstrand.net)
 *
 */

#include "nir.h"

/*
 * Implements common subexpression elimination
 */

struct lower_alu_to_scalar_state {
   void *mem_ctx;
   void *dead_ctx;
};

static bool
lower_alu_to_scalar_block(nir_block *block, void *void_state)
{
   struct lower_alu_to_scalar_state *state = void_state;

   nir_foreach_instr_safe(block, instr) {
      if (instr->type != nir_instr_type_alu)
         continue;

      nir_alu_instr *alu = nir_instr_as_alu(instr);

      if (nir_op_infos[alu->op].output_size != 0)
         continue;

      if (alu->dest.write_mask == 1)
         continue; /* It's already scalar.  Nothing to do */

      /* We only handle SSA for now. */
      assert(alu->dest.dest.is_ssa);

      /* Since it SSA, we should have already handled the 1 component case */
      assert(alu->dest.dest.ssa.num_components > 1);

      /* Create a vecN operation to combine the results.  Most of these
       * will be redundant, but copy propagation should clean them up for
       * us.  No need to add the complexity here.
       */
      nir_op vec_op;
      switch (alu->dest.dest.ssa.num_components) {
      case 2: vec_op = nir_op_vec2; break;
      case 3: vec_op = nir_op_vec3; break;
      case 4: vec_op = nir_op_vec4; break;
      default: unreachable("Invalid number of components");
      }

      nir_alu_instr *vec = nir_alu_instr_create(state->mem_ctx, vec_op);
      vec->dest.dest.is_ssa = true;
      nir_ssa_def_init(&vec->instr, &vec->dest.dest.ssa,
                       alu->dest.dest.ssa.num_components, NULL);
      vec->dest.write_mask = (1 << alu->dest.dest.ssa.num_components) - 1;

      for (unsigned i = 0; i < 4; i++) {
         if (!(alu->dest.write_mask & (1 << i)))
            continue;

         nir_alu_instr *new_alu = nir_alu_instr_create(state->mem_ctx, alu->op);
         new_alu->dest.dest.is_ssa = true;
         nir_ssa_def_init(&new_alu->instr, &new_alu->dest.dest.ssa, 1, NULL);
         new_alu->dest.write_mask = 1;
         new_alu->dest.saturate = alu->dest.saturate;

         vec->src[i].src.is_ssa = true;
         vec->src[i].src.ssa = &new_alu->dest.dest.ssa;

         for (unsigned j = 0; j < nir_op_infos[alu->op].num_inputs; j++) {
            new_alu->src[j].src = nir_src_copy(alu->src[j].src, state->mem_ctx);
            if (nir_op_infos[alu->op].input_sizes[j] == 0) {
               /* vectorized source; grab the corresponding component */
               new_alu->src[j].swizzle[0] = alu->src[j].swizzle[i];
            } else {
               /* explicit source; should be the same as the original */
               memcpy(new_alu->src[j].swizzle, alu->src[j].swizzle, 4);
            }
            new_alu->src[j].abs = alu->src[j].abs;
            new_alu->src[j].negate = alu->src[j].negate;
         }

         nir_instr_insert_before(&alu->instr, &new_alu->instr);
      }

      nir_instr_insert_before(&alu->instr, &vec->instr);

      nir_ssa_def_rewrite_uses(&alu->dest.dest.ssa,
                               nir_src_for_ssa(&vec->dest.dest.ssa),
                               state->mem_ctx);

      ralloc_steal(state->mem_ctx, alu);
      nir_instr_remove(&alu->instr);
   }

   return true;
}

static void
lower_alu_to_scalar_impl(nir_function_impl *impl)
{
   struct lower_alu_to_scalar_state state;

   state.mem_ctx = ralloc_parent(impl);
   state.dead_ctx = ralloc_context(NULL);

   nir_foreach_block(impl, lower_alu_to_scalar_block, &state);

   nir_metadata_preserve(impl, nir_metadata_block_index |
                               nir_metadata_dominance);

   ralloc_free(state.dead_ctx);
}

/** Converts all vector ALU operations in the shader to scalar operations
 *
 * This is done by replaceing each vectorized operation with one scalar
 * operation for each chananel and a vecN to pull the channels back
 * together into a vector.  The common case is that the result of the ALU
 * operations will go back into other ALU operations and that the vecN will
 * get copy-prpagated away.  If the result is used by something other than
 * an ALU operation, then we would have needed the vecN to assemble the
 * result together anyway.
 */
void
nir_lower_alu_to_scalar(nir_shader *shader)
{
   nir_foreach_overload(shader, overload) {
      if (overload->impl)
         lower_alu_to_scalar_impl(overload->impl);
   }
}
