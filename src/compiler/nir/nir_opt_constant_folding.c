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

#include "nir_builder.h"
#include "nir_constant_expressions.h"
#include <math.h>

/*
 * Implements SSA-based constant folding.
 */

static bool
constant_fold_alu(nir_builder *b, nir_alu_instr *instr)
{
   nir_const_value src[NIR_MAX_VEC_COMPONENTS][NIR_MAX_VEC_COMPONENTS];

   /* In the case that any outputs/inputs have unsized types, then we need to
    * guess the bit-size. In this case, the validator ensures that all
    * bit-sizes match so we can just take the bit-size from first
    * output/input with an unsized type. If all the outputs/inputs are sized
    * then we don't need to guess the bit-size at all because the code we
    * generate for constant opcodes in this case already knows the sizes of
    * the types involved and does not need the provided bit-size for anything
    * (although it still requires to receive a valid bit-size).
    */
   unsigned bit_size = 0;
   if (!nir_alu_type_get_type_size(nir_op_infos[instr->op].output_type))
      bit_size = instr->dest.dest.ssa.bit_size;

   for (unsigned i = 0; i < nir_op_infos[instr->op].num_inputs; i++) {
      nir_const_value *src_const = nir_src_as_const_value(instr->src[i].src);
      if (src_const == NULL)
         return NULL;

      if (bit_size == 0 &&
          !nir_alu_type_get_type_size(nir_op_infos[instr->op].input_types[i]))
         bit_size = instr->src[i].src.ssa->bit_size;

      for (unsigned j = 0; j < nir_ssa_alu_instr_src_components(instr, i);
           j++) {
         src[i][j] = src_const[instr->src[i].swizzle[j]];
      }

      /* We shouldn't have any source modifiers in the optimization loop. */
      assert(!instr->src[i].abs && !instr->src[i].negate);
   }

   if (bit_size == 0)
      bit_size = 32;

   /* We shouldn't have any saturate modifiers in the optimization loop. */
   assert(!instr->dest.saturate);

   nir_const_value dest[NIR_MAX_VEC_COMPONENTS];
   nir_const_value *srcs[NIR_MAX_VEC_COMPONENTS];
   memset(dest, 0, sizeof(dest));
   for (unsigned i = 0; i < nir_op_infos[instr->op].num_inputs; ++i)
      srcs[i] = src[i];
   nir_eval_const_opcode(instr->op, dest, instr->dest.dest.ssa.num_components,
                         bit_size, srcs);

   nir_ssa_def *imm =
      nir_build_imm(b, instr->dest.dest.ssa.num_components,
                       instr->dest.dest.ssa.bit_size, dest);

   nir_ssa_def_rewrite_uses(&instr->dest.dest.ssa, nir_src_for_ssa(imm));
   nir_instr_remove(&instr->instr);
   ralloc_free(instr);

   return true;
}

static bool
constant_fold_intrinsic(nir_builder *b, nir_intrinsic_instr *instr)
{
   if (instr->intrinsic == nir_intrinsic_discard_if &&
       nir_src_is_const(instr->src[0])) {
      if (nir_src_as_bool(instr->src[0])) {
         nir_intrinsic_instr *discard =
            nir_intrinsic_instr_create(b->shader, nir_intrinsic_discard);
         nir_builder_instr_insert(b, &discard->instr);
      }
      nir_instr_remove(&instr->instr);
      return true;
   }

   return false;
}

static bool
constant_fold_instr(nir_builder *b, nir_instr *instr,
                    UNUSED void *data, UNUSED void *mem_ctx)
{
   switch (instr->type) {
   case nir_instr_type_alu:
      return constant_fold_alu(b, nir_instr_as_alu(instr));
   case nir_instr_type_intrinsic:
      return constant_fold_intrinsic(b, nir_instr_as_intrinsic(instr));
   default:
      unreachable("Unsupported instruction type");
   }
}

const nir_pass nir_opt_constant_folding_pass = {
   .instr_pass_func = constant_fold_instr,
   .metadata_preserved = nir_metadata_block_index | nir_metadata_dominance,
};
