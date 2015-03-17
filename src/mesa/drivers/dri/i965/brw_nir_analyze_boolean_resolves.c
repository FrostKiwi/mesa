/*
 * Copyright © 2015 Intel Corporation
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
 *    Jason Ekstrand <jason@jlekstrand.net>
 */

#include "brw_nir.h"

/*
 * This file implements an analysis pass that determines when we have to do
 * a boolean resolve on GEN <= 5.  Instructions that need a boolean resolve
 * will have the booleans portion of the instr->pass_flags field set to
 * BRW_NIR_BOOLEAN_NEEDS_RESOLVE.
 */

static uint8_t
get_resolve_state_for_src(nir_alu_instr *alu, unsigned src_idx)
{
   nir_instr *src_instr;
   if (alu->src[src_idx].src.is_ssa) {
      src_instr = alu->src[src_idx].src.ssa->parent_instr;
   } else {
      src_instr = alu->src[src_idx].src.reg.reg->parent_instr;
   }

   if (src_instr) {
      uint8_t state = src_instr->pass_flags & BRW_NIR_BOOLEAN_MASK;

      /* If the source instruction nees resolve then, from the perspective
       * of the user, it's a true boolean.
       */
      if (state == BRW_NIR_BOOLEAN_NEEDS_RESOLVE)
         state = BRW_NIR_BOOLEAN_NO_RESOLVE;
      return state;
   } else {
      return BRW_NIR_NON_BOOLEAN;
   }
}

static bool
src_mark_needs_resolve(nir_src *src, void *void_state)
{
   if (src->is_ssa)
      return true;

   if (src->reg.reg->parent_instr == NULL)
      return true;

   nir_instr *src_instr = src->reg.reg->parent_instr;

   uint8_t bool_flags = src_instr->pass_flags & BRW_NIR_BOOLEAN_MASK;
   if (bool_flags == BRW_NIR_BOOLEAN_UNRESOLVED) {
      src_instr->pass_flags &= ~BRW_NIR_BOOLEAN_MASK;
      src_instr->pass_flags |= BRW_NIR_BOOLEAN_NEEDS_RESOLVE;
   }

   return true;
}

static bool
analyze_boolean_resolves_block(nir_block *block, void *void_state)
{
   nir_foreach_instr(block, instr) {
      /* Clear the boolean state */
      instr->pass_flags &= ~BRW_NIR_BOOLEAN_MASK;

      switch (instr->type) {
      case nir_instr_type_alu:
         /* For ALU instructions, we handle [un]resolved booleans below. */
         break;

      case nir_instr_type_load_const: {
         /* For load_const instructions, it's a boolean exactly when it holds
          * one of the values NIR_TRUE or NIR_FALSE.
          */
         nir_load_const_instr *load = nir_instr_as_load_const(instr);
         if (load->value.u[0] == NIR_TRUE || load->value.u[0] == NIR_FALSE) {
            instr->pass_flags |= BRW_NIR_BOOLEAN_NO_RESOLVE;
         } else {
            instr->pass_flags |= BRW_NIR_NON_BOOLEAN;
         }
         continue;
      }

      default:
         /* Everything else is an unknown non-boolean value and needs to
          * have all sources resolved.
          */
         instr->pass_flags |= BRW_NIR_NON_BOOLEAN;
         nir_foreach_src(instr, src_mark_needs_resolve, NULL);
         continue;
      }

      uint8_t bool_status;
      nir_alu_instr *alu = nir_instr_as_alu(instr);
      switch (alu->op) {
      case nir_op_flt:
      case nir_op_ilt:
      case nir_op_ult:
      case nir_op_fge:
      case nir_op_ige:
      case nir_op_uge:
      case nir_op_feq:
      case nir_op_ieq:
      case nir_op_fne:
      case nir_op_ine:
      case nir_op_f2b:
      case nir_op_i2b:
         bool_status = BRW_NIR_BOOLEAN_UNRESOLVED;

         /* Even though the destination is allowed to be left unresolved,
          * we need to resolve all the sources of a compare.
          */
         nir_foreach_src(instr, src_mark_needs_resolve, NULL);
         break;

      case nir_op_imov:
      case nir_op_inot:
         if (alu->dest.write_mask == 1) {
            /* This is a single-source instruction.  Just copy the resolution
             * state from the source.
             */
            bool_status = get_resolve_state_for_src(alu, 0);
         } else {
            bool_status = BRW_NIR_NON_BOOLEAN;
         }
         break;

      case nir_op_iand:
      case nir_op_ior:
      case nir_op_ixor: {
         assert(alu->dest.write_mask == 1);

         uint8_t src0_flags = get_resolve_state_for_src(alu, 0);
         uint8_t src1_flags = get_resolve_state_for_src(alu, 1);

         if (src0_flags == src1_flags) {
            bool_status = src0_flags;
         } else if (src0_flags == BRW_NIR_NON_BOOLEAN ||
                    src1_flags == BRW_NIR_NON_BOOLEAN) {
            bool_status = BRW_NIR_NON_BOOLEAN;
         } else {
            /* At this point one of them is a true boolean and one is a
             * boolean that needs a resolve.  We could either resolve the
             * unresolved source or we could resolve here.  If we resolve
             * the unresolved source then we get two resolves for the price
             * of one.  Just set this one to BOOLEAN_NO_RESOLVE and we'll
             * let the code below force a resolve on the unresolved source.
             */
            bool_status = BRW_NIR_BOOLEAN_NO_RESOLVE;
         }
         break;
      }

      default:
         bool_status = BRW_NIR_NON_BOOLEAN;
      }

      /* If the destination is SSA-like, go ahead allow unresolved booleans.
       * If the destination register doesn't have a well-defined parent_instr
       * we need to resolve immediately.
       */
      if (alu->dest.dest.reg.reg->parent_instr == NULL &&
          bool_status == BRW_NIR_BOOLEAN_UNRESOLVED) {
         bool_status = BRW_NIR_BOOLEAN_NEEDS_RESOLVE;
      }

      instr->pass_flags |= bool_status;

      /* Finally, resolve sources if it's needed */
      switch (bool_status) {
      case BRW_NIR_BOOLEAN_NEEDS_RESOLVE:
      case BRW_NIR_BOOLEAN_UNRESOLVED:
         /* This instruction is either unresolved or we're doing the
          * resolve here; leave the sources alone.
          */
         break;

      case BRW_NIR_BOOLEAN_NO_RESOLVE:
      case BRW_NIR_NON_BOOLEAN:
         nir_foreach_src(instr, src_mark_needs_resolve, NULL);
         break;

      default:
         unreachable("Invalid boolean flag");
      }
   }

   nir_if *following_if = nir_block_get_following_if(block);
   if (following_if)
      src_mark_needs_resolve(&following_if->condition, NULL);

   return true;
}

static void
analyze_boolean_resolves_impl(nir_function_impl *impl)
{
   nir_foreach_block(impl, analyze_boolean_resolves_block, NULL);
}

void
brw_nir_analyze_boolean_resolves(nir_shader *shader)
{
   nir_foreach_overload(shader, overload)
      if (overload->impl)
         analyze_boolean_resolves_impl(overload->impl);
}
