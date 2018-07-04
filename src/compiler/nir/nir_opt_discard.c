/*
 * Copyright © 2018 Intel Corporation
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

#include "nir.h"
#include "nir_builder.h"
#include "nir_control_flow.h"
#include "nir_worklist.h"

static bool
block_has_only_discard(nir_block *block)
{
   nir_instr *instr = nir_block_first_instr(block);
   if (instr == NULL || instr != nir_block_last_instr(block))
      return false;

   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
   return intrin->intrinsic == nir_intrinsic_discard;
}

static bool
opt_discard_if_impl(nir_function_impl *impl)
{
   bool progress = false;

   nir_builder b;
   nir_builder_init(&b, impl);

   nir_foreach_block(block, impl) {
      nir_if *nif = nir_block_get_following_if(block);
      if (!nif)
         continue;

      bool discard_in_then;
      if (block_has_only_discard(nir_if_first_then_block(nif)))
         discard_in_then = true;
      else if (block_has_only_discard(nir_if_first_else_block(nif)))
         discard_in_then = false;
      else
         continue;

      b.cursor = nir_after_block(block);
      nir_ssa_def *cond = nir_ssa_for_src(&b, nif->condition, 1);
      if (!discard_in_then)
         cond = nir_inot(&b, cond);

      nir_intrinsic_instr *discard_if =
         nir_intrinsic_instr_create(b.shader, nir_intrinsic_discard_if);
      discard_if->src[0] = nir_src_for_ssa(cond);
      nir_builder_instr_insert(&b, &discard_if->instr);

      nir_lower_phis_to_regs_block(nir_cf_node_as_block(
                                   nir_cf_node_next(&nif->cf_node)));

      nir_cf_list list;
      if (discard_in_then)
         nir_cf_list_extract(&list, &nif->else_list);
      else
         nir_cf_list_extract(&list, &nif->then_list);
      nir_cf_reinsert(&list, nir_after_instr(&discard_if->instr));

      nir_cf_node_remove(&nif->cf_node);

      progress = true;
   }

   /* If we modified control-flow, metadata is toast.  Also, we may have
    * lowered some phis to registers so we need to back into SSA.
    */
   if (progress) {
      nir_metadata_preserve(impl, 0);
      nir_lower_regs_to_ssa_impl(impl);
   }

   return progress;
}

bool
nir_opt_discard_if(nir_shader *shader)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   bool progress = false;

   nir_foreach_function(function, shader) {
      if (function->impl &&
          opt_discard_if_impl(function->impl))
         progress = true;
   }

   return progress;
}

static bool
nir_variable_mode_is_read_only(nir_variable_mode mode)
{
   return mode == nir_var_shader_in ||
          mode == nir_var_uniform ||
          mode == nir_var_system_value;
}

static bool
nir_op_is_derivative(nir_op op)
{
   return op == nir_op_fddx ||
          op == nir_op_fddy ||
          op == nir_op_fddx_fine ||
          op == nir_op_fddy_fine ||
          op == nir_op_fddx_coarse ||
          op == nir_op_fddy_coarse;
}

static bool
nir_texop_implies_derivative(nir_texop op)
{
   return op == nir_texop_tex ||
          op == nir_texop_txb ||
          op == nir_texop_lod;
}

static bool
nir_intrinsic_writes_external_memory(nir_intrinsic_op intrin)
{
   switch (intrin) {
   case nir_intrinsic_store_deref:
   case nir_intrinsic_copy_deref:
   case nir_intrinsic_deref_atomic_add:
   case nir_intrinsic_deref_atomic_imin:
   case nir_intrinsic_deref_atomic_umin:
   case nir_intrinsic_deref_atomic_imax:
   case nir_intrinsic_deref_atomic_umax:
   case nir_intrinsic_deref_atomic_and:
   case nir_intrinsic_deref_atomic_or:
   case nir_intrinsic_deref_atomic_xor:
   case nir_intrinsic_deref_atomic_exchange:
   case nir_intrinsic_deref_atomic_comp_swap:
      /* If we ever start using variables for SSBO ops, we'll need to do
       * something here.  For now, they're safe.
       */
      return false;

   case nir_intrinsic_store_ssbo:
   case nir_intrinsic_ssbo_atomic_add:
   case nir_intrinsic_ssbo_atomic_imin:
   case nir_intrinsic_ssbo_atomic_umin:
   case nir_intrinsic_ssbo_atomic_imax:
   case nir_intrinsic_ssbo_atomic_umax:
   case nir_intrinsic_ssbo_atomic_and:
   case nir_intrinsic_ssbo_atomic_or:
   case nir_intrinsic_ssbo_atomic_xor:
   case nir_intrinsic_ssbo_atomic_exchange:
   case nir_intrinsic_ssbo_atomic_comp_swap:
      return true;

   case nir_intrinsic_image_deref_store:
   case nir_intrinsic_image_deref_atomic_add:
   case nir_intrinsic_image_deref_atomic_min:
   case nir_intrinsic_image_deref_atomic_max:
   case nir_intrinsic_image_deref_atomic_and:
   case nir_intrinsic_image_deref_atomic_or:
   case nir_intrinsic_image_deref_atomic_xor:
   case nir_intrinsic_image_deref_atomic_exchange:
   case nir_intrinsic_image_deref_atomic_comp_swap:
      return true;

   default:
      return false;
   }
}

static bool
add_src_instr_to_worklist(nir_src *src, void *wl)
{
   if (!src->is_ssa)
      return false;

   nir_instr_worklist_push_tail(wl, src->ssa->parent_instr);
   return true;
}

/** Try to mark a discard instruction for moving
 *
 * This function does two things.  One is that it searches through the
 * dependency chain to see if this discard is an instruction that we can move
 * up to the top.  Second, if the discard is one we can move, it adds the
 * discard and its dependencies to discards_and_deps.
 */
static void
try_move_discard(nir_intrinsic_instr *discard,
                 struct set *discards_and_deps)
{
   /* We require the discard to be in the top level of control flow.  We
    * could, in theory, move discards that are inside ifs or loops but that
    * would be a lot more work.
    */
   if (discard->instr.block->cf_node.parent->type != nir_cf_node_function)
      return;

   /* Build the set of all instructions discard depends on.  We'll union this
    * one later with discard_and_deps if the discard is movable.
    */
   struct set *instrs = _mesa_set_create(NULL, _mesa_hash_pointer,
                                         _mesa_key_pointer_equal);
   nir_instr_worklist *work = nir_instr_worklist_create();

   _mesa_set_add(instrs, &discard->instr);
   add_src_instr_to_worklist(&discard->src[0], work);

   bool can_move_discard = true;
   nir_foreach_instr_in_worklist(instr, work) {
      /* Don't process an instruction twice */
      if (_mesa_set_search(instrs, instr))
         continue;

      _mesa_set_add(instrs, instr);

      /* Phi instructions can't be moved at all.  Also, if we're dependent on
       * a phi then we are dependent on some other bit of control flow and
       * it's hard to figure out the proper condition.
       */
      if (instr->type == nir_instr_type_phi) {
         can_move_discard = false;
         break;
      }

      if (instr->type == nir_instr_type_intrinsic) {
         nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
         if (intrin->intrinsic == nir_intrinsic_load_deref) {
            nir_deref_instr *deref = nir_src_as_deref(intrin->src[0]);
            if (!nir_variable_mode_is_read_only(deref->mode)) {
               can_move_discard = false;
               break;
            }
         } else if (!(nir_intrinsic_infos[intrin->intrinsic].flags &
                      NIR_INTRINSIC_CAN_REORDER)) {
            can_move_discard = false;
            break;
         }
      }

      if (!nir_foreach_src(instr, add_src_instr_to_worklist, work)) {
         can_move_discard = false;
         break;
      }
   }

   if (can_move_discard) {
      struct set_entry *entry;
      set_foreach(instrs, entry)
         _mesa_set_add(discards_and_deps, entry->key);
   }

   nir_instr_worklist_destroy(work);
   _mesa_set_destroy(instrs, NULL);
}

static bool
opt_move_discards_to_top_impl(nir_function_impl *impl)
{
   const nir_shader_compiler_options *options = impl->function->shader->options;

   /* This optimization only operates on discard_if.  Run the discard_if
    * optimization (it's very cheap if it doesn't make progress) so that we
    * have some hope of move_discards_to_top making progress.
    */
   bool progress = opt_discard_if_impl(impl);

   struct set *move_instrs = _mesa_set_create(NULL, _mesa_hash_pointer,
                                              _mesa_key_pointer_equal);

   /* Walk through the instructions and look for a discard that we can move
    * to the top of the program.  If we hit any operation along the way that
    * we cannot safely move a discard above, break out of the loop and stop
    * trying to move any more discards.
    */
   nir_foreach_block(block, impl) {
      nir_foreach_instr_safe(instr, block) {
         switch (instr->type) {
         case nir_instr_type_alu: {
            nir_alu_instr *alu = nir_instr_as_alu(instr);
            if (nir_op_is_derivative(alu->op) &&
                !options->derivatives_safe_after_discard)
               goto break_all;
            continue;
         }

         case nir_instr_type_deref:
         case nir_instr_type_load_const:
         case nir_instr_type_ssa_undef:
         case nir_instr_type_phi:
            /* These are all safe */
            continue;

         case nir_instr_type_call:
            /* We don't know what the function will do */
            goto break_all;

         case nir_instr_type_tex: {
            nir_tex_instr *tex = nir_instr_as_tex(instr);
            if (nir_texop_implies_derivative(tex->op) &&
                !options->derivatives_safe_after_discard)
               goto break_all;
            continue;
         }

         case nir_instr_type_intrinsic: {
            nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
            if (nir_intrinsic_writes_external_memory(intrin->intrinsic))
               goto break_all;

            if (intrin->intrinsic == nir_intrinsic_discard_if)
               try_move_discard(intrin, move_instrs);
            continue;
         }

         case nir_instr_type_jump: {
            nir_jump_instr *jump = nir_instr_as_jump(instr);
            /* A return would cause the discard to not get executed */
            if (jump->type == nir_jump_return)
               goto break_all;
            continue;
         }

         case nir_instr_type_parallel_copy:
            unreachable("Unhanded instruction type");
         }
      }
   }
break_all:

   if (move_instrs->entries) {
      /* Walk the list of instructions and move the discard and everything it
       * depends on to the top.  We walk the instruction list here because it
       * ensures that everything stays in its original order.  This provides
       * stability for the algorithm and ensures that we don't accidentally
       * get dependencies out-of-order.
       */
      nir_cursor cursor = nir_before_block(nir_start_block(impl));
      nir_foreach_block(block, impl) {
         nir_foreach_instr_safe(instr, block) {
            if (_mesa_set_search(move_instrs, instr)) {
               nir_instr_move(cursor, instr);
               cursor = nir_after_instr(instr);
            }
         }
      }
      progress = true;
   }

   _mesa_set_destroy(move_instrs, NULL);

   if (progress) {
      nir_metadata_preserve(impl, nir_metadata_block_index |
                                  nir_metadata_dominance);
   }

   return progress;
}

bool
nir_opt_move_discards_to_top(nir_shader *shader)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   bool progress = false;

   nir_foreach_function(function, shader) {
      if (function->impl &&
          opt_move_discards_to_top_impl(function->impl))
         progress = true;
   }

   return progress;
}
