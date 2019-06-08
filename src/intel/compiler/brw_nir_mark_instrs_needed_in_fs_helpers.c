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

#include "brw_nir.h"

static void
mark_instr(nir_instr *instr, bool *progress)
{
   if (!(instr->pass_flags & BRW_NIR_NEEDED_IN_HELPERS)) {
      instr->pass_flags |= BRW_NIR_NEEDED_IN_HELPERS;
      *progress = true;
   }
}

static void
mark_src(nir_src *src, bool *progress)
{
   if (src->is_ssa) {
      mark_instr(src->ssa->parent_instr, progress);
   } else {
      nir_foreach_def(dest, src->reg.reg) {
         assert(!dest->is_ssa);
         mark_instr(dest->reg.parent_instr, progress);
      }
   }
}

static bool
mark_src_cb(nir_src *src, void *_state)
{
   mark_src(src, _state);
   return true;
}

static void
mark_instr_srcs_if_needed(nir_instr *instr, bool *progress)
{
   switch (instr->type) {
   case nir_instr_type_alu: {
      nir_alu_instr *alu = nir_instr_as_alu(instr);
      switch (alu->op) {
      case nir_op_fddx:
      case nir_op_fddy:
      case nir_op_fddx_fine:
      case nir_op_fddy_fine:
      case nir_op_fddx_coarse:
      case nir_op_fddy_coarse:
         mark_src(&alu->src[0].src, progress);
         break;
      default:
         if (instr->pass_flags & BRW_NIR_NEEDED_IN_HELPERS) {
            for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
               mark_src(&alu->src[i].src, progress);
         }
         break;
      }
      return;
   }

   case nir_instr_type_tex: {
      nir_tex_instr *tex = nir_instr_as_tex(instr);
      for (unsigned i = 0; i < tex->num_srcs; i++) {
         switch (tex->src[i].src_type) {
         case nir_tex_src_coord:
            if (nir_tex_instr_has_implicit_derivative(tex))
               mark_src(&tex->src[i].src, progress);
            break;

         case nir_tex_src_texture_deref:
         case nir_tex_src_sampler_deref:
            unreachable("These shouldn't exist");

         case nir_tex_src_texture_offset:
         case nir_tex_src_sampler_offset:
         case nir_tex_src_texture_handle:
         case nir_tex_src_sampler_handle:
            /* These need to be dynamically uniform and the back-end might get
             * messed up if they aren't correct for helpers.
             */
            mark_src(&tex->src[i].src, progress);

         default:
            /* Otherwise, only require helpers if the instruction is used in
             * such a way that we need helpers.
             */
            if (instr->pass_flags & BRW_NIR_NEEDED_IN_HELPERS)
               mark_src(&tex->src[i].src, progress);
            break;
         }
      }
      return;
   }

   case nir_instr_type_intrinsic:
      switch (nir_instr_as_intrinsic(instr)->intrinsic) {
      case nir_intrinsic_store_output:
         /* This definitely doesn't require helpers */
         break;

      default:
         /* Assume all other intrinsics do.  Yes, this is very conservative
          * but if we come across a case where this pass would help more if we
          * added an intrinsic, we can deal with that then.
          */
         nir_foreach_src(instr, mark_src_cb, progress);
      }
      return;

   case nir_instr_type_load_const:
   case nir_instr_type_jump:
   case nir_instr_type_ssa_undef:
      /* No sources */
      return;

   case nir_instr_type_deref:
   case nir_instr_type_call:
   case nir_instr_type_phi:
   case nir_instr_type_parallel_copy:
      unreachable("Unsupported this late in the compile");
   }

   unreachable("Unsupported instruction type");
}

void
brw_nir_mark_instrs_needed_in_fs_helpers(nir_shader *nir)
{
   assert(nir->info.stage == MESA_SHADER_FRAGMENT);

   nir_foreach_function(func, nir) {
      if (!func->impl)
         continue;

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr(instr, block)
            instr->pass_flags &= ~BRW_NIR_NEEDED_IN_HELPERS;
      }

      bool progress;
      do {
         progress = false;
         nir_foreach_block_reverse(block, func->impl) {
            /* We always need helpers for control flow */
            nir_if *following_if = nir_block_get_following_if(block);
            if (following_if)
               mark_src(&following_if->condition, &progress);

            nir_foreach_instr_reverse(instr, block)
               mark_instr_srcs_if_needed(instr, &progress);
         }
      } while (progress);
   }
}
