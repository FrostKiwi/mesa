/*
 * Copyright 2020 Intel Corporation
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and/or sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice (including the next
 *  paragraph) shall be included in all copies or substantial portions of the
 *  Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 *  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 *  IN THE SOFTWARE.
 */

#include "isl.h"

enum isl_aux_state
isl_aux_state_transition(enum isl_aux_state initial_state,
                         enum isl_aux_usage usage,
                         enum isl_aux_op op,
                         bool full_surface)
{
   switch (op) {
   case ISL_AUX_OP_NONE:
      return initial_state;

   case ISL_AUX_OP_DRAW:
      switch (usage) {
      case ISL_AUX_USAGE_NONE:
         switch (initial_state) {
         case ISL_AUX_STATE_CLEAR:
         case ISL_AUX_STATE_PARTIAL_CLEAR:
         case ISL_AUX_STATE_COMPRESSED_CLEAR:
         case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
            if (full_surface)
               return ISL_AUX_STATE_AUX_INVALID;
            unreachable("Cannot render without AUX enabled to surfaces which "
                        "are fast-cleared or compressed");

         case ISL_AUX_STATE_RESOLVED:
         case ISL_AUX_STATE_AUX_INVALID:
            return ISL_AUX_STATE_AUX_INVALID;

         case ISL_AUX_STATE_PASS_THROUGH:
            return ISL_AUX_STATE_PASS_THROUGH;
         }
         unreachable("Invalid isl_aux_state");

      case ISL_AUX_USAGE_CCS_D:
         /* This one is special because it has no compression */
         switch (initial_state) {
         case ISL_AUX_STATE_CLEAR:
         case ISL_AUX_STATE_PARTIAL_CLEAR:
            if (full_surface)
               return ISL_AUX_STATE_PASS_THROUGH;
            else
               return ISL_AUX_STATE_PARTIAL_CLEAR;

         case ISL_AUX_STATE_COMPRESSED_CLEAR:
         case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
            unreachable("Cannot draw with CCS_D to a compressed surface");

         case ISL_AUX_STATE_RESOLVED:
            unreachable("CCS_D does not have a resolved state; resolves "
                        "take it directly to pass-through");

         case ISL_AUX_STATE_PASS_THROUGH:
            return ISL_AUX_STATE_PASS_THROUGH;

         case ISL_AUX_STATE_AUX_INVALID:
            unreachable("Drawing to a surface in AUX_INVALID with CCS_D "
                        "results in an only partially valid surface");
         }
         unreachable("Invalid isl_aux_state");

      default:
         /* All other ISL_AUX_USAGE_* support compression */
         assert(isl_aux_usage_supports_compression(usage));
         switch (initial_state) {
         case ISL_AUX_STATE_CLEAR:
         case ISL_AUX_STATE_PARTIAL_CLEAR:
         case ISL_AUX_STATE_COMPRESSED_CLEAR:
            if (full_surface)
               return ISL_AUX_STATE_COMPRESSED_NO_CLEAR;
            else
               return ISL_AUX_STATE_COMPRESSED_CLEAR;

         case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
         case ISL_AUX_STATE_RESOLVED:
         case ISL_AUX_STATE_PASS_THROUGH:
            return ISL_AUX_STATE_COMPRESSED_NO_CLEAR;

         case ISL_AUX_STATE_AUX_INVALID:
            unreachable("Drawing to a surface in AUX_INVALID with "
                        "compression has undefined results and may even "
                        "hang the GPU.");
         }
         unreachable("Invalid isl_aux_state");
      }
      unreachable("Invalid isl_aux_usage");

   case ISL_AUX_OP_FAST_CLEAR:
      assert(isl_aux_usage_supports_fast_clear(usage));

      /* Fast clears initialize the aux buffer so a full surface clear gets
       * us cleanly into the clear state regardless of the initial state.
       */
      if (full_surface)
         return ISL_AUX_STATE_CLEAR;

      switch (initial_state) {
      case ISL_AUX_STATE_CLEAR:
         return ISL_AUX_STATE_CLEAR;

      case ISL_AUX_STATE_PARTIAL_CLEAR:
      case ISL_AUX_STATE_PASS_THROUGH:
         return ISL_AUX_STATE_PARTIAL_CLEAR;

      case ISL_AUX_STATE_COMPRESSED_CLEAR:
      case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
      case ISL_AUX_STATE_RESOLVED:
         return ISL_AUX_STATE_COMPRESSED_CLEAR;

      case ISL_AUX_STATE_AUX_INVALID:
         unreachable("Partially fast-clearing a surface in AUX_INVALID "
                     "results in an only partially valid surface");
      }

   case ISL_AUX_OP_FULL_RESOLVE:
      assert(full_surface);
      assert(initial_state != ISL_AUX_STATE_AUX_INVALID);
      switch (usage) {
      case ISL_AUX_USAGE_NONE:
         unreachable("Resolves cannot be done with ISL_AUX_USAGE_NONE");

      case ISL_AUX_USAGE_HIZ:
      case ISL_AUX_USAGE_HIZ_CCS:
         return ISL_AUX_STATE_RESOLVED;

      case ISL_AUX_USAGE_MCS:
      case ISL_AUX_USAGE_MCS_CCS:
         unreachable("MCS has no full resolve operation");

      case ISL_AUX_USAGE_CCS_D:
      case ISL_AUX_USAGE_CCS_E:
      case ISL_AUX_USAGE_MC:
         return ISL_AUX_STATE_PASS_THROUGH;
      }
      unreachable("Invalid isl_aux_usage");

   case ISL_AUX_OP_PARTIAL_RESOLVE:
      assert(full_surface);
      assert(initial_state != ISL_AUX_STATE_AUX_INVALID);
      switch (usage) {
      case ISL_AUX_USAGE_NONE:
         unreachable("Resolves cannot be done with ISL_AUX_USAGE_NONE");

      case ISL_AUX_USAGE_HIZ:
      case ISL_AUX_USAGE_HIZ_CCS:
      case ISL_AUX_USAGE_CCS_D:
         unreachable("HiZ and CCS_D have no partial resolve operation");

      case ISL_AUX_USAGE_MCS:
      case ISL_AUX_USAGE_CCS_E:
      case ISL_AUX_USAGE_MC:
      case ISL_AUX_USAGE_MCS_CCS:
         switch (initial_state) {
         case ISL_AUX_STATE_CLEAR:
         case ISL_AUX_STATE_PARTIAL_CLEAR:
         case ISL_AUX_STATE_COMPRESSED_CLEAR:
            return ISL_AUX_STATE_COMPRESSED_NO_CLEAR;

         case ISL_AUX_STATE_RESOLVED:
         case ISL_AUX_STATE_PASS_THROUGH:
         case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
            /* Resolve does nothing in this state.  Why did we do it? */
            return initial_state;

         case ISL_AUX_STATE_AUX_INVALID:
            unreachable("Resolving a surface in AUX_INVALID has undefined "
                        "behavior and may even hang the GPU.");
         }
         unreachable("Invalid isl_aux_state");
      }
      unreachable("Invalid isl_aux_usage");

   case ISL_AUX_OP_AMBIGUATE:
      assert(full_surface);
      assert(usage != ISL_AUX_USAGE_NONE);
      /* We could go out of our way to check that the main surface is valid
       * and that would probably catch some bugs.  However, we also use
       * AMBIGUATE to initialize surfaces so we want to allow it even if the
       * main surface has no valid data.
       */
      return ISL_AUX_STATE_PASS_THROUGH;
   }

   unreachable("Invalid isl_aux_op");
}
