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

struct rewrite_accum_state {
   bool progress;

   uint32_t serial;

   ibc_instr *instr;
};

#ifndef NDEBUG
static bool
accum_ref_has_reg(ibc_ref *ref,
                  UNUSED int num_bytes,
                  UNUSED int num_comps,
                  UNUSED uint8_t simd_group,
                  UNUSED uint8_t simd_width,
                  UNUSED void *_state)
{
   return ref->file != IBC_FILE_ACCUM || ref->reg != NULL;
}
#endif

static bool
rewrite_accum_ref(ibc_ref *ref,
                  UNUSED int num_bytes,
                  UNUSED int num_comps,
                  UNUSED uint8_t simd_group,
                  UNUSED uint8_t simd_width,
                  void *_state)
{
   struct rewrite_accum_state *state = _state;

   if (ref->file != IBC_FILE_ACCUM || ref->reg == NULL)
      return true;

   if (ref->reg->index == 0) {
      ((ibc_reg *)ref->reg)->index = ++state->serial;
   } else {
      assert(ref->reg->index == state->serial);
   }

   ibc_ref new_ref = *ref;
   new_ref.reg = NULL;
   new_ref.accum.chan += ref->reg->accum.align_offset;

   ibc_instr_set_ref(state->instr, ref, new_ref);
   state->progress = true;

   return true;
}

bool
ibc_assign_and_lower_accums(ibc_shader *shader)
{
   struct rewrite_accum_state state = {
      .progress = false,
      .serial = 0,
   };

   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_FILE_ACCUM)
         reg->index = 0;
   }

   ibc_foreach_instr(instr, shader) {
      /* Check that we have registers before we rewrite.  We can't lower
       * virtual accumulators if there are physical accumulators in play.
       */
      assert(ibc_instr_foreach_read(instr, accum_ref_has_reg, NULL));
      assert(ibc_instr_foreach_write(instr, accum_ref_has_reg, NULL));

      state.instr = instr;
      ibc_instr_foreach_read(instr, rewrite_accum_ref, &state);
      ibc_instr_foreach_write(instr, rewrite_accum_ref, &state);
   }

   return state.progress;
}
