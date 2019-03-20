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

#include "sir.h"
#include "sir_builder.h"

#include "brw_eu.h"

bool
sir_lower_surface_access(sir_shader *shader)
{
   bool progress = false;

   sir_foreach_block(block, shader) {
      sir_foreach_instr_safe(instr, block) {
         if (instr->type != SIR_INSTR_TYPE_INTRINSIC)
            continue;

         sir_intrinsic_instr *intrin = sir_instr_as_intrinsic(instr);

         uint32_t sfid, desc;
         switch (intrin->op) {
         case SIR_INTRINSIC_OP_BTI_UNTYPED_WRITE:
            sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
            desc = brw_dp_untyped_surface_rw_desc(shader->devinfo,
                                                  instr->simd_width,
                                                  intrin->const_index[0],
                                                  true    /* write */);
            break;
         default:
            continue;
         }

         sir_send_instr *send = sir_send_instr_create(shader,
                                                      instr->simd_width,
                                                      instr->simd_group);
         send->has_side_effects = true;

         send->sfid = sfid;
         send->desc_imm = desc;
         assert(intrin->src[0].file == SIR_REG_FILE_IMM);
         send->desc_imm |= intrin->src[0].imm;

         send->dest = intrin->dest;
         send->rlen = 0; /* TODO */

         send->payload[0] = intrin->src[1].reg;
         send->mlen = instr->simd_width / 8;

         send->payload[1] = intrin->src[2].reg;
         send->mlen = intrin->const_index[0] * instr->simd_width / 8;

         list_add(&send->instr.link, &intrin->instr.link);
         list_del(&intrin->instr.link);
         progress = true;
      }
   }

   return progress;
}
