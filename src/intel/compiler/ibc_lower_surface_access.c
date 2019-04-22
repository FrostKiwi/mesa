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
#include "ibc_builder.h"

#include "brw_eu.h"

bool
ibc_lower_surface_access(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader, 32);

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr_safe(instr, block) {
         if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
            continue;

         ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);

         uint32_t sfid, desc;
         switch (intrin->op) {
         case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:
            sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
            desc = brw_dp_untyped_surface_rw_desc(shader->devinfo,
                                                  instr->simd_width,
                                                  intrin->src[2].num_comps,
                                                  true    /* write */);
            break;
         default:
            continue;
         }

         b.cursor = ibc_before_instr(instr);
         assert(b._group_stack_size == 0);
         ibc_builder_push_group(&b, instr->simd_group, instr->simd_width);

         ibc_send_instr *send = ibc_send_instr_create(shader,
                                                      instr->simd_group,
                                                      instr->simd_width);
         send->has_side_effects = intrin->has_side_effects;

         send->sfid = sfid;
         send->desc_imm = desc;
         assert(intrin->src[0].ref.file == IBC_REG_FILE_IMM);
         assert(intrin->src[0].ref.type == IBC_TYPE_UD);
         send->desc_imm |= *(uint32_t *)intrin->src[0].ref.imm;

         send->dest = intrin->dest;
         send->rlen = intrin->num_dest_comps * instr->simd_width / 8;

         send->payload[0] = ibc_MOV_raw(&b, intrin->src[1].ref);
         send->mlen = instr->simd_width / 8;

         send->payload[1] = ibc_MOV_raw(&b, intrin->src[2].ref);
         send->ex_mlen = intrin->src[2].num_comps * instr->simd_width / 8;

         ibc_builder_insert_instr(&b, &send->instr);
         ibc_builder_pop(&b);

         ibc_instr_remove(&intrin->instr);
         progress = true;
      }
   }

   return progress;
}
