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

bool
ibc_lower_overlapping_send_payloads(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_SEND)
         continue;

      ibc_send_instr *send = ibc_instr_as_send(instr);
      if (send->ex_mlen == 0)
         continue;

      assert(send->payload[1].file == IBC_FILE_LOGICAL ||
             send->payload[1].file == IBC_FILE_HW_GRF);

      if (send->payload[0].file != send->payload[1].file ||
          send->payload[0].reg != send->payload[1].reg)
         continue;

      b.cursor = ibc_before_instr(&send->instr);

      /* If we're going to make a copy (that hasn't been decided yet), we
       * should copy whichever payload is shorter.
       */
      ibc_ref *copy_ref = send->mlen < send->ex_mlen ?
                          &send->payload[0] : &send->payload[1];
      unsigned copy_len = send->mlen < send->ex_mlen ?
                          send->mlen : send->ex_mlen;

      switch (send->payload[0].file) {
      case IBC_FILE_LOGICAL: {
         assert(copy_ref->reg->logical.bit_size >= 8);
         unsigned comp_size_B =
            (copy_ref->reg->logical.bit_size / 8) * send->instr.simd_width;
         assert(comp_size_B % 32 == 0);
         unsigned comp_size_regs = comp_size_B / 32;
         assert(copy_len % comp_size_regs == 0);
         unsigned copy_comps = copy_len / comp_size_regs;

         if (send->payload[0].type == send->payload[1].type) {
            /* If they're not the same type, it's much harder to prove they
             * don't overlap.
             */
            assert(send->mlen % comp_size_regs == 0);
            assert(send->ex_mlen % comp_size_regs == 0);
            const unsigned comps = send->mlen / comp_size_regs;
            const unsigned ex_comps = send->ex_mlen / comp_size_regs;

            if (send->payload[0].logical.comp + comps <=
                send->payload[1].logical.comp)
               continue;

            if (send->payload[1].logical.comp + ex_comps <=
                send->payload[0].logical.comp)
               continue;
         }

         ibc_builder_push_group(&b, send->instr.simd_group,
                                    send->instr.simd_width);
         ibc_ref new_copy = ibc_builder_new_logical_reg(&b, copy_ref->type,
                                                        copy_comps);
         ibc_ref copy_src = *copy_ref;
         ibc_ref copy_dest = new_copy;

         for (unsigned i = 0; i < copy_comps; i++) {
            ibc_MOV_to(&b, copy_dest, copy_src);
            copy_src.logical.comp++;
            copy_dest.logical.comp++;
         }

         ibc_builder_pop(&b);

         *copy_ref = new_copy;
         progress = true;
         break;
      }

      case IBC_FILE_HW_GRF: {
         if (send->payload[0].hw_grf.byte + send->mlen <=
             send->payload[1].hw_grf.byte)
            continue;

         if (send->payload[1].hw_grf.byte + send->ex_mlen <=
             send->payload[0].hw_grf.byte)
            continue;

         ibc_reg *grf = ibc_hw_grf_reg_create(b.shader,
                                              copy_len * REG_SIZE, REG_SIZE);

         assert(copy_ref->hw_grf.byte % REG_SIZE == 0);
         ibc_ref copy_src = *copy_ref;
         copy_src.type = IBC_TYPE_UD;
         ibc_ref copy_dest = ibc_typed_ref(grf, IBC_TYPE_UD);

         while (copy_len > 0) {
            /* If the length is odd, make the first copy 8-wide */
            unsigned width = (copy_len & 1) ? 8 : 16;
            ibc_builder_push_we_all(&b, width);
            ibc_MOV_to(&b, copy_dest, copy_src);
            ibc_builder_pop(&b);
            copy_src.hw_grf.byte += width * ibc_type_byte_size(IBC_TYPE_UD);
            copy_dest.hw_grf.byte += width * ibc_type_byte_size(IBC_TYPE_UD);
            copy_len += (width * ibc_type_byte_size(IBC_TYPE_UD)) / REG_SIZE;
         }

         *copy_ref = ibc_typed_ref(grf, copy_ref->type);
         progress = true;
         break;
      }

      default:
         unreachable("Invalid register file for SEND payload");
      }
   }

   return progress;
}
