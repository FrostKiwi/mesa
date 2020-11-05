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

static bool
ibc_lower_alu_restrictions(ibc_builder *b, ibc_alu_instr *alu)
{
   const struct gen_device_info *devinfo = b->shader->devinfo;
   bool progress = false;

   if (devinfo->gen >= 11) {
      /* Byte sized operands are not supported for src1/2 on Gen11+ */
      for (unsigned i = 1; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
         if (ibc_type_bit_size(alu->src[i].ref.type) == 8) {
            b->cursor = ibc_before_instr(&alu->instr);
            enum ibc_type word_type =
               ibc_type_base_type(alu->src[i].ref.type) | IBC_TYPE_16_BIT;

            ibc_builder_push_instr_group(b, &alu->instr);
            ibc_ref tmp = ibc_MOV(b, word_type, alu->src[i].ref);
            ibc_instr_set_ref(&alu->instr, &alu->src[i].ref, tmp);
            ibc_builder_pop(b);

            progress = true;
         }
      }
   }

   return progress;
}

bool
ibc_lower_restrictions(ibc_shader *shader)
{
   bool progress = false;

   ibc_assign_logical_reg_strides(shader);

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU:
         progress |= ibc_lower_alu_restrictions(&b, ibc_instr_as_alu(instr));
         break;
      default:
         break;
      }
   }

   return progress;
}
