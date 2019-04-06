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
rewrite_logical_flag_refs_to_ud(ibc_reg_ref *ref,
                                UNUSED int8_t num_comps,
                                UNUSED uint8_t simd_group,
                                UNUSED uint8_t simd_width,
                                UNUSED void *_state)
{
   if (ref->file == IBC_REG_FILE_LOGICAL &&
       ref->type == IBC_TYPE_FLAG)
      ref->type = IBC_TYPE_UD;

   return true;
}

void
ibc_assign_and_lower_flags(ibc_shader *shader)
{
   /* Just blindly turn all logical flag regs into 32-bit regs */
   ibc_foreach_reg(reg, shader) {
      if (reg->file == IBC_REG_FILE_LOGICAL &&
          reg->logical.bit_size == 1)
         reg->logical.bit_size = 32;
   }

   ibc_reg_ref flag0 = {
      .file = IBC_REG_FILE_FLAG,
      .reg = ibc_flag_reg_create(shader, 0, 32),
      .type = IBC_TYPE_FLAG,
   };

   ibc_builder b;
   ibc_builder_init(&b, shader, 32);

   ibc_foreach_block(block, shader) {
      ibc_foreach_instr_safe(instr, block) {
         ibc_instr_foreach_read(instr, rewrite_logical_flag_refs_to_ud, NULL);
         ibc_instr_foreach_write(instr, rewrite_logical_flag_refs_to_ud, NULL);

         if (instr->flag.file == IBC_REG_FILE_NONE)
            continue;

         assert(instr->flag.file == IBC_REG_FILE_LOGICAL);

         if (instr->flag.write_instr) {
            /* It's a write.  Right now, this implies that it's a CMP and we
             * can just make it write to the logical reg as its destination.
             */
            ibc_alu_instr *cmp = ibc_instr_as_alu(instr);
            assert(cmp->op == IBC_ALU_OP_CMP);

            /* TODO */
            assert(ibc_type_bit_size(cmp->src[0].ref.type) == 32);

            ibc_reg_ref cmp_dest = cmp->instr.flag;
            cmp_dest.type = cmp->src[0].ref.type;
            ibc_instr_set_write_ref(&cmp->instr, &cmp->dest, cmp_dest);

            /* It still has to write some flag value */
            ibc_instr_set_write_ref(&cmp->instr, &cmp->instr.flag, flag0);
         } else {
            /* It's a read.  Emit a MOV to copy the value to the flag. */
            b.cursor = ibc_before_instr(instr);
            ibc_builder_push_group(&b, instr->simd_group, instr->simd_width);
            ibc_build_alu(&b, IBC_ALU_OP_MOV, ibc_null(IBC_TYPE_UD),
                          flag0, BRW_CONDITIONAL_NZ, &instr->flag, 1);
            instr->flag = flag0;
            ibc_builder_pop(&b);
         }
      }
   }
}
