/*
 * Copyright © 2010 - 2015 Intel Corporation
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

#include "brw_compiler.h"
#include "brw_eu.h"

#define BRW_ACCUM_DISABLE_BIT    (1u << 1)
#define BRW_MASTER_EXCEPTION_BIT (1u << 31)

static void
a64_dual_oword_write(struct brw_codegen *p,
                     struct brw_reg src0, unsigned mlen,
                     struct brw_reg src1, unsigned ex_mlen,
                     unsigned num_regs)
{
   const uint32_t desc =
      brw_dp_desc(p->devinfo, GEN8_BTI_STATELESS_NON_COHERENT,
                  GEN9_DATAPORT_DC_PORT1_A64_OWORD_BLOCK_WRITE,
                  BRW_DATAPORT_OWORD_BLOCK_OWORDS(num_regs * 2)) |
      brw_message_desc(p->devinfo, mlen, 0 /* rlen */, 1);

   const uint32_t ex_desc = brw_message_ex_desc(p->devinfo, ex_mlen);

   brw_push_insn_state(p);
   brw_set_default_mask_control(p, BRW_MASK_DISABLE);
   brw_set_default_exec_size(p, num_regs > 1 ? BRW_EXECUTE_16 : BRW_EXECUTE_8);

   brw_send_indirect_split_message(p, HSW_SFID_DATAPORT_DATA_CACHE_1,
                                   brw_null_reg(), src0, src1,
                                   brw_imm_ud(0), desc,
                                   brw_imm_ud(0), ex_desc,
                                   false);

   brw_pop_insn_state(p);
}

const unsigned *
brw_compile_sip(const struct brw_compiler *compiler,
                void *mem_ctx,
                uint64_t dump_addr,
                unsigned *final_assembly_size)
{
   const struct gen_device_info *devinfo = compiler->devinfo;

   struct brw_codegen *p = rzalloc(mem_ctx, struct brw_codegen);
   brw_init_codegen(devinfo, p, mem_ctx);
   p->automatic_exec_sizes = false;
   brw_set_default_mask_control(p, BRW_MASK_DISABLE);
   brw_set_default_exec_size(p, BRW_EXECUTE_1);

   /* Disable the accumulator */
   brw_AND(p, brw_cr0_reg(0), brw_cr0_reg(0),
           brw_imm_ud(~BRW_ACCUM_DISABLE_BIT));
   brw_inst_set_thread_control(devinfo, brw_last_inst, BRW_THREAD_SWITCH);

   /* We use the high address registers to save off important stuff.  We leave
    * a0.0-3 alone in case they contain indirect message descriptors from an
    * interesting SEND instruction.
    */
   struct brw_reg f0_0_save = brw_address_reg(8);
   struct brw_reg g4_0_save = vec2(retype(brw_address_reg(4), BRW_REGISTER_TYPE_UD));

   /* Save off f0 */
   brw_MOV(p, f0_0_save, brw_flag_reg(0, 0));

   /* Clear the GRF scoreboard */
   brw_MOV(p, brw_flag_reg(0, 0), brw_imm_uw(0));
   brw_push_insn_state(p);
   brw_set_default_exec_size(p, BRW_EXECUTE_16);
   brw_set_default_predicate_control(p, BRW_PREDICATE_NORMAL);
   brw_set_default_flag_reg(p, 0, 0);
   for (unsigned grf = 0; grf < 128; grf += 2) {
      brw_MOV(p, retype(brw_vec8_grf(grf, 0), BRW_REGISTER_TYPE_UD),
                 brw_imm_ud(0));
      brw_inst_set_no_dd_check(p->devinfo, brw_last_inst, true);
   }
   brw_pop_insn_state(p);

   /* Save off g4.0:uq */
   struct brw_reg g4 = retype(brw_vec8_grf(4, 0), BRW_REGISTER_TYPE_UD);
   brw_set_default_exec_size(p, BRW_EXECUTE_2);
   brw_MOV(p, g4_0_save, vec2(g4));

   /* Fill g4.0:uq with the  address */
   brw_set_default_exec_size(p, BRW_EXECUTE_1);
   brw_MOV(p, get_element_ud(g4, 1), brw_imm_ud(dump_addr >> 32));
   brw_SHR(p, g4, brw_sr0_reg(0), brw_imm_ud(24));
   brw_MUL(p, g4, g4, brw_imm_uw(sizeof(struct brw_sip_eu_dump)));
   brw_ADD(p, g4, g4, brw_imm_ud(dump_addr));

   /* Write out g0-3 */
   a64_dual_oword_write(p, g4, 1, brw_vec8_grf(0, 0), 4, 4);

   /* Now use g2 as the header */
   brw_set_default_exec_size(p, BRW_EXECUTE_1);
   struct brw_reg g2 = retype(brw_vec8_grf(2, 0), BRW_REGISTER_TYPE_UD);
   brw_MOV(p, get_element_ud(g2, 1), get_element_ud(g4, 1));
   brw_ADD(p, g2, g4, brw_imm_ud(4 * REG_SIZE));

   /* Restore g4.0:uq */
   brw_set_default_exec_size(p, BRW_EXECUTE_2);
   brw_MOV(p, vec2(g4), g4_0_save);

   /* Dump out the rest of the GRF */
   brw_set_default_exec_size(p, BRW_EXECUTE_1);
   for (unsigned grf = 4; grf < 128; grf += 4) {
      a64_dual_oword_write(p, g2, 1, brw_vec8_grf(grf, 0), 4, 4);
      brw_ADD(p, g2, g2, brw_imm_ud(4 * REG_SIZE));
   }

   /* Dump out a bunch of state registers */
   struct brw_reg g3 = retype(brw_vec8_grf(4, 0), BRW_REGISTER_TYPE_UD);
   brw_set_default_exec_size(p, BRW_EXECUTE_4);
   brw_MOV(p, vec4(retype(g3, BRW_REGISTER_TYPE_UW)),
              vec4(brw_address_reg(0)));
   brw_set_default_exec_size(p, BRW_EXECUTE_1);
   brw_MOV(p, get_element_ud(g3, 2),
              retype(brw_flag_reg(0, 0), BRW_REGISTER_TYPE_UD));
   brw_MOV(p, get_element_ud(g3, 3),
              retype(brw_flag_reg(1, 0), BRW_REGISTER_TYPE_UD));
   brw_MOV(p, get_element_ud(g3, 4), brw_cr0_reg(0));
   brw_MOV(p, get_element_ud(g3, 5), brw_cr0_reg(1));
   brw_MOV(p, get_element_ud(g3, 6), brw_cr0_reg(2));
   brw_MOV(p, get_element_ud(g3, 7), brw_imm_ud(0)); /* pad */
   a64_dual_oword_write(p, g2, 1, g3, 1, 1);

   /* Clear Master Exception State bit */
   brw_AND(p, brw_cr0_reg(0), brw_cr0_reg(0),
           brw_imm_ud(~BRW_MASTER_EXCEPTION_BIT));
   brw_NOP(p);

   const unsigned *program = brw_get_program(p, final_assembly_size);

   if (1) {
      fprintf(stderr, "clip:\n");
      brw_disassemble(compiler->devinfo,
                      program, 0, *final_assembly_size, stderr);
      fprintf(stderr, "\n");
   }

   return program;
}
