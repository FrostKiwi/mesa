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

#include "nir.h"

#include "sir.h"
#include "sir_builder.h"

struct nir_to_sir_state {
   sir_builder b;
};

static void
nts_emit_alu(struct nir_to_sir_state *nts,
             const nir_alu_instr *instr)
{
   sir_builder *b = &nts->b;

   switch (instr->op) {
   default:
      unreachable("Unhandled NIR ALU opcode");
   }
}

static void
nts_emit_intrinsic(struct nir_to_sir_state *nts,
                   const nir_intrinsic_instr *instr)
{
   sir_builder *b = &nts->b;

   switch (instr->intrinsic) {
   default:
      unreachable("Unhandled NIR intrinsic");
   }
}

sir_shader *
nir_to_sir(const nir_shader *nir, void *mem_ctx,
           unsigned dispatch_size,
           const struct gen_device_info *devinfo)
{
   struct nir_to_sir_state nts;
   sir_builder_init(&nts.b, sir_shader_create(mem_ctx, devinfo),
                    dispatch_size);

   nir_function_impl *impl = nir_shader_get_entrypoint((nir_shader *)nir);
   nir_foreach_block(block, impl) {
      nir_foreach_instr(instr, block) {
         switch (instr->type) {
         case nir_instr_type_alu:
            nts_emit_alu(&nts, nir_instr_as_alu(instr));
            break;
         case nir_instr_type_intrinsic:
            nts_emit_intrinsic(&nts, nir_instr_as_intrinsic(instr));
            break;
         default:
            unreachable("Unsupported instruction type");
         }
      }
   }

   return nts.b.shader;
}
