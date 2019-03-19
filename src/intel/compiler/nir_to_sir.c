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

   const sir_reg **ssa_to_reg;
};

static enum sir_type
sir_type_for_nir(nir_alu_type ntype)
{
   enum sir_type stype;
   switch (nir_alu_type_get_base_type(ntype)) {
   case nir_type_int:   stype = SIR_TYPE_INT;   break;
   case nir_type_uint:  stype = SIR_TYPE_UINT;  break;
   case nir_type_float: stype = SIR_TYPE_FLOAT; break;
   default:
      unreachable("Unsupported base type");
   }

   return stype | nir_alu_type_get_type_size(ntype);
}

static void
nts_emit_alu(struct nir_to_sir_state *nts,
             const nir_alu_instr *instr)
{
   sir_builder *b = &nts->b;

   sir_alu_src src[4];
   for (unsigned i = 0; i < nir_op_infos[instr->op].num_inputs; i++) {
      assert(instr->src[i].src.is_ssa);
      nir_ssa_def *ssa_src = instr->src[i].src.ssa;

      /* TODO */
      assert(ssa_src->num_components == 1);

      const sir_reg *reg = nts->ssa_to_reg[ssa_src->index];
      src[i] = (sir_alu_src) {
         .type = sir_type_for_nir(nir_op_infos[instr->op].input_types[i]) |
                 ssa_src->bit_size,
         .file = reg->file,
         .reg = {
            .reg = reg,
         },
      };
   }

   assert(instr->dest.dest.is_ssa);

   /* TODO */
   assert(instr->dest.dest.ssa.num_components == 1);

   sir_reg *dest;
   enum sir_type dest_type =
      sir_type_for_nir(nir_op_infos[instr->op].output_type) |
      instr->dest.dest.ssa.bit_size;
   switch (instr->op) {
   case nir_op_iadd:
   case nir_op_fadd:
      dest = sir_ADD(b, dest_type, src[0], src[1]);
      break;
   case nir_op_iand:
      dest = sir_AND(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ishl:
      dest = sir_SHL(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ishr:
   case nir_op_ushr:
      dest = sir_SHR(b, dest_type, src[0], src[1]);
      break;
   default:
      unreachable("Unhandled NIR ALU opcode");
   }

   assert(dest->file == SIR_REG_FILE_LOGICAL);
   assert(dest->logical.bit_size == instr->dest.dest.ssa.bit_size);
   assert(dest->logical.num_comps == instr->dest.dest.ssa.num_components);
   nts->ssa_to_reg[instr->dest.dest.ssa.index] = dest;
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

   nts.ssa_to_reg = ralloc_array(mem_ctx, const sir_reg *, impl->ssa_alloc);

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
