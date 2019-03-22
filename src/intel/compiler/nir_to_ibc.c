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

#include "ibc.h"
#include "ibc_builder.h"

struct nir_to_ibc_state {
   ibc_builder b;

   const ibc_reg **ssa_to_reg;
};

static enum ibc_type
ibc_type_for_nir(nir_alu_type ntype)
{
   enum ibc_type stype;
   switch (nir_alu_type_get_base_type(ntype)) {
   case nir_type_int:   stype = IBC_TYPE_INT;   break;
   case nir_type_uint:  stype = IBC_TYPE_UINT;  break;
   case nir_type_float: stype = IBC_TYPE_FLOAT; break;
   default:
      unreachable("Unsupported base type");
   }

   return stype | nir_alu_type_get_type_size(ntype);
}

static void
nti_emit_alu(struct nir_to_ibc_state *nti,
             const nir_alu_instr *instr)
{
   ibc_builder *b = &nti->b;

   ibc_reg_ref src[4];
   for (unsigned i = 0; i < nir_op_infos[instr->op].num_inputs; i++) {
      assert(instr->src[i].src.is_ssa);
      nir_ssa_def *ssa_src = instr->src[i].src.ssa;

      /* TODO */
      assert(ssa_src->num_components == 1);
      assert(!instr->src[i].abs);
      assert(!instr->src[i].negate);

      src[i] = ibc_typed_ref(nti->ssa_to_reg[ssa_src->index],
                             ibc_type_for_nir(nir_op_infos[instr->op].input_types[i]));
   }

   assert(instr->dest.dest.is_ssa);

   /* TODO */
   assert(instr->dest.dest.ssa.num_components == 1);
   assert(!instr->dest.saturate);

   ibc_reg_ref dest = { .file = IBC_REG_FILE_NONE, };
   enum ibc_type dest_type =
      ibc_type_for_nir(nir_op_infos[instr->op].output_type) |
      instr->dest.dest.ssa.bit_size;
   switch (instr->op) {
   case nir_op_iadd:
   case nir_op_fadd:
      dest = ibc_ADD(b, dest_type, src[0], src[1]);
      break;
   case nir_op_iand:
      dest = ibc_AND(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ishl:
      dest = ibc_SHL(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ishr:
   case nir_op_ushr:
      dest = ibc_SHR(b, dest_type, src[0], src[1]);
      break;
   default:
      unreachable("Unhandled NIR ALU opcode");
   }

   assert(dest.file == IBC_REG_FILE_LOGICAL);
   assert(dest.reg->logical.bit_size == instr->dest.dest.ssa.bit_size);
   assert(dest.reg->logical.num_comps == instr->dest.dest.ssa.num_components);
   nti->ssa_to_reg[instr->dest.dest.ssa.index] = dest.reg;
}

static void
nti_emit_intrinsic(struct nir_to_ibc_state *nti,
                   const nir_intrinsic_instr *instr)
{
   ibc_builder *b = &nti->b;

   ibc_reg_ref dest = { .file = IBC_REG_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_subgroup_id:
      /* Assume that the subgroup ID is in g1.0
       *
       * TODO: Make this more dynamic.
       */
      dest = ibc_read_hw_grf(b, 1, 0, IBC_TYPE_UD, 0);
      break;

   case nir_intrinsic_load_subgroup_invocation: {
      assert(b->simd_width == 8);
      assert(b->simd_group == 0);
      dest = ibc_MOV(b, IBC_TYPE_UD,
                     ibc_MOV(b, IBC_TYPE_UW, ibc_imm_v(0x76543210)));
      break;
   }

   case nir_intrinsic_store_ssbo: {
      ibc_intrinsic_instr *store =
         ibc_intrinsic_instr_create(b->shader,
                                    IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE,
                                    b->simd_width, b->simd_group, 3);
      store->src[0] = (ibc_intrinsic_src) {
         .file = IBC_REG_FILE_IMM,
         .imm = nir_src_as_uint(instr->src[1]),
      };

      assert(instr->src[2].is_ssa);
      store->src[1] = (ibc_intrinsic_src) {
         .file = IBC_REG_FILE_LOGICAL,
         .reg = {
            .reg = nti->ssa_to_reg[instr->src[2].ssa->index],
         },
      };

      assert(instr->src[0].is_ssa);
      store->src[2] = (ibc_intrinsic_src) {
         .file = IBC_REG_FILE_LOGICAL,
         .reg = {
            .reg = nti->ssa_to_reg[instr->src[0].ssa->index],
         },
      };
      store->const_index[0] = instr->src[0].ssa->num_components;

      ibc_builder_insert_instr(b, &store->instr);
      break;
   }

   default:
      unreachable("Unhandled NIR intrinsic");
   }

   if (nir_intrinsic_infos[instr->intrinsic].has_dest) {
      assert(dest.file == IBC_REG_FILE_LOGICAL);
      nti->ssa_to_reg[instr->dest.ssa.index] = dest.reg;
   } else {
      assert(dest.file == IBC_REG_FILE_NONE);
   }
}

static void
nti_emit_load_const(struct nir_to_ibc_state *nti,
                    const nir_load_const_instr *instr)
{
   ibc_builder *b = &nti->b;

   /* TODO */
   assert(instr->def.num_components == 1);

   assert(instr->def.bit_size >= 8);
   enum ibc_type type = IBC_TYPE_UINT | instr->def.bit_size;
   ibc_reg_ref imm_src = {
      .file = IBC_REG_FILE_IMM,
      .type = type,
   };
   switch (instr->def.bit_size) {
   case 8:
      /* 8-bit immediates aren't a thing */
      imm_src.type = IBC_TYPE_UW;
      *(uint16_t *)imm_src.imm = instr->value[0].u8;
      break;
   case 16:
      *(uint16_t *)imm_src.imm = instr->value[0].u16;
      break;
   case 32:
      *(uint32_t *)imm_src.imm = instr->value[0].u32;
      break;
   case 64:
      *(uint64_t *)imm_src.imm = instr->value[0].u64;
      break;
   default:
      unreachable("Invalid bit size");
   }

   ibc_builder_push_scalar(b);
   nti->ssa_to_reg[instr->def.index] = ibc_MOV(b, type, imm_src).reg;
   ibc_builder_pop(b);
}

static void
nti_emit_cs_thread_terminate(struct nir_to_ibc_state *nti)
{
   ibc_builder *b = &nti->b;

   ibc_reg_ref g0 = {
      .file = IBC_REG_FILE_HW_GRF,
      .type = IBC_TYPE_UD,
      .reg = ibc_hw_grf_reg_create(b->shader, 0, 32, 32),
      .stride = ibc_type_byte_size(IBC_TYPE_UD),
   };
   ibc_reg_ref tmp = {
      .file = IBC_REG_FILE_HW_GRF,
      .type = IBC_TYPE_UD,
      .reg = ibc_hw_grf_reg_create(b->shader, IBC_HW_GRF_REG_UNASSIGNED, 32, 32),
      .stride = ibc_type_byte_size(IBC_TYPE_UD),
   };

   ibc_builder_push_we_all(b, 8);
   ibc_build_alu(b, IBC_ALU_OP_MOV, tmp, &g0, 1);
   ibc_builder_pop(b);

   ibc_send_instr *send = ibc_send_instr_create(b->shader, 8, 0);
   send->instr.we_all = true;
   send->sfid = BRW_SFID_THREAD_SPAWNER;
   send->desc_imm = brw_ts_eot_desc(b->shader->devinfo);
   send->eot = true;

   send->payload[0] = tmp;
   send->mlen = 1;

   ibc_builder_insert_instr(b, &send->instr);
}

ibc_shader *
nir_to_ibc(const nir_shader *nir, void *mem_ctx,
           unsigned dispatch_size,
           const struct gen_device_info *devinfo)
{
   struct nir_to_ibc_state nti;
   ibc_builder_init(&nti.b, ibc_shader_create(mem_ctx, devinfo),
                    dispatch_size);

   nir_function_impl *impl = nir_shader_get_entrypoint((nir_shader *)nir);

   nti.ssa_to_reg = ralloc_array(mem_ctx, const ibc_reg *, impl->ssa_alloc);

   nir_foreach_block(block, impl) {
      nir_foreach_instr(instr, block) {
         assert(nti.b._group_stack_size == 0);
         switch (instr->type) {
         case nir_instr_type_alu:
            nti_emit_alu(&nti, nir_instr_as_alu(instr));
            break;
         case nir_instr_type_intrinsic:
            nti_emit_intrinsic(&nti, nir_instr_as_intrinsic(instr));
            break;
         case nir_instr_type_load_const:
            nti_emit_load_const(&nti, nir_instr_as_load_const(instr));
            break;
         default:
            unreachable("Unsupported instruction type");
         }
      }
   }

   if (nir->info.stage == MESA_SHADER_COMPUTE)
      nti_emit_cs_thread_terminate(&nti);

   return nti.b.shader;
}
