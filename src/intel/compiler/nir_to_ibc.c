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
   if (nir_alu_type_get_type_size(ntype) == 1)
      return IBC_TYPE_FLAG;

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

      nir_alu_type nir_src_type = nir_op_infos[instr->op].input_types[i];
      if (nir_alu_type_get_type_size(nir_src_type) == 0)
         nir_src_type |= instr->src[i].src.ssa->bit_size;
      src[i] = ibc_typed_ref(nti->ssa_to_reg[ssa_src->index],
                             ibc_type_for_nir(nir_src_type));
   }

   assert(instr->dest.dest.is_ssa);

   /* TODO */
   assert(instr->dest.dest.ssa.num_components == 1);
   assert(!instr->dest.saturate);

   nir_alu_type nir_dest_type = nir_op_infos[instr->op].output_type;
   if (nir_alu_type_get_type_size(nir_dest_type) == 0)
      nir_dest_type |= instr->dest.dest.ssa.bit_size;
   enum ibc_type dest_type = ibc_type_for_nir(nir_dest_type);

   ibc_reg_ref dest = { .file = IBC_REG_FILE_NONE, };
   switch (instr->op) {
   case nir_op_u2u8:
   case nir_op_u2u16:
   case nir_op_u2u32:
   case nir_op_i2i8:
   case nir_op_i2i16:
   case nir_op_i2i32:
      if (ibc_type_bit_size(dest_type) < ibc_type_bit_size(src[0].type)) {
         /* Integer down-casts can always be treated as just consuming the
          * bottom bytes of the register.
          */
         assert(src[0].logical.byte == 0);
         src[0].type = dest_type;
      }
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_i2f32:
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_unpack_32_2x16_split_x:
   case nir_op_unpack_32_2x16_split_y:
      src[0].type = dest_type;
      src[0].logical.byte = 2 * (instr->op == nir_op_unpack_32_2x16_split_y);
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_iadd:
   case nir_op_fadd:
      dest = ibc_ADD(b, dest_type, src[0], src[1]);
      break;
   case nir_op_iand:
      dest = ibc_AND(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ior:
      dest = ibc_OR(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ishl:
      dest = ibc_SHL(b, dest_type, src[0], src[1]);
      break;
   case nir_op_ishr:
   case nir_op_ushr:
      dest = ibc_SHR(b, dest_type, src[0], src[1]);
      break;

   case nir_op_ieq:
      assert(dest_type == IBC_TYPE_FLAG);
      dest = ibc_CMP(b, dest_type, BRW_CONDITIONAL_EQ, src[0], src[1]);
      break;

   case nir_op_bcsel: {
      assert(src[0].type == IBC_TYPE_FLAG);
      dest = ibc_SEL(b, dest_type, src[0], src[1], src[2]);
      break;
   }

   case nir_op_fsign: {
      /* AND(val, 0x80000000) gives the sign bit.
       *
       * Predicated OR ORs 1.0 (0x3f800000) with the sign bit if val is not
       * zero.
       */
      ibc_reg_ref is_not_zero = ibc_CMP(b, IBC_TYPE_FLAG, BRW_CONDITIONAL_NZ,
                                        src[0], ibc_imm_zero(src[0].type));

      ibc_reg_ref uint_src = src[0];
      uint_src.type = IBC_TYPE_UINT | ibc_type_bit_size(src[0].type);

      /* TODO */
      assert(ibc_type_bit_size(src[0].type) == 32);
      ibc_reg_ref high_bit = ibc_imm_ud(0x80000000u);
      ibc_reg_ref one_f = ibc_imm_ud(0x3f800000u);

      dest = ibc_AND(b, uint_src.type, uint_src, high_bit);
      ibc_alu_instr *or = ibc_build_alu2(b, IBC_ALU_OP_OR, dest, dest, one_f);
      ibc_instr_set_predicate(&or->instr, is_not_zero,
                              BRW_PREDICATE_NORMAL, false);
      break;
   }

   default:
      unreachable("Unhandled NIR ALU opcode");
   }

   assert(dest.file == IBC_REG_FILE_LOGICAL);
   assert(dest.reg->logical.bit_size == instr->dest.dest.ssa.bit_size);
   assert(dest.reg->logical.num_comps == instr->dest.dest.ssa.num_components);
   nti->ssa_to_reg[instr->dest.dest.ssa.index] = dest.reg;
}

static ibc_reg_ref
nti_reduction_op_identity(nir_op op, enum ibc_type type)
{
   const unsigned bit_size = ibc_type_bit_size(type);
   nir_const_value identity = nir_alu_binop_identity(op, bit_size);
   switch (bit_size) {
   case 8:
      if (type == IBC_TYPE_UB) {
         return ibc_imm_uw(identity.u8);
      } else {
         assert(type == IBC_TYPE_B);
         return ibc_imm_w(identity.i8);
      }
   case 16: return ibc_imm_ref(type, (char *)&identity.u16, 2);
   case 32: return ibc_imm_ref(type, (char *)&identity.u32, 4);
   case 64: return ibc_imm_ref(type, (char *)&identity.u64, 8);
   default:
      unreachable("Invalid type size");
   }
}

static enum ibc_alu_op
nti_op_for_nir_reduction_op(nir_op op)
{
   switch (op) {
   case nir_op_iadd: return IBC_ALU_OP_ADD;
   case nir_op_fadd: return IBC_ALU_OP_ADD;
//   case nir_op_imul: return IBC_ALU_OP_MUL;
//   case nir_op_fmul: return IBC_ALU_OP_MUL;
   case nir_op_imin: return IBC_ALU_OP_SEL;
   case nir_op_umin: return IBC_ALU_OP_SEL;
   case nir_op_fmin: return IBC_ALU_OP_SEL;
   case nir_op_imax: return IBC_ALU_OP_SEL;
   case nir_op_umax: return IBC_ALU_OP_SEL;
   case nir_op_fmax: return IBC_ALU_OP_SEL;
   case nir_op_iand: return IBC_ALU_OP_AND;
   case nir_op_ior:  return IBC_ALU_OP_OR;
//   case nir_op_ixor: return IBC_ALU_OP_XOR;
   default:
      unreachable("Invalid reduction operation");
   }
}

static enum brw_conditional_mod
nti_cond_mod_for_nir_reduction_op(nir_op op)
{
   switch (op) {
   case nir_op_iadd: return BRW_CONDITIONAL_NONE;
   case nir_op_fadd: return BRW_CONDITIONAL_NONE;
   case nir_op_imul: return BRW_CONDITIONAL_NONE;
   case nir_op_fmul: return BRW_CONDITIONAL_NONE;
   case nir_op_imin: return BRW_CONDITIONAL_L;
   case nir_op_umin: return BRW_CONDITIONAL_L;
   case nir_op_fmin: return BRW_CONDITIONAL_L;
   case nir_op_imax: return BRW_CONDITIONAL_GE;
   case nir_op_umax: return BRW_CONDITIONAL_GE;
   case nir_op_fmax: return BRW_CONDITIONAL_GE;
   case nir_op_iand: return BRW_CONDITIONAL_NONE;
   case nir_op_ior:  return BRW_CONDITIONAL_NONE;
   case nir_op_ixor: return BRW_CONDITIONAL_NONE;
   default:
      unreachable("Invalid reduction operation");
   }
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
      ibc_reg *w_tmp_reg =
         ibc_hw_grf_reg_create(b->shader, IBC_HW_GRF_REG_UNASSIGNED,
                               b->simd_width * 2,
                               MIN2(b->simd_width * 2, 32));
      w_tmp_reg->is_wlr = true;
      ibc_reg_ref w_tmp = ibc_typed_ref(w_tmp_reg, IBC_TYPE_UW);

      ibc_builder_push_we_all(b, 8);
      ibc_build_alu1(b, IBC_ALU_OP_MOV, w_tmp, ibc_imm_v(0x76543210));
      ibc_builder_pop(b);

      if (b->simd_width > 8) {
         ibc_reg_ref w_tmp_8 = w_tmp;
         ibc_hw_grf_slice_simd_group(&w_tmp_8.hw_grf, 8, 8);
         ibc_builder_push_we_all(b, 8);
         ibc_build_alu2(b, IBC_ALU_OP_ADD, w_tmp_8, w_tmp, ibc_imm_uw(8));
         ibc_builder_pop(b);
      }

      if (b->simd_width > 16) {
         ibc_reg_ref w_tmp_16 = w_tmp;
         ibc_hw_grf_slice_simd_group(&w_tmp_16.hw_grf, 16, 16);
         ibc_builder_push_we_all(b, 16);
         ibc_build_alu2(b, IBC_ALU_OP_ADD, w_tmp_16, w_tmp, ibc_imm_uw(16));
         ibc_builder_pop(b);
      }

      dest = ibc_MOV(b, IBC_TYPE_UD, w_tmp);
      break;
   }

   case nir_intrinsic_read_invocation: {
      ibc_reg_ref value = ibc_uref(nti->ssa_to_reg[instr->src[0].ssa->index]);
      value.logical.broadcast = true;
      value.logical.simd_channel = nir_src_as_uint(instr->src[1]);
      ibc_builder_push_we_all(b, 1);
      dest = ibc_MOV(b, IBC_TYPE_UINT, value);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_reduce:
   case nir_intrinsic_inclusive_scan: {
      nir_op redop = nir_intrinsic_reduction_op(instr);
      ibc_reg_ref src =
         ibc_typed_ref(nti->ssa_to_reg[instr->src[0].ssa->index],
                       ibc_type_for_nir(nir_op_infos[redop].input_types[0]));

      unsigned cluster_size = instr->intrinsic == nir_intrinsic_reduce ?
                              nir_intrinsic_cluster_size(instr) : 0;
      if (cluster_size == 0 || cluster_size > b->simd_width)
         cluster_size = b->simd_width;

      /* For byte types, we can avoid a whole lot of problems by just doing
       * the scan with W types instead.  Fortunately, as long as we cast
       * properly at the end, there should be no difference.
       */
      enum ibc_type scan_type = src.type;
      if (ibc_type_bit_size(scan_type) == 8)
         scan_type = ibc_type_base_type(scan_type) | IBC_TYPE_16_BIT;

      unsigned tmp_stride = ibc_type_byte_size(scan_type);
      unsigned tmp_size = b->simd_width * tmp_stride;
      unsigned tmp_align = MIN2(tmp_size, 32);
      ibc_reg *tmp_reg =
         ibc_hw_grf_reg_create(b->shader, IBC_HW_GRF_REG_UNASSIGNED,
                               tmp_size, tmp_align);
      tmp_reg->is_wlr = true;
      ibc_reg_ref tmp = ibc_typed_ref(tmp_reg, scan_type);

      ibc_builder_push_we_all(b, b->simd_width);
      ibc_build_alu1(b, IBC_ALU_OP_MOV, tmp,
                     nti_reduction_op_identity(redop, src.type));
      ibc_builder_pop(b);
      ibc_build_alu1(b, IBC_ALU_OP_MOV, tmp, src);

      ibc_build_alu_scan(b, nti_op_for_nir_reduction_op(redop), tmp,
                         nti_cond_mod_for_nir_reduction_op(redop),
                         cluster_size);

      if (instr->intrinsic == nir_intrinsic_reduce) {
         dest = ibc_cluster_broadcast(b, src.type, tmp, cluster_size);
      } else {
         /* Only take the bottom bits of the scan result in case it was a B
          * type which we upgraded to W.
          */
         ibc_reg_ref mov_src = tmp;
         mov_src.type = src.type;
         dest = ibc_MOV(b, src.type, mov_src);
      }
      break;
   }

   case nir_intrinsic_store_ssbo: {
      ibc_intrinsic_instr *store =
         ibc_intrinsic_instr_create(b->shader,
                                    IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE,
                                    b->simd_group, b->simd_width, 3);
      store->has_side_effects = true;
      store->src[0].ref = ibc_imm_ud(nir_src_as_uint(instr->src[1]));
      store->src[0].num_comps = 1;
      assert(instr->src[2].is_ssa);
      store->src[1].ref = ibc_uref(nti->ssa_to_reg[instr->src[2].ssa->index]);
      store->src[1].num_comps = 1;
      assert(instr->src[0].is_ssa);
      store->src[2].ref = ibc_ref(nti->ssa_to_reg[instr->src[0].ssa->index]);
      store->src[2].num_comps = instr->src[0].ssa->num_components;

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

   ibc_reg *g0_reg = ibc_hw_grf_reg_create(b->shader, 0, 32, 32);
   ibc_reg *tmp_reg =
      ibc_hw_grf_reg_create(b->shader, IBC_HW_GRF_REG_UNASSIGNED, 32, 32);

   ibc_reg_ref g0_ud = ibc_typed_ref(g0_reg, IBC_TYPE_UD);
   ibc_reg_ref tmp_ud = ibc_typed_ref(tmp_reg, IBC_TYPE_UD);

   ibc_builder_push_we_all(b, 8);
   ibc_build_alu1(b, IBC_ALU_OP_MOV, tmp_ud, g0_ud);
   ibc_builder_pop(b);

   ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 8);
   send->instr.we_all = true;
   send->sfid = BRW_SFID_THREAD_SPAWNER;
   send->desc_imm = brw_ts_eot_desc(b->shader->devinfo);
   send->has_side_effects = true;
   send->eot = true;

   send->payload[0] = tmp_ud;
   send->mlen = 1;

   ibc_builder_insert_instr(b, &send->instr);
}

ibc_shader *
nir_to_ibc(const nir_shader *nir, void *mem_ctx,
           unsigned dispatch_size,
           const struct gen_device_info *devinfo)
{
   ibc_shader *shader = ibc_shader_create(mem_ctx, devinfo, dispatch_size);

   struct nir_to_ibc_state nti;
   ibc_builder_init(&nti.b, shader);
   ibc_start(&nti.b);

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

   ibc_end(&nti.b);

   return nti.b.shader;
}
