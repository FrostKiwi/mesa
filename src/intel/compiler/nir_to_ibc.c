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

#include "ibc_nir.h"
#include "brw_nir.h"

#include "util/hash_table.h"

void
ibc_setup_payload_base(ibc_builder *b, struct ibc_payload_base *payload)
{
   unsigned reg = 0;
   payload->g0 = ibc_load_payload_reg(b, &reg);
   payload->num_ff_regs = reg;
}

static void
nti_emit_alu(struct nir_to_ibc_state *nti,
             const nir_alu_instr *instr)
{
   ibc_builder *b = &nti->b;

   ibc_reg_ref src[4];
   for (unsigned i = 0; i < nir_op_infos[instr->op].num_inputs; i++) {
      /* TODO */
      assert(!instr->src[i].abs);
      assert(!instr->src[i].negate);

      nir_alu_type nir_src_type = nir_op_infos[instr->op].input_types[i];
      if (nir_alu_type_get_type_size(nir_src_type) == 0)
         nir_src_type |= instr->src[i].src.ssa->bit_size;
      src[i] = ibc_nir_src(nti, instr->src[i].src,
                           ibc_type_for_nir(nir_src_type));
      assert(src[i].file == IBC_REG_FILE_LOGICAL);
      src[i].logical.comp = instr->src[i].swizzle[0];
   }

   assert(instr->dest.dest.is_ssa);

   nir_alu_type nir_dest_type = nir_op_infos[instr->op].output_type;
   if (nir_alu_type_get_type_size(nir_dest_type) == 0)
      nir_dest_type |= instr->dest.dest.ssa.bit_size;
   enum ibc_type dest_type = ibc_type_for_nir(nir_dest_type);

   ibc_reg_ref dest = { .file = IBC_REG_FILE_NONE, };

   switch (instr->op) {
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
      break;
   default:
      assert(instr->dest.dest.ssa.num_components == 1);
      break;
   }

   /* TODO */
   assert(!instr->dest.saturate);

   switch (instr->op) {
   case nir_op_mov:
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
      dest = ibc_VEC(b, src, instr->dest.dest.ssa.num_components);
      break;

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

   case nir_op_b2f32: {
      ibc_reg_ref one = ibc_MOV(b, IBC_TYPE_F, ibc_imm_f(1.0));
      dest = ibc_SEL(b, dest_type, src[0], one, ibc_imm_f(0.0));
      break;
   }

   case nir_op_extract_u8:
   case nir_op_extract_i8:
      assert(src[0].type == dest_type);
      src[0].type = ibc_type_base_type(src[0].type) | 8;
      src[0].logical.byte += nir_src_as_uint(instr->src[1].src) %
                             ibc_type_byte_size(dest_type);
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_extract_u16:
   case nir_op_extract_i16:
      assert(src[0].type == dest_type);
      src[0].type = ibc_type_base_type(src[0].type) | 16;
      src[0].logical.byte += (nir_src_as_uint(instr->src[1].src) * 2) %
                             ibc_type_byte_size(dest_type);
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_unpack_32_2x16_split_x:
   case nir_op_unpack_32_2x16_split_y:
      src[0].type = dest_type;
      src[0].logical.byte = 2 * (instr->op == nir_op_unpack_32_2x16_split_y);
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_ineg:
   case nir_op_fneg: {
      dest = ibc_MOV(b, dest_type, src[0]);
      ibc_alu_instr *mov = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
      mov->src[0].mod = IBC_ALU_SRC_MOD_NEG;
      break;
   }

   case nir_op_inot:
      dest = ibc_NOT(b, dest_type, src[0]);
      break;

   case nir_op_iabs:
   case nir_op_fabs: {
      dest = ibc_MOV(b, dest_type, src[0]);
      ibc_alu_instr *mov = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
      mov->src[0].mod = IBC_ALU_SRC_MOD_ABS;
      break;
   }

   case nir_op_iadd:
   case nir_op_fadd:
      dest = ibc_ADD(b, dest_type, src[0], src[1]);
      break;
   case nir_op_fmul:
      dest = ibc_MUL(b, dest_type, src[0], src[1]);
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

   case nir_op_flt:
   case nir_op_ilt:
   case nir_op_ult:
   case nir_op_fge:
   case nir_op_ige:
   case nir_op_uge:
   case nir_op_feq:
   case nir_op_ieq:
   case nir_op_fneu:
   case nir_op_ine:
      assert(dest_type == IBC_TYPE_FLAG);
      dest = ibc_CMP(b, dest_type, brw_cmod_for_nir_comparison(instr->op),
                     src[0], src[1]);
      break;

   case nir_op_imin:
   case nir_op_umin:
   case nir_op_fmin:
      dest = ibc_MIN(b, dest_type, src[0], src[1]);
      break;

   case nir_op_imax:
   case nir_op_umax:
   case nir_op_fmax:
      dest = ibc_MAX(b, dest_type, src[0], src[1]);
      break;

   case nir_op_ffma:
      assert(dest_type == IBC_TYPE_F);
      dest = ibc_MAD(b, dest_type, src[2], src[1], src[0]);
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
nti_initialize_flag(ibc_builder *b, int32_t flag_val)
{
   assert(b->simd_group == 0);
   ibc_reg *flag_reg = ibc_flag_reg_create(b->shader, b->simd_width);

   ibc_builder_push_scalar(b);
   if (flag_reg->flag.bits <= 16) {
      ibc_MOV_to(b, ibc_typed_ref(flag_reg, IBC_TYPE_UW),
                    ibc_imm_uw(flag_val));
   } else {
      ibc_MOV_to(b, ibc_typed_ref(flag_reg, IBC_TYPE_UD),
                    ibc_imm_ud(flag_val));
   }
   ibc_builder_pop(b);

   return ibc_ref(flag_reg);
}

static enum brw_predicate
brw_predicate_any(unsigned cluster_size)
{
   switch (cluster_size) {
   case 1:  return BRW_PREDICATE_NORMAL;
   case 2:  return BRW_PREDICATE_ALIGN1_ANY2H;
   case 4:  return BRW_PREDICATE_ALIGN1_ANY4H;
   case 8:  return BRW_PREDICATE_ALIGN1_ANY8H;
   case 16: return BRW_PREDICATE_ALIGN1_ANY16H;
   case 32: return BRW_PREDICATE_ALIGN1_ANY32H;
   default: unreachable("Invalid BRW_PREDICATE_ALIGN1_ANY size");
   }
}

static enum brw_predicate
brw_predicate_all(unsigned cluster_size)
{
   switch (cluster_size) {
   case 1:  return BRW_PREDICATE_NORMAL;
   case 2:  return BRW_PREDICATE_ALIGN1_ALL2H;
   case 4:  return BRW_PREDICATE_ALIGN1_ALL4H;
   case 8:  return BRW_PREDICATE_ALIGN1_ALL8H;
   case 16: return BRW_PREDICATE_ALIGN1_ALL16H;
   case 32: return BRW_PREDICATE_ALIGN1_ALL32H;
   default: unreachable("Invalid BRW_PREDICATE_ALIGN1_ALL size");
   }
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
   switch (nti->stage) {
   case MESA_SHADER_FRAGMENT:
      if (ibc_emit_nir_fs_intrinsic(nti, instr))
         return;
      break;
   case MESA_SHADER_COMPUTE:
      if (ibc_emit_nir_cs_intrinsic(nti, instr))
         return;
      break;
   default:
      unreachable("Unsupported shader stage");
   }

   ibc_builder *b = &nti->b;

   ibc_reg_ref dest = { .file = IBC_REG_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_subgroup_invocation: {
      ibc_reg *w_tmp_reg =
         ibc_hw_grf_reg_create(b->shader, b->simd_width * 2,
                               MIN2(b->simd_width * 2, 32));
      ibc_reg_ref w_tmp = ibc_typed_ref(w_tmp_reg, IBC_TYPE_UW);

      ibc_builder_push_we_all(b, 8);
      ibc_build_alu1(b, IBC_ALU_OP_MOV, w_tmp, ibc_imm_v(0x76543210));
      ibc_builder_pop(b);

      if (b->simd_width > 8) {
         ibc_reg_ref w_tmp_8 = w_tmp;
         ibc_hw_grf_simd_slice(&w_tmp_8.hw_grf, 8);
         ibc_builder_push_we_all(b, 8);
         ibc_build_alu2(b, IBC_ALU_OP_ADD, w_tmp_8, w_tmp, ibc_imm_uw(8));
         ibc_builder_pop(b);
      }

      if (b->simd_width > 16) {
         ibc_reg_ref w_tmp_16 = w_tmp;
         ibc_hw_grf_simd_slice(&w_tmp_16.hw_grf, 16);
         ibc_builder_push_we_all(b, 16);
         ibc_build_alu2(b, IBC_ALU_OP_ADD, w_tmp_16, w_tmp, ibc_imm_uw(16));
         ibc_builder_pop(b);
      }

      dest = ibc_MOV(b, IBC_TYPE_UD, w_tmp);
      break;
   }

   case nir_intrinsic_vote_any: {
      ibc_reg_ref flag = nti_initialize_flag(b, 0);
      ibc_MOV_to_flag(b, flag, BRW_CONDITIONAL_NZ,
                      ibc_nir_src(nti, instr->src[0], IBC_TYPE_FLAG));

      /* For some reason, the any/all predicates don't work properly with
       * SIMD32.  In particular, it appears that a SEL with a QtrCtrl of 2H
       * doesn't read the correct subset of the flag register and you end up
       * getting garbage in the second half.  Work around this by using a pair
       * of 1-wide MOVs and scattering the result.
       */
      ibc_builder_push_scalar(b);
      ibc_reg_ref tmp = ibc_MOV_from_flag(b, IBC_TYPE_W,
         brw_predicate_any(b->shader->simd_width), false, flag);
      ibc_builder_pop(b);

      /* Finally, IBC expects 1-bit booleans */
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);
      ibc_MOV_to_flag(b, dest, BRW_CONDITIONAL_NZ, tmp);
      break;
   }

   case nir_intrinsic_vote_all: {
      ibc_reg_ref flag = nti_initialize_flag(b, -1);
      ibc_MOV_to_flag(b, flag, BRW_CONDITIONAL_NZ,
                      ibc_nir_src(nti, instr->src[0], IBC_TYPE_FLAG));

      /* For some reason, the any/all predicates don't work properly with
       * SIMD32.  In particular, it appears that a SEL with a QtrCtrl of 2H
       * doesn't read the correct subset of the flag register and you end up
       * getting garbage in the second half.  Work around this by using a pair
       * of 1-wide MOVs and scattering the result.
       */
      ibc_builder_push_scalar(b);
      ibc_reg_ref tmp = ibc_MOV_from_flag(b, IBC_TYPE_W,
         brw_predicate_all(b->shader->simd_width), false, flag);
      ibc_builder_pop(b);

      /* Finally, IBC expects 1-bit booleans */
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);
      ibc_MOV_to_flag(b, dest, BRW_CONDITIONAL_NZ, tmp);
      break;
   }

   case nir_intrinsic_ballot: {
      ibc_reg_ref flag = nti_initialize_flag(b, 0);
      ibc_MOV_to_flag(b, flag, BRW_CONDITIONAL_NZ, ibc_imm_w(-1));
      flag.type = flag.reg->flag.bits <= 16 ? IBC_TYPE_UW : IBC_TYPE_UD;
      dest = ibc_MOV(b, IBC_TYPE_UD, flag);
      break;
   }

   case nir_intrinsic_read_invocation: {
      ibc_reg_ref value = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UINT);
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
         ibc_nir_src(nti, instr->src[0],
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
         ibc_hw_grf_reg_create(b->shader, tmp_size, tmp_align);
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

   case nir_intrinsic_load_ubo:
      if (nir_src_is_const(instr->src[0]) && nir_src_is_const(instr->src[1])) {
         const unsigned comp_size_B = instr->dest.ssa.bit_size / 8;
         const uint64_t offset_B = nir_src_as_uint(instr->src[1]);
         const unsigned block_size_B = 64; /* Fetch one cacheline at a time. */

         /* Block loads are inherently scalar operations.  This will actually
          * get expanded out to a SIMD16 WE_all send but for the purposes of
          * the logical instruction, it makes more sense to be scalar.
          */
         ibc_builder_push_scalar(b);

         ibc_reg_ref dest_comps[4];
         for (unsigned c = 0; c < instr->num_components; c++) {
            const uint64_t comp_offset_B = offset_B + c * comp_size_B;
            const unsigned comp_offset_in_block_B =
               comp_offset_B % block_size_B;
            const uint64_t block_offset_B =
               comp_offset_B - comp_offset_in_block_B;

            ibc_intrinsic_instr *load =
               ibc_intrinsic_instr_create(b->shader,
                                          IBC_INTRINSIC_OP_BTI_CONST_BLOCK_READ,
                                          b->simd_group, b->simd_width, 2);
            load->instr.we_all = true;
            load->src[0].ref = ibc_imm_ud(nir_src_as_uint(instr->src[0]));
            load->src[0].num_comps = 1;
            assert(instr->src[1].is_ssa);
            load->src[1].ref = ibc_imm_ud(block_offset_B);
            load->src[1].num_comps = 1;

            ibc_reg *data_reg =
               ibc_hw_grf_reg_create(b->shader, REG_SIZE * 2, REG_SIZE);
            load->dest = ibc_typed_ref(data_reg, IBC_TYPE_32_BIT);
            load->num_dest_comps = block_size_B / 4;

            ibc_builder_insert_instr(b, &load->instr);

            dest_comps[c] = ibc_typed_ref(data_reg, instr->dest.ssa.bit_size);
            dest_comps[c].hw_grf.byte = comp_offset_in_block_B;
            ibc_hw_grf_mul_stride(&dest_comps[c].hw_grf, 0);
         }
         dest = ibc_VEC(b, dest_comps, instr->num_components);

         ibc_builder_pop(b);
         break;
      }
      /* fall through */

   case nir_intrinsic_load_ssbo: {
      ibc_intrinsic_instr *load =
         ibc_intrinsic_instr_create(b->shader,
                                    IBC_INTRINSIC_OP_BTI_UNTYPED_READ,
                                    b->simd_group, b->simd_width, 2);
      load->can_reorder = false;
      load->src[0].ref = ibc_imm_ud(nir_src_as_uint(instr->src[0]));
      load->src[0].num_comps = 1;
      assert(instr->src[1].is_ssa);
      load->src[1].ref = ibc_nir_src(nti, instr->src[1], IBC_TYPE_UD);
      load->src[1].num_comps = 1;

      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD,
                                         instr->num_components);
      load->dest = dest;
      load->num_dest_comps = instr->num_components;

      ibc_builder_insert_instr(b, &load->instr);
      break;
   }

   case nir_intrinsic_store_ssbo: {
      ibc_intrinsic_instr *store =
         ibc_intrinsic_instr_create(b->shader,
                                    IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE,
                                    b->simd_group, b->simd_width, 3);
      store->can_reorder = false;
      store->has_side_effects = true;
      store->src[0].ref = ibc_imm_ud(nir_src_as_uint(instr->src[1]));
      store->src[0].num_comps = 1;
      assert(instr->src[2].is_ssa);
      store->src[1].ref = ibc_nir_src(nti, instr->src[2], IBC_TYPE_UD);
      store->src[1].num_comps = 1;
      assert(instr->src[0].is_ssa);
      store->src[2].ref = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD);
      store->src[2].num_comps = instr->num_components;

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

   enum ibc_type type = instr->def.bit_size >= 8 ?
      (IBC_TYPE_UINT | instr->def.bit_size) : IBC_TYPE_FLAG;

   ibc_reg_ref imm_srcs[4];

   for (unsigned i = 0; i < instr->def.num_components; i++) {
      imm_srcs[i] = (ibc_reg_ref) {
         .file = IBC_REG_FILE_IMM,
         .type = type,
      };
      switch (instr->def.bit_size) {
      case 1:
         /* 1-bit immediates aren't a thing */
         imm_srcs[i].type = IBC_TYPE_W;
         *(int16_t *)imm_srcs[i].imm = -(int)instr->value[i].b;
         imm_srcs[i] = ibc_MOV(b, type, imm_srcs[i]);
         break;
      case 8:
         /* 8-bit immediates aren't a thing */
         imm_srcs[i].type = IBC_TYPE_UW;
         *(uint16_t *)imm_srcs[i].imm = instr->value[i].u8;
         imm_srcs[i] = ibc_MOV(b, type, imm_srcs[i]);
         break;
      case 16:
         *(uint16_t *)imm_srcs[i].imm = instr->value[i].u16;
         break;
      case 32:
         *(uint32_t *)imm_srcs[i].imm = instr->value[i].u32;
         break;
      case 64:
         *(uint64_t *)imm_srcs[i].imm = instr->value[i].u64;
         break;
      default:
         unreachable("Invalid bit size");
      }
   }

   ibc_builder_push_scalar(b);
   ibc_reg_ref dest = ibc_VEC(b, imm_srcs, instr->def.num_components);
   ibc_builder_pop(b);

   nti->ssa_to_reg[instr->def.index] = dest.reg;
}

static void
nti_emit_phi(struct nir_to_ibc_state *nti, const nir_phi_instr *instr)
{
   ibc_builder *b = &nti->b;

   assert(b->simd_group == 0);
   assert(b->simd_width == b->shader->simd_width);

   ibc_reg_ref dest =
      ibc_builder_new_logical_reg(b, instr->dest.ssa.bit_size,
                                  instr->dest.ssa.num_components);

   /* Just emit the bare phi for now.  We'll come in and fill in the sources
    * later.
    */
   ibc_phi_instr *phi = ibc_phi_instr_create(b->shader, b->simd_group,
                                             b->simd_width);
   phi->dest = dest;
   phi->num_comps = instr->dest.ssa.num_components;
   ibc_builder_insert_instr(b, &phi->instr);

   nti->ssa_to_reg[instr->dest.ssa.index] = dest.reg;
}

static void
nti_add_phi_srcs(struct nir_to_ibc_state *nti, const nir_phi_instr *instr)
{
   ibc_builder *b = &nti->b;

   const ibc_reg *dest_reg = nti->ssa_to_reg[instr->dest.ssa.index];
   ibc_phi_instr *phi = ibc_instr_as_phi(ibc_reg_ssa_instr(dest_reg));

   nir_foreach_phi_src(nir_src, instr) {
      struct hash_entry *entry =
         _mesa_hash_table_search(nti->nir_block_to_ibc_merge, nir_src->pred);
      assert(entry);
      ibc_merge_instr *pred_merge = entry->data;
      ibc_branch_instr *pred_branch = pred_merge->block_end;

      ibc_phi_src *phi_src = ralloc(b->shader, ibc_phi_src);
      phi_src->pred = pred_branch;
      phi_src->ref = ibc_ref(nti->ssa_to_reg[nir_src->src.ssa->index]);
      list_addtail(&phi_src->link, &phi->srcs);
   }
}

static void
nti_emit_block(struct nir_to_ibc_state *nti, const nir_block *block)
{
   nir_foreach_instr(instr, block) {
      assert(nti->b._group_stack_size == 0);
      switch (instr->type) {
      case nir_instr_type_alu:
         nti_emit_alu(nti, nir_instr_as_alu(instr));
         break;
      case nir_instr_type_intrinsic:
         nti_emit_intrinsic(nti, nir_instr_as_intrinsic(instr));
         break;
      case nir_instr_type_load_const:
         nti_emit_load_const(nti, nir_instr_as_load_const(instr));
         break;
      case nir_instr_type_phi:
         nti_emit_phi(nti, nir_instr_as_phi(instr));
         break;
      default:
         unreachable("Unsupported instruction type");
      }
   }

   _mesa_hash_table_insert(nti->nir_block_to_ibc_merge,
                           block, nti->b.block_start);
}

static void
nti_emit_cf_list(struct nir_to_ibc_state *nti,
                 const struct exec_list *cf_list);

static void
nti_emit_if(struct nir_to_ibc_state *nti, nir_if *nif)
{
   ibc_builder *b = &nti->b;

   ibc_reg_ref cond = ibc_nir_src(nti, nif->condition, IBC_TYPE_FLAG);
   ibc_branch_instr *_if = ibc_if(b, cond, BRW_PREDICATE_NORMAL, false);

   nti_emit_cf_list(nti, &nif->then_list);

   ibc_branch_instr *_else = ibc_else(b, _if);

   nti_emit_cf_list(nti, &nif->else_list);

   ibc_endif(b, _if, _else);
}

static void
nti_emit_loop(struct nir_to_ibc_state *nti, nir_loop *loop)
{
   unreachable("Loops not yet supported");
}

static void
nti_emit_cf_list(struct nir_to_ibc_state *nti,
                 const struct exec_list *cf_list)
{
   foreach_list_typed(const nir_cf_node, node, node, cf_list) {
      switch (node->type) {
      case nir_cf_node_if:
         nti_emit_if(nti, nir_cf_node_as_if(node));
         break;

      case nir_cf_node_loop:
         nti_emit_loop(nti, nir_cf_node_as_loop(node));
         break;

      case nir_cf_node_block:
         nti_emit_block(nti, nir_cf_node_as_block(node));
         break;

      default:
         unreachable("Invalid CFG node block");
      }
   }
}

void
nir_to_ibc_state_init(struct nir_to_ibc_state *nti,
                      gl_shader_stage stage,
                      const struct gen_device_info *devinfo,
                      const struct brw_base_prog_key *key,
                      struct brw_stage_prog_data *prog_data,
                      void *stage_state,
                      unsigned dispatch_size,
                      void *mem_ctx)
{
   ibc_shader *shader = ibc_shader_create(mem_ctx, devinfo, dispatch_size);

   *nti = (struct nir_to_ibc_state) {
      .mem_ctx = mem_ctx,
      .stage = stage,
      .key = key,
      .prog_data = prog_data,
      .stage_state = stage_state,
   };
   ibc_builder_init(&nti->b, shader);
   ibc_start(&nti->b);
}

void
ibc_emit_nir_shader(struct nir_to_ibc_state *nti,
                    const nir_shader *nir)
{
   nir_function_impl *impl = nir_shader_get_entrypoint((nir_shader *)nir);

   nti->ssa_to_reg =
      ralloc_array(nti->mem_ctx, const ibc_reg *, impl->ssa_alloc);
   nti->nir_block_to_ibc_merge = _mesa_pointer_hash_table_create(nti->mem_ctx);

   nti_emit_cf_list(nti, &impl->body);

   /* Do a second walk of the IR to fill in the phi sources */
   nir_foreach_block(block, impl) {
      nir_foreach_instr(instr, block) {
         if (instr->type != nir_instr_type_phi)
            break;

         nti_add_phi_srcs(nti, nir_instr_as_phi(instr));
      }
   }
}

ibc_shader *
nir_to_ibc_state_finish(struct nir_to_ibc_state *nti)
{
   ibc_end(&nti->b);
   return nti->b.shader;
}
