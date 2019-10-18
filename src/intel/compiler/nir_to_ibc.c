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

/* Needed for brw_texture_offset */
#include "brw_shader.h"

#include "util/hash_table.h"

static ibc_ref
ibc_BTI_BLOCK_LOAD_UBO(ibc_builder *b, ibc_ref hw_grf, ibc_ref bti,
                       ibc_ref offset, uint32_t block_size)
{
   assert(block_size % REG_SIZE == 0);
   ibc_reg *data_reg = ibc_hw_grf_reg_create(b->shader, block_size, REG_SIZE);

   hw_grf.type = IBC_TYPE_UD;
   ibc_ref data = ibc_typed_ref(data_reg, IBC_TYPE_UD);
   unsigned block_num_comps = block_size / ibc_type_byte_size(IBC_TYPE_UD);

   ibc_intrinsic_src srcs[3] = { };

   if (hw_grf.file != IBC_FILE_NONE) {
      srcs[0] = (ibc_intrinsic_src) {
         .ref = hw_grf,
         .num_comps = block_num_comps,
      };
   }

   if (bti.file != IBC_FILE_NONE) {
      srcs[1] = (ibc_intrinsic_src) {
         .ref = ibc_uniformize(b, bti),
         .num_comps = 1,
      };
   }

   srcs[2] = (ibc_intrinsic_src) {
      .ref = offset,
      .num_comps = 1,
   };

   ibc_builder_push_scalar(b);
   ibc_build_intrinsic(b, IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO,
                       data, -1, block_num_comps, srcs, 3);
   ibc_builder_pop(b);

   return data;
}

void
ibc_setup_payload_base(ibc_builder *b, struct ibc_payload_base *payload)
{
   b->shader->g0 = ibc_hw_grf_reg_create(b->shader, 32, 32);
   ibc_builder_push_we_all(b, 8);
   ibc_intrinsic_instr *load_g0 =
      ibc_load_payload(b, ibc_typed_ref(b->shader->g0, IBC_TYPE_UD),
                          ibc_hw_grf_ref(0, 0, IBC_TYPE_UD), 1);
   load_g0->can_reorder = false;
   load_g0->has_side_effects = true;
   ibc_builder_pop(b);

   payload->num_ff_regs = 1; /* g0 */
}

/* Allow at most 16 registers worth of push constants */
#define IBC_MAX_PUSH_REGS 16

void
ibc_setup_curb_payload(ibc_builder *b, struct ibc_payload_base *payload,
                       struct brw_stage_prog_data *prog_data)
{
   assert(payload->num_curb_regs == 0);
   if (prog_data->nr_params) {
      assert(prog_data->nr_params * 4 < IBC_MAX_PUSH_REGS * REG_SIZE);
      const unsigned reg = payload->num_ff_regs + payload->num_curb_regs;
      const unsigned length = DIV_ROUND_UP(prog_data->nr_params * 4, REG_SIZE);
      payload->push =
         ibc_BTI_BLOCK_LOAD_UBO(b, ibc_hw_grf_ref(reg, 0, IBC_TYPE_UD),
                                   ibc_null(IBC_TYPE_UD), ibc_imm_ud(0),
                                   length * REG_SIZE);
      payload->num_curb_regs += length;
   }

   for (unsigned i = 0; i < ARRAY_SIZE(prog_data->ubo_ranges); i++) {
      struct brw_ubo_range *range = &prog_data->ubo_ranges[i];
      assert(payload->num_curb_regs <= IBC_MAX_PUSH_REGS);
      range->length = MIN2(range->length,
                           IBC_MAX_PUSH_REGS - payload->num_curb_regs);
      if (range->length == 0)
         continue;

      const unsigned reg = payload->num_ff_regs + payload->num_curb_regs;
      payload->ubo_push[i] =
         ibc_BTI_BLOCK_LOAD_UBO(b, ibc_hw_grf_ref(reg, 0, IBC_TYPE_UD),
                                   ibc_imm_ud(range->block),
                                   ibc_imm_ud(range->start * REG_SIZE),
                                   range->length * REG_SIZE);
      payload->num_curb_regs += range->length;
   }

   assert(payload->num_curb_regs <= IBC_MAX_PUSH_REGS);
}

static void
nti_emit_alu(struct nir_to_ibc_state *nti,
             const nir_alu_instr *instr)
{
   ibc_builder *b = &nti->b;

   ibc_ref src[4];
   for (unsigned i = 0; i < nir_op_infos[instr->op].num_inputs; i++) {
      /* TODO */
      assert(!instr->src[i].abs);
      assert(!instr->src[i].negate);

      nir_alu_type nir_src_type = nir_op_infos[instr->op].input_types[i];
      if (nir_alu_type_get_type_size(nir_src_type) == 0)
         nir_src_type |= nir_src_bit_size(instr->src[i].src);
      src[i] = ibc_nir_src(nti, instr->src[i].src,
                           ibc_type_for_nir(nir_src_type));
      assert(src[i].file == IBC_FILE_LOGICAL);
      src[i].logical.comp = instr->src[i].swizzle[0];
   }

   nir_alu_type nir_dest_type = nir_op_infos[instr->op].output_type;
   if (nir_alu_type_get_type_size(nir_dest_type) == 0)
      nir_dest_type |= nir_dest_bit_size(instr->dest.dest);
   enum ibc_type dest_type = ibc_type_for_nir(nir_dest_type);

   ibc_ref dest = { .file = IBC_FILE_NONE, };

   ibc_builder_push_nir_dest_group(b, instr->dest.dest);

   switch (instr->op) {
   case nir_op_mov:
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
      break;
   default:
      assert(nir_dest_num_components(instr->dest.dest) == 1);
      break;
   }

   /* TODO */
   assert(!instr->dest.saturate);

#define UNOP_CASE(nop, IOP)                           \
   case nir_op_##nop:                                 \
      dest = ibc_##IOP(b, dest_type, src[0]);         \
      break;

#define BINOP_CASE(nop, IOP)                          \
   case nir_op_##nop:                                 \
      dest = ibc_##IOP(b, dest_type, src[0], src[1]); \
      break;

   switch (instr->op) {
   case nir_op_mov:
      for (unsigned i = 1; i < nir_dest_num_components(instr->dest.dest); i++)
         src[i] = ibc_comp_ref(src[0], i);
      dest = ibc_VEC(b, src, nir_dest_num_components(instr->dest.dest));
      break;

   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
      dest = ibc_VEC(b, src, nir_dest_num_components(instr->dest.dest));
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

   case nir_op_f2u8:
   case nir_op_f2u16:
   case nir_op_f2u32:
   case nir_op_f2i8:
   case nir_op_f2i16:
   case nir_op_f2i32:
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_i2f32:
   case nir_op_u2f32:
      dest = ibc_MOV(b, dest_type, src[0]);
      break;

   case nir_op_b2i8:
   case nir_op_b2i16:
   case nir_op_b2i32:
   case nir_op_b2i64:
      /* ibc_assign_and_lower_flags will turn src[0] into a W type 0/-1
       * boolean and all we have to do is negate it.
       */
      dest = ibc_NEG(b, dest_type, src[0]);
      break;

   case nir_op_b2f32: {
      ibc_ref one = ibc_MOV(b, IBC_TYPE_F, ibc_imm_f(1.0));
      dest = ibc_SEL(b, dest_type, src[0], one, ibc_imm_f(0.0));
      break;
   }

   UNOP_CASE(ftrunc,       RNDZ)
   UNOP_CASE(fceil,        RNDU)
   UNOP_CASE(ffloor,       RNDD)
   UNOP_CASE(ffract,       FRC)
   UNOP_CASE(fround_even,  RNDE)

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
   case nir_op_fneg:
      dest = ibc_NEG(b, dest_type, src[0]);
      break;

   case nir_op_inot:
      dest = ibc_NOT(b, dest_type, src[0]);
      break;

   case nir_op_iabs:
   case nir_op_fabs:
      dest = ibc_ABS(b, dest_type, src[0]);
      break;

   case nir_op_fsat: {
      dest = ibc_MOV(b, dest_type, src[0]);
      ibc_alu_instr *mov = ibc_instr_as_alu(ibc_reg_ssa_instr(dest.reg));
      mov->saturate = true;
      break;
   }

   case nir_op_fddx:
   case nir_op_fddx_coarse: {
      ibc_ref left = ibc_restride(b, src[0], IBC_TYPE_F, 0, 4, 4, 0);
      ibc_ref right = ibc_restride(b, src[0], IBC_TYPE_F, 1, 4, 4, 0);
      dest = ibc_ADD(b, IBC_TYPE_F, ibc_NEG(b, IBC_TYPE_F, left), right);
      break;
   }

   case nir_op_fddx_fine: {
      ibc_ref left = ibc_restride(b, src[0], IBC_TYPE_F, 0, 2, 2, 0);
      ibc_ref right = ibc_restride(b, src[0], IBC_TYPE_F, 1, 2, 2, 0);
      dest = ibc_ADD(b, IBC_TYPE_F, ibc_NEG(b, IBC_TYPE_F, left), right);
      break;
   }

   case nir_op_fddy:
   case nir_op_fddy_coarse: {
      ibc_ref top = ibc_restride(b, src[0], IBC_TYPE_F, 0, 4, 4, 0);
      ibc_ref bottom = ibc_restride(b, src[0], IBC_TYPE_F, 2, 4, 4, 0);
      dest = ibc_ADD(b, IBC_TYPE_F, ibc_NEG(b, IBC_TYPE_F, top), bottom);
      break;
   }

   case nir_op_fddy_fine: {
      /* First, we have to move it to a GRF so we can stride */
      assert(src[0].type == IBC_TYPE_F);
      const unsigned grf_size = b->simd_width * ibc_type_byte_size(src[0].type);
      ibc_reg *grf = ibc_hw_grf_reg_create(b->shader, grf_size, 32);
      ibc_MOV_to(b, ibc_typed_ref(grf, src[0].type), src[0]);

      /* This requires an ALIGN16 instruction with a swizzle so we punt to an
       * intrinsic which gets turned into the ALIGN16 in ibc_to_binary.
       */
      ibc_intrinsic_src intrin_src = {
         .ref = ibc_typed_ref(grf, src[0].type),
         .num_comps = 1,
      };
      dest = ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_ALIGN16_DDX_FINE,
                                     src[0].type, 1, &intrin_src, 1);
      break;
   }

   BINOP_CASE(iadd, ADD)
   BINOP_CASE(fadd, ADD)
   BINOP_CASE(fmul, MUL)
   BINOP_CASE(imul, MUL)
   BINOP_CASE(iand, AND)
   BINOP_CASE(ior,  OR)
   BINOP_CASE(ixor, XOR)
   BINOP_CASE(ishl, SHL)
   BINOP_CASE(ishr, SHR)
   BINOP_CASE(ushr, SHR)

   case nir_op_ubfe:
   case nir_op_ibfe:
      assert(ibc_type_bit_size(dest_type) < 64);
      dest = ibc_BFE(b, dest_type, src[2], src[1], src[0]);
      break;
   case nir_op_bfm:
      assert(ibc_type_bit_size(dest_type) < 64);
      dest = ibc_BFI1(b, dest_type, src[0], src[1]);
      break;
   case nir_op_bfi:
      assert(ibc_type_bit_size(dest_type) < 64);
      dest = ibc_BFI2(b, dest_type, src[0], src[1], src[2]);
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

   BINOP_CASE(imin, MIN)
   BINOP_CASE(umin, MIN)
   BINOP_CASE(fmin, MIN)
   BINOP_CASE(imax, MAX)
   BINOP_CASE(umax, MAX)
   BINOP_CASE(fmax, MAX)

   case nir_op_ffma:
      assert(dest_type == IBC_TYPE_F);
      dest = ibc_MAD(b, dest_type, src[2], src[1], src[0]);
      break;

   case nir_op_flrp:
      assert(dest_type == IBC_TYPE_F);
      dest = ibc_LRP(b, dest_type, src[2], src[1], src[0]);
      break;

   UNOP_CASE(frcp,   RCP)
   UNOP_CASE(flog2,  LOG2)
   UNOP_CASE(fexp2,  EXP2)
   UNOP_CASE(fsqrt,  SQRT)
   UNOP_CASE(frsq,   RSQ)
   UNOP_CASE(fsin,   SIN)
   UNOP_CASE(fcos,   COS)
   BINOP_CASE(fpow,  POW)
   BINOP_CASE(idiv,  IDIV)
   BINOP_CASE(udiv,  IDIV)
   BINOP_CASE(umod,  IREM)
   BINOP_CASE(irem,  IREM)
   /* TODO: This isn't right for imod */
   BINOP_CASE(imod,  IREM)

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
      ibc_ref is_not_zero = ibc_CMP(b, IBC_TYPE_FLAG, BRW_CONDITIONAL_NZ,
                                    src[0], ibc_imm_zero(src[0].type));

      ibc_ref uint_src = src[0];
      uint_src.type = IBC_TYPE_UINT | ibc_type_bit_size(src[0].type);

      /* TODO */
      assert(ibc_type_bit_size(src[0].type) == 32);
      ibc_ref high_bit = ibc_imm_ud(0x80000000u);
      ibc_ref one_f = ibc_imm_ud(0x3f800000u);

      dest = ibc_AND(b, uint_src.type, uint_src, high_bit);
      ibc_alu_instr *or = ibc_build_alu2(b, IBC_ALU_OP_OR, dest, dest, one_f);
      ibc_instr_set_predicate(&or->instr, is_not_zero,
                              IBC_PREDICATE_NORMAL);
      break;
   }

   default:
      unreachable("Unhandled NIR ALU opcode");
   }

   ibc_write_nir_dest(nti, &instr->dest.dest, dest);

   ibc_builder_pop(b);
}

static ibc_ref
nti_initialize_flag(ibc_builder *b, int32_t flag_val)
{
   assert(b->simd_group == 0);
   ibc_reg *flag_reg = ibc_flag_reg_create(b->shader, MAX2(b->simd_width, 16));

   ibc_builder_push_scalar(b);
   if (flag_reg->flag.bits <= 16) {
      ibc_MOV_to(b, ibc_typed_ref(flag_reg, IBC_TYPE_UW),
                    ibc_imm_uw(flag_val));
   } else {
      ibc_MOV_to(b, ibc_typed_ref(flag_reg, IBC_TYPE_UD),
                    ibc_imm_ud(flag_val));
   }
   ibc_builder_pop(b);

   return ibc_typed_ref(flag_reg, IBC_TYPE_FLAG);
}

static void
nti_emit_tex(struct nir_to_ibc_state *nti,
             const nir_tex_instr *ntex)
{
   ibc_builder *b = &nti->b;

   ibc_intrinsic_src srcs[IBC_TEX_NUM_SRCS] = {};

   srcs[IBC_TEX_SRC_SURFACE_BTI] = (ibc_intrinsic_src) {
      .ref = ibc_imm_ud(ntex->texture_index),
      .num_comps = 1,
   };
   srcs[IBC_TEX_SRC_SAMPLER_BTI] = (ibc_intrinsic_src) {
      .ref = ibc_imm_ud(ntex->sampler_index),
      .num_comps = 1,
   };

   ibc_builder_push_nir_dest_group(b, ntex->dest);

   uint32_t header_bits = 0;
   for (unsigned i = 0; i < ntex->num_srcs; i++) {
      const enum ibc_type src_type =
         ibc_type_for_nir(nir_tex_instr_src_type(ntex, i)) |
         nir_src_bit_size(ntex->src[i].src);
      ibc_intrinsic_src src = {
         .ref = ibc_nir_src(nti, ntex->src[i].src, src_type),
         .num_comps = nir_tex_instr_src_size(ntex, i),
      };

      switch (ntex->src[i].src_type) {
      case nir_tex_src_bias:
         srcs[IBC_TEX_SRC_LOD] = src;
         break;
      case nir_tex_src_comparator:
         srcs[IBC_TEX_SRC_SHADOW_C] = src;
         break;
      case nir_tex_src_coord:
         srcs[IBC_TEX_SRC_COORD] = src;
         break;
      case nir_tex_src_ddx:
         srcs[IBC_TEX_SRC_DDX] = src;
         break;
      case nir_tex_src_ddy:
         srcs[IBC_TEX_SRC_DDY] = src;
         break;
      case nir_tex_src_lod:
         srcs[IBC_TEX_SRC_LOD] = src;
         break;
      case nir_tex_src_min_lod:
         srcs[IBC_TEX_SRC_MIN_LOD] = src;
         break;
      case nir_tex_src_ms_index:
         srcs[IBC_TEX_SRC_SAMPLE_INDEX] = src;
         break;

      case nir_tex_src_offset: {
         uint32_t offset_bits = 0;
         if (brw_texture_offset(ntex, i, &offset_bits)) {
            header_bits |= offset_bits;
         } else {
            srcs[IBC_TEX_SRC_TG4_OFFSET] = src;
            srcs[IBC_TEX_SRC_TG4_OFFSET].num_comps = 2;
         }
         break;
      }

      case nir_tex_src_projector:
         unreachable("should be lowered");

      case nir_tex_src_texture_offset: {
         /* Emit code to evaluate the actual indexing expression */
         src.ref = ibc_uniformize(b, src.ref);
         ibc_builder_push_scalar(b);
         src.ref = ibc_ADD(b, IBC_TYPE_D, src.ref,
                              ibc_imm_d(ntex->texture_index));
         ibc_builder_pop(b);
         srcs[IBC_TEX_SRC_SURFACE_BTI] = src;
         break;
      }

      case nir_tex_src_sampler_offset: {
         /* Emit code to evaluate the actual indexing expression */
         src.ref = ibc_uniformize(b, src.ref);
         ibc_builder_push_scalar(b);
         src.ref = ibc_ADD(b, IBC_TYPE_D, src.ref,
                              ibc_imm_d(ntex->sampler_index));
         ibc_builder_pop(b);
         srcs[IBC_TEX_SRC_SAMPLER_BTI] = src;
         break;
      }

      case nir_tex_src_texture_handle:
         assert(nir_tex_instr_src_index(ntex, nir_tex_src_texture_offset) == -1);
         src.ref = ibc_uniformize(b, src.ref);
         srcs[IBC_TEX_SRC_SURFACE_HANDLE] = src;
         srcs[IBC_TEX_SRC_SURFACE_BTI] = (ibc_intrinsic_src) { };
         break;

      case nir_tex_src_sampler_handle:
         assert(nir_tex_instr_src_index(ntex, nir_tex_src_sampler_offset) == -1);
         src.ref = ibc_uniformize(b, src.ref);
         srcs[IBC_TEX_SRC_SAMPLER_HANDLE] = src;
         srcs[IBC_TEX_SRC_SAMPLER_BTI] = (ibc_intrinsic_src) { };
         break;

      case nir_tex_src_ms_mcs:
         assert(ntex->op == nir_texop_txf_ms);
         srcs[IBC_TEX_SRC_MCS] = src;
         /* The NIR src says it takes 4 components but we only care about the
          * first on gen8 and the first two on gen9+.
          */
         srcs[IBC_TEX_SRC_MCS].num_comps =
            b->shader->devinfo->gen >= 9 ? 2 : 1;
         break;

      default:
         unreachable("unknown texture source");
      }
   }

   assert(nti->key->tex.gather_channel_quirk_mask == 0);
   if (ntex->op == nir_texop_tg4)
      header_bits |= ntex->component << 16;

   if (header_bits) {
      srcs[IBC_TEX_SRC_HEADER_BITS] = (ibc_intrinsic_src) {
         .ref = ibc_imm_ud(header_bits),
         .num_comps = 1,
      };
   }

   nir_intrinsic_op op;
   switch (ntex->op) {
   case nir_texop_tex:
      op = IBC_INTRINSIC_OP_TEX;
      break;
   case nir_texop_txb:
      op = IBC_INTRINSIC_OP_TXB;
      break;
   case nir_texop_txl:
      op = IBC_INTRINSIC_OP_TXL;
      break;
   case nir_texop_txd:
      op = IBC_INTRINSIC_OP_TXD;
      break;
   case nir_texop_txf:
      op = IBC_INTRINSIC_OP_TXF;
      break;
   case nir_texop_txf_ms:
      op = IBC_INTRINSIC_OP_TXF_MS;
      break;
   case nir_texop_txf_ms_mcs:
      op = IBC_INTRINSIC_OP_TXF_MCS;
      break;
   case nir_texop_query_levels:
   case nir_texop_txs:
      op = IBC_INTRINSIC_OP_TXS;
      break;
   case nir_texop_lod:
      op = IBC_INTRINSIC_OP_LOD;
      break;
   case nir_texop_tg4:
      if (srcs[IBC_TEX_SRC_TG4_OFFSET].num_comps > 0)
         op = IBC_INTRINSIC_OP_TG4_OFFSET;
      else
         op = IBC_INTRINSIC_OP_TG4;
      break;
   case nir_texop_texture_samples:
      op = IBC_INTRINSIC_OP_SAMPLEINFO;
      break;
   default:
      unreachable("unknown texture op");
   }

   unsigned num_dest_comps = 4;
   const unsigned nir_num_dest_comps = nir_tex_instr_dest_size(ntex);
   if (b->shader->devinfo->gen >= 9 &&
       ntex->op != nir_texop_tg4 && ntex->op != nir_texop_query_levels) {
      unsigned write_mask = ntex->dest.is_ssa ?
                            nir_ssa_def_components_read(&ntex->dest.ssa):
                            (1 << nir_num_dest_comps) - 1;
      assert(write_mask != 0); /* dead code should have been eliminated */
      num_dest_comps = util_last_bit(write_mask);
   }

   const enum ibc_type dest_type = ibc_type_for_nir(ntex->dest_type) |
                                   nir_dest_bit_size(ntex->dest);
   ibc_ref dest =
      ibc_build_ssa_intrinsic(b, op, dest_type, num_dest_comps,
                              srcs, IBC_TEX_NUM_SRCS);

   if (ntex->op == nir_texop_query_levels) {
      /* # levels is in .w */
      dest.logical.comp = 3;
   }

   ibc_write_nir_dest(nti, &ntex->dest, dest);

   ibc_builder_pop(b);
}

static enum ibc_predicate
ibc_predicate_any(unsigned cluster_size)
{
   switch (cluster_size) {
   case 1:  return IBC_PREDICATE_NORMAL;
   case 2:  return IBC_PREDICATE_ANY2H;
   case 4:  return IBC_PREDICATE_ANY4H;
   case 8:  return IBC_PREDICATE_ANY8H;
   case 16: return IBC_PREDICATE_ANY16H;
   case 32: return IBC_PREDICATE_ANY32H;
   default: unreachable("Invalid IBC_PREDICATE_ANY size");
   }
}

static enum ibc_predicate
ibc_predicate_all(unsigned cluster_size)
{
   switch (cluster_size) {
   case 1:  return IBC_PREDICATE_NORMAL;
   case 2:  return IBC_PREDICATE_ALL2H;
   case 4:  return IBC_PREDICATE_ALL4H;
   case 8:  return IBC_PREDICATE_ALL8H;
   case 16: return IBC_PREDICATE_ALL16H;
   case 32: return IBC_PREDICATE_ALL32H;
   default: unreachable("Invalid IBC_PREDICATE_ALL size");
   }
}

static ibc_ref
nti_reduction_op_identity(nir_op op, enum ibc_type type)
{
   const unsigned bit_size = ibc_type_bit_size(type);
   nir_const_value identity = nir_alu_binop_identity(op, bit_size);
   switch (bit_size) {
   case 1:
      return ibc_imm_w(-(int)identity.b);
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
   case nir_op_fmul: return IBC_ALU_OP_MUL;
   case nir_op_imin: return IBC_ALU_OP_SEL;
   case nir_op_umin: return IBC_ALU_OP_SEL;
   case nir_op_fmin: return IBC_ALU_OP_SEL;
   case nir_op_imax: return IBC_ALU_OP_SEL;
   case nir_op_umax: return IBC_ALU_OP_SEL;
   case nir_op_fmax: return IBC_ALU_OP_SEL;
   case nir_op_iand: return IBC_ALU_OP_AND;
   case nir_op_ior:  return IBC_ALU_OP_OR;
   case nir_op_ixor: return IBC_ALU_OP_XOR;
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

static unsigned
image_intrinsic_coord_components(const nir_intrinsic_instr *instr)
{
   switch (nir_intrinsic_image_dim(instr)) {
   case GLSL_SAMPLER_DIM_1D:
      return 1 + nir_intrinsic_image_array(instr);
   case GLSL_SAMPLER_DIM_2D:
   case GLSL_SAMPLER_DIM_RECT:
      return 2 + nir_intrinsic_image_array(instr);
   case GLSL_SAMPLER_DIM_3D:
   case GLSL_SAMPLER_DIM_CUBE:
      return 3;
   case GLSL_SAMPLER_DIM_BUF:
      return 1;
   case GLSL_SAMPLER_DIM_MS:
      return 2 + nir_intrinsic_image_array(instr);
   default:
      unreachable("Invalid image dimension");
   }
}

static void
nti_emit_intrinsic(struct nir_to_ibc_state *nti,
                   const nir_intrinsic_instr *instr)
{
   switch (nti->stage) {
   case MESA_SHADER_VERTEX:
      if (ibc_emit_nir_vs_intrinsic(nti, instr))
         return;
      break;
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

   ibc_ref dest = { .file = IBC_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_subgroup_invocation: {
      assert(nir_dest_is_divergent(instr->dest));
      ibc_reg *w_tmp_reg =
         ibc_hw_grf_reg_create(b->shader, b->simd_width * 2, 32);
      ibc_ref w_tmp = ibc_typed_ref(w_tmp_reg, IBC_TYPE_UW);

      ibc_builder_push_we_all(b, 8);
      ibc_build_alu1(b, IBC_ALU_OP_MOV, w_tmp, ibc_imm_v(0x76543210));
      ibc_builder_pop(b);

      if (b->simd_width > 8) {
         ibc_ref w_tmp_8 = w_tmp;
         ibc_hw_grf_simd_slice(&w_tmp_8.hw_grf, 8);
         ibc_builder_push_we_all(b, 8);
         ibc_build_alu2(b, IBC_ALU_OP_ADD, w_tmp_8, w_tmp, ibc_imm_uw(8));
         ibc_builder_pop(b);
      }

      if (b->simd_width > 16) {
         ibc_ref w_tmp_16 = w_tmp;
         ibc_hw_grf_simd_slice(&w_tmp_16.hw_grf, 16);
         ibc_builder_push_we_all(b, 16);
         ibc_build_alu2(b, IBC_ALU_OP_ADD, w_tmp_16, w_tmp, ibc_imm_uw(16));
         ibc_builder_pop(b);
      }

      dest = ibc_MOV(b, IBC_TYPE_UD, w_tmp);
      break;
   }

   case nir_intrinsic_vote_any: {
      assert(!nir_dest_is_divergent(instr->dest));
      ibc_ref flag = nti_initialize_flag(b, 0);
      ibc_MOV_to_flag(b, flag, BRW_CONDITIONAL_NZ,
                      ibc_nir_src(nti, instr->src[0], IBC_TYPE_FLAG));

      /* For some reason, the any/all predicates don't work properly with
       * SIMD32.  In particular, it appears that a SEL with a QtrCtrl of 2H
       * doesn't read the correct subset of the flag register and you end up
       * getting garbage in the second half.  Work around this by using a pair
       * of 1-wide MOVs and scattering the result.
       */
      ibc_builder_push_scalar(b);
      ibc_ref tmp = ibc_MOV_from_flag(b, IBC_TYPE_W,
         ibc_predicate_any(b->shader->simd_width), flag);
      ibc_builder_pop(b);

      /* Finally, IBC expects 1-bit booleans */
      ibc_builder_push_scalar(b);
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);
      ibc_MOV_to_flag(b, dest, BRW_CONDITIONAL_NZ, tmp);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_vote_all: {
      assert(!nir_dest_is_divergent(instr->dest));
      ibc_ref flag = nti_initialize_flag(b, -1);
      ibc_MOV_to_flag(b, flag, BRW_CONDITIONAL_NZ,
                      ibc_nir_src(nti, instr->src[0], IBC_TYPE_FLAG));

      /* For some reason, the any/all predicates don't work properly with
       * SIMD32.  In particular, it appears that a SEL with a QtrCtrl of 2H
       * doesn't read the correct subset of the flag register and you end up
       * getting garbage in the second half.  Work around this by using a pair
       * of 1-wide MOVs and scattering the result.
       */
      ibc_builder_push_scalar(b);
      ibc_ref tmp = ibc_MOV_from_flag(b, IBC_TYPE_W,
         ibc_predicate_all(b->shader->simd_width), flag);
      ibc_builder_pop(b);

      /* Finally, IBC expects 1-bit booleans */
      ibc_builder_push_scalar(b);
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);
      ibc_MOV_to_flag(b, dest, BRW_CONDITIONAL_NZ, tmp);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_ballot: {
      assert(!nir_dest_is_divergent(instr->dest));
      ibc_ref flag = nti_initialize_flag(b, 0);
      ibc_MOV_to_flag(b, flag, BRW_CONDITIONAL_NZ, ibc_imm_w(-1));
      flag.type = flag.reg->flag.bits <= 16 ? IBC_TYPE_UW : IBC_TYPE_UD;

      ibc_builder_push_scalar(b);
      dest = ibc_MOV(b, IBC_TYPE_UD, flag);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_read_invocation: {
      assert(!nir_dest_is_divergent(instr->dest));
      ibc_ref value = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UINT);
      value.logical.broadcast = true;
      value.logical.simd_channel = nir_src_as_uint(instr->src[1]);
      ibc_builder_push_scalar(b);
      dest = ibc_MOV(b, IBC_TYPE_UINT, value);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_reduce:
   case nir_intrinsic_inclusive_scan: {
      nir_op redop = nir_intrinsic_reduction_op(instr);
      ibc_ref src =
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
      if (scan_type == IBC_TYPE_FLAG) {
         /* TODO: Many scan ops can probably be better handled with bitfield
          * ops on the channel mask.
          */
         scan_type = IBC_TYPE_W;
      } else if (ibc_type_bit_size(scan_type) == 8) {
         scan_type = ibc_type_base_type(scan_type) | IBC_TYPE_16_BIT;
      }

      unsigned tmp_stride = ibc_type_byte_size(scan_type);
      unsigned tmp_size = b->simd_width * tmp_stride;
      unsigned tmp_align = MIN2(tmp_size, 32);
      ibc_reg *tmp_reg =
         ibc_hw_grf_reg_create(b->shader, tmp_size, tmp_align);
      ibc_ref tmp = ibc_typed_ref(tmp_reg, scan_type);

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
         ibc_ref mov_src = tmp;
         if (ibc_type_bit_size(scan_type) == 8) {
            /* Only take the bottom bits of the scan result in case it was a B
             * type which we upgraded to W.
             */
            mov_src.type = src.type;
         }
         dest = ibc_MOV(b, src.type, mov_src);
      }
      break;
   }

   case nir_intrinsic_load_uniform:
      if (nir_src_is_const(instr->src[0])) {
         const uint64_t offset_B = nir_intrinsic_base(instr) +
                                   nir_src_as_uint(instr->src[0]);
         const unsigned comp_size_B = nir_dest_bit_size(instr->dest) / 8;

         ibc_ref dest_comps[4] = { };
         for (unsigned c = 0; c < instr->num_components; c++) {
            const uint64_t comp_offset_B = offset_B + c * comp_size_B;

            assert(nti->payload->push.file == IBC_FILE_HW_GRF);
            if (comp_offset_B + comp_size_B >=
                nti->payload->push.reg->hw_grf.size)
               continue;

            dest_comps[c] = nti->payload->push;
            dest_comps[c].type = instr->dest.ssa.bit_size;
            dest_comps[c].hw_grf.byte += comp_offset_B;
            ibc_hw_grf_mul_stride(&dest_comps[c].hw_grf, 0);
         }

         ibc_builder_push_scalar(b);
         dest = ibc_VEC(b, dest_comps, instr->num_components);
         ibc_builder_pop(b);
      } else {
         unreachable("Indirect push constants not yet supported");
      }
      break;

   case nir_intrinsic_load_ubo:
      if (nir_src_is_const(instr->src[0]) && nir_src_is_const(instr->src[1])) {
         const unsigned bti = nir_src_as_uint(instr->src[0]);
         const uint64_t offset_B = nir_src_as_uint(instr->src[1]);
         const unsigned comp_size_B = nir_dest_bit_size(instr->dest) / 8;

         ibc_ref dest_comps[4] = { };
         for (unsigned c = 0; c < instr->num_components; c++) {
            const uint64_t comp_offset_B = offset_B + c * comp_size_B;

            /* First, try to look it up in one of the push ranges */
            for (unsigned i = 0; i < ARRAY_SIZE(nti->payload->ubo_push); i++) {
               ibc_ref block = nti->payload->ubo_push[i];
               if (block.file == IBC_FILE_NONE)
                  continue;

               ibc_intrinsic_instr *l =
                  ibc_instr_as_intrinsic(ibc_reg_ssa_instr(block.reg));
               assert(l->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO);
               assert(l->instr.simd_width == 1);

               const unsigned block_bti = ibc_ref_as_uint(l->src[1].ref);
               const unsigned block_offset_B = ibc_ref_as_uint(l->src[2].ref);
               const unsigned block_size_B =
                  ibc_type_byte_size(l->dest.type) * l->num_dest_comps;

               if (bti != block_bti ||
                   comp_offset_B < block_offset_B ||
                   comp_offset_B + comp_size_B > block_offset_B + block_size_B)
                  continue;

               dest_comps[c] = block;
               dest_comps[c].type = instr->dest.ssa.bit_size;
               dest_comps[c].hw_grf.byte += comp_offset_B - block_offset_B;
               ibc_hw_grf_mul_stride(&dest_comps[c].hw_grf, 0);
               break;
            }

            /* If we found it as a push constant, we're done. */
            if (dest_comps[c].file != IBC_FILE_NONE)
               continue;

            /* Fetch one cacheline at a time. */
            const unsigned block_size_B = 64;

            const unsigned comp_offset_in_block_B =
               comp_offset_B % block_size_B;
            const uint64_t block_offset_B =
               comp_offset_B - comp_offset_in_block_B;

            ibc_ref block =
               ibc_BTI_BLOCK_LOAD_UBO(b, ibc_null(IBC_TYPE_UD),
                                      ibc_nir_src(nti, instr->src[0],
                                                  IBC_TYPE_UD),
                                      ibc_imm_ud(block_offset_B),
                                      block_size_B);

            assert(block.file == IBC_FILE_HW_GRF);
            dest_comps[c] = block;
            dest_comps[c].type = instr->dest.ssa.bit_size;
            dest_comps[c].hw_grf.byte += comp_offset_in_block_B;
            ibc_hw_grf_mul_stride(&dest_comps[c].hw_grf, 0);

            nti->prog_data->has_ubo_pull = true;
         }

         ibc_builder_push_scalar(b);
         dest = ibc_VEC(b, dest_comps, instr->num_components);
         ibc_builder_pop(b);
         break;
      } else {
         ibc_builder_push_nir_dest_group(b, instr->dest);

         ibc_intrinsic_src srcs[IBC_TEX_NUM_SRCS] = {
            [IBC_TEX_SRC_SURFACE_BTI] = {
               .ref = ibc_uniformize(b, ibc_nir_src(nti, instr->src[0],
                                                    IBC_TYPE_UD)),
               .num_comps = 1,
            },
            [IBC_TEX_SRC_SAMPLER_BTI] = {
               .ref = ibc_imm_ud(0),
               .num_comps = 1,
            },
            [IBC_TEX_SRC_COORD] = {
               .ref = ibc_nir_src(nti, instr->src[1], IBC_TYPE_UD),
               .num_comps = 1,
            },
            [IBC_TEX_SRC_LOD] = {
               .ref = ibc_imm_ud(0),
               .num_comps = 1,
            },
         };

         /* We only have headerless rlen shortening on gen9+ */
         unsigned num_dest_comps =
            b->shader->devinfo->gen >= 9 ? instr->num_components : 4;
         assert(nir_dest_bit_size(instr->dest) == 32);
         dest = ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_TXF,
                                        IBC_TYPE_UD, num_dest_comps,
                                        srcs, IBC_TEX_NUM_SRCS);

         ibc_builder_pop(b);

         nti->prog_data->has_ubo_pull = true;
      }
      break;

   case nir_intrinsic_load_ssbo: {
      ibc_builder_push_nir_dest_group(b, instr->dest);

      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {
         [IBC_SURFACE_SRC_SURFACE_BTI] = {
            .ref = ibc_uniformize(b, ibc_nir_src(nti, instr->src[0],
                                                 IBC_TYPE_UD)),
            .num_comps = 1,
         },
         [IBC_SURFACE_SRC_ADDRESS] = {
            .ref = ibc_nir_src(nti, instr->src[1], IBC_TYPE_UD),
            .num_comps = 1,
         },
      };

      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD,
                                         instr->num_components);
      ibc_intrinsic_instr *load =
         ibc_build_intrinsic(b, IBC_INTRINSIC_OP_BTI_UNTYPED_READ,
                             dest, -1, instr->num_components,
                             srcs, IBC_SURFACE_NUM_SRCS);
      load->can_reorder = false;

      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_store_ssbo: {
      ibc_ref pred = {};
      if (b->shader->stage == MESA_SHADER_FRAGMENT) {
         brw_wm_prog_data(nti->prog_data)->has_side_effects = true;
         pred = ibc_emit_fs_sample_live_predicate(nti);
      }

      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {
         [IBC_SURFACE_SRC_SURFACE_BTI] = {
            .ref = ibc_uniformize(b, ibc_nir_src(nti, instr->src[1],
                                                 IBC_TYPE_UD)),
            .num_comps = 1,
         },
         [IBC_SURFACE_SRC_ADDRESS] = {
            .ref = ibc_nir_src(nti, instr->src[2], IBC_TYPE_UD),
            .num_comps = 1,
         },
         [IBC_SURFACE_SRC_DATA0] = {
            .ref = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD),
            .num_comps = instr->num_components,
         },
      };

      ibc_intrinsic_instr *store =
         ibc_build_intrinsic(b, IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE,
                             ibc_null(IBC_TYPE_UD), 0, 0,
                             srcs, IBC_SURFACE_NUM_SRCS);
      store->can_reorder = false;
      store->has_side_effects = true;

      if (pred.file != IBC_FILE_NONE) {
         ibc_instr_set_predicate(&store->instr, pred,
                                 IBC_PREDICATE_NORMAL);
      }
      break;
   }

   case nir_intrinsic_image_load:
   case nir_intrinsic_image_store:
   case nir_intrinsic_image_atomic_add:
   case nir_intrinsic_image_atomic_imin:
   case nir_intrinsic_image_atomic_umin:
   case nir_intrinsic_image_atomic_imax:
   case nir_intrinsic_image_atomic_umax:
   case nir_intrinsic_image_atomic_and:
   case nir_intrinsic_image_atomic_or:
   case nir_intrinsic_image_atomic_xor:
   case nir_intrinsic_image_atomic_exchange:
   case nir_intrinsic_image_atomic_comp_swap:
   case nir_intrinsic_bindless_image_load:
   case nir_intrinsic_bindless_image_store:
   case nir_intrinsic_bindless_image_atomic_add:
   case nir_intrinsic_bindless_image_atomic_imin:
   case nir_intrinsic_bindless_image_atomic_umin:
   case nir_intrinsic_bindless_image_atomic_imax:
   case nir_intrinsic_bindless_image_atomic_umax:
   case nir_intrinsic_bindless_image_atomic_and:
   case nir_intrinsic_bindless_image_atomic_or:
   case nir_intrinsic_bindless_image_atomic_xor:
   case nir_intrinsic_bindless_image_atomic_exchange:
   case nir_intrinsic_bindless_image_atomic_comp_swap: {
      ibc_builder_push_nir_intrinsic_dest_group(b, instr);

      /* Figure out the IBC intrinsic op */
      bool is_bindless;
      enum ibc_intrinsic_op op;
      switch (instr->intrinsic) {
      case nir_intrinsic_image_load:
         op = IBC_INTRINSIC_OP_BTI_TYPED_READ;
         is_bindless = false;
         break;
      case nir_intrinsic_bindless_image_load:
         op = IBC_INTRINSIC_OP_BTI_TYPED_READ;
         is_bindless = true;
         break;
      case nir_intrinsic_image_store:
         op = IBC_INTRINSIC_OP_BTI_TYPED_WRITE;
         is_bindless = false;
         break;
      case nir_intrinsic_bindless_image_store:
         op = IBC_INTRINSIC_OP_BTI_TYPED_WRITE;
         is_bindless = true;
         break;
      case nir_intrinsic_image_atomic_add:
      case nir_intrinsic_image_atomic_imin:
      case nir_intrinsic_image_atomic_umin:
      case nir_intrinsic_image_atomic_imax:
      case nir_intrinsic_image_atomic_umax:
      case nir_intrinsic_image_atomic_and:
      case nir_intrinsic_image_atomic_or:
      case nir_intrinsic_image_atomic_xor:
      case nir_intrinsic_image_atomic_exchange:
      case nir_intrinsic_image_atomic_comp_swap:
         op = IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC;
         is_bindless = false;
         break;
      case nir_intrinsic_bindless_image_atomic_add:
      case nir_intrinsic_bindless_image_atomic_imin:
      case nir_intrinsic_bindless_image_atomic_umin:
      case nir_intrinsic_bindless_image_atomic_imax:
      case nir_intrinsic_bindless_image_atomic_umax:
      case nir_intrinsic_bindless_image_atomic_and:
      case nir_intrinsic_bindless_image_atomic_or:
      case nir_intrinsic_bindless_image_atomic_xor:
      case nir_intrinsic_bindless_image_atomic_exchange:
      case nir_intrinsic_bindless_image_atomic_comp_swap:
         op = IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC;
         is_bindless = true;
         break;
      default:
         unreachable("Invalid image intrinsic");
      }

      ibc_ref pred = {};
      if (b->shader->stage == MESA_SHADER_FRAGMENT) {
         brw_wm_prog_data(nti->prog_data)->has_side_effects = true;
         if (op != IBC_INTRINSIC_OP_BTI_TYPED_READ)
            pred = ibc_emit_fs_sample_live_predicate(nti);
      }

      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {};

      /* Set up the BTI or handle */
      if (is_bindless) {
         srcs[IBC_SURFACE_SRC_SURFACE_HANDLE] = (ibc_intrinsic_src) {
            .ref = ibc_uniformize(b, ibc_nir_src(nti, instr->src[0],
                                                 IBC_TYPE_UD)),
            .num_comps = 1,
         };
      } else {
         srcs[IBC_SURFACE_SRC_SURFACE_BTI] = (ibc_intrinsic_src) {
            .ref = ibc_uniformize(b, ibc_nir_src(nti, instr->src[0],
                                                 IBC_TYPE_UD)),
            .num_comps = 1,
         };
      }

      srcs[IBC_SURFACE_SRC_ADDRESS] = (ibc_intrinsic_src) {
         .ref = ibc_nir_src(nti, instr->src[1], IBC_TYPE_UD),
         .num_comps = image_intrinsic_coord_components(instr),
      };

      int aop = -1;
      if (op == IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC) {
         aop = brw_aop_for_nir_intrinsic(instr);
         srcs[IBC_SURFACE_SRC_ATOMIC_OP] = (ibc_intrinsic_src) {
            .ref = ibc_imm_ud(aop),
            .num_comps = 1,
         };
      }

      /* If we're not an atomic aop == -1 so the last two conditions are true
       * vacuously.
       */
      if (op != IBC_INTRINSIC_OP_BTI_TYPED_READ &&
          aop != BRW_AOP_INC && aop != BRW_AOP_DEC) {
         srcs[IBC_SURFACE_SRC_DATA0] = (ibc_intrinsic_src) {
            .ref = ibc_nir_src(nti, instr->src[3], IBC_TYPE_UD),
            .num_comps = nir_intrinsic_src_components(instr, 3),
         };
      }

      if (aop == BRW_AOP_CMPWR) {
         srcs[IBC_SURFACE_SRC_DATA1] = (ibc_intrinsic_src) {
            .ref = ibc_nir_src(nti, instr->src[4], IBC_TYPE_UD),
            .num_comps = 1,
         };
      }

      dest = ibc_null(IBC_TYPE_UD);
      unsigned num_dest_comps = 0;
      if (op != IBC_INTRINSIC_OP_BTI_TYPED_WRITE) {
         num_dest_comps = nir_dest_num_components(instr->dest);
         if (pred.file != IBC_FILE_NONE) {
            dest = ibc_UNDEF(b, IBC_TYPE_UD, num_dest_comps);
         } else {
            dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, num_dest_comps);
         }
      }

      ibc_intrinsic_instr *image_op =
         ibc_build_intrinsic(b, op, dest, -1, num_dest_comps,
                             srcs, IBC_SURFACE_NUM_SRCS);
      image_op->can_reorder = false;
      image_op->has_side_effects = (op != IBC_INTRINSIC_OP_BTI_TYPED_READ);

      if (pred.file != IBC_FILE_NONE) {
         assert(b->simd_width > 0);
         ibc_instr_set_predicate(&image_op->instr, pred,
                                 IBC_PREDICATE_NORMAL);
      }

      ibc_builder_pop(b);
      break;
   }

   default:
      unreachable("Unhandled NIR intrinsic");
   }

   if (nir_intrinsic_infos[instr->intrinsic].has_dest) {
      ibc_builder_push_nir_dest_group(b, instr->dest);
      ibc_write_nir_dest(nti, &instr->dest, dest);
      ibc_builder_pop(b);
   } else {
      assert(dest.file == IBC_FILE_NONE);
   }
}

static void
nti_emit_load_const(struct nir_to_ibc_state *nti,
                    const nir_load_const_instr *instr)
{
   ibc_builder *b = &nti->b;

   ibc_builder_push_scalar(b);

   enum ibc_type type = instr->def.bit_size >= 8 ?
      (IBC_TYPE_UINT | instr->def.bit_size) : IBC_TYPE_FLAG;

   ibc_ref imm_srcs[4];

   for (unsigned i = 0; i < instr->def.num_components; i++) {
      imm_srcs[i] = (ibc_ref) {
         .file = IBC_FILE_IMM,
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

   ibc_ref dest = ibc_VEC(b, imm_srcs, instr->def.num_components);
   ibc_write_nir_ssa_def(nti, &instr->def, dest);

   ibc_builder_pop(b);
}

static inline ibc_flow_instr *
emit_flow_for_nir_jump_type(struct nir_to_ibc_state *nti, nir_jump_type ntype,
                            ibc_ref cond, enum ibc_predicate predicate)
{
   ibc_builder *b = &nti->b;

   switch (ntype) {
   case nir_jump_break:
      return ibc_BREAK(b, cond, predicate, &nti->breaks);

   case nir_jump_continue:
      return ibc_CONT(b, cond, predicate, nti->_do);

   default:
      unreachable("Unsupported jump instruction type");
   }
}

static void
nti_emit_jump(struct nir_to_ibc_state *nti,
              const nir_jump_instr *njump)
{
   emit_flow_for_nir_jump_type(nti, njump->type,
                               ibc_null(IBC_TYPE_FLAG),
                               IBC_PREDICATE_NONE);
}

static void
nti_emit_ssa_undef(struct nir_to_ibc_state *nti,
                   const nir_ssa_undef_instr *nundef)
{
   ibc_builder *b = &nti->b;

   ibc_builder_push_scalar(b);
   ibc_write_nir_ssa_def(nti, &nundef->def,
      ibc_builder_new_logical_reg(b, nundef->def.bit_size,
                                  nundef->def.num_components));
   ibc_builder_pop(b);
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
      case nir_instr_type_tex:
         nti_emit_tex(nti, nir_instr_as_tex(instr));
         break;
      case nir_instr_type_intrinsic:
         nti_emit_intrinsic(nti, nir_instr_as_intrinsic(instr));
         break;
      case nir_instr_type_load_const:
         nti_emit_load_const(nti, nir_instr_as_load_const(instr));
         break;
      case nir_instr_type_jump:
         nti_emit_jump(nti, nir_instr_as_jump(instr));
         break;
      case nir_instr_type_ssa_undef:
         nti_emit_ssa_undef(nti, nir_instr_as_ssa_undef(instr));
         break;
      default:
         unreachable("Unsupported instruction type");
      }
   }

   _mesa_hash_table_insert(nti->nir_block_to_ibc,
                           block, nti->b.block_start);
}

static void
nti_emit_cf_list(struct nir_to_ibc_state *nti,
                 const struct exec_list *cf_list);

static inline bool
nir_cf_list_is_simple_jump(struct exec_list *cf_list, nir_jump_type *type)
{
   if (!exec_list_is_singular(cf_list))
      return false;

   struct exec_node *head = exec_list_get_head(cf_list);
   nir_block *block =
      nir_cf_node_as_block(exec_node_data(nir_cf_node, head, node));
   if (!exec_list_is_singular(&block->instr_list))
      return false;

   nir_instr *last_instr = nir_block_last_instr(block);
   if (last_instr->type != nir_instr_type_jump)
      return false;

   *type = nir_instr_as_jump(last_instr)->type;
   return true;
}

static void
nti_emit_if(struct nir_to_ibc_state *nti, nir_if *nif)
{
   ibc_builder *b = &nti->b;

   /* This really should never happen but it's theoretically possible. */
   if (nir_cf_list_is_empty_block(&nif->then_list) &&
       nir_cf_list_is_empty_block(&nif->else_list))
      return;

   ibc_ref cond = ibc_nir_src(nti, nif->condition, IBC_TYPE_FLAG);

   nir_jump_type ntype;
   if (nir_cf_list_is_simple_jump(&nif->then_list, &ntype)) {
      emit_flow_for_nir_jump_type(nti, ntype, cond, IBC_PREDICATE_NORMAL);
      nti_emit_cf_list(nti, &nif->else_list);
      return;
   }

   if (nir_cf_list_is_simple_jump(&nif->else_list, &ntype)) {
      emit_flow_for_nir_jump_type(nti, ntype, cond, IBC_PREDICATE_NOT);
      nti_emit_cf_list(nti, &nif->else_list);
      return;
   }

   if (nir_cf_list_is_empty_block(&nif->then_list)) {
      ibc_flow_instr *_if = ibc_IF(b, cond, IBC_PREDICATE_NOT);
      nti_emit_cf_list(nti, &nif->else_list);
      ibc_ENDIF(b, _if, NULL);
      return;
   }

   ibc_flow_instr *_if = ibc_IF(b, cond, IBC_PREDICATE_NORMAL);
   nti_emit_cf_list(nti, &nif->then_list);

   ibc_flow_instr *_else = NULL;
   if (!nir_cf_list_is_empty_block(&nif->else_list)) {
      _else = ibc_ELSE(b, _if);
      nti_emit_cf_list(nti, &nif->else_list);
   }

   ibc_ENDIF(b, _if, _else);
}

static void
nti_emit_loop(struct nir_to_ibc_state *nti, nir_loop *nloop)
{
   ibc_builder *b = &nti->b;

   /* Save off the state of the outer loop */
   ibc_flow_instr *old_do = nti->_do;
   struct list_head old_breaks;
   list_replace(&nti->breaks, &old_breaks);
   list_inithead(&nti->breaks);

   nti->_do = ibc_DO(b);

   nti_emit_cf_list(nti, &nloop->body);

   ibc_WHILE(b, ibc_null(IBC_TYPE_FLAG), IBC_PREDICATE_NONE,
             nti->_do, &nti->breaks);

   /* Restore off the state of the outer loop */
   nti->_do = old_do;
   assert(list_is_empty(&nti->breaks));
   list_replace(&old_breaks, &nti->breaks);
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
   ibc_shader *shader = ibc_shader_create(mem_ctx, devinfo, stage,
                                          dispatch_size);

   shader->use_vmask = (stage == MESA_SHADER_FRAGMENT);
   shader->has_packed_dispatch =
      brw_stage_has_packed_dispatch(devinfo, stage, prog_data);

   *nti = (struct nir_to_ibc_state) {
      .mem_ctx = mem_ctx,
      .stage = stage,
      .key = key,
      .prog_data = prog_data,
      .stage_state = stage_state,
   };
   list_inithead(&nti->breaks);

   ibc_builder_init(&nti->b, shader);
   ibc_START(&nti->b);
}

void
ibc_emit_nir_shader(struct nir_to_ibc_state *nti,
                    const nir_shader *nir)
{
   nir_function_impl *impl = nir_shader_get_entrypoint((nir_shader *)nir);

   nti->ssa_to_reg =
      ralloc_array(nti->mem_ctx, const ibc_reg *, impl->ssa_alloc);

   nir_index_local_regs(impl);
   nti->reg_to_reg =
      ralloc_array(nti->mem_ctx, const ibc_reg *, impl->reg_alloc);
   nir_foreach_register(nreg, &impl->registers) {
      const uint8_t simd_group = nreg->divergent ? nti->b.simd_group : 0;
      const uint8_t simd_width = nreg->divergent ? nti->b.simd_width : 1;
      ibc_reg *ireg = ibc_logical_reg_create(nti->b.shader,
                                             nreg->bit_size,
                                             nreg->num_components,
                                             simd_group, simd_width);
      ireg->is_wlr = false;
      nti->reg_to_reg[nreg->index] = ireg;
   }

   nti->nir_block_to_ibc = _mesa_pointer_hash_table_create(nti->mem_ctx);

   nti_emit_cf_list(nti, &impl->body);
}

ibc_shader *
nir_to_ibc_state_finish(struct nir_to_ibc_state *nti)
{
   ibc_END(&nti->b);
   return nti->b.shader;
}
