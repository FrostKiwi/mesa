/*
 * Copyright © 2016 Intel Corporation
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

#include <math.h>
#include "vtn_private.h"
#include "spirv_info.h"
#include "nir_builtin_builder.h"

/*
 * Normally, column vectors in SPIR-V correspond to a single NIR SSA
 * definition. But for matrix multiplies, we want to do one routine for
 * multiplying a matrix by a matrix and then pretend that vectors are matrices
 * with one column. So we "wrap" these things, and unwrap the result before we
 * send it off.
 */

static struct vtn_ssa_value *
wrap_matrix(struct vtn_builder *b, struct vtn_ssa_value *val)
{
   if (val == NULL)
      return NULL;

   if (glsl_type_is_matrix(val->type))
      return val;

   struct vtn_ssa_value *dest = rzalloc(b, struct vtn_ssa_value);
   dest->type = val->type;
   dest->elems = ralloc_array(b, struct vtn_ssa_value *, 1);
   dest->elems[0] = val;

   return dest;
}

static struct vtn_ssa_value *
unwrap_matrix(struct vtn_ssa_value *val)
{
   if (glsl_type_is_matrix(val->type))
         return val;

   return val->elems[0];
}

static struct vtn_ssa_value *
matrix_multiply(struct vtn_builder *b,
                struct vtn_ssa_value *_src0, struct vtn_ssa_value *_src1)
{

   struct vtn_ssa_value *src0 = wrap_matrix(b, _src0);
   struct vtn_ssa_value *src1 = wrap_matrix(b, _src1);
   struct vtn_ssa_value *src0_transpose = wrap_matrix(b, _src0->transposed);
   struct vtn_ssa_value *src1_transpose = wrap_matrix(b, _src1->transposed);

   unsigned src0_rows = glsl_get_vector_elements(src0->type);
   unsigned src0_columns = glsl_get_matrix_columns(src0->type);
   unsigned src1_columns = glsl_get_matrix_columns(src1->type);

   const struct glsl_type *dest_type;
   if (src1_columns > 1) {
      dest_type = glsl_matrix_type(glsl_get_base_type(src0->type),
                                   src0_rows, src1_columns);
   } else {
      dest_type = glsl_vector_type(glsl_get_base_type(src0->type), src0_rows);
   }
   struct vtn_ssa_value *dest = vtn_create_ssa_value(b, dest_type);

   dest = wrap_matrix(b, dest);

   bool transpose_result = false;
   if (src0_transpose && src1_transpose) {
      /* transpose(A) * transpose(B) = transpose(B * A) */
      src1 = src0_transpose;
      src0 = src1_transpose;
      src0_transpose = NULL;
      src1_transpose = NULL;
      transpose_result = true;
   }

   if (src0_transpose && !src1_transpose &&
       glsl_get_base_type(src0->type) == GLSL_TYPE_FLOAT) {
      /* We already have the rows of src0 and the columns of src1 available,
       * so we can just take the dot product of each row with each column to
       * get the result.
       */

      for (unsigned i = 0; i < src1_columns; i++) {
         nir_ssa_def *vec_src[4];
         for (unsigned j = 0; j < src0_rows; j++) {
            vec_src[j] = nir_fdot(&b->nb, src0_transpose->elems[j]->def,
                                          src1->elems[i]->def);
         }
         dest->elems[i]->def = nir_vec(&b->nb, vec_src, src0_rows);
      }
   } else {
      /* We don't handle the case where src1 is transposed but not src0, since
       * the general case only uses individual components of src1 so the
       * optimizer should chew through the transpose we emitted for src1.
       */

      for (unsigned i = 0; i < src1_columns; i++) {
         /* dest[i] = sum(src0[j] * src1[i][j] for all j) */
         dest->elems[i]->def =
            nir_fmul(&b->nb, src0->elems[0]->def,
                     nir_channel(&b->nb, src1->elems[i]->def, 0));
         for (unsigned j = 1; j < src0_columns; j++) {
            dest->elems[i]->def =
               nir_fadd(&b->nb, dest->elems[i]->def,
                        nir_fmul(&b->nb, src0->elems[j]->def,
                                 nir_channel(&b->nb, src1->elems[i]->def, j)));
         }
      }
   }

   dest = unwrap_matrix(dest);

   if (transpose_result)
      dest = vtn_ssa_transpose(b, dest);

   return dest;
}

static struct vtn_ssa_value *
mat_times_scalar(struct vtn_builder *b,
                 struct vtn_ssa_value *mat,
                 nir_ssa_def *scalar)
{
   struct vtn_ssa_value *dest = vtn_create_ssa_value(b, mat->type);
   for (unsigned i = 0; i < glsl_get_matrix_columns(mat->type); i++) {
      if (glsl_base_type_is_integer(glsl_get_base_type(mat->type)))
         dest->elems[i]->def = nir_imul(&b->nb, mat->elems[i]->def, scalar);
      else
         dest->elems[i]->def = nir_fmul(&b->nb, mat->elems[i]->def, scalar);
   }

   return dest;
}

static void
vtn_handle_matrix_alu(struct vtn_builder *b, SpvOp opcode,
                      struct vtn_value *dest,
                      struct vtn_ssa_value *src0, struct vtn_ssa_value *src1)
{
   switch (opcode) {
   case SpvOpFNegate: {
      dest->ssa = vtn_create_ssa_value(b, src0->type);
      unsigned cols = glsl_get_matrix_columns(src0->type);
      for (unsigned i = 0; i < cols; i++)
         dest->ssa->elems[i]->def = nir_fneg(&b->nb, src0->elems[i]->def);
      break;
   }

   case SpvOpFAdd: {
      dest->ssa = vtn_create_ssa_value(b, src0->type);
      unsigned cols = glsl_get_matrix_columns(src0->type);
      for (unsigned i = 0; i < cols; i++)
         dest->ssa->elems[i]->def =
            nir_fadd(&b->nb, src0->elems[i]->def, src1->elems[i]->def);
      break;
   }

   case SpvOpFSub: {
      dest->ssa = vtn_create_ssa_value(b, src0->type);
      unsigned cols = glsl_get_matrix_columns(src0->type);
      for (unsigned i = 0; i < cols; i++)
         dest->ssa->elems[i]->def =
            nir_fsub(&b->nb, src0->elems[i]->def, src1->elems[i]->def);
      break;
   }

   case SpvOpTranspose:
      dest->ssa = vtn_ssa_transpose(b, src0);
      break;

   case SpvOpMatrixTimesScalar:
      if (src0->transposed) {
         dest->ssa = vtn_ssa_transpose(b, mat_times_scalar(b, src0->transposed,
                                                           src1->def));
      } else {
         dest->ssa = mat_times_scalar(b, src0, src1->def);
      }
      break;

   case SpvOpVectorTimesMatrix:
   case SpvOpMatrixTimesVector:
   case SpvOpMatrixTimesMatrix:
      if (opcode == SpvOpVectorTimesMatrix) {
         dest->ssa = matrix_multiply(b, vtn_ssa_transpose(b, src1), src0);
      } else {
         dest->ssa = matrix_multiply(b, src0, src1);
      }
      break;

   default: vtn_fail_with_opcode("unknown matrix opcode", opcode);
   }
}

static void
handle_rounding_mode(struct vtn_builder *b, struct vtn_value *val, int member,
                     const struct vtn_decoration *dec, void *_out_rounding_mode)
{
   nir_rounding_mode *out_rounding_mode = _out_rounding_mode;
   assert(dec->scope == VTN_DEC_DECORATION);
   if (dec->decoration != SpvDecorationFPRoundingMode)
      return;
   switch (dec->operands[0]) {
   case SpvFPRoundingModeRTE:
      *out_rounding_mode = nir_rounding_mode_rtne;
      break;
   case SpvFPRoundingModeRTZ:
      *out_rounding_mode = nir_rounding_mode_rtz;
      break;
   case SpvFPRoundingModeRTP:
      *out_rounding_mode = nir_rounding_mode_ru;
      break;
   case SpvFPRoundingModeRTN:
      *out_rounding_mode = nir_rounding_mode_rd;
      break;
   default:
      unreachable("Not supported rounding mode");
      break;
   }
}

static void
handle_saturation(struct vtn_builder *b, struct vtn_value *val, int member,
                  const struct vtn_decoration *dec, void *_out_saturate)
{
   bool *out_saturate = _out_saturate;
   assert(dec->scope == VTN_DEC_DECORATION);
   if (dec->decoration != SpvDecorationSaturatedConversion)
      return;
   *out_saturate = true;
}

nir_op
vtn_nir_alu_op_for_spirv_opcode(struct vtn_builder *b, struct vtn_value *val,
                                SpvOp opcode, bool *swap,
                                unsigned src_bit_size, unsigned dst_bit_size)
{
   /* Indicates that the first two arguments should be swapped.  This is
    * used for implementing greater-than and less-than-or-equal.
    */
   *swap = false;

   switch (opcode) {
   case SpvOpSNegate:            return nir_op_ineg;
   case SpvOpFNegate:            return nir_op_fneg;
   case SpvOpNot:                return nir_op_inot;
   case SpvOpIAdd:               return nir_op_iadd;
   case SpvOpFAdd:               return nir_op_fadd;
   case SpvOpISub:               return nir_op_isub;
   case SpvOpFSub:               return nir_op_fsub;
   case SpvOpIMul:               return nir_op_imul;
   case SpvOpFMul:               return nir_op_fmul;
   case SpvOpUDiv:               return nir_op_udiv;
   case SpvOpSDiv:               return nir_op_idiv;
   case SpvOpFDiv:               return nir_op_fdiv;
   case SpvOpUMod:               return nir_op_umod;
   case SpvOpSMod:               return nir_op_imod;
   case SpvOpFMod:               return nir_op_fmod;
   case SpvOpSRem:               return nir_op_irem;
   case SpvOpFRem:               return nir_op_frem;

   case SpvOpShiftRightLogical:     return nir_op_ushr;
   case SpvOpShiftRightArithmetic:  return nir_op_ishr;
   case SpvOpShiftLeftLogical:      return nir_op_ishl;
   case SpvOpLogicalOr:             return nir_op_ior;
   case SpvOpLogicalEqual:          return nir_op_ieq;
   case SpvOpLogicalNotEqual:       return nir_op_ine;
   case SpvOpLogicalAnd:            return nir_op_iand;
   case SpvOpLogicalNot:            return nir_op_inot;
   case SpvOpBitwiseOr:             return nir_op_ior;
   case SpvOpBitwiseXor:            return nir_op_ixor;
   case SpvOpBitwiseAnd:            return nir_op_iand;
   case SpvOpSelect:                return nir_op_bcsel;
   case SpvOpIEqual:                return nir_op_ieq;

   case SpvOpBitFieldInsert:        return nir_op_bitfield_insert;
   case SpvOpBitFieldSExtract:      return nir_op_ibitfield_extract;
   case SpvOpBitFieldUExtract:      return nir_op_ubitfield_extract;
   case SpvOpBitReverse:            return nir_op_bitfield_reverse;
   case SpvOpBitCount:              return nir_op_bit_count;

   case SpvOpUCountLeadingZerosINTEL: return nir_op_uclz;
   /* SpvOpUCountTrailingZerosINTEL is handled elsewhere. */
   case SpvOpAbsISubINTEL:          return nir_op_uabs_isub;
   case SpvOpAbsUSubINTEL:          return nir_op_uabs_usub;
   case SpvOpIAddSatINTEL:          return nir_op_iadd_sat;
   case SpvOpUAddSatINTEL:          return nir_op_uadd_sat;
   case SpvOpIAverageINTEL:         return nir_op_ihadd;
   case SpvOpUAverageINTEL:         return nir_op_uhadd;
   case SpvOpIAverageRoundedINTEL:  return nir_op_irhadd;
   case SpvOpUAverageRoundedINTEL:  return nir_op_urhadd;
   case SpvOpISubSatINTEL:          return nir_op_isub_sat;
   case SpvOpUSubSatINTEL:          return nir_op_usub_sat;
   case SpvOpIMul32x16INTEL:        return nir_op_imul_32x16;
   case SpvOpUMul32x16INTEL:        return nir_op_umul_32x16;

   /* The ordered / unordered operators need special implementation besides
    * the logical operator to use since they also need to check if operands are
    * ordered.
    */
   case SpvOpFOrdEqual:                            return nir_op_feq;
   case SpvOpFUnordEqual:                          return nir_op_feq;
   case SpvOpINotEqual:                            return nir_op_ine;
   case SpvOpLessOrGreater:                        /* Deprecated, use OrdNotEqual */
   case SpvOpFOrdNotEqual:                         return nir_op_fne;
   case SpvOpFUnordNotEqual:                       return nir_op_fne;
   case SpvOpULessThan:                            return nir_op_ult;
   case SpvOpSLessThan:                            return nir_op_ilt;
   case SpvOpFOrdLessThan:                         return nir_op_flt;
   case SpvOpFUnordLessThan:                       return nir_op_flt;
   case SpvOpUGreaterThan:          *swap = true;  return nir_op_ult;
   case SpvOpSGreaterThan:          *swap = true;  return nir_op_ilt;
   case SpvOpFOrdGreaterThan:       *swap = true;  return nir_op_flt;
   case SpvOpFUnordGreaterThan:     *swap = true;  return nir_op_flt;
   case SpvOpULessThanEqual:        *swap = true;  return nir_op_uge;
   case SpvOpSLessThanEqual:        *swap = true;  return nir_op_ige;
   case SpvOpFOrdLessThanEqual:     *swap = true;  return nir_op_fge;
   case SpvOpFUnordLessThanEqual:   *swap = true;  return nir_op_fge;
   case SpvOpUGreaterThanEqual:                    return nir_op_uge;
   case SpvOpSGreaterThanEqual:                    return nir_op_ige;
   case SpvOpFOrdGreaterThanEqual:                 return nir_op_fge;
   case SpvOpFUnordGreaterThanEqual:               return nir_op_fge;

   /* Conversions: */
   case SpvOpQuantizeToF16:                        return nir_op_fquantize2f16;
   case SpvOpSConvert:
   case SpvOpFConvert:
   case SpvOpUConvert: {
      nir_alu_type src_type;
      nir_alu_type dst_type;

      switch (opcode) {
      case SpvOpFConvert:
         src_type = dst_type = nir_type_float;
         break;
      case SpvOpSConvert:
         src_type = dst_type = nir_type_int;
         break;
      case SpvOpUConvert:
         src_type = dst_type = nir_type_uint;
         break;
      default:
         unreachable("unknown conversion type");
      }

      src_type |= src_bit_size;
      dst_type |= dst_bit_size;
      return nir_type_conversion_op(src_type, dst_type, nir_rounding_mode_undef);
   }


   /* Derivatives: */
   case SpvOpDPdx:         return nir_op_fddx;
   case SpvOpDPdy:         return nir_op_fddy;
   case SpvOpDPdxFine:     return nir_op_fddx_fine;
   case SpvOpDPdyFine:     return nir_op_fddy_fine;
   case SpvOpDPdxCoarse:   return nir_op_fddx_coarse;
   case SpvOpDPdyCoarse:   return nir_op_fddy_coarse;

   case SpvOpIsNormal:     return nir_op_fisnormal;
   case SpvOpIsFinite:     return nir_op_fisfinite;

   default:
      vtn_fail("No NIR equivalent: %u", opcode);
   }
}

static void
handle_no_contraction(struct vtn_builder *b, struct vtn_value *val, int member,
                      const struct vtn_decoration *dec, void *_void)
{
   vtn_assert(dec->scope == VTN_DEC_DECORATION);
   if (dec->decoration != SpvDecorationNoContraction)
      return;

   b->nb.exact = true;
}

static void
handle_no_wrap(struct vtn_builder *b, struct vtn_value *val, int member,
               const struct vtn_decoration *dec, void *_alu)
{
   nir_alu_instr *alu = _alu;
   switch (dec->decoration) {
   case SpvDecorationNoSignedWrap:
      alu->no_signed_wrap = true;
      break;
   case SpvDecorationNoUnsignedWrap:
      alu->no_unsigned_wrap = true;
      break;
   default:
      /* Do nothing. */
      break;
   }
}

static nir_ssa_def *
round_float(struct vtn_builder *b, nir_rounding_mode round,
            nir_ssa_def *src, nir_alu_type dst_type,
            unsigned int dst_bit_size)
{
   unsigned int src_bit_size = src->bit_size;
   unsigned int max_bit_size = MAX2(src->bit_size, dst_bit_size);

   nir_op low_conv = nir_type_conversion_op(nir_type_float | src_bit_size,
                                            dst_type | dst_bit_size,
                                            nir_rounding_mode_undef);
   nir_op high_conv = nir_type_conversion_op(nir_type_float | dst_bit_size,
                                             dst_type | src_bit_size,
                                             nir_rounding_mode_undef);

   nir_ssa_def *rets[NIR_MAX_VEC_COMPONENTS] = { 0 };

   for (unsigned i = 0; i < src->num_components; i++) {
      nir_ssa_def *comp = nir_channel(&b->nb, src, i);

      switch (round) {
      case nir_rounding_mode_ru: {
         if (dst_type == nir_type_int || dst_type == nir_type_uint) {
            rets[i] = nir_fceil(&b->nb, comp);
            break;
         }

         /* If lower-precision conversion results in a lower value, push it
         * up one ULP. */
         nir_ssa_def *lower_prec =
            nir_build_alu(&b->nb, low_conv, comp, NULL, NULL, NULL);
         nir_ssa_def *roundtrip =
            nir_build_alu(&b->nb, high_conv, lower_prec, NULL, NULL, NULL);
         nir_ssa_def *cmp = nir_flt(&b->nb, roundtrip, comp);

         rets[i] = nir_bcsel(&b->nb, cmp,
                        nir_nextafter(&b->nb, lower_prec,
                                       nir_imm_floatN_t(&b->nb, INFINITY, dst_bit_size)),
                           lower_prec);
         break;
      }
      case nir_rounding_mode_rd: {
         if (dst_type == nir_type_int || dst_type == nir_type_uint) {
            rets[i] = nir_ffloor(&b->nb, comp);
            break;
         }

         /* If lower-precision conversion results in a higher value, push it
         * down one ULP. */
         nir_ssa_def *lower_prec =
            nir_build_alu(&b->nb, low_conv, comp, NULL, NULL, NULL);
         nir_ssa_def *roundtrip =
            nir_build_alu(&b->nb, high_conv, lower_prec, NULL, NULL, NULL);
         nir_ssa_def *cmp = nir_flt(&b->nb, comp, roundtrip);

         rets[i] = nir_bcsel(&b->nb, cmp,
                             nir_nextafter(&b->nb, lower_prec,
                                           nir_imm_floatN_t(&b->nb, -INFINITY, dst_bit_size)),
                             lower_prec);
         break;
      }
      case nir_rounding_mode_rtz: {
         nir_ssa_def *pos =
            nir_flt(&b->nb, comp, nir_imm_floatN_t(&b->nb, 0.0f, max_bit_size));
         rets[i] = nir_bcsel(&b->nb, pos,
                             round_float(b, nir_rounding_mode_ru, comp,
                                        dst_type, dst_bit_size),
                             round_float(b, nir_rounding_mode_rd, comp,
                                        dst_type, dst_bit_size));
         break;
      }
      case nir_rounding_mode_undef:
      case nir_rounding_mode_rtne:
         rets[i] = comp;
         break;
      default:
         unreachable("unexpected rounding mode");
      }
   }

   return nir_vec(&b->nb, rets, src->num_components);
}

/**
 * Clamp the source value into the widest representatble range of the
 * destination type with cmp + bcsel.
 */
static nir_ssa_def *
clamp_to_range(struct vtn_builder *b, nir_ssa_def *src, nir_alu_type src_type,
               nir_alu_type dst_type, unsigned int dst_bit_size)
{
   /* limits of the destination type, expressed in the source type */
   nir_ssa_def *low = NULL, *high = NULL;
   switch (dst_type) {
   case nir_type_int: {
      unsigned long long ilow = (1ULL << dst_bit_size) - 1;
      unsigned long long ihigh = (1ULL << (dst_bit_size - 1)) - 1;
      if (src_type == nir_type_int) {
         low = nir_imm_intN_t(&b->nb, ilow, src->bit_size);
         high = nir_imm_intN_t(&b->nb, ihigh, src->bit_size);
      } else if (src_type == nir_type_uint) {
         assert(src->bit_size >= dst_bit_size);
         high = nir_imm_intN_t(&b->nb, ihigh, src->bit_size);
      } else {
         low = nir_imm_floatN_t(&b->nb, ilow, src->bit_size);
         high = nir_imm_floatN_t(&b->nb, ihigh, src->bit_size);
      }
      break;
   }
   case nir_type_uint: {
      unsigned long long uhigh = (1ULL << dst_bit_size) - 1;
      if (src_type != nir_type_float) {
         low = nir_imm_intN_t(&b->nb, 0, src->bit_size);
         if (src_type == nir_type_uint || src->bit_size > dst_bit_size)
            high = nir_imm_intN_t(&b->nb, uhigh, src->bit_size);
      } else {
         low = nir_imm_floatN_t(&b->nb, 0.0f, src->bit_size);
         high = nir_imm_floatN_t(&b->nb, uhigh, src->bit_size);
      }
      break;
   }
   case nir_type_float: {
      double flow, fhigh;
      if (dst_bit_size == 16) {
         flow = -65504.0f;
         fhigh = 65504.0f;
      } else {
         flow = -FLT_MAX;
         fhigh = FLT_MAX;
      }

      if (src_type != nir_type_float) {
         low = nir_imm_intN_t(&b->nb, flow, src->bit_size);
         high = nir_imm_intN_t(&b->nb, fhigh, src->bit_size);
      } else {
         low = nir_imm_floatN_t(&b->nb, flow, src->bit_size);
         high = nir_imm_floatN_t(&b->nb, fhigh, src->bit_size);
      }
      break;
   }
   default:
      unreachable("clamping to unknown type");
      break;
   }

   nir_ssa_def *rets[NIR_MAX_VEC_COMPONENTS] = { 0 };

   /* src-in-range comparators for bcsel */
   for (unsigned i = 0; i < src->num_components; i++) {
      nir_ssa_def *low_cond = NULL, *high_cond = NULL;
      nir_ssa_def *comp = nir_channel(&b->nb, src, i);
      switch (src_type) {
      case nir_type_int:
         low_cond = low ? nir_ilt(&b->nb, comp, low) : NULL;
         high_cond = high ? nir_ilt(&b->nb, high, comp) : NULL;
         break;
      case nir_type_uint:
         low_cond = low ? nir_ult(&b->nb, src, low) : NULL;
         high_cond = high ? nir_ult(&b->nb, high, comp) : NULL;
         break;
      case nir_type_float:
         low_cond = low ? nir_flt(&b->nb, comp, low) : NULL;
         high_cond = high ? nir_flt(&b->nb, high, comp) : NULL;
         break;
      default:
         unreachable("clamping from unknown type");
      }

      rets[i] = comp;
      if (low_cond)
         rets[i] = nir_bcsel(&b->nb, low_cond, low, rets[i]);
      if (high_cond)
         rets[i] = nir_bcsel(&b->nb, high_cond, high, rets[i]);
   }

   return nir_vec(&b->nb, rets, src->num_components);
}

static nir_ssa_def *
vtn_handle_convert(struct vtn_builder *b, nir_rounding_mode round,
                   bool want_clamp, nir_alu_type src_type, nir_ssa_def *src,
                   nir_alu_type dst_type, unsigned int dst_bit_size)
{
   nir_ssa_def *dst = src;

   /* Check if we need to do a saturating conversion. */
   bool do_clamp = want_clamp;
   if (src_type == dst_type && dst_bit_size >= src->bit_size) {
      do_clamp = false;
   } else if (src_type == nir_type_uint && dst_type == nir_type_int &&
              dst_bit_size > src->bit_size) {
      do_clamp = false;
   } else if (src_type == nir_type_float && src->bit_size == 16 &&
              dst_type == nir_type_int && dst_bit_size >= 32) {
      do_clamp = false;
   } else if (src_type != nir_type_float && dst_type == nir_type_float &&
              (dst_bit_size >= 32 || src->bit_size == 8)) {
      /* all signed or unsigned ints can fit in float or above;
         u8 can fit in f16 */
      do_clamp = false;
   }

   /* Skip rounding if we can. */
   if (src_type != nir_type_float) {
      round = nir_rounding_mode_undef;
   } else if (dst_type == nir_type_float && dst_bit_size >= src->bit_size) {
      round = nir_rounding_mode_undef;
   }

   /*
    * If we don't care about rounding and clamping, we can just use NIR's
    * built-in ops. There is also a special case for SPIR-V in shaders, where
    * f32/f64 -> f16 conversions can have one of two rounding modes applied,
    * which NIR has built-in opcodes for.
    *
    * For the rest, we have our own implementation of rounding and clamping.
    */
   bool trivial_convert;
   if (!do_clamp && round == nir_rounding_mode_undef) {
      trivial_convert = true;
   } else if (!do_clamp &&
              src_type == nir_type_float &&
              dst_type == nir_type_float &&
              dst_bit_size == 16 &&
              (round == nir_rounding_mode_rtne ||
              round == nir_rounding_mode_rtz)) {
      trivial_convert = true;
   } else {
      trivial_convert = false;
   }
   if (trivial_convert) {
      nir_op op = nir_type_conversion_op(src_type | src->bit_size,
                                         dst_type | dst_bit_size,
                                         round);
      return nir_build_alu(&b->nb, op, src, NULL, NULL, NULL);
   }

   /* Only OpenCL can use complex rounding modes. */
   assert(b->nb.shader->info.stage == MESA_SHADER_KERNEL);

   /* clamp the result into range */
   if (do_clamp) {
      dst = clamp_to_range(b, dst, src_type, dst_type, dst_bit_size);
   }

   /* round with selected rounding mode */
   if (!trivial_convert && round != nir_rounding_mode_undef) {
      dst = round_float(b, round, dst, dst_type, dst_bit_size);
      round = nir_rounding_mode_undef;
   }

   /* now we can convert the value */
   nir_op op = nir_type_conversion_op(src_type | dst->bit_size,
                                      dst_type | dst_bit_size,
                                      round);
   return nir_build_alu(&b->nb, op, dst, NULL, NULL, NULL);
}

void
vtn_handle_alu(struct vtn_builder *b, SpvOp opcode,
               const uint32_t *w, unsigned count)
{
   struct vtn_value *val = vtn_push_value(b, w[2], vtn_value_type_ssa);
   const struct glsl_type *type =
      vtn_value(b, w[1], vtn_value_type_type)->type->type;

   vtn_foreach_decoration(b, val, handle_no_contraction, NULL);

   /* Collect the various SSA sources */
   const unsigned num_inputs = count - 3;
   struct vtn_ssa_value *vtn_src[4] = { NULL, };
   for (unsigned i = 0; i < num_inputs; i++)
      vtn_src[i] = vtn_ssa_value(b, w[i + 3]);

   if (glsl_type_is_matrix(vtn_src[0]->type) ||
       (num_inputs >= 2 && glsl_type_is_matrix(vtn_src[1]->type))) {
      vtn_handle_matrix_alu(b, opcode, val, vtn_src[0], vtn_src[1]);
      b->nb.exact = b->exact;
      return;
   }

   val->ssa = vtn_create_ssa_value(b, type);
   nir_ssa_def *src[4] = { NULL, };
   for (unsigned i = 0; i < num_inputs; i++) {
      vtn_assert(glsl_type_is_vector_or_scalar(vtn_src[i]->type));
      src[i] = vtn_src[i]->def;
   }

   switch (opcode) {
   case SpvOpAny:
      if (src[0]->num_components == 1) {
         val->ssa->def = nir_mov(&b->nb, src[0]);
      } else {
         nir_op op;
         switch (src[0]->num_components) {
         case 2:  op = nir_op_bany_inequal2; break;
         case 3:  op = nir_op_bany_inequal3; break;
         case 4:  op = nir_op_bany_inequal4; break;
         case 8:  op = nir_op_bany_inequal8; break;
         case 16:  op = nir_op_bany_inequal16; break;
         default: vtn_fail("invalid number of components");
         }
         val->ssa->def = nir_build_alu(&b->nb, op, src[0],
                                       nir_imm_false(&b->nb),
                                       NULL, NULL);
      }
      break;

   case SpvOpAll:
      if (src[0]->num_components == 1) {
         val->ssa->def = nir_mov(&b->nb, src[0]);
      } else {
         nir_op op;
         switch (src[0]->num_components) {
         case 2:  op = nir_op_ball_iequal2;  break;
         case 3:  op = nir_op_ball_iequal3;  break;
         case 4:  op = nir_op_ball_iequal4;  break;
         case 8:  op = nir_op_ball_iequal8;  break;
         case 16:  op = nir_op_ball_iequal16;  break;
         default: vtn_fail("invalid number of components");
         }
         val->ssa->def = nir_build_alu(&b->nb, op, src[0],
                                       nir_imm_true(&b->nb),
                                       NULL, NULL);
      }
      break;

   case SpvOpOuterProduct: {
      for (unsigned i = 0; i < src[1]->num_components; i++) {
         val->ssa->elems[i]->def =
            nir_fmul(&b->nb, src[0], nir_channel(&b->nb, src[1], i));
      }
      break;
   }

   case SpvOpDot:
      val->ssa->def = nir_fdot(&b->nb, src[0], src[1]);
      break;

   case SpvOpIAddCarry:
      vtn_assert(glsl_type_is_struct_or_ifc(val->ssa->type));
      val->ssa->elems[0]->def = nir_iadd(&b->nb, src[0], src[1]);
      val->ssa->elems[1]->def = nir_uadd_carry(&b->nb, src[0], src[1]);
      break;

   case SpvOpISubBorrow:
      vtn_assert(glsl_type_is_struct_or_ifc(val->ssa->type));
      val->ssa->elems[0]->def = nir_isub(&b->nb, src[0], src[1]);
      val->ssa->elems[1]->def = nir_usub_borrow(&b->nb, src[0], src[1]);
      break;

   case SpvOpUMulExtended: {
      vtn_assert(glsl_type_is_struct_or_ifc(val->ssa->type));
      nir_ssa_def *umul = nir_umul_2x32_64(&b->nb, src[0], src[1]);
      val->ssa->elems[0]->def = nir_unpack_64_2x32_split_x(&b->nb, umul);
      val->ssa->elems[1]->def = nir_unpack_64_2x32_split_y(&b->nb, umul);
      break;
   }

   case SpvOpSMulExtended: {
      vtn_assert(glsl_type_is_struct_or_ifc(val->ssa->type));
      nir_ssa_def *smul = nir_imul_2x32_64(&b->nb, src[0], src[1]);
      val->ssa->elems[0]->def = nir_unpack_64_2x32_split_x(&b->nb, smul);
      val->ssa->elems[1]->def = nir_unpack_64_2x32_split_y(&b->nb, smul);
      break;
   }

   case SpvOpFwidth:
      val->ssa->def = nir_fadd(&b->nb,
                               nir_fabs(&b->nb, nir_fddx(&b->nb, src[0])),
                               nir_fabs(&b->nb, nir_fddy(&b->nb, src[0])));
      break;
   case SpvOpFwidthFine:
      val->ssa->def = nir_fadd(&b->nb,
                               nir_fabs(&b->nb, nir_fddx_fine(&b->nb, src[0])),
                               nir_fabs(&b->nb, nir_fddy_fine(&b->nb, src[0])));
      break;
   case SpvOpFwidthCoarse:
      val->ssa->def = nir_fadd(&b->nb,
                               nir_fabs(&b->nb, nir_fddx_coarse(&b->nb, src[0])),
                               nir_fabs(&b->nb, nir_fddy_coarse(&b->nb, src[0])));
      break;

   case SpvOpVectorTimesScalar:
      /* The builder will take care of splatting for us. */
      val->ssa->def = nir_fmul(&b->nb, src[0], src[1]);
      break;

   case SpvOpIsNan:
      val->ssa->def = nir_fne(&b->nb, src[0], src[0]);
      break;

   case SpvOpOrdered:
      val->ssa->def = nir_iand(&b->nb, nir_feq(&b->nb, src[0], src[0]),
                                       nir_feq(&b->nb, src[1], src[1]));
      break;

   case SpvOpUnordered:
      val->ssa->def = nir_ior(&b->nb, nir_fne(&b->nb, src[0], src[0]),
                                      nir_fne(&b->nb, src[1], src[1]));
      break;

   case SpvOpIsInf: {
      nir_ssa_def *inf = nir_imm_floatN_t(&b->nb, INFINITY, src[0]->bit_size);
      val->ssa->def = nir_ieq(&b->nb, nir_fabs(&b->nb, src[0]), inf);
      break;
   }

   case SpvOpFUnordEqual:
   case SpvOpFUnordNotEqual:
   case SpvOpFUnordLessThan:
   case SpvOpFUnordGreaterThan:
   case SpvOpFUnordLessThanEqual:
   case SpvOpFUnordGreaterThanEqual: {
      bool swap;
      unsigned src_bit_size = glsl_get_bit_size(vtn_src[0]->type);
      unsigned dst_bit_size = glsl_get_bit_size(type);
      nir_op op = vtn_nir_alu_op_for_spirv_opcode(b, val, opcode, &swap,
                                                  src_bit_size, dst_bit_size);

      if (swap) {
         nir_ssa_def *tmp = src[0];
         src[0] = src[1];
         src[1] = tmp;
      }

      val->ssa->def =
         nir_ior(&b->nb,
                 nir_build_alu(&b->nb, op, src[0], src[1], NULL, NULL),
                 nir_ior(&b->nb,
                         nir_fne(&b->nb, src[0], src[0]),
                         nir_fne(&b->nb, src[1], src[1])));
      break;
   }

   case SpvOpLessOrGreater:
   case SpvOpFOrdNotEqual: {
      /* For all the SpvOpFOrd* comparisons apart from NotEqual, the value
       * from the ALU will probably already be false if the operands are not
       * ordered so we don’t need to handle it specially.
       */
      bool swap;
      unsigned src_bit_size = glsl_get_bit_size(vtn_src[0]->type);
      unsigned dst_bit_size = glsl_get_bit_size(type);
      nir_op op = vtn_nir_alu_op_for_spirv_opcode(b, val, opcode, &swap,
                                                  src_bit_size, dst_bit_size);

      assert(!swap);

      val->ssa->def =
         nir_iand(&b->nb,
                  nir_build_alu(&b->nb, op, src[0], src[1], NULL, NULL),
                  nir_iand(&b->nb,
                          nir_feq(&b->nb, src[0], src[0]),
                          nir_feq(&b->nb, src[1], src[1])));
      break;
   }

   case SpvOpUConvert:
   case SpvOpConvertFToU:
   case SpvOpConvertFToS:
   case SpvOpFConvert:
   case SpvOpConvertSToF:
   case SpvOpConvertUToF:
   case SpvOpSatConvertSToU:
   case SpvOpSatConvertUToS:
   case SpvOpSConvert: {
      nir_alu_type src_type;
      nir_alu_type dst_type;
      unsigned dst_bit_size = glsl_get_bit_size(type);
      nir_rounding_mode round = nir_rounding_mode_undef;
      vtn_foreach_decoration(b, val, handle_rounding_mode, &round);

      bool saturate = false;
      vtn_foreach_decoration(b, val, handle_saturation, &saturate);
      vtn_fail_if(saturate && b->shader->info.stage != MESA_SHADER_KERNEL,
                  "Saturation can only be applied to conversions in OpenCL");

      switch (opcode) {
      case SpvOpConvertFToS:
         src_type = nir_type_float;
         dst_type = nir_type_int;
         break;
      case SpvOpConvertFToU:
         src_type = nir_type_float;
         dst_type = nir_type_uint;
         break;
      case SpvOpFConvert:
         src_type = dst_type = nir_type_float;
         break;
      case SpvOpConvertSToF:
         src_type = nir_type_int;
         dst_type = nir_type_float;
         break;
      case SpvOpSConvert:
         src_type = dst_type = nir_type_int;
         break;
      case SpvOpSatConvertSToU:
         src_type = nir_type_int;
         dst_type = nir_type_uint;
         saturate = true;
         break;
      case SpvOpSatConvertUToS:
         src_type = nir_type_uint;
         dst_type = nir_type_int;
         saturate = true;
         break;
      case SpvOpConvertUToF:
         src_type = nir_type_uint;
         dst_type = nir_type_float;
         break;
      case SpvOpUConvert:
         src_type = dst_type = nir_type_uint;
         break;
      default:
         unreachable("Invalid opcode");
      }

      /* OpenCL allows all rounding modes; graphics shaders may only
       * use rtne/rtz, and only to f16 */
      vtn_fail_if(b->shader->info.stage != MESA_SHADER_KERNEL &&
                  (round != nir_rounding_mode_undef &&
                  ((round != nir_rounding_mode_rtne &&
                    round != nir_rounding_mode_rtz) ||
                   dst_type != nir_type_float || dst_bit_size != 16)),
                  "Rounding mode can only be applied to conversions in OpenCL");

      val->ssa->def =
         vtn_handle_convert(b, round, saturate, src_type, src[0],
                            dst_type, dst_bit_size);
      break;
   }

   case SpvOpBitFieldInsert:
   case SpvOpBitFieldSExtract:
   case SpvOpBitFieldUExtract:
   case SpvOpShiftLeftLogical:
   case SpvOpShiftRightArithmetic:
   case SpvOpShiftRightLogical: {
      bool swap;
      unsigned src0_bit_size = glsl_get_bit_size(vtn_src[0]->type);
      unsigned dst_bit_size = glsl_get_bit_size(type);
      nir_op op = vtn_nir_alu_op_for_spirv_opcode(b, val, opcode, &swap,
                                                  src0_bit_size, dst_bit_size);

      assert (op == nir_op_ushr || op == nir_op_ishr || op == nir_op_ishl ||
              op == nir_op_bitfield_insert || op == nir_op_ubitfield_extract ||
              op == nir_op_ibitfield_extract);

      for (unsigned i = 0; i < nir_op_infos[op].num_inputs; i++) {
         unsigned src_bit_size =
            nir_alu_type_get_type_size(nir_op_infos[op].input_types[i]);
         if (src_bit_size == 0)
            continue;
         if (src_bit_size != src[i]->bit_size) {
            assert(src_bit_size == 32);
            /* Convert the Shift, Offset and Count  operands to 32 bits, which is the bitsize
             * supported by the NIR instructions. See discussion here:
             *
             * https://lists.freedesktop.org/archives/mesa-dev/2018-April/193026.html
             */
            src[i] = nir_u2u32(&b->nb, src[i]);
         }
      }
      val->ssa->def = nir_build_alu(&b->nb, op, src[0], src[1], src[2], src[3]);
      break;
   }

   case SpvOpSignBitSet: {
      unsigned src_bit_size = glsl_get_bit_size(vtn_src[0]->type);
      if (src[0]->num_components == 1)
         val->ssa->def =
            nir_ushr(&b->nb, src[0], nir_imm_int(&b->nb, src_bit_size - 1));
      else
         val->ssa->def =
            nir_ishr(&b->nb, src[0], nir_imm_int(&b->nb, src_bit_size - 1));

      val->ssa->def = nir_i2b(&b->nb, val->ssa->def);
      break;
   }

   case SpvOpUCountTrailingZerosINTEL:
      val->ssa->def = nir_umin(&b->nb,
                               nir_find_lsb(&b->nb, src[0]),
                               nir_imm_int(&b->nb, 32u));
      break;

   case SpvOpBitCount: {
      bool swap;
      unsigned src_bit_size = glsl_get_bit_size(vtn_src[0]->type);
      unsigned dst_bit_size = glsl_get_bit_size(type);
      nir_op op = vtn_nir_alu_op_for_spirv_opcode(b, val, opcode, &swap,
                                                  src_bit_size, dst_bit_size);
      val->ssa->def = nir_build_alu(&b->nb, op, src[0], src[1], src[2], src[3]);

      /* bit_count always returns int32, but the SPIR-V opcode just says the return
       * value needs to be big enough to store the number of bits.
       */
      if (dst_bit_size != 32) {
         val->ssa->def = nir_u2u(&b->nb, val->ssa->def, dst_bit_size);
      }
      break;
   }

   default: {
      bool swap;
      unsigned src_bit_size = glsl_get_bit_size(vtn_src[0]->type);
      unsigned dst_bit_size = glsl_get_bit_size(type);
      nir_op op = vtn_nir_alu_op_for_spirv_opcode(b, val, opcode, &swap,
                                                  src_bit_size, dst_bit_size);

      if (swap) {
         nir_ssa_def *tmp = src[0];
         src[0] = src[1];
         src[1] = tmp;
      }

      switch (op) {
      case nir_op_ishl:
      case nir_op_ishr:
      case nir_op_ushr:
         if (src[1]->bit_size != 32)
            src[1] = nir_u2u32(&b->nb, src[1]);
         break;
      default:
         break;
      }

      val->ssa->def = nir_build_alu(&b->nb, op, src[0], src[1], src[2], src[3]);
      break;
   } /* default */
   }

   switch (opcode) {
   case SpvOpIAdd:
   case SpvOpIMul:
   case SpvOpISub:
   case SpvOpShiftLeftLogical:
   case SpvOpSNegate: {
      nir_alu_instr *alu = nir_instr_as_alu(val->ssa->def->parent_instr);
      vtn_foreach_decoration(b, val, handle_no_wrap, alu);
      break;
   }
   default:
      /* Do nothing. */
      break;
   }

   b->nb.exact = b->exact;
}

void
vtn_handle_bitcast(struct vtn_builder *b, const uint32_t *w, unsigned count)
{
   vtn_assert(count == 4);
   /* From the definition of OpBitcast in the SPIR-V 1.2 spec:
    *
    *    "If Result Type has the same number of components as Operand, they
    *    must also have the same component width, and results are computed per
    *    component.
    *
    *    If Result Type has a different number of components than Operand, the
    *    total number of bits in Result Type must equal the total number of
    *    bits in Operand. Let L be the type, either Result Type or Operand’s
    *    type, that has the larger number of components. Let S be the other
    *    type, with the smaller number of components. The number of components
    *    in L must be an integer multiple of the number of components in S.
    *    The first component (that is, the only or lowest-numbered component)
    *    of S maps to the first components of L, and so on, up to the last
    *    component of S mapping to the last components of L. Within this
    *    mapping, any single component of S (mapping to multiple components of
    *    L) maps its lower-ordered bits to the lower-numbered components of L."
    */

   struct vtn_type *type = vtn_value(b, w[1], vtn_value_type_type)->type;
   struct vtn_ssa_value *vtn_src = vtn_ssa_value(b, w[3]);
   struct nir_ssa_def *src = vtn_src->def;
   struct vtn_ssa_value *val = vtn_create_ssa_value(b, type->type);

   vtn_assert(glsl_type_is_vector_or_scalar(vtn_src->type));

   vtn_fail_if(src->num_components * src->bit_size !=
               glsl_get_vector_elements(type->type) * glsl_get_bit_size(type->type),
               "Source and destination of OpBitcast must have the same "
               "total number of bits");
   val->def = nir_bitcast_vector(&b->nb, src, glsl_get_bit_size(type->type));
   vtn_push_ssa(b, w[2], type, val);
}
