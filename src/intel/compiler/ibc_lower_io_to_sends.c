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

#include "brw_eu.h"

static bool
lower_bti_block_load_ubo(ibc_builder *b, ibc_intrinsic_instr *read)
{
   /* Only lower BLOCK_LOAD_UBO if it doesn't have a push GRF */
   if (read->src[0].ref.file == IBC_FILE_HW_GRF)
      return false;

   const unsigned block_size_B =
      read->num_dest_comps * ibc_type_byte_size(read->dest.type);
   assert(block_size_B % REG_SIZE == 0);
   const unsigned block_size_DW = block_size_B / 4;

   /* It's actually going to be a SIMD8 or SIMD16 send */
   assert(read->instr.we_all);
   assert(read->instr.predicate == IBC_PREDICATE_NONE);
   ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, block_size_DW);
   send->instr.we_all = true;

   send->sfid = GEN6_SFID_DATAPORT_CONSTANT_CACHE;
   send->desc_imm =
      brw_dp_read_desc(b->shader->devinfo, 0,
                       BRW_DATAPORT_OWORD_BLOCK_DWORDS(block_size_DW),
                       GEN7_DATAPORT_DC_OWORD_BLOCK_READ,
                       BRW_DATAPORT_READ_TARGET_DATA_CACHE);
   assert(read->src[1].ref.type == IBC_TYPE_UD);
   send->desc = read->src[1].ref;

   assert(read->src[2].ref.file == IBC_FILE_IMM);
   assert(read->src[2].ref.type == IBC_TYPE_UD);
   const uint32_t offset_B = ibc_ref_as_uint(read->src[2].ref);

   ibc_reg *msg = ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);

   ibc_builder_push_we_all(b, 8);
   ibc_MOV_to(b, ibc_typed_ref(msg, IBC_TYPE_UD),
                 ibc_typed_ref(b->shader->g0, IBC_TYPE_UD));
   ibc_builder_pop(b);

   ibc_builder_push_scalar(b);
   ibc_ref offset_ref = ibc_typed_ref(msg, IBC_TYPE_UD);
   offset_ref.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
   ibc_MOV_to(b, offset_ref, ibc_imm_ud(offset_B / 16));
   ibc_builder_pop(b);

   send->payload[0] = ibc_typed_ref(msg, IBC_TYPE_32_BIT);
   send->has_header = true;
   send->mlen = 1;

   send->dest = read->dest;
   send->rlen = block_size_B / REG_SIZE;

   ibc_builder_insert_instr(b, &send->instr);
   ibc_instr_remove(&read->instr);

   return true;
}

static bool
lower_surface_access(ibc_builder *b, ibc_intrinsic_instr *intrin)
{
   const struct gen_device_info *devinfo = b->shader->devinfo;

   const ibc_ref surface_bti = intrin->src[IBC_SURFACE_SRC_SURFACE_BTI].ref;
   const ibc_ref surface_handle =
      intrin->src[IBC_SURFACE_SRC_SURFACE_HANDLE].ref;

   const ibc_ref address = intrin->src[IBC_SURFACE_SRC_ADDRESS].ref;
   const ibc_ref pixel_mask = intrin->src[IBC_SURFACE_SRC_PIXEL_MASK].ref;
   const ibc_ref data0 = intrin->src[IBC_SURFACE_SRC_DATA0].ref;
   const ibc_ref data1 = intrin->src[IBC_SURFACE_SRC_DATA1].ref;
   const ibc_ref atomic_op = intrin->src[IBC_SURFACE_SRC_ATOMIC_OP].ref;

   const unsigned num_address_comps =
      intrin->src[IBC_SURFACE_SRC_ADDRESS].num_comps;
   const unsigned num_data_comps =
      intrin->src[IBC_SURFACE_SRC_DATA0].num_comps;
   assert(intrin->src[IBC_SURFACE_SRC_DATA1].num_comps == 0 ||
          intrin->src[IBC_SURFACE_SRC_DATA1].num_comps == num_data_comps);

   switch (intrin->op) {
   case IBC_INTRINSIC_OP_BTI_TYPED_READ:
   case IBC_INTRINSIC_OP_BTI_TYPED_WRITE:
   case IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC:
   case IBC_INTRINSIC_OP_BTI_UNTYPED_READ:
   case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:
   case IBC_INTRINSIC_OP_BTI_BYTE_SCATTERED_READ:
   case IBC_INTRINSIC_OP_BTI_BYTE_SCATTERED_WRITE:
   case IBC_INTRINSIC_OP_BTI_UNTYPED_ATOMIC:
      assert(ibc_type_bit_size(address.type) == 32);
      assert((surface_bti.file == IBC_FILE_NONE) !=
             (surface_handle.file == IBC_FILE_NONE));
      break;

   case IBC_INTRINSIC_OP_A64_UNTYPED_READ:
   case IBC_INTRINSIC_OP_A64_UNTYPED_WRITE:
   case IBC_INTRINSIC_OP_A64_BYTE_SCATTERED_READ:
   case IBC_INTRINSIC_OP_A64_BYTE_SCATTERED_WRITE:
   case IBC_INTRINSIC_OP_A64_UNTYPED_ATOMIC_INT64:
   case IBC_INTRINSIC_OP_A64_UNTYPED_ATOMIC:
      assert(ibc_type_bit_size(address.type) == 64);
      assert(surface_bti.file == IBC_FILE_NONE &&
             surface_handle.file == IBC_FILE_NONE);
      break;

   default:
      unreachable("Unhandled surface access intrinsic");
   }

   ibc_builder_push_instr_group(b, &intrin->instr);
   if (b->simd_width < 8) {
      /* For a SIMD1 surface instruction, set the builder to SIMD8 so that we
       * build the payload in SIMD8.  We create the actual SEND instruction
       * SIMD1 in the hopes that it will make the HW fetch less data.
       */
      assert(b->simd_width == 1);
      b->simd_width = 8;
   }

   ibc_send_instr *send = ibc_send_instr_create(b->shader,
                                                intrin->instr.simd_group,
                                                intrin->instr.simd_width);
   send->instr.we_all = intrin->instr.we_all;
   send->can_reorder = intrin->can_reorder;
   send->has_side_effects = intrin->has_side_effects;

   if (pixel_mask.file != IBC_FILE_NONE) {
      /* Add an UNDEF so that liveness analysis doesn't extend our live range
       * too far up and add extra interference.
       */
      ibc_build_intrinsic(b, IBC_INTRINSIC_OP_UNDEF, intrin->dest,
                          -1, intrin->num_dest_comps, NULL, 0);

      assert(pixel_mask.type == IBC_TYPE_FLAG);
      send->instr.flag = pixel_mask;
      send->instr.predicate = IBC_PREDICATE_NORMAL;
   }

   ibc_intrinsic_src src[8] = {};
   unsigned num_srcs = 0;

   /* Gen8 and earlier require headers for typed surface opcodes */
   assert(devinfo->gen >= 9);

   for (unsigned i = 0; i < num_address_comps; i++)
      src[num_srcs++].ref = ibc_comp_ref(address, i);

   if (data0.file != IBC_FILE_NONE) {
      for (unsigned i = 0; i < num_data_comps; i++)
         src[num_srcs++].ref = ibc_comp_ref(data0, i);
   }

   if (data1.file != IBC_FILE_NONE) {
      assert(num_data_comps == 1);
      src[num_srcs++].ref = data1;
   }

   assert(num_srcs < ARRAY_SIZE(src));

   unsigned mlen;
   send->payload[0] = ibc_MESSAGE(b, src, num_srcs, &mlen);
   send->mlen = mlen;

   uint32_t desc;
   switch (intrin->op) {
   case IBC_INTRINSIC_OP_BTI_TYPED_READ:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_typed_surface_rw_desc(b->shader->devinfo,
                                          intrin->instr.simd_width,
                                          intrin->instr.simd_group,
                                          intrin->num_dest_comps,
                                          false   /* write */);
      break;
   case IBC_INTRINSIC_OP_BTI_TYPED_WRITE:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_typed_surface_rw_desc(b->shader->devinfo,
                                          intrin->instr.simd_width,
                                          intrin->instr.simd_group,
                                          num_data_comps,
                                          true   /* write */);
      break;
   case IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_typed_atomic_desc(b->shader->devinfo,
                                      intrin->instr.simd_width,
                                      intrin->instr.simd_group,
                                      ibc_ref_as_uint(atomic_op),
                                      intrin->num_dest_comps > 0);
      break;
   case IBC_INTRINSIC_OP_BTI_UNTYPED_READ:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_untyped_surface_rw_desc(b->shader->devinfo,
                                            intrin->instr.simd_width,
                                            intrin->num_dest_comps,
                                            false   /* write */);
      break;
   case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_untyped_surface_rw_desc(b->shader->devinfo,
                                            intrin->instr.simd_width,
                                            num_data_comps,
                                            true    /* write */);
      break;
   case IBC_INTRINSIC_OP_BTI_BYTE_SCATTERED_READ:
      send->sfid = GEN7_SFID_DATAPORT_DATA_CACHE;
      desc = brw_dp_byte_scattered_rw_desc(devinfo,
                                           intrin->instr.simd_width,
                                           ibc_type_bit_size(intrin->dest.type),
                                           false   /* write */);
      break;
   case IBC_INTRINSIC_OP_BTI_BYTE_SCATTERED_WRITE:
      send->sfid = GEN7_SFID_DATAPORT_DATA_CACHE;
      desc = brw_dp_byte_scattered_rw_desc(devinfo,
                                           intrin->instr.simd_width,
                                           ibc_type_bit_size(data0.type),
                                           true    /* write */);
      break;
   case IBC_INTRINSIC_OP_BTI_UNTYPED_ATOMIC:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_untyped_atomic_desc(b->shader->devinfo,
                                        intrin->instr.simd_width,
                                        ibc_ref_as_uint(atomic_op),
                                        intrin->num_dest_comps > 0);
      break;
   case IBC_INTRINSIC_OP_A64_UNTYPED_READ:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_a64_untyped_surface_rw_desc(devinfo,
                                                intrin->instr.simd_width,
                                                intrin->num_dest_comps,
                                                false    /*write */);
      break;
   case IBC_INTRINSIC_OP_A64_UNTYPED_WRITE:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_a64_untyped_surface_rw_desc(devinfo,
                                                intrin->instr.simd_width,
                                                num_data_comps,
                                                true  /*write */);
      break;
   case IBC_INTRINSIC_OP_A64_BYTE_SCATTERED_READ:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_a64_byte_scattered_rw_desc(devinfo,
                                               intrin->instr.simd_width,
                                               ibc_type_bit_size(intrin->dest.type),
                                               false  /*write */);
      break;
   case IBC_INTRINSIC_OP_A64_BYTE_SCATTERED_WRITE:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_a64_byte_scattered_rw_desc(devinfo,
                                               intrin->instr.simd_width,
                                               ibc_type_bit_size(data0.type),
                                               true   /*write */);
      break;
   case IBC_INTRINSIC_OP_A64_UNTYPED_ATOMIC:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_a64_untyped_atomic_desc(devinfo,
                                            intrin->instr.simd_width, 32,
                                            ibc_ref_as_uint(atomic_op),
                                            intrin->num_dest_comps > 0);
      break;
   case IBC_INTRINSIC_OP_A64_UNTYPED_ATOMIC_INT64:
      send->sfid = HSW_SFID_DATAPORT_DATA_CACHE_1;
      desc = brw_dp_a64_untyped_atomic_desc(devinfo,
                                            intrin->instr.simd_width, 64,
                                            ibc_ref_as_uint(atomic_op),
                                            intrin->num_dest_comps > 0);
      break;
   default:
      unreachable("Unhandled surface access intrinsic");
   }

   send->desc_imm = desc;
   if (surface_bti.file == IBC_FILE_IMM) {
      send->desc_imm |= ibc_ref_as_uint(surface_bti) & 0xff;
   } else if (surface_bti.file != IBC_FILE_NONE) {
      ibc_builder_push_scalar(b);
      assert(surface_bti.type == IBC_TYPE_UD);
      send->desc = ibc_AND(b, IBC_TYPE_UD, surface_bti, ibc_imm_uw(0xff));
      ibc_builder_pop(b);
   } else if (surface_handle.file != IBC_FILE_NONE) {
      /* Bindless surface */
      assert(devinfo->gen >= 9);
      send->desc_imm |= GEN9_BTI_BINDLESS;

      /* We assume that the driver provided the handle in the top 20 bits so
       * we can use the surface handle directly as the extended descriptor.
       */
      ibc_builder_push_scalar(b);
      assert(surface_handle.type == IBC_TYPE_UD);
      send->ex_desc = ibc_MOV(b, IBC_TYPE_UD, surface_handle);
      ibc_builder_pop(b);
   } else {
      assert(ibc_type_bit_size(address.type) == 64);
   }

   assert(ibc_type_bit_size(intrin->dest.type) == 32 ||
          ibc_type_bit_size(intrin->dest.type) == 64);
   if (intrin->instr.simd_width == 1) {
      send->rlen = intrin->num_dest_comps;
      ibc_reg *tmp_reg =
         ibc_hw_grf_reg_create(b->shader, send->rlen * REG_SIZE, REG_SIZE);
      send->dest = ibc_typed_ref(tmp_reg, intrin->dest.type);
   } else {
      assert(intrin->instr.simd_width >= 8);
      send->dest = intrin->dest;
      send->rlen = (intrin->num_dest_comps *
                    intrin->instr.simd_width *
                    ibc_type_byte_size(intrin->dest.type)) / REG_SIZE;
   }

   ibc_builder_insert_instr(b, &send->instr);

   ibc_builder_pop(b);

   if (intrin->instr.simd_width == 1) {
      ibc_builder_push_scalar(b);
      assert(ibc_type_bit_size(intrin->dest.type) == 32);
      assert(send->dest.type == intrin->dest.type);

      ibc_ref vec_src[4];
      assert(intrin->num_dest_comps <= ARRAY_SIZE(vec_src));
      for (unsigned i = 0; i < intrin->num_dest_comps; i++) {
         vec_src[i] = send->dest;
         vec_src[i].hw_grf.byte += i * REG_SIZE;
         ibc_hw_grf_mul_stride(&vec_src[i].hw_grf, 0);
      }
      ibc_VEC_to(b, intrin->dest, vec_src, intrin->num_dest_comps);

      ibc_builder_pop(b);
   }

   ibc_instr_remove(&intrin->instr);

   return true;
}

static bool
is_high_sampler(const ibc_ref sampler_bti)
{
   if (sampler_bti.file != IBC_FILE_IMM)
      return true;

   assert(sampler_bti.type == IBC_TYPE_UD);
   return ibc_ref_as_uint(sampler_bti) >= 16;
}

static unsigned
sampler_msg_type(const struct gen_device_info *devinfo,
                 enum ibc_intrinsic_op op,
                 bool shadow_compare, bool lod_zero)
{
   assert(devinfo->gen >= 5);
   switch (op) {
   case IBC_INTRINSIC_OP_TEX:
      return shadow_compare ? GEN5_SAMPLER_MESSAGE_SAMPLE_COMPARE :
                              GEN5_SAMPLER_MESSAGE_SAMPLE;
   case IBC_INTRINSIC_OP_TXB:
      return shadow_compare ? GEN5_SAMPLER_MESSAGE_SAMPLE_BIAS_COMPARE :
                              GEN5_SAMPLER_MESSAGE_SAMPLE_BIAS;
   case IBC_INTRINSIC_OP_TXL:
      if (lod_zero) {
         assert(devinfo->gen >= 9);
         return shadow_compare ? GEN9_SAMPLER_MESSAGE_SAMPLE_C_LZ :
                                 GEN9_SAMPLER_MESSAGE_SAMPLE_LZ;
      } else {
         return shadow_compare ? GEN5_SAMPLER_MESSAGE_SAMPLE_LOD_COMPARE :
                                 GEN5_SAMPLER_MESSAGE_SAMPLE_LOD;
      }
   case IBC_INTRINSIC_OP_TXS:
      return GEN5_SAMPLER_MESSAGE_SAMPLE_RESINFO;
   case IBC_INTRINSIC_OP_TXD:
      return shadow_compare ? HSW_SAMPLER_MESSAGE_SAMPLE_DERIV_COMPARE :
                              GEN5_SAMPLER_MESSAGE_SAMPLE_DERIVS;
   case IBC_INTRINSIC_OP_TXF:
      if (lod_zero) {
         assert(devinfo->gen >= 9);
         return GEN9_SAMPLER_MESSAGE_SAMPLE_LD_LZ;
      } else {
         return GEN5_SAMPLER_MESSAGE_SAMPLE_LD;
      }
   case IBC_INTRINSIC_OP_TXF_MS:
      return devinfo->gen >= 9 ? GEN9_SAMPLER_MESSAGE_SAMPLE_LD2DMS_W :
                                 GEN7_SAMPLER_MESSAGE_SAMPLE_LD2DMS;
   case IBC_INTRINSIC_OP_TXF_MCS:
      return GEN7_SAMPLER_MESSAGE_SAMPLE_LD_MCS;
   case IBC_INTRINSIC_OP_LOD:
      return GEN5_SAMPLER_MESSAGE_LOD;
   case IBC_INTRINSIC_OP_TG4:
      return shadow_compare ? GEN7_SAMPLER_MESSAGE_SAMPLE_GATHER4_C :
                              GEN7_SAMPLER_MESSAGE_SAMPLE_GATHER4;
      break;
   case IBC_INTRINSIC_OP_TG4_OFFSET:
      return shadow_compare ? GEN7_SAMPLER_MESSAGE_SAMPLE_GATHER4_PO_C :
                              GEN7_SAMPLER_MESSAGE_SAMPLE_GATHER4_PO;
   case IBC_INTRINSIC_OP_SAMPLEINFO:
      return GEN6_SAMPLER_MESSAGE_SAMPLE_SAMPLEINFO;
   default:
      unreachable("not reached");
   }
}

#define MAX_SAMPLER_MESSAGE_SIZE 11

unsigned
ibc_tex_instr_max_simd_width(const ibc_intrinsic_instr *intrin,
                             const struct gen_device_info *devinfo)
{
   /* TXD is only supported on SIMD8 */
   if (intrin->op == IBC_INTRINSIC_OP_TXD)
      return 8;

   /* If we have a min_lod parameter on anything other than a simple sample
    * message, it will push it over 5 arguments and we have to fall back to
    * SIMD8.
    */
   if (intrin->op != IBC_INTRINSIC_OP_TEX &&
       intrin->src[IBC_TEX_SRC_MIN_LOD].num_comps > 0)
      return 8;

   const ibc_ref lod = intrin->src[IBC_TEX_SRC_LOD].ref;
   const bool implicit_lod = devinfo->gen >= 9 &&
                             (intrin->op == IBC_INTRINSIC_OP_TXL ||
                              intrin->op == IBC_INTRINSIC_OP_TXF) &&
                             ibc_ref_is_null_or_zero(lod);

   const unsigned num_payload_components =
      intrin->src[IBC_TEX_SRC_COORD].num_comps +
      intrin->src[IBC_TEX_SRC_SHADOW_C].num_comps +
      (implicit_lod ? 0 : intrin->src[IBC_TEX_SRC_LOD].num_comps) +
      intrin->src[IBC_TEX_SRC_MIN_LOD].num_comps +
      intrin->src[IBC_TEX_SRC_DDX].num_comps +
      intrin->src[IBC_TEX_SRC_DDY].num_comps +
      intrin->src[IBC_TEX_SRC_SAMPLE_INDEX].num_comps +
      intrin->src[IBC_TEX_SRC_MCS].num_comps +
      (intrin->op == IBC_INTRINSIC_OP_TG4_OFFSET ?
         intrin->src[IBC_TEX_SRC_TG4_OFFSET].num_comps : 0);

   if (num_payload_components > MAX_SAMPLER_MESSAGE_SIZE / 2)
      return 8;
   else
      return 16;
}

static bool
lower_tex(ibc_builder *b, ibc_intrinsic_instr *intrin)
{
   const struct gen_device_info *devinfo = b->shader->devinfo;

   const ibc_ref surface_bti = intrin->src[IBC_TEX_SRC_SURFACE_BTI].ref;
   const ibc_ref surface_handle = intrin->src[IBC_TEX_SRC_SURFACE_HANDLE].ref;
   uint32_t surface_bti_imm = 0;
   if (surface_bti.file == IBC_FILE_IMM)
      surface_bti_imm = ibc_ref_as_uint(surface_bti);
   assert((surface_bti.file == IBC_FILE_NONE) !=
          (surface_handle.file == IBC_FILE_NONE));

   const ibc_ref sampler_bti = intrin->src[IBC_TEX_SRC_SAMPLER_BTI].ref;
   const ibc_ref sampler_handle = intrin->src[IBC_TEX_SRC_SAMPLER_HANDLE].ref;
   uint32_t sampler_bti_imm = 0;
   if (sampler_bti.file == IBC_FILE_IMM)
      sampler_bti_imm = ibc_ref_as_uint(sampler_bti);
   assert((sampler_bti.file == IBC_FILE_NONE) !=
          (sampler_handle.file == IBC_FILE_NONE));

   const ibc_ref coord = intrin->src[IBC_TEX_SRC_COORD].ref;
   const unsigned num_coord_comps = intrin->src[IBC_TEX_SRC_COORD].num_comps;
   const ibc_ref shadow_c = intrin->src[IBC_TEX_SRC_SHADOW_C].ref;
   const ibc_ref lod = intrin->src[IBC_TEX_SRC_LOD].ref;
   const ibc_ref min_lod = intrin->src[IBC_TEX_SRC_MIN_LOD].ref;
   const ibc_ref ddx = intrin->src[IBC_TEX_SRC_DDX].ref;
   const ibc_ref ddy = intrin->src[IBC_TEX_SRC_DDY].ref;
   assert(intrin->src[IBC_TEX_SRC_DDY].num_comps ==
          intrin->src[IBC_TEX_SRC_DDY].num_comps);
   const unsigned num_grad_comps = intrin->src[IBC_TEX_SRC_DDY].num_comps;
   const ibc_ref sample_index = intrin->src[IBC_TEX_SRC_SAMPLE_INDEX].ref;
   const ibc_ref mcs = intrin->src[IBC_TEX_SRC_MCS].ref;
   const ibc_ref tg4_offset = intrin->src[IBC_TEX_SRC_TG4_OFFSET].ref;

   uint32_t header_bits = 0;
   const ibc_ref header_bits_r = intrin->src[IBC_TEX_SRC_HEADER_BITS].ref;
   if (header_bits_r.file != IBC_FILE_NONE)
      header_bits = ibc_ref_as_uint(header_bits_r);

   ibc_builder_push_instr_group(b, &intrin->instr);
   if (b->simd_width < 8) {
      /* For a SIMD1 texture instruction, set the builder to SIMD8 so that we
       * build the payload in SIMD8.  We create the actual SEND instruction
       * SIMD1 in the hopes that it will make the HW fetch less data.
       */
      assert(b->simd_width == 1);
      b->simd_width = 8;
   }

   assert(intrin->can_reorder && !intrin->has_side_effects);
   assert(intrin->instr.predicate == IBC_PREDICATE_NONE);
   ibc_send_instr *send = ibc_send_instr_create(b->shader,
                                                intrin->instr.simd_group,
                                                intrin->instr.simd_width);
   send->instr.we_all = intrin->instr.we_all;

   ibc_intrinsic_src src[MAX_SAMPLER_MESSAGE_SIZE] = {};
   unsigned num_srcs = 0;

   if (intrin->op == IBC_INTRINSIC_OP_TG4 ||
       intrin->op == IBC_INTRINSIC_OP_TG4_OFFSET ||
       intrin->op == IBC_INTRINSIC_OP_SAMPLEINFO ||
       header_bits != 0 || sampler_handle.file != IBC_FILE_NONE ||
       is_high_sampler(sampler_bti)) {

      ibc_reg *header_reg =
         ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);
      ibc_ref header = ibc_typed_ref(header_reg, IBC_TYPE_UD);

      /* If we're requesting fewer than four channels worth of response,
       * and we have an explicit header, we need to set up the sampler
       * writemask.  It's reversed from normal: 1 means "don't write".
       */
      if (intrin->num_dest_comps < 4) {
         unsigned mask = ~((1 << intrin->num_dest_comps) - 1) & 0xf;
         header_bits |= mask << 12;
      }

      ibc_builder_push_we_all(b, 8);
      ibc_MOV_to(b, header, ibc_typed_ref(b->shader->g0, IBC_TYPE_UD));
      ibc_builder_pop(b);

      /* Everything else just sets up components */
      ibc_builder_push_scalar(b);

      /* TODO: On vertex and fragment stages this is zero by default so we can
       * avoid the MOV.
       */
      ibc_ref header_2 = header;
      header_2.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_MOV_to(b, header_2, ibc_imm_ud(header_bits));

      ibc_ref header_3 = header;
      header_3.hw_grf.byte += 3 * ibc_type_byte_size(IBC_TYPE_UD);
      if (sampler_handle.file != IBC_FILE_NONE) {
         /* Bindless sampler handles aren't relative to the sampler state
          * pointer passed into the shader through SAMPLER_STATE_POINTERS_*.
          * Instead, it's an absolute pointer relative to dynamic state base
          * address.
          *
          * Sampler states are 16 bytes each and the pointer we give here has
          * to be 32-byte aligned.  In order to avoid more indirect messages
          * than required, we assume that all bindless sampler states are
          * 32-byte aligned.  This sacrifices a bit of general state base
          * address space but means we can do something more efficient in the
          * shader.
          */
         ibc_MOV_to(b, header_3, sampler_handle);
      } else if (is_high_sampler(sampler_bti)) {
         ibc_ref sampler_base_ptr;
         ibc_ref g0_3 = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
         g0_3.hw_grf.byte += 3 * sizeof(uint32_t);
         if (sampler_bti.file == IBC_FILE_IMM) {
            assert(sampler_bti_imm >= 16);
            const unsigned sampler_state_size = 16; /* 16 bytes */
            const uint32_t sampler_offset_B =
               16 * (sampler_bti_imm / 16) * sampler_state_size;

            sampler_base_ptr =
               ibc_ADD(b, IBC_TYPE_UD, g0_3, ibc_imm_ud(sampler_offset_B));
         } else {
            sampler_base_ptr =
               ibc_ADD(b, IBC_TYPE_UD, g0_3,
                          ibc_SHL(b, IBC_TYPE_UD,
                                     ibc_AND(b, IBC_TYPE_UD, sampler_bti,
                                                ibc_imm_ud(0xf0)),
                                    ibc_imm_ud(4)));
         }
         ibc_MOV_to(b, header_3, sampler_base_ptr);
      }
      ibc_builder_pop(b);

      src[num_srcs++] = (ibc_intrinsic_src) {
         .ref = ibc_typed_ref(header_reg, IBC_TYPE_UD),
         .simd_width = 1,
         .num_comps = 8,
      };
      send->has_header = true;
   }

   if (shadow_c.file != IBC_FILE_NONE)
      src[num_srcs++].ref = shadow_c;

   bool coord_done = false;
   bool zero_lod = false;

   switch (intrin->op) {
   case IBC_INTRINSIC_OP_TXB:
      src[num_srcs++].ref = lod;
      break;

   case IBC_INTRINSIC_OP_TXL:
      if (devinfo->gen >= 9 && ibc_ref_is_null_or_zero(lod)) {
         zero_lod = true;
      } else {
         src[num_srcs++].ref = lod;
      }
      break;

   case IBC_INTRINSIC_OP_TXD:
      assert(b->simd_width == 8);

      /* Load dPdx and the coordinate together:
       * [hdr], [ref], x, dPdx.x, dPdy.x, y, dPdx.y, dPdy.y, z, dPdx.z, dPdy.z
       */
      for (unsigned i = 0; i < num_coord_comps; i++) {
         src[num_srcs++].ref = ibc_comp_ref(coord, i);

         /* For cube map array, the coordinate is (u,v,r,ai) but there are
          * only derivatives for (u, v, r).
          */
         if (i < num_grad_comps) {
            src[num_srcs++].ref = ibc_comp_ref(ddx, i);
            src[num_srcs++].ref = ibc_comp_ref(ddy, i);
         }
      }
      coord_done = true;
      break;

   case IBC_INTRINSIC_OP_TXS:
      src[num_srcs++].ref = lod;
      break;

   case IBC_INTRINSIC_OP_TXF:
      /* Unfortunately, the parameters for LD are intermixed: u, lod, v, r.
       * On Gen9 they are u, v, lod, r
       */
      src[num_srcs++].ref = ibc_comp_ref(coord, 0);

      if (devinfo->gen >= 9) {
         src[num_srcs++].ref = num_coord_comps >= 2 ? ibc_comp_ref(coord, 1) :
                                                  ibc_imm_ud(0);
      }

      if (devinfo->gen >= 9 && ibc_ref_is_null_or_zero(lod)) {
         zero_lod = true;
      } else {
         src[num_srcs++].ref = lod;
      }

      for (unsigned i = devinfo->gen >= 9 ? 2 : 1; i < num_coord_comps; i++)
         src[num_srcs++].ref = ibc_comp_ref(coord, i);

      coord_done = true;
      break;

   case IBC_INTRINSIC_OP_TXF_MS:
      src[num_srcs++].ref = sample_index;
      src[num_srcs++].ref = ibc_comp_ref(mcs, 0);
      if (devinfo->gen >= 9)
         src[num_srcs++].ref = ibc_comp_ref(mcs, 1);
      break;

   case IBC_INTRINSIC_OP_TG4_OFFSET:
      /* More crazy intermixing */
      for (unsigned i = 0; i < 2; i++) /* u, v */
         src[num_srcs++].ref = ibc_comp_ref(coord, i);

      for (unsigned i = 0; i < 2; i++) /* offu, offv */
         src[num_srcs++].ref = ibc_comp_ref(tg4_offset, i);

      if (num_coord_comps == 3) /* r if present */
         src[num_srcs++].ref = ibc_comp_ref(coord, 2);

      coord_done = true;
      break;
   default:
      break;
   }

   /* Set up the coordinate (except for cases where it was done above) */
   if (!coord_done) {
      for (unsigned i = 0; i < num_coord_comps; i++)
         src[num_srcs++].ref = ibc_comp_ref(coord, i);
   }

   if (min_lod.file != IBC_FILE_NONE) {
      /* Account for all of the missing coordinate sources */
      for (unsigned i = 0; i < 4 - num_coord_comps; i++)
         src[num_srcs++].ref = ibc_null(coord.type);

      if (intrin->op == IBC_INTRINSIC_OP_TXD) {
         for (unsigned i = 0; i < (3 - num_grad_comps); i++) {
            src[num_srcs++].ref = ibc_null(ddx.type);
            src[num_srcs++].ref = ibc_null(ddy.type);
         }
      }

      src[num_srcs++].ref = min_lod;
   }

   unsigned mlen;
   send->payload[0] = ibc_MESSAGE(b, src, num_srcs, &mlen);
   send->mlen = mlen;

   assert(ibc_type_bit_size(intrin->dest.type) == 32);
   if (intrin->instr.simd_width == 1) {
      send->rlen = intrin->num_dest_comps;
      ibc_reg *tmp_reg =
         ibc_hw_grf_reg_create(b->shader, send->rlen * REG_SIZE, REG_SIZE);
      send->dest = ibc_typed_ref(tmp_reg, intrin->dest.type);
   } else {
      assert(intrin->instr.simd_width >= 8);
      send->dest = intrin->dest;
      send->rlen = intrin->num_dest_comps * intrin->instr.simd_width / 8;
   }

   const unsigned msg_type =
      sampler_msg_type(devinfo, intrin->op,
                       shadow_c.file != IBC_FILE_NONE, zero_lod);
   const unsigned simd_mode =
      intrin->instr.simd_width <= 8 ? BRW_SAMPLER_SIMD_MODE_SIMD8 :
                                      BRW_SAMPLER_SIMD_MODE_SIMD16;

   send->sfid = BRW_SFID_SAMPLER;
   if (surface_bti.file == IBC_FILE_IMM &&
       (sampler_bti.file == IBC_FILE_IMM ||
        sampler_handle.file != IBC_FILE_NONE)) {
      send->desc_imm = brw_sampler_desc(devinfo,
                                        surface_bti_imm,
                                        sampler_bti_imm % 16,
                                        msg_type, simd_mode,
                                        0 /* return_format unused on gen7+ */);
   } else if (surface_handle.file != IBC_FILE_NONE) {
      /* Bindless surface */
      assert(devinfo->gen >= 9);
      send->desc_imm = brw_sampler_desc(devinfo,
                                        GEN9_BTI_BINDLESS,
                                        sampler_bti_imm % 16,
                                        msg_type, simd_mode,
                                        0 /* return_format unused on gen7+ */);

      /* For bindless samplers, the entire address is included in the message
       * header so we can leave the portion in the message descriptor 0.
       */
      if (sampler_bti.file != IBC_FILE_NONE &&
          sampler_bti.file != IBC_FILE_IMM) {
         ibc_builder_push_scalar(b);
         send->desc = ibc_SHL(b, IBC_TYPE_UD, sampler_bti, ibc_imm_ud(8));
         ibc_builder_pop(b);
      }

      /* We assume that the driver provided the handle in the top 20 bits so
       * we can use the surface handle directly as the extended descriptor.
       */
      send->ex_desc = surface_handle;
   } else {
      /* Immediate portion of the descriptor */
      send->desc_imm = brw_sampler_desc(devinfo,
                                        0, /* surface */
                                        0, /* sampler */
                                        msg_type, simd_mode,
                                        0 /* return_format unused on gen7+ */);
      ibc_builder_push_scalar(b);
      if (ibc_refs_equal(surface_bti, sampler_bti)) {
         /* This case is common in GL */
         send->desc = ibc_IMUL(b, IBC_TYPE_UD, surface_bti, ibc_imm_ud(0x101));
      } else {
         if (sampler_handle.file != IBC_FILE_NONE) {
            send->desc = ibc_MOV(b, IBC_TYPE_UD, surface_bti);
         } else if (sampler_bti.file == IBC_FILE_IMM) {
            send->desc = ibc_OR(b, IBC_TYPE_UD, surface_bti,
                                   ibc_imm_ud(sampler_bti_imm << 8));
         } else {
            send->desc = ibc_OR(b, IBC_TYPE_UD, surface_bti,
                                   ibc_SHL(b, IBC_TYPE_UD, sampler_bti,
                                              ibc_imm_ud(8)));
         }
         /* Mask off bits to be sure */
         send->desc = ibc_AND(b, IBC_TYPE_UD, send->desc, ibc_imm_ud(0xfff));
      }
      ibc_builder_pop(b);
   }

   ibc_builder_insert_instr(b, &send->instr);

   ibc_builder_pop(b);

   if (intrin->instr.simd_width == 1) {
      ibc_builder_push_scalar(b);
      assert(ibc_type_bit_size(intrin->dest.type) == 32);
      assert(send->dest.type == intrin->dest.type);

      ibc_ref vec_src[4];
      assert(intrin->num_dest_comps <= ARRAY_SIZE(vec_src));
      for (unsigned i = 0; i < intrin->num_dest_comps; i++) {
         vec_src[i] = send->dest;
         vec_src[i].hw_grf.byte += i * REG_SIZE;
         ibc_hw_grf_mul_stride(&vec_src[i].hw_grf, 0);
      }
      ibc_VEC_to(b, intrin->dest, vec_src, intrin->num_dest_comps);

      ibc_builder_pop(b);
   }

   ibc_instr_remove(&intrin->instr);

   return true;
}

bool
ibc_lower_io_to_sends(ibc_shader *shader)
{
   bool progress = false;

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      b.cursor = ibc_before_instr(instr);

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO:
         progress |= lower_bti_block_load_ubo(&b, intrin);
         break;

      case IBC_INTRINSIC_OP_BTI_TYPED_READ:
      case IBC_INTRINSIC_OP_BTI_TYPED_WRITE:
      case IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC:
      case IBC_INTRINSIC_OP_BTI_UNTYPED_READ:
      case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:
      case IBC_INTRINSIC_OP_BTI_BYTE_SCATTERED_READ:
      case IBC_INTRINSIC_OP_BTI_BYTE_SCATTERED_WRITE:
      case IBC_INTRINSIC_OP_BTI_UNTYPED_ATOMIC:
      case IBC_INTRINSIC_OP_A64_UNTYPED_READ:
      case IBC_INTRINSIC_OP_A64_UNTYPED_WRITE:
      case IBC_INTRINSIC_OP_A64_BYTE_SCATTERED_READ:
      case IBC_INTRINSIC_OP_A64_BYTE_SCATTERED_WRITE:
      case IBC_INTRINSIC_OP_A64_UNTYPED_ATOMIC_INT64:
      case IBC_INTRINSIC_OP_A64_UNTYPED_ATOMIC:
         progress |= lower_surface_access(&b, intrin);
         break;

      case IBC_INTRINSIC_OP_URB_READ:
         progress |= ibc_lower_io_urb_read_to_send(&b, intrin);
         break;

      case IBC_INTRINSIC_OP_URB_WRITE:
         progress |= ibc_lower_io_urb_write_to_send(&b, intrin);
         break;

      case IBC_INTRINSIC_OP_FB_WRITE:
         progress |= ibc_lower_io_fb_write_to_send(&b, intrin);
         break;

      case IBC_INTRINSIC_OP_TEX:
      case IBC_INTRINSIC_OP_TXB:
      case IBC_INTRINSIC_OP_TXL:
      case IBC_INTRINSIC_OP_TXD:
      case IBC_INTRINSIC_OP_TXF:
      case IBC_INTRINSIC_OP_TXF_MS:
      case IBC_INTRINSIC_OP_TXF_MCS:
      case IBC_INTRINSIC_OP_TXS:
      case IBC_INTRINSIC_OP_LOD:
      case IBC_INTRINSIC_OP_TG4:
      case IBC_INTRINSIC_OP_TG4_OFFSET:
      case IBC_INTRINSIC_OP_SAMPLEINFO:
         progress |= lower_tex(&b, intrin);
         break;

      default:
         break;
      }
   }

   return progress;
}
