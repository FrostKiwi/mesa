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

static ibc_reg_ref
move_to_payload(ibc_builder *b, ibc_reg_ref src, unsigned num_comps)
{
   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, src.type, num_comps);
   ibc_MOV_raw_vec_to(b, dest, src, num_comps);
   return dest;
}

static void
lower_const_block_read(ibc_builder *b, ibc_send_instr *send,
                       const ibc_intrinsic_instr *read)
{
   const unsigned block_size_B =
      read->num_dest_comps * ibc_type_byte_size(read->dest.type);
   assert(block_size_B % REG_SIZE == 0);
   const unsigned block_size_DW = block_size_B / 4;

   /* It's actually going to be a SIMD8 or SIMD16 send */
   assert(send->instr.we_all);
   send->instr.simd_width = block_size_DW;

   send->sfid = GEN6_SFID_DATAPORT_CONSTANT_CACHE;
   send->desc_imm =
      brw_dp_read_desc(b->shader->devinfo, 0,
                       BRW_DATAPORT_OWORD_BLOCK_DWORDS(block_size_DW),
                       GEN7_DATAPORT_DC_OWORD_BLOCK_READ,
                       BRW_DATAPORT_READ_TARGET_DATA_CACHE);
   assert(read->src[0].ref.type == IBC_TYPE_UD);
   send->desc = read->src[0].ref;

   assert(read->src[1].ref.file == IBC_REG_FILE_IMM);
   assert(read->src[1].ref.type == IBC_TYPE_UD);
   const uint32_t offset_B = *(uint32_t *)read->src[1].ref.imm;

   ibc_reg *msg = ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);

   ibc_builder_push_we_all(b, 8);
   ibc_MOV_to(b, ibc_typed_ref(msg, IBC_TYPE_UD),
                 ibc_hw_grf_ref(0, 0, IBC_TYPE_UD));
   ibc_builder_pop(b);

   ibc_builder_push_scalar(b);
   ibc_reg_ref offset_ref = ibc_typed_ref(msg, IBC_TYPE_UD);
   offset_ref.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
   ibc_MOV_to(b, offset_ref, ibc_imm_ud(offset_B / 16));
   ibc_builder_pop(b);

   send->payload[0] = ibc_typed_ref(msg, IBC_TYPE_32_BIT);
   send->has_header = true;
   send->mlen = 1;

   send->dest = read->dest;
   send->rlen = block_size_B / REG_SIZE;
}

static void
lower_surface_access(ibc_builder *b, ibc_send_instr *send,
                     const ibc_intrinsic_instr *intrin)
{
   uint32_t desc;
   switch (intrin->op) {
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
                                            intrin->src[2].num_comps,
                                            true    /* write */);
      break;
   default:
      unreachable("Unhandled surface access intrinsic");
   }

   assert(intrin->src[0].ref.file == IBC_REG_FILE_IMM);
   assert(intrin->src[0].ref.type == IBC_TYPE_UD);
   send->desc_imm = desc | *(uint32_t *)intrin->src[0].ref.imm;

   send->dest = intrin->dest;
   send->rlen = intrin->num_dest_comps * intrin->instr.simd_width / 8;

   send->payload[0] = move_to_payload(b, intrin->src[1].ref, 1);
   send->mlen = intrin->instr.simd_width / 8;

   if (intrin->op == IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE) {
      send->payload[1] = move_to_payload(b, intrin->src[2].ref,
                                            intrin->src[2].num_comps);
      send->ex_mlen = intrin->src[2].num_comps * intrin->instr.simd_width / 8;
   }
}

static bool
is_high_sampler(const ibc_reg_ref sampler_bti)
{
   if (sampler_bti.file != IBC_REG_FILE_IMM)
      return true;

   assert(sampler_bti.type == IBC_TYPE_UD);
   return *(uint32_t *)sampler_bti.imm >= 16;
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

static bool
ref_is_null_or_zero(ibc_reg_ref ref)
{
   if (ref.file == IBC_REG_FILE_NONE)
      return true;

   if (ref.file != IBC_REG_FILE_IMM)
      return false;

   for (unsigned i = 0; i < ibc_type_byte_size(ref.type); i++) {
      if (ref.imm[i] != 0)
         return false;
   }

   return true;
}

static ibc_reg_ref
ibc_comp_ref(ibc_reg_ref ref, unsigned comp)
{
   if (ref.file == IBC_REG_FILE_IMM) {
      assert(comp == 0);
      return ref;
   }

   assert(ref.file == IBC_REG_FILE_LOGICAL);
   ref.logical.comp += comp;
   return ref;
}

#define MAX_SAMPLER_MESSAGE_SIZE 11

static void
lower_tex(ibc_builder *b, ibc_send_instr *send,
          const ibc_intrinsic_instr *intrin)
{
   const struct gen_device_info *devinfo = b->shader->devinfo;

   const ibc_reg_ref surface_bti = intrin->src[IBC_TEX_SRC_SURFACE_BTI].ref;
   const ibc_reg_ref surface_handle = intrin->src[IBC_TEX_SRC_SURFACE_HANDLE].ref;
   uint32_t surface_bti_imm = 0;
   if (surface_bti.file == IBC_REG_FILE_IMM) {
      assert(surface_bti.type == IBC_TYPE_UD);
      surface_bti_imm = *(uint32_t *)surface_bti.imm;
   }
   assert((surface_bti.file == IBC_REG_FILE_NONE) !=
          (surface_handle.file == IBC_REG_FILE_NONE));

   const ibc_reg_ref sampler_bti = intrin->src[IBC_TEX_SRC_SAMPLER_BTI].ref;
   const ibc_reg_ref sampler_handle = intrin->src[IBC_TEX_SRC_SAMPLER_HANDLE].ref;
   uint32_t sampler_bti_imm = 0;
   if (sampler_bti.file == IBC_REG_FILE_IMM) {
      assert(sampler_bti.type == IBC_TYPE_UD);
      sampler_bti_imm = *(uint32_t *)sampler_bti.imm;
   }
   assert((sampler_bti.file == IBC_REG_FILE_NONE) ||
          (sampler_handle.file == IBC_REG_FILE_NONE));

   const ibc_reg_ref coord = intrin->src[IBC_TEX_SRC_COORD].ref;
   const unsigned num_coord_comps = intrin->src[IBC_TEX_SRC_COORD].num_comps;
   const ibc_reg_ref shadow_c = intrin->src[IBC_TEX_SRC_SHADOW_C].ref;
   const ibc_reg_ref lod = intrin->src[IBC_TEX_SRC_LOD].ref;
   const ibc_reg_ref min_lod = intrin->src[IBC_TEX_SRC_MIN_LOD].ref;
   const ibc_reg_ref ddx = intrin->src[IBC_TEX_SRC_DDX].ref;
   const ibc_reg_ref ddy = intrin->src[IBC_TEX_SRC_DDY].ref;
   assert(intrin->src[IBC_TEX_SRC_DDY].num_comps ==
          intrin->src[IBC_TEX_SRC_DDY].num_comps);
   const unsigned num_grad_comps = intrin->src[IBC_TEX_SRC_DDY].num_comps;
   const ibc_reg_ref sample_index = intrin->src[IBC_TEX_SRC_SAMPLE_INDEX].ref;
   const ibc_reg_ref mcs = intrin->src[IBC_TEX_SRC_MCS].ref;
   const ibc_reg_ref tg4_offset = intrin->src[IBC_TEX_SRC_TG4_OFFSET].ref;

   uint32_t header_bits = 0;
   const ibc_reg_ref header_bits_r = intrin->src[IBC_TEX_SRC_HEADER_BITS].ref;
   if (header_bits_r.file != IBC_REG_FILE_NONE) {
      assert(header_bits_r.file == IBC_REG_FILE_IMM);
      assert(header_bits_r.type == IBC_TYPE_UD);
      header_bits = *(uint32_t *)header_bits_r.imm;
   }

   ibc_reg_ref srcs[MAX_SAMPLER_MESSAGE_SIZE] = {};
   unsigned num_srcs = 0;

   if (intrin->op == IBC_INTRINSIC_OP_TG4 ||
       intrin->op == IBC_INTRINSIC_OP_TG4_OFFSET ||
       intrin->op == IBC_INTRINSIC_OP_SAMPLEINFO ||
       header_bits != 0 || sampler_handle.file != IBC_REG_FILE_NONE ||
       is_high_sampler(sampler_bti)) {

      ibc_reg *header_reg =
         ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);
      ibc_reg_ref header = ibc_typed_ref(header_reg, IBC_TYPE_UD);

      /* If we're requesting fewer than four channels worth of response,
       * and we have an explicit header, we need to set up the sampler
       * writemask.  It's reversed from normal: 1 means "don't write".
       */
      if (intrin->num_dest_comps < 4) {
         unsigned mask = ~((1 << intrin->num_dest_comps) - 1) & 0xf;
         header_bits |= mask << 12;
      }

      ibc_builder_push_we_all(b, 8);
      ibc_MOV_to(b, header, ibc_hw_grf_ref(0, 0, IBC_TYPE_UD));
      ibc_builder_pop(b);

      /* Everything else just sets up components */
      ibc_builder_push_scalar(b);

      /* TODO: On vertex and fragment stages this is zero by default so we can
       * avoid the MOV.
       */
      ibc_reg_ref header_2 = header;
      header_2.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_MOV_to(b, header_2, ibc_imm_ud(header_bits));

      ibc_reg_ref header_3 = header;
      header_3.hw_grf.byte += 3 * ibc_type_byte_size(IBC_TYPE_UD);
      if (sampler_handle.file != IBC_REG_FILE_NONE) {
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
         ibc_reg_ref sampler_base_ptr;
         if (sampler_bti.file == IBC_REG_FILE_IMM) {
            assert(sampler_bti_imm >= 16);
            const unsigned sampler_state_size = 16; /* 16 bytes */
            const uint32_t sampler_offset_B =
               16 * (sampler_bti_imm / 16) * sampler_state_size;

            sampler_base_ptr =
               ibc_ADD(b, IBC_TYPE_UD, ibc_hw_grf_ref(0, 3, IBC_TYPE_UD),
                          ibc_imm_ud(sampler_offset_B));
         } else {
            sampler_base_ptr =
               ibc_ADD(b, IBC_TYPE_UD, ibc_hw_grf_ref(0, 3, IBC_TYPE_UD),
                          ibc_SHL(b, IBC_TYPE_UD,
                                     ibc_AND(b, IBC_TYPE_UD, sampler_bti,
                                                ibc_imm_ud(0xf0)),
                                    ibc_imm_ud(4)));
         }
         ibc_MOV_to(b, header_3, sampler_base_ptr);
      }
      ibc_builder_pop(b);

      srcs[num_srcs++] = header;
      send->has_header = true;
   }

   if (shadow_c.file != IBC_REG_FILE_NONE)
      srcs[num_srcs++] = shadow_c;

   bool coord_done = false;
   bool zero_lod = false;

   switch (intrin->op) {
   case IBC_INTRINSIC_OP_TXB:
      srcs[num_srcs++] = lod;
      break;

   case IBC_INTRINSIC_OP_TXL:
      if (devinfo->gen >= 9 && ref_is_null_or_zero(lod)) {
         zero_lod = true;
      } else {
         srcs[num_srcs++] = lod;
      }
      break;

   case IBC_INTRINSIC_OP_TXD:
      assert(b->simd_width == 8);

      /* Load dPdx and the coordinate together:
       * [hdr], [ref], x, dPdx.x, dPdy.x, y, dPdx.y, dPdy.y, z, dPdx.z, dPdy.z
       */
      for (unsigned i = 0; i < num_coord_comps; i++) {
         srcs[num_srcs++] = ibc_comp_ref(coord, i);

         /* For cube map array, the coordinate is (u,v,r,ai) but there are
          * only derivatives for (u, v, r).
          */
         if (i < num_grad_comps) {
            srcs[num_srcs++] = ibc_comp_ref(ddx, i);
            srcs[num_srcs++] = ibc_comp_ref(ddy, i);
         }
      }
      coord_done = true;
      break;

   case IBC_INTRINSIC_OP_TXS:
      srcs[num_srcs++] = lod;
      break;

   case IBC_INTRINSIC_OP_TXF:
      /* Unfortunately, the parameters for LD are intermixed: u, lod, v, r.
       * On Gen9 they are u, v, lod, r
       */
      srcs[num_srcs++] = ibc_comp_ref(coord, 0);

      if (devinfo->gen >= 9) {
         srcs[num_srcs++] = num_coord_comps >= 2 ? ibc_comp_ref(coord, 1) :
                                                  ibc_imm_ud(0);
      }

      if (devinfo->gen >= 9 && ref_is_null_or_zero(lod)) {
         zero_lod = true;
      } else {
         srcs[num_srcs++] = lod;
      }

      for (unsigned i = devinfo->gen >= 9 ? 2 : 1; i < num_coord_comps; i++)
         srcs[num_srcs++] = ibc_comp_ref(coord, i);

      coord_done = true;
      break;

   case IBC_INTRINSIC_OP_TXF_MS:
      srcs[num_srcs++] = sample_index;
      srcs[num_srcs++] = ibc_comp_ref(mcs, 0);
      if (devinfo->gen >= 9)
         srcs[num_srcs++] = ibc_comp_ref(mcs, 1);
      break;

   case IBC_INTRINSIC_OP_TG4_OFFSET:
      /* More crazy intermixing */
      for (unsigned i = 0; i < 2; i++) /* u, v */
         srcs[num_srcs++] = ibc_comp_ref(coord, i);

      for (unsigned i = 0; i < 2; i++) /* offu, offv */
         srcs[num_srcs++] = ibc_comp_ref(tg4_offset, i);

      if (num_coord_comps == 3) /* r if present */
         srcs[num_srcs++] = ibc_comp_ref(coord, 2);

      coord_done = true;
      break;
   default:
      break;
   }

   /* Set up the coordinate (except for cases where it was done above) */
   if (!coord_done) {
      for (unsigned i = 0; i < num_coord_comps; i++)
         srcs[num_srcs++] = ibc_comp_ref(coord, i);
   }

   if (min_lod.file != IBC_REG_FILE_NONE) {
      /* Account for all of the missing coordinate sources */
      num_srcs += 4 - num_coord_comps;
      if (intrin->op == IBC_INTRINSIC_OP_TXD)
         num_srcs += (3 - num_grad_comps) * 2;

      srcs[num_srcs++] = min_lod;
   }

   for (unsigned i = 0; i < num_srcs; i++) {
      if (i == 0 && send->has_header) {
         assert(srcs[i].file == IBC_REG_FILE_HW_GRF);
         send->mlen++;
      } else {
         send->mlen += b->simd_width / 8;
      }
   }

   ibc_reg *message =
      ibc_hw_grf_reg_create(b->shader, send->mlen * REG_SIZE, REG_SIZE);
   ibc_reg_ref msg_dest = ibc_typed_ref(message, IBC_TYPE_UD);
   for (unsigned i = 0; i < num_srcs; i++) {
      msg_dest.type = srcs[i].type;
      if (i == 0 && send->has_header) {
         ibc_builder_push_we_all(b, 8);
         ibc_MOV_to(b, msg_dest, srcs[i]);
         ibc_builder_pop(b);
         msg_dest.hw_grf.byte += REG_SIZE;
      } else if (srcs[i].file == IBC_REG_FILE_NONE) {
         /* Just advance it */
         msg_dest.hw_grf.byte += b->simd_width * sizeof(uint32_t);
      } else {
         ibc_MOV_to(b, msg_dest, srcs[i]);
         msg_dest.hw_grf.byte += b->simd_width * sizeof(uint32_t);
      }
   }
   send->payload[0] = ibc_typed_ref(message, IBC_TYPE_32_BIT);

   send->dest = intrin->dest;
   send->rlen = intrin->num_dest_comps * intrin->instr.simd_width / 8;

   const unsigned msg_type =
      sampler_msg_type(devinfo, intrin->op,
                       shadow_c.file != IBC_REG_FILE_NONE, zero_lod);
   const unsigned simd_mode =
      intrin->instr.simd_width <= 8 ? BRW_SAMPLER_SIMD_MODE_SIMD8 :
                                      BRW_SAMPLER_SIMD_MODE_SIMD16;

   send->sfid = BRW_SFID_SAMPLER;
   if (surface_bti.file == IBC_REG_FILE_IMM &&
       (sampler_bti.file == IBC_REG_FILE_IMM ||
        sampler_handle.file != IBC_REG_FILE_NONE)) {
      send->desc_imm = brw_sampler_desc(devinfo,
                                        surface_bti_imm,
                                        sampler_bti_imm % 16,
                                        msg_type, simd_mode,
                                        0 /* return_format unused on gen7+ */);
   } else if (surface_handle.file != IBC_REG_FILE_NONE) {
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
      if (sampler_bti.file != IBC_REG_FILE_NONE &&
          sampler_bti.file != IBC_REG_FILE_IMM) {
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
      if (0 /* TODO: ibc_reg_refs_equal(surface_bti, sampler_bti) */) {
         /* This case is common in GL */
         send->desc = ibc_MUL(b, IBC_TYPE_UD, surface_bti, ibc_imm_ud(0x101));
      } else {
         if (sampler_handle.file != IBC_REG_FILE_NONE) {
            send->desc = ibc_MOV(b, IBC_TYPE_UD, surface_bti);
         } else if (sampler_bti.file == IBC_REG_FILE_IMM) {
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

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL:
      case IBC_INTRINSIC_OP_SIMD_BROADCAST:
      case IBC_INTRINSIC_OP_SIMD_ZIP:
      case IBC_INTRINSIC_OP_VEC:
      case IBC_INTRINSIC_OP_LOAD_PAYLOAD:
      case IBC_INTRINSIC_OP_PLN:
         continue;
      default:
         break;
      }

      b.cursor = ibc_before_instr(instr);
      assert(b._group_stack_size == 0);
      if (instr->we_all)
         ibc_builder_push_we_all(&b, instr->simd_width);
      else
         ibc_builder_push_group(&b, instr->simd_group, instr->simd_width);

      ibc_send_instr *send = ibc_send_instr_create(shader,
                                                   instr->simd_group,
                                                   instr->simd_width);
      send->instr.we_all = instr->we_all;
      send->has_side_effects = intrin->has_side_effects;

      switch (intrin->op) {
      case IBC_INTRINSIC_OP_BTI_CONST_BLOCK_READ:
         lower_const_block_read(&b, send, intrin);
         break;

      case IBC_INTRINSIC_OP_BTI_UNTYPED_READ:
      case IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE:
         lower_surface_access(&b, send, intrin);
         break;

      case IBC_INTRINSIC_OP_FB_WRITE:
         ibc_lower_io_fb_write_to_send(&b, send, intrin);
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
         lower_tex(&b, send, intrin);
         break;

      default:
         unreachable("Unsupported intrinsic");
      }

      ibc_builder_insert_instr(&b, &send->instr);
      ibc_instr_remove(&intrin->instr);

      ibc_builder_pop(&b);
      progress = true;
   }

   return progress;
}