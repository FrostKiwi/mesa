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

#include "ibc_compile.h"
#include "ibc_nir.h"

void
ibc_emit_urb_read(ibc_builder *b,
                  ibc_ref dest,
                  ibc_ref handle,
                  ibc_ref per_slot_offset,
                  unsigned global_offset,
                  unsigned num_components)
{
   ibc_intrinsic_src srcs[IBC_URB_READ_NUM_SRCS] = {
      [IBC_URB_READ_SRC_HANDLE] = { .ref = handle, .num_comps = 1 },
      [IBC_URB_READ_SRC_PER_SLOT_OFFSET] =
         { .ref = per_slot_offset, .num_comps = 1 },
      [IBC_URB_READ_SRC_GLOBAL_OFFSET] =
         { .ref = ibc_imm_ud(global_offset), .num_comps = 1 },
   };

   ibc_intrinsic_instr *read =
      ibc_build_intrinsic(b, IBC_INTRINSIC_OP_URB_READ,
                          dest, -1, num_components,
                          srcs, IBC_URB_READ_NUM_SRCS);

   read->can_reorder = false;
   read->has_side_effects = true;
}

static void
ibc_emit_urb_write(ibc_builder *b,
                   ibc_ref handle,
                   ibc_ref *sources,
                   unsigned global_offset,
                   unsigned length,
                   bool eot)
{
   ibc_ref data = ibc_VEC(b, sources, length);

   ibc_intrinsic_src srcs[IBC_URB_WRITE_NUM_SRCS] = {
      [IBC_URB_WRITE_SRC_HANDLE] = { .ref = handle, .num_comps = 1 },
      [IBC_URB_WRITE_SRC_DATA]   = { .ref = data, .num_comps = length },
      [IBC_URB_WRITE_SRC_EOT]    = { .ref = ibc_imm_ud(eot), .num_comps = 1 },
      [IBC_URB_WRITE_SRC_GLOBAL_OFFSET] =
         { .ref = ibc_imm_ud(global_offset), .num_comps = 1 },
   };

   ibc_intrinsic_instr *write =
      ibc_build_intrinsic(b, IBC_INTRINSIC_OP_URB_WRITE,
                          ibc_null(IBC_TYPE_UD), 0, 0,
                          srcs, IBC_URB_WRITE_NUM_SRCS);

   write->can_reorder = false;
   write->has_side_effects = true;
}

static bool
output_slot_unwritten(const ibc_ref *outputs,
                      const struct brw_vue_map *vue_map, int slot)
{
   gl_varying_slot varying = vue_map->slot_to_varying[slot];

   return (int)varying == BRW_VARYING_SLOT_PAD ||
          outputs[varying].file == IBC_FILE_NONE;
}

void
ibc_emit_urb_writes(ibc_builder *b,
                    const struct brw_vue_map *vue_map,
                    ibc_ref handle,
                    const ibc_ref *outputs)
{
   const uint64_t vue_header_mask =
      VARYING_BIT_LAYER | VARYING_BIT_VIEWPORT | VARYING_BIT_PSIZ;

   /* SSO shaders can have VUE slots allocated which are never actually
    * written to, so ignore them when looking for the last (written) slot.
    */
   int last_slot = vue_map->num_slots - 1;
   while (last_slot > 0 && output_slot_unwritten(outputs, vue_map, last_slot))
      last_slot--;

   unsigned starting_urb_offset = 0;
   unsigned urb_offset = starting_urb_offset;
   unsigned length = 0;
   bool flush = false;
   bool urb_written = false;
   ibc_ref sources[8];

   for (int slot = 0; slot < vue_map->num_slots; slot++) {
      int varying = vue_map->slot_to_varying[slot];

      if (varying == VARYING_SLOT_PSIZ) {
         /* This is actually the VUE header slot, and is always present.
          * But often the shader doesn't write any of the special values
          * that live there, so we can skip writing it, leaving it to the
          * driver to properly clamp those values.
          */
         if ((vue_map->slots_valid & vue_header_mask) == 0) {
            assert(length == 0);
            urb_offset++;
         } else {
            ibc_ref zero = ibc_imm_zero(IBC_TYPE_UD);

            sources[length++] = zero;
            sources[length++] = (vue_map->slots_valid & VARYING_BIT_LAYER) ?
                                outputs[VARYING_SLOT_LAYER] : zero;
            sources[length++] = (vue_map->slots_valid & VARYING_BIT_VIEWPORT) ?
                                outputs[VARYING_SLOT_VIEWPORT] : zero;
            sources[length++] = (vue_map->slots_valid & VARYING_BIT_PSIZ) ?
                                outputs[VARYING_SLOT_PSIZ] : zero;
         }
      } else if (output_slot_unwritten(outputs, vue_map, slot)) {
         /* Some outputs (like position or clip distances) may have output
          * slots assigned but not be written by the shader.  If we've already
          * queued up writes, flush them; otherwise just advance urb_offset to
          * skip the unwritten outputs.
          */
         if (length > 0)
            flush = true;
         else
            urb_offset++;
      } else {
         for (unsigned i = 0; i < 4; i++) {
            sources[length] = outputs[varying];
            sources[length].logical.comp = i;
            length++;
         }
      }

      /* If we've queued up 8 registers of payload (2 VUE slots), or if this
       * in the last slot, then we need to flush out a URB write now.
       */
      if (length == 8 || (length > 0 && slot == last_slot))
         flush = true;

      if (flush) {
         /* ICL WA 1805992985: see ibc_emit_icl_tes_eot_workaround(). */
         bool icl_tes_eot_wa = b->shader->devinfo->gen == 11 &&
                               b->shader->stage == MESA_SHADER_TESS_EVAL;

         bool eot = slot == last_slot && !icl_tes_eot_wa;

         ibc_emit_urb_write(b, handle, sources, urb_offset, length, eot);

         length = 0;
         urb_offset = starting_urb_offset + slot + 1;
         flush = false;
         urb_written = true;
      }
   }

   /* If we don't have any valid slots to write, do a minimal URB write send
    * to terminate the shader.  This includes 1 slot of undefined data since:
    *
    *    "The write data payload can be between 1 and 8 message phases long."
    */
   if (!urb_written) {
      sources[0] = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, 1);
      ibc_emit_urb_write(b, handle, sources, urb_offset, 1, true);
   }
}

bool
ibc_lower_io_urb_read_to_send(ibc_builder *b, ibc_intrinsic_instr *read)
{
   const ibc_ref handle = read->src[IBC_URB_READ_SRC_HANDLE].ref;
   ibc_ref per_slot_offset =
      read->src[IBC_URB_READ_SRC_PER_SLOT_OFFSET].ref;
   const uint32_t global_offset =
      ibc_ref_as_uint(read->src[IBC_URB_READ_SRC_GLOBAL_OFFSET].ref);
   const bool per_slot_offset_present =
      !ibc_ref_is_null_or_zero(per_slot_offset);

   ibc_builder_push_instr_group(b, &read->instr);

   assert(!read->can_reorder && read->has_side_effects);
   assert(read->instr.predicate == IBC_PREDICATE_NONE);
   ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 8);
   send->can_reorder = false;
   send->has_side_effects = true;

   ibc_intrinsic_src msg_srcs[2];
   unsigned num_msg_srcs = 0;

   msg_srcs[num_msg_srcs++] = (ibc_intrinsic_src) { .ref = handle, };

   if (read->instr.simd_width == 1)
      per_slot_offset = ibc_uniformize(b, per_slot_offset);

   if (per_slot_offset_present)
      msg_srcs[num_msg_srcs++] = (ibc_intrinsic_src){ .ref = per_slot_offset };

   unsigned mlen;
   ibc_ref msg = ibc_MESSAGE(b, msg_srcs, num_msg_srcs, &mlen);

   send->payload[0] = msg;
   send->dest = read->dest;
   send->mlen = mlen;
   send->rlen = read->num_dest_comps;
   send->sfid = BRW_SFID_URB;
   send->desc_imm =
      brw_urb_desc(b->shader->devinfo, GEN8_URB_OPCODE_SIMD8_READ,
                   per_slot_offset_present, false, global_offset);

   send->has_header = true;
   send->has_side_effects = true;

   if (read->instr.simd_width == 1) {
      ibc_reg *tmp_reg =
         ibc_hw_grf_reg_create(b->shader, send->rlen * REG_SIZE, REG_SIZE);
      send->dest = ibc_typed_ref(tmp_reg, read->dest.type);
      send->instr.we_all = true;
   }

   ibc_builder_insert_instr(b, &send->instr);

   ibc_builder_pop(b);

   /* XXX: deduplicate with lower_surface_access etc */
   if (read->instr.simd_width == 1) {
      ibc_builder_push_scalar(b);
      assert(ibc_type_bit_size(read->dest.type) == 32);
      assert(send->dest.type == read->dest.type);

      ibc_ref vec_src[4];
      assert(read->num_dest_comps <= ARRAY_SIZE(vec_src));
      for (unsigned i = 0; i < read->num_dest_comps; i++) {
         vec_src[i] = send->dest;
         vec_src[i].hw_grf.byte += i * REG_SIZE;
         ibc_hw_grf_mul_stride(&vec_src[i].hw_grf, 0);
      }
      ibc_VEC_to(b, read->dest, vec_src, read->num_dest_comps);

      ibc_builder_pop(b);
   }

   ibc_instr_remove(&read->instr);

   return true;
}

bool
ibc_lower_io_urb_write_to_send(ibc_builder *b, ibc_intrinsic_instr *write)
{
   const ibc_ref handle = write->src[IBC_URB_WRITE_SRC_HANDLE].ref;
   const ibc_ref data = write->src[IBC_URB_WRITE_SRC_DATA].ref;
   const unsigned num_data_comps = write->src[IBC_URB_WRITE_SRC_DATA].num_comps;
   assert(data.file == IBC_FILE_LOGICAL);
   const uint32_t global_offset =
      ibc_ref_as_uint(write->src[IBC_URB_WRITE_SRC_GLOBAL_OFFSET].ref);
   const bool eot = ibc_ref_as_uint(write->src[IBC_URB_WRITE_SRC_EOT].ref);
   const bool per_slot_offset_present = false;
   const bool channel_mask_present = false;

   ibc_builder_push_instr_group(b, &write->instr);

   assert(!write->instr.we_all);
   assert(!write->can_reorder && write->has_side_effects);
   assert(write->instr.predicate == IBC_PREDICATE_NONE);
   ibc_send_instr *send = ibc_send_instr_create(b->shader,
                                                write->instr.simd_group,
                                                write->instr.simd_width);
   send->can_reorder = false;
   send->has_side_effects = true;

   assert(write->src[IBC_URB_WRITE_SRC_DATA].num_comps <= 8);

   ibc_intrinsic_src msg_srcs[9];
   unsigned num_msg_srcs = 0;

   msg_srcs[num_msg_srcs++] = (ibc_intrinsic_src) {
      .ref = handle,
   };

   assert(1 + num_data_comps <= ARRAY_SIZE(msg_srcs));
   for (unsigned i = 0; i < num_data_comps; i++) {
      msg_srcs[num_msg_srcs++] = (ibc_intrinsic_src) {
         .ref = ibc_comp_ref(data, i),
      };
   }
   assert(num_msg_srcs <= ARRAY_SIZE(msg_srcs));

   unsigned mlen;
   ibc_ref msg = ibc_MESSAGE(b, msg_srcs, num_msg_srcs, &mlen);

   send->payload[0] = msg;
   send->mlen = mlen;

   send->sfid = BRW_SFID_URB;
   send->desc_imm =
      brw_urb_desc(b->shader->devinfo, GEN8_URB_OPCODE_SIMD8_WRITE,
                   per_slot_offset_present, channel_mask_present,
                   global_offset);

   send->has_header = true;
   send->has_side_effects = true;
   send->eot = eot;

   ibc_builder_insert_instr(b, &send->instr);

   ibc_builder_pop(b);

   ibc_instr_remove(&write->instr);

   return true;
}
