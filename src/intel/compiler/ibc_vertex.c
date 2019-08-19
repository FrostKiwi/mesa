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

#include "brw_nir.h"
#include "ibc_compile.h"
#include "ibc_nir.h"
#include "dev/gen_debug.h"

struct ibc_vs_payload {
   struct ibc_payload_base base;

   ibc_ref urb_return_handles;
   ibc_ref inputs[32];
};

struct nir_vs_to_ibc_state {
   struct {
      ibc_ref outputs[VARYING_SLOT_MAX];
   } out;
};

static struct ibc_vs_payload *
ibc_setup_vs_payload(ibc_builder *b,
                     struct brw_vs_prog_data *prog_data,
                     void *mem_ctx)
{
   struct ibc_vs_payload *payload = rzalloc(mem_ctx, struct ibc_vs_payload);
   ibc_setup_payload_base(b, &payload->base);

   unsigned reg = payload->base.num_ff_regs;

   payload->urb_return_handles =
      ibc_load_payload_logical(b, &reg, IBC_TYPE_UD, 1);

   payload->base.num_ff_regs = reg;

   assert(prog_data->base.base.nr_params == 0);

   assert(prog_data->base.urb_read_length <= 15);

   /* We represent per-vertex attributes in the payload as a SIMD8 vec4 for
    * each component coming in from the vertex fetcher.
    *
    * TODO: VF component packing?
    */
   for (unsigned i = 0; i < prog_data->nr_attribute_slots; i++) {
      payload->inputs[i] = ibc_load_payload_logical(b, &reg, IBC_TYPE_UD, 4);
   }

   return payload;
}

bool
ibc_emit_nir_vs_intrinsic(struct nir_to_ibc_state *nti,
                          const nir_intrinsic_instr *instr)
{
   assert(nti->stage == MESA_SHADER_VERTEX);
   struct ibc_vs_payload *payload = (struct ibc_vs_payload *)nti->payload;
   struct nir_vs_to_ibc_state *nti_vs = nti->stage_state;
   ibc_builder *b = &nti->b;

   ibc_ref dest = { .file = IBC_REG_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_input: {
      const unsigned input = nir_intrinsic_base(instr) +
                             nir_src_as_uint(instr->src[0]);
      ibc_ref srcs[4];

      for (unsigned i = 0; i < instr->num_components; i++) {
         srcs[i] = payload->inputs[input];
         srcs[i].logical.comp = nir_intrinsic_component(instr) + i;
      }

      dest = ibc_VEC(b, srcs, instr->num_components);
      break;
   }

   case nir_intrinsic_store_output: {
      ibc_ref src = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD);
      const unsigned varying = nir_intrinsic_base(instr) +
                               nir_src_as_uint(instr->src[1]);

      ibc_ref *output = &nti_vs->out.outputs[varying];

      if (output->file == IBC_REG_FILE_NONE) {
         const unsigned num_comps = (varying == VARYING_SLOT_PSIZ ||
                                     varying == VARYING_SLOT_VIEWPORT ||
                                     varying == VARYING_SLOT_LAYER) ? 1 : 4;
         *output = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, num_comps);
      }

      ibc_ref dest = *output;
      dest.logical.comp += nir_intrinsic_component(instr);
      for (unsigned j = 0; j < instr->num_components; j++) {
         ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
         src.logical.comp++;
         dest.logical.comp++;
      }
      break;
   }

   default:
      return false;
   }

   if (nir_intrinsic_infos[instr->intrinsic].has_dest)
      ibc_write_nir_dest(nti, &instr->dest, dest);
   else
      assert(dest.file == IBC_REG_FILE_NONE);

   return true;
}

enum ibc_urb_write_src {
   IBC_URB_WRITE_SRC_HANDLE,          /* REQUIRED */
   IBC_URB_WRITE_SRC_DATA,            /* REQUIRED */
   IBC_URB_WRITE_SRC_GLOBAL_OFFSET,   /* REQUIRED - immediate */
   IBC_URB_WRITE_SRC_PER_SLOT_OFFSET, /* optional */
   IBC_URB_WRITE_SRC_CHANNEL_MASK,    /* optional */
   IBC_URB_WRITE_SRC_EOT,             /* REQUIRED - immediate boolean */
   IBC_URB_WRITE_NUM_SRCS,
};

static void
ibc_emit_urb_write(struct nir_to_ibc_state *nti,
                   ibc_ref handle,
                   ibc_ref *sources,
                   unsigned global_offset,
                   unsigned length,
                   bool eot)
{
   ibc_builder *b = &nti->b;

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
                          ibc_null(IBC_TYPE_UD), 0,
                          srcs, IBC_URB_WRITE_NUM_SRCS);

   write->can_reorder = false;
   write->has_side_effects = true;
}

static bool
output_slot_unwritten(const struct nir_vs_to_ibc_state *nti_vs,
                      const struct brw_vue_map *vue_map, int slot)
{
   gl_varying_slot varying = vue_map->slot_to_varying[slot];

   return (int)varying == BRW_VARYING_SLOT_PAD ||
          nti_vs->out.outputs[varying].file == IBC_REG_FILE_NONE;
}

static void
ibc_emit_urb_writes(struct nir_to_ibc_state *nti)
{
   const struct brw_vs_prog_data *prog_data = (void *)nti->prog_data;
   const struct brw_vue_prog_data *vue_prog_data = &prog_data->base;
   const struct brw_vue_map *vue_map = &vue_prog_data->vue_map;
   struct nir_vs_to_ibc_state *nti_vs = nti->stage_state;
   struct ibc_vs_payload *payload = (struct ibc_vs_payload *)nti->payload;
   ibc_ref *outputs = nti_vs->out.outputs;
   ibc_builder *b = &nti->b;
   const uint64_t vue_header_mask =
      VARYING_BIT_LAYER | VARYING_BIT_VIEWPORT | VARYING_BIT_PSIZ;

   ibc_ref handle = payload->urb_return_handles;

   /* SSO shaders can have VUE slots allocated which are never actually
    * written to, so ignore them when looking for the last (written) slot.
    */
   int last_slot = vue_map->num_slots - 1;
   while (last_slot > 0 && output_slot_unwritten(nti_vs, vue_map, last_slot))
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
      } else if (output_slot_unwritten(nti_vs, vue_map, slot)) {
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
         bool eot = slot == last_slot;

         ibc_emit_urb_write(nti, handle, sources, urb_offset, length, eot);

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
      ibc_emit_urb_write(nti, handle, sources, urb_offset, 1, true);
   }
}

void
ibc_lower_io_urb_write_to_send(ibc_builder *b, ibc_send_instr *send,
                               const ibc_intrinsic_instr *write)
{
   const ibc_ref handle = write->src[IBC_URB_WRITE_SRC_HANDLE].ref;
   const ibc_ref data = write->src[IBC_URB_WRITE_SRC_DATA].ref;
   assert(data.file == IBC_REG_FILE_LOGICAL);
   const uint32_t global_offset =
      ibc_ref_as_uint(write->src[IBC_URB_WRITE_SRC_GLOBAL_OFFSET].ref);
   const bool eot = ibc_ref_as_uint(write->src[IBC_URB_WRITE_SRC_EOT].ref);
   const bool per_slot_offset_present = false;
   const bool channel_mask_present = false;

   assert(write->src[IBC_URB_WRITE_SRC_DATA].num_comps <= 8);

   send->payload[0] = handle;
   send->payload[1] = data;
   send->mlen = 1 + per_slot_offset_present + channel_mask_present;
   send->ex_mlen = write->src[IBC_URB_WRITE_SRC_DATA].num_comps;

   send->sfid = BRW_SFID_URB;
   send->desc_imm =
      brw_urb_desc(b->shader->devinfo, GEN8_URB_OPCODE_SIMD8_WRITE,
                   per_slot_offset_present, channel_mask_present,
                   global_offset);

   send->has_header = true;
   send->has_side_effects = true;
   send->eot = eot;
}

const unsigned *
ibc_compile_vs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_vs_prog_key *key,
               struct brw_vs_prog_data *prog_data,
               struct nir_shader *nir,
               char **error_str)
{
   assert(nir->info.stage == MESA_SHADER_VERTEX);

   struct nir_vs_to_ibc_state vs_state = { 0, };

   struct nir_to_ibc_state nti;
   nir_to_ibc_state_init(&nti, MESA_SHADER_VERTEX, compiler->devinfo,
                         &key->base, &prog_data->base.base, &vs_state,
                         8, mem_ctx);

   nti.payload = &ibc_setup_vs_payload(&nti.b, prog_data, mem_ctx)->base;
   ibc_emit_nir_shader(&nti, nir);
   ibc_emit_urb_writes(&nti);

   ibc_shader *ibc = nir_to_ibc_state_finish(&nti);
   if (INTEL_DEBUG & DEBUG_VS) {
      ibc_print_shader(ibc, stderr);
      fprintf(stderr, "\n\n");
   }
   ibc_validate_shader(ibc);

   ibc_lower_and_optimize(ibc);

   prog_data->base.base.dispatch_grf_start_reg = nti.payload->num_ff_regs;
   prog_data->base.dispatch_mode = DISPATCH_MODE_SIMD8;

   return ibc_to_binary(ibc, mem_ctx, &prog_data->base.base.program_size);
}
