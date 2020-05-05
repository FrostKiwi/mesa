/*
 * Copyright © 2020 Intel Corporation
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

struct ibc_tes_payload {
   struct ibc_payload_base base;

   ibc_ref tess_coord;
   ibc_ref urb_return_handles;

   unsigned push_input_start_reg;
};

struct nir_tes_to_ibc_state {
   struct {
      ibc_ref outputs[VARYING_SLOT_MAX];
   } out;
};

static struct ibc_tes_payload *
ibc_setup_tes_payload(ibc_builder *b,
                      const struct brw_vue_map *input_vue_map,
                      struct brw_tes_prog_data *prog_data,
                      void *mem_ctx)
{
   struct ibc_tes_payload *payload = rzalloc(mem_ctx, struct ibc_tes_payload);

   /* R0: thread header */
   ibc_setup_payload_base(b, &payload->base);

   unsigned reg = payload->base.num_ff_regs;

   /* R1-3: TessCoord.xyz */
   payload->tess_coord =
      ibc_load_payload_logical(b, &reg, IBC_TYPE_F, 3);

   /* R4: URB return handles */
   payload->urb_return_handles =
      ibc_load_payload_logical(b, &reg, IBC_TYPE_UD, 1);

   payload->base.num_ff_regs = reg;

   /* Set up push constants */
   ibc_setup_curb_payload(b, &payload->base, &prog_data->base.base);
   reg = payload->base.num_ff_regs + payload->base.num_curb_regs;

   payload->push_input_start_reg = reg;

   return payload;
}

bool
ibc_emit_nir_tes_intrinsic(struct nir_to_ibc_state *nti,
                           const nir_intrinsic_instr *instr)
{
   assert(nti->stage == MESA_SHADER_TESS_EVAL);
   struct ibc_tes_payload *payload = (struct ibc_tes_payload *)nti->payload;
   struct nir_tes_to_ibc_state *nti_tes = nti->stage_state;
   ibc_builder *b = &nti->b;

   ibc_ref dest = { .file = IBC_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_primitive_id: {
      /* g0.1<0,1,0>UD */
      ibc_ref g0_1 = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
      g0_1.hw_grf.byte += 1 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_hw_grf_mul_stride(&g0_1.hw_grf, 0);

      dest = ibc_MOV_scalar(b, IBC_TYPE_UD, g0_1);
      break;
   }

   case nir_intrinsic_load_tess_coord:
      dest = payload->tess_coord;
      break;

   case nir_intrinsic_load_input:
   case nir_intrinsic_load_per_vertex_input: {
      /* g0.0<0,1,0>UD */
      ibc_ref handle = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
      ibc_hw_grf_mul_stride(&handle.hw_grf, 0);

      unsigned first_component = nir_intrinsic_component(instr);
      unsigned global_offset = nir_intrinsic_base(instr);

      nir_src *io_offset = nir_get_io_offset_src(instr);
      ibc_ref per_slot_offset = ibc_nir_src(nti, *io_offset, IBC_TYPE_UD);

      /* The input load is convergent if-and-only-if the offset is. */
      bool divergent = nir_src_is_divergent(*io_offset);
      assert(nir_dest_is_divergent(instr->dest) == divergent);

      /* We can't read at an arbitrary component */
      unsigned read_components = instr->num_components + first_component;

      assert(read_components <= 4);

      if (!divergent)
         ibc_builder_push_scalar(b);

      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, read_components);

      ibc_emit_urb_read(b, dest, handle, per_slot_offset, global_offset,
                        read_components);

      dest.logical.comp = first_component;

      if (!divergent)
         ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_store_output: {
      ibc_ref src = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD);
      const unsigned varying = nir_intrinsic_base(instr) +
                               nir_src_as_uint(instr->src[1]);

      ibc_ref *output = &nti_tes->out.outputs[varying];

      if (output->file == IBC_FILE_NONE) {
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

   if (nir_intrinsic_infos[instr->intrinsic].has_dest) {
      ibc_builder_push_nir_dest_group(b, instr->dest);
      ibc_write_nir_dest(nti, &instr->dest, dest);
      ibc_builder_pop(b);
   } else {
      assert(dest.file == IBC_FILE_NONE);
   }

   return true;
}

/**
 * Lower URB_READ logical intrinsics for TES inputs to either reads of
 * pushed registers, or SEND intrinsics to pull those inputs.
 *
 * Returns the URB read length required for push inputs.
 */
static unsigned
ibc_lower_tes_inputs(ibc_shader *shader,
                     unsigned push_input_start_reg)
{
   ibc_builder b;
   ibc_builder_init(&b, shader);

   unsigned urb_read_length = 0;

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);

      if (intrin->op != IBC_INTRINSIC_OP_URB_READ)
         continue;

      const ibc_ref per_slot_offset =
         intrin->src[IBC_URB_READ_SRC_PER_SLOT_OFFSET].ref;
      const ibc_ref global_offset_ref =
         intrin->src[IBC_URB_READ_SRC_GLOBAL_OFFSET].ref;
      unsigned offset = ibc_ref_as_uint(global_offset_ref);

      /* Arbitrarily only push up to 32 vec4 slots worth of data,
       * which is 16 registers (since each holds 2 vec4 slots).
       */
      const unsigned push_limit = 32;

      if (offset < push_limit && ibc_ref_is_null_or_zero(per_slot_offset)) {
         /* Use push access; lower to a payload read. */
         unsigned reg = push_input_start_reg + offset / 2;

         ibc_builder_push_scalar(&b);

         b.cursor = ibc_after_start(shader);

         ibc_ref input =
            ibc_load_payload_logical(&b, &reg, intrin->dest.type, 8);

         b.cursor = ibc_before_instr(instr);

         ibc_ref srcs[4];
         for (unsigned i = 0; i < intrin->num_dest_comps; i++) {
            srcs[i] = ibc_comp_ref(input, 4 * (offset % 2) + i);
         }
         ibc_VEC_to(&b, intrin->dest, srcs, intrin->num_dest_comps);

         ibc_builder_pop(&b);

         ibc_instr_remove(&intrin->instr);

         urb_read_length = MAX2(urb_read_length, (offset / 2) + 1);
      } else {
         /* Use pull access; lower to a SEND instruction. */
         b.cursor = ibc_before_instr(instr);
         ibc_lower_io_urb_read_to_send(&b, intrin);
      }
   }

   return urb_read_length;
}

static void
ibc_emit_icl_tes_eot_workaround(ibc_builder *b, ibc_ref handle)
{
   const struct gen_device_info *devinfo = b->shader->devinfo;

   if (devinfo->gen != 11)
      return;

   /* ICL WA 1805992985:
    *
    * Icelake GPU hangs on one of the tessellation VK-CTS tests with the
    * DS unit not done.
    *
    * "The send cycle, which is a urb write with an eot must be 4 phases
    *  long and all 8 lanes must valid."
    *
    * We understand this to mean that all the channels need to be alive,
    * so first we replicate a live URB handle to all of the channels.
    */
   handle.logical.broadcast = true;

   /* Second trick is to use masked URB write where one can tell the HW to
    * actually write data only for selected channels even though all are
    * active.
    *
    * Third trick is to take advantage of the must-be-zero (MBZ) area in
    * the very beginning of the URB.
    *
    * We mask data to be written only for the first channel and use offset
    * zero explicitly to land data to the MBZ area avoiding trashing any
    * other part of the URB.
    *
    * Since the WA says that the write needs to be 4 phases long we use
    * 4 slots of data. All are explicitly zeros in order to to keep the
    * MBZ area written as zeros.
    */
   ibc_builder_push_we_all(b, 8);

   ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 8);
   send->can_reorder = false;
   send->has_side_effects = true;
   send->has_header = true;
   send->eot = true;
   send->sfid = BRW_SFID_URB;
   send->desc_imm =
      brw_urb_desc(devinfo, GEN8_URB_OPCODE_SIMD8_WRITE, false, true, 0);

   ibc_intrinsic_src msg_srcs[6] = {
      (ibc_intrinsic_src) { .ref = handle },
      (ibc_intrinsic_src) { .ref = ibc_imm_ud(0x10000u) },
      (ibc_intrinsic_src) { .ref = ibc_imm_ud(0) },
      (ibc_intrinsic_src) { .ref = ibc_imm_ud(0) },
      (ibc_intrinsic_src) { .ref = ibc_imm_ud(0) },
      (ibc_intrinsic_src) { .ref = ibc_imm_ud(0) },
   };
   unsigned mlen;
   ibc_ref msg = ibc_MESSAGE(b, msg_srcs, 6, &mlen);

   send->payload[0] = msg;
   send->mlen = mlen;

   ibc_builder_insert_instr(b, &send->instr);
   ibc_builder_pop(b);
}

const unsigned *
ibc_compile_tes(const struct brw_compiler *compiler, void *log_data,
                void *mem_ctx,
                const struct brw_tes_prog_key *key,
                const struct brw_vue_map *input_vue_map,
                struct brw_tes_prog_data *prog_data,
                struct nir_shader *nir,
                char **error_str_out)
{
   assert(nir->info.stage == MESA_SHADER_TESS_EVAL);

   struct nir_tes_to_ibc_state tes_state = { 0, };

   struct nir_to_ibc_state nti;
   nir_to_ibc_state_init(&nti, MESA_SHADER_TESS_EVAL, compiler->devinfo,
                         &key->base, &prog_data->base.base, &tes_state,
                         8, mem_ctx);

   nti.payload =
      &ibc_setup_tes_payload(&nti.b, input_vue_map, prog_data, mem_ctx)->base;
   struct ibc_tes_payload *payload = (struct ibc_tes_payload *)nti.payload;

   ibc_emit_nir_shader(&nti, nir);
   ibc_emit_urb_writes(&nti.b, &prog_data->base.vue_map,
                       payload->urb_return_handles, tes_state.out.outputs);
   ibc_emit_icl_tes_eot_workaround(&nti.b, payload->urb_return_handles);

   ibc_shader *ibc = nir_to_ibc_state_finish(&nti);
   if (INTEL_DEBUG & DEBUG_TES)
      ibc_print_shader(ibc, stderr);
   ibc_validate_shader(ibc);

   /* Optimize before lowering input reads so we see constant offsets */
   ibc_optimize(ibc);

   /* Lower inputs to either push register reads or pull messages */
   prog_data->base.urb_read_length =
      ibc_lower_tes_inputs(ibc, payload->push_input_start_reg);

   ibc_lower_and_optimize(ibc);

   bool assigned = ibc_assign_regs(ibc, compiler, true);
   if (!assigned) {
      *error_str_out = ralloc_strdup(mem_ctx,
                                     "Failed to allocate register");
      return NULL;
   }

   prog_data->base.base.dispatch_grf_start_reg = nti.payload->num_ff_regs;
   prog_data->base.dispatch_mode = DISPATCH_MODE_SIMD8;

   IBC_PASS_V(ibc, ibc_schedule_instructions_post_ra);
   return ibc_to_binary(ibc, &nir->info, compiler, log_data, mem_ctx,
                        &prog_data->base.base.program_size);
}
