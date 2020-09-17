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
      ibc_load_payload_logical(b, &reg, IBC_TYPE_UD, 1, false);

   payload->base.num_ff_regs = reg;

   /* Set up push constants */
   ibc_setup_curb_payload(b, &payload->base, &prog_data->base.base);
   reg = payload->base.num_ff_regs + payload->base.num_curb_regs;

   assert(prog_data->base.urb_read_length <= 15);

   /* We represent per-vertex attributes in the payload as a SIMD8 vec4 for
    * each component coming in from the vertex fetcher.
    *
    * TODO: VF component packing?
    */
   for (unsigned i = 0; i < prog_data->nr_attribute_slots; i++) {
      payload->inputs[i] =
         ibc_load_payload_logical(b, &reg, IBC_TYPE_UD, 4, false);
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

   ibc_ref dest = { .file = IBC_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_input: {
      assert(nir_dest_is_divergent(instr->dest));
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

const unsigned *
ibc_compile_vs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_vs_prog_key *key,
               struct brw_vs_prog_data *prog_data,
               struct nir_shader *nir,
               struct brw_compile_stats *stats,
               char **error_str_out)
{
   assert(nir->info.stage == MESA_SHADER_VERTEX);

   struct nir_vs_to_ibc_state vs_state = { 0, };

   struct nir_to_ibc_state nti;
   nir_to_ibc_state_init(&nti, MESA_SHADER_VERTEX, compiler->devinfo,
                         &key->base, &prog_data->base.base, &vs_state,
                         8, mem_ctx);

   nti.payload = &ibc_setup_vs_payload(&nti.b, prog_data, mem_ctx)->base;
   struct ibc_vs_payload *payload = (struct ibc_vs_payload *)nti.payload;

   ibc_emit_nir_shader(&nti, nir);
   ibc_emit_urb_writes(&nti.b, &prog_data->base.vue_map,
                       payload->urb_return_handles, vs_state.out.outputs);

   ibc_shader *ibc = nir_to_ibc_state_finish(&nti);
   if (INTEL_DEBUG & DEBUG_VS)
      ibc_print_shader(ibc, stderr);
   ibc_validate_shader(ibc);

   ibc_lower_and_optimize(ibc);

   bool assigned = ibc_assign_regs(ibc, compiler, true);
   if (!assigned) {
      *error_str_out = ralloc_strdup(mem_ctx,
                                     "Failed to allocate register");
      return NULL;
   }

   if (ibc->scratch_B > 0) {
      prog_data->base.base.total_scratch =
         brw_get_scratch_size(ibc->scratch_B);
   }

   prog_data->base.base.dispatch_grf_start_reg = nti.payload->num_ff_regs;
   prog_data->base.dispatch_mode = DISPATCH_MODE_SIMD8;

   IBC_PASS_V(ibc, ibc_schedule_instructions_post_ra);

   struct ibc_eu_performance *perf = ibc_estimate_performance(ibc);
   ibc->cycles = perf->latency;
   ralloc_free(perf);

   const unsigned *assembly =
      ibc_to_binary(ibc, &nir->info, compiler, log_data, mem_ctx,
                    &prog_data->base.base.program_size);

   if (stats)
      stats[0] = ibc->stats;

   return assembly;
}
