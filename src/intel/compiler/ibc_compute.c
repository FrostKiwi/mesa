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

struct ibc_cs_payload {
   struct ibc_payload_base base;

   ibc_ref subgroup_id;
};

static struct ibc_cs_payload *
ibc_setup_cs_payload(ibc_builder *b, unsigned subgroup_id_offset,
                     struct brw_cs_prog_data *prog_data, void *mem_ctx)
{
   struct ibc_cs_payload *payload = rzalloc(mem_ctx, struct ibc_cs_payload);
   ibc_setup_payload_base(b, &payload->base);
   ibc_setup_curb_payload(b, &payload->base, &prog_data->base);

   payload->subgroup_id = payload->base.push;
   payload->subgroup_id.type = IBC_TYPE_UD;
   payload->subgroup_id.hw_grf.byte += subgroup_id_offset;
   ibc_hw_grf_mul_stride(&payload->subgroup_id.hw_grf, 0);

   return payload;
}

bool
ibc_emit_nir_cs_intrinsic(struct nir_to_ibc_state *nti,
                          const nir_intrinsic_instr *instr)
{
   assert(nti->stage == MESA_SHADER_COMPUTE);
   struct brw_cs_prog_data *prog_data = (void *)nti->prog_data;
   struct ibc_cs_payload *payload = (struct ibc_cs_payload *)nti->payload;
   ibc_builder *b = &nti->b;

   ibc_ref dest = { .file = IBC_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_subgroup_id:
      dest = ibc_MOV(b, IBC_TYPE_UD, payload->subgroup_id);
      break;

   case nir_intrinsic_load_work_group_id: {
      ibc_ref group_id[3];

      group_id[0] = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
      group_id[0].hw_grf.byte += 1 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_hw_grf_mul_stride(&group_id[0].hw_grf, 0);

      group_id[1] = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
      group_id[1].hw_grf.byte += 6 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_hw_grf_mul_stride(&group_id[1].hw_grf, 0);

      group_id[2] = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
      group_id[2].hw_grf.byte += 7 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_hw_grf_mul_stride(&group_id[2].hw_grf, 0);

      ibc_builder_push_scalar(b);
      dest = ibc_VEC(b, group_id, 3);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_barrier: {
      prog_data->uses_barrier = true;

      uint32_t barrier_id_mask;
      switch (b->shader->devinfo->gen) {
      case 7:
      case 8:
         barrier_id_mask = 0x0f000000u; break;
      case 9:
      case 10:
         barrier_id_mask = 0x8f000000u; break;
      case 11:
         barrier_id_mask = 0x7f000000u; break;
      default:
         unreachable("barrier is only available on gen >= 7");
      }

      ibc_reg *tmp_reg = ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);
      ibc_ref tmp = ibc_typed_ref(tmp_reg, IBC_TYPE_UD);

      ibc_builder_push_we_all(b, 8);
      ibc_MOV_to(b, tmp, ibc_imm_ud(0));
      ibc_builder_pop(b);

      ibc_ref tmp_2 = tmp;
      tmp_2.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_ref g0_2 = ibc_typed_ref(b->shader->g0, IBC_TYPE_UD);
      g0_2.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
      ibc_builder_push_scalar(b);
      ibc_build_alu2(b, IBC_ALU_OP_AND, tmp_2, g0_2,
                     ibc_imm_ud(barrier_id_mask));
      ibc_builder_pop(b);

      ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 1);
      send->instr.we_all = true;
      send->sfid = BRW_SFID_MESSAGE_GATEWAY;
      send->payload[0] = tmp;
      send->mlen = 1;
      send->has_header = true;
      send->can_reorder = false;
      send->has_side_effects = true;
      send->desc_imm = BRW_MESSAGE_GATEWAY_SFID_BARRIER_MSG;
      ibc_builder_insert_instr(b, &send->instr);

      ibc_WAIT(b);
      break;
   }

   case nir_intrinsic_load_num_work_groups: {
      /* This is fixed as part of the shader interface */
      const unsigned num_work_groups_bti = 0;

      prog_data->uses_num_work_groups = true;

      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {
         [IBC_SURFACE_SRC_SURFACE_BTI] = {
            .ref = ibc_imm_ud(num_work_groups_bti),
            .num_comps = 1,
         },
         [IBC_SURFACE_SRC_ADDRESS] = {
            .ref = ibc_imm_ud(0),
            .num_comps = 1,
         },
      };

      ibc_builder_push_scalar(b);
      dest = ibc_build_ssa_intrinsic(b, IBC_INTRINSIC_OP_BTI_UNTYPED_READ,
                                     IBC_TYPE_UD, 3,
                                     srcs, IBC_SURFACE_NUM_SRCS);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_load_shared: {
      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {
         [IBC_SURFACE_SRC_SURFACE_BTI] = {
            .ref = ibc_imm_ud(GEN7_BTI_SLM),
            .num_comps = 1,
         },
         [IBC_SURFACE_SRC_ADDRESS] = {
            .ref = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD),
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
      break;
   }

   case nir_intrinsic_store_shared: {
      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {
         [IBC_SURFACE_SRC_SURFACE_BTI] = {
            .ref = ibc_imm_ud(GEN7_BTI_SLM),
            .num_comps = 1,
         },
         [IBC_SURFACE_SRC_ADDRESS] = {
            .ref = ibc_nir_src(nti, instr->src[1], IBC_TYPE_UD),
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

      store->can_reorder = false,
      store->has_side_effects = true;
      break;
   }

   case nir_intrinsic_shared_atomic_add:
   case nir_intrinsic_shared_atomic_imin:
   case nir_intrinsic_shared_atomic_umin:
   case nir_intrinsic_shared_atomic_imax:
   case nir_intrinsic_shared_atomic_umax:
   case nir_intrinsic_shared_atomic_and:
   case nir_intrinsic_shared_atomic_or:
   case nir_intrinsic_shared_atomic_xor:
   case nir_intrinsic_shared_atomic_exchange:
   case nir_intrinsic_shared_atomic_comp_swap: {
      unsigned aop = brw_aop_for_nir_intrinsic(instr);

      /* Set up the BTI or handle */
      ibc_intrinsic_src srcs[IBC_SURFACE_NUM_SRCS] = {
         [IBC_SURFACE_SRC_SURFACE_BTI] = (ibc_intrinsic_src) {
            .ref = ibc_imm_ud(GEN7_BTI_SLM),
         },
         [IBC_SURFACE_SRC_ADDRESS] = (ibc_intrinsic_src) {
            .ref = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD),
         },
         [IBC_SURFACE_SRC_ATOMIC_OP] = (ibc_intrinsic_src) {
            .ref = ibc_imm_ud(aop),
         },
      };

      /* If we're not an atomic aop == -1 so the last two conditions are true
       * vacuously.
       */
      if (aop != BRW_AOP_INC && aop != BRW_AOP_DEC) {
         srcs[IBC_SURFACE_SRC_DATA0] = (ibc_intrinsic_src) {
            .ref = ibc_nir_src(nti, instr->src[1], IBC_TYPE_UD),
         };
      }

      if (aop == BRW_AOP_CMPWR) {
         srcs[IBC_SURFACE_SRC_DATA1] = (ibc_intrinsic_src) {
            .ref = ibc_nir_src(nti, instr->src[2], IBC_TYPE_UD),
         };
      }

      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, 1);
      ibc_intrinsic_instr *image_op =
         ibc_build_intrinsic(b, IBC_INTRINSIC_OP_BTI_UNTYPED_ATOMIC,
                             dest, -1, 1, srcs, IBC_SURFACE_NUM_SRCS);
      image_op->can_reorder = false;
      image_op->has_side_effects = true;
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

static void
ibc_emit_cs_thread_terminate(struct nir_to_ibc_state *nti)
{
   ibc_builder *b = &nti->b;

   ibc_reg *tmp_reg = ibc_hw_grf_reg_create(b->shader, 32, 32);
   ibc_ref tmp_ud = ibc_typed_ref(tmp_reg, IBC_TYPE_UD);

   ibc_builder_push_we_all(b, 8);
   ibc_MOV_to(b, tmp_ud, ibc_typed_ref(b->shader->g0, IBC_TYPE_UD));
   ibc_builder_pop(b);

   ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 8);
   send->instr.we_all = true;
   send->sfid = BRW_SFID_THREAD_SPAWNER;
   send->desc_imm = brw_ts_eot_desc(b->shader->devinfo);
   send->can_reorder = false;
   send->has_side_effects = true;
   send->eot = true;

   send->payload[0] = tmp_ud;
   send->mlen = 1;

   ibc_builder_insert_instr(b, &send->instr);
}

static int
get_subgroup_id_param_index(const struct brw_stage_prog_data *prog_data)
{
   if (prog_data->nr_params == 0)
      return -1;

   /* The local thread id is always the last parameter in the list */
   uint32_t last_param = prog_data->param[prog_data->nr_params - 1];
   if (last_param == BRW_PARAM_BUILTIN_SUBGROUP_ID)
      return prog_data->nr_params - 1;

   return -1;
}

static void
fill_push_const_block_info(struct brw_push_const_block *block, unsigned dwords)
{
   block->dwords = dwords;
   block->regs = DIV_ROUND_UP(dwords, 8);
   block->size = block->regs * 32;
}

static void
cs_fill_push_const_info(const struct gen_device_info *devinfo,
                        struct brw_cs_prog_data *cs_prog_data)
{
   const struct brw_stage_prog_data *prog_data = &cs_prog_data->base;
   int subgroup_id_index = get_subgroup_id_param_index(prog_data);
   bool cross_thread_supported = devinfo->gen > 7 || devinfo->is_haswell;

   /* The thread ID should be stored in the last param dword */
   assert(subgroup_id_index == -1 ||
          subgroup_id_index == (int)prog_data->nr_params - 1);

   unsigned cross_thread_dwords, per_thread_dwords;
   if (!cross_thread_supported) {
      cross_thread_dwords = 0u;
      per_thread_dwords = prog_data->nr_params;
   } else if (subgroup_id_index >= 0) {
      /* Fill all but the last register with cross-thread payload */
      cross_thread_dwords = 8 * (subgroup_id_index / 8);
      per_thread_dwords = prog_data->nr_params - cross_thread_dwords;
      assert(per_thread_dwords > 0 && per_thread_dwords <= 8);
   } else {
      /* Fill all data using cross-thread payload */
      cross_thread_dwords = prog_data->nr_params;
      per_thread_dwords = 0u;
   }

   fill_push_const_block_info(&cs_prog_data->push.cross_thread, cross_thread_dwords);
   fill_push_const_block_info(&cs_prog_data->push.per_thread, per_thread_dwords);

   unsigned total_dwords =
      (cs_prog_data->push.per_thread.size * cs_prog_data->threads +
       cs_prog_data->push.cross_thread.size) / 4;
   fill_push_const_block_info(&cs_prog_data->push.total, total_dwords);

   assert(cs_prog_data->push.cross_thread.dwords % 8 == 0 ||
          cs_prog_data->push.per_thread.size == 0);
   assert(cs_prog_data->push.cross_thread.dwords +
          cs_prog_data->push.per_thread.dwords ==
             prog_data->nr_params);
}

static void
cs_set_simd_size(struct brw_cs_prog_data *cs_prog_data, unsigned size)
{
   cs_prog_data->simd_size = size;
   unsigned group_size = cs_prog_data->local_size[0] *
      cs_prog_data->local_size[1] * cs_prog_data->local_size[2];
   cs_prog_data->threads = (group_size + size - 1) / size;
}

static nir_shader *
compile_cs_to_nir(const struct brw_compiler *compiler,
                  void *mem_ctx,
                  const struct brw_cs_prog_key *key,
                  const nir_shader *src_shader,
                  unsigned dispatch_width)
{
   nir_shader *shader = nir_shader_clone(mem_ctx, src_shader);
   brw_nir_apply_key(shader, compiler, &key->base, dispatch_width, true);
   brw_nir_lower_cs_intrinsics(shader, dispatch_width);
   brw_postprocess_nir(shader, compiler, true);
   return shader;
}

const unsigned *
ibc_compile_cs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_cs_prog_key *key,
               struct brw_cs_prog_data *prog_data,
               const struct nir_shader *src_shader,
               int shader_time_index,
               char **error_str_out)
{
   assert(src_shader->info.stage == MESA_SHADER_COMPUTE);
   prog_data->local_size[0] = src_shader->info.cs.local_size[0];
   prog_data->local_size[1] = src_shader->info.cs.local_size[1];
   prog_data->local_size[2] = src_shader->info.cs.local_size[2];
   unsigned local_workgroup_size =
      src_shader->info.cs.local_size[0] * src_shader->info.cs.local_size[1] *
      src_shader->info.cs.local_size[2];

   unsigned min_simd_width =
      DIV_ROUND_UP(local_workgroup_size, compiler->devinfo->max_cs_threads);
   min_simd_width = MAX2(8, min_simd_width);
   min_simd_width = util_next_power_of_two(min_simd_width);
   assert(min_simd_width <= 32);
   unsigned max_simd_width = 32;

   if (key->base.subgroup_size_type >= BRW_SUBGROUP_SIZE_REQUIRE_8) {
      /* These enum values are expressly chosen to be equal to the subgroup
       * size that they require.
       */
      const unsigned required_simd_width = key->base.subgroup_size_type;
      assert(required_simd_width == 8 ||
             required_simd_width == 16 ||
             required_simd_width == 32);
      if (required_simd_width < min_simd_width ||
          required_simd_width > max_simd_width) {
         if (error_str_out)
            *error_str_out = "Cannot satisfy explicit subgroup size";
         return NULL;
      } else {
         min_simd_width = max_simd_width = required_simd_width;
      }
   }

   /* Add a uniform for the thread local id.  It must be the last uniform
    * on the list.
    */
   assert(src_shader->num_uniforms == prog_data->base.nr_params * 4);
   const unsigned subgroup_id_offset = src_shader->num_uniforms;
   uint32_t *param = brw_stage_prog_data_add_params(&prog_data->base, 1);
   *param = BRW_PARAM_BUILTIN_SUBGROUP_ID;

   struct {
      bool enabled;
      const unsigned *data;
      unsigned size;
      unsigned num_ff_regs;
   } bin[3] = {
      { .enabled = !(INTEL_DEBUG & DEBUG_NO8), },
      { .enabled = !(INTEL_DEBUG & DEBUG_NO16), },
//      { .enabled = (INTEL_DEBUG & DEBUG_DO32) || min_simd_width == 32, },
      { .enabled = true, },
   };

   bool first_bin = true;
   for (unsigned i = 0; i < 3; i++) {
      const unsigned bin_simd_width = 8 << i;
      if (bin_simd_width < min_simd_width)
         continue;

      if (bin_simd_width > max_simd_width)
         break;

      if (!bin[i].enabled)
         continue;

      /* Per-binary context */
      void *bin_ctx = ralloc_context(mem_ctx);

      nir_shader *shader =
         compile_cs_to_nir(compiler, mem_ctx, key, src_shader, bin_simd_width);

      struct nir_to_ibc_state nti;
      nir_to_ibc_state_init(&nti, shader->info.stage, compiler->devinfo,
                            &key->base, &prog_data->base,
                            NULL, bin_simd_width, mem_ctx);

      nti.payload = &ibc_setup_cs_payload(&nti.b, subgroup_id_offset,
                                          prog_data, mem_ctx)->base;
      ibc_emit_nir_shader(&nti, shader);
      ibc_emit_cs_thread_terminate(&nti);

      ibc_shader *ibc = nir_to_ibc_state_finish(&nti);
      if (INTEL_DEBUG & DEBUG_CS)
         ibc_print_shader(ibc, stderr);
      ibc_validate_shader(ibc);

      ibc_lower_and_optimize(ibc);

      bool assigned = ibc_assign_regs(ibc, compiler, first_bin);

      if (INTEL_DEBUG & DEBUG_CS)
         ibc_print_shader(ibc, stderr);

      if (assigned) {
         IBC_PASS_V(ibc, ibc_schedule_instructions_post_ra);
         bin[i].data = ibc_to_binary(ibc, &shader->info, compiler, log_data,
                                     mem_ctx, &bin[i].size);
         bin[i].num_ff_regs = nti.payload->num_ff_regs;
      }

      ralloc_free(bin_ctx);

      if (!assigned) {
         if (first_bin) {
            *error_str_out = ralloc_strdup(mem_ctx,
                                           "Failed to allocate register");
            return NULL;
         }
         compiler->shader_perf_log(log_data,
                                   "SIMD%u shader failed to compile: "
                                   "Failed to allocate registers",
                                   bin_simd_width);
         /* We assume that if one shader fails to allocate, all wider shaders
          * will also fail.
          */
         break;
      }

      first_bin = false;
   }

   for (int i = 2; i >= 0; i--) {
      const unsigned bin_simd_width = 8 << i;
      if (bin[i].data == NULL)
         continue;

      /* Grab the widest shader we find */
      cs_set_simd_size(prog_data, bin_simd_width);
      cs_fill_push_const_info(compiler->devinfo, prog_data);
      prog_data->base.dispatch_grf_start_reg = bin[i].num_ff_regs;
      prog_data->base.program_size = bin[i].size;

      return bin[i].data;
   }

   return NULL;
}
