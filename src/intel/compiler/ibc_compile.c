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

#include "ibc.h"
#include "ibc_compile.h"

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
               char **error_str)
{
   prog_data->local_size[0] = src_shader->info.cs.local_size[0];
   prog_data->local_size[1] = src_shader->info.cs.local_size[1];
   prog_data->local_size[2] = src_shader->info.cs.local_size[2];
   unsigned local_workgroup_size =
      src_shader->info.cs.local_size[0] * src_shader->info.cs.local_size[1] *
      src_shader->info.cs.local_size[2];

   unsigned min_dispatch_width =
      DIV_ROUND_UP(local_workgroup_size, compiler->devinfo->max_cs_threads);
   min_dispatch_width = MAX2(8, min_dispatch_width);
   min_dispatch_width = util_next_power_of_two(min_dispatch_width);
   assert(min_dispatch_width <= 32);

   /* Add a uniform for the thread local id.  It must be the last uniform
    * on the list.
    */
   uint32_t *param = brw_stage_prog_data_add_params(&prog_data->base, 1);
   *param = BRW_PARAM_BUILTIN_SUBGROUP_ID;

   const unsigned simd_width = 32;

   nir_shader *shader =
      compile_cs_to_nir(compiler, mem_ctx, key, src_shader, simd_width);

   ibc_shader *ibc = nir_to_ibc(shader, mem_ctx, simd_width,
                                compiler->devinfo);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   ibc_lower_simd_width(ibc);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   ibc_opt_copy_prop(ibc);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   ibc_opt_dead_code(ibc);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   ibc_lower_surface_access(ibc);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   ibc_assign_and_lower_flags(ibc);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   ibc_assign_regs(ibc);
   ibc_validate_shader(ibc);
   ibc_print_shader(ibc, stderr);
   fprintf(stderr, "\n\n");

   cs_set_simd_size(prog_data, simd_width);
   cs_fill_push_const_info(compiler->devinfo, prog_data);

   return ibc_to_binary(ibc, mem_ctx, &prog_data->base.program_size);
}
