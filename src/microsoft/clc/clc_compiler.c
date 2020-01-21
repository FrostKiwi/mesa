/*
 * Copyright 2019 Collabora Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "clc_compiler.h"
#include "clc_to_spirv.h"
#include "../compiler/nir_to_dxil.h"

#include "util/u_debug.h"
#include "nir/nir_builder.h"
#include "spirv/nir_spirv.h"

static const nir_shader_compiler_options
nir_options = {
   .lower_negate = true,
   .lower_inot = true,
   .lower_isign = true,
   .lower_iabs = true,
};

static void
optimize_nir(struct nir_shader *s)
{
   bool progress;
   do {
      progress = false;
      NIR_PASS_V(s, nir_lower_vars_to_ssa);
      NIR_PASS(progress, s, nir_lower_alu_to_scalar, NULL, NULL);
      NIR_PASS(progress, s, nir_copy_prop);
      NIR_PASS(progress, s, nir_opt_remove_phis);
      NIR_PASS(progress, s, nir_opt_dce);
      NIR_PASS(progress, s, nir_opt_dead_cf);
      NIR_PASS(progress, s, nir_opt_cse);
      NIR_PASS(progress, s, nir_opt_peephole_select, 8, true, true);
      NIR_PASS(progress, s, nir_opt_algebraic);
      NIR_PASS(progress, s, nir_opt_constant_folding);
      NIR_PASS(progress, s, nir_opt_undef);
      NIR_PASS(progress, s, nir_opt_deref);
      NIR_PASS_V(s, nir_lower_explicit_io, nir_var_shader_in | nir_var_mem_global, nir_address_format_32bit_global);
      NIR_PASS_V(s, nir_lower_system_values);
   } while (progress);

   do {
      progress = false;
      NIR_PASS(progress, s, nir_opt_algebraic_late);
   } while (progress);
}

/*
 * Transforming CLC into SPIR-V also uses the global memory model for all
 * access, with a hard requirement on pointers. We don't want that, partly
 * because it's unclear how to support pointers in DXIL, and partly because
 * it also requires more work on the runtime side to patch in the pointers
 * in GPU address space.
 *
 * For now, assume that all shader_in global mem accesses will instead be
 * bound sequentially as SSBOs.
 */
static void
lower_global_mem_to_ssbo(struct nir_shader *nir)
{
   /*
    * First, rewrite all our shader_in inputs (global memory model)
    * to be SSBOs instead.
    */
   foreach_list_typed_safe(nir_variable, in, node, &nir->inputs) {
      if (in->data.mode != nir_var_shader_in)
         continue;

      struct nir_variable *ssbo = rzalloc(nir, nir_variable);
      ssbo->data.mode = nir_var_mem_ssbo;
      ssbo->data.read_only = in->data.read_only;
      ssbo->data.location = in->data.location;
      ssbo->name = in->name ? ralloc_strdup(nir, in->name) : NULL;
      ssbo->type = in->type;

      nir_shader_add_variable(nir, ssbo);
      nir->num_shared++;
      exec_node_remove(&in->node);
      nir->num_inputs--;
   }

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      /*
       * Now, find all our function params: turn the load deref (pull the
       * pointer * base address) into an SSBO index which we stash in high
       * bits.
       */
      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
            if (intr->intrinsic != nir_intrinsic_load_kernel_input)
               continue;
            b.cursor = nir_before_instr(instr);

            nir_src *src = &intr->src[0]; /* param location i.e. SSBO slot */
            assert(src->is_ssa);

            nir_ssa_def *ssbo_loc = nir_ishl(&b, src->ssa, nir_imm_int(&b, 28));
            nir_ssa_def_rewrite_uses(&intr->dest.ssa,
                                     nir_src_for_ssa(ssbo_loc));
            nir_instr_remove(instr);
         }
      }

      /*
       * Find the corresponding store_global instructions and replace them
       * with SSBO accesses. This requires widening scalar sources to vec4,
       * and reconstructing the SSBO location + index from the 'pointer'
       * constructed for us by NIR's IO lowering.
       */
      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
            if (intr->intrinsic != nir_intrinsic_store_global)
               continue;
            b.cursor = nir_before_instr(instr);

            assert(intr->src[0].is_ssa);
            nir_ssa_def *val = intr->src[0].ssa; /* value */
            nir_ssa_def *vec4_val = nir_vec4(&b, val, val, val, val);

            assert(intr->src[1].is_ssa);
            nir_ssa_def *ptr = intr->src[1].ssa; /* source 'pointer' */
#if 0
            /*
            * Here's where we would ordinarily do this, and reclaim our SSBO
            * index from the high bits of the pointer:
            *  nir_ssa_def *ssbo_loc = nir_ishr(&b, ptr, nir_imm_int(&b, 28));
            *
            * However, since we don't implement runtime lookup of SSBO
            * binding index -> UAV handle yet
            * (cf. nir_to_dxil.c:emit_store_ssbo), and NIR isn't quite smart
            * enough to see that: ((x & 0x0fffffff) << 28) >> 28 == 0,
            * we ... can't actually do this.
            *
            * So for just now, you get one SSBO, and all of the above is an
            * elaborate exercise in misdirection.
            *
            * Fixing this would likely involve converting our typed UAVs into
            * raw, and pushing the handle value into a runtime array; doing
            * this would bring us closer to the pointer model as we could at
            * least deal in byte rather than element offsets into the
            * RWBuffer.
            */
            nir_ssa_def *ssbo_loc = nir_ishr(&b, ptr, nir_imm_int(&b, 28));
#else
            nir_ssa_def *ssbo_loc = nir_imm_zero(&b, 1, 32);
#endif
            nir_ssa_def *ssbo_idx = nir_iand(&b, ptr,
                                             nir_imm_int(&b, 0x00ffffff));
            ssbo_idx = nir_ishr(&b, ssbo_idx,
                                nir_imm_int(&b, val->bit_size / 16));

            nir_intrinsic_instr *store =
               nir_intrinsic_instr_create(b.shader, nir_intrinsic_store_ssbo);
            store->num_components = 4;
            nir_intrinsic_set_write_mask(store, 0xf);
            nir_intrinsic_set_align(store, 4, 0);
            store->src[0] = nir_src_for_ssa(vec4_val);
            store->src[1] = nir_src_for_ssa(ssbo_loc);
            store->src[2] = nir_src_for_ssa(ssbo_idx);
            nir_builder_instr_insert(&b, &store->instr);
            nir_instr_remove(instr);
         }
      }
   }
}

int clc_compile_from_source(
   const char *source,
   const char *source_name,
   const struct clc_define defines[], // should be sorted by name
   size_t num_defines,
   const struct clc_header headers[], // should be sorted by name
   size_t num_headers,
   clc_msg_callback warning_callback,
   clc_msg_callback error_callback,
   struct clc_metadata *metadata,
   void **blob,
   size_t *blob_size)
{
   uint32_t *spv_src;
   size_t spv_size;
   const char *err_log;
   struct nir_shader *nir;
   int ret;


   const struct spirv_to_nir_options spirv_options = {
      .environment = NIR_SPIRV_OPENCL,
      .constant_as_global = true,
      .caps = {
         .address = true,
         .float64 = true,
         .int8 = true,
         .int16 = true,
         .int64 = true,
         .kernel = true,
      },
   };

   ret = clc_to_spirv(source, source_name,
                      defines, num_defines,
                      headers, num_headers,
                     /* TODO: callbacks ... */
                     &spv_src, &spv_size,
                     &err_log);

   if (ret < 0) {
      debug_printf("D3D12: clc_to_spirv failed: %s\n", err_log);
      return -1;
   } else {
      debug_printf("D3D12: clc_to_spirv succeeded: %s\n", err_log);
   }

   nir = spirv_to_nir(spv_src, spv_size / 4,
                      NULL, 0,
                      MESA_SHADER_KERNEL, "main_test",
                      &spirv_options, &nir_options, false);
   if (!nir) {
      fprintf(stderr, "D3D12: spirv_to_nir failed\n");
      return -1;
   }

   NIR_PASS_V(nir, nir_lower_variable_initializers, nir_var_function_temp);
   NIR_PASS_V(nir, nir_lower_returns);
   NIR_PASS_V(nir, nir_inline_functions);
   NIR_PASS_V(nir, nir_opt_deref);
   foreach_list_typed_safe(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         exec_node_remove(&func->node);
   }
   assert(exec_list_length(&nir->functions) == 1);
   NIR_PASS_V(nir, nir_lower_variable_initializers, ~nir_var_function_temp);

   NIR_PASS_V(nir, nir_lower_system_values);

   optimize_nir(nir);

   /*
    * See comments on these two passes: rewrite global memory-model usage
    * with pointers into SSBO access. We re-run constant folding and undef
    * removal as these two passes both generate extra constants.
    */
   NIR_PASS_V(nir, lower_global_mem_to_ssbo);
   NIR_PASS_V(nir, nir_lower_variable_initializers, nir_var_all);
   optimize_nir(nir);

   nir_print_shader(nir, stderr);

   struct blob tmp;
   if (!nir_to_dxil(nir, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return -1;
   }

   blob_finish_get_buffer(&tmp, blob, blob_size);
   return 0;
}
