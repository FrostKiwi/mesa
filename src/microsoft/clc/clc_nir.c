/*
 * Copyright © Microsoft Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "nir.h"
#include "glsl_types.h"
#include "nir_types.h"
#include "nir_builder.h"

#include "clc_nir.h"
#include "clc_compiler.h"
#include "../compiler/dxil_nir.h"

static bool
lower_load_global_invocation_offset(nir_builder *b, nir_intrinsic_instr *intr,
                                    nir_variable *var)
{
   b->cursor = nir_after_instr(&intr->instr);

   nir_ssa_def *offset =
      build_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                          nir_imm_int(b,
                                      offsetof(struct clc_work_properties_data,
                                               global_offset_x)),
                          nir_dest_num_components(intr->dest),
                          nir_dest_bit_size(intr->dest));
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(offset));
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_load_work_dim(nir_builder *b, nir_intrinsic_instr *intr,
                    nir_variable *var)
{
   b->cursor = nir_after_instr(&intr->instr);

   nir_ssa_def *dim =
      build_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                          nir_imm_int(b,
                                      offsetof(struct clc_work_properties_data,
                                               work_dim)),
                          nir_dest_num_components(intr->dest),
                          nir_dest_bit_size(intr->dest));
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(dim));
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_load_local_group_size(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_after_instr(&intr->instr);

   nir_const_value v[3] = {
      nir_const_value_for_int(b->shader->info.cs.local_size[0], 32),
      nir_const_value_for_int(b->shader->info.cs.local_size[1], 32),
      nir_const_value_for_int(b->shader->info.cs.local_size[2], 32)
   };
   nir_ssa_def *size = nir_build_imm(b, 3, 32, v);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(size));
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_load_num_work_groups(nir_builder *b, nir_intrinsic_instr *intr,
                           nir_variable *var)
{
   b->cursor = nir_after_instr(&intr->instr);

   nir_ssa_def *count =
      build_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                         nir_imm_int(b,
                                     offsetof(struct clc_work_properties_data,
                                              group_count_total_x)),
                         nir_dest_num_components(intr->dest),
                         nir_dest_bit_size(intr->dest));
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(count));
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_load_work_group_offset(nir_builder *b, nir_intrinsic_instr *intr,
                             nir_variable *var)
{
   b->cursor = nir_after_instr(&intr->instr);

   nir_ssa_def *offset =
      build_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                         nir_imm_int(b,
                                     offsetof(struct clc_work_properties_data,
                                              group_id_offset_x)),
                         nir_dest_num_components(intr->dest),
                         nir_dest_bit_size(intr->dest));
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(offset));
   nir_instr_remove(&intr->instr);
   return true;
}

bool
clc_nir_lower_system_values(nir_shader *nir, nir_variable *var)
{
   bool progress = false;

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

            switch (intr->intrinsic) {
            case nir_intrinsic_load_global_invocation_offset:
               progress |= lower_load_global_invocation_offset(&b, intr, var);
               break;
            case nir_intrinsic_load_work_dim:
               progress |= lower_load_work_dim(&b, intr, var);
               break;
            case nir_intrinsic_load_local_group_size:
               lower_load_local_group_size(&b, intr);
               break;
            case nir_intrinsic_load_num_work_groups:
            case nir_intrinsic_load_num_total_work_groups:
               lower_load_num_work_groups(&b, intr, var);
               break;
            case nir_intrinsic_load_work_group_offset:
               lower_load_work_group_offset(&b, intr, var);
               break;
            default: break;
            }
         }
      }
   }

   return progress;
}
