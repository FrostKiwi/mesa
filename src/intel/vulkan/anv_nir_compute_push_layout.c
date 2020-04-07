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

#include "anv_nir.h"
#include "nir_builder.h"
#include "compiler/brw_nir.h"
#include "compiler/brw_nir_ubo_gather.h"
#include "util/mesa-sha1.h"

#include <stdlib.h>

static unsigned
anv_ubo_gathers_as_ubo_ranges(unsigned gather_count,
                              struct brw_ubo_gather *gathers,
                              struct anv_pipeline_bind_map *map,
                              unsigned max_push_size,
                              unsigned max_ubo_ranges,
                              struct anv_push_range *ranges_out)
{
   if (gather_count == 0)
      return 0;

   if (max_ubo_ranges == 0)
      return 0;

   struct anv_push_range ranges[4];
   memset(ranges, 0, sizeof(ranges));
   assert(max_ubo_ranges <= ARRAY_SIZE(ranges));

   unsigned num_dw = 0;
   uint8_t block = UINT8_MAX;
   int r = -1;
   for (unsigned i = 0; i < gather_count; i++) {
      if (r < 0 || block != gathers[i].block) {
         r++;
         if (r >= max_ubo_ranges)
            return 0;

         block = gathers[i].block;
         const struct anv_pipeline_binding *binding =
            &map->surface_to_descriptor[block];

         unsigned range_start = gathers[i].start / 32;
         if (range_start > UINT8_MAX)
            return 0;

         ranges[r] = (struct anv_push_range) {
            .set = binding->set,
            .index = binding->index,
            .dynamic_offset_index = binding->dynamic_offset_index,
            .start = range_start,
            .length = 0,
         };
      }

      uint32_t start = gathers[i].start;
      uint32_t end = gathers[i].start + util_last_bit(gathers[i].dwords) * 4;

      /* Things are supposed to be sorted in ascending order */
      assert(ranges[r].start * 32 <= start);

      unsigned range_length = DIV_ROUND_UP(end, 32) - ranges[r].start;
      if (range_length > UINT8_MAX)
         return 0;
      assert(ranges[r].length <= range_length);
      ranges[r].length = range_length;

      num_dw += util_bitcount(gathers[i].dwords);
   }
   const unsigned num_ranges = r + 1;
   assert(num_ranges <= max_ubo_ranges);

   unsigned push_size = 0;
   for (unsigned r = 0; r < num_ranges; r++)
      push_size += ranges[r].length * 32;

   if (push_size > max_push_size)
      return 0;

   /* Only push if doing so either only wastes at most 2 registers and our
    * push ranges are at least 75% full.
    */
   assert(num_dw * 4 <= push_size);
   if (push_size > num_dw * 4 + 64 &&
       push_size > num_dw * 4 * 4 / 3)
      return 0;

   typed_memcpy(ranges_out, ranges, num_ranges);
   return num_ranges;
}

void
anv_nir_compute_push_layout(const struct anv_physical_device *pdevice,
                            bool robust_buffer_access,
                            nir_shader *nir,
                            struct brw_stage_prog_data *prog_data,
                            struct anv_pipeline_bind_map *map,
                            void *mem_ctx)
{
   const struct brw_compiler *compiler = pdevice->compiler;
   memset(map->push_ranges, 0, sizeof(map->push_ranges));

   bool has_const_ubo = false;
   unsigned push_start = UINT_MAX, push_end = 0;
   nir_foreach_function(function, nir) {
      if (!function->impl)
         continue;

      nir_foreach_block(block, function->impl) {
         nir_foreach_instr(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;

            nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
            switch (intrin->intrinsic) {
            case nir_intrinsic_load_ubo:
               if (nir_src_is_const(intrin->src[0]) &&
                   nir_src_is_const(intrin->src[1]))
                  has_const_ubo = true;
               break;

            case nir_intrinsic_load_push_constant: {
               unsigned base = nir_intrinsic_base(intrin);
               unsigned range = nir_intrinsic_range(intrin);
               push_start = MIN2(push_start, base);
               push_end = MAX2(push_end, base + range);
               break;
            }

            default:
               break;
            }
         }
      }
   }

   const bool has_push_intrinsic = push_start <= push_end;

   const unsigned max_num_push_ranges =
      ((pdevice->info.gen <= 7 && !pdevice->info.is_haswell) ||
       nir->info.stage != MESA_SHADER_COMPUTE) ? 1 :
      4 - compiler->constant_buffer_0_is_relative;

   const bool use_gather = has_const_ubo && pdevice->use_softpin &&
                           nir->info.stage != MESA_SHADER_COMPUTE;

   const bool push_ubo_ranges = !use_gather &&
      (pdevice->info.gen >= 8 || pdevice->info.is_haswell) &&
      has_const_ubo && nir->info.stage != MESA_SHADER_COMPUTE;

   if ((use_gather || push_ubo_ranges) && robust_buffer_access) {
      /* We can't on-the-fly adjust our push ranges because doing so would
       * mess up the layout in the shader.  When robustBufferAccess is
       * enabled, we push a mask into the shader indicating which pushed
       * registers are valid and we zero out the invalid ones at the top of
       * the shader.
       */
      const uint32_t push_reg_mask_start =
         offsetof(struct anv_push_constants, push_reg_mask);
      const uint32_t push_reg_mask_end = push_reg_mask_start + sizeof(uint64_t);
      push_start = MIN2(push_start, push_reg_mask_start);
      push_end = MAX2(push_end, push_reg_mask_end);
   }

   if (nir->info.stage == MESA_SHADER_COMPUTE) {
      /* For compute shaders, we always have to have the subgroup ID.  The
       * back-end compiler will "helpfully" add it for us in the last push
       * constant slot.  Yes, there is an off-by-one error here but that's
       * because the back-end will add it so we want to claim the number of
       * push constants one dword less than the full amount including
       * gl_SubgroupId.
       */
      assert(push_end <= offsetof(struct anv_push_constants, cs.subgroup_id));
      push_end = offsetof(struct anv_push_constants, cs.subgroup_id);
   }

   /* Align push_start down to a 32B boundary and make it no larger than
    * push_end (no push constants is indicated by push_start = UINT_MAX).
    */
   push_start = MIN2(push_start, push_end);
   push_start = align_down_u32(push_start, 32);

   /* For vec4 our push data size needs to be aligned to a vec4 and for
    * scalar, it needs to be aligned to a DWORD.
    */
   const unsigned align = compiler->scalar_stage[nir->info.stage] ? 4 : 16;
   nir->num_uniforms = ALIGN(push_end - push_start, align);
   prog_data->nr_params = nir->num_uniforms / 4;
   prog_data->param = rzalloc_array(mem_ctx, uint32_t, prog_data->nr_params);

   struct anv_push_range push_constant_range = {
      .set = ANV_DESCRIPTOR_SET_PUSH_CONSTANTS,
      .start = push_start / 32,
      .length = DIV_ROUND_UP(push_end - push_start, 32),
   };

   unsigned num_push_ranges = 0;
   if (push_constant_range.length > 0)
      map->push_ranges[num_push_ranges++] = push_constant_range;

   if (has_push_intrinsic) {
      nir_foreach_function(function, nir) {
         if (!function->impl)
            continue;

         nir_foreach_block(block, function->impl) {
            nir_foreach_instr_safe(instr, block) {
               if (instr->type != nir_instr_type_intrinsic)
                  continue;

               nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
               switch (intrin->intrinsic) {
               case nir_intrinsic_load_push_constant:
                  intrin->intrinsic = nir_intrinsic_load_uniform;
                  nir_intrinsic_set_base(intrin,
                                         nir_intrinsic_base(intrin) -
                                         push_start);
                  break;

               default:
                  break;
               }
            }
         }
      }
   }


   assert(max_num_push_ranges >= num_push_ranges);
   const unsigned max_push_ubo_ranges = max_num_push_ranges - num_push_ranges;
   const unsigned max_ubo_push_regs = 64 - push_constant_range.length;

   if (use_gather) {
      unsigned gather_count;
      struct brw_ubo_gather *gathers =
         brw_nir_gather_ubo_loads(nir, max_ubo_push_regs * 32,
                                  &gather_count, mem_ctx);
      if (gathers == NULL)
         return;

      const unsigned num_push_ubo_ranges =
         anv_ubo_gathers_as_ubo_ranges(gather_count, gathers, map,
                                       max_ubo_push_regs * 32,
                                       max_push_ubo_ranges,
                                       &map->push_ranges[num_push_ranges]);
      if (num_push_ubo_ranges) {
         /* We don't need to gather and we can do a simple push */
         num_push_ranges += num_push_ubo_ranges;
      } else {
         /* We really do want to gather */
         brw_nir_lower_gathered_ubo_loads(nir,
                                          push_constant_range.length * 32,
                                          gather_count, gathers);

         map->gathers = rzalloc_array(mem_ctx, struct anv_push_gather,
                                      gather_count);
         map->gather_count = gather_count;
         map->gather_size = 0;
         for (unsigned i = 0; i < gather_count; i++) {
            const struct anv_pipeline_binding *binding =
               &map->surface_to_descriptor[gathers[i].block];
            map->gathers[i] = (struct anv_push_gather) {
               .set = binding->set,
               .index = binding->index,
               .dynamic_offset_index = binding->dynamic_offset_index,
               .start = gathers[i].start,
               .dwords = gathers[i].dwords,
            };
            map->gather_size += util_bitcount(gathers[i].dwords) * 4;
         }
         map->gather_size = align_u32(map->gather_size, 32);

         map->push_ranges[num_push_ranges++] = (struct anv_push_range) {
            .set = ANV_DESCRIPTOR_SET_GATHER_CONSTANTS,
            .length = DIV_ROUND_UP(map->gather_size, 32),
         };
      }

      nir->num_uniforms = 0;
      for (unsigned i = 0; i < num_push_ranges; i++)
         nir->num_uniforms += map->push_ranges[i].length * 32;

      /* We have to grow the push size because we did the rewrite here rather
       * than letting the back-end do it.
       */
      prog_data->nr_params = nir->num_uniforms / 4;
      prog_data->param = rzalloc_array(mem_ctx, uint32_t, prog_data->nr_params);
   } else if (push_ubo_ranges) {
      brw_nir_analyze_ubo_ranges(compiler, nir, NULL, prog_data->ubo_ranges);

      /* We can push at most 64 registers worth of data.  The back-end
       * compiler would do this fixup for us but we'd like to calculate
       * the push constant layout ourselves.
       */
      unsigned total_push_regs = push_constant_range.length;
      for (unsigned i = 0; i < 4; i++) {
         if (total_push_regs + prog_data->ubo_ranges[i].length > 64)
            prog_data->ubo_ranges[i].length = 64 - total_push_regs;
         total_push_regs += prog_data->ubo_ranges[i].length;
      }
      assert(total_push_regs <= 64);

      if (robust_buffer_access) {
         const uint32_t push_reg_mask_offset =
            offsetof(struct anv_push_constants, push_reg_mask);
         assert(push_reg_mask_offset >= push_start);
         prog_data->push_reg_mask_param =
            (push_reg_mask_offset - push_start) / 4;
      }

      unsigned range_start_reg = push_constant_range.length;

      for (int i = 0; i < 4; i++) {
         struct brw_ubo_range *ubo_range = &prog_data->ubo_ranges[i];
         if (ubo_range->length == 0)
            continue;

         if (num_push_ranges >= max_num_push_ranges) {
            memset(ubo_range, 0, sizeof(*ubo_range));
            continue;
         }

         const struct anv_pipeline_binding *binding =
            &map->surface_to_descriptor[ubo_range->block];

         map->push_ranges[num_push_ranges++] = (struct anv_push_range) {
            .set = binding->set,
            .index = binding->index,
            .dynamic_offset_index = binding->dynamic_offset_index,
            .start = ubo_range->start,
            .length = ubo_range->length,
         };

         /* We only bother to shader-zero pushed client UBOs */
         if (binding->set < MAX_SETS && robust_buffer_access) {
            prog_data->zero_push_reg |= BITFIELD64_RANGE(range_start_reg,
                                                         ubo_range->length);
         }

         range_start_reg += ubo_range->length;
      }
   } else {
      /* For Ivy Bridge, the push constants packets have a different
       * rule that would require us to iterate in the other direction
       * and possibly mess around with dynamic state base address.
       * Don't bother; just emit regular push constants at n = 0.
       *
       * In the compute case, we don't have multiple push ranges so it's
       * better to just provide one in push_ranges[0].
       */
      map->push_ranges[0] = push_constant_range;
   }

   /* Now that we're done computing the push constant portion of the
    * bind map, hash it.  This lets us quickly determine if the actual
    * mapping has changed and not just a no-op pipeline change.
    */
   _mesa_sha1_compute(map->gathers,
                      map->gather_count * sizeof(*map->gathers),
                      map->gather_sha1);
   _mesa_sha1_compute(map->push_ranges,
                      sizeof(map->push_ranges),
                      map->push_sha1);
}

void
anv_nir_validate_push_layout(struct brw_stage_prog_data *prog_data,
                             struct anv_pipeline_bind_map *map)
{
#ifndef NDEBUG
   unsigned prog_data_push_size = DIV_ROUND_UP(prog_data->nr_params, 8);
   for (unsigned i = 0; i < 4; i++)
      prog_data_push_size += prog_data->ubo_ranges[i].length;

   unsigned bind_map_push_size = 0;
   for (unsigned i = 0; i < 4; i++)
      bind_map_push_size += map->push_ranges[i].length;

   /* We could go through everything again but it should be enough to assert
    * that they push the same number of registers.  This should alert us if
    * the back-end compiler decides to re-arrange stuff or shrink a range.
    */
   assert(prog_data_push_size == bind_map_push_size);
#endif
}
