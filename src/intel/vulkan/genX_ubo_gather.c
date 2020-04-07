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

#include "anv_private.h"

#include "genxml/gen_macros.h"
#include "genxml/genX_pack.h"

#include "common/gen_l3_config.h"
#include "compiler/brw_nir_ubo_gather.h"

#define __gen_get_batch_dwords anv_batch_emit_dwords
#define __gen_address_offset anv_address_add
#include "common/gen_mi_builder.h"

#define ANV_GATHER_BO_SIZE 8192

/* The first dword of the gather BO is reserved for the number of gathers */
#define ANV_GATHER_HEADER_SIZE 4

#if GEN_GEN >= 8 /* Gather requires A64 messages; we can't do it on Gen7 */

/* Auto-Draw / Indirect Registers */
#define GEN7_3DPRIM_END_OFFSET          0x2420
#define GEN7_3DPRIM_START_VERTEX        0x2430
#define GEN7_3DPRIM_VERTEX_COUNT        0x2434
#define GEN7_3DPRIM_INSTANCE_COUNT      0x2438
#define GEN7_3DPRIM_START_INSTANCE      0x243C
#define GEN7_3DPRIM_BASE_VERTEX         0x2440

static void
emit_gather_draw(struct anv_cmd_buffer *cmd_buffer,
                 struct anv_address count_addr,
                 struct anv_address gather_addr,
                 uint32_t max_gather_size)
{
   const struct gen_device_info *devinfo = &cmd_buffer->device->info;
   assert(cmd_buffer->state.current_pipeline == _3D);
   assert(cmd_buffer->state.current_l3_config != NULL);

   /* We are about to read uniform data via the dataport.  This means we need
    * to invalidate the data cache.  Unfortunately, the only way to do that is
    * with a full data cache flush.
    */
   cmd_buffer->state.pending_pipe_bits |=
      ANV_PIPE_DATA_CACHE_FLUSH_BIT | ANV_PIPE_END_OF_PIPE_SYNC_BIT;
   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

   uint32_t *dw;
   dw = anv_batch_emitn(&cmd_buffer->batch, 5, GENX(3DSTATE_VERTEX_BUFFERS));
   GENX(VERTEX_BUFFER_STATE_pack)(&cmd_buffer->batch, dw + 1,
      &(struct GENX(VERTEX_BUFFER_STATE)) {
         .VertexBufferIndex = 32, /* Reserved for this and gpu_memcpy */
         .AddressModifyEnable = true,
         .BufferStartingAddress = gather_addr,
         .BufferPitch = BRW_NIR_GATHER_VS_ENTRY_SIZE,
         .MOCS = anv_mocs_for_bo(cmd_buffer->device, gather_addr.bo),
#if (GEN_GEN >= 8)
         .BufferSize = max_gather_size,
#else
         .EndAddress = anv_address_add(gather_addr, max_gather_size - 1),
#endif
      });

   assert(BRW_NIR_GATHER_VS_ENTRY_SIZE == 16);
   dw = anv_batch_emitn(&cmd_buffer->batch, 3, GENX(3DSTATE_VERTEX_ELEMENTS));
   GENX(VERTEX_ELEMENT_STATE_pack)(&cmd_buffer->batch, dw + 1,
      &(struct GENX(VERTEX_ELEMENT_STATE)) {
         .VertexBufferIndex = 32,
         .Valid = true,
         .SourceElementFormat = ISL_FORMAT_R32G32B32A32_UINT,
         .SourceElementOffset = 0,
         .Component0Control = VFCOMP_STORE_SRC,
         .Component1Control = VFCOMP_STORE_SRC,
         .Component2Control = VFCOMP_STORE_SRC,
         .Component3Control = VFCOMP_STORE_SRC,
      });

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VF_INSTANCING), vfi) {
      vfi.InstancingEnable = false;
      vfi.VertexElementIndex = 0;
   }

#if GEN_GEN >= 8
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VF_SGVS), sgvs);
#endif

   const struct anv_shader_bin *gather_vs =
      anv_get_gather_shader_bin(cmd_buffer->device);
   const struct brw_vs_prog_data *gather_vs_prog_data =
      (const struct brw_vs_prog_data *)gather_vs->prog_data;

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VS), vs) {
      vs.Enable                     = true;
      vs.KernelStartPointer         = gather_vs->kernel.offset;
      assert(gather_vs_prog_data->base.dispatch_mode == DISPATCH_MODE_SIMD8);
      vs.SIMD8DispatchEnable        = true;
      vs.MaximumNumberofThreads     = devinfo->max_vs_threads - 1;
      vs.VertexURBEntryReadLength   = gather_vs_prog_data->base.urb_read_length;
      vs.VertexURBEntryReadOffset   = 0;
      vs.DispatchGRFStartRegisterForURBData =
         gather_vs_prog_data->base.base.dispatch_grf_start_reg;

      assert(gather_vs_prog_data->base.base.total_scratch == 0);
   }

   /* Disable all other shader stages */
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_HS), hs);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_TE), te);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_DS), DS);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_GS), gs);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_PS), gs);

   /* Disable push constants */
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_CONSTANT_VS), hs);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_BINDING_TABLE_POINTERS_VS), bt);

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_SBE), sbe) {
      sbe.VertexURBEntryReadOffset = 1;
      sbe.NumberofSFOutputAttributes = 1;
      sbe.VertexURBEntryReadLength = 1;
      sbe.ForceVertexURBEntryReadLength = true;
      sbe.ForceVertexURBEntryReadOffset = true;

#if GEN_GEN >= 9
      for (unsigned i = 0; i < 32; i++)
         sbe.AttributeActiveComponentFormat[i] = ACF_XYZW;
#endif
   }

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_STREAMOUT), so) {
      so.RenderingDisable = true;
   }

   const unsigned entry_size[4] = { DIV_ROUND_UP(32, 64), 1, 1, 1 };
   genX(emit_urb_setup)(cmd_buffer->device, &cmd_buffer->batch,
                        cmd_buffer->state.current_l3_config,
                        VK_SHADER_STAGE_VERTEX_BIT, entry_size, NULL);

#if GEN_GEN >= 8
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VF_TOPOLOGY), topo) {
      topo.PrimitiveTopologyType = _3DPRIM_POINTLIST;
   }
#endif

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VF_STATISTICS), vf) {
      vf.StatisticsEnable = false;
   }

#if GEN_GEN >= 12
   /* Disable Primitive Replication. */
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_PRIMITIVE_REPLICATION), pr);
#endif

   /* We do an indirect draw, reading the vertex count from the first dword of
    * our buffer.
    */
   struct gen_mi_builder b;
   gen_mi_builder_init(&b, &cmd_buffer->batch);
   gen_mi_store(&b, gen_mi_reg32(GEN7_3DPRIM_INSTANCE_COUNT), gen_mi_imm(1));
   gen_mi_store(&b, gen_mi_reg32(GEN7_3DPRIM_START_VERTEX), gen_mi_imm(0));
   gen_mi_store(&b, gen_mi_reg32(GEN7_3DPRIM_BASE_VERTEX), gen_mi_imm(0));
   gen_mi_store(&b, gen_mi_reg32(GEN7_3DPRIM_START_INSTANCE), gen_mi_imm(0));
   gen_mi_store(&b, gen_mi_reg32(GEN7_3DPRIM_VERTEX_COUNT),
                    gen_mi_mem32(count_addr));

   anv_batch_emit(&cmd_buffer->batch, GENX(3DPRIMITIVE), prim) {
      prim.VertexAccessType         = SEQUENTIAL;
      prim.PrimitiveTopologyType    = _3DPRIM_POINTLIST;
      prim.IndirectParameterEnable  = true;
   }

   genX(cmd_buffer_update_dirty_vbs_for_gen8_vb_flush)(cmd_buffer, SEQUENTIAL,
                                                       1ull << 32);

   cmd_buffer->state.descriptors_dirty |= VK_SHADER_STAGE_VERTEX_BIT;
   cmd_buffer->state.push_constants_dirty |= VK_SHADER_STAGE_VERTEX_BIT;
   cmd_buffer->state.gfx.dirty |= ANV_CMD_DIRTY_PIPELINE;

   /* We wrote the data we're about to push with the data cache.  We need to
    * flush out the cache to ensure everything gets written before any
    * 3DSTATE_CONSTANT_* commands try to pick it up.
    */
   cmd_buffer->state.pending_pipe_bits |=
      ANV_PIPE_DATA_CACHE_FLUSH_BIT | ANV_PIPE_END_OF_PIPE_SYNC_BIT;
   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);
}

static bool
ensure_gather_space(struct anv_cmd_buffer *cmd_buffer,
                    struct anv_cmd_gather_state *gather,
                    uint32_t count)
{
   if (gather->bo != NULL) {
      assert(gather->bo->size == ANV_GATHER_BO_SIZE);
      /* Sanity check that we've kept the count in the buffer up-to-date */
      assert(gather->count == *(uint32_t *)gather->bo->map);

      uint32_t req_size = ANV_GATHER_HEADER_SIZE +
         BRW_NIR_GATHER_VS_ENTRY_SIZE * (gather->count + count);
      if (req_size < ANV_GATHER_BO_SIZE)
         return true;
   }

   VkResult result = anv_bo_pool_alloc(&cmd_buffer->device->batch_bo_pool,
                                       ANV_GATHER_BO_SIZE, &gather->bo);
   if (result != VK_SUCCESS) {
      anv_batch_set_error(&cmd_buffer->batch, result);
      return false;
   }

   util_dynarray_append(&gather->used_bos, struct anv_bo *, gather->bo);
   gather->count = 0;

   struct anv_address bo_addr = { .bo = gather->bo };

   emit_gather_draw(cmd_buffer, bo_addr,
                    anv_address_add(bo_addr, ANV_GATHER_HEADER_SIZE),
                    ANV_GATHER_BO_SIZE - ANV_GATHER_HEADER_SIZE);

   return true;
}

static void
add_gather(struct anv_cmd_gather_state *gather,
           uint64_t dst_u64, uint64_t src_u64, uint32_t mask)
{
   void *gather_data = gather->bo->map + ANV_GATHER_HEADER_SIZE +
                       BRW_NIR_GATHER_VS_ENTRY_SIZE * gather->count;
   assert(gather_data + BRW_NIR_GATHER_VS_ENTRY_SIZE - gather->bo->map <
          gather->bo->size);

   brw_nir_pack_gather_vs_entry(gather_data, dst_u64, src_u64, mask);

   gather->count++;
}

static struct anv_address
anv_descriptor_set_address(struct anv_cmd_buffer *cmd_buffer,
                           struct anv_descriptor_set *set)
{
   if (set->pool) {
      /* This is a normal descriptor set */
      return (struct anv_address) {
         .bo = set->pool->bo,
         .offset = set->desc_mem.offset,
      };
   } else {
      /* This is a push descriptor set.  We have to flag it as used on the GPU
       * so that the next time we push descriptors, we grab a new memory.
       */
      struct anv_push_descriptor_set *push_set =
         (struct anv_push_descriptor_set *)set;
      push_set->set_used_on_gpu = true;

      return (struct anv_address) {
         .bo = cmd_buffer->dynamic_state_stream.state_pool->block_pool.bo,
         .offset = set->desc_mem.offset,
      };
   }
}

static void
gather_stage_constants(struct anv_cmd_buffer *cmd_buffer,
                       struct anv_cmd_gather_state *gather,
                       struct anv_cmd_pipeline_state *pipe_state,
                       const struct anv_shader_bin *bin)
{
   assert(cmd_buffer->device->physical->use_softpin);
   const struct anv_pipeline_bind_map *bind_map = &bin->bind_map;
   const gl_shader_stage stage = bin->stage;

   /* Ensure we have space for 2x the gathers */
   if (!ensure_gather_space(cmd_buffer, gather, bind_map->gather_count * 2))
      return;

   struct anv_state push_data =
      anv_cmd_buffer_alloc_dynamic_state(cmd_buffer, bind_map->gather_size, 32);
   if (push_data.map == NULL) {
      anv_batch_set_error(&cmd_buffer->batch,
                          vk_error(VK_ERROR_OUT_OF_DEVICE_MEMORY));
      return;
   }
   gather->data[stage].address = (struct anv_address) {
      .bo = cmd_buffer->device->dynamic_state_pool.block_pool.bo,
      .offset = push_data.offset,
   };
   gather->data[stage].size = bind_map->gather_size;

   uint64_t dst_addr_u64 = anv_address_physical(gather->data[stage].address);
   uint64_t src_addr_base_u64 = 0xdeadbeefc0ffee42;
   uint32_t src_bound_range = 0;
   uint8_t last_set = ANV_DESCRIPTOR_SET_NULL; /* Something we'll never see */
   uint32_t last_index = 0;

   for (unsigned i = 0; i < bind_map->gather_count; i++) {
      const struct anv_push_gather *entry = &bind_map->gathers[i];
      if (entry->set != last_set || entry->index != last_index) {
         if (entry->set == ANV_DESCRIPTOR_SET_DESCRIPTORS) {
            /* This is a descriptor set buffer so the set index is
             * actually given by binding->binding.  (Yes, that's
             * confusing.)
             */
            struct anv_descriptor_set *set =
               pipe_state->descriptors[entry->index];
            src_addr_base_u64 =
               anv_address_physical(anv_descriptor_set_address(cmd_buffer, set));
            src_bound_range = set->desc_mem.alloc_size;
         } else {
            struct anv_descriptor_set *set =
               pipe_state->descriptors[entry->set];
            const struct anv_descriptor *desc =
               &set->descriptors[entry->index];

            if (desc->type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER) {
               src_addr_base_u64 =
                  anv_address_physical(desc->buffer_view->address);
               src_bound_range = desc->buffer_view->range;
            } else {
               assert(desc->type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC);
               struct anv_push_constants *push =
                  &cmd_buffer->state.push_constants[stage];
               uint32_t dynamic_offset =
                  push->dynamic_offsets[entry->dynamic_offset_index];

               uint64_t offset = desc->offset + dynamic_offset;
               /* Clamp to the buffer size */
               offset = MIN2(offset, desc->buffer->size);

               src_addr_base_u64 =
                  anv_address_physical(desc->buffer->address) + offset;

               /* Clamp the range to the buffer size */
               src_bound_range = MIN2(desc->range, desc->buffer->size - offset);

               /* Align the range for consistency */
               src_bound_range = align_u32(src_bound_range,
                                           ANV_UBO_BOUNDS_CHECK_ALIGNMENT);
            }
         }
      }

      uint64_t src_addr_u64 = src_addr_base_u64 + entry->start;

      uint32_t zero_dwords = entry->dwords;
      if (entry->start < src_bound_range) {
         uint32_t rel_range_dw = (src_bound_range - entry->start) / 4;
         uint32_t nonzero_dwords = entry->dwords;
         if (rel_range_dw < 32) {
            nonzero_dwords &= BITFIELD_RANGE(0, rel_range_dw);
            zero_dwords &= ~BITFIELD_RANGE(0, rel_range_dw);
         } else {
            zero_dwords = 0;
         }

         add_gather(gather, dst_addr_u64, src_addr_u64, nonzero_dwords);
         src_addr_u64 += util_bitcount(nonzero_dwords) * 4;
         dst_addr_u64 += util_bitcount(nonzero_dwords) * 4;
      }

      if (zero_dwords) {
         add_gather(gather, dst_addr_u64, 0, zero_dwords);
         src_addr_u64 += util_bitcount(zero_dwords) * 4;
         dst_addr_u64 += util_bitcount(zero_dwords) * 4;
      }
   }

   *(uint32_t *)gather->bo->map = gather->count;
}

void
genX(cmd_buffer_flush_gather_constants)(struct anv_cmd_buffer *cmd_buffer,
                                        struct anv_cmd_gather_state *gather,
                                        struct anv_graphics_pipeline *pipeline)
{
   for (unsigned i = 0; i < ARRAY_SIZE(pipeline->shaders); i++) {
      const struct anv_shader_bin *bin = pipeline->shaders[i];
      if (bin == NULL || bin->bind_map.gather_count == 0)
         continue;

      if (!(gather->dirty & mesa_to_vk_shader_stage(bin->stage)))
         continue;

      gather_stage_constants(cmd_buffer, gather,
                             &cmd_buffer->state.gfx.base, bin);

      gather->dirty &= ~mesa_to_vk_shader_stage(bin->stage);
   }
}

#endif /* GEN_GEN >= 8 */
