/*
 * Copyright © 2016 Intel Corporation
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

/**
 * This file implements some lightweight memcpy/memset operations on the GPU
 * using a vertex buffer and streamout.
 */

/**
 * Returns the greatest common divisor of a and b that is a power of two.
 */
static inline uint64_t
gcd_pow2_u64(uint64_t a, uint64_t b)
{
   assert(a > 0 || b > 0);

   unsigned a_log2 = ffsll(a) - 1;
   unsigned b_log2 = ffsll(b) - 1;

   /* If either a or b is 0, then a_log2 or b_log2 till be UINT_MAX in which
    * case, the MIN2() will take the other one.  If both are 0 then we will
    * hit the assert above.
    */
   return 1 << MIN2(a_log2, b_log2);
}

void
genX(cmd_buffer_gpu_memcpy)(struct anv_cmd_buffer *cmd_buffer,
                            struct anv_bo *dst, uint32_t dst_offset,
                            struct anv_bo *src, uint32_t src_offset,
                            uint32_t size)
{
   uint32_t *dw;

   unsigned bs = 16;
   bs = gcd_pow2_u64(bs, src_offset);
   bs = gcd_pow2_u64(bs, dst_offset);
   bs = gcd_pow2_u64(bs, size);

   enum isl_format format;
   switch (bs) {
   case 4:  format = ISL_FORMAT_R32_UINT;          break;
   case 8:  format = ISL_FORMAT_R32G32_UINT;       break;
   case 16: format = ISL_FORMAT_R32G32B32A32_UINT; break;
   default:
      unreachable("Invalid size");
   }

   dw = anv_batch_emitn(&cmd_buffer->batch, 5, GENX(3DSTATE_VERTEX_BUFFERS));
   GENX(VERTEX_BUFFER_STATE_pack)(&cmd_buffer->batch, dw + 1,
      &(struct GENX(VERTEX_BUFFER_STATE)) {
         .VertexBufferIndex = 32, /* Reserved for this */
         .AddressModifyEnable = true,
         .BufferStartingAddress = { src, src_offset },
         .BufferPitch = bs,
#if (GEN_GEN >= 8)
         .MemoryObjectControlState = GENX(MOCS),
         .BufferSize = size,
#else
         .VertexBufferMemoryObjectControlState = GENX(MOCS),
         .EndAddress = { src, src_offset + size - 1 },
#endif
      });

   dw = anv_batch_emitn(&cmd_buffer->batch, 3, GENX(3DSTATE_VERTEX_ELEMENTS));
   GENX(VERTEX_ELEMENT_STATE_pack)(&cmd_buffer->batch, dw + 1,
      &(struct GENX(VERTEX_ELEMENT_STATE)) {
         .VertexBufferIndex = 32,
         .Valid = true,
         .SourceElementFormat = format,
         .SourceElementOffset = 0,
         .Component0Control = (bs >= 4) ? VFCOMP_STORE_SRC : VFCOMP_STORE_0,
         .Component1Control = (bs >= 8) ? VFCOMP_STORE_SRC : VFCOMP_STORE_0,
         .Component2Control = (bs >= 12) ? VFCOMP_STORE_SRC : VFCOMP_STORE_0,
         .Component3Control = (bs >= 16) ? VFCOMP_STORE_SRC : VFCOMP_STORE_0,
      });

   /* Disable all shader stages */
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VS), vs);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_HS), hs);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_TE), te);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_DS), DS);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_GS), gs);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_PS), gs);

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_SBE), sbe) {
      sbe.VertexURBEntryReadOffset = 1;
      sbe.NumberofSFOutputAttributes = 1;
      sbe.VertexURBEntryReadLength = 1;
#if GEN_GEN >= 8
      sbe.ForceVertexURBEntryReadLength = true;
      sbe.ForceVertexURBEntryReadOffset = true;
#endif

#if GEN_GEN >= 9
      for (unsigned i = 0; i < 32; i++)
         sbe.AttributeActiveComponentFormat[i] = ACF_XYZW;
#endif
   }

   genX(emit_urb_setup)(cmd_buffer->device, &cmd_buffer->batch,
                        VK_SHADER_STAGE_VERTEX_BIT |
                        VK_SHADER_STAGE_FRAGMENT_BIT,
                        DIV_ROUND_UP(32, 64), 0,
                        cmd_buffer->state.current_l3_config);

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_SO_BUFFER), sob) {
#if GEN_GEN >= 8
      sob.SOBufferEnable = true;
#endif
      sob.SOBufferIndex = 0;
      sob.SOBufferObjectControlState = GENX(MOCS);
      sob.SurfaceBaseAddress = (struct anv_address) { dst, dst_offset };
#if GEN_GEN >= 8
      sob.SurfaceSize = size - 1;
#else
      sob.SurfaceEndAddress = sob.SurfaceBaseAddress;
      sob.SurfaceEndAddress.offset += size - 1;
      sob.SurfacePitch = bs;
#endif
   }

   dw = anv_batch_emitn(&cmd_buffer->batch, 5, GENX(3DSTATE_SO_DECL_LIST),
                        .StreamtoBufferSelects0 = (1 << 0),
                        .NumEntries0 = 1);
   GENX(SO_DECL_ENTRY_pack)(&cmd_buffer->batch, dw + 3,
      &(struct GENX(SO_DECL_ENTRY)) {
         .Stream0Decl = {
            .OutputBufferSlot = 0,
            .RegisterIndex = 0,
            .ComponentMask = (1 << (bs / 4)) - 1,
         },
      });

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_STREAMOUT), so) {
      so.SOFunctionEnable = true;
      so.RenderingDisable = true;
      so.Stream0VertexReadOffset = 0;
      so.Stream0VertexReadLength = DIV_ROUND_UP(32, 64);
#if GEN_GEN >= 8
      so.Buffer0SurfacePitch = bs;
#endif
   }

#if GEN_GEN >= 8
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_VF_TOPOLOGY), topo) {
      topo.PrimitiveTopologyType = _3DPRIM_POINTLIST;
   }
#endif

   anv_batch_emit(&cmd_buffer->batch, GENX(3DPRIMITIVE), prim) {
      prim.VertexAccessType         = SEQUENTIAL;
      prim.PrimitiveTopologyType    = _3DPRIM_POINTLIST;
      prim.VertexCountPerInstance   = size / bs;
      prim.StartVertexLocation      = 0;
      prim.InstanceCount            = 1;
      prim.StartInstanceLocation    = 0;
      prim.BaseVertexLocation       = 0;
   }

   cmd_buffer->state.dirty |= ANV_CMD_DIRTY_PIPELINE;
}

void genX(CmdCopyBuffer)(
    VkCommandBuffer                             commandBuffer,
    VkBuffer                                    srcBuffer,
    VkBuffer                                    dstBuffer,
    uint32_t                                    regionCount,
    const VkBufferCopy*                         pRegions)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   ANV_FROM_HANDLE(anv_buffer, src_buffer, srcBuffer);
   ANV_FROM_HANDLE(anv_buffer, dst_buffer, dstBuffer);

   if (!cmd_buffer->state.current_l3_config) {
      const struct gen_l3_config *cfg =
         gen_get_default_l3_config(&cmd_buffer->device->info);
      genX(cmd_buffer_config_l3)(cmd_buffer, cfg);
   }

   for (unsigned r = 0; r < regionCount; r++) {
      uint64_t src_offset = src_buffer->offset + pRegions[r].srcOffset;
      uint64_t dst_offset = dst_buffer->offset + pRegions[r].dstOffset;
      uint64_t copy_size = pRegions[r].size;

      genX(cmd_buffer_gpu_memcpy)(cmd_buffer, dst_buffer->bo, dst_offset,
                                  src_buffer->bo, src_offset, copy_size);
   }
}
