/*
 * Mesa 3-D graphics library
 *
 * Copyright (C) 2012-2013 LunarG, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Chia-I Wu <olv@lunarg.com>
 */

#include "pipe/p_state.h"
#include "os/os_misc.h"
#include "util/u_format_s3tc.h"
#include "vl/vl_decoder.h"
#include "vl/vl_video_buffer.h"
#include "genhw/genhw.h" /* for GEN6_REG_TIMESTAMP */
#include "intel_winsys.h"

#include "ilo_context.h"
#include "ilo_format.h"
#include "ilo_resource.h"
#include "ilo_transfer.h" /* for ILO_TRANSFER_MAP_BUFFER_ALIGNMENT */
#include "ilo_public.h"
#include "ilo_screen.h"

struct ilo_fence {
   struct pipe_reference reference;
   struct intel_bo *bo;
};

int ilo_debug;

static const struct debug_named_value ilo_debug_flags[] = {
   { "batch",     ILO_DEBUG_BATCH,    "Dump batch/state/surface/instruction buffers" },
   { "vs",        ILO_DEBUG_VS,       "Dump vertex shaders" },
   { "gs",        ILO_DEBUG_GS,       "Dump geometry shaders" },
   { "fs",        ILO_DEBUG_FS,       "Dump fragment shaders" },
   { "cs",        ILO_DEBUG_CS,       "Dump compute shaders" },
   { "draw",      ILO_DEBUG_DRAW,     "Show draw information" },
   { "submit",    ILO_DEBUG_SUBMIT,   "Show batch buffer submissions" },
   { "nohw",      ILO_DEBUG_NOHW,     "Do not send commands to HW" },
   { "nocache",   ILO_DEBUG_NOCACHE,  "Always invalidate HW caches" },
   { "nohiz",     ILO_DEBUG_NOHIZ,    "Disable HiZ" },
   DEBUG_NAMED_VALUE_END
};

static float
ilo_get_paramf(struct pipe_screen *screen, enum pipe_capf param)
{
   switch (param) {
   case PIPE_CAPF_MAX_LINE_WIDTH:
      /* in U3.7, defined in 3DSTATE_SF */
      return 7.0f;
   case PIPE_CAPF_MAX_LINE_WIDTH_AA:
      /* line width minus one, which is reserved for AA region */
      return 6.0f;
   case PIPE_CAPF_MAX_POINT_WIDTH:
      /* in U8.3, defined in 3DSTATE_SF */
      return 255.0f;
   case PIPE_CAPF_MAX_POINT_WIDTH_AA:
      /* same as point width, as we ignore rasterizer->point_smooth */
      return 255.0f;
   case PIPE_CAPF_MAX_TEXTURE_ANISOTROPY:
      /* [2.0, 16.0], defined in SAMPLER_STATE */
      return 16.0f;
   case PIPE_CAPF_MAX_TEXTURE_LOD_BIAS:
      /* [-16.0, 16.0), defined in SAMPLER_STATE */
      return 15.0f;
   case PIPE_CAPF_GUARD_BAND_LEFT:
   case PIPE_CAPF_GUARD_BAND_TOP:
   case PIPE_CAPF_GUARD_BAND_RIGHT:
   case PIPE_CAPF_GUARD_BAND_BOTTOM:
      /* what are these for? */
      return 0.0f;

   default:
      return 0.0f;
   }
}

static int
ilo_get_shader_param(struct pipe_screen *screen, unsigned shader,
                     enum pipe_shader_cap param)
{
   switch (shader) {
   case PIPE_SHADER_FRAGMENT:
   case PIPE_SHADER_VERTEX:
   case PIPE_SHADER_GEOMETRY:
      break;
   default:
      return 0;
   }

   switch (param) {
   /* the limits are copied from the classic driver */
   case PIPE_SHADER_CAP_MAX_INSTRUCTIONS:
      return (shader == PIPE_SHADER_FRAGMENT) ? 1024 : 16384;
   case PIPE_SHADER_CAP_MAX_ALU_INSTRUCTIONS:
      return (shader == PIPE_SHADER_FRAGMENT) ? 1024 : 0;
   case PIPE_SHADER_CAP_MAX_TEX_INSTRUCTIONS:
      return (shader == PIPE_SHADER_FRAGMENT) ? 1024 : 0;
   case PIPE_SHADER_CAP_MAX_TEX_INDIRECTIONS:
      return (shader == PIPE_SHADER_FRAGMENT) ? 1024 : 0;
   case PIPE_SHADER_CAP_MAX_CONTROL_FLOW_DEPTH:
      return UINT_MAX;
   case PIPE_SHADER_CAP_MAX_INPUTS:
   case PIPE_SHADER_CAP_MAX_OUTPUTS:
      /* this is limited by how many attributes SF can remap */
      return 16;
   case PIPE_SHADER_CAP_MAX_CONST_BUFFER_SIZE:
      return 1024 * sizeof(float[4]);
   case PIPE_SHADER_CAP_MAX_CONST_BUFFERS:
      return ILO_MAX_CONST_BUFFERS;
   case PIPE_SHADER_CAP_MAX_TEMPS:
      return 256;
   case PIPE_SHADER_CAP_MAX_PREDS:
      return 0;
   case PIPE_SHADER_CAP_TGSI_CONT_SUPPORTED:
      return 1;
   case PIPE_SHADER_CAP_INDIRECT_INPUT_ADDR:
      return 0;
   case PIPE_SHADER_CAP_INDIRECT_OUTPUT_ADDR:
      return 0;
   case PIPE_SHADER_CAP_INDIRECT_TEMP_ADDR:
      return (shader == PIPE_SHADER_FRAGMENT) ? 0 : 1;
   case PIPE_SHADER_CAP_INDIRECT_CONST_ADDR:
      return 1;
   case PIPE_SHADER_CAP_SUBROUTINES:
      return 0;
   case PIPE_SHADER_CAP_INTEGERS:
      return 1;
   case PIPE_SHADER_CAP_MAX_TEXTURE_SAMPLERS:
      return ILO_MAX_SAMPLERS;
   case PIPE_SHADER_CAP_MAX_SAMPLER_VIEWS:
      return ILO_MAX_SAMPLER_VIEWS;
   case PIPE_SHADER_CAP_PREFERRED_IR:
      return PIPE_SHADER_IR_TGSI;
   case PIPE_SHADER_CAP_TGSI_SQRT_SUPPORTED:
      return 1;

   default:
      return 0;
   }
}

static int
ilo_get_video_param(struct pipe_screen *screen,
                    enum pipe_video_profile profile,
                    enum pipe_video_entrypoint entrypoint,
                    enum pipe_video_cap param)
{
   switch (param) {
   case PIPE_VIDEO_CAP_SUPPORTED:
      return vl_profile_supported(screen, profile, entrypoint);
   case PIPE_VIDEO_CAP_NPOT_TEXTURES:
      return 1;
   case PIPE_VIDEO_CAP_MAX_WIDTH:
   case PIPE_VIDEO_CAP_MAX_HEIGHT:
      return vl_video_buffer_max_size(screen);
   case PIPE_VIDEO_CAP_PREFERED_FORMAT:
      return PIPE_FORMAT_NV12;
   case PIPE_VIDEO_CAP_PREFERS_INTERLACED:
      return 1;
   case PIPE_VIDEO_CAP_SUPPORTS_PROGRESSIVE:
      return 1;
   case PIPE_VIDEO_CAP_SUPPORTS_INTERLACED:
      return 0;
   case PIPE_VIDEO_CAP_MAX_LEVEL:
      return vl_level_supported(screen, profile);
   default:
      return 0;
   }
}

static int
ilo_get_compute_param(struct pipe_screen *screen,
                      enum pipe_compute_cap param,
                      void *ret)
{
   union {
      const char *ir_target;
      uint64_t grid_dimension;
      uint64_t max_grid_size[3];
      uint64_t max_block_size[3];
      uint64_t max_threads_per_block;
      uint64_t max_global_size;
      uint64_t max_local_size;
      uint64_t max_private_size;
      uint64_t max_input_size;
      uint64_t max_mem_alloc_size;
   } val;
   const void *ptr;
   int size;

   /* XXX some randomly chosen values */
   switch (param) {
   case PIPE_COMPUTE_CAP_IR_TARGET:
      val.ir_target = "ilog";

      ptr = val.ir_target;
      size = strlen(val.ir_target) + 1;
      break;
   case PIPE_COMPUTE_CAP_GRID_DIMENSION:
      val.grid_dimension = Elements(val.max_grid_size);

      ptr = &val.grid_dimension;
      size = sizeof(val.grid_dimension);
      break;
   case PIPE_COMPUTE_CAP_MAX_GRID_SIZE:
      val.max_grid_size[0] = 65535;
      val.max_grid_size[1] = 65535;
      val.max_grid_size[2] = 1;

      ptr = &val.max_grid_size;
      size = sizeof(val.max_grid_size);
      break;
   case PIPE_COMPUTE_CAP_MAX_BLOCK_SIZE:
      val.max_block_size[0] = 512;
      val.max_block_size[1] = 512;
      val.max_block_size[2] = 512;

      ptr = &val.max_block_size;
      size = sizeof(val.max_block_size);
      break;

   case PIPE_COMPUTE_CAP_MAX_THREADS_PER_BLOCK:
      val.max_threads_per_block = 512;

      ptr = &val.max_threads_per_block;
      size = sizeof(val.max_threads_per_block);
      break;
   case PIPE_COMPUTE_CAP_MAX_GLOBAL_SIZE:
      val.max_global_size = 4;

      ptr = &val.max_global_size;
      size = sizeof(val.max_global_size);
      break;
   case PIPE_COMPUTE_CAP_MAX_LOCAL_SIZE:
      val.max_local_size = 64 * 1024;

      ptr = &val.max_local_size;
      size = sizeof(val.max_local_size);
      break;
   case PIPE_COMPUTE_CAP_MAX_PRIVATE_SIZE:
      val.max_private_size = 32768;

      ptr = &val.max_private_size;
      size = sizeof(val.max_private_size);
      break;
   case PIPE_COMPUTE_CAP_MAX_INPUT_SIZE:
      val.max_input_size = 256;

      ptr = &val.max_input_size;
      size = sizeof(val.max_input_size);
      break;
   case PIPE_COMPUTE_CAP_MAX_MEM_ALLOC_SIZE:
      val.max_mem_alloc_size = 128 * 1024 * 1024;

      ptr = &val.max_mem_alloc_size;
      size = sizeof(val.max_mem_alloc_size);
      break;
   default:
      ptr = NULL;
      size = 0;
      break;
   }

   if (ret)
      memcpy(ret, ptr, size);

   return size;
}

static int
ilo_get_param(struct pipe_screen *screen, enum pipe_cap param)
{
   struct ilo_screen *is = ilo_screen(screen);

   switch (param) {
   case PIPE_CAP_NPOT_TEXTURES:
   case PIPE_CAP_TWO_SIDED_STENCIL:
      return true;
   case PIPE_CAP_MAX_DUAL_SOURCE_RENDER_TARGETS:
      return 0; /* TODO */
   case PIPE_CAP_ANISOTROPIC_FILTER:
   case PIPE_CAP_POINT_SPRITE:
      return true;
   case PIPE_CAP_MAX_RENDER_TARGETS:
      return ILO_MAX_DRAW_BUFFERS;
   case PIPE_CAP_OCCLUSION_QUERY:
   case PIPE_CAP_QUERY_TIME_ELAPSED:
   case PIPE_CAP_TEXTURE_SHADOW_MAP:
   case PIPE_CAP_TEXTURE_SWIZZLE: /* must be supported for shadow map */
      return true;
   case PIPE_CAP_MAX_TEXTURE_2D_LEVELS:
      /*
       * As defined in SURFACE_STATE, we have
       *
       *           Max WxHxD for 2D and CUBE     Max WxHxD for 3D
       *  GEN6           8192x8192x512            2048x2048x2048
       *  GEN7         16384x16384x2048           2048x2048x2048
       */
      return (ilo_dev_gen(&is->dev) >= ILO_GEN(7)) ? 15 : 14;
   case PIPE_CAP_MAX_TEXTURE_3D_LEVELS:
      return 12;
   case PIPE_CAP_MAX_TEXTURE_CUBE_LEVELS:
      return (ilo_dev_gen(&is->dev) >= ILO_GEN(7)) ? 15 : 14;
   case PIPE_CAP_TEXTURE_MIRROR_CLAMP:
      return false;
   case PIPE_CAP_BLEND_EQUATION_SEPARATE:
   case PIPE_CAP_SM3:
      return true;
   case PIPE_CAP_MAX_STREAM_OUTPUT_BUFFERS:
      if (ilo_dev_gen(&is->dev) >= ILO_GEN(7) && !is->dev.has_gen7_sol_reset)
         return 0;
      return ILO_MAX_SO_BUFFERS;
   case PIPE_CAP_PRIMITIVE_RESTART:
      return true;
   case PIPE_CAP_INDEP_BLEND_ENABLE:
   case PIPE_CAP_INDEP_BLEND_FUNC:
      return true;
   case PIPE_CAP_MAX_TEXTURE_ARRAY_LAYERS:
      return (ilo_dev_gen(&is->dev) >= ILO_GEN(7)) ? 2048 : 512;
   case PIPE_CAP_TGSI_FS_COORD_ORIGIN_UPPER_LEFT:
   case PIPE_CAP_TGSI_FS_COORD_ORIGIN_LOWER_LEFT:
   case PIPE_CAP_TGSI_FS_COORD_PIXEL_CENTER_HALF_INTEGER:
   case PIPE_CAP_TGSI_FS_COORD_PIXEL_CENTER_INTEGER:
   case PIPE_CAP_DEPTH_CLIP_DISABLE:
      return true;
   case PIPE_CAP_SHADER_STENCIL_EXPORT:
      return false;
   case PIPE_CAP_TGSI_INSTANCEID:
   case PIPE_CAP_VERTEX_ELEMENT_INSTANCE_DIVISOR:
      return true;
   case PIPE_CAP_FRAGMENT_COLOR_CLAMPED:
      return false;
   case PIPE_CAP_MIXED_COLORBUFFER_FORMATS:
      return true;
   case PIPE_CAP_SEAMLESS_CUBE_MAP:
   case PIPE_CAP_SEAMLESS_CUBE_MAP_PER_TEXTURE:
      return true;
   case PIPE_CAP_MIN_TEXTURE_GATHER_OFFSET:
   case PIPE_CAP_MIN_TEXEL_OFFSET:
      return -8;
   case PIPE_CAP_MAX_TEXTURE_GATHER_OFFSET:
   case PIPE_CAP_MAX_TEXEL_OFFSET:
      return 7;
   case PIPE_CAP_CONDITIONAL_RENDER:
   case PIPE_CAP_TEXTURE_BARRIER:
      return true;
   case PIPE_CAP_MAX_STREAM_OUTPUT_SEPARATE_COMPONENTS:
      return ILO_MAX_SO_BINDINGS / ILO_MAX_SO_BUFFERS;
   case PIPE_CAP_MAX_STREAM_OUTPUT_INTERLEAVED_COMPONENTS:
      return ILO_MAX_SO_BINDINGS;
   case PIPE_CAP_STREAM_OUTPUT_PAUSE_RESUME:
      if (ilo_dev_gen(&is->dev) >= ILO_GEN(7))
         return is->dev.has_gen7_sol_reset;
      else
         return false; /* TODO */
   case PIPE_CAP_TGSI_CAN_COMPACT_CONSTANTS:
      return false;
   case PIPE_CAP_VERTEX_COLOR_UNCLAMPED:
      return true;
   case PIPE_CAP_VERTEX_COLOR_CLAMPED:
      return false;
   case PIPE_CAP_GLSL_FEATURE_LEVEL:
      return 140;
   case PIPE_CAP_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION:
   case PIPE_CAP_USER_VERTEX_BUFFERS:
      return false;
   case PIPE_CAP_VERTEX_BUFFER_OFFSET_4BYTE_ALIGNED_ONLY:
   case PIPE_CAP_VERTEX_BUFFER_STRIDE_4BYTE_ALIGNED_ONLY:
   case PIPE_CAP_VERTEX_ELEMENT_SRC_OFFSET_4BYTE_ALIGNED_ONLY:
      return false;
   case PIPE_CAP_MAX_VERTEX_ATTRIB_STRIDE:
      return 2048;
   case PIPE_CAP_COMPUTE:
      return false; /* TODO */
   case PIPE_CAP_USER_INDEX_BUFFERS:
   case PIPE_CAP_USER_CONSTANT_BUFFERS:
      return true;
   case PIPE_CAP_CONSTANT_BUFFER_OFFSET_ALIGNMENT:
      /* imposed by OWord (Dual) Block Read */
      return 16;
   case PIPE_CAP_START_INSTANCE:
      return true;
   case PIPE_CAP_QUERY_TIMESTAMP:
      return is->dev.has_timestamp;
   case PIPE_CAP_TEXTURE_MULTISAMPLE:
      return false; /* TODO */
   case PIPE_CAP_MIN_MAP_BUFFER_ALIGNMENT:
      return ILO_TRANSFER_MAP_BUFFER_ALIGNMENT;
   case PIPE_CAP_CUBE_MAP_ARRAY:
   case PIPE_CAP_TEXTURE_BUFFER_OBJECTS:
      return true;
   case PIPE_CAP_TEXTURE_BUFFER_OFFSET_ALIGNMENT:
      return 1;
   case PIPE_CAP_TGSI_TEXCOORD:
      return false;
   case PIPE_CAP_PREFER_BLIT_BASED_TEXTURE_TRANSFER:
   case PIPE_CAP_QUERY_PIPELINE_STATISTICS:
      return true;
   case PIPE_CAP_TEXTURE_BORDER_COLOR_QUIRK:
      return 0;
   case PIPE_CAP_MAX_TEXTURE_BUFFER_SIZE:
      /* a GEN6_SURFTYPE_BUFFER can have up to 2^27 elements */
      return 1 << 27;
   case PIPE_CAP_MAX_VIEWPORTS:
      return ILO_MAX_VIEWPORTS;
   case PIPE_CAP_ENDIANNESS:
      return PIPE_ENDIAN_LITTLE;
   case PIPE_CAP_MIXED_FRAMEBUFFER_SIZES:
      return true;
   case PIPE_CAP_TGSI_VS_LAYER_VIEWPORT:
   case PIPE_CAP_MAX_GEOMETRY_OUTPUT_VERTICES:
   case PIPE_CAP_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS:
   case PIPE_CAP_MAX_TEXTURE_GATHER_COMPONENTS:
   case PIPE_CAP_TEXTURE_GATHER_SM5:
      return 0;
   case PIPE_CAP_BUFFER_MAP_PERSISTENT_COHERENT:
      return true;
   case PIPE_CAP_FAKE_SW_MSAA:
   case PIPE_CAP_TEXTURE_QUERY_LOD:
   case PIPE_CAP_SAMPLE_SHADING:
   case PIPE_CAP_TEXTURE_GATHER_OFFSETS:
   case PIPE_CAP_TGSI_VS_WINDOW_SPACE_POSITION:
   case PIPE_CAP_MAX_VERTEX_STREAMS:
   case PIPE_CAP_DRAW_INDIRECT:
   case PIPE_CAP_TGSI_FS_FINE_DERIVATIVE:
   case PIPE_CAP_CONDITIONAL_RENDER_INVERTED:
   case PIPE_CAP_SAMPLER_VIEW_TARGET:
      return 0;

   case PIPE_CAP_VENDOR_ID:
      return 0x8086;
   case PIPE_CAP_DEVICE_ID:
      return is->dev.devid;
   case PIPE_CAP_ACCELERATED:
      return true;
   case PIPE_CAP_VIDEO_MEMORY: {
      /* Once a batch uses more than 75% of the maximum mappable size, we
       * assume that there's some fragmentation, and we start doing extra
       * flushing, etc.  That's the big cliff apps will care about.
       */
      const uint64_t gpu_memory = is->dev.aperture_total * 3 / 4;
      uint64_t system_memory;

      if (!os_get_total_physical_memory(&system_memory))
         return 0;

      return (int) (MIN2(gpu_memory, system_memory) >> 20);
   }
   case PIPE_CAP_UMA:
      return true;

   default:
      return 0;
   }
}

static const char *
ilo_get_vendor(struct pipe_screen *screen)
{
   return "LunarG, Inc.";
}

static const char *
ilo_get_name(struct pipe_screen *screen)
{
   struct ilo_screen *is = ilo_screen(screen);
   const char *chipset = NULL;

   if (gen_is_vlv(is->dev.devid)) {
      chipset = "Intel(R) Bay Trail";
   }
   else if (gen_is_hsw(is->dev.devid)) {
      if (gen_is_desktop(is->dev.devid))
         chipset = "Intel(R) Haswell Desktop";
      else if (gen_is_mobile(is->dev.devid))
         chipset = "Intel(R) Haswell Mobile";
      else if (gen_is_server(is->dev.devid))
         chipset = "Intel(R) Haswell Server";
   }
   else if (gen_is_ivb(is->dev.devid)) {
      if (gen_is_desktop(is->dev.devid))
         chipset = "Intel(R) Ivybridge Desktop";
      else if (gen_is_mobile(is->dev.devid))
         chipset = "Intel(R) Ivybridge Mobile";
      else if (gen_is_server(is->dev.devid))
         chipset = "Intel(R) Ivybridge Server";
   }
   else if (gen_is_snb(is->dev.devid)) {
      if (gen_is_desktop(is->dev.devid))
         chipset = "Intel(R) Sandybridge Desktop";
      else if (gen_is_mobile(is->dev.devid))
         chipset = "Intel(R) Sandybridge Mobile";
      else if (gen_is_server(is->dev.devid))
         chipset = "Intel(R) Sandybridge Server";
   }

   if (!chipset)
      chipset = "Unknown Intel Chipset";

   return chipset;
}

static uint64_t
ilo_get_timestamp(struct pipe_screen *screen)
{
   struct ilo_screen *is = ilo_screen(screen);
   union {
      uint64_t val;
      uint32_t dw[2];
   } timestamp;

   intel_winsys_read_reg(is->winsys, GEN6_REG_TIMESTAMP, &timestamp.val);

   /*
    * From the Ivy Bridge PRM, volume 1 part 3, page 107:
    *
    *     "Note: This timestamp register reflects the value of the PCU TSC.
    *      The PCU TSC counts 10ns increments; this timestamp reflects bits
    *      38:3 of the TSC (i.e. 80ns granularity, rolling over every 1.5
    *      hours)."
    *
    * However, it seems dw[0] is garbage and dw[1] contains the lower 32 bits
    * of the timestamp.  We will have to live with a timestamp that rolls over
    * every ~343 seconds.
    *
    * See also brw_get_timestamp().
    */
   return (uint64_t) timestamp.dw[1] * 80;
}

static void
ilo_fence_reference(struct pipe_screen *screen,
                    struct pipe_fence_handle **p,
                    struct pipe_fence_handle *f)
{
   struct ilo_fence *fence = ilo_fence(f);
   struct ilo_fence *old;

   if (likely(p)) {
      old = ilo_fence(*p);
      *p = f;
   }
   else {
      old = NULL;
   }

   STATIC_ASSERT(&((struct ilo_fence *) NULL)->reference == NULL);
   if (pipe_reference(&old->reference, &fence->reference)) {
      if (old->bo)
         intel_bo_unreference(old->bo);
      FREE(old);
   }
}

static boolean
ilo_fence_signalled(struct pipe_screen *screen,
                    struct pipe_fence_handle *f)
{
   struct ilo_fence *fence = ilo_fence(f);

   /* mark signalled if the bo is idle */
   if (fence->bo && !intel_bo_is_busy(fence->bo)) {
      intel_bo_unreference(fence->bo);
      fence->bo = NULL;
   }

   return (fence->bo == NULL);
}

static boolean
ilo_fence_finish(struct pipe_screen *screen,
                 struct pipe_fence_handle *f,
                 uint64_t timeout)
{
   struct ilo_fence *fence = ilo_fence(f);
   const int64_t wait_timeout = (timeout > INT64_MAX) ? -1 : timeout;

   /* already signalled */
   if (!fence->bo)
      return true;

   /* wait and see if it returns error */
   if (intel_bo_wait(fence->bo, wait_timeout))
      return false;

   /* mark signalled */
   intel_bo_unreference(fence->bo);
   fence->bo = NULL;

   return true;
}

/**
 * Create a fence for \p bo.  When \p bo is not NULL, it must be submitted
 * before waited on or checked.
 */
struct ilo_fence *
ilo_fence_create(struct pipe_screen *screen, struct intel_bo *bo)
{
   struct ilo_fence *fence;

   fence = CALLOC_STRUCT(ilo_fence);
   if (!fence)
      return NULL;

   pipe_reference_init(&fence->reference, 1);

   if (bo)
      intel_bo_reference(bo);
   fence->bo = bo;

   return fence;
}

static void
ilo_screen_destroy(struct pipe_screen *screen)
{
   struct ilo_screen *is = ilo_screen(screen);

   /* as it seems, winsys is owned by the screen */
   intel_winsys_destroy(is->winsys);

   FREE(is);
}

static bool
init_dev(struct ilo_dev_info *dev, const struct intel_winsys_info *info)
{
   dev->devid = info->devid;
   dev->aperture_total = info->aperture_total;
   dev->aperture_mappable = info->aperture_mappable;
   dev->has_llc = info->has_llc;
   dev->has_address_swizzling = info->has_address_swizzling;
   dev->has_logical_context = info->has_logical_context;
   dev->has_ppgtt = info->has_ppgtt;
   dev->has_timestamp = info->has_timestamp;
   dev->has_gen7_sol_reset = info->has_gen7_sol_reset;

   if (!dev->has_logical_context) {
      ilo_err("missing hardware logical context support\n");
      return false;
   }

   /*
    * PIPE_CONTROL and MI_* use PPGTT writes on GEN7+ and privileged GGTT
    * writes on GEN6.
    *
    * From the Sandy Bridge PRM, volume 1 part 3, page 101:
    *
    *     "[DevSNB] When Per-Process GTT Enable is set, it is assumed that all
    *      code is in a secure environment, independent of address space.
    *      Under this condition, this bit only specifies the address space
    *      (GGTT or PPGTT). All commands are executed "as-is""
    *
    * We need PPGTT to be enabled on GEN6 too.
    */
   if (!dev->has_ppgtt) {
      /* experiments show that it does not really matter... */
      ilo_warn("PPGTT disabled\n");
   }

   /*
    * From the Sandy Bridge PRM, volume 4 part 2, page 18:
    *
    *     "[DevSNB]: The GT1 product's URB provides 32KB of storage, arranged
    *      as 1024 256-bit rows. The GT2 product's URB provides 64KB of
    *      storage, arranged as 2048 256-bit rows. A row corresponds in size
    *      to an EU GRF register. Read/write access to the URB is generally
    *      supported on a row-granular basis."
    *
    * From the Ivy Bridge PRM, volume 4 part 2, page 17:
    *
    *     "URB Size    URB Rows    URB Rows when SLM Enabled
    *      128k        4096        2048
    *      256k        8096        4096"
    */

   if (gen_is_hsw(info->devid)) {
      dev->gen_opaque = ILO_GEN(7.5);
      dev->gt = gen_get_hsw_gt(info->devid);
      dev->urb_size = ((dev->gt == 3) ? 512 :
                       (dev->gt == 2) ? 256 : 128) * 1024;
   }
   else if (gen_is_ivb(info->devid) || gen_is_vlv(info->devid)) {
      dev->gen_opaque = ILO_GEN(7);
      dev->gt = (gen_is_ivb(info->devid)) ? gen_get_ivb_gt(info->devid) : 1;
      dev->urb_size = ((dev->gt == 2) ? 256 : 128) * 1024;
   }
   else if (gen_is_snb(info->devid)) {
      dev->gen_opaque = ILO_GEN(6);
      dev->gt = gen_get_snb_gt(info->devid);
      dev->urb_size = ((dev->gt == 2) ? 64 : 32) * 1024;
   }
   else {
      ilo_err("unknown GPU generation\n");
      return false;
   }

   return true;
}

struct pipe_screen *
ilo_screen_create(struct intel_winsys *ws)
{
   struct ilo_screen *is;
   const struct intel_winsys_info *info;

   ilo_debug = debug_get_flags_option("ILO_DEBUG", ilo_debug_flags, 0);

   is = CALLOC_STRUCT(ilo_screen);
   if (!is)
      return NULL;

   is->winsys = ws;

   info = intel_winsys_get_info(is->winsys);
   if (!init_dev(&is->dev, info)) {
      FREE(is);
      return NULL;
   }

   util_format_s3tc_init();

   is->base.destroy = ilo_screen_destroy;
   is->base.get_name = ilo_get_name;
   is->base.get_vendor = ilo_get_vendor;
   is->base.get_param = ilo_get_param;
   is->base.get_paramf = ilo_get_paramf;
   is->base.get_shader_param = ilo_get_shader_param;
   is->base.get_video_param = ilo_get_video_param;
   is->base.get_compute_param = ilo_get_compute_param;

   is->base.get_timestamp = ilo_get_timestamp;

   is->base.flush_frontbuffer = NULL;

   is->base.fence_reference = ilo_fence_reference;
   is->base.fence_signalled = ilo_fence_signalled;
   is->base.fence_finish = ilo_fence_finish;

   is->base.get_driver_query_info = NULL;

   ilo_init_format_functions(is);
   ilo_init_context_functions(is);
   ilo_init_resource_functions(is);

   return &is->base;
}
