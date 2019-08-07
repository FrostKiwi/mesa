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

#include "d3d12_screen.h"

#include "d3d12_context.h"
#include "d3d12_format.h"
#include "d3d12_public.h"
#include "d3d12_resource.h"

#include "util/u_math.h"
#include "util/u_memory.h"
#include "util/u_screen.h"

#include <dxgi1_4.h>

static const char *
d3d12_get_vendor(struct pipe_screen *pscreen)
{
   return "Collabora Ltd";
}

static const char *
d3d12_get_device_vendor(struct pipe_screen *pscreen)
{
   return "Unknown";
}

static const char *
d3d12_get_name(struct pipe_screen *pscreen)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);
   static char buf[1000];
   snprintf(buf, sizeof(buf), "d3d12 (%s)", "unknown");
   return buf;
}

static int
d3d12_get_param(struct pipe_screen *pscreen, enum pipe_cap param)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);

   switch (param) {
   case PIPE_CAP_NPOT_TEXTURES:
      return 1;

   case PIPE_CAP_MAX_RENDER_TARGETS:
      if (screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0)
         return 8;
      else if (screen->max_feature_level == D3D_FEATURE_LEVEL_9_3)
         return 4;
      return 1;

#if 0 /* TODO: enable me */
   case PIPE_CAP_TEXTURE_SWIZZLE:
      return 1;
#endif

   case PIPE_CAP_MAX_TEXTURE_2D_SIZE:
      if (screen->max_feature_level >= D3D_FEATURE_LEVEL_11_0)
         return 16384;
      else if (screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0)
         return 8192;
      else if (screen->max_feature_level >= D3D_FEATURE_LEVEL_9_3)
         return 4096;
      return 2048;

   case PIPE_CAP_MAX_TEXTURE_3D_LEVELS:
      if (screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0)
         return 11;
      return 9;

   case PIPE_CAP_MAX_TEXTURE_CUBE_LEVELS:
      if (screen->max_feature_level >= D3D_FEATURE_LEVEL_11_0)
         return 14;
      else if (screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0)
         return 13;
      else if (screen->max_feature_level == D3D_FEATURE_LEVEL_9_3)
         return 12;
      return 9;

   case PIPE_CAP_FRAGMENT_SHADER_TEXTURE_LOD:
   case PIPE_CAP_FRAGMENT_SHADER_DERIVATIVES:
   case PIPE_CAP_VERTEX_SHADER_SATURATE:
      return 1;

   case PIPE_CAP_MAX_TEXTURE_ARRAY_LAYERS:
      if (screen->max_feature_level >= D3D_FEATURE_LEVEL_11_0)
         return 1 << 14;
      else if (screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0)
         return 1 << 13;
      return 0;

#if 0 /* TODO: Enable me */
   case PIPE_CAP_DEPTH_CLIP_DISABLE:
      return 1;
#endif

#if 0 /* TODO: Enable me */
   case PIPE_CAP_TGSI_INSTANCEID:
      return 1;
#endif

   case PIPE_CAP_MIXED_COLORBUFFER_FORMATS:
      return 1;

   case PIPE_CAP_GLSL_FEATURE_LEVEL:
   case PIPE_CAP_GLSL_FEATURE_LEVEL_COMPATIBILITY:
      return 450; /* unsure (probably wrong) */

#if 0 /* TODO: Enable me */
   case PIPE_CAP_COMPUTE:
      return 0;
#endif

   case PIPE_CAP_CUBE_MAP_ARRAY:
      return screen->max_feature_level >= D3D_FEATURE_LEVEL_10_1;

#if 0 /* TODO: Enable me */
   case PIPE_CAP_TEXTURE_BUFFER_OBJECTS:
      return 1;
#endif

   case PIPE_CAP_PREFER_BLIT_BASED_TEXTURE_TRANSFER:
      return 0; /* unsure */

   case PIPE_CAP_ENDIANNESS:
      return PIPE_ENDIAN_NATIVE; /* unsure */

   case PIPE_CAP_MAX_VIEWPORTS:
      return 1; /* probably wrong */

#if 0 /* TODO: Enable me */
   case PIPE_CAP_MIXED_FRAMEBUFFER_SIZES:
      return 1;
#endif

#if 0 /* TODO: Enable me. Enables ARB_texture_gather */
   case PIPE_CAP_MAX_TEXTURE_GATHER_COMPONENTS:
      return 4;
#endif

   case PIPE_CAP_ACCELERATED:
      return 1;

   case PIPE_CAP_VIDEO_MEMORY:
      return 1 << 20; /* totally fake */

   case PIPE_CAP_UMA:
      return screen->architecture.UMA;

   case PIPE_CAP_MAX_VERTEX_ATTRIB_STRIDE:
      return 2048; /* FIXME: no clue how to query this */

#if 0 /* TODO: Enable me */
   case PIPE_CAP_SAMPLER_VIEW_TARGET:
      return 1;
#endif

   case PIPE_CAP_TEXTURE_FLOAT_LINEAR:
   case PIPE_CAP_TEXTURE_HALF_FLOAT_LINEAR:
      return 1;

   case PIPE_CAP_SHAREABLE_SHADERS:
      return 1;

#if 0 /* TODO: Enable me. Enables GL_ARB_shader_storage_buffer_object */
   case PIPE_CAP_SHADER_BUFFER_OFFSET_ALIGNMENT:
      return screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0;
#endif

   case PIPE_CAP_PCI_GROUP:
   case PIPE_CAP_PCI_BUS:
   case PIPE_CAP_PCI_DEVICE:
   case PIPE_CAP_PCI_FUNCTION:
      return 0; /* TODO: figure these out */

   case PIPE_CAP_GLSL_OPTIMIZE_CONSERVATIVELY:
      return 0; /* not sure */

   default:
      return u_pipe_screen_get_param_defaults(pscreen, param);
   }
}

static float
d3d12_get_paramf(struct pipe_screen *pscreen, enum pipe_capf param)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);

   switch (param) {
   case PIPE_CAPF_MAX_LINE_WIDTH:
   case PIPE_CAPF_MAX_LINE_WIDTH_AA:
      return 1.0f; /* no clue */

   case PIPE_CAPF_MAX_POINT_WIDTH:
   case PIPE_CAPF_MAX_POINT_WIDTH_AA:
      return 1.0f;

   case PIPE_CAPF_MAX_TEXTURE_ANISOTROPY:
      return screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0 ? 16.0f : 2.0f;

   case PIPE_CAPF_MAX_TEXTURE_LOD_BIAS:
      return 14.0f;

   case PIPE_CAPF_MIN_CONSERVATIVE_RASTER_DILATE:
   case PIPE_CAPF_MAX_CONSERVATIVE_RASTER_DILATE:
   case PIPE_CAPF_CONSERVATIVE_RASTER_DILATE_GRANULARITY:
      return 0.0f; /* not implemented */

   default:
      unreachable("unknown pipe_capf");
   }

   return 0.0;
}

static int
d3d12_get_shader_param(struct pipe_screen *pscreen,
                       enum pipe_shader_type shader,
                       enum pipe_shader_cap param)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);

   switch (param) {
   case PIPE_SHADER_CAP_MAX_INSTRUCTIONS:
   case PIPE_SHADER_CAP_MAX_ALU_INSTRUCTIONS:
   case PIPE_SHADER_CAP_MAX_TEX_INSTRUCTIONS:
   case PIPE_SHADER_CAP_MAX_TEX_INDIRECTIONS:
   case PIPE_SHADER_CAP_MAX_CONTROL_FLOW_DEPTH:
      if (shader == PIPE_SHADER_VERTEX ||
          shader == PIPE_SHADER_FRAGMENT)
         return INT_MAX;
      return 0;

   case PIPE_SHADER_CAP_MAX_INPUTS:
      return screen->max_feature_level >= D3D_FEATURE_LEVEL_10_1 ? 32 : 16;

   case PIPE_SHADER_CAP_MAX_OUTPUTS:
      if (shader == PIPE_SHADER_FRAGMENT) {
         /* same as max MRTs (not sure if this is correct) */
         if (screen->max_feature_level >= D3D_FEATURE_LEVEL_10_0)
            return 8;
         else if (screen->max_feature_level == D3D_FEATURE_LEVEL_9_3)
            return 4;
         return 1;
      }
      return screen->max_feature_level >= D3D_FEATURE_LEVEL_10_1 ? 32 : 16;

   case PIPE_SHADER_CAP_MAX_TEXTURE_SAMPLERS:
      return PIPE_MAX_SAMPLERS; /* unsure */

   case PIPE_SHADER_CAP_MAX_CONST_BUFFER_SIZE:
   case PIPE_SHADER_CAP_MAX_CONST_BUFFERS:
      return INT_MAX; /* unsure */

   case PIPE_SHADER_CAP_MAX_TEMPS:
      return INT_MAX;

   case PIPE_SHADER_CAP_INDIRECT_INPUT_ADDR:
   case PIPE_SHADER_CAP_INDIRECT_OUTPUT_ADDR:
   case PIPE_SHADER_CAP_INDIRECT_TEMP_ADDR:
   case PIPE_SHADER_CAP_INDIRECT_CONST_ADDR:
   case PIPE_SHADER_CAP_SUBROUTINES:
   case PIPE_SHADER_CAP_INTEGERS:
   case PIPE_SHADER_CAP_INT64_ATOMICS:
   case PIPE_SHADER_CAP_FP16:
      return 0; /* not implemented */

   case PIPE_SHADER_CAP_PREFERRED_IR:
      return PIPE_SHADER_IR_TGSI;

   case PIPE_SHADER_CAP_TGSI_SQRT_SUPPORTED:
      return 0; /* not implemented */

   case PIPE_SHADER_CAP_MAX_SAMPLER_VIEWS:
      return PIPE_MAX_SHADER_SAMPLER_VIEWS; /* unsure */

   case PIPE_SHADER_CAP_TGSI_DROUND_SUPPORTED:
   case PIPE_SHADER_CAP_TGSI_DFRACEXP_DLDEXP_SUPPORTED:
   case PIPE_SHADER_CAP_TGSI_FMA_SUPPORTED:
      return 0; /* not implemented */

   case PIPE_SHADER_CAP_TGSI_ANY_INOUT_DECL_RANGE:
      return 0; /* no idea */

   case PIPE_SHADER_CAP_MAX_UNROLL_ITERATIONS_HINT:
      return 32; /* arbitrary */

   case PIPE_SHADER_CAP_MAX_SHADER_BUFFERS:
      return 8; /* no clue */

   case PIPE_SHADER_CAP_SUPPORTED_IRS:
      return 1 << PIPE_SHADER_IR_TGSI;

   case PIPE_SHADER_CAP_MAX_SHADER_IMAGES:
      return 8; /* no clue */

   case PIPE_SHADER_CAP_LOWER_IF_THRESHOLD:
   case PIPE_SHADER_CAP_TGSI_SKIP_MERGE_REGISTERS:
      return 0; /* unsure */

   case PIPE_SHADER_CAP_TGSI_LDEXP_SUPPORTED:
   case PIPE_SHADER_CAP_MAX_HW_ATOMIC_COUNTERS:
   case PIPE_SHADER_CAP_MAX_HW_ATOMIC_COUNTER_BUFFERS:
   case PIPE_SHADER_CAP_TGSI_CONT_SUPPORTED:
      return 0; /* not implemented */
   }

   /* should only get here on unhandled cases */
   return 0;
}

static bool
d3d12_is_format_supported(struct pipe_screen *pscreen,
                          enum pipe_format format,
                          enum pipe_texture_target target,
                          unsigned sample_count,
                          unsigned storage_sample_count,
                          unsigned bind)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);

   if (sample_count > 1)
      return FALSE;

   DXGI_FORMAT dxgi_format = d3d12_get_format(format);
   if (dxgi_format == DXGI_FORMAT_UNKNOWN)
      return FALSE;

   D3D12_FEATURE_DATA_FORMAT_SUPPORT fmt_info;
   fmt_info.Format = dxgi_format;
   if (FAILED(screen->dev->CheckFeatureSupport(D3D12_FEATURE_FORMAT_SUPPORT,
                                               &fmt_info, sizeof(fmt_info))))
      return FALSE;

   if (target == PIPE_BUFFER) {
      if (bind & PIPE_BIND_VERTEX_BUFFER &&
          !(fmt_info.Support1 & D3D12_FORMAT_SUPPORT1_IA_VERTEX_BUFFER))
         return FALSE;
   } else {
      /* all other targets are texture-targets */
      if (bind & PIPE_BIND_RENDER_TARGET &&
          !(fmt_info.Support1 & D3D12_FORMAT_SUPPORT1_RENDER_TARGET))
         return FALSE;

      if (bind & PIPE_BIND_BLENDABLE &&
         !(fmt_info.Support1 & D3D12_FORMAT_SUPPORT1_BLENDABLE))
        return FALSE;

      if (bind & PIPE_BIND_SAMPLER_VIEW &&
          !(fmt_info.Support1 & D3D12_FORMAT_SUPPORT1_SHADER_SAMPLE))
         return FALSE;
   }

   return TRUE;
}

static void
d3d12_destroy_screen(struct pipe_screen *pscreen)
{
   struct d3d12_screen *screen = d3d12_screen(pscreen);
   slab_destroy_parent(&screen->transfer_pool);
   FREE(screen);
}

static void
d3d12_flush_frontbuffer(struct pipe_screen *screen,
                        struct pipe_resource *res,
                        unsigned level, unsigned layer,
                        void *winsys_drawable_handle,
                        struct pipe_box *sub_box)
{
}

static void
d3d12_fence_reference(struct pipe_screen *screen,
                      struct pipe_fence_handle **ptr,
                      struct pipe_fence_handle *fence)
{
#if 0
   if (pipe_reference(&(*ptr)->reference, &fence->reference)) {
      (*ptr)->fence->Release();
      free(*ptr);
   }
#endif
   *ptr = fence;
}


static IDXGIFactory4 *
get_dxgi_factory()
{
   static const GUID IID_IDXGIFactory4 = {
      0x1bc6ea02, 0xef36, 0x464f,
      { 0xbf, 0x0c, 0x21, 0xca, 0x39, 0xe5, 0x16, 0x8a }
   };

   typedef HRESULT(WINAPI *PFN_CREATE_DXGI_FACTORY)(REFIID riid, void **ppFactory);
   PFN_CREATE_DXGI_FACTORY CreateDXGIFactory;

   HMODULE hDXGIMod = LoadLibrary("DXGI.DLL");
   if (!hDXGIMod) {
      debug_printf("D3D12: failed to load DXGI.DLL\n");
      return NULL;
   }

   CreateDXGIFactory = (PFN_CREATE_DXGI_FACTORY)GetProcAddress(hDXGIMod, "CreateDXGIFactory");
   if (!CreateDXGIFactory) {
      debug_printf("D3D12: failed to load CreateDXGIFactory from DXGI.DLL\n");
      return NULL;
   }

   IDXGIFactory4 *factory = NULL;
   HRESULT hr = CreateDXGIFactory(IID_IDXGIFactory4, (void **)&factory);
   if (FAILED(hr)) {
      debug_printf("D3D12: CreateDXGIFactory failed: %08x\n", hr);
      return NULL;
   }

   return factory;
}

static IDXGIAdapter1 *
choose_adapter(IDXGIFactory4 *factory)
{
  IDXGIAdapter1 *adapter;
  for (UINT i = 0; factory->EnumAdapters1(i, &adapter) != DXGI_ERROR_NOT_FOUND; ++i)
  {
     DXGI_ADAPTER_DESC1 desc;
     adapter->GetDesc1(&desc);

     if (desc.Flags & DXGI_ADAPTER_FLAG_SOFTWARE)
        continue;

     return adapter;
  }

  return NULL;
}

static ID3D12Device *
create_device(IDXGIAdapter1 *adapter)
{
   typedef HRESULT(WINAPI *PFN_D3D12CREATEDEVICE)(IUnknown*, D3D_FEATURE_LEVEL, REFIID, void**);
   PFN_D3D12CREATEDEVICE D3D12CreateDevice;

   HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
   if (!hD3D12Mod) {
      debug_printf("D3D12: failed to load D3D12.DLL\n");
      return NULL;
   }

   D3D12CreateDevice = (PFN_D3D12CREATEDEVICE)GetProcAddress(hD3D12Mod, "D3D12CreateDevice");
   if (!D3D12CreateDevice) {
      debug_printf("D3D12: failed to load D3D12CreateDevice from D3D12.DLL\n");
      return NULL;
   }

   ID3D12Device *dev;
   if (SUCCEEDED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_11_0,
                 __uuidof(ID3D12Device), (void **)&dev)))
      return dev;

   debug_printf("D3D12: D3D12CreateDevice failed\n");
   return NULL;
}

struct pipe_screen *
d3d12_create_screen(struct sw_winsys *winsys)
{
   struct d3d12_screen *screen = CALLOC_STRUCT(d3d12_screen);
   if (!screen)
      return NULL;

   screen->winsys = winsys;

   screen->base.get_name = d3d12_get_name;
   screen->base.get_vendor = d3d12_get_vendor;
   screen->base.get_device_vendor = d3d12_get_device_vendor;
   screen->base.get_param = d3d12_get_param;
   screen->base.get_paramf = d3d12_get_paramf;
   screen->base.get_shader_param = d3d12_get_shader_param;
   screen->base.is_format_supported = d3d12_is_format_supported;
   screen->base.context_create = d3d12_context_create;
   screen->base.flush_frontbuffer = d3d12_flush_frontbuffer;
   screen->base.fence_reference = d3d12_fence_reference;
   screen->base.destroy = d3d12_destroy_screen;

   IDXGIFactory4 *factory = get_dxgi_factory();
   if (!factory) {
      debug_printf("D3D12: failed to create DXGI factory\n");
      goto failed;
   }

   screen->adapter = choose_adapter(factory);
   if (!screen->adapter) {
      debug_printf("D3D12: no suitable adapter\n");
      return NULL;
   }

   screen->dev = create_device(screen->adapter);
   if (!screen->dev) {
      debug_printf("D3D12: failed to create device\n");
      goto failed;
   }

   screen->architecture.NodeIndex = 0;
   if (FAILED(screen->dev->CheckFeatureSupport(D3D12_FEATURE_ARCHITECTURE,
                                               &screen->architecture,
                                               sizeof(screen->architecture)))) {
      debug_printf("D3D12: failed to get device architecture\n");
      goto failed;
   }

   D3D12_FEATURE_DATA_FEATURE_LEVELS feature_levels;
   static const D3D_FEATURE_LEVEL levels[] = {
      D3D_FEATURE_LEVEL_11_0,
      D3D_FEATURE_LEVEL_11_1,
      D3D_FEATURE_LEVEL_12_0,
      D3D_FEATURE_LEVEL_12_1,
   };
   feature_levels.NumFeatureLevels = ARRAY_SIZE(levels);
   feature_levels.pFeatureLevelsRequested = levels;
   if (FAILED(screen->dev->CheckFeatureSupport(D3D12_FEATURE_FEATURE_LEVELS,
                                               &feature_levels,
                                               sizeof(feature_levels)))) {
      debug_printf("D3D12: failed to get device feature levels\n");
      goto failed;
   }
   screen->max_feature_level = feature_levels.MaxSupportedFeatureLevel;

   d3d12_screen_resource_init(&screen->base);
   slab_create_parent(&screen->transfer_pool, sizeof(struct pipe_transfer), 16);

   return &screen->base;

failed:
   FREE(screen);
   return NULL;
}
