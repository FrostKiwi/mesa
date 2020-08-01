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

#include "d3d12_format.h"

#include "pipe/p_format.h"
#include "util/format/u_format.h"
#include "util/u_math.h"

static const DXGI_FORMAT formats[PIPE_FORMAT_COUNT] = {
#define MAP_FORMAT_NORM(FMT) \
   [PIPE_FORMAT_ ## FMT ## _UNORM] = DXGI_FORMAT_ ## FMT ## _UNORM, \
   [PIPE_FORMAT_ ## FMT ## _SNORM] = DXGI_FORMAT_ ## FMT ## _SNORM,

#define MAP_FORMAT_INT(FMT) \
   [PIPE_FORMAT_ ## FMT ## _UINT] = DXGI_FORMAT_ ## FMT ## _UINT, \
   [PIPE_FORMAT_ ## FMT ## _SINT] = DXGI_FORMAT_ ## FMT ## _SINT,

#define MAP_FORMAT_SRGB(FMT) \
   [PIPE_FORMAT_ ## FMT ## _SRGB] = DXGI_FORMAT_ ## FMT ## _UNORM_SRGB,

#define MAP_FORMAT_FLOAT(FMT) \
   [PIPE_FORMAT_ ## FMT ## _FLOAT] = DXGI_FORMAT_ ## FMT ## _FLOAT,

   MAP_FORMAT_NORM(R8)
   MAP_FORMAT_INT(R8)

   MAP_FORMAT_NORM(R8G8)
   MAP_FORMAT_INT(R8G8)

   MAP_FORMAT_NORM(R8G8B8A8)
   MAP_FORMAT_INT(R8G8B8A8)
   MAP_FORMAT_SRGB(R8G8B8A8)

   [PIPE_FORMAT_B8G8R8X8_UNORM] = DXGI_FORMAT_B8G8R8X8_UNORM,
   [PIPE_FORMAT_B8G8R8A8_UNORM] = DXGI_FORMAT_B8G8R8A8_UNORM,

   MAP_FORMAT_SRGB(B8G8R8A8)

   MAP_FORMAT_INT(R32)
   MAP_FORMAT_FLOAT(R32)
   MAP_FORMAT_INT(R32G32)
   MAP_FORMAT_FLOAT(R32G32)
   MAP_FORMAT_INT(R32G32B32)
   MAP_FORMAT_FLOAT(R32G32B32)
   MAP_FORMAT_INT(R32G32B32A32)
   MAP_FORMAT_FLOAT(R32G32B32A32)

   MAP_FORMAT_NORM(R16)
   MAP_FORMAT_INT(R16)
   MAP_FORMAT_FLOAT(R16)

   MAP_FORMAT_NORM(R16G16)
   MAP_FORMAT_INT(R16G16)
   MAP_FORMAT_FLOAT(R16G16)

   MAP_FORMAT_NORM(R16G16B16A16)
   MAP_FORMAT_INT(R16G16B16A16)
   MAP_FORMAT_FLOAT(R16G16B16A16)

   [PIPE_FORMAT_R9G9B9E5_FLOAT] = DXGI_FORMAT_R9G9B9E5_SHAREDEXP,
   [PIPE_FORMAT_R11G11B10_FLOAT] = DXGI_FORMAT_R11G11B10_FLOAT,
   [PIPE_FORMAT_R10G10B10A2_UINT] = DXGI_FORMAT_R10G10B10A2_UINT,
   [PIPE_FORMAT_R10G10B10A2_UNORM] = DXGI_FORMAT_R10G10B10A2_UNORM,

   [PIPE_FORMAT_RGTC1_UNORM] = DXGI_FORMAT_BC4_UNORM,
   [PIPE_FORMAT_RGTC1_SNORM] = DXGI_FORMAT_BC4_SNORM,
   [PIPE_FORMAT_RGTC2_UNORM] = DXGI_FORMAT_BC5_UNORM,
   [PIPE_FORMAT_RGTC2_SNORM] = DXGI_FORMAT_BC5_SNORM,

   [PIPE_FORMAT_Z32_FLOAT] = DXGI_FORMAT_R32_TYPELESS,
   [PIPE_FORMAT_Z16_UNORM] = DXGI_FORMAT_R16_TYPELESS,
   [PIPE_FORMAT_Z24X8_UNORM] = DXGI_FORMAT_R24G8_TYPELESS,
   [PIPE_FORMAT_X24S8_UINT] = DXGI_FORMAT_R24G8_TYPELESS,

   [PIPE_FORMAT_Z24_UNORM_S8_UINT] = DXGI_FORMAT_R24G8_TYPELESS,
   [PIPE_FORMAT_Z32_FLOAT_S8X24_UINT] = DXGI_FORMAT_R32G8X24_TYPELESS,
   [PIPE_FORMAT_X32_S8X24_UINT] = DXGI_FORMAT_R32G8X24_TYPELESS,
};

DXGI_FORMAT
d3d12_get_format(enum pipe_format format)
{
   return formats[format];
}

DXGI_FORMAT
d3d12_get_resource_rt_format(enum pipe_format f)
{
   switch (f) {
   case PIPE_FORMAT_Z16_UNORM:
      return DXGI_FORMAT_D16_UNORM;
   case PIPE_FORMAT_Z32_FLOAT:
      return DXGI_FORMAT_D32_FLOAT;
   case PIPE_FORMAT_Z24X8_UNORM:
   case PIPE_FORMAT_X24S8_UINT:
      return DXGI_FORMAT_D24_UNORM_S8_UINT;
   case PIPE_FORMAT_Z32_FLOAT_S8X24_UINT:
   case PIPE_FORMAT_X32_S8X24_UINT:
      return DXGI_FORMAT_D32_FLOAT_S8X24_UINT;
   case PIPE_FORMAT_Z24_UNORM_S8_UINT:
      return DXGI_FORMAT_D24_UNORM_S8_UINT;
   default:
      return d3d12_get_format(f);
   }
}

DXGI_FORMAT
d3d12_get_resource_srv_format(enum pipe_format f)
{
   switch (f) {
   case PIPE_FORMAT_Z16_UNORM:
      return DXGI_FORMAT_R16_UNORM;
   case PIPE_FORMAT_Z32_FLOAT:
      return DXGI_FORMAT_R32_FLOAT;
   case PIPE_FORMAT_Z24X8_UNORM:
   case PIPE_FORMAT_Z24_UNORM_S8_UINT:
      return DXGI_FORMAT_R24_UNORM_X8_TYPELESS;
   case PIPE_FORMAT_X24S8_UINT:
      return DXGI_FORMAT_X24_TYPELESS_G8_UINT;
   case PIPE_FORMAT_Z32_FLOAT_S8X24_UINT:
      return DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS;
   case PIPE_FORMAT_X32_S8X24_UINT:
      return DXGI_FORMAT_X32_TYPELESS_G8X24_UINT;
   default:
      return d3d12_get_format(f);
   }
}

#define EMU_ALPHA_SWIZZLE { PIPE_SWIZZLE_0, PIPE_SWIZZLE_0, PIPE_SWIZZLE_0, PIPE_SWIZZLE_X }
#define EMU_LUMINANCE_SWIZZLE { PIPE_SWIZZLE_X, PIPE_SWIZZLE_X, PIPE_SWIZZLE_X, PIPE_SWIZZLE_1 }
#define EMU_LUMINANCE_ALPHA_SWIZZLE { PIPE_SWIZZLE_X, PIPE_SWIZZLE_X, PIPE_SWIZZLE_X, PIPE_SWIZZLE_Y }
#define EMU_INTENSITY_SWIZZLE { PIPE_SWIZZLE_X, PIPE_SWIZZLE_X, PIPE_SWIZZLE_X, PIPE_SWIZZLE_X }

#define EMU_FORMAT(BITS, TYPE) \
   [PIPE_FORMAT_A ## BITS ## _ ## TYPE] = {DXGI_FORMAT_R ## BITS ## _ ## TYPE, EMU_ALPHA_SWIZZLE}, \
   [PIPE_FORMAT_L ## BITS ## _ ## TYPE] = {DXGI_FORMAT_R ## BITS ## _ ## TYPE, EMU_LUMINANCE_SWIZZLE}, \
   [PIPE_FORMAT_I ## BITS ## _ ## TYPE] = {DXGI_FORMAT_R ## BITS ## _ ## TYPE, EMU_INTENSITY_SWIZZLE}, \
   [PIPE_FORMAT_L ## BITS ## A ## BITS ## _ ## TYPE] = \
          {DXGI_FORMAT_R ## BITS ## G ## BITS ## _ ## TYPE, EMU_LUMINANCE_ALPHA_SWIZZLE}

const struct d3d12_arb_emulation_format arb_emulation_format[PIPE_FORMAT_COUNT] = {
   EMU_FORMAT(8, UNORM),
   EMU_FORMAT(8, SINT),
   EMU_FORMAT(8, UINT),
   EMU_FORMAT(16, UNORM),
   EMU_FORMAT(16, SINT),
   EMU_FORMAT(16, UINT),
   EMU_FORMAT(16, FLOAT),
   EMU_FORMAT(32, SINT),
   EMU_FORMAT(32, UINT),
   EMU_FORMAT(32, FLOAT)
};

const struct d3d12_arb_emulation_format *
d3d12_get_emulated_view_format(enum pipe_format format)
{
   return &arb_emulation_format[format];
}

enum pipe_format
d3d12_emulated_vtx_format(enum pipe_format fmt)
{
   switch (fmt) {
   case PIPE_FORMAT_R10G10B10A2_SNORM:
   case PIPE_FORMAT_R10G10B10A2_SSCALED:
   case PIPE_FORMAT_R10G10B10A2_USCALED:
   case PIPE_FORMAT_B10G10R10A2_UNORM:
   case PIPE_FORMAT_B10G10R10A2_SNORM:
   case PIPE_FORMAT_B10G10R10A2_SSCALED:
   case PIPE_FORMAT_B10G10R10A2_USCALED:
      return PIPE_FORMAT_R32_UINT;

   case PIPE_FORMAT_R8G8B8_SINT:
      return PIPE_FORMAT_R8G8B8A8_SINT;
   case PIPE_FORMAT_R8G8B8_UINT:
      return PIPE_FORMAT_R8G8B8A8_UINT;

   case PIPE_FORMAT_R16G16B16_SINT:
      return PIPE_FORMAT_R16G16B16A16_SINT;
   case PIPE_FORMAT_R16G16B16_UINT:
      return PIPE_FORMAT_R16G16B16A16_UINT;

   default:
      return fmt;
   }
}


unsigned
d3d12_non_opaque_plane_count(DXGI_FORMAT format)
{
   switch (format) {
   case DXGI_FORMAT_V208:
   case DXGI_FORMAT_V408:
      return 3;

   case DXGI_FORMAT_NV12:
   case DXGI_FORMAT_P010:
   case DXGI_FORMAT_P016:
   case DXGI_FORMAT_YUY2:
   case DXGI_FORMAT_Y210:
   case DXGI_FORMAT_Y216:
   case DXGI_FORMAT_NV11:
      return 2;
   }

   return 1;
}

unsigned
d3d12_get_format_num_planes(enum pipe_format fmt)
{
   return util_format_is_depth_or_stencil(fmt) ?
      util_bitcount(util_format_get_mask(fmt)) : 1;
}
