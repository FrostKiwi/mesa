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

#include "d3d12_format.h"

#include "pipe/p_format.h"

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

   MAP_FORMAT_INT(R32G32B32)
   MAP_FORMAT_FLOAT(R32G32B32)
   MAP_FORMAT_INT(R32G32B32A32)
   MAP_FORMAT_FLOAT(R32G32B32A32)

   MAP_FORMAT_FLOAT(R16G16B16A16)

   [PIPE_FORMAT_Z32_FLOAT] = DXGI_FORMAT_D32_FLOAT,
   [PIPE_FORMAT_Z16_UNORM] = DXGI_FORMAT_D16_UNORM,
   [PIPE_FORMAT_X8Z24_UNORM] = DXGI_FORMAT_D24_UNORM_S8_UINT,
};

DXGI_FORMAT
d3d12_get_format(enum pipe_format format)
{
   return formats[format];
}
