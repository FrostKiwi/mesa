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

#ifndef D3D12_FORMATS_H
#define D3D12_FORMATS_H

#include <dxgiformat.h>

#include "pipe/p_format.h"
#include "pipe/p_defines.h"

#ifdef __cplusplus
extern "C" {
#endif

DXGI_FORMAT
d3d12_get_format(enum pipe_format format);

DXGI_FORMAT
d3d12_get_resource_base_format(DXGI_FORMAT f);

DXGI_FORMAT
d3d12_get_sampler_format_for_ds(DXGI_FORMAT f);

unsigned
d3d12_non_opaque_plane_count(DXGI_FORMAT f);

struct d3d12_arb_emulation_format {
   DXGI_FORMAT dxgi_format;
   enum pipe_swizzle swizzle[4];
};

const struct d3d12_arb_emulation_format *
d3d12_get_emulated_view_format(enum pipe_format format);

#ifdef __cplusplus
}
#endif

#endif
