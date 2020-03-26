/*
 * Copyright © Microsoft Corporation
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

#ifndef D3D12_NIR_PASSES_H
#define D3D12_NIR_PASSES_H

#include "nir.h"

#ifdef __cplusplus
extern "C" {
#endif

struct d3d12_shader;

bool
d3d12_lower_bool_loads(struct nir_shader *s);

bool
d3d12_lower_point_sprite(nir_shader *shader,
                         bool sprite_origin_lower_left,
                         unsigned point_coord_enable);

bool
d3d12_lower_state_vars(struct nir_shader *s, struct d3d12_shader *shader);

void
d3d12_lower_yflip(nir_shader *s);

#ifdef __cplusplus
}
#endif

#endif // D3D12_NIR_PASSES_H
