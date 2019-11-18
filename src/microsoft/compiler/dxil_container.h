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

#ifndef DXIL_CONTAINER_H
#define DXIL_CONTAINER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "dxil.h"

#include "util/blob.h"

#define DXIL_MAX_PARTS 8
struct dxil_container {
   struct blob parts;
   unsigned part_offsets[DXIL_MAX_PARTS];
   unsigned num_parts;
};

void
dxil_container_init(struct dxil_container *c);

bool
dxil_container_add_features(struct dxil_container *c,
                            const struct dxil_features *features);

bool
dxil_container_add_input_signature(struct dxil_container *c);

bool
dxil_container_add_output_signature(struct dxil_container *c);

bool
dxil_container_add_module(struct dxil_container *c,
                          const struct dxil_module *m);

bool
dxil_container_write(struct dxil_container *c, struct blob *blob);

#ifdef __cplusplus
}
#endif

#endif
