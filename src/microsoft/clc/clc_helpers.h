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

#ifndef CLC_TO_NIR_H
#define CLC_TO_NIR_H

#ifdef __cplusplus
extern "C" {
#endif

#include "clc_compiler.h"

#include <stddef.h>
#include <stdio.h>
#include <stdint.h>

int
clc_to_spirv(const struct clc_compile_args *args,
             struct spirv_binary *spvbin,
             char **err_buf);

int
clc_link_spirv_binaries(const struct spirv_binary **in_bins,
                        unsigned num_in_bins,
                        struct spirv_binary *dst_bin,
                        char **err_buf);

void
clc_dump_spirv(const struct spirv_binary *spvbin, FILE *f);

void
clc_free_spirv_binary(struct spirv_binary *spvbin);

#ifdef __cplusplus
}
#endif

#endif
