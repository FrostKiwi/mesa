/*
 * Copyright © 2015 Intel Corporation
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

#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "brw_device_info.h"

#ifdef __cplusplus
extern "C" {
#endif

enum brw_image_format_data_type {
   BRW_IMAGE_FORMAT_UNORM,
   BRW_IMAGE_FORMAT_SNORM,
   BRW_IMAGE_FORMAT_UINT,
   BRW_IMAGE_FORMAT_SINT,
   BRW_IMAGE_FORMAT_FLOAT,
};

struct brw_image_format_info {
   bool is_image_format;
   uint8_t red_bits;
   uint8_t green_bits;
   uint8_t blue_bits;
   uint8_t alpha_bits;
   enum brw_image_format_data_type data_type;
};

extern const struct brw_image_format_info brw_image_format_info[];

uint32_t brw_image_format_for_gl_format(uint32_t gl_format);

uint32_t brw_lower_image_format(const struct brw_device_info *devinfo,
                                uint32_t format);
#ifdef __cplusplus
}
#endif
