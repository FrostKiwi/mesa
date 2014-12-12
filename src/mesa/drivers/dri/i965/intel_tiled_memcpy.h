
/**************************************************************************
 *
 * Copyright 2003 VMware, Inc.
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sub license, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL VMWARE AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 **************************************************************************/

#ifndef INTELTILEDMEMCPY_INC
#define INTELTILEDMEMCPY_INC


#include "main/bufferobj.h"
#include "main/image.h"
#include "main/macros.h"
#include "main/mtypes.h"
#include "main/pbo.h"
#include "main/texobj.h"
#include "main/texstore.h"
#include "main/texcompress.h"
#include "main/enums.h"

#include "brw_context.h"
#include "intel_batchbuffer.h"
#include "intel_tex.h"
#include "intel_pixel.h"
#include "intel_mipmap_tree.h"
#include "intel_blit.h"
#include "intel_fbo.h"

#ifdef __SSSE3__
#include <tmmintrin.h>
#endif

#define FILE_DEBUG_FLAG DEBUG_TEXTURE

#define ALIGN_DOWN(a, b) ROUND_DOWN_TO(a, b)
#define ALIGN_UP(a, b) ALIGN(a, b)

/* Tile dimensions.
 * Width and span are in bytes, height is in pixels (i.e. unitless).
 * A "span" is the most number of bytes we can copy from linear to tiled
 * without needing to calculate a new destination address.
 */
static const uint32_t xtile_width = 512;
static const uint32_t xtile_height = 8;
static const uint32_t xtile_span = 64;
static const uint32_t ytile_width = 128;
static const uint32_t ytile_height = 32;
static const uint32_t ytile_span = 16;

typedef void *(*mem_copy_fn)(void *dest, const void *src, size_t n);

/**
 * Each row from y0 to y1 is copied in three parts: [x0,x1), [x1,x2), [x2,x3).
 * These ranges are in bytes, i.e. pixels * bytes-per-pixel.
 * The first and last ranges must be shorter than a "span" (the longest linear
 * stretch within a tile) and the middle must equal a whole number of spans.
 * Ranges may be empty.  The region copied must land entirely within one tile.
 * 'dst' is the start of the tile and 'src' is the corresponding
 * address to copy from, though copying begins at (x0, y0).
 * To enable swizzling 'swizzle_bit' must be 1<<6, otherwise zero.
 * Swizzling flips bit 6 in the copy destination offset, when certain other
 * bits are set in it.
 */
typedef void (*tile_copy_fn)(uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
                             uint32_t y0, uint32_t y1,
                             char *dst, const char *src,
                             uint32_t src_pitch,
                             uint32_t swizzle_bit,
                             mem_copy_fn mem_copy);

#ifdef __SSSE3__
static const uint8_t rgba8_permutation[16] =
   { 2,1,0,3, 6,5,4,7, 10,9,8,11, 14,13,12,15 };

/* NOTE: dst must be 16 byte aligned */
#define rgba8_copy_16(dst, src)                     \
   *(__m128i *)(dst) = _mm_shuffle_epi8(            \
      (__m128i) _mm_loadu_ps((float *)(src)),       \
      *(__m128i *) rgba8_permutation                \
   )
#endif

void
linear_to_tiled(uint32_t xt1, uint32_t xt2,
                uint32_t yt1, uint32_t yt2,
                char *dst, const char *src,
                uint32_t dst_pitch, uint32_t src_pitch,
                bool has_swizzling,
                uint32_t tiling,
                mem_copy_fn mem_copy);

bool intel_get_memcpy( mesa_format tiledFormat, GLenum format,
                        GLenum type, mem_copy_fn* mem_copy, uint32_t* cpp);

#endif
