
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

#include "intel_tiled_memcpy.h"

/**
 * Copy RGBA to BGRA - swap R and B.
 */
static inline void *
rgba8_copy(void *dst, const void *src, size_t bytes)
{
   uint8_t *d = dst;
   uint8_t const *s = src;

#ifdef __SSSE3__
   /* Fast copying for tile spans.
    *
    * As long as the destination texture is 16 aligned,
    * any 16 or 64 spans we get here should also be 16 aligned.
    */

   if (bytes == 16) {
      assert(!(((uintptr_t)dst) & 0xf));
      rgba8_copy_16(d+ 0, s+ 0);
      return dst;
   }

   if (bytes == 64) {
      assert(!(((uintptr_t)dst) & 0xf));
      rgba8_copy_16(d+ 0, s+ 0);
      rgba8_copy_16(d+16, s+16);
      rgba8_copy_16(d+32, s+32);
      rgba8_copy_16(d+48, s+48);
      return dst;
   }
#endif

   while (bytes >= 4) {
      d[0] = s[2];
      d[1] = s[1];
      d[2] = s[0];
      d[3] = s[3];
      d += 4;
      s += 4;
      bytes -= 4;
   }
   return dst;
}

/**
 * Copy texture data from linear to X tile layout.
 *
 * \copydoc tile_copy_fn
 */
static inline void
linear_to_xtiled(uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
           uint32_t y0, uint32_t y1,
           char *dst, const char *src,
           uint32_t src_pitch,
           uint32_t swizzle_bit,
           mem_copy_fn mem_copy)
{
   /* The copy destination offset for each range copied is the sum of
    * an X offset 'x0' or 'xo' and a Y offset 'yo.'
    */
   uint32_t xo, yo;

   src += y0 * src_pitch;

   for (yo = y0 * xtile_width; yo < y1 * xtile_width; yo += xtile_width) {
      /* Bits 9 and 10 of the copy destination offset control swizzling.
       * Only 'yo' contributes to those bits in the total offset,
       * so calculate 'swizzle' just once per row.
       * Move bits 9 and 10 three and four places respectively down
       * to bit 6 and xor them.
       */
      uint32_t swizzle = ((yo >> 3) ^ (yo >> 4)) & swizzle_bit;

      mem_copy(dst + ((x0 + yo) ^ swizzle), src + x0, x1 - x0);

      for (xo = x1; xo < x2; xo += xtile_span) {
         mem_copy(dst + ((xo + yo) ^ swizzle), src + xo, xtile_span);
      }

      mem_copy(dst + ((xo + yo) ^ swizzle), src + x2, x3 - x2);

      src += src_pitch;
   }
}

/**
 * Copy texture data from linear to Y tile layout.
 *
 * \copydoc tile_copy_fn
 */
static inline void
linear_to_ytiled(
   uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
   uint32_t y0, uint32_t y1,
   char *dst, const char *src,
   uint32_t src_pitch,
   uint32_t swizzle_bit,
   mem_copy_fn mem_copy)
{
   /* Y tiles consist of columns that are 'ytile_span' wide (and the same height
    * as the tile).  Thus the destination offset for (x,y) is the sum of:
    *   (x % column_width)                    // position within column
    *   (x / column_width) * bytes_per_column // column number * bytes per column
    *   y * column_width
    *
    * The copy destination offset for each range copied is the sum of
    * an X offset 'xo0' or 'xo' and a Y offset 'yo.'
    */
   const uint32_t column_width = ytile_span;
   const uint32_t bytes_per_column = column_width * ytile_height;

   uint32_t xo0 = (x0 % ytile_span) + (x0 / ytile_span) * bytes_per_column;
   uint32_t xo1 = (x1 % ytile_span) + (x1 / ytile_span) * bytes_per_column;

   /* Bit 9 of the destination offset control swizzling.
    * Only the X offset contributes to bit 9 of the total offset,
    * so swizzle can be calculated in advance for these X positions.
    * Move bit 9 three places down to bit 6.
    */
   uint32_t swizzle0 = (xo0 >> 3) & swizzle_bit;
   uint32_t swizzle1 = (xo1 >> 3) & swizzle_bit;

   uint32_t x, yo;

   src += y0 * src_pitch;

   for (yo = y0 * column_width; yo < y1 * column_width; yo += column_width) {
      uint32_t xo = xo1;
      uint32_t swizzle = swizzle1;

      mem_copy(dst + ((xo0 + yo) ^ swizzle0), src + x0, x1 - x0);

      /* Step by spans/columns.  As it happens, the swizzle bit flips
       * at each step so we don't need to calculate it explicitly.
       */
      for (x = x1; x < x2; x += ytile_span) {
         mem_copy(dst + ((xo + yo) ^ swizzle), src + x, ytile_span);
         xo += bytes_per_column;
         swizzle ^= swizzle_bit;
      }

      mem_copy(dst + ((xo + yo) ^ swizzle), src + x2, x3 - x2);

      src += src_pitch;
   }
}

/**
 * Copy texture data from X tile layout to linear.
 *
 * \copydoc tile_copy_fn
 */

static inline void
xtiled_to_linear(
   uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
   uint32_t y0, uint32_t y1,
   char *dst, const char *src,
   uint32_t dst_pitch,
   uint32_t swizzle_bit,
   mem_copy_fn mem_copy)
{
   uint32_t xo, yo;

   dst += y0 * dst_pitch;

   for (yo = y0 * xtile_width; yo < y1 * xtile_width; yo += xtile_width) {
      /* Bits 9 and 10 of the copy destination offset control swizzling.
       * Only 'yo' contributes to those bits in the total offset,
       * so calculate 'swizzle' just once per row.
       * Move bits 9 and 10 three and four places respectively down
       * to bit 6 and xor them.
       */
      uint32_t swizzle = ((yo >> 3) ^ (yo >> 4)) & swizzle_bit;

      mem_copy(dst + x0, src + ((x0 + yo) ^ swizzle), x1 - x0);

      for (xo = x1; xo < x2; xo += xtile_span) {
         mem_copy(dst + xo, src + ((xo + yo) ^ swizzle), xtile_span);
      }

      mem_copy(dst + x2, src + ((xo + yo) ^ swizzle), x3 - x2);

      dst += dst_pitch;
   }
}

 /**
 * Copy texture data from Y tile layout to linear.
 *
 * \copydoc tile_copy_fn
 */

static inline void
ytiled_to_linear(
   uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
   uint32_t y0, uint32_t y1,
   char *dst, const char *src,
   uint32_t dst_pitch,
   uint32_t swizzle_bit,
   mem_copy_fn mem_copy)
{
   /* Y tiles consist of columns that are 'ytile_span' wide (and the same height
    * as the tile).  Thus the destination offset for (x,y) is the sum of:
    *   (x % column_width)                    // position within column
    *   (x / column_width) * bytes_per_column // column number * bytes per column
    *   y * column_width
    *
    * The copy destination offset for each range copied is the sum of
    * an X offset 'xo0' or 'xo' and a Y offset 'yo.'
    */
   const uint32_t column_width = ytile_span;
   const uint32_t bytes_per_column = column_width * ytile_height;

   uint32_t xo0 = (x0 % ytile_span) + (x0 / ytile_span) * bytes_per_column;
   uint32_t xo1 = (x1 % ytile_span) + (x1 / ytile_span) * bytes_per_column;

   /* Bit 9 of the destination offset control swizzling.
    * Only the X offset contributes to bit 9 of the total offset,
    * so swizzle can be calculated in advance for these X positions.
    * Move bit 9 three places down to bit 6.
    */
   uint32_t swizzle0 = (xo0 >> 3) & swizzle_bit;
   uint32_t swizzle1 = (xo1 >> 3) & swizzle_bit;

   uint32_t x, yo;

   dst += y0 * dst_pitch;

   for (yo = y0 * column_width; yo < y1 * column_width; yo += column_width) {
      uint32_t xo = xo1;
      uint32_t swizzle = swizzle1;

      mem_copy(dst + x0, src + ((xo0 + yo) ^ swizzle0), x1 - x0);

      /* Step by spans/columns.  As it happens, the swizzle bit flips
       * at each step so we don't need to calculate it explicitly.
       */
      for (x = x1; x < x2; x += ytile_span) {
         mem_copy(dst + x, src + ((xo + yo) ^ swizzle), ytile_span);
         xo += bytes_per_column;
         swizzle ^= swizzle_bit;
      }

      mem_copy(dst + x2, src + ((xo + yo) ^ swizzle), x3 - x2);

      dst += dst_pitch;
   }
}


/**
 * Copy texture data from linear to X tile layout, faster.
 *
 * Same as \ref xtile_copy but faster, because it passes constant parameters
 * for common cases, allowing the compiler to inline code optimized for those
 * cases.
 *
 * \copydoc tile_copy_fn
 */
static FLATTEN void
linear_to_xtiled_faster(uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
                  uint32_t y0, uint32_t y1,
                  char *dst, const char *src,
                  uint32_t src_pitch,
                  uint32_t swizzle_bit,
                  mem_copy_fn mem_copy)
{
   if (x0 == 0 && x3 == xtile_width && y0 == 0 && y1 == xtile_height) {
      if (mem_copy == memcpy)
         return linear_to_xtiled(0, 0, xtile_width, xtile_width, 0, xtile_height,
                           dst, src, src_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return linear_to_xtiled(0, 0, xtile_width, xtile_width, 0, xtile_height,
                           dst, src, src_pitch, swizzle_bit, rgba8_copy);
   } else {
      if (mem_copy == memcpy)
         return linear_to_xtiled(x0, x1, x2, x3, y0, y1,
                           dst, src, src_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return linear_to_xtiled(x0, x1, x2, x3, y0, y1,
                           dst, src, src_pitch, swizzle_bit, rgba8_copy);
   }
   linear_to_xtiled(x0, x1, x2, x3, y0, y1,
              dst, src, src_pitch, swizzle_bit, mem_copy);
}

/**
 * Copy texture data from linear to Y tile layout, faster.
 *
 * Same as \ref ytile_copy but faster, because it passes constant parameters
 * for common cases, allowing the compiler to inline code optimized for those
 * cases.
 *
 * \copydoc tile_copy_fn
 */
static FLATTEN void
linear_to_ytiled_faster(uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
                  uint32_t y0, uint32_t y1,
                  char *dst, const char *src,
                  uint32_t src_pitch,
                  uint32_t swizzle_bit,
                  mem_copy_fn mem_copy)
{
   if (x0 == 0 && x3 == ytile_width && y0 == 0 && y1 == ytile_height) {
      if (mem_copy == memcpy)
         return linear_to_ytiled(0, 0, ytile_width, ytile_width, 0, ytile_height,
                           dst, src, src_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return linear_to_ytiled(0, 0, ytile_width, ytile_width, 0, ytile_height,
                           dst, src, src_pitch, swizzle_bit, rgba8_copy);
   } else {
      if (mem_copy == memcpy)
         return linear_to_ytiled(x0, x1, x2, x3, y0, y1,
                           dst, src, src_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return linear_to_ytiled(x0, x1, x2, x3, y0, y1,
                           dst, src, src_pitch, swizzle_bit, rgba8_copy);
   }
   linear_to_ytiled(x0, x1, x2, x3, y0, y1,
              dst, src, src_pitch, swizzle_bit, mem_copy);
}

/**
 * Copy texture data from X tile layout to linear, faster.
 *
 * Same as \ref xtile_copy but faster, because it passes constant parameters
 * for common cases, allowing the compiler to inline code optimized for those
 * cases.
 *
 * \copydoc tile_copy_fn
 */
static FLATTEN void
xtiled_to_linear_faster(uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
                  uint32_t y0, uint32_t y1,
                  char *dst, const char *src,
                  uint32_t dst_pitch,
                  uint32_t swizzle_bit,
                  mem_copy_fn mem_copy)
{
   if (x0 == 0 && x3 == xtile_width && y0 == 0 && y1 == xtile_height) {
      if (mem_copy == memcpy)
         return xtiled_to_linear(0, 0, xtile_width, xtile_width, 0, xtile_height,
                           dst, src, dst_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return xtiled_to_linear(0, 0, xtile_width, xtile_width, 0, xtile_height,
                           dst, src, dst_pitch, swizzle_bit, rgba8_copy);
   } else {
      if (mem_copy == memcpy)
         return xtiled_to_linear(x0, x1, x2, x3, y0, y1,
                           dst, src, dst_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return xtiled_to_linear(x0, x1, x2, x3, y0, y1,
                           dst, src, dst_pitch, swizzle_bit, rgba8_copy);
   }
   xtiled_to_linear(x0, x1, x2, x3, y0, y1,
              dst, src, dst_pitch, swizzle_bit, mem_copy);
}

/**
 * Copy texture data from Y tile layout to linear, faster.
 *
 * Same as \ref ytile_copy but faster, because it passes constant parameters
 * for common cases, allowing the compiler to inline code optimized for those
 * cases.
 *
 * \copydoc tile_copy_fn
 */
static FLATTEN void
ytiled_to_linear_faster(uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3,
                  uint32_t y0, uint32_t y1,
                  char *dst, const char *src,
                  uint32_t dst_pitch,
                  uint32_t swizzle_bit,
                  mem_copy_fn mem_copy)
{
   if (x0 == 0 && x3 == ytile_width && y0 == 0 && y1 == ytile_height) {
      if (mem_copy == memcpy)
         return ytiled_to_linear(0, 0, ytile_width, ytile_width, 0, ytile_height,
                           dst, src, dst_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return ytiled_to_linear(0, 0, ytile_width, ytile_width, 0, ytile_height,
                           dst, src, dst_pitch, swizzle_bit, rgba8_copy);
   } else {
      if (mem_copy == memcpy)
         return ytiled_to_linear(x0, x1, x2, x3, y0, y1,
                           dst, src, dst_pitch, swizzle_bit, memcpy);
      else if (mem_copy == rgba8_copy)
         return ytiled_to_linear(x0, x1, x2, x3, y0, y1,
                           dst, src, dst_pitch, swizzle_bit, rgba8_copy);
   }
   ytiled_to_linear(x0, x1, x2, x3, y0, y1,
              dst, src, dst_pitch, swizzle_bit, mem_copy);
}

/**
 * Copy from linear to tiled texture.
 *
 * Divide the region given by X range [xt1, xt2) and Y range [yt1, yt2) into
 * pieces that do not cross tile boundaries and copy each piece with a tile
 * copy function (\ref tile_copy_fn).
 * The X range is in bytes, i.e. pixels * bytes-per-pixel.
 * The Y range is in pixels (i.e. unitless).
 * 'dst' is the start of the texture and 'src' is the corresponding
 * address to copy from, though copying begins at (xt1, yt1).
 */
void
linear_to_tiled(uint32_t xt1, uint32_t xt2,
                uint32_t yt1, uint32_t yt2,
                char *dst, const char *src,
                uint32_t dst_pitch, uint32_t src_pitch,
                bool has_swizzling,
                uint32_t tiling,
                mem_copy_fn mem_copy)
{
   tile_copy_fn tile_copy;
   uint32_t xt0, xt3;
   uint32_t yt0, yt3;
   uint32_t xt, yt;
   uint32_t tw, th, span;
   uint32_t swizzle_bit = has_swizzling ? 1<<6 : 0;

   if (tiling == I915_TILING_X) {
      tw = xtile_width;
      th = xtile_height;
      span = xtile_span;
      tile_copy = linear_to_xtiled_faster;
   } else if (tiling == I915_TILING_Y) {
      tw = ytile_width;
      th = ytile_height;
      span = ytile_span;
      tile_copy = linear_to_ytiled_faster;
   } else {
      unreachable("unsupported tiling");
   }

   /* Round out to tile boundaries. */
   xt0 = ALIGN_DOWN(xt1, tw);
   xt3 = ALIGN_UP  (xt2, tw);
   yt0 = ALIGN_DOWN(yt1, th);
   yt3 = ALIGN_UP  (yt2, th);

   /* Loop over all tiles to which we have something to copy.
    * 'xt' and 'yt' are the origin of the destination tile, whether copying
    * copying a full or partial tile.
    * tile_copy() copies one tile or partial tile.
    * Looping x inside y is the faster memory access pattern.
    */
   for (yt = yt0; yt < yt3; yt += th) {
      for (xt = xt0; xt < xt3; xt += tw) {
         /* The area to update is [x0,x3) x [y0,y1).
          * May not want the whole tile, hence the min and max.
          */
         uint32_t x0 = MAX2(xt1, xt);
         uint32_t y0 = MAX2(yt1, yt);
         uint32_t x3 = MIN2(xt2, xt + tw);
         uint32_t y1 = MIN2(yt2, yt + th);

         /* [x0,x3) is split into [x0,x1), [x1,x2), [x2,x3) such that
          * the middle interval is the longest span-aligned part.
          * The sub-ranges could be empty.
          */
         uint32_t x1, x2;
         x1 = ALIGN_UP(x0, span);
         if (x1 > x3)
            x1 = x2 = x3;
         else
            x2 = ALIGN_DOWN(x3, span);

         assert(x0 <= x1 && x1 <= x2 && x2 <= x3);
         assert(x1 - x0 < span && x3 - x2 < span);
         assert(x3 - x0 <= tw);
         assert((x2 - x1) % span == 0);

         /* Translate by (xt,yt) for single-tile copier. */
         tile_copy(x0-xt, x1-xt, x2-xt, x3-xt,
                   y0-yt, y1-yt,
                   dst + xt * th + yt * dst_pitch,
                   src + xt      + yt * src_pitch,
                   src_pitch,
                   swizzle_bit,
                   mem_copy);
      }
   }
}

/**
 * Copy from tiled to linear texture.
 *
 * Divide the region given by X range [xt1, xt2) and Y range [yt1, yt2) into
 * pieces that do not cross tile boundaries and copy each piece with a tile
 * copy function (\ref tile_copy_fn).
 * The X range is in bytes, i.e. pixels * bytes-per-pixel.
 * The Y range is in pixels (i.e. unitless).
 * 'dst' is the start of the texture and 'src' is the corresponding
 * address to copy from, though copying begins at (xt1, yt1).
 */
void
tiled_to_linear(uint32_t xt1, uint32_t xt2,
                uint32_t yt1, uint32_t yt2,
                char *dst, const char *src,
                uint32_t dst_pitch, uint32_t src_pitch,
                bool has_swizzling,
                uint32_t tiling,
                mem_copy_fn mem_copy)
{
   tile_copy_fn tile_copy;
   uint32_t xt0, xt3;
   uint32_t yt0, yt3;
   uint32_t xt, yt;
   uint32_t tw, th, span;
   uint32_t swizzle_bit = has_swizzling ? 1<<6 : 0;

   if (tiling == I915_TILING_X) {
      tw = xtile_width;
      th = xtile_height;
      span = xtile_span;
      tile_copy = xtiled_to_linear_faster;
   } else if (tiling == I915_TILING_Y) {
      tw = ytile_width;
      th = ytile_height;
      span = ytile_span;
      tile_copy = ytiled_to_linear_faster;
   } else {
      unreachable("unsupported tiling");
   }

   /* Round out to tile boundaries. */
   xt0 = ALIGN_DOWN(xt1, tw);
   xt3 = ALIGN_UP  (xt2, tw);
   yt0 = ALIGN_DOWN(yt1, th);
   yt3 = ALIGN_UP  (yt2, th);

   /* Loop over all tiles to which we have something to copy.
    * 'xt' and 'yt' are the origin of the destination tile, whether copying
    * copying a full or partial tile.
    * tile_copy() copies one tile or partial tile.
    * Looping x inside y is the faster memory access pattern.
    */
   for (yt = yt0; yt < yt3; yt += th) {
      for (xt = xt0; xt < xt3; xt += tw) {
         /* The area to update is [x0,x3) x [y0,y1).
          * May not want the whole tile, hence the min and max.
          */
         uint32_t x0 = MAX2(xt1, xt);
         uint32_t y0 = MAX2(yt1, yt);
         uint32_t x3 = MIN2(xt2, xt + tw);
         uint32_t y1 = MIN2(yt2, yt + th);

         /* [x0,x3) is split into [x0,x1), [x1,x2), [x2,x3) such that
          * the middle interval is the longest span-aligned part.
          * The sub-ranges could be empty.
          */
         uint32_t x1, x2;
         x1 = ALIGN_UP(x0, span);
         if (x1 > x3)
            x1 = x2 = x3;
         else
            x2 = ALIGN_DOWN(x3, span);

         assert(x0 <= x1 && x1 <= x2 && x2 <= x3);
         assert(x1 - x0 < span && x3 - x2 < span);
         assert(x3 - x0 <= tw);
         assert((x2 - x1) % span == 0);

         /* Translate by (xt,yt) for single-tile copier. */
         tile_copy(x0-xt, x1-xt, x2-xt, x3-xt,
                   y0-yt, y1-yt,
                   dst + xt      + yt * dst_pitch,
                   src + xt * th + yt * src_pitch,
                   dst_pitch,
                   swizzle_bit,
                   mem_copy);
      }
   }
}


/**
 * Determine which type of mem_copy to use
 *
 *  TODO : add more details
 *  in   : tiledFormat, format, type
 *  out  : mem_copy, cpp
 */
bool intel_get_memcpy( mesa_format tiledFormat, GLenum format,
                        GLenum type, mem_copy_fn* mem_copy, uint32_t* cpp)
{
   if (type == GL_UNSIGNED_INT_8_8_8_8_REV &&
       !(format == GL_RGBA || format == GL_BGRA))
      return false; /* Invalid type/format combination */

   if ((tiledFormat == MESA_FORMAT_L_UNORM8 && format == GL_LUMINANCE) ||
       (tiledFormat == MESA_FORMAT_A_UNORM8 && format == GL_ALPHA)) {
      *cpp = 1;
      *mem_copy = memcpy;
   } else if ((tiledFormat == MESA_FORMAT_B8G8R8A8_UNORM) ||
              (tiledFormat == MESA_FORMAT_B8G8R8X8_UNORM)) {
      *cpp = 4;
      if (format == GL_BGRA) {
         *mem_copy = memcpy;
      } else if (format == GL_RGBA) {
         *mem_copy = rgba8_copy;
      }
   } else if ((tiledFormat == MESA_FORMAT_R8G8B8A8_UNORM) ||
              (tiledFormat == MESA_FORMAT_R8G8B8X8_UNORM)) {
      *cpp = 4;
      if (format == GL_BGRA) {
         /* Copying from RGBA to BGRA is the same as BGRA to RGBA so we can
          * use the same function.
          */
         *mem_copy = rgba8_copy;
      } else if (format == GL_RGBA) {
         *mem_copy = memcpy;
      }
   }

   if (!(*mem_copy))
      return false;

   return true;
}
