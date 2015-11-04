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

#include "main/imports.h"

#include "brw_defines.h"
#include "brw_image_load_store.h"

#define IF(r, g, b, a, dt, f) \
   [BRW_SURFACEFORMAT_##f] = { true, r, g, b, a, BRW_IMAGE_FORMAT_##dt },
const struct brw_image_format_info brw_image_format_info[] = {
   IF( 8,  0,  0,  0, UNORM, R8_UNORM)
   IF( 8,  0,  0,  0, SNORM, R8_SNORM)
   IF( 8,  0,  0,  0,  UINT, R8_UINT)
   IF( 8,  0,  0,  0,  SINT, R8_SINT)
   IF( 8,  8,  0,  0, UNORM, R8G8_UNORM)
   IF( 8,  8,  0,  0, SNORM, R8G8_SNORM)
   IF( 8,  8,  0,  0,  UINT, R8G8_UINT)
   IF( 8,  8,  0,  0,  SINT, R8G8_SINT)
   IF( 8,  8,  8,  8, UNORM, R8G8B8A8_UNORM)
   IF( 8,  8,  8,  8, SNORM, R8G8B8A8_SNORM)
   IF( 8,  8,  8,  8,  UINT, R8G8B8A8_UINT)
   IF( 8,  8,  8,  8,  SINT, R8G8B8A8_SINT)
   IF(11, 11, 10,  0, FLOAT, R11G11B10_FLOAT)
   IF(10, 10, 10,  2, UNORM, R10G10B10A2_UNORM)
   IF(10, 10, 10,  2,  UINT, R10G10B10A2_UINT)
   IF(16,  0,  0,  0, UNORM, R16_UNORM)
   IF(16,  0,  0,  0, SNORM, R16_SNORM)
   IF(16,  0,  0,  0, FLOAT, R16_FLOAT)
   IF(16,  0,  0,  0,  UINT, R16_UINT)
   IF(16,  0,  0,  0,  SINT, R16_SINT)
   IF(16, 16,  0,  0, UNORM, R16G16_UNORM)
   IF(16, 16,  0,  0, SNORM, R16G16_SNORM)
   IF(16, 16,  0,  0, FLOAT, R16G16_FLOAT)
   IF(16, 16,  0,  0,  UINT, R16G16_UINT)
   IF(16, 16,  0,  0,  SINT, R16G16_SINT)
   IF(16, 16, 16, 16, UNORM, R16G16B16A16_UNORM)
   IF(16, 16, 16, 16, SNORM, R16G16B16A16_SNORM)
   IF(16, 16, 16, 16, FLOAT, R16G16B16A16_FLOAT)
   IF(16, 16, 16, 16,  UINT, R16G16B16A16_UINT)
   IF(16, 16, 16, 16,  SINT, R16G16B16A16_SINT)
   IF(32,  0,  0,  0, FLOAT, R32_FLOAT)
   IF(32,  0,  0,  0,  UINT, R32_UINT)
   IF(32,  0,  0,  0,  SINT, R32_SINT)
   IF(32, 32,  0,  0, FLOAT, R32G32_FLOAT)
   IF(32, 32,  0,  0,  UINT, R32G32_UINT)
   IF(32, 32,  0,  0,  SINT, R32G32_SINT)
   IF(32, 32, 32, 32, FLOAT, R32G32B32A32_FLOAT)
   IF(32, 32, 32, 32,  UINT, R32G32B32A32_UINT)
   IF(32, 32, 32, 32,  SINT, R32G32B32A32_SINT)
};
#undef IF

uint32_t
brw_image_format_for_gl_format(uint32_t gl_format)
{
   switch (gl_format) {
   case GL_R8:             return BRW_SURFACEFORMAT_R8_UNORM;
   case GL_R8_SNORM:       return BRW_SURFACEFORMAT_R8_SNORM;
   case GL_R8UI:           return BRW_SURFACEFORMAT_R8_UINT;
   case GL_R8I:            return BRW_SURFACEFORMAT_R8_SINT;
   case GL_RG8:            return BRW_SURFACEFORMAT_R8G8_UNORM;
   case GL_RG8_SNORM:      return BRW_SURFACEFORMAT_R8G8_SNORM;
   case GL_RG8UI:          return BRW_SURFACEFORMAT_R8G8_UINT;
   case GL_RG8I:           return BRW_SURFACEFORMAT_R8G8_SINT;
   case GL_RGBA8:          return BRW_SURFACEFORMAT_R8G8B8A8_UNORM;
   case GL_RGBA8_SNORM:    return BRW_SURFACEFORMAT_R8G8B8A8_SNORM;
   case GL_RGBA8UI:        return BRW_SURFACEFORMAT_R8G8B8A8_UINT;
   case GL_RGBA8I:         return BRW_SURFACEFORMAT_R8G8B8A8_SINT;
   case GL_R11F_G11F_B10F: return BRW_SURFACEFORMAT_R11G11B10_FLOAT;
   case GL_RGB10_A2:       return BRW_SURFACEFORMAT_R10G10B10A2_UNORM;
   case GL_RGB10_A2UI:     return BRW_SURFACEFORMAT_R10G10B10A2_UINT;
   case GL_R16:            return BRW_SURFACEFORMAT_R16_UNORM;
   case GL_R16_SNORM:      return BRW_SURFACEFORMAT_R16_SNORM;
   case GL_R16F:           return BRW_SURFACEFORMAT_R16_FLOAT;
   case GL_R16UI:          return BRW_SURFACEFORMAT_R16_UINT;
   case GL_R16I:           return BRW_SURFACEFORMAT_R16_SINT;
   case GL_RG16:           return BRW_SURFACEFORMAT_R16G16_UNORM;
   case GL_RG16_SNORM:     return BRW_SURFACEFORMAT_R16G16_SNORM;
   case GL_RG16F:          return BRW_SURFACEFORMAT_R16G16_FLOAT;
   case GL_RG16UI:         return BRW_SURFACEFORMAT_R16G16_UINT;
   case GL_RG16I:          return BRW_SURFACEFORMAT_R16G16_SINT;
   case GL_RGBA16:         return BRW_SURFACEFORMAT_R16G16B16A16_UNORM;
   case GL_RGBA16_SNORM:   return BRW_SURFACEFORMAT_R16G16B16A16_SNORM;
   case GL_RGBA16F:        return BRW_SURFACEFORMAT_R16G16B16A16_FLOAT;
   case GL_RGBA16UI:       return BRW_SURFACEFORMAT_R16G16B16A16_UINT;
   case GL_RGBA16I:        return BRW_SURFACEFORMAT_R16G16B16A16_SINT;
   case GL_R32F:           return BRW_SURFACEFORMAT_R32_FLOAT;
   case GL_R32UI:          return BRW_SURFACEFORMAT_R32_UINT;
   case GL_R32I:           return BRW_SURFACEFORMAT_R32_SINT;
   case GL_RG32F:          return BRW_SURFACEFORMAT_R32G32_FLOAT;
   case GL_RG32UI:         return BRW_SURFACEFORMAT_R32G32_UINT;
   case GL_RG32I:          return BRW_SURFACEFORMAT_R32G32_SINT;
   case GL_RGBA32F:        return BRW_SURFACEFORMAT_R32G32B32A32_FLOAT;
   case GL_RGBA32UI:       return BRW_SURFACEFORMAT_R32G32B32A32_UINT;
   case GL_RGBA32I:        return BRW_SURFACEFORMAT_R32G32B32A32_SINT;
   default:
      assert(!"Invalid image format");
      return GL_NONE;
   }
}

uint32_t
brw_lower_image_format(const struct brw_device_info *devinfo,
                       uint32_t format)
{
   switch (format) {
   /* These are never lowered.  Up to BDW we'll have to fall back to untyped
    * surface access for 128bpp formats.
    */
   case BRW_SURFACEFORMAT_R32G32B32A32_UINT:
   case BRW_SURFACEFORMAT_R32G32B32A32_SINT:
   case BRW_SURFACEFORMAT_R32G32B32A32_FLOAT:
   case BRW_SURFACEFORMAT_R32_UINT:
   case BRW_SURFACEFORMAT_R32_SINT:
   case BRW_SURFACEFORMAT_R32_FLOAT:
      return format;

   /* From HSW to BDW the only 64bpp format supported for typed access is
    * RGBA_UINT16.  IVB falls back to untyped.
    */
   case BRW_SURFACEFORMAT_R16G16B16A16_UINT:
   case BRW_SURFACEFORMAT_R16G16B16A16_SINT:
   case BRW_SURFACEFORMAT_R16G16B16A16_FLOAT:
   case BRW_SURFACEFORMAT_R32G32_UINT:
   case BRW_SURFACEFORMAT_R32G32_SINT:
   case BRW_SURFACEFORMAT_R32G32_FLOAT:
      return (devinfo->gen >= 9 ? format :
              devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R16G16B16A16_UINT :
              BRW_SURFACEFORMAT_R32G32_UINT);

   /* Up to BDW no SINT or FLOAT formats of less than 32 bits per component
    * are supported.  IVB doesn't support formats with more than one component
    * for typed access.  For 8 and 16 bpp formats IVB relies on the
    * undocumented behavior that typed reads from R_UINT8 and R_UINT16
    * surfaces actually do a 32-bit misaligned read.  The alternative would be
    * to use two surface state entries with different formats for each image,
    * one for reading (using R_UINT32) and another one for writing (using
    * R_UINT8 or R_UINT16), but that would complicate the shaders we generate
    * even more.
    */
   case BRW_SURFACEFORMAT_R8G8B8A8_UINT:
   case BRW_SURFACEFORMAT_R8G8B8A8_SINT:
      return (devinfo->gen >= 9 ? format :
              devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R8G8B8A8_UINT : BRW_SURFACEFORMAT_R32_UINT);

   case BRW_SURFACEFORMAT_R16G16_UINT:
   case BRW_SURFACEFORMAT_R16G16_SINT:
   case BRW_SURFACEFORMAT_R16G16_FLOAT:
      return (devinfo->gen >= 9 ? format :
              devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R16G16_UINT : BRW_SURFACEFORMAT_R32_UINT);

   case BRW_SURFACEFORMAT_R8G8_UINT:
   case BRW_SURFACEFORMAT_R8G8_SINT:
      return (devinfo->gen >= 9 ? format :
              devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R8G8_UINT : BRW_SURFACEFORMAT_R16_UINT);

   case BRW_SURFACEFORMAT_R16_UINT:
   case BRW_SURFACEFORMAT_R16_FLOAT:
   case BRW_SURFACEFORMAT_R16_SINT:
      return (devinfo->gen >= 9 ? format : BRW_SURFACEFORMAT_R16_UINT);

   case BRW_SURFACEFORMAT_R8_UINT:
   case BRW_SURFACEFORMAT_R8_SINT:
      return (devinfo->gen >= 9 ? format : BRW_SURFACEFORMAT_R8_UINT);

   /* Neither the 2/10/10/10 nor the 11/11/10 packed formats are supported
    * by the hardware.
    */
   case BRW_SURFACEFORMAT_R10G10B10A2_UINT:
   case BRW_SURFACEFORMAT_R10G10B10A2_UNORM:
   case BRW_SURFACEFORMAT_R11G11B10_FLOAT:
      return BRW_SURFACEFORMAT_R32_UINT;

   /* No normalized fixed-point formats are supported by the hardware. */
   case BRW_SURFACEFORMAT_R16G16B16A16_UNORM:
   case BRW_SURFACEFORMAT_R16G16B16A16_SNORM:
      return (devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R16G16B16A16_UINT :
              BRW_SURFACEFORMAT_R32G32_UINT);

   case BRW_SURFACEFORMAT_R8G8B8A8_UNORM:
   case BRW_SURFACEFORMAT_R8G8B8A8_SNORM:
      return (devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R8G8B8A8_UINT : BRW_SURFACEFORMAT_R32_UINT);

   case BRW_SURFACEFORMAT_R16G16_UNORM:
   case BRW_SURFACEFORMAT_R16G16_SNORM:
      return (devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R16G16_UINT : BRW_SURFACEFORMAT_R32_UINT);

   case BRW_SURFACEFORMAT_R8G8_UNORM:
   case BRW_SURFACEFORMAT_R8G8_SNORM:
      return (devinfo->gen >= 8 || devinfo->is_haswell ?
              BRW_SURFACEFORMAT_R8G8_UINT : BRW_SURFACEFORMAT_R16_UINT);

   case BRW_SURFACEFORMAT_R16_UNORM:
   case BRW_SURFACEFORMAT_R16_SNORM:
      return BRW_SURFACEFORMAT_R16_UINT;

   case BRW_SURFACEFORMAT_R8_UNORM:
   case BRW_SURFACEFORMAT_R8_SNORM:
      return BRW_SURFACEFORMAT_R8_UINT;

   default:
      unreachable("Unknown image format");
   }
}
