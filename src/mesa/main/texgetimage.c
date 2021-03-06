/*
 * Mesa 3-D graphics library
 *
 * Copyright (C) 1999-2008  Brian Paul   All Rights Reserved.
 * Copyright (c) 2009 VMware, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */


/**
 * Code for glGetTexImage() and glGetCompressedTexImage().
 */


#include "glheader.h"
#include "bufferobj.h"
#include "enums.h"
#include "context.h"
#include "formats.h"
#include "format_unpack.h"
#include "glformats.h"
#include "image.h"
#include "mtypes.h"
#include "pack.h"
#include "pbo.h"
#include "pixelstore.h"
#include "texcompress.h"
#include "texgetimage.h"
#include "teximage.h"
#include "texobj.h"
#include "texstore.h"


/**
 * Can the given type represent negative values?
 */
static inline GLboolean
type_needs_clamping(GLenum type)
{
   switch (type) {
   case GL_BYTE:
   case GL_SHORT:
   case GL_INT:
   case GL_FLOAT:
   case GL_HALF_FLOAT_ARB:
   case GL_UNSIGNED_INT_10F_11F_11F_REV:
   case GL_UNSIGNED_INT_5_9_9_9_REV:
      return GL_FALSE;
   default:
      return GL_TRUE;
   }
}


/**
 * glGetTexImage for depth/Z pixels.
 */
static void
get_tex_depth(struct gl_context *ctx, GLuint dimensions,
              GLenum format, GLenum type, GLvoid *pixels,
              struct gl_texture_image *texImage)
{
   const GLint width = texImage->Width;
   GLint height = texImage->Height;
   GLint depth = texImage->Depth;
   GLint img, row;
   GLfloat *depthRow = malloc(width * sizeof(GLfloat));

   if (!depthRow) {
      _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
      return;
   }

   if (texImage->TexObject->Target == GL_TEXTURE_1D_ARRAY) {
      depth = height;
      height = 1;
   }

   for (img = 0; img < depth; img++) {
      GLubyte *srcMap;
      GLint srcRowStride;

      /* map src texture buffer */
      ctx->Driver.MapTextureImage(ctx, texImage, img,
                                  0, 0, width, height, GL_MAP_READ_BIT,
                                  &srcMap, &srcRowStride);

      if (srcMap) {
         for (row = 0; row < height; row++) {
            void *dest = _mesa_image_address(dimensions, &ctx->Pack, pixels,
                                             width, height, format, type,
                                             img, row, 0);
            const GLubyte *src = srcMap + row * srcRowStride;
            _mesa_unpack_float_z_row(texImage->TexFormat, width, src, depthRow);
            _mesa_pack_depth_span(ctx, width, dest, type, depthRow, &ctx->Pack);
         }

         ctx->Driver.UnmapTextureImage(ctx, texImage, img);
      }
      else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
         break;
      }
   }

   free(depthRow);
}


/**
 * glGetTexImage for depth/stencil pixels.
 */
static void
get_tex_depth_stencil(struct gl_context *ctx, GLuint dimensions,
                      GLenum format, GLenum type, GLvoid *pixels,
                      struct gl_texture_image *texImage)
{
   const GLint width = texImage->Width;
   const GLint height = texImage->Height;
   const GLint depth = texImage->Depth;
   GLint img, row;

   assert(format == GL_DEPTH_STENCIL);
   assert(type == GL_UNSIGNED_INT_24_8 ||
          type == GL_FLOAT_32_UNSIGNED_INT_24_8_REV);

   for (img = 0; img < depth; img++) {
      GLubyte *srcMap;
      GLint rowstride;

      /* map src texture buffer */
      ctx->Driver.MapTextureImage(ctx, texImage, img,
                                  0, 0, width, height, GL_MAP_READ_BIT,
                                  &srcMap, &rowstride);

      if (srcMap) {
         for (row = 0; row < height; row++) {
            const GLubyte *src = srcMap + row * rowstride;
            void *dest = _mesa_image_address(dimensions, &ctx->Pack, pixels,
                                             width, height, format, type,
                                             img, row, 0);
            _mesa_unpack_depth_stencil_row(texImage->TexFormat,
                                           width,
                                           (const GLuint *) src,
                                           type, dest);
            if (ctx->Pack.SwapBytes) {
               _mesa_swap4((GLuint *) dest, width);
            }
         }

         ctx->Driver.UnmapTextureImage(ctx, texImage, img);
      }
      else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
         break;
      }
   }
}


/**
 * glGetTexImage for YCbCr pixels.
 */
static void
get_tex_ycbcr(struct gl_context *ctx, GLuint dimensions,
              GLenum format, GLenum type, GLvoid *pixels,
              struct gl_texture_image *texImage)
{
   const GLint width = texImage->Width;
   const GLint height = texImage->Height;
   const GLint depth = texImage->Depth;
   GLint img, row;

   for (img = 0; img < depth; img++) {
      GLubyte *srcMap;
      GLint rowstride;

      /* map src texture buffer */
      ctx->Driver.MapTextureImage(ctx, texImage, img,
                                  0, 0, width, height, GL_MAP_READ_BIT,
                                  &srcMap, &rowstride);

      if (srcMap) {
         for (row = 0; row < height; row++) {
            const GLubyte *src = srcMap + row * rowstride;
            void *dest = _mesa_image_address(dimensions, &ctx->Pack, pixels,
                                             width, height, format, type,
                                             img, row, 0);
            memcpy(dest, src, width * sizeof(GLushort));

            /* check for byte swapping */
            if ((texImage->TexFormat == MESA_FORMAT_YCBCR
                 && type == GL_UNSIGNED_SHORT_8_8_REV_MESA) ||
                (texImage->TexFormat == MESA_FORMAT_YCBCR_REV
                 && type == GL_UNSIGNED_SHORT_8_8_MESA)) {
               if (!ctx->Pack.SwapBytes)
                  _mesa_swap2((GLushort *) dest, width);
            }
            else if (ctx->Pack.SwapBytes) {
               _mesa_swap2((GLushort *) dest, width);
            }
         }

         ctx->Driver.UnmapTextureImage(ctx, texImage, img);
      }
      else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
         break;
      }
   }
}


/**
 * Get a color texture image with decompression.
 */
static void
get_tex_rgba_compressed(struct gl_context *ctx, GLuint dimensions,
                        GLenum format, GLenum type, GLvoid *pixels,
                        struct gl_texture_image *texImage,
                        GLbitfield transferOps)
{
   /* don't want to apply sRGB -> RGB conversion here so override the format */
   const mesa_format texFormat =
      _mesa_get_srgb_format_linear(texImage->TexFormat);
   const GLenum baseFormat = _mesa_get_format_base_format(texFormat);
   const GLenum destBaseFormat = _mesa_base_tex_format(ctx, format);
   GLenum rebaseFormat = GL_NONE;
   const GLuint width = texImage->Width;
   const GLuint height = texImage->Height;
   const GLuint depth = texImage->Depth;
   GLfloat *tempImage, *tempSlice, *srcRow;
   GLuint row, slice;

   /* Decompress into temp float buffer, then pack into user buffer */
   tempImage = malloc(width * height * depth
                                  * 4 * sizeof(GLfloat));
   if (!tempImage) {
      _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage()");
      return;
   }

   /* Decompress the texture image slices - results in 'tempImage' */
   for (slice = 0; slice < depth; slice++) {
      GLubyte *srcMap;
      GLint srcRowStride;

      tempSlice = tempImage + slice * 4 * width * height;

      ctx->Driver.MapTextureImage(ctx, texImage, slice,
                                  0, 0, width, height,
                                  GL_MAP_READ_BIT,
                                  &srcMap, &srcRowStride);
      if (srcMap) {
         _mesa_decompress_image(texFormat, width, height,
                                srcMap, srcRowStride, tempSlice);

         ctx->Driver.UnmapTextureImage(ctx, texImage, slice);
      }
      else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
         free(tempImage);
         return;
      }
   }

   if (baseFormat == GL_LUMINANCE ||
       baseFormat == GL_INTENSITY ||
       baseFormat == GL_LUMINANCE_ALPHA) {
      /* If a luminance (or intensity) texture is read back as RGB(A), the
       * returned value should be (L,0,0,1), not (L,L,L,1).  Set rebaseFormat
       * here to get G=B=0.
       */
      rebaseFormat = texImage->_BaseFormat;
   }
   else if ((baseFormat == GL_RGBA ||
             baseFormat == GL_RGB  ||
             baseFormat == GL_RG) &&
            (destBaseFormat == GL_LUMINANCE ||
             destBaseFormat == GL_LUMINANCE_ALPHA ||
             destBaseFormat == GL_LUMINANCE_INTEGER_EXT ||
             destBaseFormat == GL_LUMINANCE_ALPHA_INTEGER_EXT)) {
      /* If we're reading back an RGB(A) texture as luminance then we need
       * to return L=tex(R).  Note, that's different from glReadPixels which
       * returns L=R+G+B.
       */
      rebaseFormat = GL_LUMINANCE_ALPHA; /* this covers GL_LUMINANCE too */
   }

   if (rebaseFormat) {
      _mesa_rebase_rgba_float(width * height, (GLfloat (*)[4]) tempImage,
                              rebaseFormat);
   }

   tempSlice = tempImage;
   for (slice = 0; slice < depth; slice++) {
      srcRow = tempSlice;
      for (row = 0; row < height; row++) {
         void *dest = _mesa_image_address(dimensions, &ctx->Pack, pixels,
                                          width, height, format, type,
                                          slice, row, 0);

         _mesa_pack_rgba_span_float(ctx, width, (GLfloat (*)[4]) srcRow,
                                    format, type, dest, &ctx->Pack, transferOps);
         srcRow += 4 * width;
      }
      tempSlice += 4 * width * height;
   }

   free(tempImage);
}


/**
 * Return a base GL format given the user-requested format
 * for glGetTexImage().
 */
GLenum
_mesa_base_pack_format(GLenum format)
{
   switch (format) {
   case GL_ABGR_EXT:
   case GL_BGRA:
   case GL_BGRA_INTEGER:
   case GL_RGBA_INTEGER:
      return GL_RGBA;
   case GL_BGR:
   case GL_BGR_INTEGER:
   case GL_RGB_INTEGER:
      return GL_RGB;
   case GL_RED_INTEGER:
      return GL_RED;
   case GL_GREEN_INTEGER:
      return GL_GREEN;
   case GL_BLUE_INTEGER:
      return GL_BLUE;
   case GL_ALPHA_INTEGER:
      return GL_ALPHA;
   case GL_LUMINANCE_INTEGER_EXT:
      return GL_LUMINANCE;
   case GL_LUMINANCE_ALPHA_INTEGER_EXT:
      return GL_LUMINANCE_ALPHA;
   default:
      return format;
   }
}


/**
 * Get an uncompressed color texture image.
 */
static void
get_tex_rgba_uncompressed(struct gl_context *ctx, GLuint dimensions,
                          GLenum format, GLenum type, GLvoid *pixels,
                          struct gl_texture_image *texImage,
                          GLbitfield transferOps)
{
   /* don't want to apply sRGB -> RGB conversion here so override the format */
   const mesa_format texFormat =
      _mesa_get_srgb_format_linear(texImage->TexFormat);
   const GLuint width = texImage->Width;
   GLenum destBaseFormat = _mesa_base_pack_format(format);
   GLenum rebaseFormat = GL_NONE;
   GLuint height = texImage->Height;
   GLuint depth = texImage->Depth;
   GLuint img, row;
   GLfloat (*rgba)[4];
   GLuint (*rgba_uint)[4];
   GLboolean tex_is_integer = _mesa_is_format_integer_color(texImage->TexFormat);
   GLboolean tex_is_uint = _mesa_is_format_unsigned(texImage->TexFormat);
   GLenum texBaseFormat = _mesa_get_format_base_format(texImage->TexFormat);

   /* Allocate buffer for one row of texels */
   rgba = malloc(4 * width * sizeof(GLfloat));
   rgba_uint = (GLuint (*)[4]) rgba;
   if (!rgba) {
      _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage()");
      return;
   }

   if (texImage->TexObject->Target == GL_TEXTURE_1D_ARRAY) {
      depth = height;
      height = 1;
   }

   if (texImage->_BaseFormat == GL_LUMINANCE ||
       texImage->_BaseFormat == GL_INTENSITY ||
       texImage->_BaseFormat == GL_LUMINANCE_ALPHA) {
      /* If a luminance (or intensity) texture is read back as RGB(A), the
       * returned value should be (L,0,0,1), not (L,L,L,1).  Set rebaseFormat
       * here to get G=B=0.
       */
      rebaseFormat = texImage->_BaseFormat;
   }
   else if ((texImage->_BaseFormat == GL_RGBA ||
             texImage->_BaseFormat == GL_RGB ||
             texImage->_BaseFormat == GL_RG) &&
            (destBaseFormat == GL_LUMINANCE ||
             destBaseFormat == GL_LUMINANCE_ALPHA ||
             destBaseFormat == GL_LUMINANCE_INTEGER_EXT ||
             destBaseFormat == GL_LUMINANCE_ALPHA_INTEGER_EXT)) {
      /* If we're reading back an RGB(A) texture as luminance then we need
       * to return L=tex(R).  Note, that's different from glReadPixels which
       * returns L=R+G+B.
       */
      rebaseFormat = GL_LUMINANCE_ALPHA; /* this covers GL_LUMINANCE too */
   }
   else if (texImage->_BaseFormat != texBaseFormat) {
      /* The internal format and the real format differ, so we can't rely
       * on the unpack functions setting the correct constant values.
       * (e.g. reading back GL_RGB8 which is actually RGBA won't set alpha=1)
       */
      switch (texImage->_BaseFormat) {
      case GL_RED:
         if ((texBaseFormat == GL_RGBA ||
              texBaseFormat == GL_RGB ||
              texBaseFormat == GL_RG) &&
             (destBaseFormat == GL_RGBA ||
              destBaseFormat == GL_RGB ||
              destBaseFormat == GL_RG ||
              destBaseFormat == GL_GREEN)) {
            rebaseFormat = texImage->_BaseFormat;
            break;
         }
         /* fall through */
      case GL_RG:
         if ((texBaseFormat == GL_RGBA ||
              texBaseFormat == GL_RGB) &&
             (destBaseFormat == GL_RGBA ||
              destBaseFormat == GL_RGB ||
              destBaseFormat == GL_BLUE)) {
            rebaseFormat = texImage->_BaseFormat;
            break;
         }
         /* fall through */
      case GL_RGB:
         if (texBaseFormat == GL_RGBA &&
             (destBaseFormat == GL_RGBA ||
              destBaseFormat == GL_ALPHA ||
              destBaseFormat == GL_LUMINANCE_ALPHA)) {
            rebaseFormat = texImage->_BaseFormat;
         }
         break;

      case GL_ALPHA:
         if (destBaseFormat != GL_ALPHA) {
            rebaseFormat = texImage->_BaseFormat;
         }
         break;
      }
   }

   for (img = 0; img < depth; img++) {
      GLubyte *srcMap;
      GLint rowstride;

      /* map src texture buffer */
      ctx->Driver.MapTextureImage(ctx, texImage, img,
                                  0, 0, width, height, GL_MAP_READ_BIT,
                                  &srcMap, &rowstride);
      if (srcMap) {
         for (row = 0; row < height; row++) {
            const GLubyte *src = srcMap + row * rowstride;
            void *dest = _mesa_image_address(dimensions, &ctx->Pack, pixels,
                                             width, height, format, type,
                                             img, row, 0);

	    if (tex_is_integer) {
	       _mesa_unpack_uint_rgba_row(texFormat, width, src, rgba_uint);
               if (rebaseFormat)
                  _mesa_rebase_rgba_uint(width, rgba_uint, rebaseFormat);
               if (tex_is_uint) {
                  _mesa_pack_rgba_span_from_uints(ctx, width,
                                                  (GLuint (*)[4]) rgba_uint,
                                                  format, type, dest);
               } else {
                  _mesa_pack_rgba_span_from_ints(ctx, width,
                                                 (GLint (*)[4]) rgba_uint,
                                                 format, type, dest);
               }
	    } else {
	       _mesa_unpack_rgba_row(texFormat, width, src, rgba);
               if (rebaseFormat)
                  _mesa_rebase_rgba_float(width, rgba, rebaseFormat);
	       _mesa_pack_rgba_span_float(ctx, width, (GLfloat (*)[4]) rgba,
					  format, type, dest,
					  &ctx->Pack, transferOps);
	    }
	 }

         /* Unmap the src texture buffer */
         ctx->Driver.UnmapTextureImage(ctx, texImage, img);
      }
      else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
         break;
      }
   }

   free(rgba);
}


/**
 * glGetTexImage for color formats (RGBA, RGB, alpha, LA, etc).
 * Compressed textures are handled here as well.
 */
static void
get_tex_rgba(struct gl_context *ctx, GLuint dimensions,
             GLenum format, GLenum type, GLvoid *pixels,
             struct gl_texture_image *texImage)
{
   const GLenum dataType = _mesa_get_format_datatype(texImage->TexFormat);
   GLbitfield transferOps = 0x0;

   /* In general, clamping does not apply to glGetTexImage, except when
    * the returned type of the image can't hold negative values.
    */
   if (type_needs_clamping(type)) {
      /* the returned image type can't have negative values */
      if (dataType == GL_FLOAT ||
          dataType == GL_HALF_FLOAT ||
          dataType == GL_SIGNED_NORMALIZED ||
          format == GL_LUMINANCE ||
          format == GL_LUMINANCE_ALPHA) {
         transferOps |= IMAGE_CLAMP_BIT;
      }
   }

   if (_mesa_is_format_compressed(texImage->TexFormat)) {
      get_tex_rgba_compressed(ctx, dimensions, format, type,
                              pixels, texImage, transferOps);
   }
   else {
      get_tex_rgba_uncompressed(ctx, dimensions, format, type,
                                pixels, texImage, transferOps);
   }
}


/**
 * Try to do glGetTexImage() with simple memcpy().
 * \return GL_TRUE if done, GL_FALSE otherwise
 */
static GLboolean
get_tex_memcpy(struct gl_context *ctx, GLenum format, GLenum type,
               GLvoid *pixels,
               struct gl_texture_image *texImage)
{
   const GLenum target = texImage->TexObject->Target;
   GLboolean memCopy = GL_FALSE;
   GLenum texBaseFormat = _mesa_get_format_base_format(texImage->TexFormat);

   /*
    * Check if we can use memcpy to copy from the hardware texture
    * format to the user's format/type.
    * Note that GL's pixel transfer ops don't apply to glGetTexImage()
    */
   if ((target == GL_TEXTURE_1D ||
        target == GL_TEXTURE_2D ||
        target == GL_TEXTURE_RECTANGLE ||
        _mesa_is_cube_face(target)) &&
       texBaseFormat == texImage->_BaseFormat) {
      memCopy = _mesa_format_matches_format_and_type(texImage->TexFormat,
                                                     format, type,
                                                     ctx->Pack.SwapBytes);
   }

   if (memCopy) {
      const GLuint bpp = _mesa_get_format_bytes(texImage->TexFormat);
      const GLuint bytesPerRow = texImage->Width * bpp;
      GLubyte *dst =
         _mesa_image_address2d(&ctx->Pack, pixels, texImage->Width,
                               texImage->Height, format, type, 0, 0);
      const GLint dstRowStride =
         _mesa_image_row_stride(&ctx->Pack, texImage->Width, format, type);
      GLubyte *src;
      GLint srcRowStride;

      /* map src texture buffer */
      ctx->Driver.MapTextureImage(ctx, texImage, 0,
                                  0, 0, texImage->Width, texImage->Height,
                                  GL_MAP_READ_BIT, &src, &srcRowStride);

      if (src) {
         if (bytesPerRow == dstRowStride && bytesPerRow == srcRowStride) {
            memcpy(dst, src, bytesPerRow * texImage->Height);
         }
         else {
            GLuint row;
            for (row = 0; row < texImage->Height; row++) {
               memcpy(dst, src, bytesPerRow);
               dst += dstRowStride;
               src += srcRowStride;
            }
         }

         /* unmap src texture buffer */
         ctx->Driver.UnmapTextureImage(ctx, texImage, 0);
      }
      else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage");
      }
   }

   return memCopy;
}


/**
 * This is the software fallback for Driver.GetTexImage().
 * All error checking will have been done before this routine is called.
 * We'll call ctx->Driver.MapTextureImage() to access the data, then
 * unmap with ctx->Driver.UnmapTextureImage().
 */
void
_mesa_GetTexImage_sw(struct gl_context *ctx,
                     GLenum format, GLenum type, GLvoid *pixels,
                     struct gl_texture_image *texImage)
{
   const GLuint dimensions =
      _mesa_get_texture_dimensions(texImage->TexObject->Target);

   /* map dest buffer, if PBO */
   if (_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
      /* Packing texture image into a PBO.
       * Map the (potentially) VRAM-based buffer into our process space so
       * we can write into it with the code below.
       * A hardware driver might use a sophisticated blit to move the
       * texture data to the PBO if the PBO is in VRAM along with the texture.
       */
      GLubyte *buf = (GLubyte *)
         ctx->Driver.MapBufferRange(ctx, 0, ctx->Pack.BufferObj->Size,
				    GL_MAP_WRITE_BIT, ctx->Pack.BufferObj,
                                    MAP_INTERNAL);
      if (!buf) {
         /* out of memory or other unexpected error */
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetTexImage(map PBO failed)");
         return;
      }
      /* <pixels> was an offset into the PBO.
       * Now make it a real, client-side pointer inside the mapped region.
       */
      pixels = ADD_POINTERS(buf, pixels);
   }

   if (get_tex_memcpy(ctx, format, type, pixels, texImage)) {
      /* all done */
   }
   else if (format == GL_DEPTH_COMPONENT) {
      get_tex_depth(ctx, dimensions, format, type, pixels, texImage);
   }
   else if (format == GL_DEPTH_STENCIL_EXT) {
      get_tex_depth_stencil(ctx, dimensions, format, type, pixels, texImage);
   }
   else if (format == GL_YCBCR_MESA) {
      get_tex_ycbcr(ctx, dimensions, format, type, pixels, texImage);
   }
   else {
      get_tex_rgba(ctx, dimensions, format, type, pixels, texImage);
   }

   if (_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
      ctx->Driver.UnmapBuffer(ctx, ctx->Pack.BufferObj, MAP_INTERNAL);
   }
}



/**
 * This is the software fallback for Driver.GetCompressedTexImage().
 * All error checking will have been done before this routine is called.
 */
void
_mesa_GetCompressedTexImage_sw(struct gl_context *ctx,
                               struct gl_texture_image *texImage,
                               GLvoid *img)
{
   const GLuint dimensions =
      _mesa_get_texture_dimensions(texImage->TexObject->Target);
   struct compressed_pixelstore store;
   GLuint i, slice;
   GLubyte *dest;

   _mesa_compute_compressed_pixelstore(dimensions, texImage->TexFormat,
                                       texImage->Width, texImage->Height,
                                       texImage->Depth,
                                       &ctx->Pack,
                                       &store);

   if (_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
      /* pack texture image into a PBO */
      dest = (GLubyte *)
         ctx->Driver.MapBufferRange(ctx, 0, ctx->Pack.BufferObj->Size,
				    GL_MAP_WRITE_BIT, ctx->Pack.BufferObj,
                                    MAP_INTERNAL);
      if (!dest) {
         /* out of memory or other unexpected error */
         _mesa_error(ctx, GL_OUT_OF_MEMORY,
                     "glGetCompresssedTexImage(map PBO failed)");
         return;
      }
      dest = ADD_POINTERS(dest, img);
   } else {
      dest = img;
   }

   dest += store.SkipBytes;

   for (slice = 0; slice < store.CopySlices; slice++) {
      GLint srcRowStride;
      GLubyte *src;

      /* map src texture buffer */
      ctx->Driver.MapTextureImage(ctx, texImage, 0,
                                  0, 0, texImage->Width, texImage->Height,
                                  GL_MAP_READ_BIT, &src, &srcRowStride);

      if (src) {

         for (i = 0; i < store.CopyRowsPerSlice; i++) {
            memcpy(dest, src, store.CopyBytesPerRow);
            dest += store.TotalBytesPerRow;
            src += srcRowStride;
         }

         ctx->Driver.UnmapTextureImage(ctx, texImage, 0);

         /* Advance to next slice */
         dest += store.TotalBytesPerRow * (store.TotalRowsPerSlice - store.CopyRowsPerSlice);

      } else {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGetCompresssedTexImage");
      }
   }

   if (_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
      ctx->Driver.UnmapBuffer(ctx, ctx->Pack.BufferObj, MAP_INTERNAL);
   }
}


/**
 * Validate the texture target enum supplied to glGetTex(ture)Image or
 * glGetCompressedTex(ture)Image.
 */
static GLboolean
legal_getteximage_target(struct gl_context *ctx, GLenum target, bool dsa)
{
   switch (target) {
   case GL_TEXTURE_1D:
   case GL_TEXTURE_2D:
   case GL_TEXTURE_3D:
      return GL_TRUE;
   case GL_TEXTURE_RECTANGLE_NV:
      return ctx->Extensions.NV_texture_rectangle;
   case GL_TEXTURE_1D_ARRAY_EXT:
   case GL_TEXTURE_2D_ARRAY_EXT:
      return ctx->Extensions.EXT_texture_array;
   case GL_TEXTURE_CUBE_MAP_ARRAY:
      return ctx->Extensions.ARB_texture_cube_map_array;

   /* Section 8.11 (Texture Queries) of the OpenGL 4.5 core profile spec
    * (30.10.2014) says:
    *    "An INVALID_ENUM error is generated if the effective target is not
    *    one of TEXTURE_1D, TEXTURE_2D, TEXTURE_3D, TEXTURE_1D_ARRAY,
    *    TEXTURE_2D_ARRAY, TEXTURE_CUBE_MAP_ARRAY, TEXTURE_RECTANGLE, one of
    *    the targets from table 8.19 (for GetTexImage and GetnTexImage *only*),
    *    or TEXTURE_CUBE_MAP (for GetTextureImage *only*)." (Emphasis added.)
    */
   case GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB:
   case GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB:
   case GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB:
   case GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB:
   case GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB:
   case GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB:
   {
      if (dsa)
         return GL_FALSE;
      else
         return ctx->Extensions.ARB_texture_cube_map;
   }
   case GL_TEXTURE_CUBE_MAP:
   {
      if (dsa)
         return GL_TRUE;
      else
         return GL_FALSE;
   }
   default:
      return GL_FALSE;
   }
}


/**
 * Do error checking for a glGetTex(ture)Image() call.
 * \return GL_TRUE if any error, GL_FALSE if no errors.
 */
static GLboolean
getteximage_error_check(struct gl_context *ctx,
                        struct gl_texture_object *texObj,
                        struct gl_texture_image *texImage,
                        GLenum target, GLint level,
                        GLenum format, GLenum type, GLsizei clientMemSize,
                        GLvoid *pixels, bool dsa )
{
   assert(texObj);
   assert(texImage);

   const GLint maxLevels = _mesa_max_texture_levels(ctx, target);
   const GLuint dimensions = (target == GL_TEXTURE_3D) ? 3 : 2;
   GLenum baseFormat;

   assert(maxLevels != 0);
   if (level < 0 || level >= maxLevels) {
      _mesa_error( ctx, GL_INVALID_VALUE, "glGetTex%sImage(level)",
                   dsa ? "ture" : "");
      return GL_TRUE;
   }

   /*
    * Format and type checking has been moved up to GetnTexImage and
    * GetTextureImage so that it happens before getting the texImage object.
    */

   baseFormat = _mesa_get_format_base_format(texImage->TexFormat);

   /* Make sure the requested image format is compatible with the
    * texture's format.
    */
   if (_mesa_is_color_format(format)
       && !_mesa_is_color_format(baseFormat)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glGetTex%sImage(format mismatch)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }
   else if (_mesa_is_depth_format(format)
            && !_mesa_is_depth_format(baseFormat)
            && !_mesa_is_depthstencil_format(baseFormat)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glGetTex%sImage(format mismatch)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }
   else if (_mesa_is_stencil_format(format)
            && !ctx->Extensions.ARB_texture_stencil8) {
      _mesa_error(ctx, GL_INVALID_ENUM,
                  "glGetTex%sImage(format=GL_STENCIL_INDEX)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }
   else if (_mesa_is_ycbcr_format(format)
            && !_mesa_is_ycbcr_format(baseFormat)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glGetTex%sImage(format mismatch)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }
   else if (_mesa_is_depthstencil_format(format)
            && !_mesa_is_depthstencil_format(baseFormat)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glGetTex%sImage(format mismatch)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }
   else if (_mesa_is_enum_format_integer(format) !=
            _mesa_is_format_integer(texImage->TexFormat)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glGetTex%sImage(format mismatch)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }

   if (!_mesa_validate_pbo_access(dimensions, &ctx->Pack, texImage->Width,
                                  texImage->Height, texImage->Depth,
                                  format, type, clientMemSize, pixels)) {
      if (_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glGetTex%sImage(out of bounds PBO access)",
                     dsa ? "ture" : "");
      } else {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "%s(out of bounds access:"
                     " bufSize (%d) is too small)",
                     dsa ? "glGetTextureImage" : "glGetnTexImageARB",
                     clientMemSize);
      }
      return GL_TRUE;
   }

   if (_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
      /* PBO should not be mapped */
      if (_mesa_check_disallowed_mapping(ctx->Pack.BufferObj)) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glGetTex%sImage(PBO is mapped)", dsa ? "ture" : "");
         return GL_TRUE;
      }
   }

   return GL_FALSE;
}


/**
 * This is the implementation for glGetnTexImageARB, glGetTextureImage,
 * and glGetTexImage.
 *
 * Requires caller to pass in texImage object because _mesa_GetTextureImage
 * must handle the GL_TEXTURE_CUBE_MAP target.
 *
 * \param target texture target.
 * \param level image level.
 * \param format pixel data format for returned image.
 * \param type pixel data type for returned image.
 * \param bufSize size of the pixels data buffer.
 * \param pixels returned pixel data.
 * \param dsa True when the caller an ARB_direct_state_access function,
 *            false otherwise
 */
void
_mesa_get_texture_image( struct gl_context *ctx,
                         struct gl_texture_object *texObj,
                         struct gl_texture_image *texImage, GLenum target,
                         GLint level, GLenum format, GLenum type,
                         GLsizei bufSize, GLvoid *pixels, bool dsa )
{
   assert(texObj);
   assert(texImage);

   FLUSH_VERTICES(ctx, 0);

   /*
    * Legal target checking has been moved up to GetnTexImage and
    * GetTextureImage so that it can be caught before receiving a NULL
    * texImage object and exiting.
    */

   if (getteximage_error_check(ctx, texObj, texImage, target, level, format,
                               type, bufSize, pixels, dsa)) {
      return;
   }

   if (!_mesa_is_bufferobj(ctx->Pack.BufferObj) && !pixels) {
      /* not an error, do nothing */
      return;
   }

   if (_mesa_is_zero_size_texture(texImage))
      return;

   if (MESA_VERBOSE & (VERBOSE_API | VERBOSE_TEXTURE)) {
      _mesa_debug(ctx, "glGetTex%sImage(tex %u) format = %s, w=%d, h=%d,"
                  " dstFmt=0x%x, dstType=0x%x\n",
                  dsa ? "ture": "",
                  texObj->Name,
                  _mesa_get_format_name(texImage->TexFormat),
                  texImage->Width, texImage->Height,
                  format, type);
   }

   _mesa_lock_texture(ctx, texObj);
   {
      ctx->Driver.GetTexImage(ctx, format, type, pixels, texImage);
   }
   _mesa_unlock_texture(ctx, texObj);
}

/**
 * Get texture image.  Called by glGetTexImage.
 *
 * \param target texture target.
 * \param level image level.
 * \param format pixel data format for returned image.
 * \param type pixel data type for returned image.
 * \param bufSize size of the pixels data buffer.
 * \param pixels returned pixel data.
 */
void GLAPIENTRY
_mesa_GetnTexImageARB( GLenum target, GLint level, GLenum format,
                       GLenum type, GLsizei bufSize, GLvoid *pixels )
{
   struct gl_texture_object *texObj;
   struct gl_texture_image *texImage;
   GLenum err;
   GET_CURRENT_CONTEXT(ctx);

   /*
    * This has been moved here because a format/type mismatch can cause a NULL
    * texImage object, which in turn causes the mismatch error to be
    * ignored.
    */
   err = _mesa_error_check_format_and_type(ctx, format, type);
   if (err != GL_NO_ERROR) {
      _mesa_error(ctx, err, "glGetnTexImage(format/type)");
      return;
   }

   /*
    * Legal target checking has been moved here to prevent exiting with a NULL
    * texImage object.
    */
   if (!legal_getteximage_target(ctx, target, false)) {
      _mesa_error(ctx, GL_INVALID_ENUM, "glGetnTexImage(target=0x%x)",
                  target);
      return;
   }

   texObj = _mesa_get_current_tex_object(ctx, target);
   if (!texObj)
      return;

   texImage = _mesa_select_tex_image(ctx, texObj, target, level);
   if (!texImage)
      return;

   _mesa_get_texture_image(ctx, texObj, texImage, target, level, format, type,
                           bufSize, pixels, false);
}


void GLAPIENTRY
_mesa_GetTexImage( GLenum target, GLint level, GLenum format,
                   GLenum type, GLvoid *pixels )
{
   _mesa_GetnTexImageARB(target, level, format, type, INT_MAX, pixels);
}

/**
 * Get texture image.
 *
 * \param texture texture name.
 * \param level image level.
 * \param format pixel data format for returned image.
 * \param type pixel data type for returned image.
 * \param bufSize size of the pixels data buffer.
 * \param pixels returned pixel data.
 */
void GLAPIENTRY
_mesa_GetTextureImage( GLuint texture, GLint level, GLenum format,
                       GLenum type, GLsizei bufSize, GLvoid *pixels )
{
   struct gl_texture_object *texObj;
   struct gl_texture_image *texImage;
   int i;
   GLint image_stride;
   GLenum err;
   GET_CURRENT_CONTEXT(ctx);

   /*
    * This has been moved here because a format/type mismatch can cause a NULL
    * texImage object, which in turn causes the mismatch error to be
    * ignored.
    */
   err = _mesa_error_check_format_and_type(ctx, format, type);
   if (err != GL_NO_ERROR) {
      _mesa_error(ctx, err, "glGetTextureImage(format/type)");
      return;
   }

   texObj = _mesa_lookup_texture_err(ctx, texture, "glGetTextureImage");
   if (!texObj)
      return;

   /*
    * Legal target checking has been moved here to prevent exiting with a NULL
    * texImage object.
    */
   if (!legal_getteximage_target(ctx, texObj->Target, true)) {
      _mesa_error(ctx, GL_INVALID_ENUM, "glGetTextureImage(target=0x%x)",
                  texObj->Target);
      return;
   }

   /* Must special case GL_TEXTURE_CUBE_MAP. */
   if (texObj->Target == GL_TEXTURE_CUBE_MAP) {

      /* Error checking */
      if (texObj->NumLayers < 6) {
         /* Not enough image planes for a cube map.  The spec does not say
          * what should happen in this case because the user has always
          * specified each cube face separately (using
          * GL_TEXTURE_CUBE_MAP_POSITIVE_X+i) in previous GL versions.
          * This is addressed in Khronos Bug 13223.
          */
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glGetTextureImage(insufficient cube map storage)");
         return;
      }
      if (!_mesa_cube_complete(texObj)) {
         /*
          * In Section 8.14.4 Manual Mipmap Generation, the OpenGL 4.5 Core
          * Profile spec (30.10.2014) says:
          *    "An INVALID_OPERATION error is generated by
          *    GenerateTextureMipmap if the effective target is
          *    TEXTURE_CUBE_MAP or TEXTURE_CUBE_MAP_ARRAY, and the texture
          *    object is not cube complete or cube array complete,
          *    respectively."
          */
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glGetTextureImage(cube map incomplete)");
         return;
      }


      /* Copy each face. */
      for (i = 0; i < 6; ++i) { /* For each face. */
         texImage = texObj->Image[i][level];
         _mesa_get_texture_image(ctx, texObj, texImage, texObj->Target, level,
                                 format, type, bufSize, pixels, true);

         image_stride = _mesa_image_image_stride(&ctx->Pack, texImage->Width,
                                                 texImage->Height, format,
                                                 type);
         pixels += image_stride;
         bufSize -= image_stride;
      }
   }
   else {
      texImage = _mesa_select_tex_image(ctx, texObj, texObj->Target, level);
      if (!texImage)
         return;

      _mesa_get_texture_image(ctx, texObj, texImage, texObj->Target, level,
                              format, type, bufSize, pixels, true);
   }
}

/**
 * Do error checking for a glGetCompressedTexImage() call.
 * \return GL_TRUE if any error, GL_FALSE if no errors.
 */
static GLboolean
getcompressedteximage_error_check(struct gl_context *ctx,
                                  struct gl_texture_object *texObj,
                                  struct gl_texture_image *texImage,
                                  GLenum target,
                                  GLint level, GLsizei clientMemSize,
                                  GLvoid *img, bool dsa)
{
   assert(texObj);
   assert(texImage);
   const GLint maxLevels = _mesa_max_texture_levels(ctx, target);
   GLuint compressedSize, dimensions;

   if (!legal_getteximage_target(ctx, target, dsa)) {
      _mesa_error(ctx, GL_INVALID_ENUM,
                  "glGetCompressedTex%sImage(target=0x%x)",
                  dsa ? "ture" : "", target);
      return GL_TRUE;
   }

   assert(maxLevels != 0);
   if (level < 0 || level >= maxLevels) {
      _mesa_error(ctx, GL_INVALID_VALUE,
                  "glGetCompressedTex%sImage(bad level = %d)",
                  dsa ? "ture" : "", level);
      return GL_TRUE;
   }

   if (!_mesa_is_format_compressed(texImage->TexFormat)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glGetCompressedTex%sImage(texture is not compressed)",
                  dsa ? "ture" : "");
      return GL_TRUE;
   }

   compressedSize = _mesa_format_image_size(texImage->TexFormat,
                                            texImage->Width,
                                            texImage->Height,
                                            texImage->Depth);

   /* Check for invalid pixel storage modes */
   dimensions = _mesa_get_texture_dimensions(texImage->TexObject->Target);
   if (!_mesa_compressed_pixel_storage_error_check(ctx, dimensions,
                                              &ctx->Pack, dsa ?
                                              "glGetCompressedTextureImage":
                                              "glGetCompressedTexImage")) {
      return GL_TRUE;
   }

   if (!_mesa_is_bufferobj(ctx->Pack.BufferObj)) {
      /* do bounds checking on writing to client memory */
      if (clientMemSize < (GLsizei) compressedSize) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "%s(out of bounds access: bufSize (%d) is too small)",
                     dsa ? "glGetCompressedTextureImage" :
                     "glGetnCompressedTexImageARB", clientMemSize);
         return GL_TRUE;
      }
   } else {
      /* do bounds checking on PBO write */
      if ((const GLubyte *) img + compressedSize >
          (const GLubyte *) ctx->Pack.BufferObj->Size) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glGetCompressedTex%sImage(out of bounds PBO access)",
                     dsa ? "ture" : "");
         return GL_TRUE;
      }

      /* make sure PBO is not mapped */
      if (_mesa_check_disallowed_mapping(ctx->Pack.BufferObj)) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glGetCompressedTex%sImage(PBO is mapped)",
                     dsa ? "ture" : "");
         return GL_TRUE;
      }
   }

   return GL_FALSE;
}

/** Implements glGetnCompressedTexImageARB, glGetCompressedTexImage, and
 * glGetCompressedTextureImage.
 *
 * texImage must be passed in because glGetCompressedTexImage must handle the
 * target GL_TEXTURE_CUBE_MAP.
 */
void
_mesa_get_compressed_texture_image( struct gl_context *ctx,
                                    struct gl_texture_object *texObj,
                                    struct gl_texture_image *texImage,
                                    GLenum target, GLint level,
                                    GLsizei bufSize, GLvoid *pixels,
                                    bool dsa )
{
   assert(texObj);
   assert(texImage);

   FLUSH_VERTICES(ctx, 0);

   if (getcompressedteximage_error_check(ctx, texObj, texImage, target, level,
                                         bufSize, pixels, dsa)) {
      return;
   }

   if (!_mesa_is_bufferobj(ctx->Pack.BufferObj) && !pixels) {
      /* not an error, do nothing */
      return;
   }

   if (_mesa_is_zero_size_texture(texImage))
      return;

   if (MESA_VERBOSE & (VERBOSE_API | VERBOSE_TEXTURE)) {
      _mesa_debug(ctx,
                  "glGetCompressedTex%sImage(tex %u) format = %s, w=%d, h=%d\n",
                  dsa ? "ture" : "", texObj->Name,
                  _mesa_get_format_name(texImage->TexFormat),
                  texImage->Width, texImage->Height);
   }

   _mesa_lock_texture(ctx, texObj);
   {
      ctx->Driver.GetCompressedTexImage(ctx, texImage, pixels);
   }
   _mesa_unlock_texture(ctx, texObj);
}

void GLAPIENTRY
_mesa_GetnCompressedTexImageARB(GLenum target, GLint level, GLsizei bufSize,
                                GLvoid *img)
{
   struct gl_texture_object *texObj;
   struct gl_texture_image *texImage;
   GET_CURRENT_CONTEXT(ctx);

   texObj = _mesa_get_current_tex_object(ctx, target);
   if (!texObj)
      return;

   texImage = _mesa_select_tex_image(ctx, texObj, target, level);
   if (!texImage)
      return;

   _mesa_get_compressed_texture_image(ctx, texObj, texImage, target, level,
                                      bufSize, img, false);
}

void GLAPIENTRY
_mesa_GetCompressedTexImage(GLenum target, GLint level, GLvoid *img)
{
   _mesa_GetnCompressedTexImageARB(target, level, INT_MAX, img);
}

/**
 * Get compressed texture image.
 *
 * \param texture texture name.
 * \param level image level.
 * \param bufSize size of the pixels data buffer.
 * \param pixels returned pixel data.
 */
void GLAPIENTRY
_mesa_GetCompressedTextureImage( GLuint texture, GLint level,
                                 GLsizei bufSize, GLvoid *pixels )
{
   struct gl_texture_object *texObj;
   struct gl_texture_image *texImage;
   int i;
   GLint image_stride;
   GET_CURRENT_CONTEXT(ctx);

   texObj = _mesa_lookup_texture_err(ctx, texture,
                                     "glGetCompressedTextureImage");
   if (!texObj)
      return;

   /* Must special case GL_TEXTURE_CUBE_MAP. */
   if (texObj->Target == GL_TEXTURE_CUBE_MAP) {
      assert(texObj->NumLayers >= 6);

      /* Copy each face. */
      for (i = 0; i < 6; ++i) { /* For each face. */
         texImage = texObj->Image[i][level];
         if (!texImage)
            return;

         _mesa_get_compressed_texture_image(ctx, texObj, texImage,
                                            texObj->Target, level,
                                            bufSize, pixels, true);

         /* Compressed images don't have a client format */
         image_stride = _mesa_format_image_size(texImage->TexFormat,
                                                texImage->Width,
                                                texImage->Height, 1);

         pixels += image_stride;
         bufSize -= image_stride;
      }
   }
   else {
      texImage = _mesa_select_tex_image(ctx, texObj, texObj->Target, level);
      if (!texImage)
         return;

      _mesa_get_compressed_texture_image(ctx, texObj, texImage,
                                         texObj->Target, level, bufSize,
                                         pixels, true);
   }
}
