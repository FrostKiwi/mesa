/*
 * Mesa 3-D graphics library
 *
 * Copyright (C) 2015 Intel Corporation.  All Rights Reserved.
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

#include "glheader.h"
#include "context.h"
#include "enums.h"
#include "imports.h"
#include "macros.h"
#include "teximage.h"
#include "texobj.h"
#include "fbobject.h"
#include "buffers.h"
#include "state.h"

#include "bufferobj.h"
#include "pbo.h"
#include "meta.h"
#include "glformats.h"
#include "shaderapi.h"
#include "uniforms.h"
#include "texstate.h"
#include "varray.h"


bool
_mesa_meta_TexSubImage(struct gl_context *ctx, GLuint dims,
                       struct gl_texture_image *tex_image,
                       int xoffset, int yoffset, int zoffset,
                       int width, int height, int depth,
                       GLenum format, GLenum type, const void *pixels,
                       bool allocate_storage, bool create_pbo,
                       const struct gl_pixelstore_attrib *packing)
{
   uint32_t pbo_format;
   GLenum internal_format, status;
   GLuint pbo = 0, pbo_tex = 0, row_stride, size;
   GLuint fbos[2] = { 0, 0 };
   struct gl_texture_object *pbo_tex_obj;
   struct gl_texture_image *pbo_tex_image;
   struct gl_buffer_object *buffer_obj;
   bool success = false;

   /* XXX: This should probably be passed in from somewhere */
   const char *where = "_mesa_meta_TexSubImage";

   if (!_mesa_is_bufferobj(packing->BufferObj) && !create_pbo)
      return false;

   if (format == GL_DEPTH_COMPONENT ||
       format == GL_DEPTH_STENCIL ||
       format == GL_STENCIL_INDEX ||
       format == GL_COLOR_INDEX)
      return false;

   if (packing->Alignment > 4 ||
       packing->SkipPixels > 0 ||
       packing->SkipRows > 0 ||
       (packing->RowLength != 0 && packing->RowLength != width) ||
       packing->SwapBytes ||
       packing->LsbFirst ||
       packing->Invert)
      return false;

   pbo_format = _mesa_format_from_format_and_type(format, type);
   if (_mesa_format_is_mesa_array_format(pbo_format))
      pbo_format = _mesa_format_from_array_format(pbo_format);

   if (!pbo_format)
      return false;

   if (!ctx->TextureFormatSupported[pbo_format])
      return false;

   internal_format = _mesa_get_format_base_format(pbo_format);
   row_stride = _mesa_format_row_stride(pbo_format, width);

   if (!_mesa_validate_pbo_access(dims, packing, width, height, depth,
                                  format, type, INT_MAX, pixels)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "%s(out of bounds PBO access)", where);
      return true;
   }

   if (_mesa_check_disallowed_mapping(packing->BufferObj)) {
      /* buffer is mapped - that's an error */
      _mesa_error(ctx, GL_INVALID_OPERATION, "%s(PBO is mapped)", where);
      return true;
   }

   if (allocate_storage)
      ctx->Driver.AllocTextureImageBuffer(ctx, tex_image);

   /* Only stash the current FBO */
   _mesa_meta_begin(ctx, 0);

   pbo = 0;
   if (_mesa_is_bufferobj(packing->BufferObj)) {
      buffer_obj = packing->BufferObj;
   } else {
      assert(create_pbo);

      _mesa_GenBuffers(1, &pbo);

      /* We know the client doesn't have this bound */
      _mesa_BindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo);

      size = _mesa_format_image_size(pbo_format, width, height, depth);
      _mesa_BufferData(GL_PIXEL_UNPACK_BUFFER, size, pixels, GL_STREAM_DRAW);

      buffer_obj = ctx->Unpack.BufferObj;
      pixels = NULL;

      _mesa_BindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
   }

   _mesa_GenTextures(1, &pbo_tex);
   pbo_tex_obj = _mesa_lookup_texture(ctx, pbo_tex);
   pbo_tex_obj->Target = depth > 2 ? GL_TEXTURE_2D_ARRAY : GL_TEXTURE_2D;
   pbo_tex_obj->Immutable = GL_TRUE;
   _mesa_initialize_texture_object(ctx, pbo_tex_obj, pbo_tex, GL_TEXTURE_2D);

   pbo_tex_image = _mesa_get_tex_image(ctx, pbo_tex_obj,
                                       pbo_tex_obj->Target, 0);
   _mesa_init_teximage_fields(ctx, pbo_tex_image, width, height, depth,
                              0, internal_format, pbo_format);

   if (!ctx->Driver.SetTextureStorageForBufferObject(ctx, pbo_tex_obj,
                                                     buffer_obj,
                                                     (intptr_t)pixels,
                                                     row_stride)) {
      goto fail;
   }

   _mesa_GenFramebuffers(2, fbos);
   _mesa_BindFramebuffer(GL_READ_FRAMEBUFFER, fbos[0]);
   _mesa_BindFramebuffer(GL_DRAW_FRAMEBUFFER, fbos[1]);

   if (tex_image->TexObject->Target == GL_TEXTURE_1D_ARRAY) {
      assert(depth == 1);
      depth = height;
      height = 1;
   }

   _mesa_meta_bind_fbo_image(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                             pbo_tex_image, 0);
   /* If this passes on the first layer it should pass on the others */
   status = _mesa_CheckFramebufferStatus(GL_READ_FRAMEBUFFER);
   if (status != GL_FRAMEBUFFER_COMPLETE)
      goto fail;

   _mesa_meta_bind_fbo_image(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                             tex_image, zoffset);
   /* If this passes on the first layer it should pass on the others */
   status = _mesa_CheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
   if (status != GL_FRAMEBUFFER_COMPLETE)
      goto fail;

   _mesa_update_state(ctx);

   if (_mesa_meta_BlitFramebuffer(ctx, 0, 0, width, height,
                                  xoffset, yoffset,
                                  xoffset + width, yoffset + height,
                                  GL_COLOR_BUFFER_BIT, GL_NEAREST))
      goto fail;

   for (int z = 1; z < depth; z++) {
      _mesa_meta_bind_fbo_image(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                                pbo_tex_image, z);
      _mesa_meta_bind_fbo_image(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                                tex_image, zoffset + z);

      _mesa_update_state(ctx);

      _mesa_meta_BlitFramebuffer(ctx, 0, 0, width, height,
                                 xoffset, yoffset,
                                 xoffset + width, yoffset + height,
                                 GL_COLOR_BUFFER_BIT, GL_NEAREST);
   }

   success = true;

fail:
   _mesa_DeleteFramebuffers(2, fbos);
   _mesa_DeleteTextures(1, &pbo_tex);
   _mesa_DeleteBuffers(1, &pbo);

   _mesa_meta_end(ctx);

   return success;
}
