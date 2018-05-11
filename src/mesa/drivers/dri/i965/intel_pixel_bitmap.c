/*
 * Copyright 2006 VMware, Inc.
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portionsalloc
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL VMWARE AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "isl/isl.h"
#include "main/blend.h"
#include "main/enums.h"
#include "main/image.h"
#include "main/colormac.h"
#include "main/condrender.h"
#include "main/glformats.h"
#include "main/mtypes.h"
#include "main/macros.h"
#include "main/pbo.h"
#include "main/bufferobj.h"
#include "main/state.h"
#include "main/texobj.h"
#include "main/context.h"
#include "main/fbobject.h"
#include "swrast/swrast.h"
#include "drivers/common/meta.h"

#include "brw_blorp.h"
#include "brw_context.h"
#include "intel_screen.h"
#include "intel_batchbuffer.h"
#include "intel_fbo.h"
#include "intel_image.h"
#include "intel_buffers.h"
#include "intel_pixel.h"
#include "intel_buffer_objects.h"


#define FILE_DEBUG_FLAG DEBUG_PIXEL


/* Unlike the other intel_pixel_* functions, the expectation here is
 * that the incoming data is not in a PBO.  With the XY_TEXT blit
 * method, there's no benefit haveing it in a PBO, but we could
 * implement a path based on XY_MONO_SRC_COPY_BLIT which might benefit
 * PBO bitmaps.  I think they are probably pretty rare though - I
 * wonder if Xgl uses them?
 */
static const GLubyte *map_pbo( struct gl_context *ctx,
			       GLsizei width, GLsizei height,
			       const struct gl_pixelstore_attrib *unpack,
			       const GLubyte *bitmap )
{
   GLubyte *buf;

   if (!_mesa_validate_pbo_access(2, unpack, width, height, 1,
				  GL_COLOR_INDEX, GL_BITMAP,
				  INT_MAX, (const GLvoid *) bitmap)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,"glBitmap(invalid PBO access)");
      return NULL;
   }

   buf = (GLubyte *) ctx->Driver.MapBufferRange(ctx, 0, unpack->BufferObj->Size,
						GL_MAP_READ_BIT,
						unpack->BufferObj,
                                                MAP_INTERNAL);
   if (!buf) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glBitmap(PBO is mapped)");
      return NULL;
   }

   return ADD_POINTERS(buf, bitmap);
}

/**
 * Returns the low Y value of the vertical range given, flipped according to
 * whether the framebuffer is or not.
 */
static inline int
y_flip(struct gl_framebuffer *fb, int y, int height)
{
   if (_mesa_is_user_fbo(fb))
      return y;
   else
      return fb->Height - y - height;
}

/*
 * Render a bitmap.
 */
static bool
do_blit_bitmap( struct gl_context *ctx,
		GLint dstx, GLint dsty,
		GLsizei width, GLsizei height,
		const struct gl_pixelstore_attrib *unpack,
		const GLubyte *bitmap )
{
   struct brw_context *brw = brw_context(ctx);
   struct gl_framebuffer *fb = ctx->DrawBuffer;
   struct intel_renderbuffer *irb;
   union isl_color_value rasterColor;

   /* Update draw buffer bounds */
   _mesa_update_state(ctx);

   if (ctx->Depth.Test) {
      /* The blit path produces incorrect results when depth testing is on.
       * It seems the blit Z coord is always 1.0 (the far plane) so fragments
       * will likely be obscured by other, closer geometry.
       */
      return false;
   }

   intel_prepare_render(brw);

   if (fb->_NumColorDrawBuffers != 1) {
      perf_debug("accelerated glBitmap() only supports rendering to a "
                 "single color buffer\n");
      return false;
   }

   irb = intel_renderbuffer(fb->_ColorDrawBuffers[0]);

   if (_mesa_is_bufferobj(unpack->BufferObj)) {
      bitmap = map_pbo(ctx, width, height, unpack, bitmap);
      if (bitmap == NULL)
	 return true;	/* even though this is an error, we're done */
   }

   COPY_4V(rasterColor.f32, ctx->Current.RasterColor);

   if (_mesa_need_secondary_color(ctx)) {
       ADD_3V(rasterColor.f32, rasterColor.f32, ctx->Current.RasterSecondaryColor);
   }

   if (!intel_check_blit_fragment_ops(ctx, rasterColor.f32[3] == 1.0F))
      return false;

   /* Clip to buffer bounds and scissor. */
   if (!_mesa_clip_to_region(fb->_Xmin, fb->_Ymin,
			     fb->_Xmax, fb->_Ymax,
			     &dstx, &dsty, &width, &height))
      goto out;

   dsty = y_flip(fb, dsty, height);

   mesa_format src_format = MESA_FORMAT_R_UINT8;
   mesa_format dst_format = irb->mt->format;

   /* We can safely discard sRGB encode/decode for the DrawPixels interface */
   src_format = _mesa_get_srgb_format_linear(src_format);
   dst_format = _mesa_get_srgb_format_linear(dst_format);

   int src_stride = _mesa_image_row_stride(unpack, width, GL_RED,
                                           GL_UNSIGNED_BYTE);
   /* Mesa flips the src_stride for unpack->Invert, but we want our mt to have
    * a normal src_stride.
    */
   if (unpack->Invert) {
      src_stride = -src_stride;
   }


   GLuint src_offset = (GLintptr) bitmap;
   src_offset += _mesa_image_offset(2, unpack, width, height,
                                    GL_RED, GL_UNSIGNED_BYTE, 0, 0, 0);

   struct intel_mipmap_tree *pbo_mt =
                        intel_miptree_create(brw,
                                             GL_TEXTURE_2D,
                                             MESA_FORMAT_R_UINT8,
                                             0, 0,
                                             width, height, 1,
                                             1,
                                             MIPTREE_CREATE_LINEAR);

   if (!pbo_mt)
      return false;

   void *pbo_ptr;
   ptrdiff_t pbo_stride;
   intel_miptree_map(brw, pbo_mt, 0, 0, 0, 0, width, height,
                     GL_MAP_WRITE_BIT, &pbo_ptr, &pbo_stride);

   memset(pbo_ptr, 0, height * pbo_stride);

   /*
    * As mentioned state_tracker/st_cb_bitmap.c, this will
    * produce an upside-down bitmap (thanks to GL, as usual).
    * So we have to invert it.
    */
   pbo_ptr += (height - 1) * pbo_stride;
   _mesa_expand_bitmap(width, height, unpack,
                       bitmap, pbo_ptr, -pbo_stride, 0x1);
   intel_miptree_unmap(brw, pbo_mt, 0, 0);

   /*
    * float color is fine, use rasterColor for raster color
    * Don't worry, xmove and ymove are handled in entry point.
    */
   brw_blorp_bitmap(brw, irb->mt, irb->mt_level, irb->mt_layer,
                    dst_format,
                    dstx, dsty, width, height, rasterColor,
                    pbo_mt, src_format);

   if (ctx->Query.CurrentOcclusionObject)
      ctx->Query.CurrentOcclusionObject->Result += width * height;

out:

   if (unlikely(INTEL_DEBUG & DEBUG_SYNC))
      intel_batchbuffer_flush(brw);

   if (_mesa_is_bufferobj(unpack->BufferObj)) {
      /* done with PBO so unmap it now */
      ctx->Driver.UnmapBuffer(ctx, unpack->BufferObj, MAP_INTERNAL);
   }

   return true;
}


/* There are a large number of possible ways to implement bitmap on
 * this hardware, most of them have some sort of drawback.  Here are a
 * few that spring to mind:
 *
 * Blit:
 *    - XY_MONO_SRC_BLT_CMD
 *         - use XY_SETUP_CLIP_BLT for cliprect clipping.
 *    - XY_TEXT_BLT
 *    - XY_TEXT_IMMEDIATE_BLT
 *         - blit per cliprect, subject to maximum immediate data size.
 *    - XY_COLOR_BLT
 *         - per pixel or run of pixels
 *    - XY_PIXEL_BLT
 *         - good for sparse bitmaps
 *
 * 3D engine:
 *    - Point per pixel
 *    - Translate bitmap to an alpha texture and render as a quad
 *    - Chop bitmap up into 32x32 squares and render w/polygon stipple.
 */
void
intelBitmap(struct gl_context * ctx,
	    GLint x, GLint y,
	    GLsizei width, GLsizei height,
	    const struct gl_pixelstore_attrib *unpack,
	    const GLubyte * pixels)
{
   if (!_mesa_check_conditional_render(ctx))
      return;

//   if (do_blit_bitmap(ctx, x, y, width, height,
//                          unpack, pixels))
//      return;

   _mesa_meta_Bitmap(ctx, x, y, width, height, unpack, pixels);
}
