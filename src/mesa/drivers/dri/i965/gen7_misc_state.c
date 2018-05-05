/*
 * Copyright © 2011 Intel Corporation
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

#include "main/mtypes.h"
#include "intel_batchbuffer.h"
#include "intel_mipmap_tree.h"
#include "intel_fbo.h"
#include "brw_context.h"
#include "brw_state.h"
#include "brw_defines.h"

/**
 * \see brw_context.state.depth_region
 */
const struct brw_tracked_state gen7_depthbuffer = {
   .dirty = {
      .mesa = _NEW_BUFFERS |
              _NEW_DEPTH |
              _NEW_STENCIL,
      .brw = BRW_NEW_BATCH |
             BRW_NEW_BLORP,
   },
   .emit = brw_emit_depthbuffer,
};
