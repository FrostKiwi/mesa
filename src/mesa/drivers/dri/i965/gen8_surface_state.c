/*
 * Copyright © 2012 Intel Corporation
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

#include "main/blend.h"
#include "main/mtypes.h"
#include "main/samplerobj.h"
#include "main/texformat.h"
#include "main/teximage.h"
#include "program/prog_parameter.h"
#include "program/prog_instruction.h"

#include "intel_mipmap_tree.h"
#include "intel_batchbuffer.h"
#include "intel_tex.h"
#include "intel_fbo.h"
#include "intel_buffer_objects.h"
#include "intel_image.h"

#include "brw_context.h"
#include "brw_state.h"
#include "brw_defines.h"
#include "brw_wm.h"
#include "isl/isl.h"

/**
 * Convert an swizzle enumeration (i.e. SWIZZLE_X) to one of the Gen7.5+
 * "Shader Channel Select" enumerations (i.e. HSW_SCS_RED).  The mappings are
 *
 * SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W, SWIZZLE_ZERO, SWIZZLE_ONE
 *         0          1          2          3             4            5
 *         4          5          6          7             0            1
 *   SCS_RED, SCS_GREEN,  SCS_BLUE, SCS_ALPHA,     SCS_ZERO,     SCS_ONE
 *
 * which is simply adding 4 then modding by 8 (or anding with 7).
 */
static unsigned
swizzle_to_scs(unsigned swizzle)
{
   return (swizzle + 4) & 7;
}

static uint32_t
surface_tiling_resource_mode(uint32_t tr_mode)
{
   switch (tr_mode) {
   case INTEL_MIPTREE_TRMODE_YF:
      return GEN9_SURFACE_TRMODE_TILEYF;
   case INTEL_MIPTREE_TRMODE_YS:
      return GEN9_SURFACE_TRMODE_TILEYS;
   default:
      return GEN9_SURFACE_TRMODE_NONE;
   }
}

uint32_t
gen8_surface_tiling_mode(uint32_t tiling)
{
   switch (tiling) {
   case I915_TILING_X:
      return GEN8_SURFACE_TILING_X;
   case I915_TILING_Y:
      return GEN8_SURFACE_TILING_Y;
   default:
      return GEN8_SURFACE_TILING_NONE;
   }
}

unsigned
gen8_vertical_alignment(const struct brw_context *brw,
                        const struct intel_mipmap_tree *mt,
                        uint32_t surf_type)
{
   /* On Gen9+ vertical alignment is ignored for 1D surfaces and when
    * tr_mode is not TRMODE_NONE. Set to an arbitrary non-reserved value.
    */
   if (brw->gen > 8 &&
       (mt->tr_mode != INTEL_MIPTREE_TRMODE_NONE ||
        surf_type == BRW_SURFACE_1D))
      return GEN8_SURFACE_VALIGN_4;

   switch (mt->valign) {
   case 4:
      return GEN8_SURFACE_VALIGN_4;
   case 8:
      return GEN8_SURFACE_VALIGN_8;
   case 16:
      return GEN8_SURFACE_VALIGN_16;
   default:
      unreachable("Unsupported vertical surface alignment.");
   }
}

unsigned
gen8_horizontal_alignment(const struct brw_context *brw,
                          const struct intel_mipmap_tree *mt,
                          uint32_t surf_type)
{
   /* On Gen9+ horizontal alignment is ignored when tr_mode is not
    * TRMODE_NONE. Set to an arbitrary non-reserved value.
    */
   if (brw->gen > 8 &&
       (mt->tr_mode != INTEL_MIPTREE_TRMODE_NONE ||
        gen9_use_linear_1d_layout(brw, mt)))
      return GEN8_SURFACE_HALIGN_4;

   switch (mt->halign) {
   case 4:
      return GEN8_SURFACE_HALIGN_4;
   case 8:
      return GEN8_SURFACE_HALIGN_8;
   case 16:
      return GEN8_SURFACE_HALIGN_16;
   default:
      unreachable("Unsupported horizontal surface alignment.");
   }
}

uint32_t *
gen8_allocate_surface_state(struct brw_context *brw,
                            uint32_t *out_offset, int index)
{
   int dwords = brw->gen >= 9 ? 16 : 13;
   uint32_t *surf = __brw_state_batch(brw, AUB_TRACE_SURFACE_STATE,
                                      dwords * 4, 64, index, out_offset);
   memset(surf, 0, dwords * 4);
   return surf;
}

void
gen8_emit_fast_clear_color(const struct brw_context *brw,
                           const struct intel_mipmap_tree *mt,
                           uint32_t *surf)
{
   if (brw->gen >= 9) {
      surf[12] = mt->gen9_fast_clear_color.ui[0];
      surf[13] = mt->gen9_fast_clear_color.ui[1];
      surf[14] = mt->gen9_fast_clear_color.ui[2];
      surf[15] = mt->gen9_fast_clear_color.ui[3];
   } else
      surf[7] |= mt->fast_clear_color_value;
}

uint32_t
gen8_get_aux_mode(const struct brw_context *brw,
                  const struct intel_mipmap_tree *mt)
{
   if (mt->mcs_mt == NULL)
      return GEN8_SURFACE_AUX_MODE_NONE;

   /*
    * From the BDW PRM, Volume 2d, page 260 (RENDER_SURFACE_STATE):
    * "When MCS is enabled for non-MSRT, HALIGN_16 must be used"
    *
    * From the hardware spec for GEN9:
    * "When Auxiliary Surface Mode is set to AUX_CCS_D or AUX_CCS_E, HALIGN
    *  16 must be used."
    */
   if (brw->gen >= 9 || mt->num_samples == 1)
      assert(mt->halign == 16);

   if (intel_miptree_is_lossless_compressed(brw, mt))
      return GEN9_SURFACE_AUX_MODE_CCS_E;

   return GEN8_SURFACE_AUX_MODE_MCS;
}

static void
gen8_emit_texture_surface_state(struct brw_context *brw,
                                struct intel_mipmap_tree *mt,
                                GLenum target,
                                unsigned min_layer, unsigned max_layer,
                                unsigned min_level, unsigned max_level,
                                unsigned format,
                                unsigned swizzle,
                                uint32_t *surf_offset, int surf_index,
                                bool rw, bool for_gather)
{
   const unsigned depth = max_layer - min_layer;
   struct intel_mipmap_tree *aux_mt = mt->mcs_mt;
   uint32_t mocs_wb = brw->gen >= 9 ? SKL_MOCS_WB : BDW_MOCS_WB;
   unsigned tiling_mode, pitch;
   const unsigned tr_mode = surface_tiling_resource_mode(mt->tr_mode);
   const uint32_t surf_type = translate_tex_target(target);
   uint32_t aux_mode = gen8_get_aux_mode(brw, mt);

   if (mt->format == MESA_FORMAT_S_UINT8) {
      tiling_mode = GEN8_SURFACE_TILING_W;
      pitch = 2 * mt->pitch;
   } else {
      tiling_mode = gen8_surface_tiling_mode(mt->tiling);
      pitch = mt->pitch;
   }

   /* Prior to Gen9, MCS is not uploaded for single-sampled surfaces because
    * the color buffer should always have been resolved before it is used as
    * a texture so there is no need for it. On Gen9 it will be uploaded when
    * the surface is losslessly compressed (CCS_E).
    * However, sampling engine is not capable of re-interpreting the
    * underlying color buffer in non-compressible formats when the surface
    * is configured as compressed. Therefore state upload has made sure the
    * buffer is in resolved state allowing the surface to be configured as
    * non-compressed.
    */
   if (mt->num_samples <= 1 &&
       (aux_mode != GEN9_SURFACE_AUX_MODE_CCS_E ||
        !isl_format_supports_lossless_compression(
            brw->intelScreen->devinfo, format))) {
      assert(!mt->mcs_mt ||
             mt->fast_clear_state == INTEL_FAST_CLEAR_STATE_RESOLVED);
      aux_mt = NULL;
      aux_mode = GEN8_SURFACE_AUX_MODE_NONE;
   }

   uint32_t *surf = gen8_allocate_surface_state(brw, surf_offset, surf_index);

   surf[0] = SET_FIELD(surf_type, BRW_SURFACE_TYPE) |
             format << BRW_SURFACE_FORMAT_SHIFT |
             gen8_vertical_alignment(brw, mt, surf_type) |
             gen8_horizontal_alignment(brw, mt, surf_type) |
             tiling_mode;

   if (surf_type == BRW_SURFACE_CUBE) {
      surf[0] |= BRW_SURFACE_CUBEFACE_ENABLES;
   }

   /* From the CHV PRM, Volume 2d, page 321 (RENDER_SURFACE_STATE dword 0
    * bit 9 "Sampler L2 Bypass Mode Disable" Programming Notes):
    *
    *    This bit must be set for the following surface types: BC2_UNORM
    *    BC3_UNORM BC5_UNORM BC5_SNORM BC7_UNORM
    */
   if ((brw->gen >= 9 || brw->is_cherryview) &&
       (format == BRW_SURFACEFORMAT_BC2_UNORM ||
        format == BRW_SURFACEFORMAT_BC3_UNORM ||
        format == BRW_SURFACEFORMAT_BC5_UNORM ||
        format == BRW_SURFACEFORMAT_BC5_SNORM ||
        format == BRW_SURFACEFORMAT_BC7_UNORM))
      surf[0] |= GEN8_SURFACE_SAMPLER_L2_BYPASS_DISABLE;

   if (_mesa_is_array_texture(mt->target) || mt->target == GL_TEXTURE_CUBE_MAP)
      surf[0] |= GEN8_SURFACE_IS_ARRAY;

   surf[1] = SET_FIELD(mocs_wb, GEN8_SURFACE_MOCS) | mt->qpitch >> 2;

   surf[2] = SET_FIELD(mt->logical_width0 - 1, GEN7_SURFACE_WIDTH) |
             SET_FIELD(mt->logical_height0 - 1, GEN7_SURFACE_HEIGHT);

   surf[3] = SET_FIELD(depth - 1, BRW_SURFACE_DEPTH) | (pitch - 1);

   surf[4] = gen7_surface_msaa_bits(mt->num_samples, mt->msaa_layout) |
             SET_FIELD(min_layer, GEN7_SURFACE_MIN_ARRAY_ELEMENT) |
             SET_FIELD(depth - 1, GEN7_SURFACE_RENDER_TARGET_VIEW_EXTENT);

   surf[5] = SET_FIELD(min_level - mt->first_level, GEN7_SURFACE_MIN_LOD) |
             (max_level - min_level - 1); /* mip count */

   if (brw->gen >= 9) {
      surf[5] |= SET_FIELD(tr_mode, GEN9_SURFACE_TRMODE);
      /* Disable Mip Tail by setting a large value. */
      surf[5] |= SET_FIELD(15, GEN9_SURFACE_MIP_TAIL_START_LOD);
   }

   if (aux_mt) {
      uint32_t tile_w, tile_h;
      assert(aux_mt->tiling == I915_TILING_Y);
      intel_get_tile_dims(aux_mt->tiling, aux_mt->tr_mode,
                          aux_mt->cpp, &tile_w, &tile_h);
      surf[6] = SET_FIELD(aux_mt->qpitch / 4, GEN8_SURFACE_AUX_QPITCH) |
                SET_FIELD((aux_mt->pitch / tile_w) - 1,
                          GEN8_SURFACE_AUX_PITCH) |
                aux_mode;
   }

   gen8_emit_fast_clear_color(brw, mt, surf);
   surf[7] |=
      SET_FIELD(swizzle_to_scs(GET_SWZ(swizzle, 0)), GEN7_SURFACE_SCS_R) |
      SET_FIELD(swizzle_to_scs(GET_SWZ(swizzle, 1)), GEN7_SURFACE_SCS_G) |
      SET_FIELD(swizzle_to_scs(GET_SWZ(swizzle, 2)), GEN7_SURFACE_SCS_B) |
      SET_FIELD(swizzle_to_scs(GET_SWZ(swizzle, 3)), GEN7_SURFACE_SCS_A);

   *((uint64_t *) &surf[8]) = mt->bo->offset64 + mt->offset; /* reloc */

   if (aux_mt) {
      *((uint64_t *) &surf[10]) = aux_mt->bo->offset64;
      drm_intel_bo_emit_reloc(brw->batch.bo, *surf_offset + 10 * 4,
                              aux_mt->bo, 0,
                              I915_GEM_DOMAIN_SAMPLER,
                              (rw ? I915_GEM_DOMAIN_SAMPLER : 0));
   }

   /* Emit relocation to surface contents */
   drm_intel_bo_emit_reloc(brw->batch.bo,
                           *surf_offset + 8 * 4,
                           mt->bo,
                           mt->offset,
                           I915_GEM_DOMAIN_SAMPLER,
                           (rw ? I915_GEM_DOMAIN_SAMPLER : 0));
}

/**
 * Creates a null surface.
 *
 * This is used when the shader doesn't write to any color output.  An FB
 * write to target 0 will still be emitted, because that's how the thread is
 * terminated (and computed depth is returned), so we need to have the
 * hardware discard the target 0 color output..
 */
static void
gen8_emit_null_surface_state(struct brw_context *brw,
                             unsigned width,
                             unsigned height,
                             unsigned samples,
                             uint32_t *out_offset)
{
   uint32_t *surf = gen8_allocate_surface_state(brw, out_offset, -1);

   surf[0] = BRW_SURFACE_NULL << BRW_SURFACE_TYPE_SHIFT |
             BRW_SURFACEFORMAT_B8G8R8A8_UNORM << BRW_SURFACE_FORMAT_SHIFT |
             GEN8_SURFACE_TILING_Y;
   surf[2] = SET_FIELD(width - 1, GEN7_SURFACE_WIDTH) |
             SET_FIELD(height - 1, GEN7_SURFACE_HEIGHT);
}

void
gen8_init_vtable_surface_functions(struct brw_context *brw)
{
   brw->vtbl.update_texture_surface = brw_update_texture_surface;
   brw->vtbl.update_renderbuffer_surface = brw_update_renderbuffer_surface;
   brw->vtbl.emit_null_surface_state = gen8_emit_null_surface_state;
   brw->vtbl.emit_texture_surface_state = gen8_emit_texture_surface_state;
}
