/*
 * Copyright 2015 Intel Corporation
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and/or sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice (including the next
 *  paragraph) shall be included in all copies or substantial portions of the
 *  Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 *  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 *  IN THE SOFTWARE.
 */

#include <assert.h>

#include "isl.h"

static inline bool
isl_format_has_channel_type(enum isl_format fmt, enum isl_base_type type)
{
   const struct isl_format_layout *fmtl = isl_format_get_layout(fmt);

   return fmtl->channels.r.type == type ||
          fmtl->channels.g.type == type ||
          fmtl->channels.b.type == type ||
          fmtl->channels.a.type == type ||
          fmtl->channels.l.type == type ||
          fmtl->channels.i.type == type ||
          fmtl->channels.p.type == type;
}

bool
isl_format_has_unorm_channel(enum isl_format fmt)
{
   return isl_format_has_channel_type(fmt, ISL_UNORM);
}

bool
isl_format_has_snorm_channel(enum isl_format fmt)
{
   return isl_format_has_channel_type(fmt, ISL_SNORM);
}

bool
isl_format_has_ufloat_channel(enum isl_format fmt)
{
   return isl_format_has_channel_type(fmt, ISL_UFLOAT);
}

bool
isl_format_has_sfloat_channel(enum isl_format fmt)
{
   return isl_format_has_channel_type(fmt, ISL_SFLOAT);
}

bool
isl_format_has_uint_channel(enum isl_format fmt)
{
   return isl_format_has_channel_type(fmt, ISL_UINT);
}

bool
isl_format_has_sint_channel(enum isl_format fmt)
{
   return isl_format_has_channel_type(fmt, ISL_SINT);
}

unsigned
isl_format_get_num_channels(enum isl_format fmt)
{
   const struct isl_format_layout *fmtl = isl_format_get_layout(fmt);

   assert(fmtl->channels.p.bits == 0);

   return (fmtl->channels.r.bits > 0) +
          (fmtl->channels.g.bits > 0) +
          (fmtl->channels.b.bits > 0) +
          (fmtl->channels.a.bits > 0) +
          (fmtl->channels.l.bits > 0) +
          (fmtl->channels.i.bits > 0);
}

enum isl_format
isl_sanitize_channel_select(enum isl_format format,
                            enum isl_channel_select channel_selects[4],
                            bool needs_a_alpha)
{
   /* Formats used in GL */
   bool is_intensity_fmt =
      isl_format_layouts[format].channels.i.type != ISL_VOID;
   bool is_luminance_fmt =
      isl_format_layouts[format].channels.a.type == ISL_VOID &&
      isl_format_layouts[format].channels.l.type != ISL_VOID;
   bool is_luminance_alpha_fmt =
      isl_format_layouts[format].channels.a.type != ISL_VOID &&
      isl_format_layouts[format].channels.l.type != ISL_VOID;
   bool is_alpha_fmt =
      isl_format_layouts[format].channels.r.type == ISL_VOID &&
      isl_format_layouts[format].channels.a.type != ISL_VOID &&
      isl_format_layouts[format].channels.l.type == ISL_VOID;

   /* Get the special requirements for each format */
   bool needs_rgb_red = is_luminance_fmt ||
                        is_intensity_fmt ||
                        is_luminance_alpha_fmt;
   bool needs_a_red = is_alpha_fmt ||
                      is_intensity_fmt;
   bool needs_a_green = is_luminance_alpha_fmt;

   for (int chan = 0; chan < 4; ++chan) {

      bool needs_data = false;
      switch (channel_selects[chan]) {
      case ISL_CHANNEL_SELECT_RED:
         needs_data = isl_format_layouts[format].channels.r.type == ISL_VOID;
         break;
      case ISL_CHANNEL_SELECT_GREEN:
         needs_data = isl_format_layouts[format].channels.g.type == ISL_VOID;
         break;
      case ISL_CHANNEL_SELECT_BLUE:
         needs_data = isl_format_layouts[format].channels.b.type == ISL_VOID;
         break;
      case ISL_CHANNEL_SELECT_ALPHA:
         needs_data = isl_format_layouts[format].channels.a.type == ISL_VOID;
         if (needs_a_alpha) {
            channel_selects[chan] = ISL_CHANNEL_SELECT_ALPHA;
         } else if (needs_a_red) {
            channel_selects[chan] = ISL_CHANNEL_SELECT_RED;
         } else if (needs_a_green) {
            channel_selects[chan] = ISL_CHANNEL_SELECT_GREEN;
         } else if (needs_data) {
            channel_selects[chan] = ISL_CHANNEL_SELECT_ONE;
         }
         continue;
      default:
         break;
      }

      if (needs_data)
         channel_selects[chan] = needs_rgb_red ?  ISL_CHANNEL_SELECT_RED :
                                                  ISL_CHANNEL_SELECT_ZERO;
   }

   /* TODO: create and use a lowering function to return the mapped format for
    * A, I, L, LA, Y formats.
    */
   return format;
}

enum isl_format
isl_format_rgb_to_rgba(enum isl_format rgb)
{
   assert(isl_format_is_rgb(rgb));

   switch (rgb) {
   case ISL_FORMAT_R32G32B32_FLOAT:    return ISL_FORMAT_R32G32B32A32_FLOAT;
   case ISL_FORMAT_R32G32B32_SINT:     return ISL_FORMAT_R32G32B32A32_SINT;
   case ISL_FORMAT_R32G32B32_UINT:     return ISL_FORMAT_R32G32B32A32_UINT;
   case ISL_FORMAT_R32G32B32_UNORM:    return ISL_FORMAT_R32G32B32A32_UNORM;
   case ISL_FORMAT_R32G32B32_SNORM:    return ISL_FORMAT_R32G32B32A32_SNORM;
   case ISL_FORMAT_R32G32B32_SSCALED:  return ISL_FORMAT_R32G32B32A32_SSCALED;
   case ISL_FORMAT_R32G32B32_USCALED:  return ISL_FORMAT_R32G32B32A32_USCALED;
   case ISL_FORMAT_R32G32B32_SFIXED:   return ISL_FORMAT_R32G32B32A32_SFIXED;
   case ISL_FORMAT_R8G8B8_UNORM:       return ISL_FORMAT_R8G8B8A8_UNORM;
   case ISL_FORMAT_R8G8B8_SNORM:       return ISL_FORMAT_R8G8B8A8_SNORM;
   case ISL_FORMAT_R8G8B8_SSCALED:     return ISL_FORMAT_R8G8B8A8_SSCALED;
   case ISL_FORMAT_R8G8B8_USCALED:     return ISL_FORMAT_R8G8B8A8_USCALED;
   case ISL_FORMAT_R16G16B16_FLOAT:    return ISL_FORMAT_R16G16B16A16_FLOAT;
   case ISL_FORMAT_R16G16B16_UNORM:    return ISL_FORMAT_R16G16B16A16_UNORM;
   case ISL_FORMAT_R16G16B16_SNORM:    return ISL_FORMAT_R16G16B16A16_SNORM;
   case ISL_FORMAT_R16G16B16_SSCALED:  return ISL_FORMAT_R16G16B16A16_SSCALED;
   case ISL_FORMAT_R16G16B16_USCALED:  return ISL_FORMAT_R16G16B16A16_USCALED;
   case ISL_FORMAT_R8G8B8_UNORM_SRGB:  return ISL_FORMAT_R8G8B8A8_UNORM_SRGB;
   case ISL_FORMAT_R16G16B16_UINT:     return ISL_FORMAT_R16G16B16A16_UINT;
   case ISL_FORMAT_R16G16B16_SINT:     return ISL_FORMAT_R16G16B16A16_SINT;
   case ISL_FORMAT_R8G8B8_UINT:        return ISL_FORMAT_R8G8B8A8_UINT;
   case ISL_FORMAT_R8G8B8_SINT:        return ISL_FORMAT_R8G8B8A8_SINT;
   default:
      return ISL_FORMAT_UNSUPPORTED;
   }
}
