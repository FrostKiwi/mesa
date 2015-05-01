/* -*- c++ -*- */
/*
 * Copyright © 2013-2015 Intel Corporation
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

#ifndef BRW_FS_SURFACE_BUILDER_H
#define BRW_FS_SURFACE_BUILDER_H

#include "brw_fs_builder.h"
#include "brw_context.h"

namespace brw {
   namespace surface_access {
      fs_reg
      emit_untyped_read(const fs_builder &bld,
                        const fs_reg &surface, const fs_reg &addr,
                        unsigned dims, unsigned size,
                        brw_predicate pred = BRW_PREDICATE_NONE);

      void
      emit_untyped_write(const fs_builder &bld, const fs_reg &surface,
                         const fs_reg &addr, const fs_reg &src,
                         unsigned dims, unsigned size,
                         brw_predicate pred = BRW_PREDICATE_NONE);

      fs_reg
      emit_untyped_atomic(const fs_builder &bld,
                          const fs_reg &surface, const fs_reg &addr,
                          const fs_reg &src0, const fs_reg &src1,
                          unsigned dims, unsigned rsize, unsigned op,
                          brw_predicate pred = BRW_PREDICATE_NONE);

      fs_reg
      emit_typed_read(const fs_builder &bld, const fs_reg &surface,
                      const fs_reg &addr, unsigned dims, unsigned size);

      void
      emit_typed_write(const fs_builder &bld, const fs_reg &surface,
                       const fs_reg &addr, const fs_reg &src,
                       unsigned dims, unsigned size);

      fs_reg
      emit_typed_atomic(const fs_builder &bld, const fs_reg &surface,
                        const fs_reg &addr,
                        const fs_reg &src0, const fs_reg &src1,
                        unsigned dims, unsigned rsize, unsigned op,
                        brw_predicate pred = BRW_PREDICATE_NONE);
   }

   namespace image_format_info {
      /**
       * Simple 4-tuple of scalars used to pass around per-color component
       * values.
       */
      struct color_u {
         color_u(unsigned x = 0) : r(x), g(x), b(x), a(x)
         {
         }

         color_u(unsigned r, unsigned g, unsigned b, unsigned a) :
            r(r), g(g), b(b), a(a)
         {
         }

         unsigned
         operator[](unsigned i) const
         {
            const unsigned xs[] = { r, g, b, a };
            return xs[i];
         }

         unsigned r, g, b, a;
      };

      /**
       * Return the per-channel bitfield widths for a given image format.
       */
      inline color_u
      get_bit_widths(mesa_format format)
      {
         return color_u(_mesa_get_format_bits(format, GL_RED_BITS),
                        _mesa_get_format_bits(format, GL_GREEN_BITS),
                        _mesa_get_format_bits(format, GL_BLUE_BITS),
                        _mesa_get_format_bits(format, GL_ALPHA_BITS));
      }

      /**
       * Return the per-channel bitfield shifts for a given image format.
       */
      inline color_u
      get_bit_shifts(mesa_format format)
      {
         const color_u widths = get_bit_widths(format);
         return color_u(0, widths.r, widths.r + widths.g,
                        widths.r + widths.g + widths.b);
      }

      /**
       * Return true if all present components have the same bit width.
       */
      inline bool
      is_homogeneous(mesa_format format)
      {
         const color_u widths = get_bit_widths(format);
         return ((widths.g == 0 || widths.g == widths.r) &&
                 (widths.b == 0 || widths.b == widths.r) &&
                 (widths.a == 0 || widths.a == widths.r));
      }

      /**
       * Return true if the format conversion boils down to a trivial copy.
       */
      inline bool
      is_conversion_trivial(const brw_device_info *devinfo, mesa_format format)
      {
         return (get_bit_widths(format).r == 32 && is_homogeneous(format)) ||
                 format == brw_lower_mesa_image_format(devinfo, format);
      }

      /**
       * Return true if the hardware natively supports some format with
       * compatible bitfield layout, but possibly different data types.
       */
      inline bool
      has_supported_bit_layout(const brw_device_info *devinfo,
                               mesa_format format)
      {
         const color_u widths = get_bit_widths(format);
         const color_u lower_widths = get_bit_widths(
            brw_lower_mesa_image_format(devinfo, format));

         return (widths.r == lower_widths.r &&
                 widths.g == lower_widths.g &&
                 widths.b == lower_widths.b &&
                 widths.a == lower_widths.a);
      }

      /**
       * Return true if we are required to spread individual components over
       * several components of the format used by the hardware (RG32 and
       * friends implemented as RGBA16UI).
       */
      inline bool
      has_split_bit_layout(const brw_device_info *devinfo, mesa_format format)
      {
         const mesa_format lower_format =
            brw_lower_mesa_image_format(devinfo, format);

         return (_mesa_format_num_components(format) <
                 _mesa_format_num_components(lower_format));
      }

      /**
       * Return true unless we have to fall back to untyped surface access.
       * Fail!
       */
      inline bool
      has_matching_typed_format(const brw_device_info *devinfo,
                                mesa_format format)
      {
         return (_mesa_get_format_bytes(format) <= 4 ||
                 (_mesa_get_format_bytes(format) <= 8 &&
                  (devinfo->gen >= 8 || devinfo->is_haswell)) ||
                 devinfo->gen >= 9);
      }

      /**
       * Return true if the hardware returns garbage in the unused high bits
       * of each component.  This may happen on IVB because we rely on the
       * undocumented behavior that typed reads from surfaces of the
       * unsupported R8 and R16 formats return useful data in their least
       * significant bits.
       */
      inline bool
      has_undefined_high_bits(const brw_device_info *devinfo,
                              mesa_format format)
      {
         const mesa_format lower_format =
            brw_lower_mesa_image_format(devinfo, format);

         return (devinfo->gen == 7 && !devinfo->is_haswell &&
                 (lower_format == MESA_FORMAT_R_UINT16 ||
                  lower_format == MESA_FORMAT_R_UINT8));
      }

      /**
       * Return true if the format represents values as signed integers
       * requiring sign extension when unpacking.
       */
      inline bool
      needs_sign_extension(mesa_format format)
      {
         return (_mesa_get_format_datatype(format) == GL_SIGNED_NORMALIZED ||
                 _mesa_get_format_datatype(format) == GL_INT);
      }
   }

   namespace image_access {
      fs_reg
      emit_image_load(const fs_builder &bld,
                      const fs_reg &image, const fs_reg &addr,
                      mesa_format format, unsigned dims);

      void
      emit_image_store(const fs_builder &bld, const fs_reg &image,
                       const fs_reg &addr, const fs_reg &src,
                       mesa_format format, unsigned dims);
      fs_reg
      emit_image_atomic(const fs_builder &bld,
                        const fs_reg &image, const fs_reg &addr,
                        const fs_reg &src0, const fs_reg &src1,
                        unsigned dims, unsigned rsize, unsigned op);
   }
}
#endif
