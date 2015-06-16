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

#include "brw_fs_surface_builder.h"

using namespace brw;

namespace {
   namespace array_utils {
      /**
       * Interleave logical components from the given arguments.  If two
       * arguments are provided \p size components will be copied from each to
       * the even and odd components of the result respectively.
       *
       * It may be safely used to merge the two halves of a value calculated
       * separately.
       */
      fs_reg
      emit_zip(const fs_builder &bld,
               const fs_reg &src0, const fs_reg &src1,
               unsigned size)
      {
         const unsigned n = (src0.file != BAD_FILE) + (src1.file != BAD_FILE);
         const fs_reg srcs[] = { src0, src1 };
         const fs_reg dst = size * n == 0 ? fs_reg() :
            bld.vgrf(src0.type, size * n);

         for (unsigned i = 0; i < size; ++i) {
            for (unsigned j = 0; j < n; ++j)
               set_exec_all(bld.MOV(offset(dst, j + i * n),
                                    offset(srcs[j], i)));
         }

         return dst;
      }
   }
}

namespace brw {
   namespace surface_access {
      namespace {
         using namespace array_utils;

         /**
          * Generate a send opcode for a surface message and return the
          * result.
          */
         fs_inst *
         emit_send(const fs_builder &bld, enum opcode opcode,
                   const fs_reg &dst, const fs_reg &surface, const fs_reg &addr,
                   unsigned dims, unsigned arg,
                   const fs_reg &src0 = fs_reg(), const fs_reg &src1 = fs_reg())
         {
            /* Reduce the dynamically uniform surface index to a single
             * scalar.
             */
            const fs_reg usurface =
               component(bld.vgrf(BRW_REGISTER_TYPE_UD), 0);
            bld.emit_uniformize(usurface, surface);

            fs_reg regs[] = {
               surface,
               addr,
               fs_reg(dims),
               fs_reg(arg),
               src0,
               src1
            };

            int n = 4 + (src0.file != BAD_FILE) + (src1.file != BAD_FILE);

            fs_inst *inst = bld.emit(opcode, dst, srcs, n);

            return inst;
         }
      }

      /**
       * Emit an untyped surface read opcode.  \p dims determines the number
       * of components of the address and \p size the number of components of
       * the returned value.
       */
      fs_reg
      emit_untyped_read(const fs_builder &bld,
                        const fs_reg &surface, const fs_reg &addr,
                        unsigned dims, unsigned size,
                        brw_predicate pred)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, size);

         fs_inst *inst =
            emit_send(bld, SHADER_OPCODE_UNTYPED_SURFACE_READ_LOGICAL,
                      dst, surface, addr, dims, size);
         inst->regs_written = size * bld.dispatch_width() / 8;
         inst->predicate = pred;

         return dst;
      }

      /**
       * Emit an untyped surface write opcode.  \p dims determines the number
       * of components of the address and \p size the number of components of
       * the argument.
       */
      void
      emit_untyped_write(const fs_builder &bld, const fs_reg &surface,
                         const fs_reg &addr, const fs_reg &src,
                         unsigned dims, unsigned size,
                         brw_predicate pred)
      {
         fs_inst *inst =
            emit_send(bld, SHADER_OPCODE_UNTYPED_SURFACE_WRITE_LOGICAL,
                      bld.null_reg_ud(), surface, addr, dims, size, 0);
         inst->predicate = pred;
      }

      /**
       * Emit an untyped surface atomic opcode.  \p dims determines the number
       * of components of the address and \p rsize the number of components of
       * the returned value (either zero or one).
       */
      fs_reg
      emit_untyped_atomic(const fs_builder &bld,
                          const fs_reg &surface, const fs_reg &addr,
                          const fs_reg &src0, const fs_reg &src1,
                          unsigned dims, unsigned rsize, unsigned op,
                          brw_predicate pred)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, 1);

         fs_inst *inst =
            emit_send(bld, SHADER_OPCODE_UNTYPED_ATOMIC_LOGICAL,
                      dst, surface, addr, dims, op, src0, src1);
         inst->regs_written = bld.dispatch_width() / 8;
         inst->predicate = pred;

         return dst;
      }

      /**
       * Emit a typed surface read opcode.  \p dims determines the number of
       * components of the address and \p size the number of components of the
       * returned value.
       */
      fs_reg
      emit_typed_read(const fs_builder &bld, const fs_reg &surface,
                      const fs_reg &addr, unsigned dims, unsigned size)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, size);

         fs_inst *inst =
            emit_send(bld, SHADER_OPCODE_TYPED_SURFACE_READ_LOGICAL,
                      dst, surface, addr, dims, size,
                      size * bld.dispatch_width() / 8);
         inst->regs_written = size * bld.dispatch_width() / 8;

         return dst;
      }

      /**
       * Emit a typed surface write opcode.  \p dims determines the number of
       * components of the address and \p size the number of components of the
       * argument.
       */
      void
      emit_typed_write(const fs_builder &bld, const fs_reg &surface,
                       const fs_reg &addr, const fs_reg &src,
                       unsigned dims, unsigned size)
      {
         emit_send(bld, SHADER_OPCODE_UNTYPED_SURFACE_WRITE_LOGICAL,
                   bld.null_reg_ud(), surface, addr, dims, size, 0);
      }

      /**
       * Emit a typed surface atomic opcode.  \p dims determines the number of
       * components of the address and \p rsize the number of components of
       * the returned value (either zero or one).
       */
      fs_reg
      emit_typed_atomic(const fs_builder &bld, const fs_reg &surface,
                        const fs_reg &addr,
                        const fs_reg &src0, const fs_reg &src1,
                        unsigned dims, unsigned rsize, unsigned op,
                        brw_predicate pred)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, 1);

         fs_inst *inst =
            emit_send(bld, SHADER_OPCODE_UNTYPED_ATOMIC_LOGICAL,
                      dst, surface, addr, dims, op, src0, src1);
         inst->regs_written = bld.dispatch_width() / 8;

         return dst;
      }
   }
}

namespace {
   namespace image_validity {
      /**
       * Check whether there is an image bound at the given index and write
       * the comparison result to f0.0.  Returns an appropriate predication
       * mode to use on subsequent image operations.
       */
      brw_predicate
      emit_surface_check(const fs_builder &bld, const fs_reg &image)
      {
         const brw_device_info *devinfo = bld.devinfo;
         const fs_reg size = offset(image, BRW_IMAGE_PARAM_SIZE_OFFSET);

         if (devinfo->gen == 7 && !devinfo->is_haswell) {
            /* Check the first component of the size field to find out if the
             * image is bound.  Necessary on IVB for typed atomics because
             * they don't seem to respect null surfaces and will happily
             * corrupt or read random memory when no image is bound.
             */
            bld.CMP(bld.null_reg_ud(),
                    retype(size, BRW_REGISTER_TYPE_UD),
                    fs_reg(0), BRW_CONDITIONAL_NZ);

            return BRW_PREDICATE_NORMAL;
         } else {
            /* More recent platforms implement compliant behavior when a null
             * surface is bound.
             */
            return BRW_PREDICATE_NONE;
         }
      }

      /**
       * Check whether the provided coordinates are within the image bounds
       * and write the comparison result to f0.0.  Returns an appropriate
       * predication mode to use on subsequent image operations.
       */
      brw_predicate
      emit_bounds_check(const fs_builder &bld, const fs_reg &image,
                        const fs_reg &addr, unsigned dims)
      {
         const fs_reg size = offset(image, BRW_IMAGE_PARAM_SIZE_OFFSET);

         for (unsigned c = 0; c < dims; ++c)
            set_predicate(c == 0 ? BRW_PREDICATE_NONE : BRW_PREDICATE_NORMAL,
                          bld.CMP(bld.null_reg_ud(),
                                  offset(retype(addr, BRW_REGISTER_TYPE_UD), c),
                                  offset(size, c),
                                  BRW_CONDITIONAL_L));

         return BRW_PREDICATE_NORMAL;
      }
   }

   namespace image_coordinates {
      /**
       * Calculate the offset in memory of the texel given by \p coord.
       *
       * This is meant to be used with untyped surface messages to access a
       * tiled surface, what involves taking into account the tiling and
       * swizzling modes of the surface manually so it will hopefully not
       * happen very often.
       */
      fs_reg
      emit_address_calculation(const fs_builder &bld, const fs_reg &image,
                               const fs_reg &coord, unsigned dims)
      {
         const fs_reg off = offset(image, BRW_IMAGE_PARAM_OFFSET_OFFSET);
         const fs_reg stride = offset(image, BRW_IMAGE_PARAM_STRIDE_OFFSET);
         const fs_reg tile = offset(image, BRW_IMAGE_PARAM_TILING_OFFSET);
         const fs_reg swz = offset(image, BRW_IMAGE_PARAM_SWIZZLING_OFFSET);
         const fs_reg addr = bld.vgrf(BRW_REGISTER_TYPE_UD, 2);
         const fs_reg tmp = bld.vgrf(BRW_REGISTER_TYPE_UD, 2);
         const fs_reg minor = bld.vgrf(BRW_REGISTER_TYPE_UD, 2);
         const fs_reg major = bld.vgrf(BRW_REGISTER_TYPE_UD, 2);
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD);

         /* Shift the coordinates by the fixed surface offset. */
         for (unsigned c = 0; c < 2; ++c)
            bld.ADD(offset(addr, c), offset(off, c),
                    (c < dims ? offset(retype(coord, BRW_REGISTER_TYPE_UD), c) :
                     fs_reg(0)));

         if (dims > 2) {
            /* Decompose z into a major (tmp.y) and a minor (tmp.x)
             * index.
             */
            bld.BFE(offset(tmp, 0), offset(tile, 2), fs_reg(0),
                    offset(retype(coord, BRW_REGISTER_TYPE_UD), 2));
            bld.SHR(offset(tmp, 1),
                    offset(retype(coord, BRW_REGISTER_TYPE_UD), 2),
                    offset(tile, 2));

            /* Take into account the horizontal (tmp.x) and vertical (tmp.y)
             * slice offset.
             */
            for (unsigned c = 0; c < 2; ++c) {
               bld.MUL(offset(tmp, c), offset(stride, 2 + c), offset(tmp, c));
               bld.ADD(offset(addr, c), offset(addr, c), offset(tmp, c));
            }
         }

         if (dims > 1) {
            for (unsigned c = 0; c < 2; ++c) {
               /* Calculate the minor x and y indices. */
               bld.BFE(offset(minor, c), offset(tile, c),
                       fs_reg(0), offset(addr, c));

               /* Calculate the major x and y indices. */
               bld.SHR(offset(major, c), offset(addr, c), offset(tile, c));
            }

            /* Calculate the texel index from the start of the tile row and
             * the vertical coordinate of the row.
             * Equivalent to:
             *   tmp.x = (major.x << tile.y << tile.x) +
             *           (minor.y << tile.x) + minor.x
             *   tmp.y = major.y << tile.y
             */
            bld.SHL(tmp, major, offset(tile, 1));
            bld.ADD(tmp, tmp, offset(minor, 1));
            bld.SHL(tmp, tmp, offset(tile, 0));
            bld.ADD(tmp, tmp, minor);
            bld.SHL(offset(tmp, 1), offset(major, 1), offset(tile, 1));

            /* Add it to the start of the tile row. */
            bld.MUL(offset(tmp, 1), offset(tmp, 1), offset(stride, 1));
            bld.ADD(tmp, tmp, offset(tmp, 1));

            /* Multiply by the Bpp value. */
            bld.MUL(dst, tmp, stride);

            if (bld.devinfo->gen < 8 && !bld.devinfo->is_baytrail) {
               /* Take into account the two dynamically specified shifts. */
               for (unsigned c = 0; c < 2; ++c)
                  bld.SHR(offset(tmp, c), dst, offset(swz, c));

               /* XOR tmp.x and tmp.y with bit 6 of the memory address. */
               bld.XOR(tmp, tmp, offset(tmp, 1));
               bld.AND(tmp, tmp, fs_reg(1 << 6));
               bld.XOR(dst, dst, tmp);
            }

         } else {
            /* Multiply by the Bpp/stride value. */
            bld.MUL(offset(addr, 1), offset(addr, 1), offset(stride, 1));
            bld.ADD(addr, addr, offset(addr, 1));
            bld.MUL(dst, addr, stride);
         }

         return dst;
      }
   }

   namespace image_format_conversion {
      using image_format_info::color_u;

      namespace {
         /**
          * Maximum representable value in an unsigned integer with the given
          * number of bits.
          */
         inline unsigned
         scale(unsigned n)
         {
            return (1 << n) - 1;
         }
      }

      /**
       * Pack the vector \p src in a bitfield given the per-component bit
       * shifts and widths.
       */
      fs_reg
      emit_pack(const fs_builder &bld, const fs_reg &src,
                const color_u &shifts, const color_u &widths)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, 4);
         bool seen[4] = {};

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               const fs_reg tmp = bld.vgrf(BRW_REGISTER_TYPE_UD);

               /* Shift each component left to the correct bitfield position. */
               bld.SHL(tmp, offset(src, c), fs_reg(shifts[c] % 32));

               /* Add everything up. */
               if (seen[shifts[c] / 32]) {
                  bld.OR(offset(dst, shifts[c] / 32),
                         offset(dst, shifts[c] / 32), tmp);
               } else {
                  bld.MOV(offset(dst, shifts[c] / 32), tmp);
                  seen[shifts[c] / 32] = true;
               }
            }
         }

         return dst;
      }

      /**
       * Unpack a vector from the bitfield \p src given the per-component bit
       * shifts and widths.
       */
      fs_reg
      emit_unpack(const fs_builder &bld, const fs_reg &src,
                  const color_u &shifts, const color_u &widths)
      {
         const fs_reg dst = bld.vgrf(src.type, 4);

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               /* Shift left to discard the most significant bits. */
               bld.SHL(offset(dst, c),
                       offset(src, shifts[c] / 32),
                       fs_reg(32 - shifts[c] % 32 - widths[c]));

               /* Shift back to the least significant bits using an arithmetic
                * shift to get sign extension on signed types.
                */
               bld.ASR(offset(dst, c), offset(dst, c), fs_reg(32 - widths[c]));
            }
         }

         return dst;
      }

      /**
       * Convert a vector into an integer vector of the specified signedness
       * and bit widths, properly handling overflow.
       */
      fs_reg
      emit_convert_to_integer(const fs_builder &bld, const fs_reg &src,
                              const color_u &widths, bool is_signed)
      {
         const unsigned s = (is_signed ? 1 : 0);
         const fs_reg dst = bld.vgrf(
            is_signed ? BRW_REGISTER_TYPE_D : BRW_REGISTER_TYPE_UD, 4);

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               bld.MOV(offset(dst, c), offset(src, c));

               /* Clamp to the minimum value. */
               if (is_signed)
                  bld.emit_minmax(offset(dst, c), offset(dst, c),
                                  fs_reg(-(int)scale(widths[c] - s) - 1),
                                  BRW_CONDITIONAL_G);

               /* Clamp to the maximum value. */
               bld.emit_minmax(offset(dst, c), offset(dst, c),
                               fs_reg((int)scale(widths[c] - s)),
                               BRW_CONDITIONAL_L);
            }
         }

         return dst;
      }

      /**
       * Convert a normalized fixed-point vector of the specified signedness
       * and bit widths into a floating point vector.
       */
      fs_reg
      emit_convert_from_scaled(const fs_builder &bld, const fs_reg &src,
                               const color_u &widths, bool is_signed)
      {
         const unsigned s = (is_signed ? 1 : 0);
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_F, 4);

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               /* Convert to float. */
               bld.MOV(offset(dst, c), offset(src, c));

               /* Divide by the normalization constants. */
               bld.MUL(offset(dst, c), offset(dst, c),
                       fs_reg(1.0f / scale(widths[c] - s)));

               /* Clamp to the minimum value. */
               if (is_signed)
                  bld.emit_minmax(offset(dst, c), offset(dst, c), fs_reg(-1.0f),
                                  BRW_CONDITIONAL_G);
            }
         }
         return dst;
      }

      /**
       * Convert a floating point vector into a normalized fixed-point vector
       * of the specified signedness and bit widths.
       */
      fs_reg
      emit_convert_to_scaled(const fs_builder &bld, const fs_reg &src,
                             const color_u &widths, bool is_signed)
      {
         const unsigned s = (is_signed ? 1 : 0);
         const fs_reg dst = bld.vgrf(
            is_signed ? BRW_REGISTER_TYPE_D : BRW_REGISTER_TYPE_UD, 4);
         const fs_reg fdst = retype(dst, BRW_REGISTER_TYPE_F);

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               bld.MOV(offset(fdst, c), offset(src, c));

               /* Clamp to the minimum value. */
               if (is_signed)
                  bld.emit_minmax(offset(fdst, c), offset(fdst, c),
                                  fs_reg(-1.0f), BRW_CONDITIONAL_G);

               /* Clamp to the maximum value. */
               bld.emit_minmax(offset(fdst, c), offset(fdst, c),
                               fs_reg(1.0f), BRW_CONDITIONAL_L);

               /* Multiply by the normalization constants. */
               bld.MUL(offset(fdst, c), offset(fdst, c),
                       fs_reg((float)scale(widths[c] - s)));

               /* Convert to integer. */
               bld.RNDE(offset(fdst, c), offset(fdst, c));
               bld.MOV(offset(dst, c), offset(fdst, c));
            }
         }

         return dst;
      }

      /**
       * Convert a floating point vector of the specified bit widths into a
       * 32-bit floating point vector.
       */
      fs_reg
      emit_convert_from_float(const fs_builder &bld, const fs_reg &src,
                              const color_u &widths)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, 4);
         const fs_reg fdst = retype(dst, BRW_REGISTER_TYPE_F);

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               bld.MOV(offset(dst, c), offset(src, c));

               /* Extend 10-bit and 11-bit floating point numbers to 15 bits.
                * This works because they have a 5-bit exponent just like the
                * 16-bit floating point format, and they have no sign bit.
                */
               if (widths[c] < 16)
                  bld.SHL(offset(dst, c),
                          offset(dst, c), fs_reg(15 - widths[c]));

               /* Convert to 32-bit floating point. */
               bld.F16TO32(offset(fdst, c), offset(dst, c));
            }
         }

         return fdst;
      }

      /**
       * Convert a vector into a floating point vector of the specified bit
       * widths.
       */
      fs_reg
      emit_convert_to_float(const fs_builder &bld, const fs_reg &src,
                            const color_u &widths)
      {
         const fs_reg dst = bld.vgrf(BRW_REGISTER_TYPE_UD, 4);
         const fs_reg fdst = retype(dst, BRW_REGISTER_TYPE_F);

         for (unsigned c = 0; c < 4; ++c) {
            if (widths[c]) {
               bld.MOV(offset(fdst, c), offset(src, c));

               /* Clamp to the minimum value. */
               if (widths[c] < 16)
                  bld.emit_minmax(offset(fdst, c), offset(fdst, c),
                                  fs_reg(0.0f), BRW_CONDITIONAL_G);

               /* Convert to 16-bit floating-point. */
               bld.F32TO16(offset(dst, c), offset(fdst, c));

               /* Discard the least significant bits to get floating point
                * numbers of the requested width.  This works because the
                * 10-bit and 11-bit floating point formats have a 5-bit
                * exponent just like the 16-bit format, and they have no sign
                * bit.
                */
               if (widths[c] < 16)
                  bld.SHR(offset(dst, c), offset(dst, c),
                          fs_reg(15 - widths[c]));
            }
         }

         return dst;
      }

      /**
       * Fill missing components of a vector with 0, 0, 0, 1.
       */
      fs_reg
      emit_pad(const fs_builder &bld, const fs_reg &src,
               const color_u &widths)
      {
         const fs_reg dst = bld.vgrf(src.type, 4);
         const unsigned pad[] = { 0, 0, 0, 1 };

         for (unsigned c = 0; c < 4; ++c)
            bld.MOV(offset(dst, c),
                    widths[c] ? offset(src, c) : fs_reg(pad[c]));

         return dst;
      }
   }
}

namespace brw {
   namespace image_access {
      /**
       * Load a vector from a surface of the given format and dimensionality
       * at the given coordinates.
       */
      fs_reg
      emit_image_load(const fs_builder &bld,
                      const fs_reg &image, const fs_reg &addr,
                      mesa_format format, unsigned dims)
      {
         using namespace image_format_info;
         using namespace image_format_conversion;
         using namespace image_validity;
         using namespace image_coordinates;
         using namespace surface_access;
         const brw_device_info *devinfo = bld.devinfo;
         const mesa_format lower_format =
            brw_lower_mesa_image_format(devinfo, format);
         fs_reg tmp;

         if (has_matching_typed_format(devinfo, format)) {
            /* Hopefully we get here most of the time... */
            tmp = emit_typed_read(bld, image, addr, dims,
                                  _mesa_format_num_components(lower_format));
         } else {
            /* Untyped surface reads return 32 bits of the surface per
             * component, without any sort of unpacking or type conversion,
             */
            const unsigned size = _mesa_get_format_bytes(format) / 4;

            /* they don't properly handle out of bounds access, so we have to
             * check manually if the coordinates are valid and predicate the
             * surface read on the result,
             */
            const brw_predicate pred =
               emit_bounds_check(bld, image, addr, dims);

            /* and they don't know about surface coordinates, we need to
             * convert them to a raw memory offset.
             */
            const fs_reg laddr = emit_address_calculation(bld, image, addr, dims);

            tmp = emit_untyped_read(bld, image, laddr, 1, size, pred);

            /* An out of bounds surface access should give zero as result. */
            for (unsigned c = 0; c < 4; ++c)
               set_predicate(pred, bld.SEL(offset(tmp, c),
                                           offset(tmp, c), fs_reg(0)));
         }

         /* Set the register type to D instead of UD if the data type is
          * represented as a signed integer in memory so that sign extension
          * is handled correctly by unpack.
          */
         if (needs_sign_extension(format))
            tmp = retype(tmp, BRW_REGISTER_TYPE_D);

         if (!has_supported_bit_layout(devinfo, format)) {
            /* Unpack individual vector components from the bitfield if the
             * hardware is unable to do it for us.
             */
            if (has_split_bit_layout(devinfo, format))
               tmp = emit_pack(bld, tmp, get_bit_shifts(lower_format),
                               get_bit_widths(lower_format));
            else
               tmp = emit_unpack(bld, tmp, get_bit_shifts(format),
                                 get_bit_widths(format));

         } else if ((needs_sign_extension(format) &&
                     !is_conversion_trivial(devinfo, format)) ||
                    has_undefined_high_bits(devinfo, format)) {
            /* Perform a trivial unpack even though the bit layout matches in
             * order to get the most significant bits of each component
             * initialized properly.
             */
            tmp = emit_unpack(bld, tmp, color_u(0, 32, 64, 96),
                              get_bit_widths(format));
         }

         if (!_mesa_is_format_integer(format)) {
            if (is_conversion_trivial(devinfo, format)) {
               /* Just need to cast the vector to the target type. */
               tmp = retype(tmp, BRW_REGISTER_TYPE_F);
            } else {
               /* Do the right sort of type conversion to float. */
               if (_mesa_get_format_datatype(format) == GL_FLOAT)
                  tmp = emit_convert_from_float(
                     bld, tmp, get_bit_widths(format));
               else
                  tmp = emit_convert_from_scaled(
                     bld, tmp, get_bit_widths(format),
                     _mesa_is_format_signed(format));
            }
         }

         /* Initialize missing components of the result. */
         return emit_pad(bld, tmp, get_bit_widths(format));
      }

      /**
       * Store a vector in a surface of the given format and dimensionality at
       * the given coordinates.
       */
      void
      emit_image_store(const fs_builder &bld, const fs_reg &image,
                       const fs_reg &addr, const fs_reg &src,
                       mesa_format format, unsigned dims)
      {
         using namespace image_format_info;
         using namespace image_format_conversion;
         using namespace image_validity;
         using namespace image_coordinates;
         using namespace surface_access;
         const brw_device_info *devinfo = bld.devinfo;

         if (format == MESA_FORMAT_NONE) {
            /* We don't know what the format is, but that's fine because it
             * implies write-only access, and typed surface writes are always
             * able to take care of type conversion and packing for us.
             */
            emit_typed_write(bld, image, addr, src, dims, 4);

         } else {
            const mesa_format lower_format =
               brw_lower_mesa_image_format(devinfo, format);
            fs_reg tmp = src;

            if (!is_conversion_trivial(devinfo, format)) {
               /* Do the right sort of type conversion. */
               if (_mesa_get_format_datatype(format) == GL_FLOAT)
                  tmp = emit_convert_to_float(bld, tmp, get_bit_widths(format));

               else if (_mesa_is_format_integer(format))
                  tmp = emit_convert_to_integer(bld, tmp, get_bit_widths(format),
                                                _mesa_is_format_signed(format));

               else
                  tmp = emit_convert_to_scaled(bld, tmp, get_bit_widths(format),
                                               _mesa_is_format_signed(format));
            }

            /* We're down to bit manipulation at this point. */
            tmp = retype(tmp, BRW_REGISTER_TYPE_UD);

            if (!has_supported_bit_layout(devinfo, format)) {
               /* Pack the vector components into a bitfield if the hardware
                * is unable to do it for us.
                */
               if (has_split_bit_layout(devinfo, format))
                  tmp = emit_unpack(bld, tmp, get_bit_shifts(lower_format),
                                    get_bit_widths(lower_format));

               else
                  tmp = emit_pack(bld, tmp, get_bit_shifts(format),
                                  get_bit_widths(format));
            }

            if (has_matching_typed_format(devinfo, format)) {
               /* Hopefully we get here most of the time... */
               emit_typed_write(bld, image, addr, tmp, dims,
                                _mesa_format_num_components(lower_format));

            } else {
               /* Untyped surface writes store 32 bits of the surface per
                * component, without any sort of packing or type conversion,
                */
               const unsigned size = _mesa_get_format_bytes(format) / 4;

               /* they don't properly handle out of bounds access, so we have
                * to check manually if the coordinates are valid and predicate
                * the surface write on the result,
                */
               const brw_predicate pred =
                  emit_bounds_check(bld, image, addr, dims);

               /* and, phew, they don't know about surface coordinates, we
                * need to convert them to a raw memory offset.
                */
               const fs_reg laddr = emit_address_calculation(bld, image, addr, dims);

               emit_untyped_write(bld, image, laddr, tmp, 1, size, pred);
            }
         }
      }

      /**
       * Perform an atomic read-modify-write operation in a surface of the
       * given dimensionality at the given coordinates.  Main building block
       * of the imageAtomic GLSL built-ins.
       */
      fs_reg
      emit_image_atomic(const fs_builder &bld,
                        const fs_reg &image, const fs_reg &addr,
                        const fs_reg &src0, const fs_reg &src1,
                        unsigned dims, unsigned rsize, unsigned op)
      {
         using namespace image_validity;
         using namespace image_coordinates;
         using namespace surface_access;
         /* Avoid performing an atomic operation on an unbound surface. */
         const brw_predicate pred = emit_surface_check(bld, image);

         /* Thankfully we can do without untyped atomics here. */
         const fs_reg tmp = emit_typed_atomic(bld, image, addr, src0, src1,
                                              dims, rsize, op, pred);

         /* An unbound surface access should give zero as result. */
         if (rsize)
            set_predicate(pred, bld.SEL(tmp, tmp, fs_reg(0)));

         return tmp;
      }
   }
}
