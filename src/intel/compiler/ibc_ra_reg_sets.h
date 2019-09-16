/*
 * Copyright © 2019 Intel Corporation
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

#ifndef IBC_REG_SETS_H
#define IBC_REG_SETS_H

#include "ibc.h"
#include "util/register_allocate.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_HW_GRF_SIZE (16 * 32)
#define TOTAL_GRF_BYTES 4096

typedef struct ibc_ra_reg_class {
   uint8_t nr;
   union {
      /* Assume that every class strides by its alignment */
      uint8_t reg_align;
      uint8_t grf_stride;
   };
   uint16_t reg_size;
   int reg_start;
   int num_regs;
} ibc_ra_reg_class;

typedef struct ibc_ra_reg_set {
   struct ra_regs *regs;

   unsigned simd_width;

   /** Classes for sub-32-byte physical registers */
   ibc_ra_reg_class phys_small[5];

   /** Classes for 32-byte and larger physical registers */
   ibc_ra_reg_class phys_large[MAX_HW_GRF_SIZE / 32];

   /** Classes for strided registers
    *
    * Strided registers are handled as a bit of a special case.  We only
    * bother for logical registers whose SIMD width is equal to the dispatch
    * width of the shader and only when the whole register SIMD width fits
    * inside of two registers.  We do this for up to a vec4 size register.
    */
   ibc_ra_reg_class strided[4][4];

   unsigned num_ra_regs;
   uint16_t *ra_reg_to_grf;
} ibc_ra_reg_set;

static inline const ibc_ra_reg_class *
ibc_ra_reg_set_get_physical_class(const ibc_ra_reg_set *set, uint16_t size)
{
   if (size < 32) {
      /* This is a scalar */
      int idx = ffs(size) - 1;
      assert(idx >= 0 && idx < ARRAY_SIZE(set->phys_small));
      return &set->phys_small[idx];
   } else {
      assert(size < MAX_HW_GRF_SIZE);
      return &set->phys_large[(size / 32) - 1];
   }
}

static inline ibc_ra_reg_class *
ibc_ra_reg_set_get_strided_class_mut(ibc_ra_reg_set *set,
                                     unsigned stride, unsigned num_comps,
                                     UNUSED unsigned simd_width)
{
   assert(simd_width == set->simd_width);
   assert(stride * simd_width <= 64);

   int stride_idx = ffs(stride) - 1;
   assert(stride_idx >= 0 && stride_idx < ARRAY_SIZE(set->strided));
   assert(num_comps > 0 && num_comps <= ARRAY_SIZE(set->strided[0]));
   return &set->strided[stride_idx][num_comps - 1];
}

static inline const ibc_ra_reg_class *
ibc_ra_reg_set_get_strided_class(const ibc_ra_reg_set *set,
                                 unsigned stride, unsigned num_comps,
                                 unsigned simd_width)
{
   return ibc_ra_reg_set_get_strided_class_mut((ibc_ra_reg_set *)set,
                                               stride, num_comps, simd_width);
}

static inline unsigned
ibc_ra_reg_class_grf_to_reg(const ibc_ra_reg_class *c, unsigned grf_byte)
{
   assert(grf_byte % c->grf_stride == 0);
   return c->reg_start + (grf_byte / c->grf_stride);
}

static inline unsigned
ibc_ra_reg_set_grf_to_reg(const ibc_ra_reg_set *set,
                          uint16_t grf_byte, uint16_t size)
{
   const ibc_ra_reg_class *c = ibc_ra_reg_set_get_physical_class(set, size);
   return ibc_ra_reg_class_grf_to_reg(c, grf_byte);
}

void ibc_ra_reg_set_init(ibc_ra_reg_set *set, unsigned simd_width,
                         void *mem_ctx);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_REG_SETS_H */
