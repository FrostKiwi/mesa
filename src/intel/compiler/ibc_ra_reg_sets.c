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

#include "ibc_ra_reg_sets.h"

#define MAX_NUM_CLASSES 37

static void
ibc_ra_reg_set_init_base(ibc_ra_reg_set *set, unsigned simd_width,
                         unsigned *class_count, ibc_ra_reg_class **classes,
                         void *mem_ctx)
{
   memset(set, 0, sizeof(*set));

   set->simd_width = simd_width;

   *class_count = 0;

   /* Start with scalars.  This way 1B regs and HW regs line up. */
   for (unsigned i = 0; i < ARRAY_SIZE(set->phys_small); i++) {
      set->phys_small[i].reg_align = (1 << i);
      set->phys_small[i].reg_size = (1 << i);
      set->phys_small[i].num_regs = TOTAL_GRF_BYTES >> i;
      classes[(*class_count)++] = &set->phys_small[i];
   }

   for (unsigned i = 0; i < ARRAY_SIZE(set->phys_large); i++) {
      set->phys_large[i].reg_align = 32;
      set->phys_large[i].reg_size = (i + 1) * 32;
      set->phys_large[i].num_regs = (TOTAL_GRF_BYTES / 32) - i;
      classes[(*class_count)++] = &set->phys_large[i];
   }

   for (unsigned stride = 1; stride <= 8; stride *= 2) {
      if (stride * simd_width > 64)
         continue;

      for (unsigned num_comps = 1; num_comps <= 4; num_comps++) {
         ibc_ra_reg_class *class =
            ibc_ra_reg_set_get_strided_class_mut(set, stride, num_comps,
                                                 simd_width);
         class->reg_align = stride * simd_width;
         class->reg_size = stride * simd_width * num_comps;
         class->num_regs =
            (TOTAL_GRF_BYTES / (stride * simd_width)) - (num_comps - 1);
         classes[(*class_count)++] = class;
      }
   }
   assert(*class_count <= MAX_NUM_CLASSES);

   set->num_ra_regs = 0;
   for (unsigned i = 0; i < *class_count; i++) {
      classes[i]->nr = i;
      classes[i]->reg_start = set->num_ra_regs;
      set->num_ra_regs += classes[i]->num_regs;
   }

   set->ra_reg_to_grf = ralloc_array(mem_ctx, uint16_t, set->num_ra_regs);

   int reg = 0;
   for (unsigned i = 0; i < *class_count; i++) {
      assert(classes[i]->reg_size < TOTAL_GRF_BYTES);
      for (unsigned grf = 0; grf <= TOTAL_GRF_BYTES - classes[i]->reg_size;
           grf += classes[i]->grf_stride)
         set->ra_reg_to_grf[reg++] = grf;

      assert(reg == classes[i]->reg_start + classes[i]->num_regs);
   }
   assert(reg == set->num_ra_regs);
}

static inline void
ibc_ra_reg_set_build_regs(ibc_ra_reg_set *set, unsigned simd_width,
                          unsigned class_count,
                          ibc_ra_reg_class **classes,
                          void *mem_ctx)
{
   set->regs = ra_alloc_reg_set(mem_ctx, set->num_ra_regs, false);
   ra_set_allocate_round_robin(set->regs);

   for (unsigned i = 0; i < class_count; i++) {
      UNUSED unsigned nr = ra_alloc_reg_class(set->regs);
      assert(nr == i);
      for (unsigned j = 0; j < classes[i]->num_regs; j++) {
         ra_class_add_reg(set->regs, classes[i]->nr,
                          classes[i]->reg_start + j);
      }
   }

   /* From register_allocate.c:
    *
    * q(B,C) (indexed by C, B is this register class) in Runeson/Nyström
    * paper.  This is "how many registers of B could the worst choice
    * register from C conflict with".
    *
    * If we just let the register allocation algorithm compute these values,
    * it's extremely expensive.  However, since all of our registers are laid
    * out fairly explicitly, we can very easily compute them ourselves.
    */
   unsigned **q_values = ralloc_array(mem_ctx, unsigned *, class_count);
   for (unsigned c = 0; c < class_count; c++)
      q_values[c] = ralloc_array(q_values, unsigned, class_count);

   for (unsigned b = 0; b < class_count; b++) {
      for (unsigned c = 0; c < class_count; c++) {
         assert(util_is_power_of_two_nonzero(classes[b]->reg_align));
         assert(util_is_power_of_two_nonzero(classes[c]->reg_align));

         /* Save these off for later */
         const unsigned b_start = classes[b]->reg_start;
         const unsigned c_start = classes[c]->reg_start;

         if (classes[c]->reg_align == classes[c]->reg_size &&
             classes[b]->reg_align == classes[b]->reg_size &&
             classes[b]->reg_size >= classes[c]->reg_size) {
            /* If B's and C's registers are both aligned to a register and C's
             * fit inside B's size and alignment then one register in C always
             * maps to one register in B.
             */
            q_values[b][c] = 1;

            const unsigned c_per_b = classes[b]->reg_size /
                                     classes[c]->reg_size;
            assert(classes[c]->num_regs == classes[b]->num_regs * c_per_b);
            for (unsigned i = 0; i < classes[c]->num_regs; i++) {
               unsigned c_reg = c_start + i;
               unsigned b_reg = b_start + i / c_per_b;
               assert(b_reg < b_start + classes[b]->num_regs);
               ra_add_reg_conflict(set->regs, c_reg, b_reg);
            }
         } else if (classes[b]->reg_align == classes[b]->reg_size &&
                    classes[c]->reg_align >= classes[b]->reg_align &&
                    classes[c]->reg_size >= classes[b]->reg_size) {
            /* IF B's registers are aligned to a register and fit inside C's
             * alignment then one register in C always maps to an integer
             * number of registers in B and there's no weird overlap.
             */
            assert(classes[c]->reg_size % classes[b]->reg_size == 0);
            const unsigned b_per_c = classes[c]->reg_size /
                                     classes[b]->reg_size;
            q_values[b][c] = b_per_c;

            assert(classes[c]->grf_stride % classes[b]->reg_size == 0);
            const unsigned c_stride_b = classes[c]->grf_stride /
                                        classes[b]->reg_size;
            for (unsigned i = 0; i < classes[c]->num_regs; i++) {
               unsigned c_reg = c_start + i;
               for (unsigned j = 0; j < b_per_c; j++) {
                  unsigned b_reg = b_start + i * c_stride_b + j;
                  assert(b_reg < b_start + classes[b]->num_regs);
                  ra_add_reg_conflict(set->regs, c_reg, b_reg);
               }
            }
         } else {
            /*
             * View the register from C as fixed starting at GRF n somwhere in
             * the middle, and the register from B as sliding back and forth.
             * Then the first register to conflict from B is the one starting
             * at n - classes[b].reg_size + 1 and the last register to
             * conflict will start at n + class[b].reg_size - 1.  Therefore,
             * the number of conflicts from B is
             *
             *    classes[b].reg_size + classes[c].reg_size - 1.
             *
             *   +-+-+-+-+-+-+     +-+-+-+-+-+-+
             * B | | | | | |n| --> | | | | | | |
             *   +-+-+-+-+-+-+     +-+-+-+-+-+-+
             *             +-+-+-+-+-+
             * C           |n| | | | |
             *             +-+-+-+-+-+
             */
            q_values[b][c] = classes[b]->reg_size + classes[c]->reg_size - 1;

            for (unsigned i = 0; i < classes[c]->num_regs; i++) {
               unsigned c_reg = c_start + i;
               unsigned c_grf = set->ra_reg_to_grf[c_reg];
               unsigned c_grf_end = c_grf + classes[c]->reg_size;
               for (unsigned j = 0; j < classes[b]->num_regs; j++) {
                  unsigned b_reg = b_start + j;
                  unsigned b_grf = set->ra_reg_to_grf[b_reg];
                  unsigned b_grf_end = b_grf + classes[b]->reg_size;
                  if (!(b_grf_end <= c_grf || c_grf_end <= b_grf))
                     ra_add_reg_conflict(set->regs, c_reg, b_reg);
               }
            }
         }
      }
   }

   ra_set_finalize(set->regs, q_values);

   ralloc_free(q_values);
}

#ifdef IBC_HAVE_PREBUILT_REG_SETS
#include "ibc_generated_ra_reg_sets.h"
#include "util/blob.h"

static void
ibc_ra_regsets_inflate_prebuild_regs(unsigned simd_width, struct blob *blob)
{
   const char * data_start;
   size_t data_size;

   switch (simd_width) {
   case 8:
      data_start = ra_reg_sets8;
      data_size  = sizeof(ra_reg_sets8);
      break;
   case 16:
      data_start = ra_reg_sets16;
      data_size  = sizeof(ra_reg_sets16);
      break;
   case 32:
      data_start = ra_reg_sets32;
      data_size  = sizeof(ra_reg_sets32);
      break;
   default:
      unreachable("unknown simd size");
   }

#if defined(HAVE_ZSTD_BUILD) && defined(HAVE_ZSTD)
   #include <zstd.h>
   # if !defined(ZSTD_FRAMEHEADERSIZE_MAX)
   /* They tell you to use this, but then hide it behind the static linking only API... */
   #  define ZSTD_FRAMEHEADERSIZE_MAX 18
   # endif
   unsigned long long decompressed_size = ZSTD_getFrameContentSize(data_start, ZSTD_FRAMEHEADERSIZE_MAX);


   if (decompressed_size == ZSTD_CONTENTSIZE_UNKNOWN || decompressed_size == ZSTD_CONTENTSIZE_ERROR) {
      fprintf(stderr, "Could not get size of compressed zstd ra sets\n");
      abort();
   }

   void * dst = malloc(decompressed_size + 1);

   size_t out = ZSTD_decompress(dst, decompressed_size, data_start, data_size);
   if (ZSTD_isError(out)) {
      fprintf(stderr, "Error decompressing the ra sets: %s\n", ZSTD_getErrorName(out));
      free(dst);
      abort();
   }
   if (!blob_write_bytes(blob, dst, out)) {
      fprintf(stderr, "error writing data to the blob\n");
      free(dst);
      abort();
   }
   free(dst);
#else
   #include <zlib.h>
   #define CHUNK 16384

   int ret;
   unsigned have;
   z_stream stream;
   unsigned char out[CHUNK];

   stream.zalloc = Z_NULL;
   stream.zfree = Z_NULL;
   stream.opaque = Z_NULL;
   stream.avail_in = data_size;
   stream.next_in = data_start;
   ret = inflateInit(&stream);
   if (ret != Z_OK) {
      abort();
   }

   do {
      stream.avail_out = CHUNK;
      stream.next_out = out;

      ret = inflate(&stream, stream.avail_in < CHUNK ? Z_FINISH : Z_NO_FLUSH);
      assert(ret != Z_STREAM_ERROR);
      switch (ret) {
      case Z_NEED_DICT:
         ret = Z_DATA_ERROR;
         /* fallthrough */
      case Z_DATA_ERROR:
      case Z_MEM_ERROR:
         (void)inflateEnd(&stream);
         abort();
      }

      have = CHUNK - stream.avail_out;
      if (!blob_write_bytes(blob, out, have)) {
         abort();
      }
   } while (stream.avail_in != 0 && stream.avail_out == 0);

   (void)inflateEnd(&stream);
#endif
}

static void
ibc_ra_reg_set_load_prebuilt_regs(ibc_ra_reg_set *set, unsigned simd_width,
                                  void *mem_ctx)
{
   struct blob blob;
   blob_init(&blob);
   ibc_ra_regsets_inflate_prebuild_regs(simd_width, &blob);

   struct blob_reader reader;
   blob_reader_init(&reader, blob.data, blob.size);

   set->regs = ra_set_deserialize(mem_ctx, &reader);

   blob_finish(&blob);
}
#endif

void
ibc_ra_reg_set_init(ibc_ra_reg_set *set, unsigned simd_width, void *mem_ctx)
{
   unsigned class_count = 0;
   ibc_ra_reg_class *classes[MAX_NUM_CLASSES];
   ibc_ra_reg_set_init_base(set, simd_width, &class_count, classes, mem_ctx);

#ifdef IBC_HAVE_PREBUILT_REG_SETS
   ibc_ra_reg_set_load_prebuilt_regs(set, simd_width, mem_ctx);
#else
   ibc_ra_reg_set_build_regs(set, simd_width, class_count, classes, mem_ctx);
#endif
}
