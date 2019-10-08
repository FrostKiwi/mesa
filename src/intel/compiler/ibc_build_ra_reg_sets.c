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

#include "util/blob.h"

#include <stdio.h>
#include <zlib.h>
#include <assert.h>

#define CHUNK 16384

int main(int arc, char **argv)
{
   FILE *fp = fopen(argv[1], "w");
   if (fp == NULL)
      abort();

   unsigned simd_width = atoi(argv[2]);
   assert(simd_width == 8 || simd_width == 16 || simd_width == 32);
   unsigned char out[CHUNK];

   z_stream stream;
   stream.zalloc = Z_NULL;
   stream.zfree = Z_NULL;
   stream.opaque = Z_NULL;

   int ret = deflateInit(&stream, 9);  // XXX: pick a sensible level
   if (ret != Z_OK) {
      return ret;
   }

   void *mem_ctx = ralloc_context(NULL);
   ibc_ra_reg_set set;
   ibc_ra_reg_set_init(&set, simd_width, mem_ctx);

   struct blob blob;
   blob_init(&blob);

   ra_set_serialize(set.regs, &blob);

   assert(blob.size % sizeof(uint32_t) == 0);

   stream.avail_in = blob.size;
   stream.next_in = blob.data;

   unsigned have;
   do {
      stream.avail_out = CHUNK;
      stream.next_out = out;
      ret = deflate(&stream, stream.avail_in < CHUNK ? Z_FINISH : Z_NO_FLUSH);
      assert(ret != Z_STREAM_ERROR);
      have = CHUNK - stream.avail_out;
      if (fwrite(out, 1, have, fp) != have) {
         (void)deflateEnd(&stream);
         return Z_ERRNO;
      }
   } while (ret != Z_STREAM_END);

   (void)deflateEnd(&stream);
   blob_finish(&blob);
   ralloc_free(mem_ctx);
   fclose(fp);

   return Z_OK;
}
