#include "dxil_buffer.h"
#include <assert.h>

void
dxil_buffer_init(struct dxil_buffer *b, unsigned abbrev_width)
{
   blob_init(&b->blob);
   b->buf = 0;
   b->buf_bits = 0;

   b->abbrev_width = abbrev_width;
}

static bool
flush_dword(struct dxil_buffer *b)
{
   assert(b->buf_bits >= 32 && b->buf_bits < 64);

   uint32_t lower_bits = b->buf & UINT32_MAX;
   if (!blob_write_bytes(&b->blob, &lower_bits, sizeof(lower_bits)))
      return false;

   b->buf >>= 32;
   b->buf_bits -= 32;

   return true;
}

bool
dxil_buffer_emit_bits(struct dxil_buffer *b, uint32_t data, unsigned width)
{
   assert(b->buf_bits < 32);
   assert(width > 0 && width <= 32);
   assert((data & ~((UINT64_C(1) << width) - 1)) == 0);

   b->buf |= ((uint64_t)data) << b->buf_bits;
   b->buf_bits += width;

   if (b->buf_bits >= 32)
      return flush_dword(b);

   return true;
}

bool
dxil_buffer_emit_vbr_bits(struct dxil_buffer *b, uint64_t data,
                          unsigned width)
{
   assert(width > 1 && width <= 32);

   uint32_t tag = UINT32_C(1) << (width - 1);
   uint32_t max = tag - 1;
   while (data > max) {
      uint32_t value = (data & max) | tag;
      data >>= width - 1;

      if (!dxil_buffer_emit_bits(b, value, width))
         return false;
   }

   return dxil_buffer_emit_bits(b, data, width);
}

bool
dxil_buffer_align(struct dxil_buffer *b)
{
   assert(b->buf_bits < 32);

   if (b->buf_bits) {
      b->buf_bits = 32;
      return flush_dword(b);
   }

   return true;
}
