#include "dxil_module.h"
#include <assert.h>
#include <stdio.h>

static void
init()
{
   struct dxil_module mod;
   dxil_module_init(&mod);
   assert(!mod.buf);
   assert(!mod.buf_bits);
}

static void
assert_module_data(const struct dxil_module *m, const uint8_t *data,
                   size_t len)
{
   if (m->module.size != len) {
      fprintf(stderr, "module-size mismatch, expected %zd, got %zd",
                      len, m->module.size);
      abort();
   }

   for (size_t i = 0; i < len; ++i) {
      if (m->module.data[i] != data[i]) {
         fprintf(stderr, "module-data mismatch at index %zd, "
                         "expected 0x%02x, got 0x%02x", i,
                         data[i], m->module.data[i]);
         abort();
      }
   }
}

#define ASSERT_MODULE_DATA(m, data) \
   assert_module_data(m, data, sizeof(data))

static void
align()
{
   struct dxil_module mod;
   dxil_module_init(&mod);
   assert_module_data(&mod, NULL, 0);

   dxil_module_init(&mod);
   dxil_module_emit_bits(&mod, 0xbeef, 16);
   dxil_module_align(&mod);
   assert(!mod.buf);
   assert(!mod.buf_bits);
   uint8_t expected0[] = { 0xef, 0xbe, 0x00, 0x00 };
   ASSERT_MODULE_DATA(&mod, expected0);
   dxil_module_align(&mod);
   ASSERT_MODULE_DATA(&mod, expected0);
}

static void
emit_bits()
{
   struct dxil_module mod;
   dxil_module_init(&mod);
   dxil_module_emit_bits(&mod, 0xbeef, 16);
   dxil_module_align(&mod);
   assert(!mod.buf);
   assert(!mod.buf_bits);
   uint8_t expected0[] = { 0xef, 0xbe, 0x00, 0x00 };
   ASSERT_MODULE_DATA(&mod, expected0);

   dxil_module_init(&mod);
   dxil_module_emit_bits(&mod, 0xdead, 16);
   dxil_module_emit_bits(&mod, 0xbeef, 16);
   assert(!mod.buf);
   assert(!mod.buf_bits);
   uint8_t expected1[] = { 0xad, 0xde, 0xef, 0xbe };
   ASSERT_MODULE_DATA(&mod, expected1);

   dxil_module_init(&mod);
   dxil_module_emit_bits(&mod, 0x1111111, 28);
   dxil_module_emit_bits(&mod, 0x22222222, 32);
   dxil_module_align(&mod);
   uint8_t expected2[] = { 0x11, 0x11, 0x11, 0x21, 0x22, 0x22, 0x22, 0x02 };
   ASSERT_MODULE_DATA(&mod, expected2);
}

static void
emit_vbr_bits()
{
   struct dxil_module mod;
   dxil_module_init(&mod);
   dxil_module_emit_vbr_bits(&mod, 0x1a, 8);
   dxil_module_emit_vbr_bits(&mod, 0x1a, 6);
   dxil_module_emit_vbr_bits(&mod, 0x00, 2);
   dxil_module_emit_vbr_bits(&mod, 0x0a, 4);
   dxil_module_emit_vbr_bits(&mod, 0x04, 2);
   dxil_module_emit_vbr_bits(&mod, 0x00, 2);
   uint8_t expected[] = { 0x1a, 0x1a, 0x1a, 0x1a };
   ASSERT_MODULE_DATA(&mod, expected);
}

int
main()
{
   init();
   align();
   emit_bits();
   emit_vbr_bits();
   return 0;
}
