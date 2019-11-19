/*
 * Copyright 2019 Collabora Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "dxil_container.h"
#include "dxil_module.h"

#include "util/u_debug.h"

#include <assert.h>

#define DXIL_FOURCC(ch0, ch1, ch2, ch3) ( \
  (uint32_t)(ch0)        | (uint32_t)(ch1) << 8 | \
  (uint32_t)(ch2) << 16  | (uint32_t)(ch3) << 24)

const uint32_t DXIL_DXBC = DXIL_FOURCC('D', 'X', 'B', 'C');

enum dxil_part_fourcc {
   DXIL_RDEF = DXIL_FOURCC('R', 'D', 'E', 'F'),
   DXIL_ISG1 = DXIL_FOURCC('I', 'S', 'G', '1'),
   DXIL_OSG1 = DXIL_FOURCC('O', 'S', 'G', '1'),
   DXIL_PSG1 = DXIL_FOURCC('P', 'S', 'G', '1'),
   DXIL_STAT = DXIL_FOURCC('S', 'T', 'A', 'T'),
   DXIL_ILDB = DXIL_FOURCC('I', 'L', 'D', 'B'),
   DXIL_ILDN = DXIL_FOURCC('I', 'L', 'D', 'N'),
   DXIL_SFI0 = DXIL_FOURCC('S', 'F', 'I', '0'),
   DXIL_PRIV = DXIL_FOURCC('P', 'R', 'I', 'V'),
   DXIL_RTS0 = DXIL_FOURCC('R', 'T', 'S', '0'),
   DXIL_DXIL = DXIL_FOURCC('D', 'X', 'I', 'L'),
   DXIL_PSV0 = DXIL_FOURCC('P', 'S', 'V', '0'),
   DXIL_RDAT = DXIL_FOURCC('R', 'D', 'A', 'T'),
   DXIL_HASH = DXIL_FOURCC('H', 'A', 'S', 'H'),
};

void
dxil_container_init(struct dxil_container *c)
{
   blob_init(&c->parts);
   c->num_parts = 0;
}

static bool
add_part_header(struct dxil_container *c,
                enum dxil_part_fourcc fourcc,
                uint32_t part_size)
{
   assert(c->parts.size < UINT_MAX);
   unsigned offset = (unsigned)c->parts.size;
   if (!blob_write_bytes(&c->parts, &fourcc, sizeof(fourcc)) ||
       !blob_write_bytes(&c->parts, &part_size, sizeof(part_size)))
      return false;

   assert(c->num_parts < DXIL_MAX_PARTS);
   c->part_offsets[c->num_parts++] = offset;
   return true;
}

static bool
add_part(struct dxil_container *c,
         enum dxil_part_fourcc fourcc,
         const void *part_data, uint32_t part_size)
{
   return add_part_header(c, fourcc, part_size) &&
          blob_write_bytes(&c->parts, part_data, part_size);
}

bool
dxil_container_add_features(struct dxil_container *c,
                            const struct dxil_features *features)
{
   union {
      struct dxil_features flags;
      uint64_t bits;
   } u = { .flags = *features };
   return add_part(c, DXIL_SFI0, &u.bits, sizeof(u.bits));
}

bool
dxil_container_add_input_signature(struct dxil_container *c)
{
   uint32_t data[2] = { 0, sizeof(uint32_t) * 2 };
   return add_part(c, DXIL_ISG1, data, sizeof(data));
}

bool
dxil_container_add_output_signature(struct dxil_container *c)
{
   uint32_t data[2] = { 0, sizeof(uint32_t) * 2 };
   return add_part(c, DXIL_OSG1, data, sizeof(data));
}

bool
dxil_container_add_state_validation(struct dxil_container *c,
                                    const struct dxil_resource *resources,
                                    size_t num_resources)
{
   struct {
      struct {
         union {
            uint32_t data[4]; /* actual size */
         };
         uint32_t expected_wave_lane_range[2];
      } psv0;
      struct {
         uint8_t shader_stage;
         uint8_t uses_view_id;
         union {
           uint16_t data; /* actual size */
         };
         uint8_t sig_input_elements;
         uint8_t sig_output_elements;
         uint8_t sig_patch_const_or_prim_elements;
         uint8_t sig_input_vectors;
         uint8_t sig_output_vectors[4];
      } psv1;
   } psv1_data = {
      { /* psv0 */
         { 0, 0, 0, 0 },
         { 0, UINT_MAX },
      }, { /* psv1 */
         DXIL_COMPUTE_SHADER, 0, 0, 0, 0, 0, 0,
         { 0, 0, 0, 0 }
      }
   };

   uint32_t psv1_size = sizeof(psv1_data);
   uint32_t resource_count = num_resources;
   size_t size = sizeof(psv1_size) + sizeof(psv1_data) +
                 sizeof(resource_count);
   uint32_t bind_info_size = sizeof(uint32_t) * 4;
   uint8_t string_table[4] = { 0, 0, 0, 0 };
   uint32_t string_table_size = ARRAY_SIZE(string_table);
   uint32_t semantic_index_table_size = 0;
   if (num_resources > 0)
      size += sizeof(uint32_t) + bind_info_size * num_resources;
   size += sizeof(uint32_t) * 2 + string_table_size + semantic_index_table_size;

   if (!add_part_header(c, DXIL_PSV0, size) ||
       !blob_write_bytes(&c->parts, &psv1_size, sizeof(psv1_size)) ||
       !blob_write_bytes(&c->parts, &psv1_data, sizeof(psv1_data)) ||
       !blob_write_bytes(&c->parts, &resource_count, sizeof(resource_count)))
      return false;

   if (num_resources > 0) {
      if (!blob_write_bytes(&c->parts, &bind_info_size, sizeof(bind_info_size)) ||
          !blob_write_bytes(&c->parts, resources, bind_info_size * num_resources))
         return false;
   }

   return blob_write_bytes(&c->parts, &string_table_size, sizeof(string_table_size)) &&
          blob_write_bytes(&c->parts, string_table, sizeof(string_table)) &&
          blob_write_bytes(&c->parts, &semantic_index_table_size, sizeof(semantic_index_table_size));
}

bool
dxil_container_add_module(struct dxil_container *c,
                          const struct dxil_module *m)
{
   assert(m->buf_bits == 0); // make sure the module is fully flushed
   uint32_t version = (m->shader_kind << 16) |
                      (m->major_version << 4) |
                      m->minor_version;
   uint32_t size = 6 * sizeof(uint32_t) + m->module.size;
   assert(size % sizeof(uint32_t) == 0);
   uint32_t uint32_size = size / sizeof(uint32_t);
   uint32_t magic = 0x4C495844;
   uint32_t dxil_version = 1 << 8; // I have no idea...
   uint32_t bitcode_offset = 16;
   uint32_t bitcode_size = m->module.size;

   return add_part_header(c, DXIL_DXIL, size) &&
          blob_write_bytes(&c->parts, &version, sizeof(version)) &&
          blob_write_bytes(&c->parts, &uint32_size, sizeof(uint32_size)) &&
          blob_write_bytes(&c->parts, &magic, sizeof(magic)) &&
          blob_write_bytes(&c->parts, &dxil_version, sizeof(dxil_version)) &&
          blob_write_bytes(&c->parts, &bitcode_offset, sizeof(bitcode_offset)) &&
          blob_write_bytes(&c->parts, &bitcode_size, sizeof(bitcode_size)) &&
          blob_write_bytes(&c->parts, m->module.data, m->module.size);
}

bool
dxil_container_write(struct dxil_container *c, struct blob *blob)
{
   assert(blob->size == 0);
   if (!blob_write_bytes(blob, &DXIL_DXBC, sizeof(DXIL_DXBC)))
      return false;

   const uint8_t unsigned_digest[16] = { 0 }; // null-digest means unsigned
   if (!blob_write_bytes(blob, unsigned_digest, sizeof(unsigned_digest)))
      return false;

   uint16_t major_version = 1;
   uint16_t minor_version = 0;
   if (!blob_write_bytes(blob, &major_version, sizeof(major_version)) ||
       !blob_write_bytes(blob, &minor_version, sizeof(minor_version)))
      return false;

   size_t header_size = 32 + 4 * c->num_parts;
   size_t size = header_size + c->parts.size;
   assert(size <= UINT32_MAX);
   uint32_t container_size = (uint32_t)size;
   if (!blob_write_bytes(blob, &container_size, sizeof(container_size)))
      return false;

   uint32_t part_offsets[DXIL_MAX_PARTS];
   for (int i = 0; i < c->num_parts; ++i) {
      size_t offset = header_size + c->part_offsets[i];
      assert(offset <= UINT32_MAX);
      part_offsets[i] = (uint32_t)offset;
   }

   if (!blob_write_bytes(blob, &c->num_parts, sizeof(c->num_parts)) ||
       !blob_write_bytes(blob, part_offsets, sizeof(uint32_t) * c->num_parts) ||
       !blob_write_bytes(blob, c->parts.data, c->parts.size))
      return false;

   return true;
}
