/*
 * Copyright (c) 2020 Collabora Ltd.
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

#ifndef DXIL_SIGNATURE_H
#define DXIL_SIGNATURE_H

#include "dxil_enums.h"
#include "nir.h"

/* struct taken from DXILContainer
 * Enums values were replaced by uint32_t since the must occupy 32 bit
 */

struct dxil_signature_element {
   uint32_t stream;                   // Stream index (parameters must appear in non-decreasing stream order)
   uint32_t semantic_name_offset;     // Offset to char * stream from start of DxilProgramSignature.
   uint32_t semantic_index;           // Semantic Index
   uint32_t system_value;             // Semantic type. Similar to DxilSemantic::Kind, but a serialized rather than processing rep.
   uint32_t comp_type;                // Type of bits.
   uint32_t reg;                      // Register Index (row index)
   uint8_t  mask;                     // Mask (column allocation)
   union {                            // Unconditional cases useful for validation of shader linkage.
      uint8_t never_writes_mask;      // For an output signature, the shader the signature belongs to never
                                      // writes the masked components of the output register.
      uint8_t always_reads_mask;      // For an input signature, the shader the signature belongs to always
                                      // reads the masked components of the input register.
   };
   uint16_t pad;
   uint32_t min_precision;             // Minimum precision of input/output data
};

struct dxil_signature_record {
   struct dxil_signature_element sig;
   char *name;
};
struct dxil_mdnode;
struct dxil_module;

#ifdef __cplusplus
extern "C" {
#endif

const struct dxil_mdnode *
get_signatures(struct dxil_module *mod, nir_shader *s);

#ifdef __cplusplus
}
#endif

#endif
