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

#include "clc_compiler.h"
#include "../compiler/dxil_module.h"
#include "../compiler/dxil_container.h"
#include "../compiler/dxil.h"

#include "util/u_debug.h"

#include <stdint.h>

static bool
emit_type_comdats(struct dxil_module *m)
{
   return true; /* nothing for now */
}

static bool
emit_metadata(struct dxil_module *m)
{
   // metadata
   return dxil_module_enter_subblock(m, 15, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 5) &&
          dxil_module_emit_bits(m, 0x00000001, 1) &&
          dxil_module_emit_bits(m, 0x00000001, 8) &&
          dxil_module_emit_bits(m, 0x00000000, 1) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000000, 1) &&
          dxil_module_emit_bits(m, 0x00000001, 3) &&
          dxil_module_emit_bits(m, 0x00000008, 5) &&
          dxil_module_emit_bits(m, 0x00000002, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 5) &&
          dxil_module_emit_bits(m, 0x00000001, 1) &&
          dxil_module_emit_bits(m, 0x00000004, 8) &&
          dxil_module_emit_bits(m, 0x00000000, 1) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000000, 1) &&
          dxil_module_emit_bits(m, 0x00000001, 3) &&
          dxil_module_emit_bits(m, 0x00000008, 5) &&
          dxil_module_emit_bits(m, 0x00000004, 3) &&
          dxil_module_emit_bits(m, 0x0000002a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000063, 8) &&
          dxil_module_emit_bits(m, 0x0000006c, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000067, 8) &&
          dxil_module_emit_bits(m, 0x00000020, 8) &&
          dxil_module_emit_bits(m, 0x00000076, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000020, 8) &&
          dxil_module_emit_bits(m, 0x00000033, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000037, 8) &&
          dxil_module_emit_bits(m, 0x00000020, 8) &&
          dxil_module_emit_bits(m, 0x00000028, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x00000067, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x0000002f, 8) &&
          dxil_module_emit_bits(m, 0x00000052, 8) &&
          dxil_module_emit_bits(m, 0x00000045, 8) &&
          dxil_module_emit_bits(m, 0x0000004c, 8) &&
          dxil_module_emit_bits(m, 0x00000045, 8) &&
          dxil_module_emit_bits(m, 0x00000041, 8) &&
          dxil_module_emit_bits(m, 0x00000053, 8) &&
          dxil_module_emit_bits(m, 0x00000045, 8) &&
          dxil_module_emit_bits(m, 0x0000005f, 8) &&
          dxil_module_emit_bits(m, 0x00000033, 8) &&
          dxil_module_emit_bits(m, 0x00000037, 8) &&
          dxil_module_emit_bits(m, 0x00000030, 8) &&
          dxil_module_emit_bits(m, 0x0000002f, 8) &&
          dxil_module_emit_bits(m, 0x00000066, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x0000006c, 8) &&
          dxil_module_emit_bits(m, 0x00000029, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000063, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000007, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000007, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x0000000e, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 3) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x0000004f, 8) &&
          dxil_module_emit_bits(m, 0x00000075, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000070, 8) &&
          dxil_module_emit_bits(m, 0x00000075, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000042, 8) &&
          dxil_module_emit_bits(m, 0x00000075, 8) &&
          dxil_module_emit_bits(m, 0x00000066, 8) &&
          dxil_module_emit_bits(m, 0x00000066, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000008, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x0000000d, 6) &&
          dxil_module_emit_bits(m, 0x0000000d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000009, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x0000000d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000000b, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000009, 6) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000000b, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x0000000e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x0000000f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000010, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x0000000f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 3) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000068, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x0000000b, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000007, 6) &&
          dxil_module_emit_bits(m, 0x00000014, 6) &&
          dxil_module_emit_bits(m, 0x00000015, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000016, 6) &&
          dxil_module_emit_bits(m, 0x0000000d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000013, 6) &&
          dxil_module_emit_bits(m, 0x00000017, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000012, 6) &&
          dxil_module_emit_bits(m, 0x00000018, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x0000001b, 6) &&
          dxil_module_emit_bits(m, 0x0000001b, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x0000001c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000001a, 6) &&
          dxil_module_emit_bits(m, 0x0000001d, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 3) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x0000006d, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000013, 6) &&
          dxil_module_emit_bits(m, 0x00000020, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 6) &&
          dxil_module_emit_bits(m, 0x0000001a, 6) &&
          dxil_module_emit_bits(m, 0x0000001f, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000011, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x0000006c, 8) &&
          dxil_module_emit_bits(m, 0x0000006c, 8) &&
          dxil_module_emit_bits(m, 0x00000076, 8) &&
          dxil_module_emit_bits(m, 0x0000006d, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000078, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000076, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x00000009, 6) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000078, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000076, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x0000006c, 8) &&
          dxil_module_emit_bits(m, 0x00000076, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x0000000e, 6) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000078, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000068, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x0000004d, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x0000006c, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000007, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000078, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x00000075, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000063, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000010, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x00000012, 6) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000078, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000079, 8) &&
          dxil_module_emit_bits(m, 0x00000070, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x00000041, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000061, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000018, 6) &&
          dxil_module_emit_bits(m, 0x0000001d, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 3) &&
          dxil_module_emit_bits(m, 0x0000000e, 6) &&
          dxil_module_emit_bits(m, 0x00000064, 8) &&
          dxil_module_emit_bits(m, 0x00000078, 8) &&
          dxil_module_emit_bits(m, 0x0000002e, 8) &&
          dxil_module_emit_bits(m, 0x00000065, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000072, 8) &&
          dxil_module_emit_bits(m, 0x00000079, 8) &&
          dxil_module_emit_bits(m, 0x00000050, 8) &&
          dxil_module_emit_bits(m, 0x0000006f, 8) &&
          dxil_module_emit_bits(m, 0x00000069, 8) &&
          dxil_module_emit_bits(m, 0x0000006e, 8) &&
          dxil_module_emit_bits(m, 0x00000074, 8) &&
          dxil_module_emit_bits(m, 0x00000073, 8) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_exit_block(m);
}

static bool
emit_metadata_store(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, 15, 3) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000000, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000027, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000030, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000026, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000007, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000026, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000030, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000028, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000004, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000027, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x00000005, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000035, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x0000000f, 6) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000029, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000036, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000029, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x00000007, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000029, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000030, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000008, 6) &&
          dxil_module_emit_bits(m, 0x00000008, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000029, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x00000009, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000030, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x0000001e, 6) &&
          dxil_module_emit_bits(m, 0x0000000a, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000036, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x0000002d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002d, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000030, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000003f, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000030, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000003f, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000008, 6) &&
          dxil_module_emit_bits(m, 0x0000000b, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000035, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000010, 6) &&
          dxil_module_emit_bits(m, 0x0000000c, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000026, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000018, 6) &&
          dxil_module_emit_bits(m, 0x0000000d, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000026, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000003f, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000003f, 6) &&
          dxil_module_emit_bits(m, 0x00000002, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000035, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000019, 6) &&
          dxil_module_emit_bits(m, 0x0000000e, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000038, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000028, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000033, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000035, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000023, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000029, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000035, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000025, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 3) &&
          dxil_module_emit_bits(m, 0x00000006, 6) &&
          dxil_module_emit_bits(m, 0x00000011, 6) &&
          dxil_module_emit_bits(m, 0x0000000f, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000038, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000024, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000022, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000027, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002e, 6) &&
          dxil_module_emit_bits(m, 0x00000001, 6) &&
          dxil_module_emit_bits(m, 0x00000036, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000032, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002c, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000021, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000039, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x0000002f, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000035, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_emit_bits(m, 0x00000034, 6) &&
          dxil_module_emit_bits(m, 0x00000003, 6) &&
          dxil_module_exit_block(m);
}

static bool
emit_value_symbol_table(struct dxil_module *m)
{
   return dxil_module_enter_subblock(m, DXIL_VALUE_SYMTAB_BLOCK, 4) &&
          dxil_module_emit_symtab_entry(m, 0, "\01?OutputBuffer@@3V?$RWBuffer@I@@A") &&
          dxil_module_emit_symtab_entry(m, 3, "dx.op.bufferStore.i32") &&
          dxil_module_emit_symtab_entry(m, 4, "dx.op.createHandle") &&
          dxil_module_emit_symtab_entry(m, 2, "dx.op.threadId.i32") &&
          dxil_module_emit_symtab_entry(m, 1, "main") &&
          dxil_module_exit_block(m);
}

static bool
emit_use_list_block(struct dxil_module *m)
{
   return true; /* nothing for now */
}

static bool
emit_function_bodies(struct dxil_module *m)
{
   dxil_module_enter_subblock(m, 12, 4);
      dxil_module_emit_bits(m, 0x00000003, 4);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_enter_subblock(m, 11, 4);
         dxil_module_emit_bits(m, 0x00000004, 4);
         dxil_module_emit_bits(m, 0x00000000, 5);
         dxil_module_emit_bits(m, 0x00000005, 4);
         dxil_module_emit_bits(m, 0x00000072, 8);
         dxil_module_emit_bits(m, 0x00000005, 4);
         dxil_module_emit_bits(m, 0x000000ba, 8);
         dxil_module_emit_bits(m, 0x00000001, 8);
         dxil_module_emit_bits(m, 0x00000005, 4);
         dxil_module_emit_bits(m, 0x0000008a, 8);
         dxil_module_emit_bits(m, 0x00000001, 8);
         dxil_module_emit_bits(m, 0x00000003, 4);
         dxil_module_emit_bits(m, 0x00000003, 6);
         dxil_module_emit_bits(m, 0x00000000, 6);
         dxil_module_emit_bits(m, 0x00000004, 4);
         dxil_module_emit_bits(m, 0x00000008, 5);
         dxil_module_emit_bits(m, 0x00000005, 4);
         dxil_module_emit_bits(m, 0x00000002, 8);
         dxil_module_emit_bits(m, 0x00000005, 4);
         dxil_module_emit_bits(m, 0x0000001e, 8);
      dxil_module_exit_block(m);
      dxil_module_emit_bits(m, 0x00000003, 4);
      dxil_module_emit_bits(m, 0x00000022, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000009, 6);
      dxil_module_emit_bits(m, 0x00000000, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x0000000e, 6);
      dxil_module_emit_bits(m, 0x00000012, 6);
      dxil_module_emit_bits(m, 0x00000006, 6);
      dxil_module_emit_bits(m, 0x00000002, 6);
      dxil_module_emit_bits(m, 0x00000010, 6);
      dxil_module_emit_bits(m, 0x00000010, 6);
      dxil_module_emit_bits(m, 0x00000009, 6);
      dxil_module_emit_bits(m, 0x00000003, 4);
      dxil_module_emit_bits(m, 0x00000022, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000006, 6);
      dxil_module_emit_bits(m, 0x00000000, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000006, 6);
      dxil_module_emit_bits(m, 0x00000015, 6);
      dxil_module_emit_bits(m, 0x00000006, 6);
      dxil_module_emit_bits(m, 0x00000011, 6);
      dxil_module_emit_bits(m, 0x00000003, 4);
      dxil_module_emit_bits(m, 0x00000022, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x0000000d, 6);
      dxil_module_emit_bits(m, 0x00000000, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000020, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x0000000b, 6);
      dxil_module_emit_bits(m, 0x00000015, 6);
      dxil_module_emit_bits(m, 0x00000006, 6);
      dxil_module_emit_bits(m, 0x00000002, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000005, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000001, 6);
      dxil_module_emit_bits(m, 0x00000003, 6);
      dxil_module_emit_bits(m, 0x00000008, 4);
      dxil_module_enter_subblock(m, 14, 4);
         dxil_module_emit_bits(m, 0x00000006, 4);
         dxil_module_emit_bits(m, 0x00000016, 8);
         dxil_module_emit_bits(m, 0x00000014, 6);
         dxil_module_emit_bits(m, 0x00000028, 6);
         dxil_module_emit_bits(m, 0x00000014, 6);
         dxil_module_emit_bits(m, 0x00000013, 6);
         dxil_module_emit_bits(m, 0x0000000f, 6);
         dxil_module_emit_bits(m, 0x00000014, 6);
         dxil_module_emit_bits(m, 0x00000013, 6);
         dxil_module_emit_bits(m, 0x0000001b, 6);
         dxil_module_emit_bits(m, 0x00000014, 6);
         dxil_module_emit_bits(m, 0x00000005, 6);
         dxil_module_emit_bits(m, 0x00000005, 6);
         dxil_module_emit_bits(m, 0x00000004, 6);
         dxil_module_emit_bits(m, 0x00000011, 6);
         dxil_module_emit_bits(m, 0x0000003f, 6);
         dxil_module_emit_bits(m, 0x0000002e, 6);
         dxil_module_emit_bits(m, 0x0000001a, 6);
         dxil_module_emit_bits(m, 0x0000002f, 6);
         dxil_module_emit_bits(m, 0x0000003f, 6);
         dxil_module_emit_bits(m, 0x00000001, 6);
         dxil_module_emit_bits(m, 0x00000014, 6);
         dxil_module_emit_bits(m, 0x00000005, 6);
      dxil_module_exit_block(m);
   dxil_module_exit_block(m);

   return true;
}

static bool
emit_module(struct dxil_module *m)
{
   if (!dxil_module_emit_bits(m, 'B', 8) ||
       !dxil_module_emit_bits(m, 'C', 8) ||
       !dxil_module_emit_bits(m, 0xC0, 8) ||
       !dxil_module_emit_bits(m, 0xDE, 8))
      return false;

   uint64_t version = 1;
   if (!dxil_module_enter_subblock(m, DXIL_MODULE, 3) ||
       !dxil_module_emit_record(m, DXIL_MODULE_CODE_VERSION, &version, 1))
      return false;

   struct dxil_attrib attrs1[] = {
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_NO_UNWIND },
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_READ_NONE },
   };
   struct dxil_attrib attrs2[] = {
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_NO_UNWIND },
   };
   struct dxil_attrib attrs3[] = {
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_NO_UNWIND },
      { DXIL_ATTR_ENUM, DXIL_ATTR_KIND_READ_ONLY },
   };

   struct dxil_attrib *attrs[] = {
      attrs1, attrs2, attrs3
   };
   size_t attr_sizes[] = {
      ARRAY_SIZE(attrs1), ARRAY_SIZE(attrs2), ARRAY_SIZE(attrs3)
   };
   assert(ARRAY_SIZE(attrs) == ARRAY_SIZE(attr_sizes));

   unsigned attr_data[] = {
      1, 2, 3
   };

   struct dxil_type *int32_type = dxil_module_add_int_type(m, 32);
   struct dxil_type *rwbuffer_struct_type = dxil_module_add_struct_type(m, "class.RWBuffer<unsigned int>", &int32_type, 1);
   struct dxil_type *rwbuffer_pointer_type = dxil_module_add_pointer_type(m, rwbuffer_struct_type);

   struct dxil_type *void_type = dxil_module_add_void_type(m);
   struct dxil_type *main_func_type = dxil_module_add_function_type(m, void_type, NULL, 0);
   struct dxil_type *main_func_pointer_type = dxil_module_add_pointer_type(m, main_func_type);

   struct dxil_type *threadid_args[] = { int32_type, int32_type };
   struct dxil_type *threadid_func_type = dxil_module_add_function_type(m, int32_type, threadid_args, ARRAY_SIZE(threadid_args));
   struct dxil_type *threadid_func_pointer_type = dxil_module_add_pointer_type(m, threadid_func_type);

   struct dxil_type *int8_type = dxil_module_add_int_type(m, 8);
   struct dxil_type *int8_pointer_type = dxil_module_add_pointer_type(m, int8_type);
   struct dxil_type *handle_type = dxil_module_add_struct_type(m, "dx.types.Handle", &int8_pointer_type, 1);

   struct dxil_type *bufferstore_args[] = { int32_type, handle_type, int32_type, int32_type, int32_type, int32_type, int32_type, int32_type, int8_type };
   struct dxil_type *bufferstore_func_type = dxil_module_add_function_type(m, void_type, bufferstore_args, ARRAY_SIZE(bufferstore_args));
   struct dxil_type *bufferstore_func_pointer_type = dxil_module_add_pointer_type(m, bufferstore_func_type);

   struct dxil_type *bool_type = dxil_module_add_int_type(m, 1);

   struct dxil_type *createhandle_args[] = { int32_type, int8_type, int32_type, int32_type, bool_type };
   struct dxil_type *createhandle_func_type = dxil_module_add_function_type(m, handle_type, createhandle_args, ARRAY_SIZE(createhandle_args));
   struct dxil_type *createhandle_func_pointer_type = dxil_module_add_pointer_type(m, createhandle_func_type);

   struct dxil_function_module_info funcs[] = {
      { main_func_type, false, 0 },
      { threadid_func_type, true, 1 },
      { bufferstore_func_type, true, 2 },
      { createhandle_func_type, true, 3 }
   };

   struct dxil_const consts[] = {
      { int32_type, .int_value = 1 },
      { int32_type, .int_value = 0 },
      { int32_type, .int_value = 6 },
      { int32_type, .int_value = 10 },
      { int32_type, .int_value = 5 },
      { int32_type, .int_value = 4 },
      { int32_type, .int_value = 3 },
      { int32_type, .int_value = 7 },
      { bool_type, .int_value = 0 },
      { rwbuffer_pointer_type, .undef = true },
      { rwbuffer_struct_type, .undef = true },
   };

   const int num_type_bits = 5;
   if (!dxil_module_emit_blockinfo(m, num_type_bits) ||
       !dxil_emit_attrib_group_table(m, attrs, attr_sizes,
                                     ARRAY_SIZE(attrs)) ||
       !dxil_emit_attribute_table(m, attr_data, ARRAY_SIZE(attr_data)) ||
       !dxil_module_emit_type_table(m, num_type_bits) ||
       !emit_type_comdats(m) ||
       !dxil_emit_module_info(m, funcs, ARRAY_SIZE(funcs)) ||
       !dxil_emit_module_consts(m, consts, ARRAY_SIZE(consts)) ||
       !emit_metadata(m) ||
       !emit_metadata_store(m) ||
       !emit_value_symbol_table(m) ||
       !emit_use_list_block(m) ||
       !emit_function_bodies(m))
      return false;

   return dxil_module_exit_block(m);
}

int clc_compile_from_source(
   const char *source,
   const char *source_name,
   const struct clc_define defines[], // should be sorted by name
   size_t num_defines,
   const struct clc_header headers[], // should be sorted by name
   size_t num_headers,
   clc_msg_callback warning_callback,
   clc_msg_callback error_callback,
   struct clc_metadata *metadata,
   void **blob,
   size_t *blob_size)
{

   struct dxil_container container;
   dxil_container_init(&container);

   struct dxil_features features = { 0 };
   if (!dxil_container_add_features(&container, &features)) {
      debug_printf("D3D12: dxil_container_add_features failed\n");
      return -1;
   }

   if (!dxil_container_add_input_signature(&container) ||
       !dxil_container_add_output_signature(&container)) {
      debug_printf("D3D12: failed to write input/output signature\n");
      return -1;
   }

   const struct dxil_resource resources[] = {
      { DXIL_RES_UAV_TYPED, 0, 0, 0 }
   };
   if (!dxil_container_add_state_validation(&container, resources,
                                            ARRAY_SIZE(resources))) {
      debug_printf("D3D12: failed to write state-validation\n");
      return -1;
   }

   struct dxil_module mod;
   dxil_module_init(&mod);
   mod.shader_kind = DXIL_COMPUTE_SHADER;
   mod.major_version = 6;
   mod.minor_version = 0;
   if (!emit_module(&mod)) {
      debug_printf("D3D12: dxil_container_add_module failed\n");
      return -1;
   }

   if (!dxil_container_add_module(&container, &mod)) {
      debug_printf("D3D12: failed to write module\n");
      return -1;
   }

   struct blob tmp;
   blob_init(&tmp);
   if (!dxil_container_write(&container, &tmp)) {
      debug_printf("D3D12: dxil_container_write failed\n");
      return -1;
   }

   blob_finish_get_buffer(&tmp, blob, blob_size);
   return 0;
}
