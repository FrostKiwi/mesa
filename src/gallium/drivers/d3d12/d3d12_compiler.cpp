/*
 * Copyright 2020 Collabora Ltd.
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

#include "d3d12_compiler.h"
#include "d3d12_debug.h"
#include "microsoft/compiler/nir_to_dxil.h"

#include "pipe/p_state.h"

#include "nir.h"
#include "compiler/nir/nir_builder.h"

#include "util/u_memory.h"

const void *
d3d12_get_compiler_options(struct pipe_screen *screen,
                           enum pipe_shader_ir ir,
                           enum pipe_shader_type shader)
{
   assert(ir == PIPE_SHADER_IR_NIR);
   return dxil_get_nir_compiler_options();
}

struct d3d12_shader *
d3d12_compile_nir(struct nir_shader *nir)
{
   struct d3d12_shader *ret = CALLOC_STRUCT(d3d12_shader);

   ret->info = nir->info;

   struct blob tmp;
   if (!nir_to_dxil(nir, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return NULL;
   }
   blob_finish_get_buffer(&tmp, &ret->bytecode, &ret->bytecode_length);

   if (d3d12_debug & D3D12_DEBUG_DXIL) {
      char buf[256];
      static int i;
      snprintf(buf, sizeof(buf), "dump%02d.dxil", i++);
      FILE *fp = fopen(buf, "wb");
      fwrite(ret->bytecode, sizeof(char), ret->bytecode_length, fp);
      fclose(fp);
      fprintf(stderr, "wrote '%s'...\n", buf);
   }

   return ret;
}

void
d3d12_shader_free(struct d3d12_shader *shader)
{
   FREE(shader);
}
