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

#include "brw_nir.h"

#include "sir.h"
#include "sir_compile.h"

static nir_shader *
compile_cs_to_nir(const struct brw_compiler *compiler,
                  void *mem_ctx,
                  const struct brw_cs_prog_key *key,
                  const nir_shader *src_shader,
                  unsigned dispatch_width)
{
   nir_shader *shader = nir_shader_clone(mem_ctx, src_shader);
   brw_nir_apply_key(shader, compiler, &key->base, dispatch_width, true);
   brw_nir_lower_cs_intrinsics(shader, dispatch_width);
   brw_postprocess_nir(shader, compiler, true);
   return shader;
}

const unsigned *
sir_compile_cs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_cs_prog_key *key,
               struct brw_cs_prog_data *prog_data,
               const struct nir_shader *src_shader,
               int shader_time_index,
               char **error_str)
{
   nir_shader *shader =
      compile_cs_to_nir(compiler, mem_ctx, key, src_shader, 8);

   sir_shader *sir = nir_to_sir(shader, mem_ctx, 8, compiler->devinfo);
   sir_validate_shader(sir);
   return NULL;
}
