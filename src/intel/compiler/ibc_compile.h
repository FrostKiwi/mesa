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

#ifndef IBC_COMPILE_H
#define IBC_COMPILE_H

#include "brw_compiler.h"
#include "ibc.h"

#ifdef __cplusplus
extern "C" {
#endif

void
ibc_init_compiler(struct brw_compiler *compiler);

const unsigned *
ibc_compile_tes(const struct brw_compiler *compiler, void *log_data,
                void *mem_ctx,
                const struct brw_tes_prog_key *key,
                const struct brw_vue_map *input_vue_map,
                struct brw_tes_prog_data *prog_data,
                struct nir_shader *shader,
                char **error_str);

const unsigned *
ibc_compile_vs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_vs_prog_key *key,
               struct brw_vs_prog_data *prog_data,
               struct nir_shader *shader,
               char **error_str);

const unsigned *
ibc_compile_fs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_wm_prog_key *key,
               struct brw_wm_prog_data *prog_data,
               struct nir_shader *shader,
               int shader_time_index8,
               int shader_time_index16,
               int shader_time_index32,
               bool allow_spilling,
               bool use_rep_send, struct brw_vue_map *vue_map,
               char **error_str);

const unsigned *
ibc_compile_cs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_cs_prog_key *key,
               struct brw_cs_prog_data *prog_data,
               const struct nir_shader *shader,
               int shader_time_index,
               char **error_str);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_COMPILE_H */
