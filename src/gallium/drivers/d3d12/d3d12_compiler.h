/*
 * Copyright © Microsoft Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef D3D12_COMPILER_H
#define D3D12_COMPILER_H

#include "pipe/p_defines.h"
#include "pipe/p_state.h"

#include "compiler/shader_info.h"

#include "nir.h"

struct pipe_screen;

typedef enum {
   D3D12_BINDING_CONSTANT_BUFFER,
   D3D12_BINDING_SHADER_RESOURCE_VIEW,
   D3D12_BINDING_SAMPLER,
   D3D12_NUM_BINDING_TYPES
} D3D12_BINDING_TYPE;

struct d3d12_validation_tools *d3d12_validator_create();

void d3d12_validator_destroy(struct d3d12_validation_tools *validator);

const void *
d3d12_get_compiler_options(struct pipe_screen *screen,
                           enum pipe_shader_ir ir,
                           enum pipe_shader_type shader);

struct d3d12_shader_key {
   uint64_t required_varying_inputs;
   uint64_t required_varying_outputs;
};

struct d3d12_shader {
   void *bytecode;
   size_t bytecode_length;

   nir_shader *nir;

   unsigned cb_bindings[PIPE_MAX_CONSTANT_BUFFERS];
   size_t num_cb_bindings;

   struct {
      int index;
      int binding;
      uint32_t dimension;
   } srv_bindings[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   size_t num_srv_bindings;

   struct d3d12_shader_key key;
   struct d3d12_shader *next_variant;
};

struct d3d12_shader_selector {
   struct d3d12_shader *first;
   struct d3d12_shader *current;
   nir_shader *nir;
};


struct d3d12_shader_selector *
d3d12_compile_nir(struct d3d12_context *ctx, struct nir_shader *nir);

void
d3d12_shader_free(struct d3d12_shader_selector *shader);

void
d3d12_select_shader_variants(struct d3d12_context *ctx);

uint64_t
d3d12_reassign_driver_locations(exec_list *io);

void
d3d12_sort_by_driver_location(exec_list *io);

void
d3d12_sort_ps_outputs(exec_list *io);

#endif
