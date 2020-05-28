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

#ifndef D3D12_BUFMGR_H
#define D3D12_BUFMGR_H

#include "util/u_atomic.h"

#include <d3d12.h>

struct TransitionableResourceState;

struct d3d12_bo {
   int refcount;
   ID3D12Resource *res;
   struct TransitionableResourceState *trans_state;
};

static inline struct d3d12_bo *
d3d12_bo_get_base(struct d3d12_bo *bo, uint64_t *offset)
{
   *offset = 0;
   return bo;
}

static inline uint64_t
d3d12_bo_get_size(struct d3d12_bo *bo)
{
   return bo->res->GetDesc().Width;
}

struct d3d12_bo *
d3d12_bo_new(ID3D12Device *dev, uint64_t size, uint64_t alignment);

struct d3d12_bo *
d3d12_bo_wrap_res(ID3D12Resource *res, enum pipe_format format);

static inline void
d3d12_bo_reference(struct d3d12_bo *bo)
{
   p_atomic_inc(&bo->refcount);
}

void
d3d12_bo_unreference(struct d3d12_bo *bo);

void *
d3d12_bo_map(struct d3d12_bo *bo, D3D12_RANGE *range);

void
d3d12_bo_unmap(struct d3d12_bo *bo, D3D12_RANGE *range);

#endif
