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

#include "d3d12_bufmgr.h"
#include "d3d12_format.h"
#include "d3d12_screen.h"

#include "D3D12ResourceState.h"

#include "util/format/u_format.h"
#include "util/u_memory.h"

#include <d3d12.h>

static struct TransitionableResourceState *
create_trans_state(ID3D12Resource *res, enum pipe_format format)
{
   D3D12_RESOURCE_DESC desc = res->GetDesc();

   // Calculate the total number of subresources
   unsigned arraySize = desc.Dimension == D3D12_RESOURCE_DIMENSION_TEXTURE3D ?
                        1 : desc.DepthOrArraySize;
   unsigned total_subresources = desc.MipLevels *
                                 arraySize *
                                 d3d12_non_opaque_plane_count(desc.Format);
   total_subresources *= util_format_has_stencil(util_format_description(format)) ?
                         2 : 1;
   bool simultaneous_access = !!(desc.Flags & D3D12_RESOURCE_FLAG_ALLOW_SIMULTANEOUS_ACCESS);

   return new TransitionableResourceState(res,
                                          total_subresources,
                                          simultaneous_access);
}

struct d3d12_bo *
d3d12_bo_wrap_res(ID3D12Resource *res, enum pipe_format format)
{
   struct d3d12_bo *bo;

   bo = CALLOC_STRUCT(d3d12_bo);
   if (!bo)
      return NULL;

   bo->refcount = 1;
   bo->res = res;
   bo->trans_state = create_trans_state(res, format);

   return bo;
}

struct d3d12_bo *
d3d12_bo_new(ID3D12Device *dev, uint64_t size, uint64_t alignment)
{
   ID3D12Resource *res;

   D3D12_RESOURCE_DESC res_desc;
   res_desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
   res_desc.Format = DXGI_FORMAT_UNKNOWN;
   res_desc.Alignment = alignment;
   res_desc.Width = size;
   res_desc.Height = 1;
   res_desc.DepthOrArraySize = 1;
   res_desc.MipLevels = 1;
   res_desc.SampleDesc.Count = 1;
   res_desc.SampleDesc.Quality = 0;
   res_desc.Flags = D3D12_RESOURCE_FLAG_NONE;
   res_desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

   D3D12_HEAP_PROPERTIES heap_pris = dev->GetCustomHeapProperties(0, D3D12_HEAP_TYPE_UPLOAD);
   HRESULT hres = dev->CreateCommittedResource(&heap_pris,
                                               D3D12_HEAP_FLAG_NONE,
                                               &res_desc,
                                               D3D12_RESOURCE_STATE_COMMON,
                                               NULL,
                                               __uuidof(ID3D12Resource),
                                               (void **)&res);

   if (FAILED(hres))
      return NULL;

   return d3d12_bo_wrap_res(res, PIPE_FORMAT_NONE);
}

void
d3d12_bo_unreference(struct d3d12_bo *bo)
{
   if (bo == NULL)
      return;

   assert(p_atomic_read(&bo->refcount) > 0);

   if (p_atomic_dec_zero(&bo->refcount)) {
      delete bo->trans_state;
      bo->res->Release();
      FREE(bo);
   }
}

void *
d3d12_bo_map(struct d3d12_bo *bo, D3D12_RANGE *range)
{
   struct d3d12_bo *base_bo;
   D3D12_RANGE offset_range = {0, 0};
   uint64_t offset;
   void *ptr;

   base_bo = d3d12_bo_get_base(bo, &offset);

   if (!range || offset == 0) {
      /* Nothing to do */
   } else if (range->Begin >= range->End) {
      offset_range.Begin = offset;
      offset_range.End = offset + d3d12_bo_get_size(bo);
      range = &offset_range;
   } else {
      offset_range.Begin = range->Begin + offset;
      offset_range.End = range->End + offset;
      range = &offset_range;
   }

   if (FAILED(base_bo->res->Map(0, range, &ptr)))
      return NULL;

   return (uint8_t *)ptr + (range ? range->Begin : 0);
}

void
d3d12_bo_unmap(struct d3d12_bo *bo, D3D12_RANGE *range)
{
   struct d3d12_bo *base_bo;
   D3D12_RANGE offset_range = {0, 0};
   uint64_t offset;

   base_bo = d3d12_bo_get_base(bo, &offset);

   if (!range || bo == base_bo)
   {
      /* Nothing to do */
   } else if (range->Begin >= range->End) {
      offset_range.Begin = offset;
      offset_range.End = offset + base_bo->res->GetDesc().Width;
   } else {
      offset_range.Begin = range->Begin + offset;
      offset_range.End = range->End + offset;
   }

   base_bo->res->Unmap(0, range);
}
