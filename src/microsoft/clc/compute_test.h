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

#include <stdio.h>
#include <stdint.h>
#include <stdexcept>

#include <d3d12.h>
#include <dxgi1_4.h>
#include <gtest/gtest.h>
#include <wrl.h>

#include "clc_compiler.h"

using std::runtime_error;
using Microsoft::WRL::ComPtr;

inline D3D12_CPU_DESCRIPTOR_HANDLE
offset_cpu_handle(D3D12_CPU_DESCRIPTOR_HANDLE handle, UINT offset)
{
   handle.ptr += offset;
   return handle;
}

inline size_t
align(size_t value, unsigned alignment)
{
   assert(alignment > 0);
   return ((value + (alignment - 1)) / alignment) * alignment;
}

class ComputeTest : public ::testing::Test {
protected:

   static void
   enable_d3d12_debug_layer();

   static IDXGIFactory4 *
   get_dxgi_factory();

   static IDXGIAdapter1 *
   choose_adapter(IDXGIFactory4 *factory);

   static ID3D12Device *
   create_device(IDXGIAdapter1 *adapter);

   ComPtr<ID3D12RootSignature>
   create_root_signature(int num_uavs, int num_cbvs);

   ComPtr<ID3D12PipelineState>
   create_pipeline_state(ComPtr<ID3D12RootSignature> &root_sig,
                         const std::vector<uint8_t> &blob);

   ComPtr<ID3D12Resource>
   create_buffer(int size, D3D12_HEAP_TYPE heap_type);

   ComPtr<ID3D12Resource>
   create_upload_buffer_with_data(const void *data, size_t size);

   ComPtr<ID3D12Resource>
   create_sized_buffer_with_data(size_t buffer_size, const void *data,
                                 size_t data_size);

   ComPtr<ID3D12Resource>
   create_buffer_with_data(const void *data, size_t size)
   {
      return create_sized_buffer_with_data(size, data, size);
   }

   template <typename T>
   std::vector<T>
   get_buffer_data(ComPtr<ID3D12Resource> res, size_t width)
   {
      auto readback_res = create_buffer(sizeof(T) * width, D3D12_HEAP_TYPE_READBACK);
      resource_barrier(res, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_SOURCE);
      cmdlist->CopyResource(readback_res.Get(), res.Get());
      resource_barrier(res, D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_COMMON);
      execute_cmdlist();

      T *data = NULL;
      D3D12_RANGE res_range = { 0, (SIZE_T)(sizeof(T) * width) };
      if (FAILED(readback_res->Map(0, &res_range, (void **)&data)))
         throw runtime_error("Failed to map readback-buffer");

      std::vector<T> ret;
      ret.assign(data, data + width);

      D3D12_RANGE empty_range = { 0, 0 };
      readback_res->Unmap(0, &empty_range);
      return ret;
   }

   void
   resource_barrier(ComPtr<ID3D12Resource> &res,
                    D3D12_RESOURCE_STATES state_before,
                    D3D12_RESOURCE_STATES state_after);

   void
   execute_cmdlist();

   void
   create_uav_buffer(ComPtr<ID3D12Resource> res,
                     size_t width, size_t byte_stride,
                     D3D12_CPU_DESCRIPTOR_HANDLE cpu_handle);

   void create_cbv(ComPtr<ID3D12Resource> res, size_t size,
                   D3D12_CPU_DESCRIPTOR_HANDLE cpu_handle);

   void
   SetUp() override;

   void
   TearDown() override;

   static std::vector<uint8_t>
   compile_and_validate(const char *kernel_source,
                        struct clc_metadata *metadata);

   template <typename T>
   std::vector<T>
   run_shader_with_input(const char *kernel_source,
                                      int width, const T *input)
   {
      static HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
      if (!hD3D12Mod)
         throw runtime_error("Failed to load D3D12.DLL");

      D3D12SerializeRootSignature = (PFN_D3D12_SERIALIZE_ROOT_SIGNATURE)GetProcAddress(hD3D12Mod, "D3D12SerializeRootSignature");

      struct clc_metadata metadata;
      std::vector<uint8_t> blob = compile_and_validate(kernel_source, &metadata);

      auto root_sig = create_root_signature(1, metadata.num_consts);
      auto pipeline_state = create_pipeline_state(root_sig, blob);

      auto res = create_buffer_with_data(input, width * sizeof(T));
      create_uav_buffer(res, width, sizeof(T), uav_heap->GetCPUDescriptorHandleForHeapStart());

      for (unsigned i = 0; i < metadata.num_consts; ++i) {
         size_t size = metadata.consts[i].size;
         unsigned cbuf_size = align(size * sizeof(T), 256);
         auto cbuf = create_sized_buffer_with_data(cbuf_size,
                                                   metadata.consts[i].data,
                                                   size);
         create_cbv(cbuf, cbuf_size,
                    offset_cpu_handle(
                       uav_heap->GetCPUDescriptorHandleForHeapStart(),
                       (1 + i) * uav_heap_incr));
      }

      resource_barrier(res, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
      cmdlist->SetDescriptorHeaps(1, &uav_heap);
      cmdlist->SetComputeRootSignature(root_sig.Get());
      cmdlist->SetComputeRootDescriptorTable(0, uav_heap->GetGPUDescriptorHandleForHeapStart());
      cmdlist->SetPipelineState(pipeline_state.Get());
      cmdlist->Dispatch(width, 1, 1);
      resource_barrier(res, D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON);
      execute_cmdlist();

      return get_buffer_data<T>(res, width);
   }

   IDXGIFactory4 *factory;
   IDXGIAdapter1 *adapter;
   ID3D12Device *dev;
   ID3D12Fence *cmdqueue_fence;
   ID3D12CommandQueue *cmdqueue;
   ID3D12CommandAllocator *cmdalloc;
   ID3D12GraphicsCommandList *cmdlist;
   ID3D12DescriptorHeap *uav_heap;

   UINT uav_heap_incr;
   int fence_value;

   HANDLE event;
   static PFN_D3D12_SERIALIZE_ROOT_SIGNATURE D3D12SerializeRootSignature;
};
