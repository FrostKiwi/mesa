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
   class ObjectArray : public std::vector<struct clc_object *> {
   public:
      ~ObjectArray() {
         for (auto obj : *this)
            clc_free_object(obj);
      }
   };

   struct Shader {
      std::shared_ptr<struct clc_object> obj;
      std::shared_ptr<struct clc_dxil_object> dxil;
   };

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
                         const struct clc_dxil_object &dxil);

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

   void
   get_buffer_data(ComPtr<ID3D12Resource> res,
                   void *buf, size_t size);

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
   add_uav_resource(std::vector<ComPtr<ID3D12Resource>> &resources,
                    const void *data = NULL, size_t num_elems = 0,
                    size_t elem_size = 0);

   void
   add_cbv_resource(std::vector<ComPtr<ID3D12Resource>> &resources,
                    const void *data, size_t size);

   void
   SetUp() override;

   void
   TearDown() override;

   static Shader
   compile_and_validate(const std::vector<const char *> &sources);

   template <typename T>
   std::vector<T>
   run_shader_with_inputs(const std::vector<const char *> &sources,
                          const std::vector<std::vector<T>> &inputs)
   {
      if (inputs.size() < 1)
         throw runtime_error("no inputs");

      static HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
      if (!hD3D12Mod)
         throw runtime_error("Failed to load D3D12.DLL");

      D3D12SerializeVersionedRootSignature = (PFN_D3D12_SERIALIZE_VERSIONED_ROOT_SIGNATURE)GetProcAddress(hD3D12Mod, "D3D12SerializeVersionedRootSignature");

      Shader shader = compile_and_validate(sources);
      std::shared_ptr<struct clc_dxil_object> &dxil = shader.dxil;

      if (inputs.size() != dxil->metadata.num_uavs)
         throw runtime_error("incorrect number of inputs");

      auto root_sig = create_root_signature(dxil->metadata.num_uavs,
                                            dxil->metadata.num_consts);
      auto pipeline_state = create_pipeline_state(root_sig, *dxil);

      std::vector<ComPtr<ID3D12Resource>> resources;
      for(auto input : inputs) {
         if (input.size() != inputs[0].size())
            throw runtime_error("mismatching input sizes");

         add_uav_resource(resources, input.data(),
                          input.size(), sizeof(T));
      }

      for (unsigned i = 0; i < dxil->metadata.num_consts; ++i)
         add_cbv_resource(resources, dxil->metadata.consts[i].data,
                          dxil->metadata.consts[i].size);

      cmdlist->SetDescriptorHeaps(1, &uav_heap);
      cmdlist->SetComputeRootSignature(root_sig.Get());
      cmdlist->SetComputeRootDescriptorTable(0, uav_heap->GetGPUDescriptorHandleForHeapStart());
      cmdlist->SetPipelineState(pipeline_state.Get());
      cmdlist->Dispatch(inputs[0].size(), 1, 1);

      for(unsigned i = 0; i < inputs.size(); i++)
         resource_barrier(resources[i], D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON);

      execute_cmdlist();

      std::vector<T> out(inputs[0].size());
      get_buffer_data(resources[0], out.data(), out.size() * sizeof(T));

      return out;
   }

   template <typename T>
   std::vector<T>
   run_shader_with_inputs(const char *kernel_source,
                          const std::vector<std::vector<T>> &inputs)
   {
      std::vector<const char *> srcs = { kernel_source };
      return run_shader_with_inputs(srcs, inputs);
   }

   template <typename T>
   std::vector<T>
   run_shader_with_input(const std::vector<const char *> &sources,
                         const std::vector<T> &input)
   {
      std::vector<std::vector<T>> inputs = { input };
      return run_shader_with_inputs(sources, inputs);
   }

   template <typename T>
   std::vector<T>
   run_shader_with_input(const char *kernel_source,
                         const std::vector<T> &input)
   {
      std::vector<std::vector<T>> inputs = { input };
      return run_shader_with_inputs(kernel_source, inputs);
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
   static PFN_D3D12_SERIALIZE_VERSIONED_ROOT_SIGNATURE D3D12SerializeVersionedRootSignature;
};
