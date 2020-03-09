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

class ComputeTest : public ::testing::Test {
protected:

   static void
   enable_d3d12_debug_layer()
   {
      HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
      if (!hD3D12Mod) {
         fprintf(stderr, "D3D12: failed to load D3D12.DLL\n");
         return;
      }

      typedef HRESULT(WINAPI * PFN_D3D12_GET_DEBUG_INTERFACE)(REFIID riid,
                                                              void **ppFactory);
      PFN_D3D12_GET_DEBUG_INTERFACE D3D12GetDebugInterface = (PFN_D3D12_GET_DEBUG_INTERFACE)GetProcAddress(hD3D12Mod, "D3D12GetDebugInterface");
      if (!D3D12GetDebugInterface) {
         fprintf(stderr, "D3D12: failed to load D3D12GetDebugInterface from D3D12.DLL\n");
         return;
      }

      ID3D12Debug *debug;
      if (FAILED(D3D12GetDebugInterface(__uuidof(ID3D12Debug), (void **)& debug))) {
         fprintf(stderr, "D3D12: D3D12GetDebugInterface failed\n");
         return;
      }

      debug->EnableDebugLayer();
   }

   static IDXGIFactory4 *
   get_dxgi_factory()
   {
      static const GUID IID_IDXGIFactory4 = {
         0x1bc6ea02, 0xef36, 0x464f,
         { 0xbf, 0x0c, 0x21, 0xca, 0x39, 0xe5, 0x16, 0x8a }
      };

      typedef HRESULT(WINAPI * PFN_CREATE_DXGI_FACTORY)(REFIID riid,
                                                        void **ppFactory);
      PFN_CREATE_DXGI_FACTORY CreateDXGIFactory;

      HMODULE hDXGIMod = LoadLibrary("DXGI.DLL");
      if (!hDXGIMod)
         throw runtime_error("Failed to load DXGI.DLL");

      CreateDXGIFactory = (PFN_CREATE_DXGI_FACTORY)GetProcAddress(hDXGIMod, "CreateDXGIFactory");
      if (!CreateDXGIFactory)
         throw runtime_error("Failed to load CreateDXGIFactory from DXGI.DLL");

      IDXGIFactory4 *factory = NULL;
      HRESULT hr = CreateDXGIFactory(IID_IDXGIFactory4, (void **)&factory);
      if (FAILED(hr))
         throw runtime_error("CreateDXGIFactory failed");

      return factory;
   }

   static IDXGIAdapter1 *
   choose_adapter(IDXGIFactory4 *factory)
   {
      IDXGIAdapter1 *ret;
      if (FAILED(factory->EnumWarpAdapter(__uuidof(IDXGIAdapter1),
          (void **)& ret)))
         throw runtime_error("Failed to enum warp adapter");
      return ret;
   }

   static ID3D12Device *
   create_device(IDXGIAdapter1 *adapter)
   {
      typedef HRESULT(WINAPI *PFN_D3D12CREATEDEVICE)(IUnknown *, D3D_FEATURE_LEVEL, REFIID, void **);
      PFN_D3D12CREATEDEVICE D3D12CreateDevice;

      HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
      if (!hD3D12Mod)
         throw runtime_error("failed to load D3D12.DLL");

      D3D12CreateDevice = (PFN_D3D12CREATEDEVICE)GetProcAddress(hD3D12Mod, "D3D12CreateDevice");
      if (!D3D12CreateDevice)
         throw runtime_error("failed to load D3D12CreateDevice from D3D12.DLL");

      ID3D12Device *dev;
      if (FAILED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_12_0,
          __uuidof(ID3D12Device), (void **)& dev)))
         throw runtime_error("D3D12CreateDevice failed");

      return dev;
   }

   ComPtr<ID3D12RootSignature>
   create_root_signature()
   {
      D3D12_DESCRIPTOR_RANGE desc_range;
      desc_range.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_UAV;
      desc_range.NumDescriptors = 1;
      desc_range.BaseShaderRegister = 0;
      desc_range.RegisterSpace = 0;
      desc_range.OffsetInDescriptorsFromTableStart = 0;

      D3D12_ROOT_PARAMETER root_param;
      root_param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
      root_param.DescriptorTable.NumDescriptorRanges = 1;
      root_param.DescriptorTable.pDescriptorRanges = &desc_range;
      root_param.ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

      D3D12_ROOT_SIGNATURE_DESC root_sig_desc;
      root_sig_desc.NumParameters = 1;
      root_sig_desc.pParameters = &root_param;
      root_sig_desc.NumStaticSamplers = 0;
      root_sig_desc.pStaticSamplers = NULL;
      root_sig_desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_NONE;

      ID3DBlob *sig, *error;
      if (FAILED(D3D12SerializeRootSignature(&root_sig_desc,
          D3D_ROOT_SIGNATURE_VERSION_1,
          &sig, &error)))
         throw runtime_error("D3D12SerializeRootSignature failed");

      ComPtr<ID3D12RootSignature> ret;
      if (FAILED(dev->CreateRootSignature(0,
          sig->GetBufferPointer(),
          sig->GetBufferSize(),
          __uuidof(ret),
          (void **)& ret)))
         throw runtime_error("CreateRootSignature failed");

      return ret;
   }

   ComPtr<ID3D12PipelineState>
   create_pipeline_state(ComPtr<ID3D12RootSignature> &root_sig,
                         const std::vector<uint8_t> &blob)
   {
      D3D12_COMPUTE_PIPELINE_STATE_DESC pipeline_desc = { root_sig.Get() };
      pipeline_desc.CS.pShaderBytecode = blob.data();
      pipeline_desc.CS.BytecodeLength = blob.size();

      ComPtr<ID3D12PipelineState> pipeline_state;
      if (FAILED(dev->CreateComputePipelineState(&pipeline_desc,
                                                 __uuidof(pipeline_state),
                                                 (void **)& pipeline_state)))
         throw runtime_error("Failed to create pipeline state");
      return pipeline_state;
   }

   ComPtr<ID3D12Resource>
   create_buffer(int size, D3D12_HEAP_TYPE heap_type)
   {
      D3D12_RESOURCE_DESC desc;
      desc.Format = DXGI_FORMAT_UNKNOWN;
      desc.Alignment = D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT;
      desc.Width = size;
      desc.Height = 1;
      desc.DepthOrArraySize = 1;
      desc.MipLevels = 1;
      desc.SampleDesc.Count = 1;
      desc.SampleDesc.Quality = 0;
      desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
      desc.Flags = heap_type == D3D12_HEAP_TYPE_DEFAULT ? D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS : D3D12_RESOURCE_FLAG_NONE;
      desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

      D3D12_HEAP_PROPERTIES heap_pris = dev->GetCustomHeapProperties(0, heap_type);

      D3D12_RESOURCE_STATES initial_state = D3D12_RESOURCE_STATE_COMMON;
      switch (heap_type) {
      case D3D12_HEAP_TYPE_UPLOAD:
         initial_state = D3D12_RESOURCE_STATE_GENERIC_READ;
         break;

      case D3D12_HEAP_TYPE_READBACK:
         initial_state = D3D12_RESOURCE_STATE_COPY_DEST;
         break;
      }

      ComPtr<ID3D12Resource> res;
      if (FAILED(dev->CreateCommittedResource(&heap_pris,
          D3D12_HEAP_FLAG_NONE, &desc, initial_state,
          NULL, __uuidof(ID3D12Resource), (void **)&res)))
         throw runtime_error("CreateCommittedResource failed");

      return res;
   }

   ComPtr<ID3D12Resource>
   create_upload_buffer_with_data(const void *data, size_t size)
   {
      auto upload_res = create_buffer(size, D3D12_HEAP_TYPE_UPLOAD);

      void *ptr = NULL;
      D3D12_RANGE res_range = { 0, (SIZE_T)size };
      if (FAILED(upload_res->Map(0, &res_range, (void **)&ptr)))
         throw runtime_error("Failed to map upload-buffer");
      assert(ptr);
      memcpy(ptr, data, size);
      upload_res->Unmap(0, &res_range);
      return upload_res;
   }

   ComPtr<ID3D12Resource>
   create_buffer_with_data(const void *data, size_t size)
   {
      auto upload_res = create_upload_buffer_with_data(data, size);

      auto res = create_buffer(size, D3D12_HEAP_TYPE_DEFAULT);
      resource_barrier(res, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_DEST);
      cmdlist->CopyResource(res.Get(), upload_res.Get());
      resource_barrier(res, D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON);
      execute_cmdlist();

      return res;
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
                    D3D12_RESOURCE_STATES state_after)
   {
      D3D12_RESOURCE_BARRIER barrier;
      barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
      barrier.Flags = D3D12_RESOURCE_BARRIER_FLAG_NONE;
      barrier.Transition.pResource = res.Get();
      barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
      barrier.Transition.StateBefore = state_before;
      barrier.Transition.StateAfter = state_after;
      cmdlist->ResourceBarrier(1, &barrier);
   }

   void execute_cmdlist()
   {
      if (FAILED(cmdlist->Close()))
         throw runtime_error("Closing ID3D12GraphicsCommandList failed");

      ID3D12CommandList *cmdlists[] = { cmdlist };
      cmdqueue->ExecuteCommandLists(1, cmdlists);
      cmdqueue_fence->SetEventOnCompletion(fence_value, event);
      cmdqueue->Signal(cmdqueue_fence, fence_value);
      fence_value++;
      WaitForSingleObject(event, INFINITE);

      if (FAILED(cmdalloc->Reset()))
         throw runtime_error("resetting ID3D12CommandAllocator failed");

      if (FAILED(cmdlist->Reset(cmdalloc, NULL)))
         throw runtime_error("resetting ID3D12GraphicsCommandList failed");
   }

   void create_uav_buffer(ComPtr<ID3D12Resource> res,
                          size_t width, size_t byte_stride,
                          D3D12_CPU_DESCRIPTOR_HANDLE cpu_handle)
   {
      D3D12_UNORDERED_ACCESS_VIEW_DESC uav_desc;
      uav_desc.Format = DXGI_FORMAT_UNKNOWN;
      uav_desc.ViewDimension = D3D12_UAV_DIMENSION_BUFFER;
      uav_desc.Buffer.FirstElement = 0;
      uav_desc.Buffer.NumElements = width;
      uav_desc.Buffer.StructureByteStride = byte_stride;
      uav_desc.Buffer.CounterOffsetInBytes = 0;
      uav_desc.Buffer.Flags = D3D12_BUFFER_UAV_FLAG_NONE;

      dev->CreateUnorderedAccessView(res.Get(), NULL, &uav_desc, cpu_handle);
   }

   void SetUp() override
   {
      enable_d3d12_debug_layer();

      factory = get_dxgi_factory();
      if (!factory)
         throw runtime_error("failed to create DXGI factory");

      adapter = choose_adapter(factory);
      if (!adapter)
         throw runtime_error("failed to choose adapter");

      dev = create_device(adapter);
      if (!dev)
         throw runtime_error("failed to create device");

      if (FAILED(dev->CreateFence(0, D3D12_FENCE_FLAG_NONE,
                                  __uuidof(cmdqueue_fence),
                                  (void **)&cmdqueue_fence)))
         throw runtime_error("failed to create fence\n");

      D3D12_COMMAND_QUEUE_DESC queue_desc;
      queue_desc.Type = D3D12_COMMAND_LIST_TYPE_COMPUTE;
      queue_desc.Priority = D3D12_COMMAND_QUEUE_PRIORITY_NORMAL;
      queue_desc.Flags = D3D12_COMMAND_QUEUE_FLAG_NONE;
      queue_desc.NodeMask = 0;
      if (FAILED(dev->CreateCommandQueue(&queue_desc,
                                         __uuidof(cmdqueue),
                                         (void **)&cmdqueue)))
         throw runtime_error("failed to create command queue");

      if (FAILED(dev->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_COMPUTE,
                __uuidof(cmdalloc), (void **)&cmdalloc)))
         throw runtime_error("failed to create command allocator");

      if (FAILED(dev->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_COMPUTE,
                cmdalloc, NULL, __uuidof(cmdlist), (void **)&cmdlist)))
         throw runtime_error("failed to create command list");

      D3D12_DESCRIPTOR_HEAP_DESC heap_desc;
      heap_desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
      heap_desc.NumDescriptors = 1;
      heap_desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
      heap_desc.NodeMask = 0;
      if (FAILED(dev->CreateDescriptorHeap(&heap_desc,
          __uuidof(uav_heap), (void **)&uav_heap)))
         throw runtime_error("failed to create descriptor heap");

      event = CreateEvent(NULL, FALSE, FALSE, NULL);
      if (!event)
         throw runtime_error("Failed to create event");
      fence_value = 1;
   }

   void TearDown() override
   {
      CloseHandle(event);

      uav_heap->Release();
      cmdlist->Release();
      cmdalloc->Release();
      cmdqueue->Release();
      cmdqueue_fence->Release();
      dev->Release();
      adapter->Release();
      factory->Release();
   }

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

      auto root_sig = create_root_signature();
      auto pipeline_state = create_pipeline_state(root_sig, blob);

      auto res = create_buffer_with_data(input, width * sizeof(T));
      create_uav_buffer(res, width, sizeof(T), uav_heap->GetCPUDescriptorHandleForHeapStart());

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
   int fence_value;

   HANDLE event;
   static PFN_D3D12_SERIALIZE_ROOT_SIGNATURE D3D12SerializeRootSignature;
};
