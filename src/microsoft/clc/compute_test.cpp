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
#include "compute_test.h"
#include "dxcapi.h"

using std::runtime_error;
using Microsoft::WRL::ComPtr;

void
ComputeTest::enable_d3d12_debug_layer()
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

IDXGIFactory4 *
ComputeTest::get_dxgi_factory()
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

IDXGIAdapter1 *
ComputeTest::choose_adapter(IDXGIFactory4 *factory)
{
   IDXGIAdapter1 *ret;
   if (FAILED(factory->EnumWarpAdapter(__uuidof(IDXGIAdapter1),
       (void **)& ret)))
      throw runtime_error("Failed to enum warp adapter");
   return ret;
}

ID3D12Device *
ComputeTest::create_device(IDXGIAdapter1 *adapter)
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
ComputeTest::create_root_signature(int num_uavs, int num_cbvs)
{
   D3D12_DESCRIPTOR_RANGE desc_ranges[2];
   unsigned num_desc_ranges = 0;
   if (num_uavs > 0) {
      desc_ranges[num_desc_ranges].RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_UAV;
      desc_ranges[num_desc_ranges].NumDescriptors = num_uavs;
      desc_ranges[num_desc_ranges].BaseShaderRegister = 0;
      desc_ranges[num_desc_ranges].RegisterSpace = 0;
      desc_ranges[num_desc_ranges].OffsetInDescriptorsFromTableStart = num_desc_ranges;
      num_desc_ranges++;
   }

   if (num_cbvs > 0) {
      desc_ranges[num_desc_ranges].RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_CBV;
      desc_ranges[num_desc_ranges].NumDescriptors = num_cbvs;
      desc_ranges[num_desc_ranges].BaseShaderRegister = 0;
      desc_ranges[num_desc_ranges].RegisterSpace = 0;
      desc_ranges[num_desc_ranges].OffsetInDescriptorsFromTableStart = num_desc_ranges;
      num_desc_ranges++;
   }

   D3D12_ROOT_PARAMETER root_param;
   root_param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
   root_param.DescriptorTable.NumDescriptorRanges = num_desc_ranges;
   root_param.DescriptorTable.pDescriptorRanges = desc_ranges;
   root_param.ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

   D3D12_ROOT_SIGNATURE_DESC root_sig_desc;
   root_sig_desc.NumParameters = num_uavs + num_cbvs;
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
ComputeTest::create_pipeline_state(ComPtr<ID3D12RootSignature> &root_sig,
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
ComputeTest::create_buffer(int size, D3D12_HEAP_TYPE heap_type)
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
ComputeTest::create_upload_buffer_with_data(const void *data, size_t size)
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
ComputeTest::create_sized_buffer_with_data(size_t buffer_size,
                                           const void *data,
                                           size_t data_size)
{
   auto upload_res = create_upload_buffer_with_data(data, data_size);

   auto res = create_buffer(buffer_size, D3D12_HEAP_TYPE_DEFAULT);
   resource_barrier(res, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_DEST);
   cmdlist->CopyBufferRegion(res.Get(), 0, upload_res.Get(), 0, data_size);
   resource_barrier(res, D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON);
   execute_cmdlist();

   return res;
}

void
ComputeTest::resource_barrier(ComPtr<ID3D12Resource> &res,
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

void
ComputeTest::execute_cmdlist()
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

void
ComputeTest::create_uav_buffer(ComPtr<ID3D12Resource> res,
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

void
ComputeTest::SetUp()
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
   heap_desc.NumDescriptors = 1000;
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

void
ComputeTest::TearDown()
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

PFN_D3D12_SERIALIZE_ROOT_SIGNATURE ComputeTest::D3D12SerializeRootSignature;

void warning_callback(const char *src, int line, const char *str)
{
   fprintf(stderr, "%s(%d): WARNING: %s\n", src, line, str);
}

void error_callback(const char *src, int line, const char *str)
{
   fprintf(stderr, "%s(%d): ERROR: %s\n", src, line, str);
}

bool
validate_module(void *data, size_t size)
{
   static HMODULE hmod = LoadLibrary("DXIL.DLL");
   if (!hmod)
      throw runtime_error("failed to load DXIL.DLL");

   DxcCreateInstanceProc pfnDxcCreateInstance =
      (DxcCreateInstanceProc)GetProcAddress(hmod, "DxcCreateInstance");
   if (!pfnDxcCreateInstance)
      throw runtime_error("failed to load DxcCreateInstance");

   struct shader_blob : public IDxcBlob {
      shader_blob(void *data, size_t size) : data(data), size(size) {}
      LPVOID STDMETHODCALLTYPE GetBufferPointer() override { return data; }
      SIZE_T STDMETHODCALLTYPE GetBufferSize() override { return size; }
      HRESULT STDMETHODCALLTYPE QueryInterface(REFIID, void **) override { return E_NOINTERFACE; }
      ULONG STDMETHODCALLTYPE AddRef() override { return 1; }
      ULONG STDMETHODCALLTYPE Release() override { return 0; }
      void *data;
      size_t size;
   } blob(data, size);

   IDxcValidator *validator;
   if (FAILED(pfnDxcCreateInstance(CLSID_DxcValidator, __uuidof(IDxcValidator),
                                   (void **)&validator)))
      throw runtime_error("failed to create IDxcValidator");

   IDxcOperationResult *result;
   if (FAILED(validator->Validate(&blob, DxcValidatorFlags_InPlaceEdit,
                                  &result)))
      throw runtime_error("Validate failed");

   HRESULT hr;
   if (FAILED(result->GetStatus(&hr)) ||
       FAILED(hr)) {
      IDxcBlobEncoding *message;
      result->GetErrorBuffer(&message);
      fprintf(stderr, "D3D12: validation failed: %*s\n",
                   (int)message->GetBufferSize(),
                   (char *)message->GetBufferPointer());
      message->Release();
      validator->Release();
      result->Release();
      return false;
   }

   validator->Release();
   result->Release();
   return true;
}

static void
dump_blob(const char *path, const void *data, size_t size)
{
   FILE *fp = fopen(path, "wb");
   if (fp) {
      fwrite(data, 1, size, fp);
      fclose(fp);
      printf("D3D12: wrote '%s'...\n", path);
   }
}

std::vector<uint8_t>
ComputeTest::compile_and_validate(const char *kernel_source,
                                  struct clc_metadata *metadata)
{
   void *blob;
   size_t size;
   if (clc_compile_from_source(
       kernel_source, "kernel.cl",
       NULL, 0,
       NULL, 0,
       warning_callback, error_callback,
       metadata,
       &blob, &size) < 0)
      throw runtime_error("failed to compile kernel!");

   dump_blob("unsigned.cso", blob, size);
   if (!validate_module(blob, size))
      throw runtime_error("failed to validate module!");
   dump_blob("signed.cso", blob, size);

   std::vector<uint8_t> ret;
   ret.assign(static_cast<uint8_t *>(blob),
              static_cast<uint8_t *>(blob) + size);
   clc_free_blob(blob);
   return ret;
}
