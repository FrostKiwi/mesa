#include <stdio.h>
#include <stdint.h>

#include <d3d12.h>
#include <dxgi1_4.h>

#include "clc_compiler.h"

const char *kernel_source =
"__kernel void main(__global char *output)\n\
{\n\
    output[get_global_id(0)] = get_global_id(0);\n\
}\n";

#define debug_printf printf

static void
enable_d3d12_debug_layer()
{
   typedef HRESULT(WINAPI * PFN_D3D12_GET_DEBUG_INTERFACE)(REFIID riid,
                                                           void **ppFactory);
   PFN_D3D12_GET_DEBUG_INTERFACE D3D12GetDebugInterface;

   HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
   if (!hD3D12Mod) {
      debug_printf("D3D12: failed to load D3D12.DLL\n");
      return;
   }

   D3D12GetDebugInterface = (PFN_D3D12_GET_DEBUG_INTERFACE)GetProcAddress(hD3D12Mod, "D3D12GetDebugInterface");
   if (!D3D12GetDebugInterface) {
      debug_printf("D3D12: failed to load D3D12GetDebugInterface from D3D12.DLL\n");
      return;
   }

   ID3D12Debug *debug;
   if (FAILED(D3D12GetDebugInterface(__uuidof(ID3D12Debug), (void **)& debug))) {
      debug_printf("D3D12: D3D12GetDebugInterface failed\n");
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
   if (!hDXGIMod) {
      debug_printf("D3D12: failed to load DXGI.DLL\n");
      return NULL;
   }

   CreateDXGIFactory = (PFN_CREATE_DXGI_FACTORY)GetProcAddress(hDXGIMod, "CreateDXGIFactory");
   if (!CreateDXGIFactory) {
      debug_printf("D3D12: failed to load CreateDXGIFactory from DXGI.DLL\n");
      return NULL;
   }

   IDXGIFactory4 *factory = NULL;
   HRESULT hr = CreateDXGIFactory(IID_IDXGIFactory4, (void **)&factory);
   if (FAILED(hr)) {
      debug_printf("D3D12: CreateDXGIFactory failed: %08x\n", hr);
      return NULL;
   }

   return factory;
}

static IDXGIAdapter1 *
choose_adapter(IDXGIFactory4 *factory)
{
   IDXGIAdapter1 *ret;
   if (SUCCEEDED(factory->EnumWarpAdapter(__uuidof(IDXGIAdapter1),
      (void **)& ret)))
      return ret;
   debug_printf("D3D12: failed to enum warp adapter\n");
   return NULL;
}

static ID3D12Device *
create_device(IDXGIAdapter1 *adapter)
{
   typedef HRESULT(WINAPI *PFN_D3D12CREATEDEVICE)(IUnknown *, D3D_FEATURE_LEVEL, REFIID, void **);
   PFN_D3D12CREATEDEVICE D3D12CreateDevice;

   HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
   if (!hD3D12Mod) {
      debug_printf("D3D12: failed to load D3D12.DLL\n");
      return NULL;
   }

   D3D12CreateDevice = (PFN_D3D12CREATEDEVICE)GetProcAddress(hD3D12Mod, "D3D12CreateDevice");
   if (!D3D12CreateDevice) {
      debug_printf("D3D12: failed to load D3D12CreateDevice from D3D12.DLL\n");
      return NULL;
   }

   ID3D12Device *dev;
   if (SUCCEEDED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_12_0,
      __uuidof(ID3D12Device), (void **)& dev)))
      return dev;

   debug_printf("D3D12: D3D12CreateDevice failed\n");
   return NULL;
}

static PFN_D3D12_SERIALIZE_ROOT_SIGNATURE pfD3D12SerializeRootSignature;
#define D3D12SerializeRootSignature pfD3D12SerializeRootSignature

ID3D12RootSignature *
create_root_signature(ID3D12Device *dev)
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
      &sig, &error))) {
      debug_printf("D3D12SerializeRootSignature failed\n");
      return NULL;
   }

   ID3D12RootSignature *ret;
   if (FAILED(dev->CreateRootSignature(0,
      sig->GetBufferPointer(),
      sig->GetBufferSize(),
      __uuidof(ret),
      (void **)& ret))) {
      debug_printf("CreateRootSignature failed\n");
      return NULL;
   }
   return ret;
}

ID3D12Resource *
create_buffer(ID3D12Device *dev, int size, D3D12_HEAP_TYPE heap_type)
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

   ID3D12Resource *res;
   HRESULT hres = dev->CreateCommittedResource(&heap_pris,
      D3D12_HEAP_FLAG_NONE,
      &desc,
      initial_state,
      NULL,
      __uuidof(ID3D12Resource),
      (void **)&res);
   if (FAILED(hres))
      return NULL;

   return res;
}

void
resource_barrier(ID3D12GraphicsCommandList *cmdlist, ID3D12Resource *res,
                 D3D12_RESOURCE_STATES state_before,
                 D3D12_RESOURCE_STATES state_after)
{
   D3D12_RESOURCE_BARRIER barrier;
   barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
   barrier.Flags = D3D12_RESOURCE_BARRIER_FLAG_NONE;
   barrier.Transition.pResource = res;
   barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
   barrier.Transition.StateBefore = state_before;
   barrier.Transition.StateAfter = state_after;
   cmdlist->ResourceBarrier(1, &barrier);
}

void warning_callback(const char *src, int line, const char *str)
{
   fprintf(stderr, "%s(%d): WARNING: %s\n", src, line, str);
}

void error_callback(const char *src, int line, const char *str)
{
   fprintf(stderr, "%s(%d): ERROR: %s\n", src, line, str);
}

#include "dxcapi.h"

bool
validate_module(void *data, size_t size)
{
   static HMODULE hmod = LoadLibrary("DXIL.DLL");
   if (!hmod) {
      debug_printf("D3D12: failed to load DXIL.DLL");
      return false;
   }

   DxcCreateInstanceProc pfnDxcCreateInstance = (DxcCreateInstanceProc)GetProcAddress(hmod, "DxcCreateInstance");
   if (!pfnDxcCreateInstance) {
      debug_printf("D3D12: failed to load DxcCreateInstance");
      return false;
   }

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
                                   (void **)&validator))) {
      debug_printf("D3D12: failed to create IDxcValidator");
      return false;
   }

   IDxcOperationResult *result;
   if (FAILED(validator->Validate(&blob, DxcValidatorFlags_InPlaceEdit,
                                  &result))) {
      // TODO: print error message!
      debug_printf("D3D12: failed to validate");
      validator->Release();
      return false;
   }

   HRESULT hr;
   if (FAILED(result->GetStatus(&hr)) ||
       FAILED(hr)) {
      IDxcBlobEncoding *message;
      result->GetErrorBuffer(&message);
      debug_printf("D3D12: validation failed: %*s\n",
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

const int
width = 4;

int
main()
{
   if (true)
      enable_d3d12_debug_layer();

   static HMODULE hD3D12Mod = LoadLibrary("D3D12.DLL");
   if (!hD3D12Mod) {
      debug_printf("D3D12: failed to load D3D12.DLL\n");
      return -1;
   }

   D3D12SerializeRootSignature = (PFN_D3D12_SERIALIZE_ROOT_SIGNATURE)GetProcAddress(hD3D12Mod, "D3D12SerializeRootSignature");

   IDXGIFactory4 *factory = get_dxgi_factory();
   if (!factory) {
      debug_printf("D3D12: failed to create DXGI factory\n");
      return -1;
   }

   IDXGIAdapter1 *adapter = choose_adapter(factory);
   if (!adapter) {
      debug_printf("D3D12: failed to choose adapter\n");
      return -1;
   }

   ID3D12Device *dev = create_device(adapter);
   if (!dev) {
      debug_printf("D3D12: failed to create device\n");
      return -1;
   }

   HANDLE event = CreateEvent(NULL, FALSE, FALSE, NULL);
   if (!event) {
      debug_printf("D3D12: failed to create event\n");
      return -1;
   }
   ID3D12Fence *cmdqueue_fence;
   if (FAILED(dev->CreateFence(0, D3D12_FENCE_FLAG_NONE, __uuidof(cmdqueue_fence),
      (void **)&cmdqueue_fence))) {
      debug_printf("D3D12: failed to create fence\n");
      return -1;
   }

   D3D12_COMMAND_QUEUE_DESC queue_desc;
   queue_desc.Type = D3D12_COMMAND_LIST_TYPE_COMPUTE;
   queue_desc.Priority = D3D12_COMMAND_QUEUE_PRIORITY_NORMAL;
   queue_desc.Flags = D3D12_COMMAND_QUEUE_FLAG_NONE;
   queue_desc.NodeMask = 0;
   ID3D12CommandQueue *cmdqueue;
   if (FAILED(dev->CreateCommandQueue(&queue_desc,
                                      __uuidof(cmdqueue),
                                      (void **)&cmdqueue))) {
      debug_printf("D3D12: failed to create command queue\n");
      return -1;
   }

   ID3D12CommandAllocator *cmdalloc;
   if (FAILED(dev->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_COMPUTE,
             __uuidof(cmdalloc), (void **)&cmdalloc))) {
      debug_printf("D3D12: failed to create command allocator\n");
      return -1;
   }

   ID3D12GraphicsCommandList *cmdlist;
   if (FAILED(dev->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_COMPUTE,
             cmdalloc, NULL, __uuidof(cmdlist), (void **)&cmdlist))) {
      debug_printf("D3D12: failed to create command list\n");
      return -1;
   }

   struct clc_metadata metadata;
   void *blob = NULL;
   size_t blob_size = 0;
   if (clc_compile_from_source(
       kernel_source,
       "kernel.cl",
       NULL, 0,
       NULL, 0,
       warning_callback,
       error_callback,
       &metadata,
       &blob,
       &blob_size) < 0) {
      fprintf(stderr, "failed to compile kernel!\n");
      return -1;
   }

   FILE *fp = fopen("unsigned.cso", "wb");
   if (fp) {
      fwrite(blob, 1, blob_size, fp);
      fclose(fp);
      debug_printf("D3D12: wrote 'unsigned.cso'...\n");
   }

   if (!validate_module(blob, blob_size))
      return -1;

   fp = fopen("signed.cso", "wb");
   if (fp) {
      fwrite(blob, 1, blob_size, fp);
      fclose(fp);
      debug_printf("D3D12: wrote 'signed.cso'...\n");
   }

   ID3D12RootSignature *root_sig = create_root_signature(dev);

   D3D12_COMPUTE_PIPELINE_STATE_DESC pipeline_desc = { root_sig };
   pipeline_desc.CS.pShaderBytecode = blob;
   pipeline_desc.CS.BytecodeLength = blob_size;

   ID3D12PipelineState *pipeline_state;
   if (FAILED(dev->CreateComputePipelineState(&pipeline_desc,
                                              __uuidof(pipeline_state),
                                              (void **)& pipeline_state))) {
      debug_printf("D3D12: failed to create pipeline state\n");
      return -1;
   }

   D3D12_DESCRIPTOR_HEAP_DESC heap_desc;
   heap_desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
   heap_desc.NumDescriptors = 1;
   heap_desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
   heap_desc.NodeMask = 0;

   ID3D12DescriptorHeap *uav_heap;
   if (FAILED(dev->CreateDescriptorHeap(&heap_desc,
      __uuidof(uav_heap), (void **)&uav_heap))) {
      debug_printf("D3D12: failed to create descriptor heap\n");
      return -1;
   }
   ID3D12Resource *upload_res = create_buffer(dev, width * sizeof(uint32_t), D3D12_HEAP_TYPE_UPLOAD);
   if (!upload_res) {
      debug_printf("D3D12: failed to create resource heap\n");
      return -1;
   }
   ID3D12Resource *res = create_buffer(dev, width * sizeof(uint32_t), D3D12_HEAP_TYPE_DEFAULT);
   if (!res) {
      debug_printf("D3D12: failed to create resource heap\n");
      return -1;
   }

   ID3D12Resource *readback_res = create_buffer(dev, width * sizeof(uint32_t), D3D12_HEAP_TYPE_READBACK);
   if (!readback_res) {
      debug_printf("D3D12: failed to create resource heap\n");
      return -1;
   }

   uint32_t *data = NULL;
   D3D12_RANGE res_range = { 0, sizeof(uint32_t) * width };
   if (FAILED(upload_res->Map(0, &res_range, (void **)&data))) {
      debug_printf("D3D12: failed to map buffer\n");
      return -1;
   }
   for (int i = 0; i < width; ++i) {
      data[i] = 0xdeadbeef;
   }
   upload_res->Unmap(0, &res_range);

   D3D12_UNORDERED_ACCESS_VIEW_DESC uav_desc;
   uav_desc.Format = DXGI_FORMAT_R32_UINT;
   uav_desc.ViewDimension = D3D12_UAV_DIMENSION_BUFFER;
   uav_desc.Buffer.FirstElement = 0;
   uav_desc.Buffer.NumElements = width;
   uav_desc.Buffer.StructureByteStride = 0;
   uav_desc.Buffer.CounterOffsetInBytes = 0;
   uav_desc.Buffer.Flags = D3D12_BUFFER_UAV_FLAG_NONE;

   dev->CreateUnorderedAccessView(res, NULL, &uav_desc, uav_heap->GetCPUDescriptorHandleForHeapStart());

   resource_barrier(cmdlist, res, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_DEST);
   cmdlist->CopyResource(res, upload_res);
   resource_barrier(cmdlist, res, D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
   cmdlist->SetDescriptorHeaps(1, &uav_heap);
   cmdlist->SetComputeRootSignature(root_sig);
   cmdlist->SetComputeRootDescriptorTable(0, uav_heap->GetGPUDescriptorHandleForHeapStart());
   cmdlist->SetPipelineState(pipeline_state);
   cmdlist->Dispatch(width, 1, 1);
   resource_barrier(cmdlist, res, D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE);
   cmdlist->CopyResource(readback_res, res);

   if (FAILED(cmdlist->Close())) {
      debug_printf("D3D12: closing ID3D12GraphicsCommandList failed\n");
      return -1;
   }

   ID3D12CommandList *cmdlists[] = { cmdlist };
   cmdqueue->ExecuteCommandLists(1, cmdlists);
   int value = 1;
   cmdqueue_fence->SetEventOnCompletion(value, event);
   cmdqueue->Signal(cmdqueue_fence, value);
   WaitForSingleObject(event, INFINITE);

   if (FAILED(readback_res->Map(0, &res_range, (void **)&data))) {
      debug_printf("D3D12: failed to map buffer\n");
      return -1;
   }
   int ret = 0;
   for (int i = 0; i < width; ++i) {
      if (data[i] != i) {
         printf("ERROR: expected 0x%08x, got 0x%08x\n", i, data[i]);
         ret = -1;
      }
   }
   D3D12_RANGE empty_range = { 0, 0 };
   readback_res->Unmap(0, &empty_range);

   if (FAILED(cmdalloc->Reset())) {
      debug_printf("D3D12: resetting ID3D12CommandAllocator failed\n");
      return -1;
   }

   if (FAILED(cmdlist->Reset(cmdalloc, NULL))) {
      debug_printf("D3D12: resetting ID3D12GraphicsCommandList failed\n");
      return -1;
   }

   upload_res->Release();
   readback_res->Release();
   res->Release();
   uav_heap->Release();
   pipeline_state->Release();
   root_sig->Release();
   cmdlist->Release();
   cmdalloc->Release();
   cmdqueue->Release();
   cmdqueue_fence->Release();
   dev->Release();
   adapter->Release();
   factory->Release();
   return ret;
}
