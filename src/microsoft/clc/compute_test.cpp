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

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

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
