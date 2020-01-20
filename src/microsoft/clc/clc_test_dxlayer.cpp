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

#include "clc_test_dxlayer.h"
#include "util/blob.h"
#include "nir.h"
#include "microsoft/compiler/nir_to_dxil.h"

#include <d3d12.h>
#include <dxcapi.h>
#include <wrl.h>

#include <winerror.h>
#include <string>
#include <sstream>
#include <cassert>

#pragma comment(lib, "dxcompiler")

using std::string;
using std::stringstream;

static const nir_shader_compiler_options nir_options = { 0 };

using Microsoft::WRL::ComPtr;

class DXILCompiler {
public:
   bool disassamble(blob* data, std::string& disassembly);
   bool validate(blob* data);
   static DXILCompiler *instance();

private:


   DXILCompiler() = default;

   static ComPtr<IDxcLibrary> m_library;
   static ComPtr<IDxcCompiler> m_compiler;
   static ComPtr<IDxcValidator> m_validator;
   static bool m_initialized;
};

void NirToDXILTest::run(const string& in_shader, const string& dxil_expect) const
{
   glsl_type_singleton_init_or_ref();

   nir_shader* shader = nir_shader_from_string(in_shader.c_str(), &nir_options);
   ASSERT_TRUE(shader != nullptr);

   struct blob tmp;
   bool convert_success = nir_to_dxil(shader, &tmp);
   ralloc_free(shader);
   ASSERT_TRUE(convert_success);

   string dxil_disass;
   auto compiler =  DXILCompiler::instance();
   ASSERT_TRUE(compiler != nullptr);
   bool dissasamble_success = compiler->disassamble(&tmp, dxil_disass);
   ASSERT_TRUE(dissasamble_success);

   EXPECT_EQ(dxil_disass, dxil_expect);

   bool validate_success = compiler->validate(&tmp);
   ASSERT_TRUE(validate_success);

   glsl_type_singleton_init_or_ref();
}

class ShaderBlob : public IDxcBlob {
public:
   ShaderBlob(blob* data) : m_data(data) {}

   LPVOID STDMETHODCALLTYPE GetBufferPointer(void) override { return m_data->data; }

   SIZE_T STDMETHODCALLTYPE GetBufferSize() override { return m_data->size; }

   HRESULT STDMETHODCALLTYPE QueryInterface(REFIID, void**) override { return E_NOINTERFACE; }

   ULONG STDMETHODCALLTYPE AddRef() override { return 1; }

   ULONG STDMETHODCALLTYPE Release() override { return 0; }

   blob* m_data;
};


bool DXILCompiler::disassamble(blob *data,
                               string& disassembly)
{
   // Disassamble and convert to std::string
   ShaderBlob source(data);
   IDxcBlobEncoding* pDisassembly = nullptr;

   if (FAILED(m_compiler->Disassemble(&source, &pDisassembly)))
      return false;

   ComPtr<IDxcBlobEncoding> dissassably(pDisassembly);
   ComPtr<IDxcBlobEncoding> blobUtf8;
   m_library->GetBlobAsUtf8(pDisassembly, blobUtf8.GetAddressOf());
   if (!blobUtf8)
      return false;

   disassembly = reinterpret_cast<const char*>(blobUtf8->GetBufferPointer());
   disassembly[blobUtf8->GetBufferSize() - 1] = 0;

   stringstream os;

   const char* c = disassembly.c_str();
   assert(c);

   bool in_comment = false;
   bool linestart = true;
   while (*c) {

      if (!in_comment) {
         if (linestart && ((*c == ';') || (*c == '!' && c[1] == '0'))) {
               in_comment = true;
         } else {
            os << *c;
         }
      }

      if (*c == '\n') {
         linestart = true;
         in_comment = false;
      } else
         linestart = false;

      ++c;
   }
   disassembly = os.str();
   return true;
}

bool DXILCompiler::validate(blob *data)
{
   ShaderBlob source(data);
   ComPtr<IDxcOperationResult> result;
   m_validator->Validate(&source, DxcValidatorFlags_InPlaceEdit, &result);
   HRESULT validationStatus;
   result->GetStatus(&validationStatus);
   if (FAILED(validationStatus)) {
      ComPtr<IDxcBlobEncoding> printBlob, printBlobUtf8;
      result->GetErrorBuffer(&printBlob);
      m_library->GetBlobAsUtf8(printBlob.Get(), printBlobUtf8.GetAddressOf());

      std::string errorString;
      if (printBlobUtf8) {
         errorString = reinterpret_cast<const char*>(printBlobUtf8->GetBufferPointer());
      }

      std::cerr << "== VALIDATION ERROR =============================================\n"
                << errorString  << '\n'
                << "== END ==========================================================\n";

      return false;
   }
   return true;
}

DXILCompiler *DXILCompiler::instance()
{
   // TODO? not reentrant
   static DXILCompiler me;
   if (!m_initialized) {
     HRESULT hr = DxcCreateInstance(CLSID_DxcLibrary, IID_PPV_ARGS(&m_library));
      if (FAILED(hr))
         return nullptr;

      hr = DxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(&m_compiler));
      if (FAILED(hr))
         return nullptr;

      hr = DxcCreateInstance(CLSID_DxcValidator, IID_PPV_ARGS(&m_validator));
      if (FAILED(hr))
         return nullptr;

      m_initialized = true;
   }
   return &me;
}

ComPtr<IDxcLibrary> DXILCompiler::m_library;
ComPtr<IDxcCompiler> DXILCompiler::m_compiler;
ComPtr<IDxcValidator> DXILCompiler::m_validator;
bool DXILCompiler::m_initialized = false;

