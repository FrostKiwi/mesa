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

static bool copy_char_skip_double_ws(stringstream &os, char c, bool last_was_ws);

void NirToDXILTest::run(const string& in_shader, const string& dxil_expect) const
{
   glsl_type_singleton_init_or_ref();

   nir_shader* shader = nir_shader_from_string(in_shader.c_str(), &nir_options);
   ASSERT_TRUE(shader != nullptr);

   stringstream dxil_expect_condesed;
   bool last_was_ws = false;
   for (auto c: dxil_expect) {
      last_was_ws = copy_char_skip_double_ws(dxil_expect_condesed, c, last_was_ws);
   }

   struct nir_to_dxil_options opts = {};
   opts.interpolate_at_vertex = true;

   struct blob tmp;
   bool convert_success = nir_to_dxil(shader, &opts, &tmp);
   ralloc_free(shader);
   ASSERT_TRUE(convert_success);

   string dxil_disass;
   auto compiler =  DXILCompiler::instance();
   ASSERT_TRUE(compiler != nullptr);
   bool dissasamble_success = compiler->disassamble(&tmp, dxil_disass);
   ASSERT_TRUE(dissasamble_success);

   if (dxil_expect_condesed.str().length() > 0)
      EXPECT_EQ(dxil_disass, dxil_expect_condesed.str());

   bool validate_success = compiler->validate(&tmp);
   ASSERT_TRUE(validate_success);

   blob_finish(&tmp);
   glsl_type_singleton_decref();
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
   ComPtr<IDxcBlobEncoding> pDisassembly = nullptr;

   if (FAILED(m_compiler->Disassemble(&source, &pDisassembly)))
      return false;

   ComPtr<IDxcBlobEncoding> blobUtf8;
   m_library->GetBlobAsUtf8(pDisassembly.Get(), blobUtf8.GetAddressOf());
   if (!blobUtf8)
      return false;

   char *c = reinterpret_cast<char *>(blobUtf8->GetBufferPointer());
   assert(c);
   c[blobUtf8->GetBufferSize() - 1] = 0;

   stringstream os;
   bool in_comment = false;
   bool linestart = true;
   bool last_was_ws = false;
   while (*c) {
      if (!in_comment) {
         if (linestart && ((*c == ';') || (*c == '!' && c[1] == '0'))) {
               in_comment = true;
         } else {
            /* condense ws */
            bool is_ws = isspace(*c);
            if (!(is_ws && last_was_ws))
                os << *c;
            last_was_ws = is_ws;
         }
      }

      if (*c == '\n') {
         linestart = true;
         in_comment = false;
      } else
         linestart = false;

      ++c;
   }
   /* Different versions of dxcompiler seem to disagree whether a new line should
    * be added at the end or not */
   if (!linestart)
      os << '\n';

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

static bool
copy_char_skip_double_ws(stringstream &os, char c, bool last_was_ws)
{
   bool retval = isspace(c);
   if (!(retval && last_was_ws))
       os << c;
   return retval;
}

ComPtr<IDxcLibrary> DXILCompiler::m_library;
ComPtr<IDxcCompiler> DXILCompiler::m_compiler;
ComPtr<IDxcValidator> DXILCompiler::m_validator;
bool DXILCompiler::m_initialized = false;
