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

#include "d3d12_compiler.h"
#include "d3d12_context.h"
#include "d3d12_debug.h"
#include "d3d12_screen.h"
#include "d3d12_nir_passes.h"
#include "microsoft/compiler/nir_to_dxil.h"

#include "pipe/p_state.h"

#include "nir.h"
#include "compiler/nir/nir_builder.h"
#include "tgsi/tgsi_from_mesa.h"

#include "util/u_memory.h"

#include <d3d12.h>
#include <dxcapi.h>
#include <wrl.h>

using Microsoft::WRL::ComPtr;

struct d3d12_validation_tools
{
   d3d12_validation_tools();

   bool validate_and_sign(struct blob *dxil);

   void disassemble(struct blob *dxil);

   void load_dxil_dll();

   struct HModule {
      HModule();
      ~HModule();

      bool load(LPCSTR file_name);
      operator HMODULE () const;
   private:
      HMODULE module;
   };

   HModule dxil_module;
   HModule dxc_compiler_module;
   ComPtr<IDxcCompiler> compiler;
   ComPtr<IDxcValidator> validator;
   ComPtr<IDxcLibrary> library;
};

struct d3d12_validation_tools *d3d12_validator_create()
{
   return new d3d12_validation_tools();
}

void d3d12_validator_destroy(struct d3d12_validation_tools *validator)
{
   delete validator;
}


const void *
d3d12_get_compiler_options(struct pipe_screen *screen,
                           enum pipe_shader_ir ir,
                           enum pipe_shader_type shader)
{
   assert(ir == PIPE_SHADER_IR_NIR);
   return dxil_get_nir_compiler_options();
}

static uint32_t
resource_dimension(enum glsl_sampler_dim dim)
{
   switch (dim) {
   case GLSL_SAMPLER_DIM_1D:
      return RESOURCE_DIMENSION_TEXTURE1D;
   case GLSL_SAMPLER_DIM_2D:
      return RESOURCE_DIMENSION_TEXTURE2D;
   case GLSL_SAMPLER_DIM_3D:
      return RESOURCE_DIMENSION_TEXTURE3D;
   case GLSL_SAMPLER_DIM_CUBE:
      return RESOURCE_DIMENSION_TEXTURECUBE;
   default:
      return RESOURCE_DIMENSION_UNKNOWN;
   }
}

struct d3d12_shader *
d3d12_compile_nir(struct d3d12_context *ctx, struct nir_shader *nir)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   struct d3d12_shader *ret = CALLOC_STRUCT(d3d12_shader);

   ret->info = nir->info;

   struct nir_lower_tex_options tex_options = { };
   tex_options.lower_txp = ~0u; /* No equivalent for textureProj */

   NIR_PASS_V(nir, nir_lower_uniforms_to_ubo, 16);
   NIR_PASS_V(nir, nir_lower_clip_halfz);
   NIR_PASS_V(nir, nir_lower_tex, &tex_options);
   NIR_PASS_V(nir, d3d12_lower_bool_loads);

   struct nir_to_dxil_options opts = {};
   opts.interpolate_at_vertex = screen->opts3.BarycentricsSupported;

   struct blob tmp;
   if (!nir_to_dxil(nir, &opts, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return NULL;
   }

   enum pipe_shader_type stage = pipe_shader_type_from_mesa(nir->info.stage);
   nir_foreach_variable(var, &nir->uniforms) {
      if (glsl_type_is_sampler(var->type)) {
         ret->srv_bindings[ret->num_srv_bindings].index = var->data.binding;
         ret->srv_bindings[ret->num_srv_bindings].binding = ret->num_srv_bindings;
         ret->srv_bindings[ret->num_srv_bindings].dimension = resource_dimension(glsl_get_sampler_dim(var->type));
         ret->num_srv_bindings++;
      } else if (var->interface_type) {
         ret->cb_bindings[ret->num_cb_bindings++] = var->data.binding;
      }
   }

   ctx->validation_tools->validate_and_sign(&tmp);

   if (d3d12_debug & D3D12_DEBUG_DISASS) {
      ctx->validation_tools->disassemble(&tmp);
   }

   blob_finish_get_buffer(&tmp, &ret->bytecode, &ret->bytecode_length);

   if (d3d12_debug & D3D12_DEBUG_DXIL) {
      char buf[256];
      static int i;
      snprintf(buf, sizeof(buf), "dump%02d.dxil", i++);
      FILE *fp = fopen(buf, "wb");
      fwrite(ret->bytecode, sizeof(char), ret->bytecode_length, fp);
      fclose(fp);
      fprintf(stderr, "wrote '%s'...\n", buf);
   }

   return ret;
}

void
d3d12_shader_free(struct d3d12_shader *shader)
{
   FREE(shader);
}

// Used to get path to self
extern "C" extern IMAGE_DOS_HEADER __ImageBase;

void d3d12_validation_tools::load_dxil_dll()
{
   if (!dxil_module.load("dxil.dll")) {
      char selfPath[MAX_PATH] = "";
      uint32_t pathSize = GetModuleFileNameA((HINSTANCE)&__ImageBase, selfPath, sizeof(selfPath));
      if (pathSize == 0 || pathSize == sizeof(selfPath)) {
         debug_printf("D3D12: Unable to get path to self");
         return;
      }

      auto lastSlash = strrchr(selfPath, '\\');
      if (!lastSlash) {
         debug_printf("D3D12: Unable to get path to self");
         return;
      }

      *(lastSlash + 1) = '\0';
      if (strcat_s(selfPath, "dxil.dll") != 0) {
         debug_printf("D3D12: Unable to get path to dxil.dll next to self");
         return;
      }

      dxil_module.load(selfPath);
   }
}

d3d12_validation_tools::d3d12_validation_tools()
{
   load_dxil_dll();
   DxcCreateInstanceProc dxil_create_func = (DxcCreateInstanceProc)GetProcAddress(dxil_module, "DxcCreateInstance");
   assert(dxil_create_func);

   HRESULT hr = dxil_create_func(CLSID_DxcValidator,  IID_PPV_ARGS(&validator));
   if (FAILED(hr)) {
      debug_printf("D3D12: Unable to create validator\n");
   }

   DxcCreateInstanceProc compiler_create_func  = nullptr;
   if(dxc_compiler_module.load("dxcompiler.dll"))
      compiler_create_func = (DxcCreateInstanceProc)GetProcAddress(dxc_compiler_module, "DxcCreateInstance");

   if (compiler_create_func) {
      hr = compiler_create_func(CLSID_DxcLibrary, IID_PPV_ARGS(&library));
      if (FAILED(hr)) {
         debug_printf("D3D12: Unable to create library instance: %x\n", hr);
      }

      if (d3d12_debug & D3D12_DEBUG_DISASS) {
         hr = compiler_create_func(CLSID_DxcCompiler, IID_PPV_ARGS(&compiler));
         if (FAILED(hr)) {
            debug_printf("D3D12: Unable to create compiler instance\n");
         }
      }
   } else if (d3d12_debug & D3D12_DEBUG_DISASS) {
      debug_printf("D3D12: Disassembly requested but compiler couldn't be loaded\n");
   }
}

d3d12_validation_tools::HModule::HModule():
   module(0)
{
}

d3d12_validation_tools::HModule::~HModule()
{
   if (module)
      ::FreeLibrary(module);
}

inline
d3d12_validation_tools::HModule::operator HMODULE () const
{
   return module;
}

bool
d3d12_validation_tools::HModule::load(LPCSTR file_name)
{
   module = ::LoadLibrary(file_name);
   return module != nullptr;
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

bool d3d12_validation_tools::validate_and_sign(struct blob *dxil)
{
   ShaderBlob source(dxil);

   ComPtr<IDxcOperationResult> result;
   if (!validator)
      return false;

   validator->Validate(&source, DxcValidatorFlags_InPlaceEdit, &result);
   HRESULT validationStatus;
   result->GetStatus(&validationStatus);
   if (FAILED(validationStatus) && library) {
      ComPtr<IDxcBlobEncoding> printBlob, printBlobUtf8;
      result->GetErrorBuffer(&printBlob);
      library->GetBlobAsUtf8(printBlob.Get(), printBlobUtf8.GetAddressOf());

      char *errorString;
      if (printBlobUtf8) {
         errorString = reinterpret_cast<char*>(printBlobUtf8->GetBufferPointer());
      }

      errorString[printBlobUtf8->GetBufferSize() - 1] = 0;
      debug_printf("== VALIDATION ERROR =============================================\n%s\n"
                   "== END ==========================================================\n",
                   errorString);

      return false;
   }
   return true;

}

void d3d12_validation_tools::disassemble(struct blob *dxil)
{
   if (!compiler) {
      fprintf(stderr, "D3D12: No Disassembler\n");
      return;
   }
   ShaderBlob source(dxil);
   IDxcBlobEncoding* pDisassembly = nullptr;

   if (FAILED(compiler->Disassemble(&source, &pDisassembly))) {
      fprintf(stderr, "D3D12: Disassembler failed\n");
      return;
   }

   ComPtr<IDxcBlobEncoding> dissassably(pDisassembly);
   ComPtr<IDxcBlobEncoding> blobUtf8;
   library->GetBlobAsUtf8(pDisassembly, blobUtf8.GetAddressOf());
   if (!blobUtf8) {
      fprintf(stderr, "D3D12: Unable to get utf8 encoding\n");
      return;
   }

   char *disassembly = reinterpret_cast<char*>(blobUtf8->GetBufferPointer());
   disassembly[blobUtf8->GetBufferSize() - 1] = 0;

   fprintf(stderr, "== BEGIN SHADER ============================================\n"
           "%s\n"
           "== END SHADER ==============================================\n",
           disassembly);
}

/* Sort io values so that first come normal varyings,
 * then system values, and then system generated values.
 */
static void insert_sorted(struct exec_list *var_list, nir_variable *new_var)
{
   nir_foreach_variable(var, var_list) {
      if (var->data.driver_location > new_var->data.driver_location ||
          (var->data.driver_location == new_var->data.driver_location &&
           var->data.location > new_var->data.location)) {
         exec_node_insert_node_before(&var->node, &new_var->node);
         return;
      }
   }
   exec_list_push_tail(var_list, &new_var->node);
}

/* Order VS inputs and PS outputs according to driver location */
void
d3d12_sort_by_driver_location(exec_list *io)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   nir_foreach_variable_safe(var, io) {
      exec_node_remove(&var->node);
      insert_sorted(&new_list, var);
   }
   exec_list_move_nodes_to(&new_list, io);
}

/* Order between stage values so that normal varyings come first,
 * then sysvalues and then system generated values.
 */
void
d3d12_reassign_driver_locations(exec_list *io)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   nir_foreach_variable_safe(var, io) {
      exec_node_remove(&var->node);
      /* We use the driver_location here to avoid introducing a new
       * struct or member variable here. The true, updated driver location
       * will be written below, after sorting */
      var->data.driver_location = nir_var_to_dxil_sysvalue_type(var);
      insert_sorted(&new_list, var);
   }
   exec_list_move_nodes_to(&new_list, io);

   unsigned driver_loc = 0;
   nir_foreach_variable(var, io)
      var->data.driver_location = driver_loc++;
}
