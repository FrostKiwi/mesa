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

static struct d3d12_shader *
compile_shader(struct d3d12_context *ctx, struct d3d12_shader_selector *sel,
               struct nir_shader *nir)
{
   struct d3d12_screen *screen = d3d12_screen(ctx->base.screen);
   struct d3d12_shader *shader = rzalloc(sel, d3d12_shader);
   shader->nir = nir;
   sel->current = shader;

   struct nir_lower_tex_options tex_options = { };
   tex_options.lower_txp = ~0u; /* No equivalent for textureProj */

   NIR_PASS_V(nir, nir_lower_tex, &tex_options);
   NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_uniform);
   NIR_PASS_V(nir, nir_lower_clip_halfz);
   NIR_PASS_V(nir, d3d12_lower_bool_loads);
   NIR_PASS_V(nir, d3d12_lower_yflip);

   NIR_PASS_V(nir, d3d12_lower_state_vars, shader);
   NIR_PASS_V(nir, nir_lower_uniforms_to_ubo, 16);

   struct nir_to_dxil_options opts = {};
   opts.interpolate_at_vertex = screen->opts3.BarycentricsSupported;

   struct blob tmp;
   if (!nir_to_dxil(nir, &opts, &tmp)) {
      debug_printf("D3D12: nir_to_dxil failed\n");
      return NULL;
   }

   nir_foreach_variable(var, &nir->uniforms) {
      if (glsl_type_is_sampler(var->type)) {
         shader->srv_bindings[shader->num_srv_bindings].index = var->data.binding;
         shader->srv_bindings[shader->num_srv_bindings].binding = shader->num_srv_bindings;
         shader->srv_bindings[shader->num_srv_bindings].dimension = resource_dimension(glsl_get_sampler_dim(var->type));
         shader->num_srv_bindings++;
      } else if (var->interface_type) {
         if (var->num_state_slots > 0) /* State Vars UBO */
            shader->state_vars_binding = var->data.binding;
         else
            shader->cb_bindings[shader->num_cb_bindings++].binding = var->data.binding;
      }
   }

   ctx->validation_tools->validate_and_sign(&tmp);

   if (d3d12_debug & D3D12_DEBUG_DISASS) {
      ctx->validation_tools->disassemble(&tmp);
   }

   blob_finish_get_buffer(&tmp, &shader->bytecode, &shader->bytecode_length);

   if (d3d12_debug & D3D12_DEBUG_DXIL) {
      char buf[256];
      static int i;
      snprintf(buf, sizeof(buf), "dump%02d.dxil", i++);
      FILE *fp = fopen(buf, "wb");
      fwrite(shader->bytecode, sizeof(char), shader->bytecode_length, fp);
      fclose(fp);
      fprintf(stderr, "wrote '%s'...\n", buf);
   }
   return shader;
}

void
d3d12_fill_self_shader_key(d3d12_shader *shader)
{
   const uint64_t system_generated_in_values =
         1ull << VARYING_SLOT_FACE;

   const uint64_t system_out_values =
         1ull << VARYING_SLOT_POS |
         1ull << VARYING_SLOT_CLIP_DIST0 |
         1ull << VARYING_SLOT_CLIP_DIST1;

   /* We assume that this shader reads and writes exactly what is needed */
   memset(&shader->key, 0, sizeof(d3d12_shader_key));
   shader->key.required_varying_inputs = shader->nir->info.inputs_read & ~system_generated_in_values;
   shader->key.required_varying_outputs = shader->nir->info.outputs_written & ~system_out_values;
}

struct d3d12_shader_selector *
d3d12_compile_nir(struct d3d12_context *ctx, struct nir_shader *nir)
{
   if (nir->info.stage == MESA_SHADER_FRAGMENT)
      d3d12_sort_ps_outputs(&nir->outputs);

   struct d3d12_shader_selector *sel = rzalloc(nullptr, d3d12_shader_selector);

   /* Keep this initial shader as the blue print for possible variants */
   sel->nir = nir_shader_clone(sel, nir);
   sel->first = sel->current = compile_shader(ctx, sel, nir);

   if (sel->current) {
      d3d12_fill_self_shader_key(sel->current);
      return sel;
   }
   ralloc_free(sel);
   return NULL;
}

bool
d3d12_compare_shader_keys(const d3d12_shader_key *expect, const d3d12_shader_key *have)
{
   assert(expect);
   assert(have);

   /* Because we only add varyings we check that a shader has at least the expected in-
    * and outputs. */
   uint64_t delta_in = expect->required_varying_inputs & ~have->required_varying_inputs;
   uint64_t delta_out = expect->required_varying_outputs & ~have->required_varying_outputs;
   return !(delta_in || delta_out);
}

void
d3d12_fill_shader_key(d3d12_shader_key *key, d3d12_shader *prev, d3d12_shader *next)
{
   const uint64_t system_generated_in_values =
         (1ull << VARYING_SLOT_FACE) |
         (1ull << VARYING_SLOT_POS);

   const uint64_t system_out_values =
         1ull << VARYING_SLOT_POS |
         1ull << VARYING_SLOT_CLIP_DIST0 |
         1ull << VARYING_SLOT_CLIP_DIST1;

   memset(key, 0, sizeof(d3d12_shader_key));

   /* We require as inputs what the previous stage has written,
    * except certain system values */
   if (prev)
      key->required_varying_inputs = prev->nir->info.outputs_written & ~system_out_values;

   /* We require as outputs what the next stage reads,
    * except certain system values */
   if (next)
      key->required_varying_outputs = next->nir->info.inputs_read & ~system_generated_in_values;
}

static void
select_shader_variant(struct d3d12_context *ctx, d3d12_shader_selector *sel,
                      d3d12_shader *prev, d3d12_shader *next)
{
   d3d12_shader_key key;
   d3d12_fill_shader_key(&key, prev, next);

   for (d3d12_shader *variant = sel->first; variant;
        variant = variant->next_variant) {

      if (d3d12_compare_shader_keys(&key, &variant->key)) {
         sel->current = variant;
         return;
      }
   }

   /* Clone the nir shader, add the needed in and outputs, and re-sort */
   nir_shader *new_nir_variant = nir_shader_clone(sel, sel->nir);
   uint64_t mask = key.required_varying_inputs & ~new_nir_variant->info.inputs_read;

   if (prev && mask) {
      nir_foreach_variable(var, &prev->nir->outputs) {
         if (mask & (1ull << var->data.location)) {
            nir_variable *new_var = nir_variable_clone(var, new_nir_variant);
            new_var->data.mode = nir_var_shader_in;
            new_var->data.driver_location = exec_list_length(&new_nir_variant->inputs);
            exec_list_push_tail(&new_nir_variant->inputs, &new_var->node);
         }
      }
      d3d12_reassign_driver_locations(&new_nir_variant->inputs);
   }

   mask = key.required_varying_outputs & ~new_nir_variant->info.outputs_written;

   if (next && mask) {
      nir_foreach_variable(var, &next->nir->inputs) {
         if (mask & (1ull << var->data.location)) {
            nir_variable *new_var = nir_variable_clone(var, new_nir_variant);
            new_var->data.mode = nir_var_shader_out;
            new_var->data.driver_location = exec_list_length(&new_nir_variant->outputs);
            exec_list_push_tail(&new_nir_variant->outputs, &new_var->node);
         }
      }
      d3d12_reassign_driver_locations(&new_nir_variant->outputs);
   }

   d3d12_shader *new_variant = compile_shader(ctx, sel, new_nir_variant);
   assert(new_variant);
   new_variant->key = key;

   /* prepend the new shader in the selector chain and pick it */
   new_variant->next_variant = sel->first;
   sel->current = sel->first = new_variant;

   ctx->dirty_program = true;
}

static d3d12_shader *
get_prev_shader(struct d3d12_context *ctx, pipe_shader_type current)
{
   /* No TESS_CTRL or TESS_EVAL yet */

   switch (current) {
   case PIPE_SHADER_VERTEX:
      return NULL;
   case PIPE_SHADER_FRAGMENT:
      if (ctx->gfx_stages[PIPE_SHADER_GEOMETRY])
         return ctx->gfx_stages[PIPE_SHADER_GEOMETRY]->current;
      /* fallthrough */
   case PIPE_SHADER_GEOMETRY:
      assert(ctx->gfx_stages[PIPE_SHADER_VERTEX]);
      return ctx->gfx_stages[PIPE_SHADER_VERTEX]->current;
   default:
      unreachable("shader type not supported");
   }
}

static d3d12_shader *
get_next_shader(struct d3d12_context *ctx, pipe_shader_type current)
{
   /* No TESS_CTRL or TESS_EVAL yet */

   switch (current) {
   case PIPE_SHADER_VERTEX:
      if (ctx->gfx_stages[PIPE_SHADER_GEOMETRY])
         return ctx->gfx_stages[PIPE_SHADER_GEOMETRY]->current;
      /* fallthrough */
   case PIPE_SHADER_GEOMETRY:
      assert(ctx->gfx_stages[PIPE_SHADER_FRAGMENT]);
      return ctx->gfx_stages[PIPE_SHADER_FRAGMENT]->current;
      /* fallthrough */
   case PIPE_SHADER_FRAGMENT:
      return NULL;
   default:
      unreachable("shader type not supported");
   }
}

void
d3d12_select_shader_variants(struct d3d12_context *ctx)
{
   for (int i = 0; i < D3D12_GFX_SHADER_STAGES; ++i) {
      auto sel = ctx->gfx_stages[i];
      if (!sel)
         continue;

      d3d12_shader *prev = get_prev_shader(ctx, (pipe_shader_type)i);
      d3d12_shader *next = get_next_shader(ctx, (pipe_shader_type)i);

      select_shader_variant(ctx, sel, prev, next);
   }
}

void
d3d12_shader_free(struct d3d12_shader_selector *sel)
{
   ralloc_free(sel);
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

/* Order VS inputs according to driver location */
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

/* Sort PS outputs so that color outputs come first */
void
d3d12_sort_ps_outputs(exec_list *io)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   nir_foreach_variable_safe(var, io) {
      exec_node_remove(&var->node);
      /* We use the driver_location here to avoid introducing a new
       * struct or member variable here. The true, updated driver location
       * will be written below, after sorting */
      switch (var->data.location) {
      case FRAG_RESULT_DEPTH:
         var->data.driver_location = 1;
         break;
      case FRAG_RESULT_STENCIL:
         var->data.driver_location = 2;
         break;
      case FRAG_RESULT_SAMPLE_MASK:
         var->data.driver_location = 3;
         break;
      default:
         var->data.driver_location = 0;
      }
      insert_sorted(&new_list, var);
   }
   exec_list_move_nodes_to(&new_list, io);

   unsigned driver_loc = 0;
   nir_foreach_variable(var, io) {
      var->data.driver_location = driver_loc++;
   }
}

/* Order between stage values so that normal varyings come first,
 * then sysvalues and then system generated values.
 */
uint64_t
d3d12_reassign_driver_locations(exec_list *io)
{
   struct exec_list new_list;
   exec_list_make_empty(&new_list);

   uint64_t result = 0;
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
   nir_foreach_variable(var, io) {
      result |= 1ull << var->data.location;
      var->data.driver_location = driver_loc++;
   }
   return result;
}
