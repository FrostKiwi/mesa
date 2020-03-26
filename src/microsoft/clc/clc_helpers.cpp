//
// Copyright 2012-2016 Francisco Jerez
// Copyright 2012-2016 Advanced Micro Devices, Inc.
// Copyright 2014-2016 Jan Vesely
// Copyright 2014-2015 Serge Martin
// Copyright 2015 Zoltan Gilian
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

#include <sstream>

#include <llvm/IR/DiagnosticPrinter.h>
#include <llvm/IR/DiagnosticInfo.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm-c/Target.h>
#include <LLVMSPIRVLib/LLVMSPIRVLib.h>

#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Lex/PreprocessorOptions.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Basic/TargetInfo.h>

#include "clc_helpers.h"

using ::llvm::Function;
using ::llvm::LLVMContext;
using ::llvm::Module;
using ::llvm::raw_string_ostream;

static void
llvm_log_handler(const ::llvm::DiagnosticInfo &di, void *data) {
   raw_string_ostream os { *reinterpret_cast<std::string *>(data) };
   ::llvm::DiagnosticPrinterRawOStream printer { os };
   di.print(printer);
}

int
clc_to_spirv(const struct clc_compile_args *args,
             struct spirv_binary *spvbin,
             char **err_buf)
{
   LLVMInitializeAllTargets();
   LLVMInitializeAllTargetInfos();
   LLVMInitializeAllTargetMCs();
   LLVMInitializeAllAsmPrinters();

   // FIXME: Is hard-coding 64-bit OK?
   //        The below is parachuted in from create_compiler_instance(), but
   //        neutered to avoid a device dependency.
   const std::string target_triple = "spir-unknown-unknown";
   const std::string clc_version_human = "1.2";
   const std::string clc_version_define = "120";
   clang::LangStandard::Kind clc_version_clang = clang::LangStandard::lang_opencl12;

   std::string log;
   std::unique_ptr<LLVMContext> llvm_ctx { new LLVMContext };
   llvm_ctx->setDiagnosticHandlerCallBack(llvm_log_handler, &log);

   std::unique_ptr<clang::CompilerInstance> c { new clang::CompilerInstance };
   clang::TextDiagnosticBuffer *diag_buffer = new clang::TextDiagnosticBuffer;
   clang::DiagnosticsEngine diag { new clang::DiagnosticIDs,
         new clang::DiagnosticOptions, diag_buffer };

   std::vector<const char *> clang_opts = { args->source.name };

   if (!clang::CompilerInvocation::CreateFromArgs(c->getInvocation(),
#if LLVM_VERSION_MAJOR >= 10
                                                  clang_opts,
#else
                                                  clang_opts.data(),
                                                  clang_opts.data() + clang_opts.size(),
#endif
                                                  diag)) {
      log += "Couldn't create Clang invocation.\n";
      *err_buf = strdup(log.c_str());
      return -1;
   }

   diag_buffer->FlushDiagnostics(diag);
   if (diag.hasErrorOccurred()) {
      log += "Errors occurred during Clang invocation.\n";
      *err_buf = strdup(log.c_str());
      return -1;
   }

   c->getTargetOpts().CPU = "";
   c->getTargetOpts().Triple = target_triple;
   c->getLangOpts().NoBuiltin = true;

   // This is a workaround for a Clang bug which causes the number
   // of warnings and errors to be printed to stderr.
   // http://www.llvm.org/bugs/show_bug.cgi?id=19735
   c->getDiagnosticOpts().ShowCarets = false;

   c->getInvocation().setLangDefaults(c->getLangOpts(),
#if LLVM_VERSION_MAJOR >= 10
                                      clang::Language::OpenCL,
#else
                                      clang::InputKind::OpenCL,
#endif
                                      ::llvm::Triple(target_triple),
                                      c->getPreprocessorOpts(),
                                      clc_version_clang);

   c->createDiagnostics(new clang::TextDiagnosticPrinter(
                           *new raw_string_ostream(log),
                           &c->getDiagnosticOpts(), true));

   c->setTarget(clang::TargetInfo::CreateTargetInfo(
                   c->getDiagnostics(), c->getInvocation().TargetOpts));

   c->getFrontendOpts().ProgramAction = clang::frontend::EmitLLVMOnly;
   c->getHeaderSearchOpts().UseBuiltinIncludes = true;
   c->getHeaderSearchOpts().UseStandardSystemIncludes = true;
   c->getHeaderSearchOpts().ResourceDir = CLANG_RESOURCE_DIR;

   // Add opencl-c generic search path and include
   c->getHeaderSearchOpts().AddPath(CLANG_RESOURCE_DIR,
                                    clang::frontend::Angled,
                                    false, false);
   c->getPreprocessorOpts().Includes.push_back("opencl-c.h");

   // Add definition for the OpenCL version
   c->getPreprocessorOpts().addMacroDef("__OPENCL_VERSION__=" +
                                        clc_version_define);

   for (size_t i = 0; i < args->num_defines; i++) {
      std::string def = std::string(args->defines[i].name);
      if (args->defines[i].value != nullptr)
         def += "=" + std::string(args->defines[i].value);
      c->getPreprocessorOpts().addMacroDef(def);
   }

   if (args->num_headers) {
      const std::string tmp_header_path = "/tmp/clover/";

      c->getHeaderSearchOpts().AddPath(tmp_header_path,
                                       clang::frontend::Angled,
                                       false, false);


      for (size_t i = 0; i < args->num_headers; i++) {
         std::string h = std::string(args->headers[i].name);
         std::string src = std::string(args->headers[i].value);
         c->getPreprocessorOpts().addRemappedFile(tmp_header_path + h,
            ::llvm::MemoryBuffer::getMemBufferCopy(src).release());
      }
   }

   c->getPreprocessorOpts().addRemappedFile(
           args->source.name,
           ::llvm::MemoryBuffer::getMemBufferCopy(std::string(args->source.value)).release());

   // Compile the code
   clang::EmitLLVMOnlyAction act(llvm_ctx.get());
   if (!c->ExecuteAction(act)) {
      log += "Error executing LLVM compilation action.\n";
      *err_buf = strdup(log.c_str());
      return -1;
   }

   auto mod = act.takeModule();
   if (!::llvm::regularizeLlvmForSpirv(mod.get(), log)) {
      log += "Translation from LLVM IR to SPIR-V failed.\n";
      *err_buf = strdup(log.c_str());
      return -1;
   }

   std::ostringstream spv_stream;
   if (!::llvm::writeSpirv(mod.get(), spv_stream, log)) {
      log += "Translation from LLVM IR to SPIR-V failed.\n";
      *err_buf = strdup(log.c_str());
      return -1;
   }

   const std::string spv_out = spv_stream.str();
   spvbin->size = spv_out.size();
   spvbin->data = static_cast<uint32_t *>(malloc(spvbin->size));
   memcpy(spvbin->data, spv_out.data(), spvbin->size);

   return 0;
}

void
clc_free_spirv_binary(struct spirv_binary *spvbin)
{
   free(spvbin->data);
}
