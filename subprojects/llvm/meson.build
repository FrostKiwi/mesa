project('llvm', ['cpp'])

cpp = meson.get_compiler('cpp')

_deps_llvm = []
_deps_clang = []

_search = join_paths(meson.current_source_dir(), 'lib')

foreach d : ['LLVMAggressiveInstCombine', 'LLVMAnalysis', 'LLVMAsmParser',
             'LLVMAsmPrinter', 'LLVMBinaryFormat', 'LLVMBitReader',
	     'LLVMBitstreamReader', 'LLVMBitWriter', 'LLVMCFGuard',
	     'LLVMCodeGen', 'LLVMCore', 'LLVMCoroutines',
	     'LLVMCoverage', 'LLVMDebugInfoCodeView', 'LLVMDebugInfoDWARF',
	     'LLVMDebugInfoGSYM', 'LLVMDebugInfoMSF', 'LLVMDebugInfoPDB',
	     'LLVMDemangle', 'LLVMDlltoolDriver', 'LLVMExecutionEngine',
	     'LLVMFrontendOpenMP', 'LLVMFuzzMutate', 'LLVMGlobalISel',
	     'LLVMInstCombine', 'LLVMInstrumentation', 'LLVMInterpreter',
	     'LLVMipo', 'LLVMIRReader', 'LLVMLibDriver',
	     'LLVMLineEditor', 'LLVMLinker', 'LLVMLTO',
	     'LLVMMC', 'LLVMMCA', 'LLVMMCDisassembler', 
	     'LLVMMCJIT', 'LLVMMCParser', 'LLVMMIRParser',
	     'LLVMObjCARCOpts', 'LLVMObject', 'LLVMObjectYAML',
	     'LLVMOption', 'LLVMPasses', 'LLVMProfileData',
	     'LLVMRemarks', 'LLVMRuntimeDyld', 'LLVMScalarOpts',
	     'LLVMSelectionDAG', 'LLVMSPIRVLib', 'LLVMSupport',
	     'LLVMSymbolize', 'LLVMTableGen', 'LLVMTarget',
	     'LLVMTextAPI', 'LLVMTransformUtils', 'LLVMVectorize',
	     'LLVMWindowsManifest', 'LLVMX86AsmParser', 'LLVMX86CodeGen',
	     'LLVMX86Desc', 'LLVMX86Disassembler', 'LLVMX86Info',
	     'LLVMX86Utils']

  _deps_llvm += cpp.find_library(d, dirs : _search)
endforeach

foreach d : ['clangCodeGen', 'clangFrontend', 'clangFrontendTool',
             'clangDriver', 'clangSerialization', 'clangParse',
	     'clangSema', 'clangAnalysis', 'clangAST',
             'clangASTMatchers', 'clangEdit', 'clangLex',
	     'clangBasic']

  _deps_clang += cpp.find_library(d, dirs : _search)
endforeach

dep_llvm = declare_dependency(
    include_directories : include_directories('include'),
    dependencies : _deps_llvm,
    link_args : 'version.lib',
    version : '11.0.0',
)

dep_clang = declare_dependency(
    include_directories : include_directories('include'),
    dependencies : _deps_clang,
    version : '11.0.0',
)

has_rtti = false
irbuilder_h = files('include/llvm/IR/IRBuilder.h')
