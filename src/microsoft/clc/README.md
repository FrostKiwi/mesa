# LLVM to SPIRV

SPIRV-LLVM-Translator is used for the OpenCL toolchain.
This document describe the steps to compile this tool.

## Cloning LLVM

```
git clone --config core.autocrlf=false https://github.com/llvm/llvm-project.git
```

## Cloning SPIRV-LLVM-Translator

SPIRV-LLVM-Translator is following LLVM tot so we recommend building SPIRV-LLVM-Translator in the LLVM tree.
The translator can be built as a regular LLVM subproject. To do that you need to clone it into the llvm/projects or llvm/tools directory.

```
cd llvm-project/llvm/projects
git clone https://github.com/KhronosGroup/SPIRV-LLVM-Translator.git
```

## Building SPIRV-LLVM-Translator

```
mkdir llvm-project/build && cd llvm-project/build
cmake -G Ninja ../llvm -DCMAKE_BUILD_TYPE:STRING="Release" ^
                       -DLLVM_TARGETS_TO_BUILD:STRING="X86" ^
                       -DLLVM_OPTIMIZED_TABLEGEN:BOOL=TRUE ^
                       -DLLVM_ENABLE_ASSERTIONS:BOOL=TRUE ^
                       -DLLVM_SPIRV_INCLUDE_TESTS=OFF ^
                       -DLLVM_ENABLE_PROJECTS="clang"
ninja llvm-spirv -j`nproc`
```

In the previous example, the target platform is set to "X86". This value should be modify regarding you platform (ARM, AArch64, RISCV...).
For more detail about building LLVM with CMake, visit (https://llvm.org/docs/CMake.html)[https://llvm.org/docs/CMake.html].

## Installing SPIRV-LLVM-Translator

SPIRV-LLVM-Translator needs to be install in `<mesa repo>\subproject\llvm`
