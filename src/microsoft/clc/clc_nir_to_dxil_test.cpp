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

#include "nir.h"

#include "clc_test_dxlayer.h"

using std::stringstream;

static const char nir_shader_start[] =
      R"(shader: MESA_SHADER_)";

static const char nir_shader_end[] =
      R"(
inputs: 0
outputs: 0
uniforms: 0
shared: 0
impl main {
}
)";

static const char expect_start[] =
   R"(target datalayout = "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64"
target triple = "dxil-ms-dx"

define void @main() {
  ret void
}

!llvm.ident = !{!0}
!dx.version = !{!1}
!dx.valver = !{!2}
!dx.shaderModel = !{!3}
!dx.typeAnnotations = !{!4}
!dx.entryPoints = !{!8}

!1 = !{i32 1, i32 1}
!2 = !{i32 1, i32 4}
!3 = !{!")";

static const char expect_ps_end[] =
R"(", i32 6, i32 1}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", !9, null, null}
!9 = !{null, !7, null}
)";

static const char expect_end[] =
R"(", i32 6, i32 1}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", null, null, null}
)";

const  char expect_cs_end[] =
R"(", i32 6, i32 1}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", null, null, !9}
!9 = !{i32 4, !10}
!10 = !{i32 1, i32 1, i32 1}
)";

TEST_F(NirToDXILTest, test_empty_VS_shader)
{
   stringstream shader;
   shader << nir_shader_start << "VERTEX" << nir_shader_end;

   stringstream expect;
   expect << expect_start << "vs" << expect_end;

   run(shader.str(), expect.str());
}

TEST_F(NirToDXILTest, test_empty_PS_shader)
{
   stringstream shader;
   shader << nir_shader_start << "FRAGMENT" << nir_shader_end;

   stringstream expect;
   expect << expect_start << "ps" << expect_end;

   run(shader.str(), expect.str());
}

TEST_F(NirToDXILTest, test_empty_CS_shader)
{
   stringstream shader;
   shader << nir_shader_start << "COMPUTE" << nir_shader_end;

   stringstream expect;
   expect << expect_start << "cs" << expect_cs_end;

   run(shader.str(), expect.str());
}



