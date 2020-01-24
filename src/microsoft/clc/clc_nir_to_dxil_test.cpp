/*
 * Copyright 2020 Collabora Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
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

!1 = !{i32 1, i32 0}
!2 = !{i32 1, i32 4}
!3 = !{!")";

static const char expect_ps_end[] =
R"(", i32 6, i32 0}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", !9, null, null}
!9 = !{null, !7, null}
)";

static const char expect_end[] =
R"(", i32 6, i32 0}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", null, null, null}
)";

const  char expect_cs_end[] =
R"(", i32 6, i32 0}
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

TEST_F(NirToDXILTest, test_fs_constant_color)
{
   const char expect[] = R"(target datalayout = "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64"
target triple = "dxil-ms-dx"

declare void @dx.op.storeOutput.f32(i32, i32, i32, i8, float) #0

define void @main() {
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float 1.000000e+00) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float 0.000000e+00) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float 0.000000e+00) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float 1.000000e+00) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  ret void
}

attributes #0 = { nounwind }

!llvm.ident = !{!0}
!dx.version = !{!1}
!dx.valver = !{!2}
!dx.shaderModel = !{!3}
!dx.typeAnnotations = !{!4}
!dx.entryPoints = !{!8}

!1 = !{i32 1, i32 0}
!2 = !{i32 1, i32 4}
!3 = !{!"ps", i32 6, i32 0}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", !9, null, null}
!9 = !{null, !10, null}
!10 = !{!11}
!11 = !{i32 0, !"SV_Target", i8 9, i8 16, !12, i8 0, i32 1, i8 4, i32 0, i8 0, null}
!12 = !{i32 0}
)";

   const char shader[] = R"(
shader: MESA_SHADER_FRAGMENT
inputs: 0
outputs: 1
uniforms: 0
shared: 0
decl_var shader_out INTERP_MODE_NONE vec4 color (FRAG_RESULT_DATA0.xyzw, 0, 0)
decl_function main (0 params)

impl main {
   decl_var  INTERP_MODE_NONE vec4 const_temp
   decl_var  INTERP_MODE_NONE vec4 out@color-temp
   decl_var  INTERP_MODE_NONE vec4 color@0
   block block_0:
   /* preds: */
   vec4 32 ssa_0 = load_const (0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
   vec1 32 ssa_1 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_1, ssa_0) (15, 0) /* wrmask=xyzw */ /* access=0 */
   /* succs: block_1 */
   block block_1:
}
)";
   run(shader, expect);
}


TEST_F(NirToDXILTest, test_vs_passthrough_pos)
{
   const char expect[] =
R"(target datalayout = "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64"
target triple = "dxil-ms-dx"

declare float @dx.op.loadInput.f32(i32, i32, i32, i8, i32) #0

declare void @dx.op.storeOutput.f32(i32, i32, i32, i8, float) #1

define void @main() {
  %1 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 0, i32 undef)  ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  %2 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 1, i32 undef) ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  %3 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 2, i32 undef) ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %1) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %2) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %3) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float 1.000000e+00) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  ret void
}

attributes #0 = { nounwind readnone }
attributes #1 = { nounwind }

!llvm.ident = !{!0}
!dx.version = !{!1}
!dx.valver = !{!2}
!dx.shaderModel = !{!3}
!dx.typeAnnotations = !{!4}
!dx.entryPoints = !{!8}

!1 = !{i32 1, i32 0}
!2 = !{i32 1, i32 4}
!3 = !{!"vs", i32 6, i32 0}
!4 = !{i32 1, void ()* @main, !5}
!5 = !{!6}
!6 = !{i32 0, !7, !7}
!7 = !{}
!8 = !{void ()* @main, !"main", !9, null, null}
!9 = !{!10, !13, null}
!10 = !{!11}
!11 = !{i32 0, !"GENERICAA", i8 9, i8 0, !12, i8 0, i32 1, i8 3, i32 0, i8 0, null}
!12 = !{i32 0}
!13 = !{!14}
!14 = !{i32 0, !"SV_Position", i8 9, i8 3, !12, i8 2, i32 1, i8 4, i32 0, i8 0, null}
)";

const char nir_shader[] =
R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 1
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec3 vertexPosition_modelspace (VERT_ATTRIB_POS.xyz, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_function main (0 params)

impl main {
   decl_var  INTERP_MODE_NONE float const_temp
   decl_var  INTERP_MODE_NONE vec4 in@vertexPosition_modelspace-temp
   decl_var  INTERP_MODE_NONE vec4 out@gl_Position-temp
   decl_var  INTERP_MODE_NONE vec4 gl_Position@0
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &vertexPosition_modelspace (shader_in vec3)
   vec3 32 ssa_1 = intrinsic load_deref (ssa_0) (0) /* access=0 */
   vec1 32 ssa_2 = deref_var &gl_Position (shader_out vec4)
   vec1 32 ssa_3 = load_const (0x3f800000 /* 1.000000 */)
   vec4 32 ssa_4 = vec4 ssa_1.x, ssa_1.y, ssa_1.z, ssa_3
   intrinsic store_deref (ssa_2, ssa_4) (15, 0) /* wrmask=xyzw */ /* access=0 */
   /* succs: block_1 */
      block block_1:
 })";

   run(nir_shader, expect);
}
