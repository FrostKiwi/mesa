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

TEST_F(NirToDXILTest, test_fs_constant_color)
{
   const char expect[] = R"(target datalayout = "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64"
target triple = "dxil-ms-dx"

declare void @dx.op.storeOutput.f32(i32, i32, i32, i8, float) #0

define void @main() {
  %1 = bitcast i32 1065353216 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %1)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %2 = bitcast i32 0 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %2)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %3 = bitcast i32 0 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %3)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %4 = bitcast i32 1065353216 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %4)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  ret void
}

attributes #0 = { nounwind }

!llvm.ident = !{!0}
!dx.version = !{!1}
!dx.valver = !{!2}
!dx.shaderModel = !{!3}
!dx.typeAnnotations = !{!4}
!dx.entryPoints = !{!8}

!1 = !{i32 1, i32 1}
!2 = !{i32 1, i32 4}
!3 = !{!"ps", i32 6, i32 1}
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
  %2 = bitcast float %1 to i32
  %3 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 1, i32 undef)  ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  %4 = bitcast float %3 to i32
  %5 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 2, i32 undef)  ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  %6 = bitcast float %5 to i32
  %7 = bitcast i32 %2 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %7)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %8 = bitcast i32 %4 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %8)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %9 = bitcast i32 %6 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %9)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %10 = bitcast i32 1065353216 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %10)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
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

!1 = !{i32 1, i32 1}
!2 = !{i32 1, i32 4}
!3 = !{!"vs", i32 6, i32 1}
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


TEST_F(NirToDXILTest, test_fs_uniform_color)
{
   const char expect[] = R"(target datalayout = "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64"
target triple = "dxil-ms-dx"

%dx.types.Handle = type { i8* }
%dx.types.CBufRet.f32 = type { float, float, float, float }
%uniform_0 = type { [4 x float] }

declare %dx.types.Handle @dx.op.createHandle(i32, i8, i32, i32, i1) #0
declare %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32, %dx.types.Handle, i32) #0
declare void @dx.op.storeOutput.f32(i32, i32, i32, i8, float) #1

define void @main() {
  %1 = call %dx.types.Handle @dx.op.createHandle(i32 57, i8 2, i32 0, i32 0, i1 false) ; CreateHandle(resourceClass,rangeId,index,nonUniformIndex)
  %2 = call %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32 59, %dx.types.Handle %1, i32 0) ; CBufferLoadLegacy(handle,regIndex)
  %3 = extractvalue %dx.types.CBufRet.f32 %2, 0
  %4 = bitcast float %3 to i32
  %5 = extractvalue %dx.types.CBufRet.f32 %2, 1
  %6 = bitcast float %5 to i32
  %7 = extractvalue %dx.types.CBufRet.f32 %2, 2
  %8 = bitcast float %7 to i32
  %9 = extractvalue %dx.types.CBufRet.f32 %2, 3
  %10 = bitcast float %9 to i32
  %11 = bitcast i32 %4 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %11) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %12 = bitcast i32 %6 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %12) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %13 = bitcast i32 %8 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %13) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %14 = bitcast i32 %10 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %14) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  ret void
}

attributes #0 = { nounwind readonly }
attributes #1 = { nounwind }

!llvm.ident = !{!0}
!dx.version = !{!1}
!dx.valver = !{!2}
!dx.shaderModel = !{!3}
!dx.resources = !{!4}
!dx.typeAnnotations = !{!7}
!dx.entryPoints = !{!11}

!1 = !{i32 1, i32 1}
!2 = !{i32 1, i32 4}
!3 = !{!"ps", i32 6, i32 1}
!4 = !{null, null, !5, null}
!5 = !{!6}
!6 = !{i32 0, %uniform_0* undef, !"uniform_0", i32 0, i32 0, i32 1, i32 16, null}
!7 = !{i32 1, void ()* @main, !8}
!8 = !{!9}
!9 = !{i32 0, !10, !10}
!10 = !{}
!11 = !{void ()* @main, !"main", !12, !4, null}
!12 = !{null, !13, null}
!13 = !{!14}
!14 = !{i32 0, !"SV_Target", i8 9, i8 16, !15, i8 0, i32 1, i8 4, i32 0, i8 0, null}
!15 = !{i32 0}
)";

   const char shader[] = R"(
shader: MESA_SHADER_FRAGMENT
inputs: 0
outputs: 1
uniforms: 1
shared: 0
decl_var uniform INTERP_MODE_NONE vec4 color_in (0, 0, 0)
decl_var ubo INTERP_MODE_NONE vec4[1] uniform_0 (0, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 color (FRAG_RESULT_DATA0.xyzw, 0, 0)
decl_function main (0 params)

impl main {
   decl_var  INTERP_MODE_NONE vec4 const_temp
   decl_var  INTERP_MODE_NONE vec4 out@color-temp
   decl_var  INTERP_MODE_NONE vec4 color@0
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = load_const (0x00000000 /* 0.000000 */)
   vec1 32 ssa_1 = deref_var &color_in (uniform vec4)
   vec4 32 ssa_2 = intrinsic load_ubo (ssa_0, ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_3 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_3, ssa_2) (15, 0) /* wrmask=xyzw */ /* access=0 */
   /* succs: block_1 */
   block block_1:
}
)";
   run(shader, expect);
}

TEST_F(NirToDXILTest, test_fs_uniform_nonconst_offset)
{
   const char expect[] = R"(target datalayout = "e-m:e-p:32:32-i1:32-i8:32-i16:32-i32:32-i64:64-f16:32-f32:32-f64:64-n8:16:32:64"
target triple = "dxil-ms-dx"

%dx.types.Handle = type { i8* }
%dx.types.CBufRet.f32 = type { float, float, float, float }
%uniform_0 = type { [8 x float] }

declare %dx.types.Handle @dx.op.createHandle(i32, i8, i32, i32, i1) #0
declare %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32, %dx.types.Handle, i32) #0
declare void @dx.op.storeOutput.f32(i32, i32, i32, i8, float) #1

define void @main() {
  %1 = call %dx.types.Handle @dx.op.createHandle(i32 57, i8 2, i32 0, i32 0, i1 false) ; CreateHandle(resourceClass,rangeId,index,nonUniformIndex)
  %2 = call %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32 59, %dx.types.Handle %1, i32 0) ; CBufferLoadLegacy(handle,regIndex)
  %3 = extractvalue %dx.types.CBufRet.f32 %2, 0
  %4 = bitcast float %3 to i32
  %5 = shl i32 %4, 4
  %6 = ashr i32 %5, 4
  %7 = call %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32 59, %dx.types.Handle %1, i32 %6) ; CBufferLoadLegacy(handle,regIndex)
  %8 = extractvalue %dx.types.CBufRet.f32 %7, 0
  %9 = bitcast float %8 to i32
  %10 = extractvalue %dx.types.CBufRet.f32 %7, 1
  %11 = bitcast float %10 to i32
  %12 = extractvalue %dx.types.CBufRet.f32 %7, 2
  %13 = bitcast float %12 to i32
  %14 = extractvalue %dx.types.CBufRet.f32 %7, 3
  %15 = bitcast float %14 to i32
  %16 = bitcast i32 %9 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %16) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %17 = bitcast i32 %11 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %17) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %18 = bitcast i32 %13 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %18) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  %19 = bitcast i32 %15 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %19) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  ret void
}

attributes #0 = { nounwind readonly }
attributes #1 = { nounwind }

!llvm.ident = !{!0}
!dx.version = !{!1}
!dx.valver = !{!2}
!dx.shaderModel = !{!3}
!dx.resources = !{!4}
!dx.typeAnnotations = !{!7}
!dx.entryPoints = !{!11}

!1 = !{i32 1, i32 1}
!2 = !{i32 1, i32 4}
!3 = !{!"ps", i32 6, i32 1}
!4 = !{null, null, !5, null}
!5 = !{!6}
!6 = !{i32 0, %uniform_0* undef, !"uniform_0", i32 0, i32 0, i32 1, i32 32, null}
!7 = !{i32 1, void ()* @main, !8}
!8 = !{!9}
!9 = !{i32 0, !10, !10}
!10 = !{}
!11 = !{void ()* @main, !"main", !12, !4, null}
!12 = !{null, !13, null}
!13 = !{!14}
!14 = !{i32 0, !"SV_Target", i8 9, i8 16, !15, i8 0, i32 1, i8 4, i32 0, i8 0, null}
!15 = !{i32 0}
)";

   const char shader[] = R"(
shader: MESA_SHADER_FRAGMENT
inputs: 0
outputs: 1
uniforms: 2
shared: 0
decl_var ubo INTERP_MODE_NONE vec4[2] uniform_0 (0, 0, 0)
decl_var uniform INTERP_MODE_NONE int index (1, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 color (FRAG_RESULT_DATA0.xyzw, 0, 0)
decl_function main (0 params)

impl main {
   decl_var  INTERP_MODE_NONE vec4 const_temp
   decl_var  INTERP_MODE_NONE vec4 out@color-temp
   decl_var  INTERP_MODE_NONE vec4 color@0
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = load_const (0x00000000 /* 0.000000 */)
   vec1 32 ssa_1 = load_const (0x00000001 /* 0.000000 */)
   vec1 32 ssa_6 = load_const (0x00000004 /* 0.000000 */)
   vec1 32 ssa_2 = intrinsic load_ubo (ssa_0, ssa_1) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_3 = ishl ssa_2, ssa_6
   vec4 32 ssa_4 = intrinsic load_ubo (ssa_0, ssa_3) (0, 4, 160) /* base=0 */ /* range=4 */ /* type=float32 */ /* color_in */
   vec1 32 ssa_5 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_5, ssa_4) (15, 0) /* wrmask=xyzw */ /* access=0 */
   /* succs: block_1 */
   block block_1:
}
)";
   run(shader, expect);
}
