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
   intrinsic store_deref (ssa_1, ssa_0) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
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
  %2 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 1, i32 undef)  ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  %3 = call float @dx.op.loadInput.f32(i32 4, i32 0, i32 0, i8 2, i32 undef)  ; LoadInput(inputSigId,rowIndex,colIndex,gsVertexAxis)
  %4 = bitcast i32 1065353216 to float
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %1)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %2)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %3)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %4)  ; StoreOutput(outputSigId,rowIndex,colIndex,value)
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
   vec3 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_2 = deref_var &gl_Position (shader_out vec4)
   vec1 32 ssa_3 = load_const (0x3f800000 /* 1.000000 */)
   vec4 32 ssa_4 = vec4 ssa_1.x, ssa_1.y, ssa_1.z, ssa_3
   intrinsic store_deref (ssa_2, ssa_4) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
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
%__ubo0 = type { [16384 x float] }

declare %dx.types.Handle @dx.op.createHandle(i32, i8, i32, i32, i1) #0
declare %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32, %dx.types.Handle, i32) #0
declare void @dx.op.storeOutput.f32(i32, i32, i32, i8, float) #1

define void @main() {
  %1 = call %dx.types.Handle @dx.op.createHandle(i32 57, i8 2, i32 0, i32 0, i1 false) ; CreateHandle(resourceClass,rangeId,index,nonUniformIndex)
  %2 = call %dx.types.CBufRet.f32 @dx.op.cbufferLoadLegacy.f32(i32 59, %dx.types.Handle %1, i32 0) ; CBufferLoadLegacy(handle,regIndex)
  %3 = extractvalue %dx.types.CBufRet.f32 %2, 0
  %4 = extractvalue %dx.types.CBufRet.f32 %2, 1
  %5 = extractvalue %dx.types.CBufRet.f32 %2, 2
  %6 = extractvalue %dx.types.CBufRet.f32 %2, 3
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %3) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %4) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %5) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %6) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
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
!6 = !{i32 0, %__ubo0* undef, !"__ubo0", i32 0, i32 0, i32 1, i32 65536, null}
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
   vec4 32 ssa_2 = intrinsic load_ubo (ssa_0, ssa_0) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
   vec1 32 ssa_3 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_3, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
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
%__ubo0 = type { [16384 x float] }

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
  %9 = extractvalue %dx.types.CBufRet.f32 %7, 1
  %10 = extractvalue %dx.types.CBufRet.f32 %7, 2
  %11 = extractvalue %dx.types.CBufRet.f32 %7, 3
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 0, float %8) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 1, float %9) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 2, float %10) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
  call void @dx.op.storeOutput.f32(i32 5, i32 0, i32 0, i8 3, float %11) ; StoreOutput(outputSigId,rowIndex,colIndex,value)
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
!6 = !{i32 0, %__ubo0* undef, !"__ubo0", i32 0, i32 0, i32 1, i32 65536, null}
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
   vec1 32 ssa_2 = intrinsic load_ubo (ssa_0, ssa_1) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
   vec1 32 ssa_3 = ishl ssa_2, ssa_6
   vec4 32 ssa_4 = intrinsic load_ubo (ssa_0, ssa_3) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
   vec1 32 ssa_5 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_5, ssa_4) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
}
)";
   run(shader, expect);
}

TEST_F(NirToDXILTest, test_shader_validates_fs_0)
{
 const char shader[] =
R"(shader: MESA_SHADER_FRAGMENT
 name: GLSL3
 inputs: 2
 outputs: 1
 uniforms: 0
 shared: 0
 decl_var shader_in INTERP_MODE_NONE vec4 gl_Color (VARYING_SLOT_COL0.xyzw, 0, 0)
 decl_var shader_in INTERP_MODE_NONE vec4 gl_in (VARYING_SLOT_BFC0.xyzw, 1, 0)
 decl_var shader_in INTERP_MODE_FLAT uint isFrontFace (VARYING_SLOT_FACE.x, 2, 0)
 decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
 decl_function main (0 params) (entrypoint)

 impl main {
         block block_0:
         /* preds: */
         vec1 32 ssa_0 = deref_var &gl_Color (shader_in vec4)
         vec4 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
         vec1 32 ssa_2 = deref_var &gl_in (shader_in vec4)
         vec4 32 ssa_3 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
         vec1 32 ssa_4 = intrinsic load_front_face () ()
         vec1 32 ssa_5 = load_const (0x00000000 /* 0.000000 */)
         vec1 1 ssa_6 = ine ssa_4, ssa_5
         vec1 32 ssa_7 = bcsel ssa_6, ssa_1.x, ssa_3.x
         vec1 32 ssa_8 = bcsel ssa_6, ssa_1.y, ssa_3.y
         vec1 32 ssa_9 = bcsel ssa_6, ssa_1.z, ssa_3.z
         vec1 32 ssa_10 = bcsel ssa_6, ssa_1.w, ssa_3.w
         vec4 32 ssa_11 = vec4 ssa_7, ssa_8, ssa_9, ssa_10
         vec1 32 ssa_12 = deref_var &gl_FragColor (shader_out vec4)
         intrinsic store_deref (ssa_12, ssa_11) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
         /* succs: block_1 */
         block block_1:
 })";
 run(shader, "");
}

TEST_F(NirToDXILTest, test_shader_bitcast_gl_fragdepth)
{
 const char shader[] =
R"(shader: MESA_SHADER_FRAGMENT
name: GLSL3
inputs: 2
outputs: 2
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec4 gl_Color (VARYING_SLOT_COL0.xyzw, 0, 0)
decl_var shader_in INTERP_MODE_NONE float packed:z (VARYING_SLOT_VAR0.x, 1, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE float gl_FragDepth (FRAG_RESULT_DEPTH.x, 1, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_0 = deref_var &packed:z (shader_in float)
        vec1 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_2 = deref_var &gl_Color (shader_in vec4)
        vec4 32 ssa_3 = intrinsic load_deref (ssa_2) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_4 = deref_var &gl_FragDepth (shader_out float)
        intrinsic store_deref (ssa_4, ssa_1) (1, 0, 0, 0) /* wrmask=x */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_5 = deref_var &gl_FragColor (shader_out vec4)
        intrinsic store_deref (ssa_5, ssa_3) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        /* succs: block_1 */
        block block_1:
})";
 run(shader, "");
}

TEST_F(NirToDXILTest, test_shader_clipdist_validation)
{
 const char shader[] =
R"(shader: MESA_SHADER_VERTEX
name: GLSL3
inputs: 1
outputs: 3
uniforms: 8
shared: 0
decl_var uniform INTERP_MODE_NONE vec4 gl_ClipPlane0MESA (0, 0, 0)
decl_var uniform INTERP_MODE_NONE vec4 gl_ClipPlane2MESA (0, 2, 0)
decl_var uniform INTERP_MODE_NONE vec4 gl_ClipPlane4MESA (0, 4, 0)
decl_var ubo INTERP_MODE_NONE vec4[8] uniform_0 (0, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec4 gl_Vertex (VERT_ATTRIB_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 clipdist_2 (VARYING_SLOT_CLIP_DIST0.xyzw, 1, 0)
decl_var shader_out INTERP_MODE_NONE vec4 clipdist_3 (VARYING_SLOT_CLIP_DIST1.xyzw, 2, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_0 = deref_var &gl_Vertex (shader_in vec4)
        vec4 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_33 = load_const (0x00000000 /* 0.000000 */)
        vec4 32 ssa_53 = intrinsic load_ubo (ssa_33, ssa_33) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec1 32 ssa_85 = fmul ssa_53.y, ssa_1.y
        vec1 32 ssa_129 = ffma ssa_53.x, ssa_1.x, ssa_85
        vec1 32 ssa_128 = ffma ssa_53.z, ssa_1.z, ssa_129
        vec1 32 ssa_127 = ffma ssa_53.w, ssa_1.w, ssa_128
        vec1 32 ssa_134 = load_const (0x00000020 /* 0.000000 */)
        vec4 32 ssa_59 = intrinsic load_ubo (ssa_33, ssa_134) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec1 32 ssa_92 = fmul ssa_59.y, ssa_1.y
        vec1 32 ssa_126 = ffma ssa_59.x, ssa_1.x, ssa_92
        vec1 32 ssa_125 = ffma ssa_59.z, ssa_1.z, ssa_126
        vec1 32 ssa_124 = ffma ssa_59.w, ssa_1.w, ssa_125
        vec1 32 ssa_135 = load_const (0x00000040 /* 0.000000 */)
        vec4 32 ssa_65 = intrinsic load_ubo (ssa_33, ssa_135) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec1 32 ssa_99 = fmul ssa_65.y, ssa_1.y
        vec1 32 ssa_123 = ffma ssa_65.x, ssa_1.x, ssa_99
        vec1 32 ssa_122 = ffma ssa_65.z, ssa_1.z, ssa_123
        vec1 32 ssa_121 = ffma ssa_65.w, ssa_1.w, ssa_122
        vec1 32 ssa_25 = deref_var &gl_Position (shader_out vec4)
        vec1 32 ssa_41 = fadd ssa_1.z, ssa_1.w
        vec1 32 ssa_42 = load_const (0x3f000000 /* 0.500000 */)
        vec1 32 ssa_120 = ffma ssa_41, ssa_42, ssa_1.w
        vec1 32 ssa_70 = fmul ssa_120, ssa_42
        vec4 32 ssa_74 = vec4 ssa_1.x, ssa_1.y, ssa_70, ssa_1.w
        intrinsic store_deref (ssa_25, ssa_74) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_27 = deref_var &clipdist_2 (shader_out vec4)
        vec4 32 ssa_114 = vec4 ssa_127, ssa_33, ssa_124, ssa_33
        intrinsic store_deref (ssa_27, ssa_114) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_29 = deref_var &clipdist_3 (shader_out vec4)
        vec4 32 ssa_119 = vec4 ssa_121, ssa_33, ssa_33, ssa_33
        intrinsic store_deref (ssa_29, ssa_119) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        /* succs: block_1 */
        block block_1:
})";
 run(shader, "");
}

TEST_F(NirToDXILTest, test_shader_txf_validation)
{
 const char shader[] =
R"(shader: MESA_SHADER_FRAGMENT
name: GLSL3
inputs: 0
outputs: 1
uniforms: 2
shared: 0
decl_var uniform INTERP_MODE_NONE sampler2D tex (3, 0, 0)
decl_var uniform INTERP_MODE_NONE sampler samp (3, 0, 0)
decl_var ubo INTERP_MODE_NONE vec4[2] uniform_0 (0, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_1 = load_const (0x00000000 /* 0.000000 */)
        vec2 32 ssa_12 = intrinsic load_ubo (ssa_1, ssa_1) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec1 32 ssa_23 = load_const (0x00000010 /* 0.000000 */)
        vec1 32 ssa_18 = intrinsic load_ubo (ssa_1, ssa_23) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec4 32 ssa_5 = txf ssa_12 (coord), ssa_18 (lod), 0 (texture), 0 (sampler)
        vec1 32 ssa_6 = deref_var &gl_FragColor (shader_out vec4)
        intrinsic store_deref (ssa_6, ssa_5) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        /* succs: block_1 */
        block block_1:
})";
   run(shader, "");
}

TEST_F(NirToDXILTest, test_shader_array_of_sampler_validation)
{
 const char shader[] =
R"(shader: MESA_SHADER_FRAGMENT
name: GLSL3
inputs: 1
outputs: 1
uniforms: 0
shared: 0
decl_var uniform INTERP_MODE_NONE sampler2D[2] tex (0, 0, 0)
decl_var uniform INTERP_MODE_NONE sampler[2] samp (0, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec4 texcoords (VARYING_SLOT_VAR9.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_0 = load_const (0x3f000000 /* 0.500000 */)
        vec1 32 ssa_3 = deref_var &texcoords (shader_in vec4)
        vec4 32 ssa_4 = intrinsic load_deref (ssa_3) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 1 ssa_5 = flt ssa_4.x, ssa_0
        /* succs: block_1 block_2 */
        if ssa_5 {
                block block_1:
                /* preds: block_0 */
                vec2 32 ssa_18 = vec2 ssa_4.x, ssa_4.y
                vec4 32 ssa_9 = tex ssa_18 (coord), 0 (texture), 0 (sampler)
                /* succs: block_3 */
        } else {
                block block_2:
                /* preds: block_0 */
                vec2 32 ssa_21 = vec2 ssa_4.x, ssa_4.y
                vec4 32 ssa_13 = tex ssa_21 (coord), 1 (texture), 1 (sampler)
                /* succs: block_3 */
        }
        block block_3:
        /* preds: block_1 block_2 */
        vec4 32 ssa_14 = phi block_1: ssa_9, block_2: ssa_13
        vec1 32 ssa_15 = deref_var &gl_FragColor (shader_out vec4)
        intrinsic store_deref (ssa_15, ssa_14) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        /* succs: block_4 */
        block block_4:
})";
 run(shader, "");
}

TEST_F(NirToDXILTest, test_shader_two_arrays_of_sampler_validation)
{
 const char shader[] =
R"(shader: MESA_SHADER_FRAGMENT
name: GLSL3
inputs: 1
outputs: 1
uniforms: 0
shared: 0
decl_var uniform INTERP_MODE_NONE sampler2D[2] tex (0, 0, 0)
decl_var uniform INTERP_MODE_NONE sampler2D[2] tex2 (1, 2, 2)
 decl_var uniform INTERP_MODE_NONE sampler[2] samp (0, 0, 0)
 decl_var uniform INTERP_MODE_NONE sampler[2] samp2 (1, 2, 2)
decl_var shader_in INTERP_MODE_NONE vec4 texcoords (VARYING_SLOT_VAR9.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_0 = load_const (0x3f000000 /* 0.500000 */)
        vec1 32 ssa_3 = deref_var &texcoords (shader_in vec4)
        vec4 32 ssa_4 = intrinsic load_deref (ssa_3) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 1 ssa_5 = flt ssa_4.x, ssa_0
        /* succs: block_1 block_5 */
        if ssa_5 {
                block block_1:
                /* preds: block_0 */
                vec1 1 ssa_6 = flt ssa_4.y, ssa_0
                /* succs: block_2 block_3 */
                if ssa_6 {
                        block block_2:
                        /* preds: block_1 */
                        vec2 32 ssa_30 = vec2 ssa_4.x, ssa_4.y
                        vec4 32 ssa_10 = tex ssa_30 (coord), 0 (texture), 0 (sampler)
                        /* succs: block_4 */
                } else {
                        block block_3:
                        /* preds: block_1 */
                        vec2 32 ssa_33 = vec2 ssa_4.x, ssa_4.y
                        vec4 32 ssa_14 = tex ssa_33 (coord), 2 (texture), 2 (sampler)
                        /* succs: block_4 */
                }
                block block_4:
                /* preds: block_2 block_3 */
                vec4 32 ssa_15 = phi block_2: ssa_10, block_3: ssa_14
                /* succs: block_9 */
        } else {
                block block_5:
                /* preds: block_0 */
                vec1 1 ssa_16 = flt ssa_4.y, ssa_0
                /* succs: block_6 block_7 */
                if ssa_16 {
                        block block_6:
                        /* preds: block_5 */
                        vec2 32 ssa_36 = vec2 ssa_4.x, ssa_4.y
                        vec4 32 ssa_20 = tex ssa_36 (coord), 1 (texture), 1 (sampler)
                        /* succs: block_8 */
                } else {
                        block block_7:
                        /* preds: block_5 */
                        vec2 32 ssa_39 = vec2 ssa_4.x, ssa_4.y
                        vec4 32 ssa_24 = tex ssa_39 (coord), 3 (texture), 3 (sampler)
                        /* succs: block_8 */
                }
                block block_8:
                /* preds: block_6 block_7 */
                vec4 32 ssa_25 = phi block_6: ssa_20, block_7: ssa_24
                /* succs: block_9 */
        }
        block block_9:
        /* preds: block_4 block_8 */
        vec4 32 ssa_26 = phi block_4: ssa_15, block_8: ssa_25
        vec1 32 ssa_27 = deref_var &gl_FragColor (shader_out vec4)
        intrinsic store_deref (ssa_27, ssa_26) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        /* succs: block_10 */
        block block_10:
})";
 run(shader, "");
}

TEST_F(NirToDXILTest, test_shader_stencil_ref_validation)
{
 const char shader[] =
R"(shader: MESA_SHADER_FRAGMENT
name: GLSL0
inputs: 0
outputs: 1
uniforms: 3
shared: 0
decl_var uniform INTERP_MODE_NONE vec4[3] gl_CurrentAttribFragMESA (0, 0, 0)
decl_var ubo INTERP_MODE_NONE vec4[3] uniform_0 (0, 0, 0)
decl_var ubo INTERP_MODE_NONE vec4[1] d3d12_state_vars (0, 0, 1)
decl_var shader_in INTERP_MODE_FLAT uint IsFrontFace (VARYING_SLOT_FACE.x, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE uint StencilRef (FRAG_RESULT_STENCIL.x, 1, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_0 = load_const (0x00000000 /* 0.000000 */)
        vec1 32 ssa_1 = load_const (0x00000020 /* 0.000000 */)
        vec4 32 ssa_2 = intrinsic load_ubo (ssa_0, ssa_1) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec1 32 ssa_3 = deref_var &gl_FragColor (shader_out vec4)
        intrinsic store_deref (ssa_3, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_11 = load_const (0x00000001 /* 0.000000 */)
        vec2 32 ssa_13 = intrinsic load_ubo (ssa_11, ssa_0) (0, 4, 0) /* access=0 */ /* align_mul=4 */ /* align_offset=0 */
        vec1 32 ssa_6 = intrinsic load_front_face () ()
        vec1 1 ssa_15 = ine ssa_6, ssa_0
        vec1 32 ssa_9 = bcsel ssa_15, ssa_13.x, ssa_13.y
        vec1 32 ssa_10 = deref_var &StencilRef (shader_out uint)
        intrinsic store_deref (ssa_10, ssa_9) (1, 0, 0, 0) /* wrmask=x */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        /* succs: block_1 */
        block block_1:
})";
 run(shader, "");
}




TEST_F(NirToDXILTest, test_shader_GS_triangle_to_point)
{
   struct SetupShaderGS : public SetupShader {
      void operator () (UNUSED nir_shader *shader) const override {
         shader->info.gs.vertices_in = 3;
         shader->info.gs.vertices_out = 1;
         shader->info.gs.input_primitive = GL_TRIANGLES;
         shader->info.gs.output_primitive = GL_TRIANGLE_STRIP;
         shader->info.gs.invocations = 1;
         shader->info.gs.active_stream_mask = 1;
      };
   };


   const char shader[] =
R"(shader: MESA_SHADER_GEOMETRY
name: GLSL1
inputs: 1
outputs: 1
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_FLAT int[3] vertex_id (VARYING_SLOT_VAR9.x, 0, 0)
decl_var shader_out INTERP_MODE_FLAT ivec3 vertex_out (VARYING_SLOT_VAR9.xyz, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
        block block_0:
        /* preds: */
        vec1 32 ssa_0 = deref_var &vertex_id (shader_in int[3])
        vec1 32 ssa_1 = load_const (0x00000000 /* 0.000000 */)
        vec1 32 ssa_2 = deref_array &(*ssa_0)[0] (shader_in int) /* &packed:vertex_id[0] */
        vec1 32 ssa_3 = intrinsic load_deref (ssa_2) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_4 = load_const (0x00000001 /* 0.000000 */)
        vec1 32 ssa_5 = deref_array &(*ssa_0)[1] (shader_in int) /* &packed:vertex_id[1] */
        vec1 32 ssa_6 = intrinsic load_deref (ssa_5) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_7 = load_const (0x00000002 /* 0.000000 */)
        vec1 32 ssa_8 = deref_array &(*ssa_0)[2] (shader_in int) /* &packed:vertex_id[2] */
        vec1 32 ssa_9 = intrinsic load_deref (ssa_8) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        vec1 32 ssa_10 = iadd ssa_3, ssa_4
        vec1 32 ssa_11 = iadd ssa_6, ssa_4
        vec1 32 ssa_12 = iadd ssa_9, ssa_4
        vec3 32 ssa_13 = vec3 ssa_10, ssa_11, ssa_12
        vec1 32 ssa_14 = deref_var &vertex_out (shader_out ivec3)
        intrinsic store_deref (ssa_14, ssa_13) (7, 0, 0, 0) /* wrmask=xyz */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
        intrinsic emit_vertex (0) /* stream-id=0 */
        /* succs: block_1 */
        block block_1:
})";
   run(shader, "", SetupShaderGS());
}
