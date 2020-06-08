/*
 * Copyright © 2019 Collabora LTD
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
#include "nir.h"
#include "nir_builder.h"

#include <gtest/gtest.h>
#include <unistd.h>
#include <fcntl.h>
#include <cstdio>
#include <cstring>

using std::string;
using std::stringstream;

class nir_shader_from_string_test : public ::testing::Test {
protected:
   void run(const std::string& shader) const;

   nir_shader_compiler_options options;
   gl_shader_stage processor;

   static const char *shader_stages[MESA_SHADER_STAGES];

   static const char *empty_main;

private:
   /* remove all double spaces tabs in favour of spaces and
    * also remove empty lines */
   string remove_comments(const string& s) const;
   string trim_spaces(const string& s) const;
};

TEST_F(nir_shader_from_string_test, test_default_shader_simple)
{
   const char sh_head[] = "shader: MESA_SHADER_";

   const char shader_mid[] =
R"(
inputs: 0
outputs: 0
uniforms: 0
shared: 0
decl_function main (0 params) (entrypoint)
)";

   for (int i = 0; i < MESA_SHADER_COMPUTE; ++i) {
      stringstream s;
      s << sh_head;
      s << shader_stages[i] << shader_mid << empty_main;
      run(s.str());
   }
}

TEST_F(nir_shader_from_string_test, test_default_shader_cs)
{
static const char shader[] =
R"(shader: MESA_SHADER_COMPUTE
local-size: 0, 0, 0
shared-size: 0
inputs: 0
outputs: 0
uniforms: 0
shared: 0
decl_function main (0 params) (entrypoint)
)";

   stringstream s;
   s << shader << empty_main;

   run(s.str());
}

TEST_F(nir_shader_from_string_test, test_default_shader_input_generic_0_xyz)
{
static const char shader[] =
R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 0
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec3 vertexPosition_modelspace (VERT_ATTRIB_GENERIC0.xyz, 0, 0)
decl_function main (0 params) (entrypoint)
)";

   stringstream s;
   s << shader << empty_main;

   run(s.str());
}

TEST_F(nir_shader_from_string_test, test_default_shader_output_pos)
{
static const char shader[] =
R"(shader: MESA_SHADER_VERTEX
inputs: 0
outputs: 1
uniforms: 0
shared: 0
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)
)";

   stringstream s;
   s << shader << empty_main;

   run(s.str());
}


TEST_F(nir_shader_from_string_test, test_default_shader_uniform_mat4)
{
static const char shader[] =
R"(shader: MESA_SHADER_VERTEX
inputs: 0
outputs: 0
uniforms: 4
shared: 0
decl_var uniform INTERP_MODE_NONE mat4 MVP (0, 0, 0)
decl_function main (0 params) (entrypoint)
)";

   stringstream s;
   s << shader << empty_main;

   run(s.str());
}

TEST_F(nir_shader_from_string_test, test_default_shader_u14_i1_o3)
{
static const char shader[] =
R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 3
uniforms: 14
shared: 0
decl_var uniform INTERP_MODE_NONE mat4 gl_ModelViewProjectionMatrix (0, 0, 0)
decl_var uniform INTERP_MODE_NONE vec4[10] uniform_0 (0, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec4 gl_Vertex (VERT_ATTRIB_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_out_TexCoord0 (VARYING_SLOT_VAR0.xyzw, 1, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_out_TexCoord2 (VARYING_SLOT_VAR2.xyzw, 2, 0)
decl_function main (0 params) (entrypoint)
)";

   stringstream s;
   s << shader << empty_main;

   run(s.str());
}

TEST_F(nir_shader_from_string_test, test_default_shader_types)
{
static const char shader[] =
R"(shader: MESA_SHADER_VERTEX
inputs: 4
outputs: 3
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec4 gl_Vertex (VERT_ATTRIB_POS.xyzw, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec3 gl_Normal (VERT_ATTRIB_GENERIC0.xyz, 1, 0)
decl_var shader_in INTERP_MODE_NONE vec2 color (VERT_ATTRIB_GENERIC1.xy, 2, 0)
decl_var shader_in INTERP_MODE_NONE float scale (VERT_ATTRIB_GENERIC2.x, 3, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_out_TexCoord0 (VARYING_SLOT_VAR0.xyzw, 1, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_out_TexCoord2 (VARYING_SLOT_VAR2.xyzw, 2, 0)
decl_function main (0 params) (entrypoint)
)";

   stringstream s;
   s << shader << empty_main;

   run(s.str());
}

TEST_F(nir_shader_from_string_test, test_red_triangle_vs)
{
run(R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 1
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec3 vertexPosition_modelspace (VERT_ATTRIB_GENERIC0.xyz, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE float const_temp
   decl_var  INTERP_MODE_NONE vec3 in@vertexPosition_modelspace-temp
   decl_var  INTERP_MODE_NONE vec4 out@gl_Position-temp
   decl_var  INTERP_MODE_NONE vec4 gl_Position@0
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &vertexPosition_modelspace (shader_in vec3)
   vec3 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_2 = load_const (0x3f800000 /* 1.000000 */)
   vec4 32 ssa_3 = vec4 ssa_1.x, ssa_1.y, ssa_1.z, ssa_2
   vec1 32 ssa_4 = deref_var &gl_Position (shader_out vec4)
   intrinsic store_deref (ssa_4, ssa_3) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
})");
}

TEST_F(nir_shader_from_string_test, test_red_triangle_fs)
{
run(
R"(shader: MESA_SHADER_FRAGMENT
inputs: 0
outputs: 1
uniforms: 0
shared: 0
decl_var shader_out INTERP_MODE_NONE vec3 color (FRAG_RESULT_DATA0.xyz, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE vec3 const_temp
   decl_var  INTERP_MODE_NONE vec3 out@color-temp
   decl_var  INTERP_MODE_NONE vec3 color@0
   block block_0:
   /* preds: */
   vec3 32 ssa_0 = load_const (0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */)
   vec1 32 ssa_1 = deref_var &color (shader_out vec3)
   intrinsic store_deref (ssa_1, ssa_0) (7, 0, 0, 0) /* wrmask=xyz */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
})");
}


TEST_F(nir_shader_from_string_test, test_red_triangle_unforms)
{
run(R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 1
uniforms: 4
shared: 0
decl_var uniform INTERP_MODE_NONE mat4 MVP (0, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec3 vertexPosition_modelspace (VERT_ATTRIB_GENERIC0.xyz, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE int const_temp
   decl_var  INTERP_MODE_NONE int const_temp@0
   decl_var  INTERP_MODE_NONE int const_temp@1
   decl_var  INTERP_MODE_NONE int const_temp@2
   decl_var  INTERP_MODE_NONE vec3 in@vertexPosition_modelspace-temp
   decl_var  INTERP_MODE_NONE vec4 gl_Position@3
   decl_var  INTERP_MODE_NONE vec4 out@gl_Position-temp
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &vertexPosition_modelspace (shader_in vec3)
   vec3 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_2 = load_const (0x00000000 /* 0.000000 */)
   vec4 32 ssa_3 = intrinsic load_uniform (ssa_2) (0, 4, 2) /* base=0 */ /* range=4 */	/* type=int */ /* MVP */
   vec4 32 ssa_4 = fmul ssa_3, ssa_1.xxxx
   vec1 32 ssa_5 = load_const (0x00000001 /* 0.000000 */)
   vec4 32 ssa_6 = intrinsic load_uniform (ssa_5) (0, 4, 3) /* base=0 */ /* range=4 */	/* type=int1 */ /* MVP */
   vec4 32 ssa_7 = fmul ssa_6, ssa_1.yyyy
   vec4 32 ssa_8 = fadd ssa_4, ssa_7
   vec1 32 ssa_9 = load_const (0x00000002 /* 0.000000 */)
   vec4 32 ssa_10 = intrinsic load_uniform (ssa_9) (0, 4, 4) /* base=0 */ /* range=4 */ /* type=uint */ 	/* MVP */
   vec4 32 ssa_11 = fmul ssa_10, ssa_1.zzzz
   vec4 32 ssa_12 = fadd ssa_8, abs(ssa_11)
   vec1 32 ssa_13 = load_const (0x00000003 /* 0.000000 */)
   vec4 32 ssa_14 = intrinsic load_uniform (ssa_13) (0, 4, 5) /* base=0 */ /* range=4 */	/* type=uint1 */ 	/* MVP */
   vec4 32 ssa_15 = fadd ssa_12, ssa_14
   vec1 32 ssa_16 = deref_var &gl_Position (shader_out vec4)
   intrinsic store_deref (ssa_16, ssa_15) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
})");
}

TEST_F(nir_shader_from_string_test, test_cube_textured)
{
run(R"(shader: MESA_SHADER_FRAGMENT
inputs: 1
outputs: 1
uniforms: 1
shared: 0
decl_var uniform INTERP_MODE_NONE sampler2D myTextureSampler (1, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec2 UV (VARYING_SLOT_VAR9.xy, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec3 color (FRAG_RESULT_DATA0.xyz, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE vec3 color@0
   decl_var  INTERP_MODE_NONE vec3 out@color-temp
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &UV (shader_in vec2)
   vec2 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec4 32 ssa_2 = tex ssa_1 (coord), 0 (texture), 0 (sampler)
   vec1 32 ssa_3 = deref_var &color (shader_out vec3)
   vec3 32 ssa_4 = mov ssa_2.xyz
   intrinsic store_deref (ssa_3, ssa_4) (7, 0, 0, 0) /* wrmask=xyz */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
})");
}

TEST_F(nir_shader_from_string_test, test_shadow_tex)
{
run(R"(shader: MESA_SHADER_FRAGMENT
inputs: 1
outputs: 1
uniforms: 1
shared: 0
decl_var uniform INTERP_MODE_NONE sampler1DShadow tex (0, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec4 texcoords (VARYING_SLOT_VAR9.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE vec4 out@gl_FragColor-temp
   decl_var  INTERP_MODE_NONE vec4 compiler_temp
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &texcoords (shader_in vec4)
   vec4 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_2 = mov ssa_1.x
   vec1 32 ssa_3 = mov ssa_1.y
   vec4 32 ssa_4 = tex ssa_2 (coord), ssa_3 (comparator), 0 (texture), 0 (sampler)
   vec1 32 ssa_5 = deref_var &gl_FragColor (shader_out vec4)
   intrinsic store_deref (ssa_5, ssa_4) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
})");
}

TEST_F(nir_shader_from_string_test, test_simple_if)
{
run(R"(shader: MESA_SHADER_FRAGMENT
inputs: 2
outputs: 1
uniforms: 2
shared: 0
decl_var uniform INTERP_MODE_NONE sampler2D tex0 (0, 0, 0)
decl_var uniform INTERP_MODE_NONE sampler2D tex1 (1, 1, 1)
decl_var uniform INTERP_MODE_NONE vec4 gl_FbWposYTransform (0, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec4 gl_FragCoord (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_in INTERP_MODE_NONE vec4 gl_in_TexCoord0 (VARYING_SLOT_VAR0.xyzw, 1, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE bool compiler_temp
   decl_var  INTERP_MODE_NONE float const_temp
   decl_var  INTERP_MODE_NONE float const_temp@0
   decl_var  INTERP_MODE_NONE vec4 compiler_temp@1
   decl_var  INTERP_MODE_NONE vec4 const_temp@2
   decl_var  INTERP_MODE_NONE vec4 out@gl_FragColor-temp
   decl_var  INTERP_MODE_NONE vec4 compiler_temp@3
   decl_reg vec4 32 r0
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = load_const (0x3f000000 /* 0.500000 */)
   vec4 32 ssa_1 = load_const (0x3f800000 /* 1.000000 */, 0x3f800000 /* 1.000000 */, 0x3f800000 /* 1.000000 */, 0x3f800000 /* 1.000000 */)
   vec1 32 ssa_2 = deref_var &gl_FragCoord (shader_in vec4)
   vec4 32 ssa_3 = intrinsic load_deref (ssa_2) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_4 = fmul ssa_3.x, ssa_0
   vec1 32 ssa_5 = ffract ssa_4
   vec1 1 ssa_6 = flt ssa_5, ssa_0
   /* succs: block_1 block_2 */
   if ssa_6 {
      block block_1:
      /* preds: block_0 */
      vec1 32 ssa_7 = deref_var &gl_in_TexCoord0 (shader_in vec4)
      vec4 32 ssa_8 = intrinsic load_deref (ssa_7) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
      vec2 32 ssa_9 = mov ssa_8.xy
      r0 = tex ssa_9 (coord), 0 (texture), 0 (sampler)
      /* succs: block_3 */
   } else {
      block block_2:
      /* preds: block_0 */
      vec1 32 ssa_10 = deref_var &gl_in_TexCoord0 (shader_in vec4)
      vec4 32 ssa_11 = intrinsic load_deref (ssa_10) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
      vec2 32 ssa_12 = mov ssa_11.xy
      r0 = tex ssa_12 (coord), 1 (texture), 1 (sampler)
      /* succs: block_3 */
   }
   block block_3:
   /* preds: block_1 block_2 */
   vec4 32 ssa_13 = fadd ssa_1, -r0
   vec4 32 ssa_14 = bcsel ssa_6.xxxx, ssa_13, r0
   vec1 32 ssa_15 = deref_var &gl_FragColor (shader_out vec4)
   intrinsic store_deref (ssa_15, ssa_14) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_4 */
   block block_4:
})");
}

TEST_F(nir_shader_from_string_test, test_simple_loop)
{
run(R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 2
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec4 piglit_vertex (VERT_ATTRIB_GENERIC0.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 color (VARYING_SLOT_VAR9.xyzw, 1, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE int const_temp
   decl_var  INTERP_MODE_NONE int const_temp@0
   decl_var  INTERP_MODE_NONE int const_temp@1
   decl_var  INTERP_MODE_NONE int const_temp@2
   decl_var  INTERP_MODE_NONE int const_temp@3
   decl_var  INTERP_MODE_NONE int const_temp@4
   decl_var  INTERP_MODE_NONE int const_temp@5
   decl_var  INTERP_MODE_NONE int const_temp@6
   decl_var  INTERP_MODE_NONE vec4 const_temp@7
   decl_var  INTERP_MODE_NONE vec4 const_temp@8
   decl_var  INTERP_MODE_NONE int num_calls_to_foo
   decl_var  INTERP_MODE_NONE vec4 color@9
   decl_var  INTERP_MODE_NONE vec4 out@color-temp
   decl_var  INTERP_MODE_NONE vec4 out@gl_Position-temp
   decl_var  INTERP_MODE_NONE vec4 gl_Position@10
   decl_var  INTERP_MODE_NONE int i
   decl_var  INTERP_MODE_NONE vec4 in@piglit_vertex-temp
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &piglit_vertex (shader_in vec4)
   vec4 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_2 = load_const (0x00000000 /* 0.000000 */)
   vec1 32 ssa_3 = load_const (0x00000003 /* 0.000000 */)
   vec4 32 ssa_4 = load_const (0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
   vec4 32 ssa_5 = load_const (0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
   /* succs: block_1 */
   loop {
      block block_1:
      /* preds: block_0 block_4 */
      vec1 32 ssa_6 = phi block_0: ssa_2, block_4: ssa_3
      vec1 32 ssa_7 = phi block_0: ssa_2, block_4: ssa_9
      vec1 1 ssa_8 = ige ssa_6, ssa_3
      /* succs: block_2 block_3 */
      if ssa_8 {
         block block_2:
         /* preds: block_1 */
         break
         /* succs: block_5 */
      } else {
         block block_3:
         /* preds: block_1 */
         /* succs: block_4 */
      }
      block block_4:
      /* preds: block_3 */
      vec1 32 ssa_9 = iadd ssa_3, ssa_7
      /* succs: block_1 */
   }
   block block_5:
   /* preds: block_2 */
   vec1 1 ssa_10 = ieq ssa_7, ssa_3
   vec4 32 ssa_11 = bcsel ssa_10.xxxx, ssa_4, ssa_5
   vec1 32 ssa_12 = deref_var &gl_Position (shader_out vec4)
   intrinsic store_deref (ssa_12, ssa_1) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_13 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_13, ssa_11) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_6 */
   block block_6:
})");
}

TEST_F(nir_shader_from_string_test, test_loop_with_registers)
{
run(R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 2
uniforms: 0
shared: 0
decl_var shader_in INTERP_MODE_NONE vec4 piglit_vertex (VERT_ATTRIB_GENERIC0.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 color (VARYING_SLOT_VAR9.xyzw, 1, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_reg vec1 32 r0
   decl_reg vec1 32 r1
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &piglit_vertex (shader_in vec4)
   vec4 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_2 = load_const (0x00000000 /* 0.000000 */)
   vec1 32 ssa_3 = load_const (0x00000003 /* 0.000000 */)
   vec4 32 ssa_4 = load_const (0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
   vec4 32 ssa_5 = load_const (0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
   r1 = mov ssa_2
   r0 = mov r1
   /* succs: block_1 */
   loop {
      block block_1:
      /* preds: block_0 block_4 */
      vec1 1 ssa_6 = ige r1, ssa_3
      /* succs: block_2 block_3 */
      if ssa_6 {
         block block_2:
         /* preds: block_1 */
         break
         /* succs: block_5 */
      } else {
         block block_3:
         /* preds: block_1 */
         /* succs: block_4 */
      }
      block block_4:
      /* preds: block_3 */
      r1 = mov ssa_3
      /* succs: block_1 */
    r0 = iadd ssa_3, r0
   }
   block block_5:
   /* preds: block_2 */
   vec1 1 ssa_7 = ieq r0, ssa_3
   vec4 32 ssa_8 = bcsel ssa_7.xxxx, ssa_4, ssa_5
   vec1 32 ssa_9 = deref_var &gl_Position (shader_out vec4)
   intrinsic store_deref (ssa_9, ssa_1) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_10 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_10, ssa_8) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_6 */
   block block_6:
})");
}

TEST_F(nir_shader_from_string_test, test_temp_array)
{
run(R"(shader: MESA_SHADER_VERTEX
inputs: 1
outputs: 2
uniforms: 2
shared: 0
decl_var uniform INTERP_MODE_NONE int writeIndex (0, 0, 0)
decl_var uniform INTERP_MODE_NONE int readIndex (1, 1, 0)
decl_var shader_in INTERP_MODE_NONE vec4 gl_Vertex (VERT_ATTRIB_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_Position (VARYING_SLOT_POS.xyzw, 0, 0)
decl_var shader_out INTERP_MODE_NONE vec4 color (VARYING_SLOT_VAR9.xyzw, 1, 0)
decl_function main (0 params) (entrypoint)

impl main {
   decl_var  INTERP_MODE_NONE vec4[5] a2
   decl_var  INTERP_MODE_NONE vec4[5] a1
   block block_0:
   /* preds: */
   vec1 32 ssa_0 = deref_var &gl_Vertex (shader_in vec4)
   vec4 32 ssa_1 = intrinsic load_deref (ssa_0) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec4 32 ssa_2 = load_const (0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */)
   vec1 32 ssa_3 = load_const (0x00000000 /* 0.000000 */)
   vec1 32 ssa_4 = load_const (0x00000001 /* 0.000000 */)
   vec1 32 ssa_5 = load_const (0x00000002 /* 0.000000 */)
   vec1 32 ssa_6 = load_const (0x00000003 /* 0.000000 */)
   vec1 32 ssa_7 = load_const (0x00000004 /* 0.000000 */)
   vec4 32 ssa_8 = load_const (0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x3e800000 /* 0.250000 */, 0x00000000 /* 0.000000 */)
   vec4 32 ssa_9 = load_const (0x3f800000 /* 1.000000 */, 0x3e800000 /* 0.250000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
   vec4 32 ssa_10 = load_const (0x3f800000 /* 1.000000 */, 0x3e800000 /* 0.250000 */, 0x3f400000 /* 0.750000 */, 0x3f800000 /* 1.000000 */)
   vec1 32 ssa_11 = deref_var &a1 (function_temp vec4[5])
   vec1 32 ssa_12 = deref_array &(*ssa_11)[0] (function_temp vec4) /* &a1[0] */
   intrinsic store_deref (ssa_12, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_13 = deref_array &(*ssa_11)[1] (function_temp vec4) /* &a1[1] */
   intrinsic store_deref (ssa_13, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_14 = deref_array &(*ssa_11)[2] (function_temp vec4) /* &a1[2] */
   intrinsic store_deref (ssa_14, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_15 = deref_array &(*ssa_11)[3] (function_temp vec4) /* &a1[3] */
   intrinsic store_deref (ssa_15, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_16 = deref_array &(*ssa_11)[4] (function_temp vec4) /* &a1[4] */
   intrinsic store_deref (ssa_16, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_17 = deref_var &a2 (function_temp vec4[5])
   vec1 32 ssa_18 = deref_array &(*ssa_17)[0] (function_temp vec4) /* &a2[0] */
   intrinsic store_deref (ssa_18, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_19 = deref_array &(*ssa_17)[1] (function_temp vec4) /* &a2[1] */
   intrinsic store_deref (ssa_19, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_20 = deref_array &(*ssa_17)[2] (function_temp vec4) /* &a2[2] */
   intrinsic store_deref (ssa_20, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_21 = deref_array &(*ssa_17)[3] (function_temp vec4) /* &a2[3] */
   intrinsic store_deref (ssa_21, ssa_2) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_22 = deref_array &(*ssa_17)[4] (function_temp vec4) /* &a2[4] */
   intrinsic store_deref (ssa_22, ssa_8) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_23 = intrinsic load_uniform (ssa_3) (0, 1, 2) /* base=0 */ /* range=1 */	/* writeIndex */
   vec1 32 ssa_24 = deref_array &(*ssa_11)[ssa_23] (function_temp vec4) /* &a1[ssa_23] */
   intrinsic store_deref (ssa_24, ssa_9) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_25 = deref_array &(*ssa_17)[ssa_23] (function_temp vec4) /* &a2[ssa_23] */
   intrinsic store_deref (ssa_25, ssa_10) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_26 = intrinsic load_uniform (ssa_3) (1, 1, 2) /* base=1 */ /* range=1 */	/* readIndex */
   vec1 32 ssa_27 = deref_array &(*ssa_11)[ssa_26] (function_temp vec4) /* &a1[ssa_26] */
   vec4 32 ssa_28 = intrinsic load_deref (ssa_27) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_29 = deref_array &(*ssa_17)[ssa_26] (function_temp vec4) /* &a2[ssa_26] */
   vec4 32 ssa_30 = intrinsic load_deref (ssa_29) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec4 32 ssa_31 = fadd ssa_28, ssa_30
   vec4 32 ssa_32 = intrinsic load_deref (ssa_22) (0, 0, 0) /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec4 32 ssa_33 = fadd ssa_31, ssa_32
   vec1 32 ssa_34 = deref_var &gl_Position (shader_out vec4)
   intrinsic store_deref (ssa_34, ssa_1) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   vec1 32 ssa_35 = deref_var &color (shader_out vec4)
   intrinsic store_deref (ssa_35, ssa_33) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
   /* succs: block_1 */
   block block_1:
})");
}

TEST_F(nir_shader_from_string_test, test_temp_array_locals_lowered_to_regs)
{
run(R"(shader: MESA_SHADER_FRAGMENT
inputs: 0
outputs: 1
uniforms: 3
shared: 0
decl_var uniform INTERP_MODE_NONE int index (1, 0, 0)
decl_var uniform INTERP_MODE_NONE int col (2, 1, 0)
decl_var uniform INTERP_MODE_NONE vec2 expect (3, 2, 0)
decl_var shader_out INTERP_MODE_NONE vec4 gl_FragColor (FRAG_RESULT_COLOR.xyzw, 0, 0)
decl_function main (0 params) (entrypoint)

impl main {
    decl_var  INTERP_MODE_NONE mat2[3] m
    decl_reg vec2 32 r0[6]
    block block_0:
    /* preds: */
    vec2 32 ssa_0 = load_const (0x3f800000 /* 1.000000 */, 0x40000000 /* 2.000000 */)
    vec2 32 ssa_1 = load_const (0x40400000 /* 3.000000 */, 0x40800000 /* 4.000000 */)
    vec1 32 ssa_2 = load_const (0x00000000 /* 0.000000 */)
    vec2 32 ssa_3 = load_const (0x40a00000 /* 5.000000 */, 0x40c00000 /* 6.000000 */)
    vec2 32 ssa_4 = load_const (0x40e00000 /* 7.000000 */, 0x41000000 /* 8.000000 */)
    vec2 32 ssa_5 = load_const (0x41100000 /* 9.000000 */, 0x41200000 /* 10.000000 */)
    vec2 32 ssa_6 = load_const (0x41300000 /* 11.000000 */, 0x41400000 /* 12.000000 */)
    vec4 32 ssa_7 = load_const (0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
    vec4 32 ssa_8 = load_const (0x3f800000 /* 1.000000 */, 0x00000000 /* 0.000000 */, 0x00000000 /* 0.000000 */, 0x3f800000 /* 1.000000 */)
    r0[0] = mov ssa_0
    r0[1] = mov ssa_1
    r0[2] = mov ssa_3
    r0[3] = mov ssa_4
    r0[4] = mov ssa_5
    r0[5 + ssa_2] = mov ssa_6
    vec1 32 ssa_9 = intrinsic load_uniform (ssa_2) (1, 1, 2) /* base=1 */ /* range=1 */ /* type=int */	/* col */
    vec1 32 ssa_10 = intrinsic load_uniform (ssa_2) (0, 1, 2) /* base=0 */ /* range=1 */ /* type=int */	/* index */
    vec1 32 ssa_11 = load_const (0x00000000 /* 0.000000 */)
    vec1 32 ssa_12 = load_const (0x00000001 /* 0.000000 */)
    vec1 32 ssa_13 = imul ssa_9, ssa_12
    vec1 32 ssa_14 = iadd ssa_11, ssa_13
    vec1 32 ssa_15 = load_const (0x00000002 /* 0.000000 */)
    vec1 32 ssa_16 = imul ssa_10, ssa_15
    vec1 32 ssa_17 = iadd ssa_14, ssa_16
    vec2 32 ssa_18 = mov r0[0 + ssa_17]
    vec2 32 ssa_19 = intrinsic load_uniform (ssa_2) (2, 1, 128) /* base=2 */ /* range=1 */ /* type=float */ /* expect */
    vec1 1 ssa_20 = ball_fequal2 ssa_18, ssa_1
    vec4 32 ssa_21 = bcsel ssa_20.xxxx, ssa_7, ssa_8
    vec1 32 ssa_22 = deref_var &gl_FragColor (shader_out vec4)
    intrinsic store_deref (ssa_22, ssa_21) (15, 0, 0, 0) /* wrmask=xyzw */ /* access=0 */ /* align_mul=0 */ /* align_offset=0 */
    /* succs: block_1 */
    block block_1:
})");
}

/* Redirect fprintf to a stream to capture the output of nir_print_shader */
class CaptureFile {

public:
   CaptureFile() = default;
   ~CaptureFile();
   bool init();

   FILE *get_file_descriptor();
   std::string text();

private:
   int m_pipe[2];
   FILE *m_f;
   std::stringstream m_buf;
};

bool CaptureFile::init()
{
#ifdef __MINGW32__
   if (_pipe(m_pipe, 256, O_TEXT) != 0)
      return false;
#else
   if (pipe2(m_pipe, O_NONBLOCK) != 0)
      return false;
#endif

   m_f = fdopen(m_pipe[1], "w");
   if (!m_f)
      return false;
   return true;
}

CaptureFile::~CaptureFile()
{
   fclose(m_f);
   close(m_pipe[0]);
   close(m_pipe[1]);
}

FILE *CaptureFile::get_file_descriptor()
{
   return m_f;
}
std::string CaptureFile::text()
{
   std::vector<char> buf;
   char b[1024];
   ssize_t n;
   while ((n = read(m_pipe[0], b, 1024)) > 0) {
      size_t k = buf.size();
      buf.resize(k + n);
      copy(b, b + n, buf.begin() + k);
   }
   return std::string(buf.begin(),buf.end());
}

const char *nir_shader_from_string_test::shader_stages[MESA_SHADER_STAGES] = {
  "VERTEX", "TESS_CTRL", "TESS_EVAL", "GEOMETRY", "FRAGMENT", "COMPUTE"
};

const char *nir_shader_from_string_test::empty_main =
R"(impl main {
   block block_0:
   /* preds: */
   /* succs: block_1 */
   block block_1:
}
)";


void nir_shader_from_string_test::run(const std::string& shader) const
{
   glsl_type_singleton_init_or_ref();

   nir_shader *sh = nir_shader_from_string(shader.c_str(), &options);
   ASSERT_TRUE(sh != nullptr);

   nir_validate_shader(sh, "nir_shader_from_string");

   CaptureFile cf;
   ASSERT_TRUE(cf.init());
   nir_print_shader(sh,  cf.get_file_descriptor());

   auto result = trim_spaces(remove_comments(cf.text()));
   auto expect = trim_spaces(remove_comments(shader));

   ralloc_free(sh);

   EXPECT_EQ(result, expect);
   glsl_type_singleton_decref();
}

string nir_shader_from_string_test::remove_comments(const string &s) const
{
   stringstream os;

   string::size_type cursor = 0;
   auto start_comment = s.find("/*", cursor);

   while (start_comment != string::npos) {
      os << s.substr(cursor, start_comment - cursor);
      cursor = s.find("*/", start_comment);
      if (cursor == string::npos)
         break;
      cursor += 2;
      start_comment = s.find("/*", cursor);
   }

   if (cursor != string::npos)
      os << s.substr(cursor, start_comment - cursor);

   return os.str();
}

string nir_shader_from_string_test::trim_spaces(const string& s) const
{
   stringstream os;

   bool start_ws = false;
   bool have_nl = false;
   for (auto c : s) {
      if (!start_ws)
         start_ws = strchr(" \t\r\n", c);
      if (!have_nl)
         have_nl = c == '\n';

      if (!strchr(" \t\r\n", c))  {
         if (start_ws) {
            start_ws = false;
            os << (have_nl ? '\n' : ' ');
         }
         have_nl = false;
            os << c;
      }

   }
   return os.str();
}
