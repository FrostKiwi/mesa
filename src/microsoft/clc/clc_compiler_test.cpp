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

#include <stdio.h>
#include <stdint.h>
#include <stdexcept>
#include <vector>

#include <d3d12.h>
#include <dxgi1_4.h>
#include <gtest/gtest.h>
#include <wrl.h>

#include "compute_test.h"

using std::vector;

TEST_F(ComputeTest, two_global_arrays)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *g1, __global uint *g2)\n\
   {\n\
       uint idx = get_global_id(0);\n\
       g1[idx] -= g2[idx];\n\
   }\n";
   auto g1 = ShaderArg<uint32_t>({ 10, 20, 30, 40 }, SHADER_ARG_INOUT);
   auto g2 = ShaderArg<uint32_t>({ 1, 2, 3, 4 }, SHADER_ARG_INPUT);
   const uint32_t expected[] = {
      9, 18, 27, 36
   };

   run_shader(kernel_source, g1.size(), 1, 1, g1, g2);
   for (int i = 0; i < g1.size(); ++i)
      EXPECT_EQ(g1[i], expected[i]);
}

TEST_F(ComputeTest, globals_8bit)
{
   const char *kernel_source =
   "__kernel void main_test(__global unsigned char *inout)\n\
   {\n\
       uint idx = get_global_id(0);\n\
       inout[idx] = inout[idx] + 1;\n\
   }\n";
   auto inout = ShaderArg<uint8_t> ({ 100, 110, 120, 130 }, SHADER_ARG_INOUT);
   const uint8_t expected[] = {
      101, 111, 121, 131
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, globals_16bit)
{
   const char *kernel_source =
   "__kernel void main_test(__global unsigned short *inout)\n\
   {\n\
       uint idx = get_global_id(0);\n\
       inout[idx] = inout[idx] + 1;\n\
   }\n";
   auto inout = ShaderArg<uint16_t> ({ 10000, 10010, 10020, 10030 }, SHADER_ARG_INOUT);
   const uint16_t expected[] = {
      10001, 10011, 10021, 10031
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, DISABLED_globals_64bit)
{
   /* Test disabled, because we need a fixed version of WARP that hasn't
      been officially shipped yet */

   const char *kernel_source =
   "__kernel void main_test(__global unsigned long *inout)\n\
   {\n\
       uint idx = get_global_id(0);\n\
       inout[idx] = inout[idx] + 1;\n\
   }\n";
   uint64_t base = 1ull << 50;
   auto inout = ShaderArg<uint64_t>({ base, base + 10, base + 20, base + 30 },
                                    SHADER_ARG_INOUT);
   const uint64_t expected[] = {
      base + 1, base + 11, base + 21, base + 31
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, built_ins_global_id)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = get_global_id(0);\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      0, 1, 2, 3
   };

   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, built_ins_global_id_rmw)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       uint id = get_global_id(0);\n\
       output[id] = output[id] * (id + 1);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({0x00000001, 0x10000001, 0x00020002, 0x04010203},
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0x00000001, 0x20000002, 0x00060006, 0x1004080c
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, types_float_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((float)get_global_id(0) + 1.5f);\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, types_double_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((double)get_global_id(0) + 1.5);\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, types_short_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((short)get_global_id(0) + (short)1);\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, types_char_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((char)get_global_id(0) + (char)1);\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, types_if_statement)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       int idx = get_global_id(0);\n\
       if (idx > 0)\n\
           output[idx] = ~idx;\n\
       else\n\
           output[0] = 0xff;\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      0xff, ~1u, ~2u, ~3u
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, types_do_while_loop)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       int value = 1;\n\
       int i = 1, n = get_global_id(0);\n\
       do {\n\
          value *= i++;\n\
       } while (i <= n);\n\
       output[n] = value;\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(5, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      1, 1, 1*2, 1*2*3, 1*2*3*4
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, types_for_loop)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       int value = 1;\n\
       int n = get_global_id(0);\n\
       for (int i = 1; i <= n; ++i)\n\
          value *= i;\n\
       output[n] = value;\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(5, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      1, 1, 1*2, 1*2*3, 1*2*3*4
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, complex_types_local_array)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
      uint tmp[] = {\n\
         get_global_id(1) + 0x00,\n\
         get_global_id(1) + 0x10,\n\
         get_global_id(1) + 0x20,\n\
         get_global_id(1) + 0x30,\n\
      };\n\
      uint idx = get_global_id(0);\n\
      inout[idx] = tmp[idx];\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 0, 0, 0 }, SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0x00, 0x10, 0x20, 0x30,
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, complex_types_global_struct_array)
{
   struct two_vals { uint32_t add; uint32_t mul; };
   const char *kernel_source =
   "struct two_vals { uint add; uint mul; };\n\
   __kernel void main_test(__global struct two_vals *in_out)\n\
   {\n\
      uint id = get_global_id(0);\n\
      in_out[id].add = in_out[id].add + id;\n\
      in_out[id].mul = in_out[id].mul * id;\n\
   }\n";
   auto inout = ShaderArg<struct two_vals>({ { 8, 8 }, { 16, 16 }, { 64, 64 }, { 65536, 65536 } },
                                           SHADER_ARG_INOUT);
   const struct two_vals expected[] = {
      { 8 + 0, 8 * 0 },
      { 16 + 1, 16 * 1 },
      { 64 + 2, 64 * 2 },
      { 65536 + 3, 65536 * 3 }
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i) {
      EXPECT_EQ(inout[i].add, expected[i].add);
      EXPECT_EQ(inout[i].mul, expected[i].mul);
   }
}

TEST_F(ComputeTest, complex_types_const_array)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       uint foo[4] = {100, 101, 102, 103};\n\
       output[get_global_id(0)] = foo[get_global_id(0)];\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      100, 101, 102, 103
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, two_const_arrays)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
      uint id = get_global_id(0);\n\
      uint foo[4] = {100, 101, 102, 103};\n\
      uint bar[4] = {1, 2, 3, 4};\n\
      output[id] = foo[id] * bar[id];\n\
   }\n";
   auto output = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                     SHADER_ARG_OUTPUT);
   const uint32_t expected[] = {
      100, 202, 306, 412
   };
   run_shader(kernel_source, output.size(), 1, 1, output);
   for (int i = 0; i < output.size(); ++i)
      EXPECT_EQ(output[i], expected[i]);
}

TEST_F(ComputeTest, imod_pos)
{
   const char *kernel_source =
   "__kernel void main_test(__global int *inout)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] % 3;\n\
   }\n";
   auto inout = ShaderArg<int32_t>({ -4, -3, -2, -1, 0, 1, 2, 3, 4 },
                                   SHADER_ARG_INOUT);
   const int32_t expected[] = {
      -1, 0, -2, -1,  0, 1, 2, 0, 1
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, imod_neg)
{
   const char *kernel_source =
   "__kernel void main_test(__global int *inout)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] % -3;\n\
   }\n";
   auto inout = ShaderArg<int32_t>({ -4, -3, -2, -1, 0, 1, 2, 3, 4 },
                                   SHADER_ARG_INOUT);
   const int32_t expected[] = {
      -1, 0, -2, -1,  0, 1, 2, 0, 1
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, umod)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] % 0xfffffffc;\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0xfffffffa, 0xfffffffb, 0xfffffffc, 0xfffffffd, 0xfffffffe },
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0xfffffffa, 0xfffffffb, 0, 1, 2
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, rotate)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = rotate(inout[get_global_id(0)], get_global_id(0) * 4);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>(std::vector<uint32_t>(4, 0xdeadbeef),
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0xdeadbeef, 0xeadbeefd, 0xadbeefde, 0xdbeefdea
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, popcount)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = popcount(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 0x1, 0x3, 0x101, 0x110011, ~0u },
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0, 1, 2, 2, 4, 32
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, hadd)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = hadd(inout[get_global_id(0)], 1u << 31);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 1, 2, 3, 0xfffffffc, 0xfffffffd, 0xfffffffe, 0xffffffff },
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      (1u << 31) >> 1,
      ((1u << 31) + 1) >> 1,
      ((1u << 31) + 2) >> 1,
      ((1u << 31) + 3) >> 1,
      ((1ull << 31) + 0xfffffffc) >> 1,
      ((1ull << 31) + 0xfffffffd) >> 1,
      ((1ull << 31) + 0xfffffffe) >> 1,
      ((1ull << 31) + 0xffffffff) >> 1,
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, rhadd)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = rhadd(inout[get_global_id(0)], 1u << 31);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 1, 2, 3, 0xfffffffc, 0xfffffffd, 0xfffffffe, 0xffffffff },
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      ((1u << 31) + 1) >> 1,
      ((1u << 31) + 2) >> 1,
      ((1u << 31) + 3) >> 1,
      ((1u << 31) + 4) >> 1,
      ((1ull << 31) + 0xfffffffd) >> 1,
      ((1ull << 31) + 0xfffffffe) >> 1,
      ((1ull << 31) + 0xffffffff) >> 1,
      ((1ull << 31) + (1ull << 32)) >> 1,
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, add_sat)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = add_sat(inout[get_global_id(0)], 2u);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0xffffffff - 3, 0xffffffff - 2, 0xffffffff - 1, 0xffffffff },
                                    SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0xffffffff - 1, 0xffffffff, 0xffffffff, 0xffffffff
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, sub_sat)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = sub_sat(inout[get_global_id(0)], 2u);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 1, 2, 3 }, SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0, 0, 0, 1
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, mul_hi)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = mul_hi(inout[get_global_id(0)], 1u << 31);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 1, 2, 3, (1u << 31) }, SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      0, 0, 1, 1, (1u << 30)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, ldexp_x)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = ldexp(inout[get_global_id(0)], 5);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 0.5f, 1.0f, 2.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      ldexp(0.0f, 5), ldexp(0.5f, 5), ldexp(1.0f, 5), ldexp(2.0f, 5)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, ldexp_y)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = ldexp(inout[get_global_id(0)], get_global_id(0));\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.25f, 0.5f, 0.75f, 1.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      ldexp(0.25f, 0), ldexp(0.5f, 1), ldexp(0.75f, 2), ldexp(1.0f, 3)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, frexp_ret)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       int exp;\n\
       inout[get_global_id(0)] = frexp(inout[get_global_id(0)], &exp);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 0.5f, 1.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      0.0f, 0.5f, 0.5f, 0.75f
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, frexp_exp)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       int exp;\n\
       frexp(inout[get_global_id(0)], &exp);\n\
       inout[get_global_id(0)] = (float)exp;\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 0.5f, 1.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      0.0f, 0.0f, 1.0f, 2.0f
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, clz)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = clz(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<uint32_t>({ 0, 1, 0xffff,  (1u << 30), (1u << 31) }, SHADER_ARG_INOUT);
   const uint32_t expected[] = {
      32, 31, 16, 1, 0
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, exp)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = native_exp(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 1.0f, 2.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      exp(0.0f), exp(1.0f), exp(2.0f), exp(3.0f)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, exp10)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = native_exp10(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 1.0f, 2.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      pow(10.0f, 0.0f), pow(10.0f, 1.0f), pow(10.0f, 2.0f), pow(10.0f, 3.0f)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, exp2)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = native_exp2(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 1.0f, 2.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      pow(2.0f, 0.0f), pow(2.0f, 1.0f), pow(2.0f, 2.0f), pow(2.0f, 3.0f)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, log)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = native_log(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 1.0f, 2.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      log(0.0f), log(1.0f), log(2.0f), log(3.0f)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, log10)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = native_log10(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 1.0f, 2.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      log10(0.0f), log10(1.0f), log10(2.0f), log10(3.0f)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, log2)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = native_log2(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0.0f, 1.0f, 2.0f, 3.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      log(0.0f) / log(2), log(1.0f) / log(2), log(2.0f) / log(2), log(3.0f) / log(2)
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, rint)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
      inout[get_global_id(0)] = rint(inout[get_global_id(0)]);\n\
   }\n";

   auto inout = ShaderArg<float>({ 0.5f, 1.5f, -0.5f, -1.5f, 1.4f }, SHADER_ARG_INOUT);
   const float expected[] = {
      0.0f, 2.0f, 0.0f, -2.0f, 1.0f,
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, round)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = round(inout[get_global_id(0)]);\n\
   }\n";
   auto inout = ShaderArg<float>({ 0, 0.3f, -0.3f, 0.5f, -0.5f, 1.1f, -1.1f },
                                 SHADER_ARG_INOUT);
   const float expected[] = {
      0.0f, 0.0f, -0.0f, 1.0f, -1.0f, 1.0f, -1.0f
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, arg_by_val)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout, float mul)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] * mul;\n\
   }\n";
   auto inout = ShaderArg<float>({ 0, 0.3f, -0.3f, 0.5f, -0.5f, 1.1f, -1.1f },
                                 SHADER_ARG_INOUT);
   auto mul = ShaderArg<float>(10.0f, SHADER_ARG_INPUT);
   const float expected[] = {
      0.0f, 3.0f, -3.0f, 5.0f, -5.0f, 11.0f, -11.0f
   };
   run_shader(kernel_source, inout.size(), 1, 1, inout, mul);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_FLOAT_EQ(inout[i], expected[i]);
}

TEST_F(ComputeTest, link)
{
   const char *foo_src =
   "float foo(float in)\n\
   {\n\
       return in * in;\n\
   }\n";
   const char *kernel_source =
   "float foo(float in);\n\
   __kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = foo(inout[get_global_id(0)]);\n\
   }\n";
   std::vector<const char *> srcs = { foo_src, kernel_source };
   auto inout = ShaderArg<float>({ 2.0f }, SHADER_ARG_INOUT);
   const float expected[] = {
      4.0f,
   };
   run_shader(srcs, inout.size(), 1, 1, inout);
   for (int i = 0; i < inout.size(); ++i)
      EXPECT_EQ(inout[i], expected[i]);
}
