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

#include <d3d12.h>
#include <dxgi1_4.h>
#include <gtest/gtest.h>
#include <wrl.h>

#include "compute_test.h"

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

TEST_F(ComputeTest, built_ins_global_id)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = get_global_id(0);\n\
   }\n";
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      0, 1, 2, 3
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, built_ins_global_id_rmw)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       uint id = get_global_id(0);\n\
       output[id] = output[id] * (id + 1);\n\
   }\n";
   const uint32_t input[] = {
      0x00000001, 0x10000001, 0x00020002, 0x04010203
   };
   const uint32_t expected[] = {
      0x00000001, 0x20000002, 0x00060006, 0x1004080c
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, types_float_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((float)get_global_id(0) + 1.5f);\n\
   }\n";
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, types_double_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((double)get_global_id(0) + 1.5);\n\
   }\n";
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, types_short_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((short)get_global_id(0) + (short)1);\n\
   }\n";
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, types_char_basics)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *output)\n\
   {\n\
       output[get_global_id(0)] = (uint)((char)get_global_id(0) + (char)1);\n\
   }\n";
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      1, 2, 3, 4
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
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
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      0xff, ~1u, ~2u, ~3u
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
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
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      1, 1, 1*2, 1*2*3, 1*2*3*4
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
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
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      1, 1, 1*2, 1*2*3, 1*2*3*4
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
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
   const uint32_t input[] = {
      0, 0, 0, 0,
   };
   const uint32_t expected[] = {
      0x00, 0x10, 0x20, 0x30,
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
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
   const struct two_vals input[] = {
      { 8, 8 }, { 16, 16 }, { 64, 64 }, { 65536, 65536 }
   };
   const struct two_vals expected[] = {
      { 8 + 0, 8 * 0 },
      { 16 + 1, 16 * 1 },
      { 64 + 2, 64 * 2 },
      { 65536 + 3, 65536 * 3 }
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i) {
      EXPECT_EQ(buf[i].add, expected[i].add);
      EXPECT_EQ(buf[i].mul, expected[i].mul);
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
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef
   };
   const uint32_t expected[] = {
      100, 101, 102, 103
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, imod_pos)
{
   const char *kernel_source =
   "__kernel void main_test(__global int *inout)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] % 3;\n\
   }\n";
   const int32_t input[] = {
      -4, -3, -2, -1, 0, 1, 2, 3, 4
   };
   const int32_t expected[] = {
      -1, 0, -2, -1,  0, 1, 2, 0, 1
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, imod_neg)
{
   const char *kernel_source =
   "__kernel void main_test(__global int *inout)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] % -3;\n\
   }\n";
   const int32_t input[] = {
      -4, -3, -2, -1, 0, 1, 2, 3, 4
   };
   const int32_t expected[] = {
      -1, 0, -2, -1,  0, 1, 2, 0, 1
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, umod)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = inout[get_global_id(0)] % 0xfffffffc;\n\
   }\n";
   const uint32_t input[] = {
      0xfffffffa, 0xfffffffb, 0xfffffffc, 0xfffffffd, 0xfffffffe
   };
   const uint32_t expected[] = {
      0xfffffffa, 0xfffffffb, 0, 1, 2
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, rotate)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = rotate(inout[get_global_id(0)], get_global_id(0) * 4);\n\
   }\n";
   const uint32_t input[] = {
      0xdeadbeef, 0xdeadbeef, 0xdeadbeef, 0xdeadbeef,
   };
   const uint32_t expected[] = {
      0xdeadbeef, 0xeadbeefd, 0xadbeefde, 0xdbeefdea
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, popcount)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = popcount(inout[get_global_id(0)]);\n\
   }\n";
   const uint32_t input[] = {
      0, 0x1, 0x3, 0x101, 0x110011, ~0u
   };
   const uint32_t expected[] = {
      0, 1, 2, 2, 4, 32
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, hadd)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = hadd(inout[get_global_id(0)], 1u << 31);\n\
   }\n";
   const uint32_t input[] = {
      0, 1, 2, 3, 0xfffffffc, 0xfffffffd, 0xfffffffe, 0xffffffff
   };
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
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, rhadd)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = rhadd(inout[get_global_id(0)], 1u << 31);\n\
   }\n";
   const uint32_t input[] = {
      0, 1, 2, 3, 0xfffffffc, 0xfffffffd, 0xfffffffe, 0xffffffff
   };
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
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, add_sat)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = add_sat(inout[get_global_id(0)], 2u);\n\
   }\n";
   const uint32_t input[] = {
      0xffffffff - 3, 0xffffffff - 2, 0xffffffff - 1, 0xffffffff
   };
   const uint32_t expected[] = {
      0xffffffff - 1, 0xffffffff, 0xffffffff, 0xffffffff
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, sub_sat)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = sub_sat(inout[get_global_id(0)], 2u);\n\
   }\n";
   const uint32_t input[] = {
      0, 1, 2, 3
   };
   const uint32_t expected[] = {
      0, 0, 0, 1
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, mul_hi)
{
   const char *kernel_source =
   "__kernel void main_test(__global uint *inout)\n\
   {\n\
       inout[get_global_id(0)] = mul_hi(inout[get_global_id(0)], 1u << 31);\n\
   }\n";
   const uint32_t input[] = {
      0, 1, 2, 3, (1u << 31)
   };
   const uint32_t expected[] = {
      0, 0, 1, 1, (1u << 30)
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, ldexp_x)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = ldexp(inout[get_global_id(0)], 5);\n\
   }\n";
   const float input[] = {
      0.0f, 0.5f, 1.0f, 2.0f
   };
   const float expected[] = {
      ldexp(0.0f, 5), ldexp(0.5f, 5), ldexp(1.0f, 5), ldexp(2.0f, 5)
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}

TEST_F(ComputeTest, ldexp_y)
{
   const char *kernel_source =
   "__kernel void main_test(__global float *inout)\n\
   {\n\
       inout[get_global_id(0)] = ldexp(inout[get_global_id(0)], get_global_id(0));\n\
   }\n";
   const float input[] = {
      0.25f, 0.5f, 0.75f, 1.0f
   };
   const float expected[] = {
      ldexp(0.25f, 0), ldexp(0.5f, 1), ldexp(0.75f, 2), ldexp(1.0f, 3)
   };
   auto buf = run_shader_with_input(kernel_source, ARRAY_SIZE(expected), input);
   for (int i = 0; i < ARRAY_SIZE(expected); ++i)
      EXPECT_EQ(buf[i], expected[i]);
}
