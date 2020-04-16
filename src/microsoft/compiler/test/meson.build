# Copyright © Microsoft Corporation

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice (including the next
# paragraph) shall be included in all copies or substantial portions of the
# Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

test('clc_nir_to_dxil',
  executable(
   'clc_nir_to_dxil_test',
    ['clc_nir_to_dxil_test.cpp', 'clc_test_dxlayer.cpp'],
    cpp_args : [cpp_vis_args, cpp_msvc_compat_args],
    include_directories : [inc_include, inc_src, inc_mapi, inc_mesa, inc_gallium],
    dependencies : [idep_libdxil_compiler, dep_thread, idep_gtest, idep_nir, idep_mesautil]
  ),
  suite : ['microsoft' , 'compiler'],
)

test('dxil_buffer test',
  executable('dxil_buffer_test', 'dxil_buffer_test.c',
    include_directories : inc_src,
    dependencies : [idep_libdxil_compiler]
  ),
  suite : ['microsoft' , 'compiler'],
)
