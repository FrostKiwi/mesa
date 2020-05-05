#
# Copyright (C) 2020 Microsoft Corporation
#
# Copyright (C) 2018 Alyssa Rosenzweig
#
# Copyright (C) 2016 Intel Corporation
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice (including the next
# paragraph) shall be included in all copies or substantial portions of the
# Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

import argparse
import sys
import math

a = 'a'

# The nir_lower_bit_size() pass gets rid of all 8bit ALUs but insert new u2u8
# and i2i8 operations to convert the result back to the original type after the
# arithmetic operation is done. Those u2u8 and i2i8 operations, as any other
# 8bit operations, are not supported by DXIL and needs to be discarded. The
# dxil_nir_lower_8bit_conv() pass is here for that.

no_8bit_conv = []

for outer_op_type in ('u2u', 'i2i', 'u2f', 'i2f'):
    for outer_op_sz in (16, 32, 64):
        outer_op = outer_op_type + str(int(outer_op_sz))
        for inner_op in ('u2u8', 'i2i8'):
            for src_sz in (16, 32, 64):
                orig_seq = (outer_op, (inner_op, 'a@' + str(int(src_sz))))
                if (outer_op[0] == 'u'):
                    new_seq = ('iand', a, 0xff)
                else:
                    shift = src_sz - 8
                    new_seq = ('ishr', ('ishl', a, shift), shift)
                if outer_op_sz != src_sz:
                    conv_op = outer_op[0] + '2' + outer_op[0] + str(int(outer_op_sz))
                    new_seq = (conv_op, new_seq)
                no_8bit_conv += [(orig_seq, new_seq)]

lower_b2b = [
  (('b2b32', 'a'), ('b2i32', 'a')),
  (('b2b1', 'a'), ('i2b1', 'a')),
]

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--import-path', required=True)
    args = parser.parse_args()
    sys.path.insert(0, args.import_path)
    run()


def run():
    import nir_algebraic  # pylint: disable=import-error

    print('#include "dxil_nir.h"')

    print(nir_algebraic.AlgebraicPass("dxil_nir_lower_8bit_conv",
                                      no_8bit_conv).render())
    print(nir_algebraic.AlgebraicPass("dxil_nir_lower_b2b",
                                      lower_b2b).render())

if __name__ == '__main__':
    main()
