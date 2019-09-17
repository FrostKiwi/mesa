from __future__ import print_function, division

COPYRIGHT = """
/*
 * Copyright © 2019 Intel Corporation
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
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
"""

import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument('-p', '--import-path', required=True)
args = parser.parse_args()
sys.path.insert(0, args.import_path)

from register_allocate import *
import mako.template

MAX_HW_GRF_SIZE = 16 * 32
TOTAL_GRF_BYTES = 4096
NUM_PHYS_SMALL = 5
NUM_PHYS_LARGE = MAX_HW_GRF_SIZE // 32

STRUCT_DECL = """
"""

TEMPLATE = mako.template.Template("""
<% name = "ibc_reg_set_simd" + str(simd_width) %>
<%def name="class_arr(arr)">
% for c in arr:
    {
        .nr = ${c.nr},
        .reg_align = ${c.reg_align},
        .reg_size = ${c.reg_size},
        .reg_start = ${c.reg_start},
        .reg_end = ${c.reg_start + c.num_regs},
    },
% endfor
</%def>

${reg_set.render(name + "_regs")}

struct ibc_reg_set ${name} = {
    .regs = &${name + "_regs"},
    .simd_width = ${simd_width},
    .phys_small = {
        ${class_arr(phys_small)},
    },
    .phys_large = {
        ${class_arr(phys_large)},
    },
    .strided = {
% for per_num_comp in strided:
        {
            ${class_arr(per_num_comp)}
        },
% endfor
    },
    .ra_reg_to_grf = {
% for grf in ra_reg_to_grf:
        ${grf},
% endfor
    },
};
""")

class ibc_reg_class(object):
    def __init__(self, reg_size, reg_align, num_regs):
        self.nr = None
        self.reg_size = reg_size
        self.reg_align = reg_align
        self.num_regs = num_regs

def ibc_render_reg_set(simd_width):
    classes = []
    phys_small = [ ibc_reg_class(1 << i, 1 << i, TOTAL_GRF_BYTES >> i)
                   for i in range(NUM_PHYS_SMALL) ]
    classes += phys_small

    phys_large = [ ibc_reg_class((i + 1) * 32, 32, (TOTAL_GRF_BYTES // 32) - i)
                   for i in range(NUM_PHYS_LARGE) ]
    classes += phys_large

    strided = []
    for stride in [1, 2, 4, 8]:
        if stride * simd_width > 64:
            break

        per_num_comps = []
        for num_comps in [1, 2, 3, 4]:
            c = ibc_reg_class(stride * simd_width * num_comps,
                              stride * simd_width,
                              (TOTAL_GRF_BYTES // (stride * simd_width)) -
                                  (num_comps - 1))
            per_num_comps.append(c)
        strided += [per_num_comps]
        classes += per_num_comps

    ra_reg_count = 0
    for i, c in enumerate(classes):
        c.nr = i
        c.reg_start = ra_reg_count
        ra_reg_count += c.num_regs

    reg_set = ra_reg_set(ra_reg_count, len(classes), True)
    ra_reg_to_grf = [None] * ra_reg_count
    ra_reg_to_grf_end = [None] * ra_reg_count

    reg = 0
    for c in classes:
        # Assume that every class strides by its alignment
        assert reg == c.reg_start
        for grf in range(0, TOTAL_GRF_BYTES - c.reg_size + 1, c.reg_align):
            reg_set.class_add_reg(c.nr, reg)
            ra_reg_to_grf[reg] = grf
            ra_reg_to_grf_end[reg] = grf + c.reg_size
            for i in range(c.reg_size):
                reg_set.add_reg_conflict(reg, grf + i)
            reg += 1
        assert reg == c.reg_start + c.num_regs
    assert reg == ra_reg_count

    for base in range(TOTAL_GRF_BYTES):
        reg_set.make_reg_conflicts_transitive(base)

    for b in classes:
        for c in classes:
            if c.reg_align == c.reg_size and \
               b.reg_align == b.reg_size and \
               b.reg_size == c.reg_size:
                # If B's and C's registers are both aligned to a register and
                # C's fit inside B's size and alignment then one register in C
                # always maps to one register in B.
                reg_set.class_set_q(b.nr, c.nr, 1)

#                c_per_b = c.reg_size // b.reg_size
#                for i in range(c.num_regs):
#                    c_reg = c.reg_start + i
#                    b_reg = b.reg_start + i // c_per_b
#                    reg_set.add_reg_conflict_non_reflexive(c_reg, b_reg)

            elif b.reg_align == b.reg_size and \
                 c.reg_align >= b.reg_align and \
                 c.reg_size >= b.reg_size:
                # If B's registers are aligned to a register and fit inside C's
                # alignment then one register in C always maps to an integer
                # number of registers in B and there's no weird overlap.
                reg_set.class_set_q(b.nr, c.nr, c.reg_size // b.reg_size)

#                b_per_c = c.reg_size // b.reg_size
#                c_stride_b = c.reg_align // b.reg_size
#                for i in range(c.num_regs):
#                    c_reg = c.reg_start + i
#                    for j in range(b_per_c):
#                        b_reg = b.reg_start + i * c_stride_b + j
#                        reg_set.add_reg_conflict_non_reflexive(c_reg, b_reg)

            else:
                # View the register from C as fixed starting at GRF n somwhere
                # in the middle, and the register from B as sliding back and
                # forth.  Then the first register to conflict from B is the
                # one starting at n - classes[b].reg_size + 1 and the last
                # register to conflict will start at n + class[b].reg_size - 1.
                # Therefore, the number of conflicts from B is
                #
                #    classes[b].reg_size + classes[c].reg_size - 1.
                #
                #   +-+-+-+-+-+-+     +-+-+-+-+-+-+
                # B | | | | | |n| --> | | | | | | |
                #   +-+-+-+-+-+-+     +-+-+-+-+-+-+
                #             +-+-+-+-+-+
                # C           |n| | | | |
                #             +-+-+-+-+-+
                reg_set.class_set_q(b.nr, c.nr, c.reg_size + b.reg_size - 1)

#                for i in range(c.num_regs):
#                    for j in range(b.num_regs):
#                        c_reg = c.reg_start + i
#                        b_reg = b.reg_start + j
#                        if ra_reg_to_grf_end[c_reg] > ra_reg_to_grf[b_reg] and \
#                           ra_reg_to_grf_end[b_reg] > ra_reg_to_grf[c_reg]:
#                            reg_set.add_reg_conflict_non_reflexive(c_reg, b_reg)

    try:
        return TEMPLATE.render(simd_width=simd_width,
                               reg_set=reg_set,
                               phys_small=phys_small,
                               phys_large=phys_large,
                               strided=strided,
                               ra_reg_to_grf=ra_reg_to_grf)
    except Exception:
        # In the event there's an error, this imports some helpers from mako
        # to print a useful stack trace and prints it, then exits with
        # status 1, if python is run with debug; otherwise it just raises
        # the exception
        if __debug__:
            import sys
            from mako import exceptions
            sys.stderr.write(exceptions.text_error_template().render() + '\n')
            sys.exit(1)
        raise

print(COPYRIGHT)
print(ibc_render_reg_set(8))
print(ibc_render_reg_set(16))
print(ibc_render_reg_set(32))
