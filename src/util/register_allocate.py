from __future__ import division

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

import itertools
import mako.template

TEMPLATE = mako.template.Template("""
#include "util/register_allocate_priv.h"

static const struct ra_reg %{name}_regs[] = {
% for i in range(len(reg_set.regs)):
    {
        .conflicts = {
${reg_set.regs[i].conflicts.words_to_str()}
        },
    },
% endfor
};

static const struct ra_class %{name}_classes[] = {
% for i in range(len(reg_set.classes)):
    {
        .regs = {
${reg_set.classes[i].regs.words_to_str()}
        },
        .p = ${reg_set.classes[i].p},
        .q = ${reg_set.classes[i].q},
    },
% endfor
};

static const struct ra_regs ${name} = {
    .regs = ${name}_regs,
    .count = ${len(reg_set.regs)}
    .classes = {
% for i in range(len(reg_set.classes)):
        &${name}_class_${i},
% endfor
    },
    .class_count = ${len(reg_set.classes)},
    .round_robin = ${"true" if reg_set.round_robin else "false"},
};
""")

BITSET_WORDBITS = 32
def BITSET_WORDS(x):
    return (x + BITSET_WORDBITS - 1) // BITSET_WORDBITS


class BitSet(object):
    def __init__(self, size):
        self.words = [0] * BITSET_WORDS(size)

    def test(self, i):
        w = i // BITSET_WORDBITS
        b = i % BITSET_WORDBITS
        return bool(self.words[w] & (1 << b))

    def set(self, i):
        w = i // BITSET_WORDBITS
        b = i % BITSET_WORDBITS
        self.words[w] |= (1 << b)

    def or_eq(self, other):
        assert len(self.words) == len(other.words)
        for w in range(len(self.words)):
            self.words[w] |= other.words[w]

    def foreach_set(self):
        for w in range(len(self.words)):
            for b in range(BITSET_WORDBITS):
                if self.words[w] & (1 << b):
                    yield w * BITSET_WORDBITS + b

    def _words_to_str(self):
        for w in range(len(self.words)):
            if w > 0 and w % 8 == 0:
                yield "\n"
            yield hex(self.words[w]) + ", "

    def words_to_str(self):
        return "".join(self._words_to_str())


class ra_reg(object):
    def __init__(self, idx, reg_count):
        self.conflicts = BitSet(reg_count)
        self.conflicts.set(idx)


class ra_class(object):
    def __init__(self, reg_count, class_count):
        self.regs = BitSet(reg_count)
        self.p = 0;
        self.q = [None] * class_count;


class ra_reg_set(object):
    def __init__(self, reg_count, class_count, round_robin = False):
        self.regs = [ ra_reg(i, reg_count) for i in range(reg_count) ]
        self.classes = [ ra_class(reg_count, class_count)
                         for i in range(class_count) ]
        self.round_robin = round_robin

    def add_reg_conflict(self, r1, r2):
        if not self.regs[r1].conflicts.test(r2):
            self.regs[r1].conflicts.set(r2)
            self.regs[r2].conflicts.set(r1)

    def add_reg_conflict_non_reflexive(self, r1, r2):
        self.regs[r1].conflicts.set(r2)

    def make_reg_conflicts_transitive(self, r):
        for c in self.regs[r].conflicts.foreach_set():
            self.regs[c].conflicts.or_eq(self.regs[r].conflicts)

    def class_add_reg(self, c, r):
        assert not self.classes[c].regs.test(r)
        self.classes[c].regs.set(r)
        self.classes[c].p += 1

    def class_set_q(self, b, c, q):
        self.classes[b].q[c] = q

    def render(self, name):
        return TEMPLATE.render(reg_set=self, name=name)
