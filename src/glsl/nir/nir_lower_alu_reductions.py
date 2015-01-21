#! /usr/bin/env python
#
# Copyright (C) 2014 Intel Corporation
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
#
# Authors:
#    Jason Ekstrand (jason@jlekstrand.net)

import nir_algebraic

def reduction(name, num_srcs, reduce_op,
              pre_reduce_op = None, pre_reduce_const = None):
   def get_pre_reduce(chan):
      swz = 'xyzw'[chan]
      if pre_reduce_op is None:
         return 'a.' + swz

      if num_srcs == 1:
         if pre_reduce_const is None:
            return (pre_reduce_op, 'a.' + swz)
         else:
            return (pre_reduce_op, 'a.' + swz, pre_reduce_const)
      elif num_srcs == 2:
         return (pre_reduce_op, 'a.' + swz, 'b.' + swz)
      else:
         assert False

   def get_search(chans):
      if num_srcs == 1:
         return (name + str(chans), 'a')
      elif num_srcs == 2:
         return (name + str(chans), 'a', 'b')
      else:
         assert False

   return [
      (get_search(2), (reduce_op, get_pre_reduce(0), get_pre_reduce(1))),
      (get_search(3), (reduce_op,
                       (reduce_op, get_pre_reduce(0), get_pre_reduce(1)),
                       get_pre_reduce(2))),
      (get_search(4), (reduce_op,
                       (reduce_op, get_pre_reduce(0), get_pre_reduce(1)),
                       (reduce_op, get_pre_reduce(2), get_pre_reduce(3))))
   ]

transforms = []

transforms += reduction('bany', 1, 'ior', 'ine', 0)
transforms += reduction('ball', 1, 'iand', 'ine', 0)
transforms += reduction('fany', 1, 'for')
transforms += reduction('fall', 1, 'fand')
transforms += reduction('ball_fequal', 2, 'iand', 'feq')
transforms += reduction('bany_fnequal', 2, 'ior', 'fne')
transforms += reduction('ball_iequal', 2, 'iand', 'ieq')
transforms += reduction('bany_inequal', 2, 'ior', 'ine')
transforms += reduction('fall_equal', 2, 'fand', 'seq')
transforms += reduction('fany_nequal', 2, 'for', 'sne')
transforms += reduction('fdot', 2, 'fadd', 'fmul')

print nir_algebraic.AlgebraicPass("nir_lower_alu_reductions", transforms).render()
