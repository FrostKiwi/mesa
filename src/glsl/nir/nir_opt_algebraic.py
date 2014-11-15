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
#

import itertools, sys, mako.template

# Represents a set of variables, each with a unique id
class VarSet(object):
   def __init__(self):
      self.names = {}
      self.ids = itertools.count()

   def __getitem__(self, name):
      if name not in self.names:
         self.names[name] = self.ids.next()

      return self.names[name]

class Expression(object):
   def __init__(self, expr, name_base, varset):
      self.name = name_base
      self.op = expr[0]
      self.varset = varset
      self.sources = [ Value(src, "{0}_{1}".format(name_base, i), varset)
                       for (i, src) in enumerate(expr[1:]) ]

   def __str__(self):
      return "&{0}".format(self.name)

class Value(object):
   def __init__(self, val, name_base, varset):
      if isinstance(val, tuple):
         self.val = Expression(val, name_base, varset)
         self.field = "expression"
         self.type_str = "nir_search_value_expression"
      elif isinstance(val, Expression):
         self.val = val
         self.field = "expression"
         self.type_str = "nir_search_value_expression"
      elif isinstance(val, str):
         self.val = varset[val]
         self.field = "variable"
         self.type_str = "nir_search_value_variable"
      elif isinstance(val, (int, long)):
         self.val = val
         self.field = "constant.i"
         self.type_str = "nir_search_value_constant"
      elif isinstance(val, (float)):
         self.val = val
         self.field = "constant.f"
         self.type_str = "nir_search_value_constant"

   def __str__(self):
      return "{{ .type = {0}, .{1} = {2} }}".format(self.type_str,
                                                    self.field, self.val)

optimization_ids = itertools.count()

class Optimization(object):
   def __init__(self, opt):
      self.id = optimization_ids.next()

      varset = VarSet()
      self.search = Expression(opt[0], "search{0}".format(self.id), varset)
      self.replace = Value(opt[1], "replace{0}".format(self.id), varset)

template = mako.template.Template(filename=sys.argv[1])
print template.render(
   Optimization=Optimization,
   Expression=Expression,
   Value=Value,
)

