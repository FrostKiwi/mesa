/*
 * Copyright (C) 2014 Intel Corporation
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
 *
 * Authors:
 *    Jason Ekstrand (jason@jlekstrand.net)
 *
 */

#include "nir.h"
#include "nir_search.h"

<%
# Convenience variables
a = 'a'
b = 'b'
c = 'c'
d = 'd'

# Written in the form (<search>, <replace>) where <search> is an expression
# and <replace> is either an expression or a value.  An expression is
# defined as a tuple of the form (<op>, <src0>, <src1>, <src2>, <src3>)
# where each source is either an expression or a value.  A value can be
# either a numeric constant or a string representing a variable name.  For
# constants, you have to be careful to make sure that it is the right type
# because python is unaware of the source and destination types of the
# opcodes.

optimizations = [
   (('fadd', a, 0.0), a),
   (('iadd', a, 0), a),
   (('fmul', a, 0.0), 0.0),
   (('imul', a, 0), 0),
   (('fadd', ('fmul', a, b), c), ('ffma', a, b, c)),
   (('fge', ('fneg', ('fabs', a)), 0.0), ('feq', a, 0.0)),
   (('fmin', ('fmax', a, 1.0), 0.0), ('fsat', a)),
# This one may not be exact
   (('feq', ('fadd', a, b), 0.0), ('feq', a, ('fneg', b))),
]

opts = [ Optimization(opt) for opt in optimizations ]

# Let's sort them by operation
opt_dict = {}
for opt in opts:
   if opt.search.op not in opt_dict:
      opt_dict[opt.search.op] = []

   opt_dict[opt.search.op].append(opt)
%>

<%def name="dump_expression(expr)" >
% for src in expr.sources:
   % if isinstance(src.val, Expression):
${dump_expression(src.val)}
   % endif
% endfor

static const nir_search_expression ${expr.name} = {
   .op = nir_op_${expr.op},
   .srcs = {
% for src in expr.sources:
      ${src},
% endfor
   },
};
</%def>

% for opt in opts:
   ${dump_expression(opt.search)}
   % if isinstance(opt.replace.val, Expression):
      ${dump_expression(opt.replace.val)}
   % endif
% endfor

struct pair {
   const nir_search_expression *search;
   nir_search_value replace;
};

% for (op, opt_list) in opt_dict.iteritems():
static const struct pair ${op}_opts[] = {
% for opt in opt_list:
   { ${opt.search}, ${opt.replace} },
% endfor
};
% endfor

struct opt_state {
   void *mem_ctx;
   bool progress;
};

static bool
nir_opt_algebraic_block(nir_block *block, void *void_state)
{
   struct opt_state *state = void_state;

   nir_foreach_instr_safe(block, instr) {
      if (instr->type != nir_instr_type_alu)
         continue;

      nir_alu_instr *alu = nir_instr_as_alu(instr);
      if (!alu->dest.dest.is_ssa)
         continue;

      switch (alu->op) {
      % for op in opt_dict.keys():
      case nir_op_${op}:
         for (unsigned i = 0; i < ARRAY_SIZE(${op}_opts); i++) {
            if (nir_replace_instr(alu, ${op}_opts[i].search,
                                  &${op}_opts[i].replace, state->mem_ctx))
               state->progress = true;
         }
         break;
      % endfor
      default:
         break;
      }
   }

   return true;
}

static bool
nir_opt_algebraic_impl(nir_function_impl *impl)
{
   struct opt_state state;

   state.mem_ctx = ralloc_parent(impl);
   state.progress = false;

   nir_foreach_block(impl, nir_opt_algebraic_block, &state);

   if (state.progress)
      nir_metadata_dirty(impl, nir_metadata_block_index |
                               nir_metadata_dominance);

   return state.progress;
}

bool
nir_opt_algebraic(nir_shader *shader)
{
   bool progress = false;

   nir_foreach_overload(shader, overload) {
      if (overload->impl)
         progress |= nir_opt_algebraic_impl(overload->impl);
   }

   return progress;
}
