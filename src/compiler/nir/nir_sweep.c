/*
 * Copyright © 2015 Intel Corporation
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

#include "nir.h"

/**
 * \file nir_sweep.c
 *
 * The nir_sweep() pass performs a mark and sweep pass over a nir_shader's associated
 * memory - anything still connected to the program will be kept, and any dead memory
 * we dropped on the floor will be freed.
 *
 * The expectation is that drivers should call this when finished compiling the shader
 * (after any optimization, lowering, and so on).  However, it's also fine to call it
 * earlier, and even many times, trading CPU cycles for memory savings.
 */

#define steal_list(mem_ctx, type, list) \
   foreach_list_typed(type, obj, node, list) { ralloc_steal(mem_ctx, obj); }

static void sweep_cf_node(nir_shader *nir, nir_cf_node *cf_node);

static void
sweep_deref(nir_shader *nir, nir_deref *deref)
{
   if (!deref)
      return;

   ralloc_steal(nir, deref);
   sweep_deref(nir, deref->child);
}

static bool
sweep_src_indirect(nir_src *src, void *nir)
{
   if (!src->is_ssa && src->reg.indirect)
      ralloc_steal(nir, src->reg.indirect);

   return true;
}

static bool
sweep_dest_indirect(nir_dest *dest, void *nir)
{
   if (!dest->is_ssa && dest->reg.indirect)
      ralloc_steal(nir, dest->reg.indirect);

   return true;
}

static void
sweep_instr(nir_shader *nir, nir_instr *instr)
{
   ralloc_steal(nir, instr);

   switch (instr->type) {
   case nir_instr_type_intrinsic: {
      nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
      for (unsigned i = 0;
           i < nir_intrinsic_infos[intrin->intrinsic].num_variables; i++) {
         sweep_deref(nir, &intrin->variables[i]->deref);
      }
      break;
   }

   case nir_instr_type_tex: {
      nir_tex_instr *tex = nir_instr_as_tex(instr);
      sweep_deref(nir, &tex->texture->deref);
      sweep_deref(nir, &tex->sampler->deref);
      break;
   }

   case nir_instr_type_call: {
      nir_call_instr *call = nir_instr_as_call(instr);
      sweep_deref(nir, &call->return_deref->deref);
      for (unsigned i = 0; i < call->num_params; i++)
         sweep_deref(nir, &call->params[i]->deref);
      break;
   }

   default:
      break;
   }

   nir_foreach_src(instr, sweep_src_indirect, nir);
   nir_foreach_dest(instr, sweep_dest_indirect, nir);
}

static void
sweep_block(nir_shader *nir, nir_block *block)
{
   ralloc_steal(nir, block);

   nir_foreach_instr(instr, block)
      sweep_instr(nir, instr);
}

static void
sweep_if(nir_shader *nir, nir_if *iff)
{
   ralloc_steal(nir, iff);

   foreach_list_typed(nir_cf_node, cf_node, node, &iff->then_list) {
      sweep_cf_node(nir, cf_node);
   }

   foreach_list_typed(nir_cf_node, cf_node, node, &iff->else_list) {
      sweep_cf_node(nir, cf_node);
   }
}

static void
sweep_loop(nir_shader *nir, nir_loop *loop)
{
   ralloc_steal(nir, loop);

   foreach_list_typed(nir_cf_node, cf_node, node, &loop->body) {
      sweep_cf_node(nir, cf_node);
   }
}

static void
sweep_cf_node(nir_shader *nir, nir_cf_node *cf_node)
{
   switch (cf_node->type) {
   case nir_cf_node_block:
      sweep_block(nir, nir_cf_node_as_block(cf_node));
      break;
   case nir_cf_node_if:
      sweep_if(nir, nir_cf_node_as_if(cf_node));
      break;
   case nir_cf_node_loop:
      sweep_loop(nir, nir_cf_node_as_loop(cf_node));
      break;
   default:
      unreachable("Invalid CF node type");
   }
}

static void
sweep_impl(nir_shader *nir, nir_function_impl *impl)
{
   ralloc_steal(nir, impl);

   ralloc_steal(nir, impl->params);
   for (unsigned i = 0; i < impl->num_params; i++)
      ralloc_steal(nir, impl->params[i]);
   ralloc_steal(nir, impl->return_var);
   steal_list(nir, nir_variable, &impl->locals);
   steal_list(nir, nir_register, &impl->registers);

   foreach_list_typed(nir_cf_node, cf_node, node, &impl->body) {
      sweep_cf_node(nir, cf_node);
   }

   sweep_block(nir, impl->end_block);

   /* Wipe out all the metadata, if any. */
   nir_metadata_preserve(impl, nir_metadata_none);
}

static void
sweep_function(nir_shader *nir, nir_function *f)
{
   ralloc_steal(nir, f);
   ralloc_steal(nir, f->params);

   if (f->impl)
      sweep_impl(nir, f->impl);
}

void
nir_sweep(nir_shader *nir)
{
   void *rubbish = ralloc_context(NULL);

   /* The shader may not own shader_info so check first */
   bool steal_info = false;
   if (nir == ralloc_parent(nir->info))
      steal_info = true;

   /* First, move ownership of all the memory to a temporary context; assume dead. */
   ralloc_adopt(rubbish, nir);

   if (steal_info)
      ralloc_steal(nir, nir->info);

   ralloc_steal(nir, (char *)nir->info->name);
   if (nir->info->label)
      ralloc_steal(nir, (char *)nir->info->label);

   /* Variables and registers are not dead.  Steal them back. */
   steal_list(nir, nir_variable, &nir->uniforms);
   steal_list(nir, nir_variable, &nir->inputs);
   steal_list(nir, nir_variable, &nir->outputs);
   steal_list(nir, nir_variable, &nir->shared);
   steal_list(nir, nir_variable, &nir->globals);
   steal_list(nir, nir_variable, &nir->system_values);
   steal_list(nir, nir_register, &nir->registers);

   /* Recurse into functions, stealing their contents back. */
   foreach_list_typed(nir_function, func, node, &nir->functions) {
      sweep_function(nir, func);
   }

   /* Free everything we didn't steal back. */
   ralloc_free(rubbish);
}
