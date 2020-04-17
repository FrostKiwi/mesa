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

#include "dxil_nir.h"

#include "nir_builder.h"

static nir_ssa_def *
ptr_to_buffer(nir_builder *b, nir_ssa_def *ptr)
{
   return nir_ishr(b, ptr, nir_imm_int(b, 28));
}

static nir_ssa_def *
ptr_to_offset(nir_builder *b, nir_ssa_def *ptr)
{
   return nir_iand(b, ptr, nir_imm_int(b, 0x0ffffffc));
}

static void
lower_load_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->num_components == 1); // no support for vectors

   /* source 'pointer' */
   assert(intr->src[0].is_ssa);
   nir_ssa_def *ptr = intr->src[0].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   nir_ssa_def *result;
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   if (bit_size == 64) {
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_load_global_dxil);

      load->num_components = 2;
      assert(intr->dest.is_ssa);
      load->src[0] = nir_src_for_ssa(buffer);
      load->src[1] = nir_src_for_ssa(offset);

      nir_ssa_dest_init(&load->instr, &load->dest,
                        2, 32, intr->dest.ssa.name);
      nir_builder_instr_insert(b, &load->instr);

      result = &load->dest.ssa;
      nir_ssa_def *value_lo = nir_channel(b, result, 0);
      nir_ssa_def *value_hi = nir_channel(b, result, 1);
      result = nir_ior(b, nir_u2u64(b, value_lo),
                          nir_ishl(b, nir_u2u64(b, value_hi),
                                      nir_imm_int(b, 32)));
   } else {
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_load_global_dxil);
      load->num_components = 1;

      assert(intr->dest.is_ssa);
      load->src[0] = nir_src_for_ssa(buffer);
      load->src[1] = nir_src_for_ssa(offset);
      nir_ssa_dest_init(&load->instr, &load->dest, load->num_components,
                        32, intr->dest.ssa.name);
      nir_builder_instr_insert(b, &load->instr);

      result = &load->dest.ssa;
      switch (bit_size) {
      case 8:
         result = nir_extract_u8(b, result,
                                    nir_iand(b, ptr, nir_imm_int(b, 3)));
         result = nir_u2u8(b, result);
         break;

      case 16:
         result = nir_extract_u16(b, result,
                                     nir_iand(b, nir_ushr(b, ptr,
                                                             nir_imm_int(b, 1)),
                                                 nir_imm_int(b, 1)));
         result = nir_u2u16(b, result);
         break;

      case 32:
         /* nothing */
         break;

      default:
         unreachable("unexpected bit_size");
      }
   }

   nir_ssa_def_rewrite_uses(&intr->dest.ssa,
                            nir_src_for_ssa(result));
   nir_instr_remove(&intr->instr);
}

static void
lower_store_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->num_components == 1); // no support for vectors

   /* source 'pointer' */
   assert(intr->src[1].is_ssa);
   nir_ssa_def *ptr = intr->src[1].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   assert(intr->src[0].is_ssa);
   nir_ssa_def *value = intr->src[0].ssa;

   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   if (bit_size == 64) {
      nir_intrinsic_instr *store =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_store_global_dxil);
      store->num_components = 2;
      nir_ssa_def *value_lo = nir_u2u32(b, value);
      nir_ssa_def *value_hi = nir_u2u32(b, nir_ushr(b, value,
                                                       nir_imm_int(b, 32)));

      store->src[0] = nir_src_for_ssa(nir_vec2(b, value_lo, value_hi));
      store->src[1] = nir_src_for_ssa(buffer);
      store->src[2] = nir_src_for_ssa(offset);
      nir_builder_instr_insert(b, &store->instr);
   } else {
      nir_ssa_def *shift = NULL, *mask;
      switch (bit_size) {
      case 8:
         shift = nir_iand(b, ptr, nir_imm_int(b, 3));
         break;

      case 16:
         shift = nir_iand(b, ptr, nir_imm_int(b, 2));
         break;

      case 32:
         /* nothing */
         break;

      default:
         unreachable("unexpected bit_size");
      }

      if (shift) {
         /* need to decompose this into 32-bit masked writes */
         shift = nir_imul_imm(b, shift, 8);
         value = nir_ishl(b, nir_u2u32(b, value), shift);
         nir_ssa_def *mask =
            nir_inot(b, nir_ishl(b, nir_imm_int(b, (1 << bit_size) - 1),
                                    shift));

         nir_intrinsic_instr *store =
            nir_intrinsic_instr_create(b->shader,
                                       nir_intrinsic_store_global_masked_dxil);
         store->num_components = 1;
         store->src[0] = nir_src_for_ssa(value);
         store->src[1] = nir_src_for_ssa(mask);
         store->src[2] = nir_src_for_ssa(buffer);
         store->src[3] = nir_src_for_ssa(offset);
         nir_builder_instr_insert(b, &store->instr);
      } else {
         nir_intrinsic_instr *store =
            nir_intrinsic_instr_create(b->shader,
                                       nir_intrinsic_store_global_dxil);
         store->num_components = 1;
         store->src[0] = nir_src_for_ssa(value);
         store->src[1] = nir_src_for_ssa(buffer);
         store->src[2] = nir_src_for_ssa(offset);
         nir_builder_instr_insert(b, &store->instr);
      }
   }

   nir_instr_remove(&intr->instr);
}

bool
dxil_nir_lower_global_mem_to_dxil(nir_shader *nir)
{
   bool progress = false;

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;
            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

            switch (intr->intrinsic) {
            case nir_intrinsic_load_global:
               lower_load_global(&b, intr);
               progress = true;
               break;
            case nir_intrinsic_store_global:
               lower_store_global(&b, intr);
               progress = true;
               break;
            }
         }
      }
   }

   return progress;
}
