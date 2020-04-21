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
extract_comps_from_vec32(nir_builder *b, nir_ssa_def *vec32,
                         unsigned dst_bit_size,
                         nir_ssa_def **dst_comps,
                         unsigned num_dst_comps)
{
   unsigned step = DIV_ROUND_UP(dst_bit_size, 32);
   unsigned comps_per32b = 32 / dst_bit_size;
   nir_ssa_def *tmp;

   for (unsigned i = 0; i < vec32->num_components; i += step) {
      switch (dst_bit_size) {
      case 64:
         tmp = nir_pack_64_2x32_split(b, nir_channel(b, vec32, i),
                                         nir_channel(b, vec32, i + 1));
         dst_comps[i / 2] = tmp;
         break;
      case 32:
         dst_comps[i] = nir_channel(b, vec32, i);
         break;
      case 16:
      case 8:
         unsigned dst_offs = i * comps_per32b;

         tmp = nir_unpack_bits(b, nir_channel(b, vec32, i), dst_bit_size);
         for (unsigned j = 0; j < comps_per32b && dst_offs + j < num_dst_comps; j++)
            dst_comps[dst_offs + j] = nir_channel(b, tmp, j);

         break;
      }
   }
}

static nir_ssa_def *
load_comps_to_vec32(nir_builder *b, unsigned src_bit_size,
                    nir_ssa_def **src_comps, unsigned num_src_comps)
{
   unsigned num_vec32comps = DIV_ROUND_UP(num_src_comps * src_bit_size, 32);
   unsigned step = DIV_ROUND_UP(src_bit_size, 32);
   unsigned comps_per32b = 32 / src_bit_size;
   nir_ssa_def *vec32comps[4];

   for (unsigned i = 0; i < num_vec32comps; i += step) {
      nir_ssa_def *tmp;
      switch (src_bit_size) {
      case 64:
         vec32comps[i] = nir_unpack_64_2x32_split_x(b, src_comps[i / 2]);
         vec32comps[i + 1] = nir_unpack_64_2x32_split_y(b, src_comps[i / 2]);
         break;
      case 32:
         vec32comps[i] = src_comps[i];
         break;
      case 16:
      case 8:
         unsigned src_offs = i * comps_per32b;

         vec32comps[i] = nir_u2u32(b, src_comps[src_offs]);
         for (unsigned j = 1; j < comps_per32b && src_offs + j < num_src_comps; j++) {
             nir_ssa_def *tmp = nir_ishl(b, nir_u2u32(b, src_comps[src_offs + j]),
                                            nir_imm_int(b, j * src_bit_size));
             vec32comps[i] = nir_ior(b, vec32comps[i], tmp);
         }
         break;
      }
   }

   return nir_vec(b, vec32comps, num_vec32comps);
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

   nir_intrinsic_instr *load =
      nir_intrinsic_instr_create(b->shader,
                                 nir_intrinsic_load_global_dxil);

   load->num_components = DIV_ROUND_UP(bit_size, 32);
   assert(intr->dest.is_ssa);
   load->src[0] = nir_src_for_ssa(buffer);
   load->src[1] = nir_src_for_ssa(offset);

   nir_ssa_dest_init(&load->instr, &load->dest, load->num_components,
                     32, intr->dest.ssa.name);
   nir_builder_instr_insert(b, &load->instr);

   nir_ssa_def *vec32 = &load->dest.ssa;

   /* If we have 2 bytes or less to load we need to adjust the u32 value so
    * we can always extract the LSB.
    */
   if (bit_size < 32) {
      nir_ssa_def *shift = nir_imul(b, nir_iand(b, ptr, nir_imm_int(b, 3)),
                                       nir_imm_int(b, 8));
      vec32 = nir_ushr(b, vec32, shift);
   }

   unsigned comp_offset = 0;
   extract_comps_from_vec32(b, vec32, bit_size, &result, 1);

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
   nir_ssa_def *value;

   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   unsigned comp_offset = 0;

   value = load_comps_to_vec32(b, bit_size, &intr->src[0].ssa, 1);

   if (bit_size >= 32) {
      nir_intrinsic_instr *store =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_store_global_dxil);
      store->num_components = value->num_components;
      store->src[0] = nir_src_for_ssa(value);
      store->src[1] = nir_src_for_ssa(buffer);
      store->src[2] = nir_src_for_ssa(offset);
      nir_builder_instr_insert(b, &store->instr);
   } else {
      nir_ssa_def *shift, *mask;
      switch (bit_size) {
      case 8:
         shift = nir_iand(b, ptr, nir_imm_int(b, 3));
         break;

      case 16:
         shift = nir_iand(b, ptr, nir_imm_int(b, 2));
         break;

      default:
         unreachable("unexpected bit_size");
      }

      /* need to decompose this into 32-bit masked writes */
      shift = nir_imul_imm(b, shift, 8);
      value = nir_ishl(b, value, shift);
      mask = nir_inot(b, nir_ishl(b, nir_imm_int(b, (1 << bit_size) - 1),
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
