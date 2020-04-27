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
   assert(intr->dest.is_ssa);
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   unsigned num_components = nir_dest_num_components(intr->dest);
   unsigned num_bits = num_components * bit_size;

   b->cursor = nir_before_instr(&intr->instr);

   /* source 'pointer' */
   assert(intr->src[0].is_ssa);
   nir_ssa_def *ptr = intr->src[0].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];
   unsigned comp_idx = 0;

   /* We need to split loads in 16byte chunks because that's the optimal
    * granularity of bufferLoad(). Minimum alignment is 4byte, which saves
    * from us from extra complexity to extract >= 32 bit components.
    */
   for (unsigned i = 0; i < num_bits; i += 4 * 32) {
      /* For each 16byte chunk (or smaller) we generate a 32bit global vec
       * load.
       */
      unsigned subload_num_bits = MIN2(num_bits - i, 4 * 32);
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_load_global_dxil);

      /* The number of components to store depends on the number of bytes. */
      load->num_components = DIV_ROUND_UP(subload_num_bits, 32);
      load->src[0] = nir_src_for_ssa(buffer);
      load->src[1] = nir_src_for_ssa(nir_iadd(b, offset, nir_imm_int(b, i / 8)));
      nir_ssa_dest_init(&load->instr, &load->dest, load->num_components,
                        32, NULL);
      nir_builder_instr_insert(b, &load->instr);

      nir_ssa_def *vec32 = &load->dest.ssa;

      /* If we have 2 bytes or less to load we need to adjust the u32 value so
       * we can always extract the LSB.
       */
      if (subload_num_bits <= 16) {
         nir_ssa_def *shift = nir_imul(b, nir_iand(b, ptr, nir_imm_int(b, 3)),
                                          nir_imm_int(b, 8));
         vec32 = nir_ushr(b, vec32, shift);
      }

      /* And now comes the pack/unpack step to match the original type. */
      extract_comps_from_vec32(b, vec32, bit_size, &comps[comp_idx],
                               subload_num_bits / bit_size);
      comp_idx += subload_num_bits / bit_size;
   }

   assert(comp_idx == num_components);
   nir_ssa_def *result = nir_vec(b, comps, num_components);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(result));
   nir_instr_remove(&intr->instr);
}

static void
lower_store_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->src[0].is_ssa);
   unsigned num_components = nir_src_num_components(intr->src[0]);
   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   unsigned num_bits = num_components * bit_size;

   b->cursor = nir_before_instr(&intr->instr);

   /* source 'pointer' */
   assert(intr->src[1].is_ssa);
   nir_ssa_def *ptr = intr->src[1].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];
   unsigned comp_idx = 0;

   for (unsigned i = 0; i < num_components; i++)
      comps[i] = nir_channel(b, intr->src[0].ssa, i);

   /* We split stores in 16byte chunks because that's the optimal granularity
    * of bufferStore(). Minimum alignment is 4byte, which saves from us from
    * extra complexity to store >= 32 bit components.
    */
   for (unsigned i = 0; i < num_bits; i += 4 * 32) {
      /* For each 16byte chunk (or smaller) we generate a 32bit global vec
       * store.
       */
      unsigned substore_num_bits = MIN2(num_bits - i, 4 * 32);
      nir_ssa_def *local_offset = nir_iadd(b, offset, nir_imm_int(b, i / 8));
      nir_ssa_def *vec32 = load_comps_to_vec32(b, bit_size, &comps[comp_idx],
                                               substore_num_bits / bit_size);
      nir_intrinsic_instr *store;

      if (substore_num_bits < 32) {
         nir_ssa_def *mask = nir_imm_int(b, (1 << substore_num_bits) - 1);

        /* If we have 16 bits or less to store we need to place them
         * correctly in the u32 component. Anything greater than 16 bits
         * (including uchar3) is naturally aligned on 32bits.
         */
         if (substore_num_bits <= 16) {
            nir_ssa_def *pos = nir_iand(b, ptr, nir_imm_int(b, 3));
            nir_ssa_def *shift = nir_imul_imm(b, pos, 8);

            vec32 = nir_ishl(b, vec32, shift);
            mask = nir_ishl(b, mask, shift);
         }

         store = nir_intrinsic_instr_create(b->shader,
                                            nir_intrinsic_store_global_masked_dxil);
         store->src[0] = nir_src_for_ssa(vec32);
         store->src[1] = nir_src_for_ssa(nir_inot(b, mask));
         store->src[2] = nir_src_for_ssa(buffer);
         store->src[3] = nir_src_for_ssa(local_offset);
      } else {
         store = nir_intrinsic_instr_create(b->shader,
                                            nir_intrinsic_store_global_dxil);
         store->src[0] = nir_src_for_ssa(vec32);
         store->src[1] = nir_src_for_ssa(buffer);
         store->src[2] = nir_src_for_ssa(local_offset);
      }

      /* The number of components to store depends on the number of bits. */
      store->num_components = DIV_ROUND_UP(substore_num_bits, 32);
      nir_builder_instr_insert(b, &store->instr);
      comp_idx += substore_num_bits / bit_size;
   }

   nir_instr_remove(&intr->instr);
}

bool
dxil_nir_lower_loads_stores_to_dxil(nir_shader *nir)
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
