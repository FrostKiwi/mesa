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
#include "nir_deref.h"

static nir_ssa_def *
ptr_to_buffer(nir_builder *b, nir_ssa_def *ptr)
{
   /* Buffers IDs are 1-based to support NULL pointers. We need to decrement
    * them when calculating the buffer index passed to global_dxil
    * intrinsics.
    */
   return nir_isub(b, nir_ishr(b, ptr, nir_imm_int(b, 28)), nir_imm_int(b, 1));
}

static nir_ssa_def *
ptr_to_offset(nir_builder *b, nir_ssa_def *ptr)
{
   return nir_iand(b, ptr, nir_imm_int(b, 0x0ffffffc));
}

static void
cl_type_size_align(const struct glsl_type *type, unsigned *size,
                   unsigned *align)
{
   *size = glsl_get_cl_size(type);
   *align = glsl_get_cl_alignment(type);
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

static bool
lower_load_deref(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->dest.is_ssa);

   b->cursor = nir_before_instr(&intr->instr);

   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   if (var->data.mode != nir_var_function_temp &&
       var->data.mode != nir_var_shader_temp)
      return false;
   nir_ssa_def *ptr = nir_build_deref_offset(b, deref, cl_type_size_align);
   nir_ssa_def *offset = nir_iand(b, ptr, nir_inot(b, nir_imm_int(b, 3)));
   unsigned load_size = 32;

   assert(intr->dest.is_ssa);
   unsigned num_components = nir_dest_num_components(intr->dest);
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   unsigned num_bits = num_components * bit_size;
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];
   unsigned comp_idx = 0;

   nir_deref_path path;
   nir_deref_path_init(&path, deref, NULL);

   /* Split loads into 32-bit chunks */
   for (unsigned i = 0; i < num_bits; i += load_size) {
      unsigned subload_num_bits = MIN2(num_bits - i, load_size);
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader, nir_intrinsic_load_ptr_dxil);

      load->num_components = 1;

      load->src[0] = nir_src_for_ssa(&path.path[0]->dest.ssa);

      load->src[1] =
         nir_src_for_ssa(nir_ishr(b, nir_iadd(b, offset, nir_imm_int(b, i / 8)),
                                  nir_imm_int(b, 2 /* log2(32 / 8) */)));
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

   nir_deref_path_finish(&path);
   assert(comp_idx == num_components);
   nir_ssa_def *result = nir_vec(b, comps, num_components);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(result));
   nir_instr_remove(&intr->instr);
   return true;
}

static nir_ssa_def *
ubo_load_select_32b_comps(nir_builder *b, nir_ssa_def *vec32,
                          nir_ssa_def *offset, unsigned num_bytes)
{
   assert(num_bytes == 16 || num_bytes == 12 || num_bytes == 8 ||
          num_bytes == 4 || num_bytes == 3 || num_bytes == 2 ||
          num_bytes == 1);
   assert(vec32->num_components == 4);

   /* 16 and 12 byte types are always aligned on 16 bytes. */
   if (num_bytes > 8)
      return vec32;

   nir_ssa_def *comps[4];
   nir_ssa_def *cond;

   for (unsigned i = 0; i < 4; i++)
      comps[i] = nir_channel(b, vec32, i);

   /* If we have 8bytes or less to load, select which half the vec4 should
    * be used.
    */
   cond = nir_ine(b, nir_iand(b, offset, nir_imm_int(b, 0x8)),
                                 nir_imm_int(b, 0));

   comps[0] = nir_bcsel(b, cond, comps[2], comps[0]);
   comps[1] = nir_bcsel(b, cond, comps[3], comps[1]);

   /* Thanks to the CL alignment constraints, if we want 8 bytes we're done. */
   if (num_bytes == 8)
      return nir_vec(b, comps, 2);

   /* 4 bytes or less needed, select which of the 32bit component should be
    * used and return it. The sub-32bit split is handled in
    * extract_comps_from_vec32().
    */
   cond = nir_ine(b, nir_iand(b, offset, nir_imm_int(b, 0x4)),
                                 nir_imm_int(b, 0));
   return nir_bcsel(b, cond, comps[1], comps[0]);
}

static nir_ssa_def *
emit_load_ubo_dxil(nir_builder *b, nir_ssa_def *buffer,
                   nir_ssa_def *offset, unsigned num_components,
                   unsigned bit_size)
{
   nir_ssa_def *idx = nir_ushr(b, offset, nir_imm_int(b, 4));
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];
   unsigned num_bits = num_components * bit_size;
   unsigned comp_idx = 0;

   /* We need to split loads in 16byte chunks because that's the
    * granularity of cBufferLoadLegacy().
    */
   for (unsigned i = 0; i < num_bits; i += (16 * 8)) {
      /* For each 16byte chunk (or smaller) we generate a 32bit ubo vec
       * load.
       */
      unsigned subload_num_bits = MIN2(num_bits - i, 16 * 8);
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_load_ubo_dxil);

      load->num_components = 4;
      load->src[0] = nir_src_for_ssa(buffer);
      load->src[1] = nir_src_for_ssa(nir_iadd(b, idx, nir_imm_int(b, i / (16 * 8))));
      nir_ssa_dest_init(&load->instr, &load->dest, load->num_components,
                        32, NULL);
      nir_builder_instr_insert(b, &load->instr);

      nir_ssa_def *vec32 = &load->dest.ssa;

      /* First re-arrange the vec32 to account for intra 16-byte offset. */
      vec32 = ubo_load_select_32b_comps(b, vec32, offset, subload_num_bits / 8);

      /* If we have 2 bytes or less to load we need to adjust the u32 value so
       * we can always extract the LSB.
       */
      if (subload_num_bits <= 16) {
         nir_ssa_def *shift = nir_imul(b, nir_iand(b, offset,
                                                      nir_imm_int(b, 3)),
                                          nir_imm_int(b, 8));
         vec32 = nir_ushr(b, vec32, shift);
      }

      /* And now comes the pack/unpack step to match the original type. */
      extract_comps_from_vec32(b, vec32, bit_size, &comps[comp_idx],
                               subload_num_bits / bit_size);
      comp_idx += subload_num_bits / bit_size;
   }

   assert(comp_idx == num_components);
   return nir_vec(b, comps, num_components);
}

static bool
lower_store_deref(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   if (var->data.mode != nir_var_function_temp &&
       var->data.mode != nir_var_shader_temp)
      return false;
   nir_src val = intr->src[1];
   nir_ssa_def *ptr = nir_build_deref_offset(b, deref, cl_type_size_align);
   nir_ssa_def *offset = nir_iand(b, ptr, nir_inot(b, nir_imm_int(b, 3)));
   unsigned store_size = 32;

   assert(val.is_ssa);
   unsigned num_components = nir_src_num_components(val);
   unsigned bit_size = nir_src_bit_size(val);
   unsigned num_bits = num_components * bit_size;
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];
   unsigned comp_idx = 0;

   for (unsigned i = 0; i < num_components; i++)
      comps[i] = nir_channel(b, val.ssa, i);

   nir_deref_path path;
   nir_deref_path_init(&path, deref, NULL);
   /* Split stores into bitsize components */
   for (unsigned i = 0; i < num_bits; i += store_size) {
      unsigned substore_num_bits = MIN2(num_bits - i, store_size);
      nir_ssa_def *local_offset =
         nir_ishr(b, nir_iadd(b, offset, nir_imm_int(b, i / 8)),
                  nir_imm_int(b, 2 /* log2(32 / 8) */));
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

         nir_intrinsic_instr *load =
            nir_intrinsic_instr_create(b->shader, nir_intrinsic_load_ptr_dxil);
         load->num_components = 1;
         load->src[0] = nir_src_for_ssa(&path.path[0]->dest.ssa);
         load->src[1] = nir_src_for_ssa(local_offset);
         nir_ssa_dest_init(&load->instr, &load->dest, load->num_components, 32,
                           NULL);
         nir_builder_instr_insert(b, &load->instr);
         vec32 = nir_ior(b, vec32,
                        nir_iand(b, &load->dest.ssa, nir_inot(b, mask)));
      }

      store = nir_intrinsic_instr_create(b->shader,
                                       nir_intrinsic_store_ptr_dxil);
      store->src[0] = nir_src_for_ssa(vec32);
      store->src[1] = nir_src_for_ssa(&path.path[0]->dest.ssa);
      store->src[2] = nir_src_for_ssa(local_offset);


      store->num_components = 1;
      nir_builder_instr_insert(b, &store->instr);
      comp_idx += substore_num_bits / bit_size;
   }

   nir_deref_path_finish(&path);
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_load_global_impl(nir_builder *b, nir_intrinsic_instr *intr,
                       nir_ssa_def *buffer, nir_ssa_def *offset)
{
   assert(intr->dest.is_ssa);
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   unsigned num_components = nir_dest_num_components(intr->dest);
   unsigned num_bits = num_components * bit_size;

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
         nir_ssa_def *shift = nir_imul(b, nir_iand(b, offset, nir_imm_int(b, 3)),
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
   return true;
}

static bool
lower_load_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   /* source 'pointer' */
   assert(intr->src[0].is_ssa);
   nir_ssa_def *ptr = intr->src[0].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   return lower_load_global_impl(b, intr, buffer, offset);
}

static bool
lower_load_ssbo(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->dest.is_ssa);
   assert(intr->src[0].is_ssa);
   assert(intr->src[1].is_ssa);

   nir_ssa_def *buffer = nir_isub(b, intr->src[0].ssa, nir_imm_int(b, 1));
   nir_ssa_def *offset = intr->src[1].ssa;
   return lower_load_global_impl(b, intr, buffer, offset);
}

static bool
lower_store_global_impl(nir_builder *b, nir_intrinsic_instr *intr, nir_ssa_def *val,
                        nir_ssa_def *buffer, nir_ssa_def *offset)
{
   unsigned bit_size = val->bit_size;
   unsigned num_components = val->num_components;
   unsigned num_bits = num_components * bit_size;

   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];
   unsigned comp_idx = 0;

   for (unsigned i = 0; i < num_components; i++)
      comps[i] = nir_channel(b, val, i);

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
            nir_ssa_def *pos = nir_iand(b, offset, nir_imm_int(b, 3));
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
   return true;
}

static bool
lower_store_global(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->src[0].is_ssa);

   /* source 'pointer' */
   assert(intr->src[1].is_ssa);
   nir_ssa_def *ptr = intr->src[1].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   return lower_store_global_impl(b, intr, intr->src[0].ssa, buffer, offset);
}

static bool
lower_store_ssbo(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->src[0].is_ssa);
   assert(intr->src[1].is_ssa);
   assert(intr->src[2].is_ssa);

   nir_ssa_def *buffer = nir_isub(b, intr->src[1].ssa, nir_imm_int(b, 1));
   nir_ssa_def *offset = intr->src[2].ssa;

   return lower_store_global_impl(b, intr, intr->src[0].ssa, buffer, offset);
}

static nir_ssa_def *
lower_load_shared_vec32(nir_builder *b, nir_ssa_def *index, unsigned num_comps)
{
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];

   for (unsigned i = 0; i < num_comps; i++) {
      nir_intrinsic_instr *load =
         nir_intrinsic_instr_create(b->shader, nir_intrinsic_load_shared_dxil);

      load->num_components = 1;
      load->src[0] = nir_src_for_ssa(nir_iadd(b, index, nir_imm_int(b, i)));
      nir_ssa_dest_init(&load->instr, &load->dest, 1, 32, NULL);
      nir_builder_instr_insert(b, &load->instr);
      comps[i] = &load->dest.ssa;
   }

   return nir_vec(b, comps, num_comps);
}

static bool
lower_load_shared(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->dest.is_ssa);
   unsigned bit_size = nir_dest_bit_size(intr->dest);
   unsigned num_components = nir_dest_num_components(intr->dest);
   unsigned num_bits = num_components * bit_size;

   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->src[0].is_ssa);
   nir_ssa_def *offset =
      nir_iadd(b, intr->src[0].ssa, nir_imm_int(b, nir_intrinsic_base(intr)));
   nir_ssa_def *index = nir_ushr(b, offset, nir_imm_int(b, 2));
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];

   /* We need to split loads in 32-bit accesses because the sharedvars buffer
    * is an i32 array and DXIL does not support type casts on shared memory.
    */
   nir_ssa_def *vec32 =
      lower_load_shared_vec32(b, index, DIV_ROUND_UP(num_bits, 32));

   /* If we have 16 bits or less to load we need to adjust the u32 value so
    * we can always extract the LSB.
    */
   if (num_bits <= 16) {
      nir_ssa_def *shift =
         nir_imul(b, nir_iand(b, offset, nir_imm_int(b, 3)),
                     nir_imm_int(b, 8));
      vec32 = nir_ushr(b, vec32, shift);
   }

   /* And now comes the pack/unpack step to match the original type. */
   extract_comps_from_vec32(b, vec32, bit_size, comps, num_components);

   nir_ssa_def *result = nir_vec(b, comps, num_components);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(result));
   nir_instr_remove(&intr->instr);

   return true;
}

static void
lower_store_shared_vec32(nir_builder *b, nir_ssa_def *index, nir_ssa_def *vec32)
{

   for (unsigned i = 0; i < vec32->num_components; i++) {
      nir_intrinsic_instr *store =
         nir_intrinsic_instr_create(b->shader, nir_intrinsic_store_shared_dxil);

      store->src[0] = nir_src_for_ssa(nir_channel(b, vec32, i));
      store->src[1] = nir_src_for_ssa(nir_iadd(b, index, nir_imm_int(b, i)));
      store->num_components = 1;
      nir_builder_instr_insert(b, &store->instr);
   }
}

static bool
lower_store_shared(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->src[0].is_ssa);
   unsigned num_components = nir_src_num_components(intr->src[0]);
   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   unsigned num_bits = num_components * bit_size;

   b->cursor = nir_before_instr(&intr->instr);

   nir_ssa_def *offset =
      nir_iadd(b, intr->src[1].ssa, nir_imm_int(b, nir_intrinsic_base(intr)));
   nir_ssa_def *index = nir_ushr(b, offset, nir_imm_int(b, 2));
   nir_ssa_def *comps[NIR_MAX_VEC_COMPONENTS];

   for (unsigned i = 0; i < num_components; i++)
      comps[i] = nir_channel(b, intr->src[0].ssa, i);

   nir_ssa_def *vec32 = load_comps_to_vec32(b, bit_size, comps, num_components);

   /* For anything less than 32bits we need to use the masked version of the
    * intrinsic to preserve data living in the same 32bit slot.
    */
   if (num_bits < 32) {
      nir_ssa_def *mask = nir_imm_int(b, (1 << num_bits) - 1);

      /* If we have 16 bits or less to store we need to place them correctly in
       * the u32 component. Anything greater than 16 bits (including uchar3) is
       * naturally aligned on 32bits.
       */
      if (num_bits <= 16) {
         nir_ssa_def *shift =
            nir_imul_imm(b, nir_iand(b, offset, nir_imm_int(b, 3)), 8);

         vec32 = nir_ishl(b, vec32, shift);
         mask = nir_ishl(b, mask, shift);
      }

      nir_intrinsic_instr *store =
         nir_intrinsic_instr_create(b->shader,
                                    nir_intrinsic_store_shared_masked_dxil);
      store->src[0] = nir_src_for_ssa(vec32);
      store->src[1] = nir_src_for_ssa(nir_inot(b, mask));
      store->src[2] = nir_src_for_ssa(index);
      store->num_components = 1;
      nir_builder_instr_insert(b, &store->instr);
   } else {
      lower_store_shared_vec32(b, index, vec32);
   }

   nir_instr_remove(&intr->instr);

   return true;
}

struct dxil_ubo_deref {
   bool chain_has_deref_cast;
};

struct dxil_ubo_to_temp_ctx {
   struct hash_table *ubos;
};

static inline nir_variable *
dxil_nir_deref_instr_get_variable(const nir_deref_instr *instr,
                                  bool *has_deref_cast)
{
   *has_deref_cast = false;
   while (instr->deref_type != nir_deref_type_var) {
      if (instr->deref_type == nir_deref_type_cast)
         *has_deref_cast = true;

      instr = nir_deref_instr_parent(instr);
      if (!instr)
         return NULL;
   }

   return instr->var;
}

static bool
dxil_nir_deref_instr_patch_variable(nir_deref_instr *instr,
                                    struct hash_table *ubo_to_temp)
{
   nir_deref_instr *parent;
   bool patched = false;

   if (instr->deref_type == nir_deref_type_var) {
      struct hash_entry *he = _mesa_hash_table_search(ubo_to_temp, instr->var);
      const nir_variable *var = he ? he->data : NULL;

      if (!var)
         return false;

      instr->mode = var->data.mode;
      return true;
   }

   parent = nir_deref_instr_parent(instr);
   if (!parent || !dxil_nir_deref_instr_patch_variable(parent, ubo_to_temp))
      return false;

   instr->mode = parent->mode;
   return true;
}

bool
dxil_nir_lower_ubo_to_temp(nir_shader *nir)
{
   struct hash_table *ubo_to_temp = _mesa_pointer_hash_table_create(NULL);
   bool progress = false;

   /* First pass: collect all UBO accesses that could be turned into
    * shader temp accesses.
    */
   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;
            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

            if (intr->intrinsic != nir_intrinsic_load_deref)
               continue;

            nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
            nir_variable_mode mode = deref->mode;

            if (mode != nir_var_mem_ubo)
               continue;

            bool has_deref_cast;
            nir_variable *var =
               dxil_nir_deref_instr_get_variable(deref, &has_deref_cast);

            if (!var || !var->constant_initializer)
               continue;

            struct hash_entry *he = _mesa_hash_table_search(ubo_to_temp, var);
            if (he && has_deref_cast) {
               /* As soon as we have one deref_cast, we should avoid turning the
                * UBO into a shader temp with constant initializer.
                */
                he->data = NULL;
            } else if (!he) {
                _mesa_hash_table_insert(ubo_to_temp, var,
                                        has_deref_cast ? NULL : var);
            }
         }
      }
   }

   hash_table_foreach(ubo_to_temp, he) {
      nir_variable *var = he->data;

      if (!var)
         continue;

      /* Move the variable to the globals list. */
      var->data.mode = nir_var_shader_temp;
      exec_node_remove(&var->node);
      nir_shader_add_variable(nir, var);
   }

   /* Second pass: patch all derefs that were accessing the converted UBOs
    * variables.
    */
   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_intrinsic)
               continue;
            nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

            if (intr->intrinsic != nir_intrinsic_load_deref)
               continue;

            nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
            nir_variable_mode mode = deref->mode;

            if (mode != nir_var_mem_ubo)
               continue;

            progress |= dxil_nir_deref_instr_patch_variable(deref, ubo_to_temp);
         }
      }
   }

   _mesa_hash_table_destroy(ubo_to_temp, NULL);
   return progress;
}

static bool
lower_kernel_input_offset_bit_size(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->src[0].is_ssa);
   if (intr->src[0].ssa->parent_instr->type != nir_instr_type_load_const)
      return false;

   nir_load_const_instr *load = nir_instr_as_load_const(intr->src[0].ssa->parent_instr);
   if (load->def.bit_size <= 32)
      return false;

   nir_const_value *value = load->value;
   if (value->u64 > 0xffffffff)
      return false;

   value->u32 = (unsigned)value->u64;
   load->def.bit_size = 32;
   return true;
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
            case nir_intrinsic_load_deref:
               progress |= lower_load_deref(&b, intr);
               break;
            case nir_intrinsic_load_global:
               progress |= lower_load_global(&b, intr);
               break;
            case nir_intrinsic_load_shared:
               progress |= lower_load_shared(&b, intr);
               break;
            case nir_intrinsic_load_ssbo:
               progress |= lower_load_ssbo(&b, intr);
               break;
            case nir_intrinsic_store_deref:
               progress |= lower_store_deref(&b, intr);
               break;
            case nir_intrinsic_store_global:
               progress |= lower_store_global(&b, intr);
               break;
            case nir_intrinsic_store_shared:
               progress |= lower_store_shared(&b, intr);
               break;
            case nir_intrinsic_store_ssbo:
               progress |= lower_store_ssbo(&b, intr);
               break;
            }
         }
      }
   }

   return progress;
}

static bool
lower_global_atomic(nir_builder *b, nir_intrinsic_instr *intr,
                    nir_intrinsic_op dxil_op)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->src[0].is_ssa);
   nir_ssa_def *ptr = intr->src[0].ssa;
   nir_ssa_def *buffer = ptr_to_buffer(b, ptr);
   nir_ssa_def *offset = ptr_to_offset(b, ptr);

   nir_intrinsic_instr *atomic = nir_intrinsic_instr_create(b->shader, dxil_op);
   atomic->src[0] = nir_src_for_ssa(buffer);
   atomic->src[1] = nir_src_for_ssa(offset);
   assert(intr->src[1].is_ssa);
   atomic->src[2] = nir_src_for_ssa(intr->src[1].ssa);
   if (dxil_op == nir_intrinsic_global_atomic_comp_swap_dxil) {
      assert(intr->src[2].is_ssa);
      atomic->src[3] = nir_src_for_ssa(intr->src[2].ssa);
   }
   atomic->num_components = 1;
   nir_ssa_dest_init(&atomic->instr, &atomic->dest, 1, 32, intr->dest.ssa.name);

   nir_builder_instr_insert(b, &atomic->instr);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(&atomic->dest.ssa));
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_ssbo_atomic(nir_builder *b, nir_intrinsic_instr *intr,
                  nir_intrinsic_op dxil_op)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->src[0].is_ssa);
   assert(intr->src[1].is_ssa);
   nir_ssa_def *buffer = nir_isub(b, intr->src[0].ssa, nir_imm_int(b, 1));
   nir_ssa_def *offset = intr->src[1].ssa;

   nir_intrinsic_instr *atomic = nir_intrinsic_instr_create(b->shader, dxil_op);
   atomic->src[0] = nir_src_for_ssa(buffer);
   atomic->src[1] = nir_src_for_ssa(offset);
   assert(intr->src[2].is_ssa);
   atomic->src[2] = nir_src_for_ssa(intr->src[2].ssa);
   if (dxil_op == nir_intrinsic_global_atomic_comp_swap_dxil) {
      assert(intr->src[3].is_ssa);
      atomic->src[3] = nir_src_for_ssa(intr->src[3].ssa);
   }
   atomic->num_components = 1;
   nir_ssa_dest_init(&atomic->instr, &atomic->dest, 1, 32, intr->dest.ssa.name);

   nir_builder_instr_insert(b, &atomic->instr);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(&atomic->dest.ssa));
   nir_instr_remove(&intr->instr);
   return true;
}

static bool
lower_shared_atomic(nir_builder *b, nir_intrinsic_instr *intr,
                    nir_intrinsic_op dxil_op)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(intr->src[0].is_ssa);
   nir_ssa_def *offset =
      nir_iadd(b, intr->src[0].ssa, nir_imm_int(b, nir_intrinsic_base(intr)));
   nir_ssa_def *index = nir_ushr(b, offset, nir_imm_int(b, 2));

   nir_intrinsic_instr *atomic = nir_intrinsic_instr_create(b->shader, dxil_op);
   atomic->src[0] = nir_src_for_ssa(index);
   assert(intr->src[1].is_ssa);
   atomic->src[1] = nir_src_for_ssa(intr->src[1].ssa);
   if (dxil_op == nir_intrinsic_shared_atomic_comp_swap_dxil) {
      assert(intr->src[2].is_ssa);
      atomic->src[2] = nir_src_for_ssa(intr->src[2].ssa);
   }
   atomic->num_components = 1;
   nir_ssa_dest_init(&atomic->instr, &atomic->dest, 1, 32, intr->dest.ssa.name);

   nir_builder_instr_insert(b, &atomic->instr);
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(&atomic->dest.ssa));
   nir_instr_remove(&intr->instr);
   return true;
}

bool
dxil_nir_lower_atomics_to_dxil(nir_shader *nir)
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

#define ATOMIC(op)                                                            \
  case nir_intrinsic_global_atomic_##op:                                     \
     progress |= lower_global_atomic(&b, intr,                                \
                                     nir_intrinsic_global_atomic_##op##_dxil); \
     break;                                                                   \
  case nir_intrinsic_shared_atomic_##op:                                     \
     progress |= lower_shared_atomic(&b, intr,                                \
                                     nir_intrinsic_shared_atomic_##op##_dxil); \
     break;                                                                  \
  case nir_intrinsic_ssbo_atomic_##op:                                       \
     progress |= lower_ssbo_atomic(&b, intr,                                 \
                                   nir_intrinsic_global_atomic_##op##_dxil); \
     break

            ATOMIC(add);
            ATOMIC(imin);
            ATOMIC(umin);
            ATOMIC(imax);
            ATOMIC(umax);
            ATOMIC(and);
            ATOMIC(or);
            ATOMIC(xor);
            ATOMIC(exchange);
            ATOMIC(comp_swap);

#undef ATOMIC
            }
         }
      }
   }

   return progress;
}

static void
update_deref_mode(nir_deref_instr *deref, nir_variable_mode mode)
{
   deref->mode = mode;

   nir_foreach_use_safe(src, &deref->dest.ssa) {
      assert(src->is_ssa);
      if (src->parent_instr->type != nir_instr_type_deref)
         continue;

      nir_deref_instr *sub_deref = nir_instr_as_deref(src->parent_instr);
      if (!sub_deref)
         continue;

      update_deref_mode(sub_deref, mode);
   }
}

static bool
lower_deref_ssbo(nir_builder *b, nir_deref_instr *deref)
{
   assert(deref->mode == nir_var_mem_ssbo);
   assert(deref->deref_type == nir_deref_type_var ||
          deref->deref_type == nir_deref_type_cast);
   nir_variable *var = deref->var;

   b->cursor = nir_before_instr(&deref->instr);

   if (deref->deref_type == nir_deref_type_var) {
      /* We turn all deref_var into deref_cast and build a pointer value based on
       * the var binding which encodes the UAV id.
       */
      nir_ssa_def *ptr = nir_ishl(b, nir_imm_int(b, var->data.binding + 1),
                                  nir_imm_int(b, 28));
      nir_deref_instr *deref_cast =
         nir_build_deref_cast(b, ptr, nir_var_mem_global, deref->type,
                              glsl_get_explicit_stride(var->type));
      nir_ssa_def_rewrite_uses(&deref->dest.ssa,
                               nir_src_for_ssa(&deref_cast->dest.ssa));
      nir_instr_remove(&deref->instr);

      deref = deref_cast;
   }

   update_deref_mode(deref, nir_var_mem_global);
   return true;
}

bool
dxil_nir_lower_deref_ssbo(nir_shader *nir)
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
            if (instr->type != nir_instr_type_deref)
               continue;

            nir_deref_instr *deref = nir_instr_as_deref(instr);

            if (deref->mode != nir_var_mem_ssbo ||
                (deref->deref_type != nir_deref_type_var &&
                 deref->deref_type != nir_deref_type_cast))
               continue;

            progress |= lower_deref_ssbo(&b, deref);
         }
      }
   }

   return progress;
}

static bool
lower_alu_deref_srcs(nir_builder *b, nir_alu_instr *alu)
{
   const nir_op_info *info = &nir_op_infos[alu->op];
   bool progress = false;

   b->cursor = nir_before_instr(&alu->instr);

   for (unsigned i = 0; i < info->num_inputs; i++) {
      nir_deref_instr *deref = nir_src_as_deref(alu->src[i].src);

      if (!deref)
         continue;

      nir_deref_path path;
      nir_deref_path_init(&path, deref, NULL);
      nir_deref_instr *root_deref = path.path[0];
      nir_deref_path_finish(&path);

      if (root_deref->deref_type != nir_deref_type_cast)
         continue;

      nir_ssa_def *ptr =
         nir_iadd(b, root_deref->parent.ssa,
                     nir_build_deref_offset(b, deref, cl_type_size_align));
      nir_instr_rewrite_src(&alu->instr, &alu->src[i].src, nir_src_for_ssa(ptr));
      progress = true;
   }

   return progress;
}

bool
dxil_nir_opt_alu_deref_srcs(nir_shader *nir)
{
   bool progress = false;

   foreach_list_typed(nir_function, func, node, &nir->functions) {
      if (!func->is_entrypoint)
         continue;
      assert(func->impl);

      bool progress = false;
      nir_builder b;
      nir_builder_init(&b, func->impl);

      nir_foreach_block(block, func->impl) {
         nir_foreach_instr_safe(instr, block) {
            if (instr->type != nir_instr_type_alu)
               continue;

            nir_alu_instr *alu = nir_instr_as_alu(instr);
            progress |= lower_alu_deref_srcs(&b, alu);
         }
      }
   }

   return progress;
}

static bool
lower_load_kernel_input(nir_builder *b, nir_intrinsic_instr *intr,
                        nir_variable *var)
{
   nir_intrinsic_instr *load;

   b->cursor = nir_before_instr(&intr->instr);

   nir_ssa_def *result =
      emit_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                         intr->src[0].ssa,
                         nir_dest_num_components(intr->dest),
                         nir_dest_bit_size(intr->dest));
   nir_ssa_def_rewrite_uses(&intr->dest.ssa, nir_src_for_ssa(result));
   nir_instr_remove(&intr->instr);
   return true;
}

bool
dxil_nir_lower_kernel_input_loads(nir_shader *nir, nir_variable *var)
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

            if (intr->intrinsic == nir_intrinsic_load_kernel_input) {
               progress |= lower_kernel_input_offset_bit_size(&b, intr);
               progress |= lower_load_kernel_input(&b, intr, var);
            }
         }
      }
   }

   return progress;
}

static bool
lower_load_global_invocation_id(nir_builder *b, nir_intrinsic_instr *intr,
                                nir_variable *var)
{
   nir_intrinsic_instr *load;

   b->cursor = nir_after_instr(&intr->instr);

   nir_ssa_def *offset =
      emit_load_ubo_dxil(b, nir_imm_int(b, var->data.binding),
                         nir_imm_int(b, 0),
                         nir_dest_num_components(intr->dest),
                         nir_dest_bit_size(intr->dest));
   nir_ssa_def *result = nir_iadd(b, &intr->dest.ssa, offset);
   nir_ssa_def_rewrite_uses_after(&intr->dest.ssa, nir_src_for_ssa(result),
                                  result->parent_instr);
   return true;
}

/* Make sure you only call this lowering pass once. */
bool
dxil_nir_lower_kernel_global_work_offset(nir_shader *nir, nir_variable *var)
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

            if (intr->intrinsic == nir_intrinsic_load_global_invocation_id)
               progress |= lower_load_global_invocation_id(&b, intr, var);
         }
      }
   }

   return progress;
}
