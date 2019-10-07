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

#include "brw_nir.h"
#include "compiler/nir/nir_builder.h"
#include "compiler/nir/nir_format_convert.h"

#include "genxml/genX_bits.h"

static bool
lower_buffer_tex_to_2d_array_impl(nir_function_impl *impl,
                                  const struct gen_device_info *devinfo)
{
   nir_builder b;
   nir_builder_init(&b, impl);
   bool progress = false;

   nir_foreach_block(block, impl) {
      nir_foreach_instr_safe(instr, block) {
         if (instr->type != nir_instr_type_tex)
            continue;

         nir_tex_instr *tex = nir_instr_as_tex(instr);
         if (tex->sampler_dim != GLSL_SAMPLER_DIM_BUF)
            continue;

         nir_intrinsic_op size_op = 0;
         nir_ssa_def *texture = NULL;
         for (unsigned i = 0; i < tex->num_srcs; i++) {
            switch (tex->src[i].src_type) {
            case nir_tex_src_texture_deref:
               assert(tex->src[i].src.is_ssa);
               texture = tex->src[i].src.ssa;
               size_op = nir_intrinsic_texture_buffer_size_deref_intel;
               break;

            case nir_tex_src_texture_offset:
            case nir_tex_src_texture_handle:
               unreachable("Lowering buffer textures to 2D arrays is "
                           "currently only implemented for derefs");

            default:
               continue;
            }

            /* Break in the switch should also break the loop */
            break;
         }
         assert(texture != NULL);

         b.cursor = nir_before_instr(&tex->instr);

         nir_intrinsic_instr *size_intrin =
            nir_intrinsic_instr_create(b.shader, size_op);
         size_intrin->src[0] = nir_src_for_ssa(texture);
         nir_ssa_dest_init(&size_intrin->instr, &size_intrin->dest,
                           1, 32, NULL);
         nir_builder_instr_insert(&b, &size_intrin->instr);

         switch (tex->op) {
         case nir_texop_txf: {
            int coord_idx = nir_tex_instr_src_index(tex, nir_tex_src_coord);
            assert(coord_idx >= 0);

            assert(tex->src[coord_idx].src.is_ssa);
            nir_ssa_def *coord = tex->src[coord_idx].src.ssa;
            assert(coord->num_components == 1);

            /* If it's out-of-bounds, set it to UINT32_MAX to ensure it's
             * really out-of-bounds when we make a 3D coordinate out of it.
             */
            coord = nir_bcsel(&b, nir_ult(&b, coord, &size_intrin->dest.ssa),
                              coord, nir_imm_int(&b, 0xffffffff));

            unsigned bits[3] = {
               RENDER_SURFACE_STATE_Width_bits(devinfo),
               RENDER_SURFACE_STATE_Height_bits(devinfo),
            };
            bits[2] = 32 - (bits[0] + bits[1]);
            coord = nir_format_unpack_int(&b, coord, bits, 3, false);

            tex->sampler_dim = GLSL_SAMPLER_DIM_2D;
            tex->coord_components = 3;
            tex->is_array = true;
            nir_instr_rewrite_src(&tex->instr, &tex->src[coord_idx].src,
                                  nir_src_for_ssa(coord));
            progress = true;
            break;
         }

         case nir_texop_txs:
            assert(tex->dest.ssa.num_components == 1);
            nir_ssa_def_rewrite_uses(&tex->dest.ssa,
                                     nir_src_for_ssa(&size_intrin->dest.ssa));
            nir_instr_remove(&tex->instr);
            progress = true;
            break;

         default:
            unreachable("Invalid texture op on a buffer texture");
         }
      }
   }

   return progress;
}

bool
brw_nir_lower_buffer_tex_to_2d_array(nir_shader *shader,
                                     const struct gen_device_info *devinfo)
{
   bool progress = false;

   nir_foreach_function(function, shader) {
      if (function->impl)
         progress |= lower_buffer_tex_to_2d_array_impl(function->impl, devinfo);
   }

   return progress;
}
