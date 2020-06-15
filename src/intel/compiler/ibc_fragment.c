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
#include "ibc_compile.h"
#include "ibc_nir.h"
#include "dev/gen_debug.h"

struct ibc_fs_payload {
   struct ibc_payload_base base;

   ibc_ref pixel[2];
   ibc_ref barycentric[BRW_BARYCENTRIC_MODE_COUNT];
   ibc_ref src_z;
   ibc_ref src_w;
   ibc_ref sample_pos[2];
   ibc_ref sample_mask_in;

   ibc_ref inputs[32][4];
};

struct nir_fs_to_ibc_state {
   struct {
      ibc_ref color[BRW_MAX_DRAW_BUFFERS];
      ibc_ref dual_src_color;
      ibc_ref depth;
      ibc_ref stencil;
      ibc_ref sample_mask;
   } out;

   unsigned max_simd_width;
   const char *simd_restrict_reason;

   /* Mask of which pixels are live.  Used for discard */
   ibc_ref live_pix;
   struct list_head halt_jumps;
};

static void
ibc_fs_limit_dispatch_width(struct nir_to_ibc_state *nti,
                            unsigned max_width, const char *msg)
{
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;
   if (max_width < nti_fs->max_simd_width) {
      nti_fs->max_simd_width = max_width;
      nti_fs->simd_restrict_reason = msg;
   }
}

static void
set_ref_or_zip(ibc_builder *b, ibc_ref *dest, ibc_ref src,
               unsigned num_comps)
{
   if (dest->file == IBC_FILE_NONE) {
      *dest = src;
   } else {
      *dest = ibc_SIMD_ZIP2(b, *dest, src, num_comps);
   }
}

static void
ibc_load_fs_payload_reg(ibc_builder *b, ibc_ref *dest,
                        unsigned simd_group, unsigned *reg,
                        enum ibc_type type)
{
   ibc_builder_push_group(b, simd_group, MIN2(16, b->simd_width));
   ibc_ref tmp = ibc_load_payload_logical(b, reg, type, 1, false);
   ibc_builder_pop(b);
   set_ref_or_zip(b, dest, tmp, 1);
}

static void
ibc_load_fs_barycentric(ibc_builder *b, ibc_ref *dest,
                        unsigned simd_group, unsigned *reg)
{
   /* The PLN instruction is silly.  In SIMD16 mode, it doesn't act on a
    * SIMD16 vec2 as would make sense.  Instead, it acts on two SIMD8 vec2s
    * sequentially in registers.  The barycentrics come into the shader in a
    * way that makes them convenient for plane but it means we actually get
    * the one SIMD8 vec2 at a time.
    */
   ibc_ref bary = {};
   ibc_builder_push_group(b, simd_group, MIN2(16, b->simd_width));
   for (unsigned int g = 0; g < b->simd_width; g += 8) {
      ibc_builder_push_group(b, g, 8);
      ibc_ref bary8 = ibc_load_payload_logical(b, reg, IBC_TYPE_F, 2, false);
      ibc_builder_pop(b);
      set_ref_or_zip(b, &bary, bary8, 2);
   }
   ibc_builder_pop(b);
   set_ref_or_zip(b, dest, bary, 2);
}

static struct ibc_fs_payload *
ibc_setup_fs_payload(ibc_builder *b, struct brw_wm_prog_data *prog_data,
                     void *mem_ctx)
{
   struct ibc_fs_payload *payload = rzalloc(mem_ctx, struct ibc_fs_payload);
   ibc_setup_payload_base(b, &payload->base);

   unsigned reg = payload->base.num_ff_regs;
   for (unsigned g = 0; g < b->shader->simd_width; g += 16)
      payload->pixel[g / 16] = ibc_load_payload_reg(b, &reg, true);

   for (unsigned g = 0; g < b->shader->simd_width; g += 16) {
      for (int i = 0; i < BRW_BARYCENTRIC_MODE_COUNT; ++i) {
         if (prog_data->barycentric_interp_modes & (1 << i))
            ibc_load_fs_barycentric(b, &payload->barycentric[i], g, &reg);
      }

      /* R27-28: interpolated depth if uses source depth */
      if (prog_data->uses_src_depth)
         ibc_load_fs_payload_reg(b, &payload->src_z, g, &reg, IBC_TYPE_F);

      /* R27-28: interpolated depth if uses source depth */
      if (prog_data->uses_src_w)
         ibc_load_fs_payload_reg(b, &payload->src_w, g, &reg, IBC_TYPE_F);

      /* R31: MSAA position offsets. */
      if (prog_data->uses_pos_offset)
         payload->sample_pos[g / 16] = ibc_load_payload_reg(b, &reg, false);

      /* R32-33: MSAA input coverage mask */
      if (prog_data->uses_sample_mask)
         ibc_load_fs_payload_reg(b, &payload->sample_mask_in, g,
                                 &reg, IBC_TYPE_UD);
   }

   payload->base.num_ff_regs = reg;

   /* Set up push constants */
   ibc_setup_curb_payload(b, &payload->base, &prog_data->base);
   reg = payload->base.num_ff_regs + payload->base.num_curb_regs;

   /* We represent per-vertex attributes in the payload as a SIMD1 vec4 for
    * each component coming out of the geometry pipeline.
    */
   ibc_builder_push_scalar(b);
   for (unsigned i = 0; i < prog_data->num_varying_inputs; i++) {
      for (unsigned c = 0; c < 4; c++) {
         ibc_ref grf = ibc_hw_grf_ref(reg + (c / 2), (c % 2) * 4, IBC_TYPE_UD);
         payload->inputs[i][c] =
            ibc_builder_new_logical_reg(b, IBC_TYPE_UD, 4);
         ibc_load_payload(b, payload->inputs[i][c], grf, 4, false);
      }
      reg += 2;
   }
   ibc_builder_pop(b);

   return payload;
}

static ibc_ref
ibc_frag_coord(struct nir_to_ibc_state *nti)
{
   struct ibc_fs_payload *payload = (struct ibc_fs_payload *)nti->payload;
   ibc_builder *b = &nti->b;

   assert(b->simd_group == 0);

   ibc_ref frag_coord[4] = {};
   for (unsigned g = 0; g < b->simd_width; g += 16) {
      ibc_builder_push_group(b, g, MIN2(b->simd_width, 16));

      /* The "Register Region Restrictions" page says for BDW (and newer,
       * presumably):
       *
       *     "When destination spans two registers, the source may be one or
       *      two registers. The destination elements must be evenly split
       *      between the two registers."
       *
       * Thus we can do a single add(16) in SIMD8 or an add(32) in SIMD16
       * to compute our pixel centers.
       */
      ibc_reg *tmp_grf =
         ibc_hw_grf_reg_create(b->shader, b->simd_width * 4, REG_SIZE);

      ibc_builder_push_we_all(b, b->simd_width * 2);
      ibc_ref add_src = ibc_restride(b, payload->pixel[g / 16],
                                     IBC_TYPE_UW, 4, 1, 4, 0);
      ibc_build_alu2(b, IBC_ALU_OP_ADD, ibc_typed_ref(tmp_grf, IBC_TYPE_UW),
                     add_src, ibc_imm_v(0x11001010));
      ibc_builder_pop(b);

      /* Now we carefully move the X and Y bits into their proper channels */
      ibc_ref x = ibc_MOV(b, IBC_TYPE_F,
                          ibc_restride(b, ibc_typed_ref(tmp_grf, IBC_TYPE_UW),
                                       IBC_TYPE_UW, 0, 8, 4, 1));
      ibc_ref y = ibc_MOV(b, IBC_TYPE_F,
                          ibc_restride(b, ibc_typed_ref(tmp_grf, IBC_TYPE_UW),
                                       IBC_TYPE_UW, 4, 8, 4, 1));

      ibc_builder_pop(b);

      set_ref_or_zip(b, &frag_coord[0], x, 1);
      set_ref_or_zip(b, &frag_coord[1], y, 1);
   }

   frag_coord[2] = payload->src_z;
   frag_coord[3] = ibc_RCP(b, IBC_TYPE_F, payload->src_w);

   return ibc_VEC(b, frag_coord, 4);
}

static ibc_ref
ibc_sample_pos(struct nir_to_ibc_state *nti)
{
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct ibc_fs_payload *payload = (struct ibc_fs_payload *)nti->payload;
   ibc_builder *b = &nti->b;

   if (!prog_data->persample_dispatch) {
      /* From ARB_sample_shading specification:
       * "When rendering to a non-multisample buffer, or if multisample
       *  rasterization is disabled, gl_SamplePosition will always be
       *  (0.5, 0.5).
       */
      return ibc_VEC2(b, ibc_imm_f(0.5), ibc_imm_f(0.5));
   }

   ibc_ref sample_pos[2] = { };
   for (unsigned g = 0; g < b->simd_width; g += 16) {
      for (unsigned i = 0; i < 2; i++) {
         /* From the Ivy Bridge PRM, volume 2 part 1, page 344:
          * R31.1:0         Position Offset X/Y for Slot[3:0]
          * R31.3:2         Position Offset X/Y for Slot[7:4]
          * .....
          *
          * The X, Y sample positions come in as bytes in  thread payload. So,
          * read the positions using vstride=16, width=8, hstride=2.
          */
         ibc_ref pos_fixed = payload->sample_pos[g / 16];
         assert(pos_fixed.file == IBC_FILE_HW_GRF);
         pos_fixed.type = IBC_TYPE_B;
         pos_fixed.hw_grf.byte += i;
         pos_fixed.hw_grf.vstride = 16;
         pos_fixed.hw_grf.width = 8;
         pos_fixed.hw_grf.hstride = 2;

         ibc_builder_push_group(b, g, MIN2(b->simd_width, 16));
         ibc_ref pos_float =
            ibc_FMUL(b, IBC_TYPE_F, ibc_MOV(b, IBC_TYPE_F, pos_fixed),
                                    ibc_imm_f(1 / 16.0f));
         ibc_builder_pop(b);

         set_ref_or_zip(b, &sample_pos[i], pos_float, 1);
      }
   }

   return ibc_VEC(b, sample_pos, 2);
}

static ibc_ref
ibc_sample_id(struct nir_to_ibc_state *nti)
{
   const struct brw_wm_prog_key *key = (const void *)nti->key;
   struct ibc_fs_payload *payload = (struct ibc_fs_payload *)nti->payload;
   ibc_builder *b = &nti->b;

   if (!key->multisample_fbo) {
      /* As per GL_ARB_sample_shading specification:
       * "When rendering to a non-multisample buffer, or if multisample
       *  rasterization is disabled, gl_SampleID will always be zero."
       */
      return ibc_MOV(b, IBC_TYPE_UD, ibc_imm_uw(0));
   } else {
      /* Sample ID comes in as 4-bit numbers in g1.0:
       *
       *    15:12 Slot 3 SampleID (only used in SIMD16)
       *     11:8 Slot 2 SampleID (only used in SIMD16)
       *      7:4 Slot 1 SampleID
       *      3:0 Slot 0 SampleID
       *
       * Each slot corresponds to four channels, so we want to replicate
       * each half-byte value to 4 channels in a row:
       *
       *    dst+0:    .7    .6    .5    .4    .3    .2    .1    .0
       *             7:4   7:4   7:4   7:4   3:0   3:0   3:0   3:0
       *
       *    dst+1:    .7    .6    .5    .4    .3    .2    .1    .0  (if SIMD16)
       *           15:12 15:12 15:12 15:12  11:8  11:8  11:8  11:8
       *
       * First, we read g1.0 with a <1,8,0>UB region, causing the first 8
       * channels to read the first byte (7:0), and the second group of 8
       * channels to read the second byte (15:8).  Then, we shift right by
       * a vector immediate of <4, 4, 4, 4, 0, 0, 0, 0>, moving the slot
       * 1 / 3 values into place.  Finally, we AND with 0xf to keep the low
       * nibble.
       *
       *    shr(16) tmp<1>W g1.0<1,8,0>B 0x44440000:V
       *    and(16) dst<1>D tmp<8,8,1>W  0xf:W
       *
       * TODO: These payload bits exist on Gen7 too, but they appear to
       *       always be zero, so this code fails to work.  We should find
       *       out why.
       */
      assert(b->simd_group == 0);
      ibc_ref zip = { };
      for (unsigned g = 0; g < b->simd_width; g += 16) {
         ibc_builder_push_group(b, g, MIN2(b->simd_width, 16));
         ibc_ref strided = ibc_restride(b, payload->pixel[g / 16],
                                        IBC_TYPE_UB, 0, 1, 8, 0);
         ibc_ref sample_id = ibc_SHR(b, IBC_TYPE_UW,
                                     strided, ibc_imm_v(0x44440000));
         ibc_builder_pop(b);
         set_ref_or_zip(b, &zip, sample_id, 1);
      }
      return ibc_AND(b, IBC_TYPE_UD, zip, ibc_imm_uw(0xf));
   }
}

static ibc_ref
ibc_sample_mask_in(struct nir_to_ibc_state *nti)
{
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct ibc_fs_payload *payload = (struct ibc_fs_payload *)nti->payload;
   ibc_builder *b = &nti->b;

   if (prog_data->persample_dispatch) {
      /* gl_SampleMaskIn[] comes from two sources: the input coverage mask,
       * and a mask representing which sample is being processed by the
       * current shader invocation.
       *
       * From the OES_sample_variables specification:
       * "When per-sample shading is active due to the use of a fragment input
       *  qualified by "sample" or due to the use of the gl_SampleID or
       *  gl_SamplePosition variables, only the bit for the current sample is
       *  set in gl_SampleMaskIn."
       */
      ibc_ref enabled_mask =
         ibc_SHL(b, IBC_TYPE_UD, ibc_MOV(b, IBC_TYPE_UD, ibc_imm_ud(1)),
                                 ibc_sample_id(nti));
      return ibc_AND(b, IBC_TYPE_UD, payload->sample_mask_in, enabled_mask);
   } else {
      /* In per-pixel mode, the coverage mask is sufficient. */
      return payload->sample_mask_in;
   }
}

static ibc_ref
ibc_pixel_mask_as_flag(struct nir_to_ibc_state *nti)
{
   struct ibc_fs_payload *payload = (struct ibc_fs_payload *)nti->payload;
   ibc_builder *b = &nti->b;

   ibc_reg *live_pix_reg =
      ibc_flag_reg_create(b->shader, MAX2(b->shader->simd_width, 16));
   ibc_builder_push_scalar(b);
   for (unsigned g = 0; g < b->shader->simd_width; g += 16) {
      ibc_ref pix_mask = payload->pixel[g / 16];
      assert(pix_mask.file == IBC_FILE_HW_GRF);
      pix_mask.hw_grf.byte += 7 * sizeof(uint32_t);
      pix_mask.type = IBC_TYPE_UW;

      ibc_ref flag_w = ibc_typed_ref(live_pix_reg, IBC_TYPE_UW);
      flag_w.flag.bit += g;

      ibc_MOV_to(b, flag_w, pix_mask);
   }
   ibc_builder_pop(b);

   return ibc_typed_ref(live_pix_reg, IBC_TYPE_FLAG);
}

ibc_ref
ibc_emit_fs_sample_live_predicate(struct nir_to_ibc_state *nti)
{
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;

   if (prog_data->uses_kill) {
      assert(nti_fs->live_pix.file == IBC_FILE_FLAG);
      assert(nti_fs->live_pix.type == IBC_TYPE_FLAG);
      return nti_fs->live_pix;
   } else {
      return ibc_pixel_mask_as_flag(nti);
   }
}

static enum brw_barycentric_mode
brw_barycentric_mode(enum glsl_interp_mode mode, nir_intrinsic_op op)
{
   /* Barycentric modes don't make sense for flat inputs. */
   assert(mode != INTERP_MODE_FLAT);

   unsigned bary;
   switch (op) {
   case nir_intrinsic_load_barycentric_pixel:
   case nir_intrinsic_load_barycentric_at_offset:
      bary = BRW_BARYCENTRIC_PERSPECTIVE_PIXEL;
      break;
   case nir_intrinsic_load_barycentric_centroid:
      bary = BRW_BARYCENTRIC_PERSPECTIVE_CENTROID;
      break;
   case nir_intrinsic_load_barycentric_sample:
   case nir_intrinsic_load_barycentric_at_sample:
      bary = BRW_BARYCENTRIC_PERSPECTIVE_SAMPLE;
      break;
   default:
      unreachable("invalid intrinsic");
   }

   if (mode == INTERP_MODE_NOPERSPECTIVE)
      bary += 3;

   return (enum brw_barycentric_mode) bary;
}

bool
ibc_emit_nir_fs_intrinsic(struct nir_to_ibc_state *nti,
                          const nir_intrinsic_instr *instr)
{
   assert(nti->stage == MESA_SHADER_FRAGMENT);
   const struct brw_wm_prog_key *key = (const void *)nti->key;
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct ibc_fs_payload *payload = (struct ibc_fs_payload *)nti->payload;
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;
   ibc_builder *b = &nti->b;

   ibc_ref dest = { .file = IBC_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_frag_coord:
      assert(nir_dest_is_divergent(instr->dest));
      dest = ibc_frag_coord(nti);
      break;

   case nir_intrinsic_load_sample_pos:
      assert(nir_dest_is_divergent(instr->dest));
      dest = ibc_sample_pos(nti);
      break;

   case nir_intrinsic_load_front_face: {
      /* Bit 15 of g0.0 is 0 if the polygon is front facing. */
      ibc_ref g00 = ibc_typed_ref(b->shader->g0, IBC_TYPE_UW);
      ibc_hw_grf_mul_stride(&g00.hw_grf, 0);

      ibc_builder_push_scalar(b);
      ibc_ref and_srcs[2] = { g00, ibc_imm_uw(0x8000), };
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_FLAG, 1);
      ibc_build_alu(b, IBC_ALU_OP_AND, ibc_null(IBC_TYPE_UW),
                    dest, BRW_CONDITIONAL_Z, and_srcs, 2);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_load_layer_id: {
      /* The render target array index is provided in the thread payload as
       * bits 26:16 of r0.0.
       */
      ibc_builder_push_scalar(b);
      ibc_ref rtai = ibc_typed_ref(b->shader->g0, IBC_TYPE_UW);
      rtai.hw_grf.byte = 2;
      ibc_hw_grf_mul_stride(&rtai.hw_grf, 0);
      dest = ibc_MOV(b, IBC_TYPE_UD, rtai);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_load_sample_id:
      dest = ibc_sample_id(nti);
      break;

   case nir_intrinsic_load_sample_mask_in:
      dest = ibc_sample_mask_in(nti);
      break;

   case nir_intrinsic_load_fs_input_interp_deltas: {
      assert(!nir_dest_is_divergent(instr->dest));
      const unsigned base = nir_intrinsic_base(instr);
      assert(prog_data->urb_setup[base] >= 0);
      const unsigned slot = prog_data->urb_setup[base];
      const unsigned comp = nir_intrinsic_component(instr);

      ibc_ref per_vert = payload->inputs[slot][comp];
      ibc_ref dest_comps[] = {
         ibc_comp_ref(per_vert, 3),
         ibc_comp_ref(per_vert, 1),
         ibc_comp_ref(per_vert, 0),
      };
      ibc_builder_push_scalar(b);
      dest = ibc_VEC(b, dest_comps, 3);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_load_barycentric_pixel:
   case nir_intrinsic_load_barycentric_centroid:
   case nir_intrinsic_load_barycentric_sample: {
      /* Use the delta_xy values computed from the payload */
      assert(nir_dest_is_divergent(instr->dest));
      const enum glsl_interp_mode interp_mode =
         (enum glsl_interp_mode) nir_intrinsic_interp_mode(instr);
      enum brw_barycentric_mode bary =
         brw_barycentric_mode(interp_mode, instr->intrinsic);
      dest = payload->barycentric[bary];
      break;
   }

   case nir_intrinsic_load_interpolated_input: {
      assert(nir_dest_is_divergent(instr->dest));
      const unsigned base = nir_intrinsic_base(instr);
      assert(prog_data->urb_setup[base] >= 0);
      const unsigned slot = prog_data->urb_setup[base];
      const unsigned comp = nir_intrinsic_component(instr);

      ibc_ref bary = ibc_nir_src(nti, instr->src[0], IBC_TYPE_F);

      ibc_ref dest_comps[4];
      for (unsigned int i = 0; i < instr->num_components; i++) {
         ibc_ref per_vert = payload->inputs[slot][comp + i];
         per_vert.type = IBC_TYPE_F;
         dest_comps[i] = ibc_PLN(b, per_vert, bary);
      }
      dest = ibc_VEC(b, dest_comps, instr->num_components);
      break;
   }

   case nir_intrinsic_load_input: {
      const unsigned base = nir_intrinsic_base(instr);
      assert(prog_data->urb_setup[base] >= 0);
      const unsigned slot = prog_data->urb_setup[base];
      unsigned comp = nir_intrinsic_component(instr);

      /* Special case fields in the VUE header */
      if (base == VARYING_SLOT_LAYER)
         comp = 1;
      else if (base == VARYING_SLOT_VIEWPORT)
         comp = 2;

      ibc_ref dest_comps[4];
      for (unsigned int i = 0; i < instr->num_components; i++) {
         dest_comps[i] = payload->inputs[slot][comp + i];
         assert(dest_comps[i].file == IBC_FILE_LOGICAL);
         dest_comps[i].logical.comp = 3;
      }
      ibc_builder_push_scalar(b);
      dest = ibc_VEC(b, dest_comps, instr->num_components);
      ibc_builder_pop(b);
      break;
   }

   case nir_intrinsic_store_output: {
      ibc_ref src = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD);
      const unsigned store_offset = nir_src_as_uint(instr->src[1]);
      const unsigned location = nir_intrinsic_base(instr) +
         SET_FIELD(store_offset, BRW_NIR_FRAG_OUTPUT_LOCATION);
      const unsigned l = GET_FIELD(location, BRW_NIR_FRAG_OUTPUT_LOCATION);
      const unsigned i = GET_FIELD(location, BRW_NIR_FRAG_OUTPUT_INDEX);

      ibc_ref *output;
      if (i > 0 || (key->force_dual_color_blend && l == FRAG_RESULT_DATA1))
         output = &nti_fs->out.dual_src_color;
      else if (l == FRAG_RESULT_COLOR)
         output = &nti_fs->out.color[0];
      else if (l == FRAG_RESULT_DEPTH)
         output = &nti_fs->out.depth;
      else if (l == FRAG_RESULT_STENCIL)
         output = &nti_fs->out.stencil;
      else if (l == FRAG_RESULT_SAMPLE_MASK)
         output = &nti_fs->out.sample_mask;
      else if (l >= FRAG_RESULT_DATA0 &&
               l < FRAG_RESULT_DATA0 + BRW_MAX_DRAW_BUFFERS)
         output = &nti_fs->out.color[l - FRAG_RESULT_DATA0];
      else
         unreachable("Invalid location");

      if (output->file == IBC_FILE_NONE) {
         const unsigned num_comps = (l == FRAG_RESULT_DEPTH ||
                                     l == FRAG_RESULT_STENCIL ||
                                     l == FRAG_RESULT_SAMPLE_MASK) ? 1 : 4;
         *output = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, num_comps);

         if (l == FRAG_RESULT_COLOR) {
            /* gl_FragColor is replicated to all color outputs */
            assert(output == &nti_fs->out.color[0]);
            for (unsigned i = 1; i < key->nr_color_regions; i++)
               nti_fs->out.color[i] = nti_fs->out.color[0];
         }
      }

      ibc_ref dest = *output;
      dest.logical.comp += nir_intrinsic_component(instr);
      for (unsigned j = 0; j < instr->num_components; j++) {
         ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
         src.logical.comp++;
         dest.logical.comp++;
      }
      break;
   }

   case nir_intrinsic_load_helper_invocation:
      dest = ibc_NOT(b, IBC_TYPE_FLAG, ibc_pixel_mask_as_flag(nti));
      break;

   case nir_intrinsic_begin_invocation_interlock: {
      /* For beginInvocationInterlock(), we issue a memory fence using SENDC
       * which waits on other FS invocations on the same pixel to complete
       * before applying the fence.
       */
      ibc_send_instr *send = ibc_MEMORY_FENCE(b, ibc_null(IBC_TYPE_UD), 0);
      send->check_tdr = true;;
      break;
   }

   case nir_intrinsic_end_invocation_interlock: {
      /* For endInvocationInterlock(), we need to insert a memory fence which
       * stalls in the shader until the memory transactions prior to that
       * fence are complete.  This ensures that the shader does not end before
       * any writes from its critical section have landed.  Otherwise, you can
       * end up with a case where the next invocation on that pixel properly
       * stalls for previous FS invocation on its pixel to complete but
       * doesn't actually wait for the dataport memory transactions from that
       * thread to land before submitting its own.
       */
      ibc_reg *stall_reg = ibc_hw_grf_reg_create(b->shader, 32, 32);
      ibc_ref stall_ref = ibc_typed_ref(stall_reg, IBC_TYPE_UD);

      ibc_MEMORY_FENCE(b, stall_ref, 0);
      ibc_STALL_REG(b, stall_ref);
      break;
   }

   case nir_intrinsic_demote:
   case nir_intrinsic_discard:
   case nir_intrinsic_demote_if:
   case nir_intrinsic_discard_if: {
      assert(prog_data->uses_kill);

      ibc_ref cond = ibc_imm_w(1);
      if (instr->intrinsic == nir_intrinsic_demote_if ||
          instr->intrinsic == nir_intrinsic_discard_if)
         cond = ibc_nir_src(nti, instr->src[0], IBC_TYPE_FLAG);

      ibc_alu_instr *mov =
         ibc_build_alu(b, IBC_ALU_OP_MOV, ibc_null(cond.type),
                       nti_fs->live_pix, BRW_CONDITIONAL_Z, &cond, 1);
      mov->instr.predicate = IBC_PREDICATE_NORMAL;

      ibc_HALT_JUMP(b, nti_fs->live_pix, IBC_PREDICATE_NOT_ANY4H,
                    &nti_fs->halt_jumps);
      break;
   }

   default:
      return false;
   }

   if (nir_intrinsic_infos[instr->intrinsic].has_dest) {
      ibc_builder_push_nir_dest_group(b, instr->dest);
      ibc_write_nir_dest(nti, &instr->dest, dest);
      ibc_builder_pop(b);
   } else {
      assert(dest.file == IBC_FILE_NONE);
   }

   return true;
}

static void
ibc_emit_alhpa_to_coverage_workaround(struct nir_to_ibc_state *nti)
{
   unreachable("Unsupported");
}

enum ibc_fb_write_src {
   IBC_FB_WRITE_SRC_COLOR0,      /* REQUIRED */
   IBC_FB_WRITE_SRC_COLOR1,      /* for dual source blend messages */
   IBC_FB_WRITE_SRC_SRC0_ALPHA,
   IBC_FB_WRITE_SRC_DEPTH,       /* gl_FragDepth */
   IBC_FB_WRITE_SRC_STENCIL,     /* gl_FragStencilRefARB */
   IBC_FB_WRITE_SRC_OMASK,       /* Sample Mask (gl_SampleMask) */
   IBC_FB_WRITE_SRC_TARGET,
   IBC_FB_WRITE_SRC_FLAGS,
   IBC_FB_WRITE_NUM_SRCS
};

enum ibc_fb_write_flags {
   IBC_FB_WRITE_FLAGS_NONE =              0,
   IBC_FB_WRITE_FLAG_LAST_RT =            (1 << 0),
   IBC_FB_WRITE_FLAG_NULL_RT =            (1 << 1),
   IBC_FB_WRITE_FLAG_REPLICATE_ALPHA =    (1 << 2),
   IBC_FB_WRITE_FLAG_COMPUTED_STENCIL =   (1 << 3),
};

static ibc_intrinsic_instr *
ibc_emit_fb_write(struct nir_to_ibc_state *nti,
                  unsigned target,
                  ibc_ref color0, ibc_ref color1,
                  ibc_ref src0_alpha,
                  enum ibc_fb_write_flags flags)
{
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;
   ibc_builder *b = &nti->b;

   ibc_intrinsic_src srcs[IBC_FB_WRITE_NUM_SRCS] = {
      [IBC_FB_WRITE_SRC_COLOR0] = { .ref = color0, .num_comps = 4 },
      [IBC_FB_WRITE_SRC_COLOR1] = { .ref = color1, .num_comps = 4 },
      [IBC_FB_WRITE_SRC_SRC0_ALPHA] = { .ref = src0_alpha, .num_comps = 1 },
      [IBC_FB_WRITE_SRC_DEPTH] = { .ref = nti_fs->out.depth, .num_comps = 1 },
      [IBC_FB_WRITE_SRC_STENCIL] =
         { .ref = nti_fs->out.stencil, .num_comps = 1},
      [IBC_FB_WRITE_SRC_TARGET] = { ibc_imm_ud(target), .num_comps = 1 },
      /* LAST_RT will be filled with the real value later */
      [IBC_FB_WRITE_SRC_FLAGS] = { .ref = ibc_imm_ud(flags), .num_comps = 1},
   };

   if (prog_data->uses_omask) {
      srcs[IBC_FB_WRITE_SRC_OMASK].ref = nti_fs->out.sample_mask;
      srcs[IBC_FB_WRITE_SRC_OMASK].num_comps = 1;
   }

   for (unsigned i = 0; i < IBC_FB_WRITE_NUM_SRCS; i++) {
      if (srcs[i].ref.file == IBC_FILE_NONE)
         srcs[i].num_comps = 0;
   }

   ibc_intrinsic_instr *write =
      ibc_build_intrinsic(b, IBC_INTRINSIC_OP_FB_WRITE,
                          ibc_null(IBC_TYPE_UD), 0, 0,
                          srcs, IBC_FB_WRITE_NUM_SRCS);
   write->can_reorder = false;
   write->has_side_effects = true;

   if (prog_data->uses_kill) {
      ibc_instr_set_predicate(&write->instr, nti_fs->live_pix,
                              IBC_PREDICATE_NORMAL);
   }

   if (color1.file != IBC_FILE_NONE && color0.file != IBC_FILE_NONE)
      prog_data->dual_src_blend = true;

   return write;
}

static void
fb_write_instr_add_flags(ibc_intrinsic_instr *instr,
                         enum ibc_fb_write_flags new_flags)
{
   enum ibc_fb_write_flags flags =
      ibc_ref_as_uint(instr->src[IBC_FB_WRITE_SRC_FLAGS].ref);
   flags |= new_flags;
   instr->src[IBC_FB_WRITE_SRC_FLAGS].ref = ibc_imm_ud(flags);
}

static void
ibc_emit_fb_writes(struct nir_to_ibc_state *nti)
{
   const struct brw_wm_prog_key *key = (const void *)nti->key;
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;

   if (nti_fs->out.stencil.file != IBC_FILE_NONE) {
      /* From the 'Render Target Write message' section of the docs:
       * "Output Stencil is not supported with SIMD16 Render Target Write
       * Messages."
       */
      ibc_fs_limit_dispatch_width(nti, 8, "gl_FragStencilRefARB unsupported "
                                          "in SIMD16+ mode.\n");
   }

   /* ANV doesn't know about sample mask output during the wm key creation
    * so we compute if we need replicate alpha and emit alpha to coverage
    * workaround here.
    */
   bool replicate_alpha = key->alpha_test_replicate_alpha ||
      (key->nr_color_regions > 1 && key->alpha_to_coverage &&
       nti_fs->out.sample_mask.file == IBC_FILE_NONE);

   /* From the SKL PRM, Volume 7, "Alpha Coverage":
    *  "If Pixel Shader outputs oMask, AlphaToCoverage is disabled in
    *   hardware, regardless of the state setting for this feature."
    */
   if (key->alpha_to_coverage &&
       nti_fs->out.sample_mask.file != IBC_FILE_NONE &&
       nti_fs->out.color[0].file != IBC_FILE_NONE)
      ibc_emit_alhpa_to_coverage_workaround(nti);

   assert(!key->clamp_fragment_color);

   enum ibc_fb_write_flags flags = IBC_FB_WRITE_FLAGS_NONE;
   if (replicate_alpha)
      flags |= IBC_FB_WRITE_FLAG_REPLICATE_ALPHA;

   if (prog_data->computed_stencil)
      flags |= IBC_FB_WRITE_FLAG_COMPUTED_STENCIL;

   ibc_intrinsic_instr *last_fb_write = NULL;
   for (unsigned target = 0; target < key->nr_color_regions; target++) {
      /* Skip over outputs that weren't written. */
      if (nti_fs->out.color[target].file == IBC_FILE_NONE)
         continue;

      ibc_ref src0_alpha = {};
      if (replicate_alpha && target != 0) {
         src0_alpha = nti_fs->out.color[0];
         src0_alpha.logical.comp = 3;
      }

      last_fb_write =
         ibc_emit_fb_write(nti, target, nti_fs->out.color[target],
                           nti_fs->out.dual_src_color, src0_alpha, flags);
   }

   if (last_fb_write == NULL) {
      /* Even if there's no color buffers enabled, we still need to send
       * alpha out the pipeline to our null renderbuffer to support
       * alpha-testing, alpha-to-coverage, and so on.
       */
      last_fb_write =
         ibc_emit_fb_write(nti, 0 /* target */, nti_fs->out.color[0],
                           ibc_null(IBC_TYPE_UD), ibc_null(IBC_TYPE_UD),
                           flags | IBC_FB_WRITE_FLAG_NULL_RT);
   }

   fb_write_instr_add_flags(last_fb_write, IBC_FB_WRITE_FLAG_LAST_RT);
}

unsigned
ibc_fb_write_instr_max_simd_width(const ibc_intrinsic_instr *write,
                                  const struct gen_device_info *devinfo)
{
   assert(write->op == IBC_INTRINSIC_OP_FB_WRITE);
   if (write->src[IBC_FB_WRITE_SRC_COLOR1].ref.file != IBC_FILE_NONE)
      return 8; /* Dual-source FB writes are unsupported in SIMD16 mode. */
   else
      return 16;
}

bool
ibc_lower_io_fb_write_to_send(ibc_builder *b, ibc_intrinsic_instr *write)
{
   const struct gen_device_info *devinfo = b->shader->devinfo;
   struct ibc_fs_payload *payload = b->shader->stage_data;
   assert(write->op == IBC_INTRINSIC_OP_FB_WRITE);

   const ibc_ref color0 = write->src[IBC_FB_WRITE_SRC_COLOR0].ref;
   const ibc_ref color1 = write->src[IBC_FB_WRITE_SRC_COLOR1].ref;
   const ibc_ref src0_alpha = write->src[IBC_FB_WRITE_SRC_SRC0_ALPHA].ref;
   const ibc_ref src_depth = write->src[IBC_FB_WRITE_SRC_DEPTH].ref;
   const ibc_ref src_stencil = write->src[IBC_FB_WRITE_SRC_STENCIL].ref;
   const ibc_ref src_omask = write->src[IBC_FB_WRITE_SRC_OMASK].ref;
   const unsigned target =
      ibc_ref_as_uint(write->src[IBC_FB_WRITE_SRC_TARGET].ref);
   const enum ibc_fb_write_flags flags =
      ibc_ref_as_uint(write->src[IBC_FB_WRITE_SRC_FLAGS].ref);

   ibc_builder_push_instr_group(b, &write->instr);

   ibc_send_instr *send = ibc_send_instr_create(b->shader,
                                                write->instr.simd_group,
                                                write->instr.simd_width);
   send->instr.we_all = write->instr.we_all;
   send->can_reorder = write->can_reorder;
   send->has_side_effects = write->has_side_effects;

   if (write->instr.predicate != IBC_PREDICATE_NONE) {
      send->instr.flag = write->instr.flag;
      send->instr.predicate = write->instr.predicate;
   }

   ibc_intrinsic_src src[16] = {};
   unsigned num_srcs = 0;
   bool has_header = false;

   if (devinfo->gen < 11 && (color1.file != IBC_FILE_NONE || target > 0)) {
      has_header = true;
      ibc_reg *header_reg =
         ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);
      ibc_ref header = ibc_typed_ref(header_reg, IBC_TYPE_UD);

      ibc_builder_push_we_all(b, 8);
      ibc_MOV_to(b, header, ibc_typed_ref(b->shader->g0, IBC_TYPE_UD));
      ibc_builder_pop(b);

      uint32_t g00_bits = 0;

      /* Set "Source0 Alpha Present to RenderTarget" bit in message
       * header.
       */
      if (target > 0 && (flags & IBC_FB_WRITE_FLAG_REPLICATE_ALPHA))
         g00_bits |= 1 << 11;

      if (flags & IBC_FB_WRITE_FLAG_COMPUTED_STENCIL)
         g00_bits |= 1 << 14;

      if (g00_bits) {
         ibc_builder_push_scalar(b);
         ibc_build_alu2(b, IBC_ALU_OP_OR, header,
                        ibc_typed_ref(b->shader->g0, IBC_TYPE_UD),
                        ibc_imm_ud(g00_bits));
         ibc_builder_pop(b);
      }

      /* TODO: uses_kill? */

      ibc_ref header_2 = header;
      header_2.hw_grf.byte += 2 * ibc_type_byte_size(IBC_TYPE_UD);
      if (target > 0) {
         ibc_builder_push_scalar(b);
         ibc_MOV_to(b, header_2, ibc_imm_ud(target));
         ibc_builder_pop(b);
      }

      src[num_srcs++] = (ibc_intrinsic_src) {
         .ref = header,
         .simd_width = 1,
         .num_comps = 8,
      };

      ibc_ref pix = payload->pixel[write->instr.simd_group / 16];
      if (write->instr.predicate != IBC_PREDICATE_NONE) {
         /* Make a copy of pix */
         ibc_reg *tmp_pix_reg =
            ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);

         ibc_builder_push_we_all(b, 8);
         ibc_MOV_to(b, ibc_typed_ref(tmp_pix_reg, pix.type), pix);
         ibc_builder_pop(b);

         pix = ibc_typed_ref(tmp_pix_reg, IBC_TYPE_UD);

         /* Write the flag value to the bottom 16 bits of pix.7 */
         ibc_ref pix_7 = pix;
         pix_7.hw_grf.byte += 7 * ibc_type_byte_size(IBC_TYPE_UD);
         pix_7.type = IBC_TYPE_UW;

         assert(write->instr.flag.file == IBC_FILE_FLAG);
         ibc_ref flag_uw = write->instr.flag;
         flag_uw.type = IBC_TYPE_UW;
         flag_uw.flag.bit = (write->instr.simd_group / 16) * 16;

         ibc_builder_push_scalar(b);
         ibc_MOV_to(b, pix_7, flag_uw);
         ibc_builder_pop(b);
      }

      src[num_srcs++] = (ibc_intrinsic_src) {
         .ref = pix,
         .simd_width = 1,
         .num_comps = 8,
      };
   }
   assert(num_srcs == 2 * has_header);

   bool src0_alpha_present = false;
   if (src0_alpha.file != IBC_FILE_NONE) {
      assert(ibc_type_bit_size(src0_alpha.type) == 32);
      src[num_srcs++].ref = src0_alpha;
      src0_alpha_present = true;
   } else if ((flags & IBC_FB_WRITE_FLAG_REPLICATE_ALPHA) && target != 0) {
      /* Handle the case when fragment shader doesn't write to draw buffer
       * zero. No need to to provide real data for src0_alpha because the
       * alpha value will be undefined.
       */
      src[num_srcs++].ref = ibc_null(IBC_TYPE_F);
   }

   if (src_omask.file != IBC_FILE_NONE) {
      /* Unlike most payload sources, the hardware actually consumes the
       * sample mask as a UW type.
       */
      assert(ibc_type_bit_size(src_omask.type) == 32);
      ibc_ref mask_ref = src_omask;
      mask_ref.type = IBC_TYPE_UW;
      src[num_srcs++].ref = mask_ref;
   }


   if (color0.file == IBC_FILE_NONE) {
      for (unsigned i = 0; i < 4; i++)
         src[num_srcs++].ref = ibc_null(IBC_TYPE_F);
   } else {
      assert(ibc_type_bit_size(color0.type) == 32);
      for (unsigned i = 0; i < 4; i++)
         src[num_srcs++].ref = ibc_comp_ref(color0, i);
   }

   if (color1.file != IBC_FILE_NONE) {
      assert(ibc_type_bit_size(color1.type) == 32);
      for (unsigned i = 0; i < 4; i++)
         src[num_srcs++].ref = ibc_comp_ref(color1, i);
   }

   if (src_depth.file != IBC_FILE_NONE) {
      assert(ibc_type_bit_size(src_depth.type) == 32);
      src[num_srcs++].ref = src_depth;
   }

   if (src_stencil.file != IBC_FILE_NONE) {
      assert(devinfo->gen >= 9);
      assert(write->instr.simd_group == 0 && write->instr.simd_width == 8);

      /* Stencil is a packed byte value */
      assert(ibc_type_bit_size(src_stencil.type) == 32);
      ibc_ref stencil = src_stencil;
      stencil.type = IBC_TYPE_UB;
      src[num_srcs++].ref = stencil;
   }

   unsigned mlen;
   send->payload[0] = ibc_MESSAGE(b, src, num_srcs, &mlen);
   send->mlen = mlen;

   uint32_t msg_control;
   if (color1.file != IBC_FILE_NONE) {
      assert(write->instr.simd_width == 8);
      msg_control = write->instr.simd_group % 16 == 0 ?
            BRW_DATAPORT_RENDER_TARGET_WRITE_SIMD8_DUAL_SOURCE_SUBSPAN01 :
            BRW_DATAPORT_RENDER_TARGET_WRITE_SIMD8_DUAL_SOURCE_SUBSPAN23;
   } else {
      assert(write->instr.simd_group == 0 ||
             (write->instr.simd_group == 16 &&
              write->instr.simd_width == 16));
      msg_control = write->instr.simd_width == 16 ?
         BRW_DATAPORT_RENDER_TARGET_WRITE_SIMD16_SINGLE_SOURCE :
         BRW_DATAPORT_RENDER_TARGET_WRITE_SIMD8_SINGLE_SOURCE_SUBSPAN01;
   }
   msg_control |= (write->instr.simd_group / 16) << 3;

   send->sfid = GEN6_SFID_DATAPORT_RENDER_CACHE;
   send->desc_imm =
      brw_dp_write_desc(b->shader->devinfo, target, msg_control,
                        GEN6_DATAPORT_WRITE_MESSAGE_RENDER_TARGET_WRITE,
                        (flags & IBC_FB_WRITE_FLAG_LAST_RT) != 0,
                        0 /* send_commit_msg */);

   if (devinfo->gen >= 11) {
      /* Set the "Render Target Index" and "Src0 Alpha Present" fields
       * in the extended message descriptor, in lieu of using a header.
       */
      send->ex_desc_imm = target << 12 | src0_alpha_present << 15;

      if (flags & IBC_FB_WRITE_FLAG_NULL_RT)
         send->ex_desc_imm |= 1 << 20;
   }

   send->has_header = has_header;
   send->has_side_effects = true;
   send->check_tdr = true;
   send->eot = (flags & IBC_FB_WRITE_FLAG_LAST_RT) &&
      write->instr.simd_group + write->instr.simd_width == b->shader->simd_width;

   ibc_builder_insert_instr(b, &send->instr);
   ibc_instr_remove(&write->instr);

   ibc_builder_pop(b);

   return true;
}

const unsigned *
ibc_compile_fs(const struct brw_compiler *compiler, void *log_data,
               void *mem_ctx,
               const struct brw_wm_prog_key *key,
               struct brw_wm_prog_data *prog_data,
               struct nir_shader *shader,
               int shader_time_index8,
               int shader_time_index16,
               int shader_time_index32,
               bool allow_spilling,
               bool use_rep_send,
               struct brw_vue_map *vue_map,
               char **error_str_out)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

//   unsigned max_subgroup_size = unlikely(INTEL_DEBUG & DEBUG_DO32) ? 32 : 16;
   unsigned max_subgroup_size = 32;

   brw_nir_apply_key(shader, compiler, &key->base, max_subgroup_size, true);
   brw_nir_lower_fs_inputs(shader, compiler->devinfo, key);
   brw_nir_lower_fs_outputs(shader);

   if (!key->multisample_fbo)
      NIR_PASS_V(shader, brw_nir_demote_sample_qualifiers);
   NIR_PASS_V(shader, brw_nir_move_interpolation_to_top);
   brw_postprocess_nir(shader, compiler, true);

   brw_nir_populate_wm_prog_data(shader, compiler->devinfo, key, prog_data);

   /* TODO: Should this go in populate_wm_prog_data? */
   prog_data->uses_src_depth = prog_data->uses_src_w =
      (shader->info.system_values_read & (1ull << SYSTEM_VALUE_FRAG_COORD)) != 0;

   struct {
      bool enabled;
      const unsigned *data;
      unsigned size;
      unsigned num_ff_regs;
   } bin[3] = {
      { .enabled = !(INTEL_DEBUG & DEBUG_NO8), },
      { .enabled = !(INTEL_DEBUG & DEBUG_NO16), },
      { .enabled = !(INTEL_DEBUG & DEBUG_NO32), },
   };

   bool first_bin = true;
   unsigned max_simd_width = 32;
   for (unsigned i = 0; i < 3; i++) {
      const unsigned bin_simd_width = 8 << i;
      if (bin_simd_width > max_simd_width)
         break;

      if (!bin[i].enabled)
         continue;

      /* Per-binary context */
      void *bin_ctx = ralloc_context(mem_ctx);

      struct nir_fs_to_ibc_state fs_state = {
         .max_simd_width = 32,
      };
      struct nir_to_ibc_state nti;
      nir_to_ibc_state_init(&nti, MESA_SHADER_FRAGMENT, compiler->devinfo,
                            &key->base, &prog_data->base,
                            &fs_state, bin_simd_width, bin_ctx);

      nti.payload = &ibc_setup_fs_payload(&nti.b, prog_data, bin_ctx)->base;

      if (prog_data->uses_kill) {
         fs_state.live_pix = ibc_pixel_mask_as_flag(&nti);
         assert(fs_state.live_pix.file == IBC_FILE_FLAG);
         ((ibc_reg *)fs_state.live_pix.reg)->is_wlr = false;
         list_inithead(&fs_state.halt_jumps);
      }

      ibc_emit_nir_shader(&nti, shader);

      if (prog_data->uses_kill)
         ibc_HALT_MERGE(&nti.b, &fs_state.halt_jumps);

      ibc_emit_fb_writes(&nti);

      /* It's possible if SIMD8 is disabled for the minimum size to not be
       * valid.  In that case, we go ahead and build the IBC but bail here so
       * we don't try to go further with an invalid shader.
       */
      if (bin_simd_width > fs_state.max_simd_width) {
         ralloc_free(bin_ctx);
         break;
      }
      max_simd_width = MIN2(max_simd_width, fs_state.max_simd_width);

      ibc_shader *ibc = nir_to_ibc_state_finish(&nti);
      if (INTEL_DEBUG & DEBUG_WM)
         ibc_print_shader(ibc, stderr);
      ibc_validate_shader(ibc);

      /* We need this for FB write lowering */
      ibc->stage_data = nti.payload;

      ibc_lower_and_optimize(ibc);

      bool assigned = ibc_assign_regs(ibc, compiler, first_bin);

      if (INTEL_DEBUG & DEBUG_WM)
         ibc_print_shader(ibc, stderr);

      if (assigned) {
         IBC_PASS_V(ibc, ibc_schedule_instructions_post_ra);
         bin[i].data = ibc_to_binary(ibc, &shader->info, compiler, log_data,
                                     mem_ctx, &bin[i].size);
         bin[i].num_ff_regs = nti.payload->num_ff_regs;
      }

      ralloc_free(bin_ctx);

      if (!assigned) {
         if (first_bin) {
            *error_str_out = ralloc_strdup(mem_ctx,
                                           "Failed to allocate register");
            return NULL;
         }
         compiler->shader_perf_log(log_data,
                                   "SIMD%u shader failed to compile: "
                                   "Failed to allocate registers",
                                   bin_simd_width);
         /* We assume that if one shader fails to allocate, all wider shaders
          * will also fail.
          */
         break;
      }

      first_bin = false;
   }

   if (prog_data->persample_dispatch) {
      /* Starting with SandyBridge (where we first get MSAA), the different
       * pixel dispatch combinations are grouped into classifications A
       * through F (SNB PRM Vol. 2 Part 1 Section 7.7.1).  On all hardware
       * generations, the only configurations supporting persample dispatch
       * are are this in which only one dispatch width is enabled.
       */
      if (bin[2].data || bin[1].data)
         bin[0].data = NULL;
      if (bin[2].data)
         bin[1].data = NULL;
   }

   unsigned final_size = 0;
   void *final_bin = NULL;

   for (unsigned i = 0; i < 3; i++) {
      const unsigned bin_simd_width = 8 << i;
      if (bin[i].data == NULL)
         continue;

      unsigned offset = final_size;

      /* The start has to be aligned to 64B */
      const unsigned pad = ALIGN(offset, 64) - offset;

      final_size = final_size + pad + bin[i].size;
      final_bin = reralloc_size(mem_ctx, final_bin, final_size);

      if (pad > 0) {
         memset(final_bin + offset, 0, pad);
         offset += pad;
      }

      assert(offset == ALIGN(offset, 64));
      memcpy(final_bin + offset, bin[i].data, bin[i].size);

      switch (bin_simd_width) {
      case 8:
         assert(offset == 0);
         prog_data->dispatch_8 = true;
         prog_data->base.dispatch_grf_start_reg = bin[i].num_ff_regs;
         break;
      case 16:
         prog_data->dispatch_16 = true;
         prog_data->prog_offset_16 = offset;
         prog_data->dispatch_grf_start_reg_16 = bin[i].num_ff_regs;
         break;
      case 32:
         prog_data->dispatch_32 = true;
         prog_data->prog_offset_32 = offset;
         prog_data->dispatch_grf_start_reg_32 = bin[i].num_ff_regs;
         break;
      default:
         unreachable("Invalid dispatch width");
      }
   }

   prog_data->base.program_size = final_size;
   return final_bin;
}
