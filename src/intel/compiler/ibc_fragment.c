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

struct ibc_fs_payload {
   struct ibc_payload_base base;

   ibc_reg_ref pixel[2];
   ibc_reg_ref barycentric[BRW_BARYCENTRIC_MODE_COUNT];
   ibc_reg_ref src_z;
   ibc_reg_ref src_w;
   ibc_reg_ref sample_pos[2];
   ibc_reg_ref sample_mask_in;

   ibc_reg_ref inputs[32][4];
};

struct nir_fs_to_ibc_state {
   struct {
      ibc_reg_ref color[BRW_MAX_DRAW_BUFFERS];
      ibc_reg_ref dual_src_color;
      ibc_reg_ref depth;
      ibc_reg_ref stencil;
      ibc_reg_ref sample_mask;
   } out;

   unsigned max_simd_width;
   const char *simd_restrict_reason;
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
set_reg_ref_or_zip(ibc_builder *b, ibc_reg_ref *dest, ibc_reg_ref src,
                   unsigned num_comps)
{
   if (dest->file == IBC_REG_FILE_NONE) {
      *dest = src;
   } else {
      *dest = ibc_SIMD_ZIP2(b, *dest, src, num_comps);
   }
}

static void
ibc_load_fs_payload_reg(ibc_builder *b, ibc_reg_ref *dest,
                        unsigned simd_group, unsigned *reg,
                        enum ibc_type type)
{
   ibc_builder_push_group(b, simd_group, MIN2(16, b->simd_width));
   ibc_reg_ref tmp = ibc_load_payload_logical(b, reg, type, 1);
   ibc_builder_pop(b);
   set_reg_ref_or_zip(b, dest, tmp, 1);
}

static void
ibc_load_fs_barycentric(ibc_builder *b, ibc_reg_ref *dest,
                        unsigned simd_group, unsigned *reg)
{
   /* The PLN instruction is silly.  In SIMD16 mode, it doesn't act on a
    * SIMD16 vec2 as would make sense.  Instead, it acts on two SIMD8 vec2s
    * sequentially in registers.  The barycentrics come into the shader in a
    * way that makes them convenient for plane but it means we actually get
    * the one SIMD8 vec2 at a time.
    */
   ibc_reg_ref bary = {};
   ibc_builder_push_group(b, simd_group, MIN2(16, b->simd_width));
   for (unsigned int g = 0; g < b->simd_width; g += 8) {
      ibc_builder_push_group(b, g, 8);
      ibc_reg_ref bary8 = ibc_load_payload_logical(b, reg, IBC_TYPE_F, 2);
      ibc_builder_pop(b);
      set_reg_ref_or_zip(b, &bary, bary8, 2);
   }
   ibc_builder_pop(b);
   set_reg_ref_or_zip(b, dest, bary, 2);
}

static struct ibc_fs_payload *
ibc_setup_fs_payload(ibc_builder *b, struct brw_wm_prog_data *prog_data,
                     void *mem_ctx)
{
   struct ibc_fs_payload *payload = rzalloc(mem_ctx, struct ibc_fs_payload);
   ibc_setup_payload_base(b, &payload->base);

   unsigned reg = payload->base.num_ff_regs;
   for (unsigned g = 0; g < b->shader->simd_width; g += 16)
      payload->pixel[g / 16] = ibc_load_payload_reg(b, &reg);

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
         payload->sample_pos[g / 16] = ibc_load_payload_reg(b, &reg);

      /* R32-33: MSAA input coverage mask */
      if (prog_data->uses_sample_mask)
         ibc_load_fs_payload_reg(b, &payload->sample_mask_in, g,
                                 &reg, IBC_TYPE_UD);
   }

   payload->base.num_ff_regs = reg;

   assert(prog_data->base.nr_params == 0);

   /* We represent per-vertex attributes in the payload as a SIMD1 vec4 for
    * each component coming out of the geometry pipeline.
    */
   ibc_builder_push_we_all(b, 1);
   for (unsigned i = 0; i < prog_data->num_varying_inputs; i++) {
      for (unsigned c = 0; c < 4; c++) {
         ibc_reg_ref grf =
            ibc_hw_grf_ref(reg + (c / 2), (c % 2) * 4, IBC_TYPE_UD);
         payload->inputs[i][c] =
            ibc_builder_new_logical_reg(b, IBC_TYPE_UD, 4);
         ibc_load_payload(b, payload->inputs[i][c], grf, 4);
      }
      reg += 2;
   }
   ibc_builder_pop(b);

   return payload;
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

   ibc_reg_ref dest = { .file = IBC_REG_FILE_NONE, };
   switch (instr->intrinsic) {
   case nir_intrinsic_load_barycentric_pixel:
   case nir_intrinsic_load_barycentric_centroid:
   case nir_intrinsic_load_barycentric_sample: {
      /* Use the delta_xy values computed from the payload */
      const enum glsl_interp_mode interp_mode =
         (enum glsl_interp_mode) nir_intrinsic_interp_mode(instr);
      enum brw_barycentric_mode bary =
         brw_barycentric_mode(interp_mode, instr->intrinsic);
      dest = payload->barycentric[bary];
      break;
   }

   case nir_intrinsic_load_interpolated_input: {
      ibc_reg_ref bary = ibc_nir_src(nti, instr->src[0], IBC_TYPE_F);
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_F, instr->num_components);

      for (unsigned int i = 0; i < instr->num_components; i++) {
         assert(prog_data->urb_setup[nir_intrinsic_base(instr)] >= 0);
         const unsigned l = prog_data->urb_setup[nir_intrinsic_base(instr)];
         const unsigned c = nir_intrinsic_component(instr) + i;
         ibc_reg_ref per_vert = payload->inputs[l][c];
         per_vert.type = IBC_TYPE_F;
         ibc_reg_ref comp = ibc_PLN(b, per_vert, bary);

         ibc_reg_ref mov_dest = dest;
         mov_dest.logical.comp = i;
         ibc_build_alu1(b, IBC_ALU_OP_MOV, mov_dest, comp);
      }
      break;
   }

   case nir_intrinsic_load_input: {
      dest = ibc_builder_new_logical_reg(b, IBC_TYPE_UD, instr->num_components);

      for (unsigned int i = 0; i < instr->num_components; i++) {
         assert(prog_data->urb_setup[nir_intrinsic_base(instr)] >= 0);
         const unsigned l = prog_data->urb_setup[nir_intrinsic_base(instr)];
         const unsigned c = nir_intrinsic_component(instr) + i;
         ibc_reg_ref comp = payload->inputs[l][c];
         comp.logical.comp = 3;

         ibc_reg_ref mov_dest = dest;
         mov_dest.logical.comp = i;
         ibc_build_alu1(b, IBC_ALU_OP_MOV, mov_dest, comp);
      }
      break;
   }

   case nir_intrinsic_store_output: {
      ibc_reg_ref src = ibc_nir_src(nti, instr->src[0], IBC_TYPE_UD);
      const unsigned store_offset = nir_src_as_uint(instr->src[1]);
      const unsigned location = nir_intrinsic_base(instr) +
         SET_FIELD(store_offset, BRW_NIR_FRAG_OUTPUT_LOCATION);
      const unsigned l = GET_FIELD(location, BRW_NIR_FRAG_OUTPUT_LOCATION);
      const unsigned i = GET_FIELD(location, BRW_NIR_FRAG_OUTPUT_INDEX);

      ibc_reg_ref *output;
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

      if (output->file == IBC_REG_FILE_NONE) {
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

      ibc_reg_ref dest = *output;
      dest.logical.comp += nir_intrinsic_component(instr);
      for (unsigned j = 0; j < instr->num_components; j++) {
         ibc_build_alu1(b, IBC_ALU_OP_MOV, dest, src);
         src.logical.comp++;
         dest.logical.comp++;
      }
      break;
   }

   default:
      return false;
   }

   if (nir_intrinsic_infos[instr->intrinsic].has_dest) {
      assert(dest.file == IBC_REG_FILE_LOGICAL);
      nti->ssa_to_reg[instr->dest.ssa.index] = dest.reg;
   } else {
      assert(dest.file == IBC_REG_FILE_NONE);
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
   IBC_FB_WRITE_SRC_LAST_RT,
   IBC_FB_WRITE_NUM_SRCS
};

static void
ibc_emit_fb_write(struct nir_to_ibc_state *nti,
                  unsigned target,
                  ibc_reg_ref color0, ibc_reg_ref color1,
                  ibc_reg_ref src0_alpha,
                  bool last_rt)
{
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;
   ibc_builder *b = &nti->b;

   ibc_intrinsic_instr *write =
      ibc_intrinsic_instr_create(b->shader, IBC_INTRINSIC_OP_FB_WRITE,
                                 b->simd_group, b->simd_width,
                                 IBC_FB_WRITE_NUM_SRCS);

   write->src[IBC_FB_WRITE_SRC_COLOR0].ref = color0;
   write->src[IBC_FB_WRITE_SRC_COLOR0].num_comps = 4;

   write->src[IBC_FB_WRITE_SRC_COLOR1].ref = color1;
   write->src[IBC_FB_WRITE_SRC_COLOR1].num_comps = 4;

   write->src[IBC_FB_WRITE_SRC_SRC0_ALPHA].ref = src0_alpha;
   write->src[IBC_FB_WRITE_SRC_SRC0_ALPHA].num_comps = 1;

   write->src[IBC_FB_WRITE_SRC_DEPTH].ref = nti_fs->out.depth;
   write->src[IBC_FB_WRITE_SRC_DEPTH].num_comps = 1;

   write->src[IBC_FB_WRITE_SRC_STENCIL].ref = nti_fs->out.stencil;
   write->src[IBC_FB_WRITE_SRC_STENCIL].num_comps = 1;

   write->src[IBC_FB_WRITE_SRC_OMASK].ref = nti_fs->out.sample_mask;
   write->src[IBC_FB_WRITE_SRC_OMASK].num_comps = 1;

   write->src[IBC_FB_WRITE_SRC_TARGET].ref = ibc_imm_ud(target);
   write->src[IBC_FB_WRITE_SRC_TARGET].num_comps = 1;

   write->src[IBC_FB_WRITE_SRC_LAST_RT].ref = ibc_imm_ud(last_rt);
   write->src[IBC_FB_WRITE_SRC_LAST_RT].num_comps = 1;

   for (unsigned i = 0; i < IBC_FB_WRITE_NUM_SRCS; i++) {
      if (write->src[i].ref.file == IBC_REG_FILE_NONE)
         write->src[i].num_comps = 0;
   }

   write->can_reorder = false;
   write->has_side_effects = true;
   ibc_builder_insert_instr(b, &write->instr);

   assert(!prog_data->uses_kill);
}

static void
ibc_emit_fb_writes(struct nir_to_ibc_state *nti)
{
   const struct brw_wm_prog_key *key = (const void *)nti->key;
   struct brw_wm_prog_data *prog_data = (void *)nti->prog_data;
   struct nir_fs_to_ibc_state *nti_fs = nti->stage_state;

   if (nti_fs->out.stencil.file != IBC_REG_FILE_NONE) {
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
   prog_data->replicate_alpha = key->alpha_test_replicate_alpha ||
      (key->nr_color_regions > 1 && key->alpha_to_coverage &&
       nti_fs->out.sample_mask.file == IBC_REG_FILE_NONE);

   /* From the SKL PRM, Volume 7, "Alpha Coverage":
    *  "If Pixel Shader outputs oMask, AlphaToCoverage is disabled in
    *   hardware, regardless of the state setting for this feature."
    */
   if (key->alpha_to_coverage &&
       nti_fs->out.sample_mask.file != IBC_REG_FILE_NONE &&
       nti_fs->out.color[0].file != IBC_REG_FILE_NONE)
      ibc_emit_alhpa_to_coverage_workaround(nti);

   bool fb_written = false;
   if (key->nr_color_regions > 0) {
      for (unsigned target = 0; target < key->nr_color_regions; target++) {
         /* Skip over outputs that weren't written. */
         if (nti_fs->out.color[target].file == IBC_REG_FILE_NONE)
            continue;

         ibc_reg_ref src0_alpha = {};
         if (prog_data->replicate_alpha && target != 0) {
            src0_alpha = nti_fs->out.color[0];
            src0_alpha.logical.comp = 3;
         }

         ibc_emit_fb_write(nti, target, nti_fs->out.color[target],
                           nti_fs->out.dual_src_color, src0_alpha,
                           target == key->nr_color_regions - 1);
         fb_written = true;
      }
   }

   if (!fb_written) {
      /* Even if there's no color buffers enabled, we still need to send
       * alpha out the pipeline to our null renderbuffer to support
       * alpha-testing, alpha-to-coverage, and so on.
       */
      ibc_emit_fb_write(nti, 0 /* target */, nti_fs->out.color[0],
                        ibc_null(IBC_TYPE_UD), ibc_null(IBC_TYPE_UD),
                        true /* last_rt */);
   }
}

unsigned
ibc_lower_simd_width_fb_write_max_width(ibc_intrinsic_instr *write)
{
   assert(write->op == IBC_INTRINSIC_OP_FB_WRITE);
   if (write->src[IBC_FB_WRITE_SRC_COLOR1].ref.file != IBC_REG_FILE_NONE)
      return 8; /* Dual-source FB writes are unsupported in SIMD16 mode. */
   else
      return 16;
}

static ibc_reg_ref
move_to_payload(ibc_builder *b, ibc_reg_ref src, unsigned num_comps)
{
   if (src.file == IBC_REG_FILE_NONE) {
      return ibc_builder_new_logical_reg(b, IBC_TYPE_F, num_comps);
   } else {
      ibc_reg_ref dest = ibc_builder_new_logical_reg(b, src.type, num_comps);
      ibc_MOV_raw_vec_to(b, dest, src, num_comps);
      return dest;
   }
}

void
ibc_lower_io_fb_write_to_send(ibc_builder *b, ibc_send_instr *send,
                              const ibc_intrinsic_instr *write)
{
   assert(write->op == IBC_INTRINSIC_OP_FB_WRITE);

   const ibc_reg_ref color0 = write->src[IBC_FB_WRITE_SRC_COLOR0].ref;
   const ibc_reg_ref color1 = write->src[IBC_FB_WRITE_SRC_COLOR1].ref;
   assert(!color1.file);
   assert(!write->src[IBC_FB_WRITE_SRC_SRC0_ALPHA].ref.file);
   assert(!write->src[IBC_FB_WRITE_SRC_DEPTH].ref.file);
   assert(!write->src[IBC_FB_WRITE_SRC_STENCIL].ref.file);
   assert(!write->src[IBC_FB_WRITE_SRC_OMASK].ref.file);
   assert(write->src[IBC_FB_WRITE_SRC_TARGET].ref.file == IBC_REG_FILE_IMM);
   const unsigned target =
      *(uint32_t *)write->src[IBC_FB_WRITE_SRC_TARGET].ref.imm;
   assert(write->src[IBC_FB_WRITE_SRC_LAST_RT].ref.file == IBC_REG_FILE_IMM);
   const bool last_rt =
      *(uint32_t *)write->src[IBC_FB_WRITE_SRC_LAST_RT].ref.imm;
   bool has_header = false;

   send->payload[0] = move_to_payload(b, color0, 4);
   send->mlen = 4 * (write->instr.simd_width / 8);

   uint32_t msg_control;
   if (color1.file != IBC_REG_FILE_NONE) {
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
                        last_rt, 0 /* send_commit_msg */);

   send->has_header = has_header; /* TODO */
   send->has_side_effects = true;
   send->check_tdr = true;
   send->eot = last_rt &&
      write->instr.simd_group + write->instr.simd_width == b->shader->simd_width;
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
               bool use_rep_send, struct brw_vue_map *vue_map,
               char **error_str)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   const unsigned simd_width = 32;

   brw_nir_apply_key(shader, compiler, &key->base, simd_width, true);
   brw_nir_lower_fs_inputs(shader, compiler->devinfo, key);
   brw_nir_lower_fs_outputs(shader);

   if (!key->multisample_fbo)
      NIR_PASS_V(shader, brw_nir_demote_sample_qualifiers);
   NIR_PASS_V(shader, brw_nir_move_interpolation_to_top);
   brw_postprocess_nir(shader, compiler, true);

   brw_nir_populate_wm_prog_data(shader, compiler->devinfo, key, prog_data);

   struct nir_fs_to_ibc_state fs_state = {
      .max_simd_width = 32,
   };

   struct nir_to_ibc_state nti;
   nir_to_ibc_state_init(&nti, MESA_SHADER_FRAGMENT, compiler->devinfo,
                         &key->base, &prog_data->base,
                         &fs_state, simd_width, mem_ctx);

   nti.payload = &ibc_setup_fs_payload(&nti.b, prog_data, mem_ctx)->base;
   ibc_emit_nir_shader(&nti, shader);
   ibc_emit_fb_writes(&nti);

   ibc_shader *ibc = nir_to_ibc_state_finish(&nti);
   ibc_print_shader(ibc, stderr);
   ibc_validate_shader(ibc);
   fprintf(stderr, "\n\n");

   ibc_lower_and_optimize(ibc);

   switch (simd_width) {
   case 8:
      prog_data->dispatch_8 = true;
      prog_data->base.dispatch_grf_start_reg = nti.payload->num_ff_regs;
      break;
   case 16:
      prog_data->dispatch_16 = true;
      prog_data->dispatch_grf_start_reg_16 = nti.payload->num_ff_regs;
      break;
   case 32:
      prog_data->dispatch_32 = true;
      prog_data->dispatch_grf_start_reg_32 = nti.payload->num_ff_regs;
      break;
   default:
      unreachable("Invalid dispatch width");
   }

   return ibc_to_binary(ibc, mem_ctx, &prog_data->base.program_size);
}
