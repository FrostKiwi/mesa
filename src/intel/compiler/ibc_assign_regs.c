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

#include "ibc.h"
#include "ibc_builder.h"
#include "ibc_live_intervals.h"
#include "ibc_ra_reg_sets.h"

#include "brw_compiler.h"

#include "dev/gen_debug.h"

#include "util/register_allocate.h"

static bool
ibc_alu_instr_is_raw_mov(const ibc_alu_instr *alu)
{
   return alu->op == IBC_ALU_OP_MOV &&
          alu->dest.type == alu->src[0].ref.type &&
          alu->src[0].mod == IBC_ALU_SRC_MOD_NONE &&
          !alu->saturate;
}

static void
ref_set_reg_packed(ibc_ref ref)
{
   if (ref.file == IBC_FILE_HW_GRF) {
      assert(ref.hw_grf.hstride == ibc_type_byte_size(ref.type));
      assert(ref.hw_grf.vstride == ref.hw_grf.hstride * ref.hw_grf.width);
      return;
   }

   assert(ref.file == IBC_FILE_LOGICAL);

   ibc_reg *reg = (ibc_reg *)ref.reg;
   if (reg->logical.packed)
      return;

   if (reg->logical.stride == 0)
      reg->logical.stride = ibc_type_byte_size(ref.type);
   assert(reg->logical.stride == ibc_type_byte_size(ref.type));
   reg->logical.packed = true;
}

bool
ibc_assign_logical_reg_strides(ibc_shader *shader)
{
   bool progress = false;

   ibc_foreach_instr(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_SEND)
         continue;

      ibc_send_instr *send = ibc_instr_as_send(instr);

      if (send->rlen > 0)
         ref_set_reg_packed(send->dest);

      if (send->mlen > 0)
         ref_set_reg_packed(send->payload[0]);

      if (send->ex_mlen > 0)
         ref_set_reg_packed(send->payload[1]);
   }

   ibc_foreach_reg(reg, shader) {
      if (reg->file != IBC_FILE_LOGICAL)
         continue;

      /* Scalars and booleans won't be mapped to HW regs so they don't need to
       * have assigned strides.
       */
      if (reg->logical.bit_size == 1)
         continue;

      unsigned max_exec_width = reg->logical.bit_size / 8;
      ibc_reg_foreach_write(write, reg) {
         if (write->instr->type != IBC_INSTR_TYPE_ALU)
            continue;

         ibc_alu_instr *alu = ibc_instr_as_alu(write->instr);
         enum ibc_type exec_type = ibc_alu_instr_exec_type(alu);

         /* We don't use ibc_type_byte_size here because we want to gracefully
          * ignore the source if it's IBC_TYPE_FLAG.
          */
         max_exec_width = MAX2(max_exec_width,
                               ibc_type_bit_size(exec_type) / 8);

         /* Only raw MOV supports a packed-byte destination */
         if (!ibc_alu_instr_is_raw_mov(alu))
            max_exec_width = MAX2(max_exec_width, 2);

         /* ALIGN1 3src instructions on gen10+ only have bits [4:3] of the
          * subnr so we have to align to 8 bytes.
          */
         if (shader->devinfo->gen >= 10 && reg->logical.align < 8) {
            reg->logical.align = 8;
            progress = true;
         }
      }

      if (reg->logical.align < max_exec_width) {
         reg->logical.align = max_exec_width;
         progress = true;
      }

      /* Uniform values don't have a stride across SIMD channels */
      if (reg->logical.simd_width > 1) {
         if (reg->logical.stride == 0) {
            reg->logical.stride = max_exec_width;
            progress = true;
         } else {
            assert(reg->logical.stride >= max_exec_width);
         }
      }
   }

   return progress;
}

static bool
should_assign_reg(const ibc_reg *reg)
{
   return reg->file == IBC_FILE_LOGICAL ||
          reg->file == IBC_FILE_HW_GRF;
}

struct ibc_ra_singleton {
   /* One for each SIMD width */
   ibc_ra_reg_set reg_sets[3];
};

void
ibc_assign_regs_init(struct brw_compiler *compiler)
{
   struct ibc_ra_singleton *ra = rzalloc(compiler, struct ibc_ra_singleton);
   for (unsigned i = 0; i < 3; i++)
      ibc_ra_reg_set_init(&ra->reg_sets[i], 8 << i, ra);
   compiler->ibc_data = ra;
}

static const ibc_ra_reg_set *
brw_compiler_get_ibc_reg_set(const struct brw_compiler *compiler,
                             uint8_t simd_width)
{
   const struct ibc_ra_singleton *ra = compiler->ibc_data;

   assert(simd_width == 8 || simd_width == 16 || simd_width == 32);
   int idx = ffs(simd_width) - 4;
   assert(idx >= 0 && idx < ARRAY_SIZE(ra->reg_sets));
   return &ra->reg_sets[idx];
}

static bool
ibc_reg_has_strided_class(const ibc_reg *reg, const ibc_ra_reg_set *set)
{
   if (reg->file != IBC_FILE_LOGICAL)
      return false;

   if (reg->logical.simd_width < 4)
      return false;

   if (reg->logical.num_comps > 4)
      return false;

   assert(reg->logical.stride > 0);
   uint16_t comp_size = (uint16_t)reg->logical.stride *
                        (uint16_t)reg->logical.simd_width;
   return reg->logical.simd_width == set->simd_width && comp_size <= 64;
}

static const ibc_ra_reg_class *
ibc_reg_to_class(const ibc_reg *reg, const ibc_ra_reg_set *set)
{
   switch (reg->file) {
   case IBC_FILE_LOGICAL:
      assert(reg->logical.bit_size % 8 == 0);
      if (reg->logical.simd_width < 4) {
         assert(reg->logical.simd_width == 1);
         uint16_t phys_size = (reg->logical.bit_size / 8) *
                              reg->logical.num_comps;
         /* TODO: We can probably do something better than this */
         phys_size = MAX2(phys_size, reg->logical.align);
         return ibc_ra_reg_set_get_physical_class(set, phys_size);
      } else {
         assert(reg->logical.simd_width >= 8);
         assert(reg->logical.stride > 0);
         if (ibc_reg_has_strided_class(reg, set)) {
            return ibc_ra_reg_set_get_strided_class(set, reg->logical.stride,
                                                    reg->logical.num_comps,
                                                    reg->logical.simd_width);
         } else {
            uint16_t phys_size = (uint16_t)reg->logical.stride *
                                 (uint16_t)reg->logical.simd_width *
                                 (uint16_t)reg->logical.num_comps;
            return ibc_ra_reg_set_get_physical_class(set, phys_size);
         }
      }

   case IBC_FILE_HW_GRF:
      assert(reg->hw_grf.align <= 32);
      return ibc_ra_reg_set_get_physical_class(set, ALIGN(reg->hw_grf.size,
                                                          reg->hw_grf.align));

   default:
      unreachable("Unsupported register fil");
   }
}

static void
split_load_payloads(ibc_shader *shader)
{
   struct ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_safe(instr, shader) {
      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      if (intrin->op != IBC_INTRINSIC_OP_LOAD_PAYLOAD  &&
          intrin->op != IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO)
         continue;

      if (intrin->src[0].ref.file == IBC_FILE_NONE)
         continue;

      /* We only care about splitting HW_GRF regs */
      if (intrin->dest.file != IBC_FILE_HW_GRF)
         continue;

      assert(intrin->dest.reg);
      assert(ibc_reg_ssa_instr(intrin->dest.reg) == instr);
      assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
      assert(intrin->src[0].ref.reg == NULL);

      /* They should also have trivial packed strides */
      assert(intrin->src[0].ref.hw_grf.hstride ==
             ibc_type_byte_size(intrin->src[0].ref.type));
      assert(intrin->src[0].ref.hw_grf.vstride ==
             intrin->src[0].ref.hw_grf.hstride *
             intrin->src[0].ref.hw_grf.width);

      assert(intrin->dest.hw_grf.hstride ==
             ibc_type_byte_size(intrin->dest.type));
      assert(intrin->dest.hw_grf.vstride ==
             intrin->dest.hw_grf.hstride *
             intrin->dest.hw_grf.width);

      /* We're also going to assume that one vector component fits in a single
       * register.
       */
      unsigned comp_size = intrin->instr.simd_width *
                           ibc_type_byte_size(intrin->dest.type);
      assert(comp_size <= 32);

      if (comp_size * intrin->num_dest_comps <= 32)
         continue;

      assert(32 % comp_size == 0);
      unsigned comps_per_split = 32 / comp_size;

      b.cursor = ibc_before_instr(instr);
      if (intrin->instr.we_all) {
         ibc_builder_push_we_all(&b, intrin->instr.simd_width);
      } else {
         ibc_builder_push_group(&b, intrin->instr.simd_group,
                                    intrin->instr.simd_width);
      }

      ibc_ref split_dest = intrin->dest;
      ibc_intrinsic_src split_srcs[3];
      assert(intrin->num_srcs <= ARRAY_SIZE(split_srcs));
      for (unsigned i = 0; i < intrin->num_srcs; i++)
         split_srcs[i] = intrin->src[i];

      for (unsigned c = 0; c < intrin->num_dest_comps; c += comps_per_split) {
         unsigned num_comps = comps_per_split;
         if (c + comps_per_split > intrin->num_dest_comps)
            num_comps = intrin->num_dest_comps - c;

         split_srcs[0].num_comps = num_comps;
         ibc_build_intrinsic(&b, intrin->op, split_dest, -1, num_comps,
                             split_srcs, intrin->num_srcs);

         split_dest.hw_grf.byte += 32;
         split_srcs[0].ref.hw_grf.byte += 32;
         if (intrin->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO) {
            unsigned offset = ibc_ref_as_uint(split_srcs[2].ref);
            split_srcs[2].ref = ibc_imm_ud(offset + 32);
         }
      }

      ibc_instr_remove(&intrin->instr);
   }
}

struct ibc_assign_regs_gc_state {
   void *mem_ctx;

   /* Current instruction when iterating */
   ibc_instr *iter_instr;

   /* True if we're currently walking instruction reads */
   bool is_read;

   bool have_spill_costs;

   const ibc_ra_reg_set *reg_set;
   const ibc_live_intervals *live;

   /** Mapping from spill temp reg::index to the IP where they are live */
   struct util_dynarray spill_temp_live_ip;

   /** Header register to be used in spill/fill messages */
   ibc_reg *spill_header;

   /** Mapping from ibc_reg::index to the assigned ibc_ra_reg_class */
   const struct ibc_ra_reg_class **reg_class;

   struct ra_graph *g;
   int grf127_send_hack_node;
   unsigned num_hack_nodes;

   unsigned program_start_ip;

   unsigned last_scratch;
};

static unsigned
reg_index_to_ra_node(const struct ibc_assign_regs_gc_state *state,
                     unsigned reg_index)
{
   return state->num_hack_nodes + reg_index;
}

static unsigned
reg_to_ra_node(const struct ibc_assign_regs_gc_state *state,
               const ibc_reg *reg)
{
   return reg_index_to_ra_node(state, reg->index);
}

static ibc_ref
convert_logical_to_hw_grf(const ibc_ref *ref,
                          bool is_read,
                          uint8_t simd_group,
                          ibc_reg *new_vgrf,
                          unsigned grf_byte)
{
   if (ref->file != IBC_FILE_LOGICAL || !ref->reg)
      return (ibc_ref){};

   assert(!new_vgrf || new_vgrf->file == IBC_FILE_HW_GRF);

   const ibc_reg *reg = ref->reg;

   ibc_ref new_ref = {
      .file = IBC_FILE_HW_GRF,
      .type = ref->type,
      .reg = new_vgrf,
   };

   assert(reg->logical.bit_size % 8 == 0);

   /* Stash this so we can access it unchanged */
   unsigned stride;
   if (reg->logical.simd_width == 1) {
      assert(reg->logical.stride == 0);
      new_ref.hw_grf.byte = grf_byte;
      new_ref.hw_grf.byte += ref->logical.comp *
                             (reg->logical.bit_size / 8);
      new_ref.hw_grf.byte += ref->logical.byte;
      /* This is needed for the !state->is_read case below */
      stride = reg->logical.bit_size / 8;
   } else {
      assert(reg->logical.stride >= reg->logical.bit_size / 8);
      new_ref.hw_grf.byte = grf_byte;
      new_ref.hw_grf.byte += ref->logical.comp *
                             reg->logical.simd_width *
                             reg->logical.stride;
      new_ref.hw_grf.byte += ref->logical.byte;
      stride = reg->logical.stride;
   }

   if (ref->logical.broadcast) {
      new_ref.hw_grf.byte += ref->logical.simd_channel * stride;
   } else if (reg->logical.simd_width > 1) {
      new_ref.hw_grf.byte +=
         (simd_group - reg->logical.simd_group) * stride;
   }

   if (ref->logical.broadcast || reg->logical.simd_width == 1) {
      if (is_read) {
         new_ref.hw_grf.vstride = 0;
         new_ref.hw_grf.width = 1;
         new_ref.hw_grf.hstride = 0;
      } else {
         new_ref.hw_grf.hstride = ibc_type_byte_size(ref->type);
         new_ref.hw_grf.width = 8;
         new_ref.hw_grf.vstride = new_ref.hw_grf.hstride *
                                  new_ref.hw_grf.width;
      }
   } else {
      new_ref.hw_grf.hstride = stride;
      new_ref.hw_grf.width = 8;
      new_ref.hw_grf.vstride = new_ref.hw_grf.hstride *
                               new_ref.hw_grf.width;
   }

   return new_ref;
}

static bool
rewrite_ref_from_gc_graph(ibc_ref *_ref,
                          int num_bytes, int num_comps,
                          uint8_t simd_group, uint8_t simd_width,
                          void *_state)
{
   const ibc_ref *ref = _ref;
   struct ibc_assign_regs_gc_state *state = _state;
   if (ref->file != IBC_FILE_LOGICAL &&
       ref->file != IBC_FILE_HW_GRF)
      return true;

   if (ref->reg == NULL)
      return true;

   const ibc_reg *reg = ref->reg;
   const unsigned node = reg_to_ra_node(state, reg);
   const unsigned ra_reg = ra_get_node_reg(state->g, node);
   assert(ra_reg != ~0U);
   const unsigned grf_byte = state->reg_set->ra_reg_to_grf[ra_reg];

   ibc_ref new_ref = {
      .file = IBC_FILE_HW_GRF,
      .type = ref->type,
   };
   switch (ref->file) {
   case IBC_FILE_HW_GRF:
      new_ref.hw_grf = ref->hw_grf;
      new_ref.hw_grf.byte += grf_byte;
      break;

   case IBC_FILE_LOGICAL: {
      new_ref = convert_logical_to_hw_grf(ref, state->is_read, simd_group,
                                          NULL, grf_byte);
      assert(new_ref.file != IBC_FILE_NONE);
      break;
   }

   default:
      unreachable("Unhandled register file");
   }

   ibc_instr_set_ref(state->iter_instr, _ref, new_ref);

   return true;
}

static void
ibc_pin_payload_and_eot_regs(struct ibc_assign_regs_gc_state *state,
                             ibc_instr *instr)
{
   struct ra_graph *g = state->g;

   switch (instr->type) {
   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);

      /* We can't validate this in ibc_validate because .packed gets set as
       * a late-binding thing.  However, before we RA, we should assert
       * that things are packed.
       */
      if (send->rlen > 0 && send->dest.file == IBC_FILE_LOGICAL)
         assert(send->dest.reg->logical.packed);
      if (send->payload[0].file == IBC_FILE_LOGICAL)
         assert(send->payload[0].reg->logical.packed);
      if (send->ex_mlen > 0 && send->payload[1].file == IBC_FILE_LOGICAL)
         assert(send->payload[1].reg->logical.packed);

      if (send->eot) {
         unsigned byte = TOTAL_GRF_BYTES;
         if (send->ex_mlen > 0) {
            assert(send->payload[1].file == IBC_FILE_LOGICAL ||
                   send->payload[1].file == IBC_FILE_HW_GRF);

            const ibc_ra_reg_class *c =
               ibc_reg_to_class(send->payload[1].reg, state->reg_set);
            byte -= c->reg_size;
            ra_set_node_reg(g, reg_to_ra_node(state, send->payload[1].reg),
                            ibc_ra_reg_class_grf_to_reg(c, byte));
         }

         const ibc_ra_reg_class *c =
            ibc_reg_to_class(send->payload[0].reg, state->reg_set);
         byte -= c->reg_size;
         ra_set_node_reg(g, reg_to_ra_node(state, send->payload[0].reg),
                         ibc_ra_reg_class_grf_to_reg(c, byte));
      }
      break;
   }

   case IBC_INSTR_TYPE_INTRINSIC: {
      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      if (intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD ||
          (intrin->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO &&
           intrin->src[0].ref.file != IBC_FILE_NONE)) {
         assert(intrin->dest.reg);
         assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
         assert(intrin->src[0].ref.reg == NULL);

         const ibc_ra_reg_class *c =
            ibc_reg_to_class(intrin->dest.reg, state->reg_set);
         unsigned byte = intrin->src[0].ref.hw_grf.byte -
                         intrin->dest.hw_grf.byte;
         unsigned node = reg_to_ra_node(state, intrin->dest.reg);
         if (ra_get_node_reg(g, node) == ~0U)
            ra_set_node_reg(g, node, ibc_ra_reg_class_grf_to_reg(c, byte));
      }
      break;
   }

   default:
      break;
   }
}

#define NEVER_SPILL_COST (-1.0f)

struct ibc_spill_cost_state {
   bool is_read;
   float block_scale;
   float *cost;
};

static bool
count_cost_cb(ibc_ref *ref,
              int num_bytes, int num_comps,
              uint8_t simd_group, uint8_t simd_width,
              void *data)
{
   struct ibc_spill_cost_state *st = data;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM || !ref->reg)
      return true;

   struct ibc_ref grf;

   if (ref->file == IBC_FILE_LOGICAL) {
      grf = convert_logical_to_hw_grf(ref, st->is_read, simd_group, NULL, 0);
   } else {
      grf = *ref;
   }

   unsigned min, max;
   ibc_calc_hw_grf_range(&grf, num_bytes, num_comps, simd_width, &min, &max);

   assert(max > min);

   /* Weight fills as being more expensive than spills since we likely
    * have to stall for their results, while spills are fire-and-forget.
    */
   float weight = st->is_read ? 10 : 1;

   if (st->cost[ref->reg->index] >= 0)
      st->cost[ref->reg->index] += weight * (max - min);

   return true;
}

static void
ibc_gc_set_spill_costs(struct ibc_assign_regs_gc_state *state,
                       ibc_shader *shader)
{
   struct ibc_spill_cost_state st = {
      .block_scale = 1.0,
      .cost = calloc(state->live->num_regs, sizeof(float)),
   };

   /* Calculate costs for spilling nodes.  Call it a cost of 1 per
    * spill/unspill we'll have to do, and guess that the insides of
    * loops run 10 times.
    */
   ibc_foreach_instr_safe(instr, shader) {
      ibc_intrinsic_instr *intrin = instr->type == IBC_INSTR_TYPE_INTRINSIC ?
                                    ibc_instr_as_intrinsic(instr) : NULL;

      if (ibc_instr_is_load_payload(instr)) {
         /* We don't support spilling unaligned payload registers yet. */
         if (intrin->src[0].ref.hw_grf.byte % REG_SIZE)
            st.cost[intrin->dest.reg->index] = -1000.0f;
      } else if (intrin && intrin->op == IBC_INTRINSIC_OP_PLN &&
                 instr->simd_width > 8) {
         /* SIMD16 PLNs need adjacent src[1..2]; we don't handle that yet. */
         st.cost[intrin->src[1].ref.reg->index] = -1000.0f;
         st.cost[intrin->src[2].ref.reg->index] = -1000.0f;
      } else {
         st.is_read = true;
         ibc_instr_foreach_read(instr, count_cost_cb, &st);
         st.is_read = false;
         ibc_instr_foreach_write(instr, count_cost_cb, &st);
      }

      if (instr->type == IBC_INSTR_TYPE_FLOW) {
         ibc_flow_instr *flow = ibc_instr_as_flow(instr);
         switch (flow->op) {
         case IBC_FLOW_OP_DO:
            st.block_scale *= 10;
            break;
         case IBC_FLOW_OP_WHILE:
            st.block_scale /= 10;
            break;
         case IBC_FLOW_OP_IF:
            st.block_scale *= 0.5;
            break;
         case IBC_FLOW_OP_ENDIF:
            st.block_scale /= 0.5;
            break;
         default:
            break;
         }
      }
   }

   ibc_foreach_reg(reg, shader) {
      if (!should_assign_reg(reg))
         continue;

      ibc_reg_live_intervals *rli = &state->live->regs[reg->index];
      int live_length = rli->physical_end - rli->physical_start;
      if (live_length <= 0)
         continue;

      /* Divide the cost (in number of spills/fills) by the log of the length
       * of the live range of the register.  This will encourage spill logic
       * to spill long-living things before spilling short-lived things where
       * spilling is less likely to actually do us any good.  We use the log
       * of the length because it will fall off very quickly and not cause us
       * to spill medium length registers with more uses.
       */
      float adjusted_cost = st.cost[reg->index] / logf(live_length);
      const unsigned reg_node = reg_to_ra_node(state, reg);

      ra_set_node_spill_cost(state->g, reg_node, adjusted_cost);
   }

   /* It makes no sense to spill g0; it's required to do a spill */
   ra_set_node_spill_cost(state->g, reg_to_ra_node(state, shader->g0),
                          NEVER_SPILL_COST);

   state->have_spill_costs = true;

   free(st.cost);
}

static unsigned
choose_spill_reg(struct ibc_assign_regs_gc_state *state,
                 ibc_shader *shader)
{
   if (!state->have_spill_costs)
      ibc_gc_set_spill_costs(state, shader);

   int node = ra_get_best_spill_node(state->g);
   if (node < 0)
      return NO_REG;

   assert(node >= state->num_hack_nodes);
   return node - state->num_hack_nodes;
}

static void
setup_oword_block_header(ibc_builder *b,
                         ibc_reg *header_reg,
                         unsigned scratch_offset_B,
                         unsigned instr_ip)
{
   ibc_alu_instr *alu;

   ibc_builder_push_scalar(b);
   ibc_ref offset_ref = ibc_typed_ref(header_reg, IBC_TYPE_UD);
   offset_ref.hw_grf.byte += 2 * sizeof(uint32_t);
   alu = ibc_MOV_to(b, offset_ref, ibc_imm_ud(scratch_offset_B / 16));
   alu->instr.index = instr_ip;
   ibc_builder_pop(b);
}

/**
 * Emit an OWord block read message to fill from scratch space.
 *
 * Assumes `msg` contains the message header, and the destination
 * should be 1 REG_SIZE over from that.
 */
static void
emit_unspill(ibc_builder *b,
             ibc_reg *header_reg,
             ibc_ref dest_ref,
             uint8_t simd_width,
             unsigned scratch_offset_B,
             unsigned num_regs,
             unsigned instr_ip)
{
   while (num_regs > 0) {
      const unsigned block_size_regs = 1 << util_logbase2(MIN2(num_regs, 4));
      const unsigned block_size_bytes = block_size_regs * REG_SIZE;

      setup_oword_block_header(b, header_reg, scratch_offset_B, instr_ip);

      /* Always use SIMD16...SIMD8 messages are horribly broken. */
      ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 16);
      send->instr.index = instr_ip;
      send->can_reorder = false;
      send->has_side_effects = true;
      send->has_header = true;
      send->instr.we_all = true;
      send->sfid = GEN7_SFID_DATAPORT_DATA_CACHE;
      send->mlen = 1;
      send->rlen = block_size_regs;
      send->desc_imm =
         brw_dp_read_desc(b->shader->devinfo, GEN8_BTI_STATELESS_NON_COHERENT,
                          BRW_DATAPORT_OWORD_BLOCK_DWORDS(block_size_regs * 8),
                          GEN7_DATAPORT_DC_OWORD_BLOCK_READ,
                          BRW_DATAPORT_READ_TARGET_DATA_CACHE);
      send->payload[0] = ibc_typed_ref(header_reg, IBC_TYPE_UD);
      send->dest = dest_ref;

      ibc_builder_insert_instr(b, &send->instr);

      scratch_offset_B += block_size_bytes;
      dest_ref.hw_grf.byte += block_size_bytes;
      num_regs -= block_size_regs;

      b->shader->stats.fills++;
   }
}

/**
 * Emit an OWord block write message to spill to scratch space.
 *
 * Assumes `msg` already has the header set up and the payload data.
 */
static void
emit_spill(ibc_builder *b,
           ibc_reg *header_reg,
           ibc_ref data_ref,
           uint8_t simd_width,
           unsigned scratch_offset_B,
           unsigned num_regs,
           unsigned instr_ip)
{
   while (num_regs > 0) {
      const unsigned block_size_regs = 1 << util_logbase2(MIN2(num_regs, 4));
      const unsigned block_size_bytes = block_size_regs * REG_SIZE;

      setup_oword_block_header(b, header_reg, scratch_offset_B, instr_ip);

      /* Always use SIMD16...SIMD8 messages are horribly broken. */
      ibc_send_instr *send = ibc_send_instr_create(b->shader, 0, 16);
      send->instr.index = instr_ip;
      send->can_reorder = false;
      send->has_side_effects = true;
      send->has_header = true;
      send->instr.we_all = true;
      send->sfid = GEN7_SFID_DATAPORT_DATA_CACHE;
      send->rlen = 0;
      send->mlen = 1;
      send->ex_mlen = block_size_regs;
      send->desc_imm =
         brw_dp_write_desc(b->shader->devinfo, GEN8_BTI_STATELESS_NON_COHERENT,
                           BRW_DATAPORT_OWORD_BLOCK_DWORDS(block_size_regs * 8),
                           GEN7_DATAPORT_DC_OWORD_BLOCK_WRITE, 0, 0);
      send->payload[0] = ibc_typed_ref(header_reg, IBC_TYPE_UD);
      send->payload[1] = data_ref,

      ibc_builder_insert_instr(b, &send->instr);

      scratch_offset_B += block_size_bytes;
      data_ref.hw_grf.byte += block_size_bytes;
      num_regs -= block_size_regs;

      b->shader->stats.spills++;
   }
}

struct ibc_spill_fill_state {
   ibc_builder *b;

   /** Current instruction being updated */
   ibc_instr *iter_instr;

   /** Register to spill */
   unsigned reg_idx;

   /** [min, max] interval in GRF units */
   unsigned min, max;

   /** Are we processing a read?  (The same callback is used for both.) */
   bool is_read;

   /** Temporary to use for spill/fills at this instruction */
   ibc_reg *temp_reg;
};

static bool
retarget_ref_and_update_range(ibc_ref *ref,
                              int num_bytes, int num_comps,
                              uint8_t simd_group, uint8_t simd_width,
                              void *_st)
{
   struct ibc_spill_fill_state *st = _st;

   /* Bail if this isn't accessing the register we're spilling. */
   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM ||
       !ref->reg || ref->reg->index != st->reg_idx)
      return true;

   struct ibc_ref new_ref;

   if (ref->file == IBC_FILE_LOGICAL) {
      /* Make this instead reference our temporary HW_GRF. */
      new_ref = convert_logical_to_hw_grf(ref, st->is_read, simd_group,
                                          st->temp_reg, 0);
   } else {
      /* Retarget to the temporary HW_GRF. */
      new_ref = *ref;
      new_ref.reg = st->temp_reg;
   }

   ibc_instr_set_ref(st->iter_instr, ref, new_ref);

   /* Expand the range of accessed data */
   unsigned min, max;
   ibc_calc_hw_grf_range(&new_ref, num_bytes, num_comps, simd_width,
                         &min, &max);

   st->min = MIN2(st->min, min);
   st->max = MAX2(st->max, max);

   return true;
}

static const uint16_t SPILL_PLACEHOLDER_SIZE = UINT16_MAX;

static bool
shrink_spill_temp_ref(ibc_ref *ref,
                      int num_bytes, int num_comps,
                      uint8_t simd_group, uint8_t simd_width,
                      void *_st)
{
   struct ibc_spill_fill_state *st = _st;

   /* Note that SPILL_PLACEHOLDER_SIZE is unique to our temporaries;
    * it's far larger than any actual legal register size.
    */
   if (ref->file == IBC_FILE_HW_GRF && ref->reg &&
       ref->reg->hw_grf.size == SPILL_PLACEHOLDER_SIZE) {
      ref->hw_grf.byte -= st->min * REG_SIZE;
   }
   return true;
}

static unsigned
calc_first_non_payload_ip(struct ibc_shader *shader)
{
   const ibc_instr *after_start =
      LIST_ENTRY(ibc_instr, shader->instrs.next->next, link);

   ibc_foreach_instr_from(instr, shader, after_start) {
      if (ibc_instr_is_load_payload(instr))
         continue;
      return instr->index;
   }
   unreachable("end instruction prevents this");
}

static bool
ibc_spill_reg(struct ibc_assign_regs_gc_state *state,
              struct ibc_builder *b,
              unsigned reg_idx)
{
   struct ra_graph *g = state->g;
   const struct ibc_ra_reg_class *class = state->reg_class[reg_idx];
   unsigned size = ALIGN(class->reg_size, REG_SIZE);

   if (!state->spill_header) {
      /* Copy g0 to a distinct temporary for spill message headers. */
      ibc_reg *header = state->spill_header =
         ibc_hw_grf_reg_create(b->shader, REG_SIZE, REG_SIZE);
      const ibc_ra_reg_class *c = ibc_reg_to_class(header, state->reg_set);
      unsigned header_node = ra_add_node(g, c->nr);
      ra_set_node_spill_cost(g, header_node, NEVER_SPILL_COST);
      header->index = header_node - state->num_hack_nodes;
      header->is_wlr = false;

      unsigned g0_node = reg_to_ra_node(state, b->shader->g0);
      ra_add_node_interference(g, header_node, g0_node);

      b->cursor = ibc_after_payload(b->shader);
      ibc_builder_push_we_all(b, 8);
      ibc_MOV_to(b, ibc_typed_ref(header, IBC_TYPE_UD),
                 ibc_typed_ref(b->shader->g0, IBC_TYPE_UD));
      ibc_builder_pop(b);
   }

   /* We're about to replace all uses of this register, which means it
    * won't conflicts with anything.  We can drop its interference.
    */
   unsigned orig_node = reg_index_to_ra_node(state, reg_idx);
   ra_set_node_spill_cost(g, orig_node, NEVER_SPILL_COST);
   ra_set_node_reg(g, orig_node, NO_REG);
   ra_reset_node_interference(g, orig_node);

   ibc_instr *spill_header_init =
      LIST_ENTRY(ibc_reg_write, state->spill_header->writes.next, link)->instr;

   struct ibc_spill_fill_state st = {
      .b = b,
      .reg_idx = reg_idx,
   };

   unsigned spill_offset_B = state->last_scratch;

   state->last_scratch += size;

   st.temp_reg =
      ibc_hw_grf_reg_create(b->shader, SPILL_PLACEHOLDER_SIZE, REG_SIZE);

   ibc_foreach_instr_safe(instr, b->shader) {
      st.iter_instr = instr;

      const bool is_payload = ibc_instr_is_load_payload(instr);

      if (0) {
         fprintf(stderr, "spill debug: [%02u] ", instr->index);
         ibc_print_instr(stderr, instr);
      }

      st.min = ~0;
      st.max = 0;
      st.is_read = true;
      ibc_instr_foreach_read(instr, retarget_ref_and_update_range, &st);

      unsigned unspill_min = st.min;
      unsigned unspill_max = st.max;

      st.min = ~0;
      st.max = 0;
      st.is_read = false;
      ibc_instr_foreach_write(instr, retarget_ref_and_update_range, &st);

      const unsigned spill_min = st.min;
      const unsigned spill_max = st.max;

      st.min = MIN2(spill_min, unspill_min);
      st.max = MAX2(spill_max, unspill_max);

      /* Continue on if this instruction doesn't reference our register. */
      if (st.min == ~0)
         continue;

      /* TODO: Only do this when necessary:
       * 1. Inside non-uniform control flow
       * 2. Predicated instruction with non-uniform condition
       * 3. Partial writes...
       *
       * We could also look at disabling nomask when things line up,
       * which would prevent the need for this as well.
       */
      if (!is_payload) {
         unspill_min = st.min;
         unspill_max = st.max;
      }

      /* Set up the temporary register. */

      if (is_payload) {
         /* For load_payload destinations, fix the register to be an exact
          * match for the one we're replacing, as it needs to be in a fixed
          * location.  We'll pin it there shortly.
          */
         assert(st.min == 0);
         st.temp_reg->hw_grf.size = size;
      } else {
         /* Now that we know what subrange this instruction accesses, we can
          * shrink our temporary register to only hold that much data.
          */
         if (st.min != 0) {
            /* Subtract st.min * REG_SIZE so references are zero-based. */
            ibc_instr_foreach_read(instr, shrink_spill_temp_ref, &st);
            ibc_instr_foreach_write(instr, shrink_spill_temp_ref, &st);
         }

         st.temp_reg->hw_grf.size = (st.max - st.min) * REG_SIZE;
      }

      assert(st.temp_reg->hw_grf.size <= size);

      const ibc_ra_reg_class *c =
         ibc_reg_to_class(st.temp_reg, state->reg_set);

      unsigned temp_node = ra_add_node(g, c->nr);

      st.temp_reg->index = temp_node - state->num_hack_nodes;
      ra_set_node_spill_cost(g, temp_node, NEVER_SPILL_COST);

      ibc_pin_payload_and_eot_regs(state, instr);

      /* Record that our temporary is live at this IP */
      if (!util_dynarray_resize(&state->spill_temp_live_ip, unsigned,
                                st.temp_reg->index + 1))
         return false;

      unsigned new_ip = -instr->index;

      unsigned *temp_live_ip = util_dynarray_begin(&state->spill_temp_live_ip);
      temp_live_ip[st.temp_reg->index] = new_ip;

      /* Our new temporaries need to conflict with other live registers. */
      ibc_foreach_reg(other, b->shader) {
         if (!should_assign_reg(other) ||
             other->index == reg_idx ||
             other->index == st.temp_reg->index)
            continue;

         const unsigned other_node = reg_to_ra_node(state, other);

         bool interferes = false;

         if (instr->index < state->program_start_ip &&
             ibc_instr_is_load_payload(ibc_reg_ssa_instr(other))) {
            /* Payload fields all interfere with one another. */
            interferes = true;
         } else if (other->index < state->live->num_regs) {
            /* `other` is a normal register, use liveness to determine
             * whether it interferes with our new temporaries.
             */
            const ibc_reg_live_intervals *oli =
               &state->live->regs[other->index];

            interferes = !(MAX2(instr->index, state->program_start_ip)
                             <= oli->physical_start ||
                           oli->physical_end <= instr->index);
         } else {
            /* `other` is a temporary from an earlier spill operation, which
             * is alive for one IP (in the original program before spilling).
             * We might have spilled one source of this instruction in an
             * earlier pass, and be spilling a second source now, at which
             * point the temporaries for both spills will be live at the
             * same time and need to interfere.
             */
            interferes = temp_live_ip[other->index] == new_ip;
         }

         if (interferes)
            ra_add_node_interference(g, temp_node, other_node);
      }

      /* Fill the existing values from scratch space */
      if (unspill_min != ~0) {
         ibc_ref temp_ref = ibc_typed_ref(st.temp_reg, IBC_TYPE_UD);
         temp_ref.hw_grf.byte += (unspill_min - st.min) * REG_SIZE;

         b->cursor = ibc_before_instr(instr);

         emit_unspill(b, state->spill_header, temp_ref, instr->simd_width,
                      spill_offset_B + unspill_min * REG_SIZE,
                      unspill_max - unspill_min,
                      new_ip);
      }

      /* Spill the new values back to scratch space */
      if (spill_min != ~0) {
         ibc_ref temp_ref = ibc_typed_ref(st.temp_reg, IBC_TYPE_UD);
         temp_ref.hw_grf.byte += (spill_min - st.min) * REG_SIZE;

         b->cursor = ibc_after_instr(is_payload ? spill_header_init : instr);

         emit_spill(b, state->spill_header, temp_ref, instr->simd_width,
                    spill_offset_B + spill_min * REG_SIZE,
                    spill_max - spill_min,
                    new_ip);
      }

      /* Set up a fresh temporary for the next iteration */
      st.temp_reg =
         ibc_hw_grf_reg_create(b->shader, SPILL_PLACEHOLDER_SIZE, REG_SIZE);
   }

   /* Make the spill header conflict with everything. */
   unsigned header_node = reg_to_ra_node(state, state->spill_header);

   ibc_foreach_reg(other, b->shader) {
      if (!should_assign_reg(other))
         continue;

      const unsigned other_node = reg_to_ra_node(state, other);
      ra_add_node_interference(g, other_node, header_node);
   }

   ibc_repair_wlr_order(b->shader);

   return true;
}

static bool
ibc_assign_regs_graph_color(ibc_shader *shader,
                            const struct brw_compiler *compiler,
                            bool allow_spilling)
{
   IBC_PASS_V(shader, split_load_payloads);
   IBC_PASS_V(shader, ibc_split_regs);
   IBC_PASS_V(shader, ibc_opt_dead_code);

   ibc_assign_logical_reg_strides(shader);

   struct ibc_assign_regs_gc_state state = {
      .mem_ctx = ralloc_context(shader),
      .reg_set = brw_compiler_get_ibc_reg_set(compiler, shader->simd_width),
      .last_scratch = shader->scratch_B,
   };

   state.live = ibc_compute_live_intervals(shader, should_assign_reg,
                                           state.mem_ctx);

   util_dynarray_init(&state.spill_temp_live_ip, state.mem_ctx);

   unsigned node_count = 0;
   state.grf127_send_hack_node = node_count++;
   state.num_hack_nodes = node_count;
   node_count += state.live->num_regs;

   struct ra_graph *g = state.g =
      ra_alloc_interference_graph(state.reg_set->regs, node_count);
   ralloc_steal(state.mem_ctx, state.g);

   ra_set_node_reg(g, state.grf127_send_hack_node,
                   ibc_ra_reg_set_grf_to_reg(state.reg_set, 127 * 32, 32));

   /* Liveness analysis gives us one live set per chunk.  We need a live set
    * per reg because graph coloring doesn't have any finer granularity.
    */
   struct interval_set **reg_logical_live =
      rzalloc_array(state.mem_ctx, struct interval_set *, state.live->num_regs);
   for (unsigned i = 0; i < state.live->num_regs; i++) {
      struct interval_set *reg_live = NULL;
      for (unsigned c = 0; c < state.live->regs[i].num_chunks; c++) {
         struct interval_set *chunk_live =
            state.live->regs[i].chunks[c].logical;
         if (reg_live != NULL && chunk_live != NULL) {
            reg_live = interval_set_from_union(state.mem_ctx,
                                               reg_live, chunk_live);
         } else if (chunk_live != NULL) {
            reg_live = chunk_live;
         }
      }
      reg_logical_live[i] = reg_live;
   }

   state.reg_class = rzalloc_array(state.mem_ctx, const ibc_ra_reg_class *,
                                   state.live->num_regs);

   ibc_foreach_reg(reg, shader) {
      if (!should_assign_reg(reg))
         continue;

      const unsigned reg_node = reg_to_ra_node(&state, reg);
      const ibc_ra_reg_class *c = ibc_reg_to_class(reg, state.reg_set);
      state.reg_class[reg->index] = c;
      ra_set_node_class(g, reg_node, c->nr);

      bool reg_has_strided_class =
         ibc_reg_has_strided_class(reg, state.reg_set);

      ibc_foreach_reg(other, shader) {
         if (!should_assign_reg(other))
            continue;

         /* We only need to look at registers before this one as reflexivity
          * of interference will take care of the rest.
          */
         if (other == reg)
            break;

         const unsigned other_node = reg_to_ra_node(&state, other);

         if (reg_has_strided_class &&
             other->file == IBC_FILE_LOGICAL &&
             reg->logical.stride == other->logical.stride &&
             ibc_reg_has_strided_class(other, state.reg_set)) {
            /* If both registers have a strided class an have the same stride
             * then we can use a logical interference model rather than a
             * physical one.
             */
            if (reg_logical_live[reg->index] != NULL &&
                reg_logical_live[other->index] != NULL &&
                interval_sets_intersect(reg_logical_live[reg->index],
                                        reg_logical_live[other->index]))
               ra_add_node_interference(g, reg_node, other_node);
         } else {
            const ibc_reg_live_intervals *rli = &state.live->regs[reg->index];
            const ibc_reg_live_intervals *oli = &state.live->regs[other->index];
            if (!(rli->physical_end <= oli->physical_start ||
                  oli->physical_end <= rli->physical_start))
               ra_add_node_interference(g, reg_node, other_node);
         }
      }
   }

   /* Walk backwards so that we hit the EOT send before any LOAD_PAYLOAD */
   ibc_foreach_instr_reverse(instr, shader) {
      ibc_pin_payload_and_eot_regs(&state, instr);
   }

   state.program_start_ip = calc_first_non_payload_ip(shader);

   struct ibc_builder b;
   ibc_builder_init(&b, shader);

#if 0
   if (unlikely((INTEL_DEBUG & DEBUG_SPILL_FS))) {
      fprintf(stderr, "\nIBC before SPILLING:\n");
      ibc_print_shader(shader, stderr);

      ibc_spill_reg(&state, &b, 3);

      fprintf(stderr, "\nIBC after SPILLING:\n");
      ibc_print_shader(shader, stderr);
      ibc_validate_shader(shader);
   }
#else
   if (unlikely((INTEL_DEBUG & DEBUG_SPILL_FS))) {
      while (true) {
         unsigned spill_reg = choose_spill_reg(&state, shader);
         if (spill_reg == NO_REG)
            break;

         if (0)
            fprintf(stderr, "ibc_spill_reg(&state, &b, %u);\n", spill_reg);

         ibc_spill_reg(&state, &b, spill_reg);
         ibc_validate_shader(shader);
      }
   }
#endif

   while (true) {
      if (ra_allocate(g))
         break;

      if (!allow_spilling) {
         ralloc_free(state.mem_ctx);
         return false;
      }

      int spill_reg = choose_spill_reg(&state, shader);
      if (spill_reg == NO_REG)
         return false;

      if (!ibc_spill_reg(&state, &b, spill_reg))
         return false;

      ibc_validate_shader(shader);
   }

   shader->scratch_B = state.last_scratch;

   ibc_foreach_instr_safe(instr, shader) {
      state.iter_instr = instr;

      state.is_read = true;
      ibc_instr_foreach_read(instr, rewrite_ref_from_gc_graph, &state);
      state.is_read = false;
      ibc_instr_foreach_write(instr, rewrite_ref_from_gc_graph, &state);

      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      switch (intrin->op) {
      case IBC_INTRINSIC_OP_UNDEF:
         ibc_instr_remove(instr);
         break;

      case IBC_INTRINSIC_OP_LOAD_PAYLOAD:
      case IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO:
         if (intrin->src[0].ref.file == IBC_FILE_NONE)
            continue;

         assert(intrin->dest.file == IBC_FILE_HW_GRF);
         assert(intrin->dest.reg == NULL);
         assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
         assert(intrin->src[0].ref.reg == NULL);

         if (intrin->src[0].ref.hw_grf.byte != intrin->dest.hw_grf.byte) {
            /* Some constraint (such as high GRFs for sends with EOT) may
             * cause the source and destination to get allocated to different
             * registers.  In this case, we need to replace the LOAD_PAYLOAD
             * with a MOV.
             */
            b.cursor = ibc_before_instr(instr);
            ibc_MOV_raw(&b, intrin->dest, intrin->src[0].ref,
                        intrin->num_dest_comps);
         }
         ibc_instr_remove(instr);
         break;

      default:
         break;
      }
   }

   ralloc_free(state.mem_ctx);

   return true;
}

bool
ibc_assign_regs(ibc_shader *shader, const struct brw_compiler *compiler,
                bool allow_spilling)
{
   return ibc_assign_regs_graph_color(shader, compiler, allow_spilling);
//   return ibc_assign_regs_linear_scan(shader, allow_spilling);
}
