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

      if (reg->logical.stride > 0)
         continue;

      /* Scalars and booleans won't be mapped to HW regs so they don't need to
       * have assigned strides.
       */
      if (reg->logical.bit_size == 1)
         continue;

      /* Uniform values don't have a stride across SIMD channels */
      if (reg->logical.simd_width == 1)
         continue;

      /* At the very least, we want it to be the size of the register */
      assert(reg->logical.bit_size >= 8);
      unsigned stride = reg->logical.bit_size / 8;

      ibc_reg_foreach_write(write, reg) {
         if (write->instr->type != IBC_INSTR_TYPE_ALU)
            continue;

         ibc_alu_instr *alu = ibc_instr_as_alu(write->instr);

         /* This can't be a flag write */
         assert(write == &alu->dest_write);

         for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
            if (alu->src[i].ref.type == IBC_TYPE_FLAG)
               stride = MAX2(stride, ibc_type_byte_size(IBC_TYPE_W));
            else
               stride = MAX2(stride, ibc_type_byte_size(alu->src[i].ref.type));
         }

         /* Only raw MOV supports a packed-byte destination */
         if (stride == 1 && !ibc_alu_instr_is_raw_mov(alu))
            stride = 2;
      }

      reg->logical.stride = stride;
      progress = true;
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
         ibc_build_intrinsic(&b, intrin->op, split_dest, num_comps,
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

   const ibc_ra_reg_set *reg_set;
   const ibc_live_intervals *live;

   struct ra_graph *g;
   int grf127_send_hack_node;
   unsigned num_hack_nodes;
};

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
   const unsigned node = state->num_hack_nodes + reg->index;
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

      if (ref->logical.broadcast ||
          (reg->logical.simd_width == 1 && state->is_read)) {
         new_ref.hw_grf.vstride = 0;
         new_ref.hw_grf.width = 1;
         new_ref.hw_grf.hstride = 0;
      } else {
         new_ref.hw_grf.hstride = stride;
         new_ref.hw_grf.width = 8;
         new_ref.hw_grf.vstride = stride * new_ref.hw_grf.width;
      }
      break;
   }

   default:
      unreachable("Unhandled register file");
   }

   ibc_instr_set_ref(state->iter_instr, _ref, new_ref);

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
   };

   state.live = ibc_compute_live_intervals(shader, should_assign_reg,
                                           state.mem_ctx);

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

   ibc_foreach_reg(reg, shader) {
      if (!should_assign_reg(reg))
         continue;

      const ibc_ra_reg_class *c = ibc_reg_to_class(reg, state.reg_set);
      ra_set_node_class(g, state.num_hack_nodes + reg->index, c->nr);

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
               ra_add_node_interference(g, state.num_hack_nodes + reg->index,
                                        state.num_hack_nodes + other->index);
         } else {
            const ibc_reg_live_intervals *rli = &state.live->regs[reg->index];
            const ibc_reg_live_intervals *oli = &state.live->regs[other->index];
            if (!(rli->physical_end <= oli->physical_start ||
                  oli->physical_end <= rli->physical_start))
               ra_add_node_interference(g, state.num_hack_nodes + reg->index,
                                        state.num_hack_nodes + other->index);
         }
      }
   }

   /* Walk backwards so that we hit the EOT send before any LOAD_PAYLOAD */
   ibc_foreach_instr_reverse(instr, shader) {
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
                  ibc_reg_to_class(send->payload[1].reg, state.reg_set);
               byte -= c->reg_size;
               ra_set_node_reg(g, state.num_hack_nodes +
                                  send->payload[1].reg->index,
                               ibc_ra_reg_class_grf_to_reg(c, byte));
            }

            const ibc_ra_reg_class *c =
               ibc_reg_to_class(send->payload[0].reg, state.reg_set);
            byte -= c->reg_size;
            ra_set_node_reg(g, state.num_hack_nodes +
                               send->payload[0].reg->index,
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
            assert(ibc_reg_ssa_instr(intrin->dest.reg) == instr);
            assert(intrin->src[0].ref.file == IBC_FILE_HW_GRF);
            assert(intrin->src[0].ref.reg == NULL);

            const ibc_ra_reg_class *c =
               ibc_reg_to_class(intrin->dest.reg, state.reg_set);
            unsigned byte = intrin->src[0].ref.hw_grf.byte;
            unsigned node = state.num_hack_nodes + intrin->dest.reg->index;
            if (ra_get_node_reg(g, node) == ~0U)
               ra_set_node_reg(g, node, ibc_ra_reg_class_grf_to_reg(c, byte));
         }
         break;
      }

      default:
         break;
      }
   }

   if (!ra_allocate(g)) {
      //assert(!allow_spilling);
      ralloc_free(state.mem_ctx);
      return false;
   }

   struct ibc_builder b;
   ibc_builder_init(&b, shader);

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
            ibc_MOV_raw_vec_to(&b, intrin->dest, intrin->src[0].ref,
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
