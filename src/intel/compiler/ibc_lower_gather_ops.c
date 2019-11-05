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

#define INVALID_BLOCK_START ((ibc_flow_instr *)1)

struct reg_info {
   unsigned num_reads;
   ibc_flow_instr *write_block_start;
};

struct lower_gather_ops_state {
   ibc_flow_instr *block_start;

   unsigned num_regs;
   struct reg_info *reg_info;
};

static bool
count_refs(ibc_ref *ref,
           UNUSED int num_bytes, UNUSED int num_comps,
           UNUSED uint8_t simd_group, UNUSED uint8_t simd_width,
           void *_state)
{
   struct lower_gather_ops_state *state = _state;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg == NULL)
      return true;

   struct reg_info *reg_info = &state->reg_info[ref->reg->index];

   reg_info->num_reads++;

   return true;
}

static bool
record_write(ibc_ref *ref,
             UNUSED int num_bytes, UNUSED int num_comps,
             UNUSED uint8_t simd_group, UNUSED uint8_t simd_width,
             void *_state)
{
   struct lower_gather_ops_state *state = _state;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg == NULL)
      return true;

   struct reg_info *reg_info = &state->reg_info[ref->reg->index];

   if (reg_info->write_block_start == NULL) {
      reg_info->write_block_start = state->block_start;
   } else if (reg_info->write_block_start != state->block_start) {
      reg_info->write_block_start = INVALID_BLOCK_START;
   }

   return true;
}

static bool
ibc_instr_is_load_payload(const ibc_instr *instr)
{
   if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
      return false;

   ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
   return intrin->op == IBC_INTRINSIC_OP_LOAD_PAYLOAD ||
          (intrin->op == IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO &&
           intrin->src[0].ref.file != IBC_FILE_NONE);
}

struct rewrite_ref_state {
   const ibc_reg *src_reg;
   ibc_ref dest;
};

static bool
rewrite_write(ibc_reg_write *write,
              ibc_ref *ref, void *_state)
{
   struct rewrite_ref_state *state = _state;

   if (ref->reg == state->src_reg) {
      ibc_ref new_ref = state->dest;
      new_ref.type = ref->type;
      ibc_instr_set_ref(write->instr, ref, new_ref);
   }

   return true;
}

/* We have to split the actual coalesce attempt from the initial check for if
 * the destination is coalescable because the destination check involves
 * ibc_reg_ssa_instr and the process of coalescing adds writes which suddenly
 * makes the destination register not SSA.
 */
static bool
dest_can_try_coalesce(ibc_ref dest)
{
   /* For now, coalesce only works on logical and HW_GRF */
   if (dest.file != IBC_FILE_LOGICAL && dest.file != IBC_FILE_HW_GRF)
      return false;

   /* Destination needs to be SSA */
   if (dest.reg == NULL || ibc_reg_ssa_instr(dest.reg) == NULL)
      return false;

   return true;
}

static bool
try_coalesce(ibc_ref dest, ibc_intrinsic_src src,
             struct lower_gather_ops_state *state)
{
   /* For now, this only works on logical */
   if (src.ref.file != IBC_FILE_LOGICAL)
      return false;

   /* If nothing writes this register, we can trivially coalesce */
   if (list_is_empty(&src.ref.reg->writes))
      return true;

   /* Source needs to be SSA */
   ibc_instr *ssa_instr = ibc_reg_ssa_instr(src.ref.reg);
   if (ssa_instr == NULL)
      return false;

   /* We don't want to coalesce into LOAD_PAYLOAD and BTI_BLOCK_LOAD_UBO
    * instructions because RA is going to make them no-ops.
    */
   if (ibc_instr_is_load_payload(ssa_instr))
      return false;

   const struct reg_info *dest_info = &state->reg_info[dest.reg->index];
   const struct reg_info *src_info = &state->reg_info[src.ref.reg->index];

   /* We need to be the only use of this source
    *
    * TODO: This isn't strictly required but if it's true it makes everything
    * really easy so we'll go with it for now.
    */
   assert(src_info->num_reads > 0);
   if (src_info->num_reads != 1)
      return false;

   /* For now, we want to ensure that the instructions are in the same block.
    * We could, in theory, handle cross-block coalescing but that shouldn't be
    * common, will wreak havoc on RA, and breaks WLR.
    */
   assert(dest_info->write_block_start != INVALID_BLOCK_START);
   assert(src_info->write_block_start != INVALID_BLOCK_START);
   if (dest_info->write_block_start != src_info->write_block_start)
      return false;

   /* Our source has to read the whole register */
   if (src.ref.logical.byte > 0 ||
       src.ref.logical.comp > 0 ||
       src.ref.logical.broadcast ||
       ibc_type_bit_size(src.ref.type) != src.ref.reg->logical.bit_size ||
       src.num_comps != src.ref.reg->logical.num_comps ||
       src.simd_group != src.ref.reg->logical.simd_group ||
       src.simd_width != src.ref.reg->logical.simd_width)
      return false;

   switch (dest.file) {
   case IBC_FILE_LOGICAL:
      if (dest.reg->logical.stride != src.ref.reg->logical.stride)
         return false;

      /* If the source is packed then it needs to either have a single
       * component or else have the same SIMD group as the destination so that
       * it can be packed the same way.
       */
      if (src.ref.reg->logical.packed &&
          src.ref.reg->logical.num_comps > 1 &&
          (src.ref.reg->logical.simd_group != dest.reg->logical.simd_group ||
           src.ref.reg->logical.simd_width != dest.reg->logical.simd_width))
         return false;

      if (src.ref.reg->logical.align > 0) {
         if (dest.reg->logical.align < src.ref.reg->logical.align)
            return false;

         if (dest.logical.byte > 0)
            return false;
      }

      break;

   case IBC_FILE_HW_GRF:
      /* We have to have a simple stride on the destination */
      if (dest.hw_grf.vstride != dest.hw_grf.hstride * dest.hw_grf.width)
         return false;

      /* And it has to match the source */
      if (dest.hw_grf.hstride != src.ref.reg->logical.stride)
         return false;

      if (src.ref.reg->logical.align > 0) {
         if (dest.reg->hw_grf.align < src.ref.reg->logical.align)
            return false;

         if (dest.hw_grf.byte % src.ref.reg->logical.align != 0)
            return false;
      }
      break;

   default:
      unreachable("Unhandled register file");
   }

   ibc_instr_foreach_reg_write(ssa_instr, rewrite_write,
                               &(struct rewrite_ref_state) {
                                  .src_reg = src.ref.reg,
                                  .dest = dest,
                               });
   return true;
}

/** Lowers gather operations like SIMD ZIP, VEC, etc. */
bool
ibc_lower_gather_ops(ibc_shader *shader)
{
   bool progress = false;

   ibc_assign_logical_reg_strides(shader);

   struct lower_gather_ops_state state = { };

   ibc_foreach_reg(reg, shader)
      reg->index = state.num_regs++;

   state.reg_info = rzalloc_array(shader, struct reg_info, state.num_regs);

   ibc_foreach_instr(instr, shader) {
      ibc_instr_foreach_read(instr, count_refs, &state);
      ibc_instr_foreach_write(instr, record_write, &state);

      if (instr->type == IBC_INSTR_TYPE_FLOW)
         state.block_start = ibc_instr_as_flow(instr);
   }

   ibc_builder b;
   ibc_builder_init(&b, shader);

   ibc_foreach_instr_reverse_safe(instr, shader) {
      if (instr->type == IBC_INSTR_TYPE_FLOW) {
         state.block_start = ibc_instr_as_flow(instr);
         continue;
      }

      if (instr->type != IBC_INSTR_TYPE_INTRINSIC)
         continue;

      b.cursor = ibc_after_instr(instr);

      ibc_intrinsic_instr *intrin = ibc_instr_as_intrinsic(instr);
      bool can_try_coalesce = dest_can_try_coalesce(intrin->dest);

      switch (intrin->op) {
      case IBC_INTRINSIC_OP_SIMD_ZIP:
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            assert(intrin->src[i].num_comps == intrin->num_dest_comps);

            const unsigned rel_group = intrin->src[i].simd_group -
                                       instr->simd_group;
            ibc_ref dest = intrin->dest;
            ibc_ref_simd_slice(&dest, rel_group);

            if (can_try_coalesce && intrin->src[i].num_comps == 1 &&
                try_coalesce(dest, intrin->src[i], &state))
               continue;


            ibc_builder_push_group(&b, intrin->src[i].simd_group,
                                       intrin->src[i].simd_width);
            assert(b.simd_group == intrin->src[i].simd_group);

            /* We have to do our own loop here because we the SIMD width is
             * used by the builder to offset the source and destination and it
             * assumes the same SIMD width on both sides.
             */
            for (unsigned c = 0; c < intrin->src[i].num_comps; c++) {
               ibc_ref comp_dest = dest, comp_src = intrin->src[i].ref;
               ibc_ref_comp_offset(&comp_dest, c, intrin->instr.simd_width);
               ibc_ref_comp_offset(&comp_src, c, intrin->src[i].simd_width);
               ibc_MOV_raw(&b, comp_dest, comp_src, 1);
            }

            ibc_builder_pop(&b);
         }
         break;

      case IBC_INTRINSIC_OP_PACK: {
         ibc_builder_push_instr_group(&b, instr);

         ibc_ref dest = intrin->dest;
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            dest.type = intrin->src[i].ref.type;
            if (!can_try_coalesce ||
                !try_coalesce(dest, intrin->src[i], &state)) {
               ibc_MOV_raw(&b, dest, intrin->src[i].ref,
                           intrin->num_dest_comps);
            }
            ibc_ref_byte_offset(&dest,
                                ibc_type_byte_size(intrin->src[i].ref.type));
         }

         ibc_builder_pop(&b);
         break;
      }

      case IBC_INTRINSIC_OP_VEC: {
         ibc_builder_push_instr_group(&b, instr);

         ibc_ref dest = intrin->dest;
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            if (!can_try_coalesce ||
                !try_coalesce(dest, intrin->src[i], &state)) {
               ibc_MOV_raw(&b, dest, intrin->src[i].ref,
                           intrin->src[i].num_comps);
            }
            ibc_ref_comp_offset(&dest, intrin->src[i].num_comps,
                                intrin->instr.simd_width);
         }

         ibc_builder_pop(&b);
         break;
      }

      case IBC_INTRINSIC_OP_MESSAGE: {
         assert(intrin->dest.file == IBC_FILE_HW_GRF);

         ibc_ref dest = intrin->dest;
         for (unsigned i = 0; i < intrin->num_srcs; i++) {
            unsigned num_comps;
            if (intrin->src[i].simd_width == 1) {
               ibc_builder_push_we_all(&b, intrin->src[i].num_comps);
               num_comps = 1;
            } else {
               assert(instr->simd_group == intrin->src[i].simd_group);
               assert(instr->simd_width == intrin->src[i].simd_width);
               ibc_builder_push_instr_group(&b, instr);
               num_comps = intrin->src[i].num_comps;
            }

            dest.type = intrin->src[i].ref.type;
            dest.hw_grf.vstride = 8 * ibc_type_byte_size(dest.type);
            dest.hw_grf.width = 8;
            dest.hw_grf.hstride = ibc_type_byte_size(dest.type);

            if (intrin->src[i].ref.file != IBC_FILE_NONE &&
                (!can_try_coalesce ||
                 !try_coalesce(dest, intrin->src[i], &state))) {
               ibc_MOV_raw(&b, dest, intrin->src[i].ref, num_comps);
            }

            unsigned src_bytes = ibc_type_byte_size(intrin->src[i].ref.type) *
                                 intrin->src[i].simd_width *
                                 intrin->src[i].num_comps;
            dest.hw_grf.byte += ALIGN(src_bytes, REG_SIZE);

            ibc_builder_pop(&b);
         }
         assert(dest.hw_grf.byte == intrin->num_dest_bytes);
         break;
      }

      default:
         continue;
      }

      ibc_instr_remove(instr);
      progress = true;
   }

   if (progress)
      ibc_repair_wlr_order(shader);

   ralloc_free(state.reg_info);

   return progress;
}
