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

#include "util/hash_table.h"
#include "util/set.h"

#define HASH(hash, data) _mesa_fnv32_1a_accumulate((hash), (data))

static uint32_t
hash_ref(uint32_t hash, const ibc_ref *ref, const ibc_reg *base_reg)
{
   hash = HASH(hash, ref->file);
   hash = HASH(hash, ref->type);

   switch (ref->file) {
   case IBC_REG_FILE_NONE:
      assert(ref->reg == NULL);
      return hash;

   case IBC_REG_FILE_IMM:
      return _mesa_fnv32_1a_accumulate_block(hash, ref->imm,
                                             ibc_type_byte_size(ref->type));

   case IBC_REG_FILE_LOGICAL:
      hash = HASH(hash, ref->logical.byte);
      hash = HASH(hash, ref->logical.comp);
      hash = HASH(hash, ref->logical.broadcast);
      if (ref->logical.broadcast)
         hash = HASH(hash, ref->logical.simd_channel);
      break;

   case IBC_REG_FILE_HW_GRF:
      hash = HASH(hash, ref->hw_grf.byte);
      hash = HASH(hash, ref->hw_grf.hstride);
      if (ref->hw_grf.vstride != ref->hw_grf.width * ref->hw_grf.hstride) {
         hash = HASH(hash, ref->hw_grf.vstride);
         hash = HASH(hash, ref->hw_grf.width);
      }
      break;

   case IBC_REG_FILE_FLAG:
      hash = HASH(hash, ref->flag.bit);
      break;
   }

   /* See also refs_equal */
   if (ref->reg && ref->reg != base_reg)
      hash = HASH(hash, ref->reg);

   return hash;
}

static bool
refs_equal_except_reg(const ibc_ref *ref_a, const ibc_ref *ref_b)
{
   if (ref_a->file != ref_b->file ||
       ref_a->type != ref_b->type)
      return false;

   switch (ref_a->file) {
   case IBC_REG_FILE_NONE:
      assert(ref_a->reg == NULL && ref_b->reg == NULL);
      return true;

   case IBC_REG_FILE_IMM:
      return !memcmp(ref_a->imm, ref_b->imm, ibc_type_byte_size(ref_a->type));

   case IBC_REG_FILE_LOGICAL:
      if (ref_a->logical.byte != ref_b->logical.byte ||
          ref_a->logical.comp != ref_b->logical.comp ||
          ref_a->logical.broadcast != ref_b->logical.broadcast)
         return false;
      if (ref_a->logical.broadcast &&
          ref_a->logical.simd_channel != ref_b->logical.simd_channel)
         return false;
      break;

   case IBC_REG_FILE_HW_GRF:
      if (ref_a->hw_grf.byte != ref_b->hw_grf.byte ||
          ref_a->hw_grf.hstride != ref_b->hw_grf.hstride)
         return false;
      if ((ref_a->hw_grf.vstride !=
           ref_a->hw_grf.width * ref_a->hw_grf.hstride) ||
          (ref_b->hw_grf.vstride !=
           ref_b->hw_grf.width * ref_b->hw_grf.hstride)) {
         if (ref_a->hw_grf.vstride != ref_b->hw_grf.vstride ||
             ref_a->hw_grf.width != ref_b->hw_grf.width)
            return false;
      }
      break;

   case IBC_REG_FILE_FLAG:
      if (ref_a->flag.bit != ref_b->flag.bit)
         return false;
      break;
   }

   return true;
}

static bool
refs_equal(const ibc_ref *ref_a, const ibc_ref *ref_b,
           const ibc_reg *base_reg_a, const ibc_reg *base_reg_b)
{
   if (!refs_equal_except_reg(ref_a, ref_b))
      return false;

   /* Reads from non-WLR registers constitute different values and we can't
    * CSE them.
    */
   if (!ref_a->reg || !ref_a->reg->is_wlr ||
       !ref_b->reg || !ref_b->reg->is_wlr)
      return false;

   /* If we see a read from the base register in both instructions, we allow
    * that because it's something like a |= b and as log as a matches before
    * and b matches, it's the same value.
    */
   return (ref_a->reg == base_reg_a && ref_b->reg == base_reg_b) ||
          ref_a->reg == ref_b->reg;
}

bool
ibc_refs_equal(ibc_ref a, ibc_ref b)
{
   return refs_equal_except_reg(&a, &b) && a.reg == b.reg;
}

static uint32_t
hash_instr(uint32_t hash, const ibc_instr *instr,
           const ibc_reg *base_reg)
{
   hash = HASH(hash, instr->type);

   /* Ignore index and link */

   hash = HASH(hash, instr->simd_group);
   hash = HASH(hash, instr->simd_width);
   hash = HASH(hash, instr->we_all);

   hash = hash_ref(hash, &instr->flag, base_reg);

   hash = HASH(hash, instr->predicate);
   hash = HASH(hash, instr->pred_inverse);

   return hash;
}

static bool
instrs_equal(const ibc_instr *instr_a, const ibc_instr *instr_b,
             const ibc_reg *base_reg_a, const ibc_reg *base_reg_b)
{
   if (instr_a->type != instr_b->type)
      return false;

   /* Ignore index and link */

   if (instr_a->simd_group != instr_b->simd_group ||
       instr_a->simd_width != instr_b->simd_width ||
       instr_a->we_all != instr_b->we_all)
      return false;

   if (!refs_equal(&instr_a->flag, &instr_b->flag, base_reg_a, base_reg_b))
      return false;

   if (instr_a->predicate != instr_b->predicate ||
       instr_a->pred_inverse != instr_b->pred_inverse)
      return false;

   return true;
}

static uint32_t
hash_alu_instr(uint32_t hash, const ibc_alu_instr *alu,
               const ibc_reg *base_reg)
{
   hash = hash_instr(hash, &alu->instr, base_reg);

   hash = HASH(hash, alu->op);
   hash = HASH(hash, alu->cmod);
   hash = HASH(hash, alu->saturate);

   hash = hash_ref(hash, &alu->dest, base_reg);

   for (unsigned i = 0; i < ibc_alu_op_infos[alu->op].num_srcs; i++) {
      hash = hash_ref(hash, &alu->src[i].ref, base_reg);
      hash = HASH(hash, alu->src[i].mod);
   }

   return hash;
}

static bool
alu_instrs_equal(const ibc_alu_instr *alu_a, const ibc_alu_instr *alu_b,
                 const ibc_reg *base_reg_a, const ibc_reg *base_reg_b)
{
   if (!instrs_equal(&alu_a->instr, &alu_b->instr, base_reg_a, base_reg_b))
      return false;

   if (alu_a->op != alu_b->op ||
       alu_a->cmod != alu_b->cmod ||
       alu_a->saturate != alu_b->saturate)
      return false;

   if (!refs_equal(&alu_a->dest, &alu_b->dest, base_reg_a, base_reg_b))
      return false;

   for (unsigned i = 0; i < ibc_alu_op_infos[alu_a->op].num_srcs; i++) {
      if (!refs_equal(&alu_a->src[i].ref, &alu_b->src[i].ref,
                          base_reg_a, base_reg_b))
         return false;
      if (alu_a->src[i].mod != alu_b->src[i].mod)
         return false;
   }

   return true;
}

static uint32_t
hash_intrinsic_instr(uint32_t hash, const ibc_intrinsic_instr *intrin,
                     const ibc_reg *base_reg)
{
   hash = hash_instr(hash, &intrin->instr, base_reg);

   hash = HASH(hash, intrin->op);

   /* Ignore can_reorder and has_side_effects */

   hash = hash_ref(hash, &intrin->dest, base_reg);
   hash = HASH(hash, intrin->num_dest_comps);

   hash = HASH(hash, intrin->num_srcs);
   for (unsigned i = 0; i < intrin->num_srcs; i++) {
      hash = hash_ref(hash, &intrin->src[i].ref, base_reg);
      hash = HASH(hash, intrin->src[i].simd_group);
      hash = HASH(hash, intrin->src[i].simd_width);
      hash = HASH(hash, intrin->src[i].num_comps);
   }

   return hash;
}

static bool
intrinsic_instrs_equal(const ibc_intrinsic_instr *intrin_a,
                       const ibc_intrinsic_instr *intrin_b,
                       const ibc_reg *base_reg_a, const ibc_reg *base_reg_b)
{
   if (!instrs_equal(&intrin_a->instr, &intrin_b->instr,
                     base_reg_a, base_reg_b))
      return false;

   if (intrin_a->op != intrin_b->op)

   /* If one of the instructions can't be re-ordered or has side-effects, we
    * can't combine them so compare unequal.
    */
   if (!intrin_a->can_reorder ||
       !intrin_b->can_reorder ||
       intrin_a->has_side_effects ||
       intrin_b->has_side_effects)
      return false;

   if (!refs_equal(&intrin_a->dest, &intrin_b->dest,
                   base_reg_a, base_reg_b) ||
       intrin_a->num_dest_comps != intrin_b->num_dest_comps)
      return false;

   if (intrin_a->num_srcs != intrin_b->num_srcs)
      return false;
   for (unsigned i = 0; i < intrin_a->num_srcs; i++) {
      if (!refs_equal(&intrin_a->src[i].ref, &intrin_b->src[i].ref,
                      base_reg_a, base_reg_b))
         return false;
      if (intrin_a->src[i].simd_group != intrin_b->src[i].simd_group ||
          intrin_a->src[i].simd_width != intrin_b->src[i].simd_width ||
          intrin_a->src[i].num_comps != intrin_b->src[i].num_comps)
         return false;
   }

   return true;
}

static uint32_t
hash_wlr_reg_cb(const void *_reg)
{
   const struct ibc_reg *reg = _reg;
   assert(reg->is_wlr);

   uint32_t hash = _mesa_fnv32_1a_offset_bias;

   hash = HASH(hash, reg->file);

   /* Ignore index and link */

   switch (reg->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Invalid register file ibc_reg");

   case IBC_REG_FILE_LOGICAL:
      hash = HASH(hash, reg->logical.bit_size);
      hash = HASH(hash, reg->logical.num_comps);
      hash = HASH(hash, reg->logical.simd_group);
      hash = HASH(hash, reg->logical.simd_width);
      /* Ignore stride because it's a derived parameter */
      break;

   case IBC_REG_FILE_HW_GRF:
      hash = HASH(hash, reg->hw_grf.size);
      hash = HASH(hash, reg->hw_grf.align);
      break;

   case IBC_REG_FILE_FLAG:
      hash = HASH(hash, reg->flag.bits);
      hash = HASH(hash, reg->flag.align_mul);
      hash = HASH(hash, reg->flag.align_offset);
      break;
   }

   ibc_reg_foreach_write(write, reg) {
      const ibc_instr *instr = write->instr;
      switch (instr->type) {
      case IBC_INSTR_TYPE_ALU:
         hash = hash_alu_instr(hash, ibc_instr_as_alu(instr), reg);
         break;
      case IBC_INSTR_TYPE_SEND:
         unreachable("TODO: We should be able to CSE sends");
      case IBC_INSTR_TYPE_INTRINSIC:
         hash = hash_intrinsic_instr(hash, ibc_instr_as_intrinsic(instr), reg);
         break;
      case IBC_INSTR_TYPE_FLOW:
         unreachable("Branch and merge instructions don't write anything");
      }
   }

   return hash;
}

static bool
wlr_regs_equal_cb(const void *_reg_a, const void *_reg_b)
{
   const struct ibc_reg *reg_a = _reg_a, *reg_b = _reg_b;
   assert(reg_a->is_wlr && reg_b->is_wlr);

   if (reg_a->file != reg_b->file)
      return false;

   switch (reg_a->file) {
   case IBC_REG_FILE_NONE:
   case IBC_REG_FILE_IMM:
      unreachable("Invalid register file ibc_reg");

   case IBC_REG_FILE_LOGICAL:
      if (reg_a->logical.bit_size != reg_b->logical.bit_size ||
          reg_a->logical.num_comps != reg_b->logical.num_comps ||
          reg_a->logical.simd_group != reg_b->logical.simd_group ||
          reg_a->logical.simd_width != reg_b->logical.simd_width)
         return false;
      /* Ignore stride because it's a derived parameter */
      break;

   case IBC_REG_FILE_HW_GRF:
      if (reg_a->hw_grf.size != reg_b->hw_grf.size ||
          reg_a->hw_grf.align != reg_b->hw_grf.align)
         return false;
      break;

   case IBC_REG_FILE_FLAG:
      if (reg_a->flag.bits != reg_b->flag.bits ||
          reg_a->flag.align_mul != reg_b->flag.align_mul ||
          reg_a->flag.align_offset != reg_b->flag.align_offset)
         return false;
      break;
   }

   list_pair_for_each_entry(const ibc_reg_write, write_a, write_b,
                            &reg_a->writes, &reg_b->writes, link) {
      const ibc_instr *instr_a = write_a->instr;
      const ibc_instr *instr_b = write_b->instr;
      if (instr_a->type != instr_b->type)
         return false;

      switch (instr_a->type) {
      case IBC_INSTR_TYPE_ALU:
         if (!alu_instrs_equal(ibc_instr_as_alu(instr_a),
                               ibc_instr_as_alu(instr_b),
                               reg_a, reg_b))
            return false;
         break;
      case IBC_INSTR_TYPE_SEND:
         unreachable("TODO: We should be able to CSE sends");
      case IBC_INSTR_TYPE_INTRINSIC:
         if (!intrinsic_instrs_equal(ibc_instr_as_intrinsic(instr_a),
                                     ibc_instr_as_intrinsic(instr_b),
                                     reg_a, reg_b))
            return false;
         break;
      case IBC_INSTR_TYPE_FLOW:
         unreachable("Branch and merge instructions don't write anything");
      }
   }

   return true;
}

struct opt_cse_state {
   struct set *reg_set;
   struct hash_table *reg_remap;

   bool progress;
};

static bool
rewrite_read(struct ibc_ref *ref,
             UNUSED int num_bytes, UNUSED int num_comps,
             UNUSED uint8_t simd_group, UNUSED uint8_t simd_width,
             void *_state)
{
   struct opt_cse_state *state = _state;

   if (ref->file == IBC_REG_FILE_NONE ||
       ref->file == IBC_REG_FILE_IMM)
      return true;

   if (ref->reg == NULL || !ref->reg->is_wlr)
      return true;

   struct hash_entry *entry =
      _mesa_hash_table_search(state->reg_remap, ref->reg);
   if (entry && ref->reg != entry->data) {
      ref->reg = entry->data;
      state->progress = true;
   }

   return true;
}

static bool
try_cse_write(ibc_reg_write *write, ibc_ref *ref, void *_state)
{
   struct opt_cse_state *state = _state;

   assert(ref->file != IBC_REG_FILE_IMM);
   if (ref->file == IBC_REG_FILE_NONE)
      return true;

   if (ref->reg == NULL || !ref->reg->is_wlr)
      return true;

   /* Only bother on the last write instruction */
   if (write->link.next != &ref->reg->writes)
      return true;

   if (_mesa_hash_table_search(state->reg_remap, ref->reg))
      return true;

   struct set_entry *entry =
      _mesa_set_search_or_add(state->reg_set, ref->reg);
   _mesa_hash_table_insert(state->reg_remap, ref->reg, (void *)entry->key);

   return true;
}

bool
ibc_opt_cse(ibc_shader *shader)
{
   struct opt_cse_state state = {
      .reg_set = _mesa_set_create(NULL, hash_wlr_reg_cb, wlr_regs_equal_cb),
      .reg_remap = _mesa_pointer_hash_table_create(NULL),
      .progress = false,
   };

   ibc_foreach_instr_safe(instr, shader) {
      ibc_instr_foreach_read(instr, rewrite_read, &state);
      ibc_instr_foreach_reg_write(instr, try_cse_write, &state);

      /* When we cross block boundaries, reset the reg set.  This prevents us
       * from CSEing two regs in different blocks.  We leave the remap set
       * around so that we can still do remaps.
       */
      if (instr->type == IBC_INSTR_TYPE_FLOW)
         _mesa_set_clear(state.reg_set, NULL);
   }

   _mesa_set_destroy(state.reg_set, NULL);
   _mesa_hash_table_destroy(state.reg_remap, NULL);

   return state.progress;
}
