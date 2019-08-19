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

#ifndef IBC_NIR_H
#define IBC_NIR_H

#include "nir.h"

#include "ibc.h"
#include "ibc_builder.h"

#ifdef __cplusplus
extern "C" {
#endif

struct hash_table;

struct ibc_payload_base {
   /** Number of registers used for fixed-function thread payload */
   unsigned num_ff_regs;

   /** Number of registers used for push constants */
   unsigned num_curb_regs;
};

void ibc_setup_payload_base(ibc_builder *b, struct ibc_payload_base *payload);

struct nir_to_ibc_state {
   void *mem_ctx;

   ibc_builder b;

   gl_shader_stage stage;
   const struct brw_base_prog_key *key;
   struct brw_stage_prog_data *prog_data;
   struct ibc_payload_base *payload;
   void *stage_state;

   ibc_flow_instr *_do;
   struct list_head breaks;

   const ibc_reg **ssa_to_reg;
   const ibc_reg **reg_to_reg;

   struct hash_table *nir_block_to_ibc;
};

void nir_to_ibc_state_init(struct nir_to_ibc_state *nti,
                           gl_shader_stage stage,
                           const struct gen_device_info *devinfo,
                           const struct brw_base_prog_key *key,
                           struct brw_stage_prog_data *prog_data,
                           void *stage_state,
                           unsigned dispatch_size,
                           void *mem_ctx);
void ibc_emit_nir_shader(struct nir_to_ibc_state *nti,
                         const nir_shader *nir);
ibc_shader *nir_to_ibc_state_finish(struct nir_to_ibc_state *nti);

static inline enum ibc_type
ibc_type_for_nir(nir_alu_type ntype)
{
   if (nir_alu_type_get_type_size(ntype) == 1)
      return IBC_TYPE_FLAG;

   enum ibc_type stype;
   switch (nir_alu_type_get_base_type(ntype)) {
   case nir_type_int:   stype = IBC_TYPE_INT;   break;
   case nir_type_uint:  stype = IBC_TYPE_UINT;  break;
   case nir_type_float: stype = IBC_TYPE_FLOAT; break;
   default:
      unreachable("Unsupported base type");
   }

   return stype | nir_alu_type_get_type_size(ntype);
}

static inline ibc_ref
ibc_nir_src(struct nir_to_ibc_state *nti, nir_src src, enum ibc_type type)
{
   if (ibc_type_bit_size(type) == 0)
      type |= nir_src_bit_size(src);

   if (src.is_ssa) {
      assert(ibc_type_bit_size(type) <= src.ssa->bit_size);
      return ibc_typed_ref(nti->ssa_to_reg[src.ssa->index], type);
   } else {
      assert(ibc_type_bit_size(type) <= src.reg.reg->bit_size);
      assert(src.reg.indirect == NULL);
      assert(src.reg.base_offset == 0);
      return ibc_typed_ref(nti->reg_to_reg[src.reg.reg->index], type);
   }
}

static inline void
ibc_write_nir_ssa_def(struct nir_to_ibc_state *nti,
                      const nir_ssa_def *ndef,
                      ibc_ref src)
{
   assert(src.file == IBC_REG_FILE_LOGICAL);
   assert(src.reg->is_wlr);

   /* If we get handed something that isn't a "simple" src, we need to insert
    * a MOV.  We assume this only happens for scalars.
    */
   if (src.logical.byte > 0 || src.logical.comp > 0 || src.logical.broadcast) {
      assert(ndef->num_components == 1);
      src = ibc_MOV(&nti->b, src.type, src);
   }

   /* IBC's handling of partial writes is such that we can harmlessly grow
    * the register to fit what we need for the NIR SSA ndef.  If the
    * destination is too big for the NIR SSA ndef, that's harmless as reads
    * will simply take the bottom components or bytes.
    */
   ibc_reg *src_reg = (ibc_reg *)src.reg;
   if (src_reg->logical.num_comps < ndef->num_components)
      src_reg->logical.num_comps = ndef->num_components;
   if (src_reg->logical.bit_size < ndef->bit_size)
      src_reg->logical.bit_size = ndef->bit_size;

   nti->ssa_to_reg[ndef->index] = src.reg;
}

static inline void
ibc_write_nir_reg(struct nir_to_ibc_state *nti,
                  const nir_register *nreg,
                  ibc_ref src)
{
   assert(src.file == IBC_REG_FILE_LOGICAL);
   assert(src.reg->is_wlr);

   /* If we get handed something that isn't a "simple" src, we need to insert
    * a MOV.  We assume this only happens for scalars.
    */
   if (src.logical.byte > 0 || src.logical.comp > 0 || src.logical.broadcast ||
       src.reg->logical.simd_width != nti->b.simd_width) {
      assert(nreg->num_components == 1);
      src = ibc_MOV_raw(&nti->b, src);
   }

   ibc_reg *dest_reg = (ibc_reg *)nti->reg_to_reg[nreg->index];
   assert(dest_reg->file == IBC_REG_FILE_LOGICAL);

   /* IBC's handling of partial writes is such that we can harmlessly grow
    * the register to fit what we need for any writes.  Liveness and register
    * splitting will trim things off as needed.
    */
   if (dest_reg->logical.num_comps < src.reg->logical.num_comps)
      dest_reg->logical.num_comps = src.reg->logical.num_comps;
   if (dest_reg->logical.bit_size < src.reg->logical.bit_size)
      dest_reg->logical.bit_size = src.reg->logical.bit_size;

   /* Instead of emitting a MOV into the register, just rewrite all
    * instructions that wrote the old destination to write to the register
    * mapping to the nir_register.  This is safe as long as all such
    * instructions occur nicely packed together within the logical NIR
    * instruction we're emitting.  This is a safe assumption in ibc_to_nir.
    */
   ibc_reg_foreach_write_safe(write, src.reg) {
      assert(write->instr != NULL);
      ibc_ref *write_ref = ibc_reg_write_get_ref(write);
      ibc_ref new_ref = *write_ref;
      new_ref.reg = dest_reg;
      ibc_instr_set_ref(write->instr, write_ref, new_ref);
   }

   /* Delete the register so it doesn't clutter things */
   ibc_reg *src_reg = (ibc_reg *)src.reg;
   list_del(&src_reg->link);
}

static inline void
ibc_write_nir_dest(struct nir_to_ibc_state *nti, const nir_dest *ndest,
                   ibc_ref ref)
{
   if (ndest->is_ssa) {
      ibc_write_nir_ssa_def(nti, &ndest->ssa, ref);
   } else {
      assert(ndest->reg.indirect == NULL);
      assert(ndest->reg.base_offset == 0);
      ibc_write_nir_reg(nti, ndest->reg.reg, ref);
   }
}

static inline ibc_intrinsic_instr *
ibc_load_payload(ibc_builder *b, ibc_ref dest,
                 ibc_ref src, unsigned num_comps)
{
   return ibc_build_intrinsic(b, IBC_INTRINSIC_OP_LOAD_PAYLOAD,
                              dest, num_comps,
                              &(ibc_intrinsic_src) { .ref = src }, 1);
}

static inline ibc_ref
ibc_load_payload_reg(ibc_builder *b, unsigned *reg)
{
   ibc_reg *dest_reg = ibc_hw_grf_reg_create(b->shader, 32, 32);
   dest_reg->is_wlr = true;
   ibc_ref dest = ibc_typed_ref(dest_reg, IBC_TYPE_UD);
   ibc_builder_push_we_all(b, 8);
   ibc_load_payload(b, dest, ibc_hw_grf_ref(*reg, 0, IBC_TYPE_UD), 1);
   ibc_builder_pop(b);
   *reg += 1;
   return dest;
}

static inline ibc_ref
ibc_load_payload_logical(ibc_builder *b, unsigned *reg, enum ibc_type type,
                         unsigned num_comps)
{
   assert(ibc_type_bit_size(type) == 32);
   ibc_ref dest = ibc_builder_new_logical_reg(b, type, num_comps);
   ibc_load_payload(b, dest, ibc_hw_grf_ref(*reg, 0, type), num_comps);
   *reg += (b->simd_width * ibc_type_byte_size(type) * num_comps) / REG_SIZE;
   return dest;
}

bool ibc_emit_nir_vs_intrinsic(struct nir_to_ibc_state *nti,
                               const nir_intrinsic_instr *instr);
bool ibc_emit_nir_fs_intrinsic(struct nir_to_ibc_state *nti,
                               const nir_intrinsic_instr *instr);
bool ibc_emit_nir_cs_intrinsic(struct nir_to_ibc_state *nti,
                               const nir_intrinsic_instr *instr);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_NIR_H */
