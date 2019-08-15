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

static inline ibc_reg_ref
ibc_nir_src(struct nir_to_ibc_state *nti, nir_src src, enum ibc_type type)
{
   assert(src.is_ssa);
   if (ibc_type_bit_size(type) == 0)
      type |= nir_src_bit_size(src);
   return ibc_typed_ref(nti->ssa_to_reg[src.ssa->index], type);
}

static inline void
ibc_load_payload(ibc_builder *b, ibc_reg_ref dest,
                 ibc_reg_ref src, unsigned num_comps)
{
   ibc_build_intrinsic(b, IBC_INTRINSIC_OP_LOAD_PAYLOAD,
                       dest, num_comps,
                       &(ibc_intrinsic_src) { .ref = src }, 1);
}

static inline ibc_reg_ref
ibc_load_payload_reg(ibc_builder *b, unsigned *reg)
{
   ibc_reg *dest_reg = ibc_hw_grf_reg_create(b->shader, 32, 32);
   dest_reg->is_wlr = true;
   ibc_reg_ref dest = ibc_typed_ref(dest_reg, IBC_TYPE_UD);
   ibc_builder_push_we_all(b, 8);
   ibc_load_payload(b, dest, ibc_hw_grf_ref(*reg, 0, IBC_TYPE_UD), 1);
   ibc_builder_pop(b);
   *reg += 1;
   return dest;
}

static inline ibc_reg_ref
ibc_load_payload_logical(ibc_builder *b, unsigned *reg, enum ibc_type type,
                         unsigned num_comps)
{
   assert(ibc_type_bit_size(type) == 32);
   ibc_reg_ref dest = ibc_builder_new_logical_reg(b, type, num_comps);
   ibc_load_payload(b, dest, ibc_hw_grf_ref(*reg, 0, type), 1);
   *reg += (b->simd_width * ibc_type_byte_size(type) * num_comps) / REG_SIZE;
   return dest;
}

bool ibc_emit_nir_fs_intrinsic(struct nir_to_ibc_state *nti,
                               const nir_intrinsic_instr *instr);
bool ibc_emit_nir_cs_intrinsic(struct nir_to_ibc_state *nti,
                               const nir_intrinsic_instr *instr);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_NIR_H */
