/*
 * Copyright © 2018 Intel Corporation
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

#ifndef SIR_H
#define SIR_H

#include <stdbool.h>
#include <stdint.h>

#include <util/list.h>
#include <util/macros.h>
#include <util/ralloc.h>

#include "brw_eu.h"

#ifdef __cplusplus
extern "C" {
#endif


/** Defines a cast function
 *
 * This macro defines a cast function from in_type to out_type where
 * out_type is some structure type that contains a field of type out_type.
 *
 * Note that you have to be a bit careful as the generated cast function
 * destroys constness.
 */
#define SIR_DEFINE_CAST(name, in_type, out_type, field, \
                        type_field, type_value)         \
static inline out_type *                                \
name(const in_type *parent)                             \
{                                                       \
   assert(parent && parent->type_field == type_value);  \
   return LIST_ENTRY(out_type, parent, field);          \
}

struct gen_device_info;
struct sir_instr;
struct sir_alu_instr;
struct sir_shader;

/** An enum representing SIR src and dest data types */
enum PACKED sir_type {
   SIR_TYPE_INVALID = 0,
   SIR_TYPE_INT = 1,
   SIR_TYPE_UINT = 2,
   SIR_TYPE_FLOAT = 3,

   SIR_TYPE_V  = 4 | 16,
   SIR_TYPE_UV = 5 | 16,
   SIR_TYPE_VF = 6 | 32,

   SIR_TYPE_B  = SIR_TYPE_INT | 8,
   SIR_TYPE_UB = SIR_TYPE_UINT | 8,
   SIR_TYPE_W  = SIR_TYPE_INT | 16,
   SIR_TYPE_UW = SIR_TYPE_UINT | 16,
   SIR_TYPE_HF = SIR_TYPE_FLOAT | 16,
   SIR_TYPE_D  = SIR_TYPE_INT | 32,
   SIR_TYPE_UD = SIR_TYPE_UINT | 32,
   SIR_TYPE_F =  SIR_TYPE_FLOAT | 32,
   SIR_TYPE_Q  = SIR_TYPE_INT | 64,
   SIR_TYPE_UQ = SIR_TYPE_UINT | 64,
   SIR_TYPE_DF = SIR_TYPE_FLOAT | 64,
};

#define SIR_TYPE_BIT_SIZE_MASK 0x78
#define SIR_TYPE_BASE_TYPE_MASK 0x07

static inline unsigned
sir_type_bit_size(enum sir_type t)
{
   return t & SIR_TYPE_BIT_SIZE_MASK;
}

static inline unsigned
sir_type_base_type(enum sir_type t)
{
   return t & SIR_TYPE_BASE_TYPE_MASK;
}

/** An enum representing the different types of SIR registers */
enum PACKED sir_reg_file {
   SIR_REG_FILE_NONE,
   SIR_REG_FILE_IMM,
   SIR_REG_FILE_LOGICAL,
   SIR_REG_FILE_HW_GRF,
   SIR_REG_FILE_FLAG,
};


/** A struct representing a logical register
 *
 * A logical register is not represented as a linear number of bytes or
 * integral number of GEN registers.  Instead, it has a 3-dimensional logical
 * size which later gets translated into bytes for register allocation.
 */
typedef struct sir_logical_reg {
   /** Number of bits per component */
   uint8_t bit_size;

   /** Number of vector components */
   uint8_t num_comps;

   /** Number of SIMD invocations */
   uint8_t simd_width;

   /** SIMD invocation offset */
   uint8_t simd_group;

   /** Definition if this register is statically assigned once */
   struct sir_instr *ssa;
} sir_logical_reg;


#define SIR_HW_REG_UNASSIGNED UINT16_MAX

/** A struct representing a HW GRF register
 *
 * A physical HW register representing an actual byte range in the hardware
 * general-purpose register file.  Unlike logical registers HW GRF registers
 * do not have a simple 1D size in bytes and have no concept of SIMD width or
 * invocation offset.
 */
typedef struct sir_hw_grf_reg {
   /** Start of this register in bytes
    *
    * A value of SIR_HW_REG_UNASSIGNED means this HW reg is "virtual" and will
    * have an actual register assigned later.
    */
   uint16_t byte;

   /** Size of this register in bytes */
   uint8_t size;

   /** Alignment requirement of this register in bytes */
   uint8_t align;
} sir_hw_grf_reg;


#define SIR_FLAG_REG_UNASSIGNED UINT8_MAX

/** A struct representing a flag register
 *
 * A flag register is only one-dimensional; it only has a number bits.  It can
 * be viewed in one of two ways:
 *
 *  * When written with a flag destination or read as a conditional modifier,
 *    it's viewed as a 1-bit scalar with one bit per SIMD invocation.
 *
 *  * When read or written as a regular source, it's viewed as a N-Bit scalar
 *    with only one SIMD invocation.
 */
typedef struct sir_flag_reg {
   /** Flag register subnumber in units of 16-bit chunks */
   uint8_t subnr;

   /** Size in bits  */
   uint8_t bits;

   /** Definition if this register is statically assigned once */
   struct sir_alu_instr *ssa;
} sir_flag_reg;


/** A struct representing a register */
typedef struct sir_reg {
   /** Register type */
   enum sir_reg_file file;

   /** Link in sir_shader::regs */
   struct list_head link;

   union {
      sir_logical_reg logical;
      sir_hw_grf_reg hw_grf;
      sir_flag_reg flag;
   };
} sir_reg;

sir_reg *sir_logical_reg_create(struct sir_shader *shader,
                                uint8_t bit_size, uint8_t num_comps,
                                uint8_t simd_width, uint8_t simd_group);

sir_reg *sir_hw_grf_reg_create(struct sir_shader *shader,
                               uint16_t byte, uint8_t size, uint8_t align);


/** A structure representing a register reference (source or destination) in
 * an instruction
 */
typedef struct sir_reg_ref {
   /** Pointer to the register; NULL if immediate */
   const sir_reg *reg;

   union {
      /** Component to reference for logical registers */
      uint8_t comp;

      struct {
         /** Byte offset at which the reference starts for HW regs */
         uint8_t offset;

         /** Stride in bytes for HW regs */
         uint8_t stride;
      };
   };
} sir_reg_ref;


enum sir_instr_type {
   SIR_INSTR_TYPE_ALU,
   SIR_INSTR_TYPE_SEND,
   SIR_INSTR_TYPE_INTRINSIC,
   SIR_INSTR_TYPE_JUMP,
};

/** A structure representing an instruction */
typedef struct sir_instr {
   /** The type of this instruction */
   enum sir_instr_type type;

   /** Link in sir_block::instrs */
   struct list_head link;

   uint8_t simd_width;
   uint8_t simd_group;
   bool we_all;

   /** Flag reference for predication or cmod */
   sir_reg_ref flag;

   /** If not BRW_PREDICATE_NONE, this in struction is predicated using the
    * predicate from flag.
    */
   enum brw_predicate predicate;

   /** True if the predicate is to be inverted */
   bool pred_inverse;
} sir_instr;


/** Enum of SIR ALU opcodes */
enum sir_alu_op {
   SIR_ALU_OP_MOV = 1,
   SIR_ALU_OP_AND = 5,
   SIR_ALU_OP_SHR = 8,
   SIR_ALU_OP_SHL = 9,
   SIR_ALU_OP_ADD = 64,
};


/** A structure representing an ALU instruction source */
typedef struct sir_alu_src {
   /** Register file or IMM for immediate or NONE for null */
   enum sir_reg_file file;

   /** Type with which the register or immediate is interpreted */
   enum sir_type type;

   bool negate:1;
   bool abs:1;

   union {
      /** A register reference for non-immediate sources */
      sir_reg_ref reg;

      /** 64 bits of immediate data for immediate sources */
      char imm[8];
   };
} sir_alu_src;


typedef struct sir_alu_dest {
   /** Register file or NONE for null */
   enum sir_reg_file file;

   /** Type with which the register or immediate is interpreted */
   enum sir_type type;

   bool sat;

   sir_reg_ref reg;
} sir_alu_dest;


typedef struct sir_alu_instr {
   sir_instr instr;

   /** Opcode */
   enum sir_alu_op op;

   enum brw_conditional_mod cmod;

   sir_alu_dest dest;

   sir_alu_src src[0];
} sir_alu_instr;

SIR_DEFINE_CAST(sir_instr_as_alu, sir_instr, sir_alu_instr, instr,
                type, SIR_INSTR_TYPE_ALU)

sir_alu_instr *sir_alu_instr_create(struct sir_shader *shader,
                                    enum sir_alu_op op,
                                    uint8_t simd_width,
                                    uint8_t simd_group);


typedef struct sir_send_instr {
   sir_instr instr;

   unsigned mlen:4;
   unsigned mlen2:4;
   unsigned rlen:4;
   bool has_header:1;
   bool has_side_effects:1;
   bool check_tdr:1;
   bool eot:1;

   uint32_t desc_imm;
   sir_reg_ref desc;
   sir_reg_ref ex_desc;

   sir_reg_ref dest;

   sir_reg_ref payload[2];
} sir_send_instr;

SIR_DEFINE_CAST(sir_instr_as_send, sir_instr, sir_send_instr, instr,
                type, SIR_INSTR_TYPE_SEND)


typedef struct {
   /** A register reference for non-immediate sources */
   sir_reg_ref reg;
} sir_intrinsic_reg;

typedef struct {
   sir_instr instr;

   sir_reg_ref dest;
} sir_intrinsic_instr;

SIR_DEFINE_CAST(sir_instr_as_intrinsic, sir_instr, sir_intrinsic_instr, instr,
                type, SIR_INSTR_TYPE_INTRINSIC)

typedef struct sir_block {
   /* Link in the list of blocks */
   struct list_head link;

   /* Instructions in this block.  The last instruction is guaranteed to be a
    * jump instruction.
    */
   struct list_head instrs;
} sir_block;

sir_block *sir_block_create(struct sir_shader *shader);

#define sir_foreach_instr(instr, block) \
   list_for_each_entry(sir_instr, instr, &(block)->instrs, link)

#define sir_foreach_instr_safe(instr, block) \
   list_for_each_entry_safe(sir_instr, instr, &(block)->instrs, link)

#define sir_foreach_instr_reverse(instr, block) \
   list_for_each_entry_rev(sir_instr, instr, &(block)->instrs, link)

#define sir_foreach_instr_reverse_safe(instr, block) \
   list_for_each_entry_safe_rev(sir_instr, instr, &(block)->instrs, link)


typedef struct sir_shader {
   const struct gen_device_info *devinfo;

   /** Blocks */
   struct list_head blocks;

   /** Registers */
   struct list_head regs;
} sir_shader;

sir_shader *sir_shader_create(void *mem_ctx,
                              const struct gen_device_info *devinfo);

#define sir_foreach_block(block, shader) \
   list_for_each_entry(sir_block, block, &(shader)->blocks, link)

#define sir_foreach_block_safe(block, shader) \
   list_for_each_entry_safe(sir_block, block, &(shader)->blocks, link)


void sir_validate_shader(const sir_shader *shader);


/**********************************************************************
 * SIR optimization and lowering passes
 *
 * KEEP IN ALPHABETICAL ORDER!
 **********************************************************************/

struct nir_shader;
sir_shader *nir_to_sir(const struct nir_shader *nir, void *mem_ctx,
                       unsigned dispatch_size,
                       const struct gen_device_info *devinfo);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SIR_H */
