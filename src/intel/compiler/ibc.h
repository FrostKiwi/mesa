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

#ifndef IBC_H
#define IBC_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

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
#define IBC_DEFINE_CAST(name, in_type, out_type, field, \
                        type_field, type_value)         \
static inline out_type *                                \
name(const in_type *parent)                             \
{                                                       \
   assert(parent && parent->type_field == type_value);  \
   return LIST_ENTRY(out_type, parent, field);          \
}

struct gen_device_info;
struct ibc_instr;
struct ibc_alu_instr;
struct ibc_shader;

/** An enum representing IBC src and dest data types */
enum PACKED ibc_type {
   IBC_TYPE_INVALID = 0,
   IBC_TYPE_INT = 2,
   IBC_TYPE_UINT = 4,
   IBC_TYPE_FLOAT = 6,

   IBC_TYPE_FLAG = 1,

   IBC_TYPE_8_BIT = 8,
   IBC_TYPE_16_BIT = 16,
   IBC_TYPE_32_BIT = 32,
   IBC_TYPE_64_BIT = 64,

   IBC_TYPE_B  = IBC_TYPE_INT    | IBC_TYPE_8_BIT,
   IBC_TYPE_UB = IBC_TYPE_UINT   | IBC_TYPE_8_BIT,
   IBC_TYPE_W  = IBC_TYPE_INT    | IBC_TYPE_16_BIT,
   IBC_TYPE_UW = IBC_TYPE_UINT   | IBC_TYPE_16_BIT,
   IBC_TYPE_HF = IBC_TYPE_FLOAT  | IBC_TYPE_16_BIT,
   IBC_TYPE_D  = IBC_TYPE_INT    | IBC_TYPE_32_BIT,
   IBC_TYPE_UD = IBC_TYPE_UINT   | IBC_TYPE_32_BIT,
   IBC_TYPE_F =  IBC_TYPE_FLOAT  | IBC_TYPE_32_BIT,
   IBC_TYPE_Q  = IBC_TYPE_INT    | IBC_TYPE_64_BIT,
   IBC_TYPE_UQ = IBC_TYPE_UINT   | IBC_TYPE_64_BIT,
   IBC_TYPE_DF = IBC_TYPE_FLOAT  | IBC_TYPE_64_BIT,

   IBC_TYPE_VECTOR = 128,
   IBC_TYPE_V  = IBC_TYPE_VECTOR | IBC_TYPE_W,
   IBC_TYPE_UV = IBC_TYPE_VECTOR | IBC_TYPE_UW,
   IBC_TYPE_VF = IBC_TYPE_VECTOR | IBC_TYPE_F,
};

#define IBC_TYPE_BIT_SIZE_MASK 0x79
#define IBC_TYPE_BASE_TYPE_MASK 0x87

static inline unsigned
ibc_type_bit_size(enum ibc_type t)
{
   return t & IBC_TYPE_BIT_SIZE_MASK;
}

static inline unsigned
ibc_type_byte_size(enum ibc_type t)
{
   assert(ibc_type_bit_size(t) % 8 == 0);
   return ibc_type_bit_size(t) / 8;
}

static inline enum ibc_type
ibc_type_base_type(enum ibc_type t)
{
   return (enum ibc_type)(t & IBC_TYPE_BASE_TYPE_MASK);
}

static inline enum ibc_type
ibc_type_bit_type(enum ibc_type t)
{
   return (enum ibc_type)(t & IBC_TYPE_BIT_SIZE_MASK);
}

/** An enum representing the different types of IBC registers */
enum PACKED ibc_reg_file {
   IBC_REG_FILE_NONE,
   IBC_REG_FILE_IMM,
   IBC_REG_FILE_LOGICAL,
   IBC_REG_FILE_HW_GRF,
   IBC_REG_FILE_FLAG,
};


/** A struct representing a logical register
 *
 * A logical register is not represented as a linear number of bytes or
 * integral number of GEN registers.  Instead, it has a 3-dimensional logical
 * size which later gets translated into bytes for register allocation.
 */
typedef struct ibc_logical_reg {
   /** Number of bits per component */
   uint8_t bit_size;

   /** Number of vector components */
   uint8_t num_comps;

   /** SIMD invocation offset */
   uint8_t simd_group;

   /** Number of SIMD invocations */
   uint8_t simd_width;
} ibc_logical_reg;


#define IBC_HW_GRF_REG_UNASSIGNED UINT16_MAX

/** A struct representing a HW GRF register
 *
 * A physical HW register representing an actual byte range in the hardware
 * general-purpose register file.  Unlike logical registers HW GRF registers
 * do not have a simple 1D size in bytes and have no concept of SIMD width or
 * invocation offset.
 */
typedef struct ibc_hw_grf_reg {
   /** Start of this register in bytes
    *
    * A value of IBC_HW_GRF_UNASSIGNED means this HW reg is "virtual" and will
    * have an actual register assigned later.
    */
   uint16_t byte;

   /** Size of this register in bytes */
   uint16_t size;

   /** Alignment requirement of this register in bytes */
   uint8_t align;
} ibc_hw_grf_reg;


#define IBC_FLAG_REG_UNASSIGNED UINT8_MAX

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
typedef struct ibc_flag_reg {
   /** Flag register subnumber in units of 16-bit chunks */
   uint8_t subnr;

   /** Size in bits  */
   uint8_t bits;

   /** Definition if this register is statically assigned once */
   struct ibc_alu_instr *ssa;
} ibc_flag_reg;


/** A struct representing a register */
typedef struct ibc_reg {
   /** Register type */
   enum ibc_reg_file file;

   /** True if this register is in write-lock-read form.
    *
    * Write-lock-read form is a generalization of SSA in which multiple writes
    * to a value are allowed in a contained way.  For a value to be in WLR
    * form, both of the following conditions must hold:
    *
    *  1. All writes to the value must be in the same block
    *  2. All instructions which read the value must satisfy one of the
    *     following:
    *      a. The instruction's only output is a write to the value (i.e.
    *         x |= 7 is ok.)
    *      b. The instruction is dominated by the final write to the value
    *
    * With these restrictions, we get most of the same benefits of SSA for the
    * purposes of RA, copy-prop, CSE (with a bit of work), etc. but can still
    * support multiple writes for constructing values such as instruction
    * headers, values requiring predicates, etc.
    *
    * When is_wlr is true, the list of writes must be maintained in program
    * order and ibc_validate asserts this.  Newly created logical registers
    * default is_wlr to true.
    */
   bool is_wlr;

   /** Index used for printing shaders */
   uint32_t index;

   /** Link in ibc_shader::regs */
   struct list_head link;

   /** List of all writes to this register */
   struct list_head writes;

   union {
      ibc_logical_reg logical;
      ibc_hw_grf_reg hw_grf;
      ibc_flag_reg flag;
   };
} ibc_reg;

ibc_reg *ibc_logical_reg_create(struct ibc_shader *shader,
                                uint8_t bit_size, uint8_t num_comps,
                                uint8_t simd_group, uint8_t simd_width);

ibc_reg *ibc_hw_grf_reg_create(struct ibc_shader *shader,
                               uint16_t byte, uint8_t size, uint8_t align);

ibc_reg *ibc_flag_reg_create(struct ibc_shader *shader,
                             uint8_t subnr, uint8_t bits);

#define ibc_reg_foreach_write(ref, reg) \
   list_for_each_entry(ibc_reg_ref, ref, &(reg)->writes, write_link)

struct ibc_instr *ibc_reg_ssa_instr(const ibc_reg *reg);

unsigned ibc_logical_reg_stride(const ibc_reg *reg);

/** A structure representing a reference to a LOGICAL register
 *
 * Logical registers have a 3D size in terms of bits, components, and SIMD
 * channels.  As such, the reference is also 3D with a byte offset, component
 * offset, and SIMD channel for broadcast reads operations.
 */
typedef struct ibc_logical_reg_ref {
   /** Byte offset into the logical register component
    *
    * This is used when the referenced register has a bit_size that is larger
    * than the reference type.  If the referenced register has a bit size
    * equal to the reference type, this must be zero.
    */
   uint8_t byte;

   /** Component to reference for logical registers */
   uint8_t comp;

   /** If true, broadcast one SIMD channel to all channels
    *
    * In most cases (when broadcast is false), the SIMD channel information is
    * taken from the instruction itself and each channel in the instruction
    * automatically reads from the corresponding channel in the logical
    * register.  However, when broadcast is set, all channels in the
    * instruction read from one channel specified by the simd_channel field.
    */
   bool broadcast;

   /** SIMD channel to broadcast
    *
    * If broadcast is set, this is the SIMD channel to broadcast.  If
    * broadcast is not set, this must be 0.
    */
   uint8_t simd_channel;
} ibc_logical_reg_ref;


/** A structure representing a reference to a HW_GRF register */
typedef struct ibc_hw_grf_reg_ref {
   /** Byte offset at which the reference starts for HW regs */
   uint8_t offset;

   /* Vertical stride of the HW register region in bytes
    *
    * This is different from the HW vstride where it's in units of the type.
    */
   uint8_t vstride;

   /* Width of the HW register region */
   uint8_t width;

   /** Horizontal stride of the HW register region in bytes
    *
    * This is different from the HW stride where it's in units of the type.
    */
   uint8_t hstride;
} ibc_hw_grf_reg_ref;

static inline void
ibc_hw_grf_slice_simd_group(ibc_hw_grf_reg_ref *ref,
                            uint8_t rel_simd_group, uint8_t simd_width)
{
   assert(rel_simd_group % simd_width == 0);
   if (ref->hstride * ref->width == ref->vstride) {
      ref->offset += rel_simd_group * ref->hstride;
   } else {
      /* If we don't have a regular stride, then we can only shift it by a
       * multiple of the width.  Otherwise, it will mess up the 2D region.
       */
      assert(rel_simd_group % ref->width == 0);
      ref->offset += (rel_simd_group / ref->width) * ref->vstride;
   }
}

static inline void
ibc_hw_grf_add_byte_offset(ibc_hw_grf_reg_ref *ref, unsigned byte_offset)
{
   ref->offset += byte_offset;
}

static inline void
ibc_hw_grf_mul_stride(ibc_hw_grf_reg_ref *ref, unsigned stride_mul)
{
   ref->vstride *= stride_mul;
   ref->hstride *= stride_mul;

   /* If we're making it a scalar, make the width 1.  This doesn't really
    * matter but it makes things more uniform in the IR and the width doesn't
    * matter if hstride and vstride are both zero.
    */
   if (stride_mul == 0)
      ref->width = 1;
}


/** A structure representing a register reference (source or destination) in
 * an instruction
 */
typedef struct ibc_reg_ref {
   /** Register file or IMM for immediate or NONE for null */
   enum ibc_reg_file file;

   /** Type with which the register or immediate is interpreted */
   enum ibc_type type;

   union {
      ibc_logical_reg_ref logical;
      ibc_hw_grf_reg_ref hw_grf;
   };

   /** Link in the ibc_reg::writes list
    *
    * This link will be valid if and only if write_instr != NULL
    */
   struct list_head write_link;

   /** A pointer to the instruction if this is a write */
   struct ibc_instr *write_instr;

   /** Pointer to the register; NULL if immediate */
   union {
      const ibc_reg *reg;

      /** 64 bits of immediate data for immediate sources */
      char imm[8];

      /* We want to cast the above char array to a uint64_t safely so throw
       * one in to assure that the union is properly aligned.  This field
       * should not be accessed directly.
       */
      uint64_t _align;
   };
} ibc_reg_ref;

/**
 * Returns true if the given ref reads the same value regardless of where the
 * read occurs in the program (assuming dominance rules still hold).
 */
static inline bool
ibc_reg_ref_read_is_static(ibc_reg_ref ref)
{
   if (ref.file == IBC_REG_FILE_NONE || ref.file == IBC_REG_FILE_IMM)
      return false;

   return ref.reg->is_wlr;
}

enum ibc_instr_type {
   IBC_INSTR_TYPE_ALU,
   IBC_INSTR_TYPE_SEND,
   IBC_INSTR_TYPE_INTRINSIC,
   IBC_INSTR_TYPE_PHI,
   IBC_INSTR_TYPE_BRANCH,
   IBC_INSTR_TYPE_MERGE,
};

/** A structure representing an instruction */
typedef struct ibc_instr {
   /** The type of this instruction */
   enum ibc_instr_type type;

   /** Link in ibc_shader::instrs */
   struct list_head link;

   uint8_t simd_group;
   uint8_t simd_width;
   bool we_all;

   /** Flag reference for predication or cmod */
   ibc_reg_ref flag;

   /** If not BRW_PREDICATE_NONE, this in struction is predicated using the
    * predicate from flag.
    */
   enum brw_predicate predicate;

   /** True if the predicate is to be inverted */
   bool pred_inverse;
} ibc_instr;

typedef bool (*ibc_reg_ref_cb)(ibc_reg_ref *ref, int8_t num_comps,
                               uint8_t simd_group, uint8_t simd_width,
                               void *state);
bool ibc_instr_foreach_read(ibc_instr *instr, ibc_reg_ref_cb cb, void *state);
bool ibc_instr_foreach_write(ibc_instr *instr, ibc_reg_ref_cb cb, void *state);

void ibc_instr_set_write_ref(ibc_instr *instr, ibc_reg_ref *write_ref,
                             ibc_reg_ref new_ref);
void ibc_instr_set_predicate(ibc_instr *instr, ibc_reg_ref flag,
                             enum brw_predicate predicate,
                             bool pred_inverse);


/** Enum of IBC ALU opcodes */
enum ibc_alu_op {
#define IBC_ALU_OP_DECL(OP, ...) IBC_ALU_OP_##OP,
#include "ibc_alu_ops.h"
#undef IBC_ALU_OP_DECL
   IBC_ALU_NUM_OPS,
};

enum PACKED ibc_alu_src_mod {
   IBC_ALU_SRC_MOD_NONE    = 0x0,
   IBC_ALU_SRC_MOD_NEG     = 0x1,
   IBC_ALU_SRC_MOD_ABS     = 0x2,
   IBC_ALU_SRC_MOD_NEG_ABS = IBC_ALU_SRC_MOD_NEG | IBC_ALU_SRC_MOD_ABS,
   IBC_ALU_SRC_MOD_NOT     = 0x4,
};

typedef struct ibc_alu_op_info {
   const char *name;

   unsigned num_srcs:2;

   enum ibc_alu_src_mod supported_src_mods;
} ibc_alu_op_info;

extern const ibc_alu_op_info ibc_alu_op_infos[IBC_ALU_NUM_OPS];

/** A structure representing an ALU instruction source */
typedef struct ibc_alu_src {
   /** A register reference for non-immediate sources */
   ibc_reg_ref ref;

   enum ibc_alu_src_mod mod;
} ibc_alu_src;


typedef struct ibc_alu_instr {
   ibc_instr instr;

   /** Opcode */
   enum ibc_alu_op op;

   enum brw_conditional_mod cmod;

   bool saturate;

   ibc_reg_ref dest;

   ibc_alu_src src[0];
} ibc_alu_instr;

IBC_DEFINE_CAST(ibc_instr_as_alu, ibc_instr, ibc_alu_instr, instr,
                type, IBC_INSTR_TYPE_ALU)

ibc_alu_instr *ibc_alu_instr_create(struct ibc_shader *shader,
                                    enum ibc_alu_op op,
                                    uint8_t simd_group,
                                    uint8_t simd_width);


typedef struct ibc_send_instr {
   ibc_instr instr;

   unsigned sfid:4;
   unsigned mlen:4;
   unsigned ex_mlen:4;
   unsigned rlen:4;
   bool has_header:1;
   bool has_side_effects:1;
   bool check_tdr:1;
   bool eot:1;

   uint32_t desc_imm;
   uint32_t ex_desc_imm;
   ibc_reg_ref desc;
   ibc_reg_ref ex_desc;

   ibc_reg_ref dest;

   ibc_reg_ref payload[2];
} ibc_send_instr;

IBC_DEFINE_CAST(ibc_instr_as_send, ibc_instr, ibc_send_instr, instr,
                type, IBC_INSTR_TYPE_SEND)

ibc_send_instr *ibc_send_instr_create(struct ibc_shader *shader,
                                      uint8_t simd_group,
                                      uint8_t simd_width);


enum ibc_intrinsic_op {
   IBC_INTRINSIC_OP_INVALID,
   IBC_INTRINSIC_OP_SIMD_ZIP,
   IBC_INTRINSIC_OP_BTI_UNTYPED_READ,
   IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE,
};

typedef struct {
   ibc_reg_ref ref;

   /** SIMD invocation offset */
   uint8_t simd_group;

   /** Number of SIMD invocations */
   uint8_t simd_width;

   /** Number of vector components produced or consumed via this ref */
   unsigned num_comps;
} ibc_intrinsic_src;

typedef struct {
   ibc_instr instr;

   enum ibc_intrinsic_op op;

   bool has_side_effects;

   ibc_reg_ref dest;
   unsigned num_dest_comps;

   unsigned num_srcs;
   ibc_intrinsic_src src[0];
} ibc_intrinsic_instr;

IBC_DEFINE_CAST(ibc_instr_as_intrinsic, ibc_instr, ibc_intrinsic_instr, instr,
                type, IBC_INSTR_TYPE_INTRINSIC)

ibc_intrinsic_instr *ibc_intrinsic_instr_create(struct ibc_shader *shader,
                                                enum ibc_intrinsic_op op,
                                                uint8_t simd_group,
                                                uint8_t simd_width,
                                                unsigned num_srcs);

typedef struct ibc_merge_instr ibc_merge_instr;
typedef struct ibc_branch_instr ibc_branch_instr;

enum ibc_merge_op {
   IBC_MERGE_OP_MERGE, /**< Generic merge */
   IBC_MERGE_OP_ENDIF,
   IBC_MERGE_OP_DO,
   IBC_MERGE_OP_START, /**< Start of program */
};

typedef struct ibc_merge_pred {
   /** Link in ibc_merge_instr::preds */
   struct list_head link;

   bool logical;

   ibc_branch_instr *branch;
} ibc_merge_pred;

struct ibc_merge_instr {
   ibc_instr instr;

   enum ibc_merge_op op;

   uint32_t block_index;

   ibc_branch_instr *block_end;

   /** List of predecessors */
   struct list_head preds;
};

IBC_DEFINE_CAST(ibc_instr_as_merge, ibc_instr, ibc_merge_instr, instr,
                type, IBC_INSTR_TYPE_MERGE)

ibc_merge_instr *ibc_merge_instr_create(struct ibc_shader *shader,
                                        enum ibc_merge_op op,
                                        uint8_t simd_width);

#define ibc_foreach_merge_instr(merge, shader) \
   for (ibc_merge_instr *merge = LIST_ENTRY(ibc_merge_instr, \
           shader->instrs.next, instr.link); \
        &merge->instr.link != &shader->instrs ? \
           assert(merge->instr.type == IBC_INSTR_TYPE_MERGE), true : false; \
        merge = LIST_ENTRY(ibc_merge_instr, \
           merge->block_end->instr.link.next, instr.link))

enum ibc_branch_op {
   IBC_BRANCH_OP_NEXT, /**< Just fall through to the next instruction */
   IBC_BRANCH_OP_IF,
   IBC_BRANCH_OP_ELSE,
   IBC_BRANCH_OP_WHILE,
   IBC_BRANCH_OP_BREAK,
   IBC_BRANCH_OP_CONTINUE,
   IBC_BRANCH_OP_END, /**< End of program */
};

struct ibc_branch_instr {
   ibc_instr instr;

   enum ibc_branch_op op;

   ibc_merge_instr *block_start;

   ibc_merge_instr *jump;
   ibc_merge_instr *merge;
};

IBC_DEFINE_CAST(ibc_instr_as_branch, ibc_instr, ibc_branch_instr, instr,
                type, IBC_INSTR_TYPE_BRANCH)

ibc_branch_instr *ibc_branch_instr_create(struct ibc_shader *shader,
                                          enum ibc_branch_op op,
                                          uint8_t simd_width);

#define ibc_foreach_branch_instr_reverse(branch, shader) \
   for (ibc_branch_instr *branch = LIST_ENTRY(ibc_branch_instr, \
           shader->instrs.prev, instr.link); \
        &branch->instr.link != &shader->instrs ? \
           assert(branch->instr.type == IBC_INSTR_TYPE_BRANCH), true : false; \
        branch = LIST_ENTRY(ibc_branch_instr, \
           branch->block_start->instr.link.prev, instr.link))

/**
 * Returns true of the immediately following merge instruction is also a
 * successor of this branch instruction.
 */
static inline bool
ibc_branch_instr_falls_through(ibc_branch_instr *branch)
{
   switch (branch->op) {
   case IBC_BRANCH_OP_NEXT:
   case IBC_BRANCH_OP_IF:
   case IBC_BRANCH_OP_ELSE:
      return true;
   case IBC_BRANCH_OP_WHILE:
   case IBC_BRANCH_OP_BREAK:
   case IBC_BRANCH_OP_CONTINUE:
      return branch->instr.predicate != BRW_PREDICATE_NONE;
   case IBC_BRANCH_OP_END:
      return false;
   }
   unreachable("Invalid branch instruction opcode");
}

typedef struct ibc_phi_src {
   /* Link in ibc_phi_instr::srcs */
   struct list_head link;

   ibc_branch_instr *pred;
   ibc_reg_ref ref;
} ibc_phi_src;

typedef struct ibc_phi_instr {
   ibc_instr instr;

   /** Number of vector components produced or consumed via this ref */
   unsigned num_comps;

   ibc_reg_ref dest;

   struct list_head srcs;
} ibc_phi_instr;

IBC_DEFINE_CAST(ibc_instr_as_phi, ibc_instr, ibc_phi_instr, instr,
                type, IBC_INSTR_TYPE_PHI)

#define ibc_foreach_phi_src(phi_src, phi) \
   list_for_each_entry(ibc_phi_src, phi_src, &(phi)->srcs, link)

ibc_phi_instr *ibc_phi_instr_create(struct ibc_shader *shader,
                                    uint8_t simd_group, uint8_t simd_width);

#define ibc_foreach_instr(instr, shader) \
   list_for_each_entry(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_reverse(instr, shader) \
   list_for_each_entry_rev(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_safe(instr, shader) \
   list_for_each_entry_safe(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_reverse_safe(instr, shader) \
   list_for_each_entry_safe_rev(ibc_instr, instr, &(shader)->instrs, link)

static inline ibc_instr *
ibc_instr_next(const ibc_instr *instr)
{
   assert(instr->type != IBC_INSTR_TYPE_BRANCH ||
          ibc_instr_as_branch(instr)->op != IBC_BRANCH_OP_END);
   return LIST_ENTRY(ibc_instr, instr->link.next, link);
}

static inline ibc_instr *
ibc_instr_prev(const ibc_instr *instr)
{
   assert(instr->type != IBC_INSTR_TYPE_MERGE ||
          ibc_instr_as_merge(instr)->op != IBC_MERGE_OP_START);
   return LIST_ENTRY(ibc_instr, instr->link.prev, link);
}


typedef struct ibc_shader {
   const struct gen_device_info *devinfo;

   uint8_t simd_width;

   /** Instructions */
   struct list_head instrs;

   /** Registers */
   struct list_head regs;
} ibc_shader;

ibc_shader *ibc_shader_create(void *mem_ctx,
                              const struct gen_device_info *devinfo,
                              uint8_t simd_width);

#define ibc_foreach_reg(reg, shader) \
   list_for_each_entry(ibc_reg, reg, &(shader)->regs, link)

#define ibc_foreach_reg_safe(reg, shader) \
   list_for_each_entry_safe(ibc_reg, reg, &(shader)->regs, link)

typedef struct {
   struct list_head *prev;
} ibc_cursor;

static inline ibc_cursor
ibc_before_instr(ibc_instr *instr)
{
   return (ibc_cursor) { instr->link.prev };
}

static inline ibc_cursor
ibc_after_instr(ibc_instr *instr)
{
   return (ibc_cursor) { &instr->link };
}

static inline ibc_cursor
ibc_before_shader(ibc_shader *shader)
{
   return (ibc_cursor) { &shader->instrs };
}

void ibc_instr_insert(ibc_instr *instr, ibc_cursor cursor);
void ibc_instr_remove(ibc_instr *instr);


/**********************************************************************
 * IBC validation, optimization and lowering passes
 *
 * KEEP IN ALPHABETICAL ORDER!
 **********************************************************************/

struct nir_shader;
ibc_shader *nir_to_ibc(const struct nir_shader *nir, void *mem_ctx,
                       unsigned dispatch_size,
                       const struct gen_device_info *devinfo);

void ibc_assign_and_lower_flags(ibc_shader *shader);
void ibc_assign_regs(ibc_shader *shader);

bool ibc_lower_gather_ops(ibc_shader *shader);
bool ibc_lower_phis(ibc_shader *shader);
bool ibc_lower_simd_width(ibc_shader *shader);
bool ibc_lower_surface_access(ibc_shader *shader);

bool ibc_opt_copy_prop(ibc_shader *shader);
bool ibc_opt_dead_code(ibc_shader *shader);

void ibc_print_shader(const ibc_shader *shader, FILE *fp);

bool ibc_split_logical_regs(ibc_shader *shader);

unsigned *ibc_to_binary(const ibc_shader *shader, void *mem_ctx,
                        unsigned *program_size);

void ibc_validate_shader(const ibc_shader *shader);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_H */
