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

#include <compiler/shader_enums.h>

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
struct brw_compiler;
struct shader_info;
struct ibc_instr;
struct ibc_alu_instr;
struct ibc_shader;
struct ibc_builder;

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
enum PACKED ibc_file {
   IBC_FILE_NONE,
   IBC_FILE_IMM,
   IBC_FILE_LOGICAL,
   IBC_FILE_HW_GRF,
   IBC_FILE_FLAG,
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

   /** Stride in bytes
    *
    * For logical registers with simd_width > 1, this gives the stride between
    * SIMD channels in bytes or is 0 indicating the stride is undecided.  For
    * registers with simd_width == 1, stride is always zero.
    */
   uint8_t stride;

   /** True if this register must be packed for a SEND
    *
    * Packed requires that stride == bit_size / 8 and requires that the
    * register be allocated in such a way that that the register consumes a
    * contiguous region of the register file.
    */
   bool packed;
} ibc_logical_reg;


/** A struct representing a HW GRF register
 *
 * A physical HW register representing an actual byte range in the hardware
 * general-purpose register file.  Unlike logical registers HW GRF registers
 * have a simple 1D size in bytes and have no concept of SIMD width or
 * invocation offset.
 */
typedef struct ibc_hw_grf_reg {
   /** Size of this register in bytes */
   uint16_t size;

   /** Alignment requirement of this register in bytes */
   uint8_t align;
} ibc_hw_grf_reg;


/** A struct representing a flag register
 *
 * A flag register is only one-dimensional; it only has a number of bits.
 * It can be viewed in one of two ways:
 *
 *  * When written with a flag destination or read as a conditional modifier,
 *    it's viewed as a 1-bit scalar with one bit per SIMD invocation.
 *
 *  * When read or written as a regular source, it's viewed as a N-Bit scalar
 *    with only one SIMD invocation.
 */
typedef struct ibc_flag_reg {
   /** Size in bits */
   uint8_t bits;

   /** Alignment requirement of this register
    *
    * The flag register is aligned such that the subnr chosen satisfies
    *
    *       subnr = k * align_mul + align_offset
    *
    * for some integer k.  This allows us to handle the alignment requirements
    * of flag registers which are allocated for use with second-half execution
    * groups.
    */
   uint8_t align_mul;
   uint8_t align_offset;
} ibc_flag_reg;


/** A struct representing a register */
typedef struct ibc_reg {
   /** Register type */
   enum ibc_file file;

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
                               uint16_t size, uint8_t align);

ibc_reg *ibc_flag_reg_create(struct ibc_shader *shader, uint8_t bits);

#define ibc_reg_foreach_write(ref, reg) \
   list_for_each_entry(ibc_reg_write, ref, &(reg)->writes, link)

#define ibc_reg_foreach_write_safe(ref, reg) \
   list_for_each_entry_safe(ibc_reg_write, ref, &(reg)->writes, link)

struct ibc_instr *ibc_reg_ssa_instr(const ibc_reg *reg);

/** A structure representing a LOGICAL register access region
 *
 * Logical registers have a 3D size in terms of bits, components, and SIMD
 * channels.  As such, the reference is also 3D with a byte offset, component
 * offset, and SIMD channel for broadcast reads operations.
 *
 * This is a sub-struct of ibc_ref
 */
struct ibc_ref_logical {
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
};


/** A structure representing a HW_GRF register access region
 *
 * This is a sub-struct of ibc_ref
 */
struct ibc_ref_hw_grf {
   /** Byte offset at which the reference starts
    *
    * If ibc_ref::reg is not NULL, this is relative to the start of the
    * virtual HW reg.  If ibc_ref::reg is NULL, this is the byte offset
    * from the start of the register file.
    */
   uint16_t byte;

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
};

static inline void
ibc_hw_grf_simd_slice(struct ibc_ref_hw_grf *ref, uint8_t rel_simd_group)
{
   if (ref->hstride * ref->width == ref->vstride) {
      ref->byte += rel_simd_group * ref->hstride;
   } else {
      /* If we don't have a regular stride, then we can only shift it by a
       * multiple of the width.  Otherwise, it will mess up the 2D region.
       */
      assert(rel_simd_group % ref->width == 0);
      ref->byte += (rel_simd_group / ref->width) * ref->vstride;
   }
}

static inline void
ibc_hw_grf_add_byte_offset(struct ibc_ref_hw_grf *ref, unsigned byte_offset)
{
   ref->byte += byte_offset;
}

static inline void
ibc_hw_grf_mul_stride(struct ibc_ref_hw_grf *ref, unsigned stride_mul)
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

static inline int
ibc_hw_grf_comp_stride(struct ibc_ref_hw_grf ref, unsigned num_bytes,
                       uint8_t simd_width)
{
   /* If it's a replicated scalar, use the component size */
   if (ref.vstride == 0)
      return num_bytes;

   if (simd_width >= ref.width) {
      assert(simd_width % ref.width == 0);
      return ref.vstride * (simd_width / ref.width);
   }

   assert(ref.hstride * ref.width == ref.vstride);
   assert(ref.hstride >= num_bytes);
   return ref.hstride * simd_width;
}


/** A structure representing FLAG register access region
 *
 * This is a sub-struct of ibc_ref
 */
struct ibc_ref_flag {
   /** Bit at which the reference starts
    *
    * If ibc_ref::reg is not NULL, this is relative to the start of the
    * virtual flag reg.  If ibc_ref::reg is NULL, this is relative to the
    * hardware flag f0.0 where f1.0 starts at bit 32.
    */
   uint8_t bit;
};


/** A structure representing a register reference (source or destination) in
 * an instruction
 */
typedef struct ibc_ref {
   /** Register file or IMM for immediate or NONE for null */
   enum ibc_file file;

   /** Type with which the register or immediate is interpreted */
   enum ibc_type type;

   union {
      struct ibc_ref_logical logical;
      struct ibc_ref_hw_grf hw_grf;
      struct ibc_ref_flag flag;
   };

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
} ibc_ref;

bool ibc_refs_equal(ibc_ref a, ibc_ref b);

static inline uint64_t
ibc_ref_as_uint(ibc_ref ref)
{
   assert(ibc_type_base_type(ref.type) != IBC_TYPE_FLOAT);
   uint64_t data = 0;
   memcpy(&data, ref.imm, ibc_type_byte_size(ref.type));
   return data;
}

static inline int64_t
ibc_ref_as_int(ibc_ref ref)
{
   int64_t data = ibc_ref_as_uint(ref);
   unsigned shift = 8 - ibc_type_byte_size(ref.type);
   return (data << shift) >> shift;
}

/**
 * Returns true if the given ref reads the same value regardless of where the
 * read occurs in the program (assuming dominance rules still hold).
 */
static inline bool
ibc_ref_read_is_static(ibc_ref ref)
{
   if (ref.file == IBC_FILE_NONE || ref.file == IBC_FILE_IMM)
      return true;

   return ref.reg && ref.reg->is_wlr;
}

/**
 * Returns true if the given ref reads the same value in all SIMD channels.
 */
static inline bool
ibc_ref_read_is_uniform(ibc_ref ref)
{
   switch (ref.file) {
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
      return true;

   case IBC_FILE_LOGICAL:
      return ref.logical.broadcast || ref.reg->logical.simd_width == 1;

   case IBC_FILE_HW_GRF:
      return ref.hw_grf.vstride == 0 && ref.hw_grf.hstride == 0;

   case IBC_FILE_FLAG:
      return ibc_type_bit_size(ref.type) > 1;
   }

   unreachable("Invalid IBC reg file");
}

static inline void
ibc_ref_simd_slice(ibc_ref *ref, uint8_t rel_simd_group)
{
   switch (ref->file) {
   case IBC_FILE_NONE:
   case IBC_FILE_IMM:
   case IBC_FILE_LOGICAL:
      return;

   case IBC_FILE_HW_GRF:
      ibc_hw_grf_simd_slice(&ref->hw_grf, rel_simd_group);
      return;

   case IBC_FILE_FLAG:
      if (ref->type == IBC_TYPE_FLAG) {
         ref->flag.bit += rel_simd_group;
      } else {
         assert(ref->flag.bit % ibc_type_bit_size(ref->type) == 0);
      }
      return;
   }

   unreachable("Unhandled register file");
}

static inline void
ibc_hw_grf_ref_foreach_byte(ibc_ref ref,
                            unsigned num_comps,
                            uint8_t simd_width,
                            void (*cb)(unsigned byte, void *data),
                            void *data)
{
   assert(ref.file == IBC_FILE_HW_GRF);

   unsigned ref_byte_size = ibc_type_byte_size(ref.type);
   assert(ref.hw_grf.vstride % ref_byte_size == 0);
   assert(ref.hw_grf.hstride % ref_byte_size == 0);

   if (ref.hw_grf.hstride == 0 && ref.hw_grf.vstride == 0) {
      for (unsigned b = 0; b < ref_byte_size * num_comps; b++)
         cb(ref.hw_grf.byte + b, data);
   } else if (ref.hw_grf.vstride == ref.hw_grf.hstride * ref.hw_grf.width) {
      /* In this case, things are nicely strided out and computing the access
       * mask is easy and fairly efficient.
       */
      unsigned offset = ref.hw_grf.byte;
      for (unsigned i = 0; i < simd_width * num_comps; i++) {
         for (unsigned b = 0; b < ref_byte_size; b++)
            cb(offset + b, data);
         offset += ref.hw_grf.hstride;
      }
   } else {
      unsigned offset = ref.hw_grf.byte;
      for (unsigned c = 0; c < num_comps; c++) {
         unsigned voffset = 0;
         unsigned hoffset = 0;
         for (unsigned s = 0; s < simd_width;) {
            for (unsigned b = 0; b < ref_byte_size; b++)
               cb(offset + voffset + hoffset + b, data);

            s++;
            assert(util_is_power_of_two_nonzero(ref.hw_grf.width));
            if ((s & (ref.hw_grf.width - 1)) == 0) {
               voffset += ref.hw_grf.vstride;
               hoffset = 0;
            } else {
               hoffset += ref.hw_grf.hstride;
            }
         }
         offset += ibc_hw_grf_comp_stride(ref.hw_grf, ref_byte_size,
                                          simd_width);
      }
   }
}

typedef struct ibc_reg_write {
   /** Link in ibc_reg::writes
    *
    * This link will be valid if and only if instr != NULL
    */
   struct list_head link;

   /** Instruction performing the write */
   struct ibc_instr *instr;
} ibc_reg_write;

ibc_ref *ibc_reg_write_get_ref(ibc_reg_write *write);


#define IBC_PREDICATE_INVERSE 0x10

enum PACKED ibc_predicate {
   /** Corresponds to the PredCtrl bits. ANYV and ALLV are not used. */
   IBC_PREDICATE_NONE   =  0,
   IBC_PREDICATE_NORMAL =  1,
   IBC_PREDICATE_ANY2H  =  4,
   IBC_PREDICATE_ALL2H  =  5,
   IBC_PREDICATE_ANY4H  =  6,
   IBC_PREDICATE_ALL4H  =  7,
   IBC_PREDICATE_ANY8H  =  8,
   IBC_PREDICATE_ALL8H  =  9,
   IBC_PREDICATE_ANY16H = 10,
   IBC_PREDICATE_ALL16H = 11,
   IBC_PREDICATE_ANY32H = 12,
   IBC_PREDICATE_ALL32H = 13,

   IBC_PREDICATE_NOT        = IBC_PREDICATE_NORMAL | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ANY2H  = IBC_PREDICATE_ANY2H  | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ALL2H  = IBC_PREDICATE_ALL2H  | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ANY4H  = IBC_PREDICATE_ANY4H  | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ALL4H  = IBC_PREDICATE_ALL4H  | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ANY8H  = IBC_PREDICATE_ANY8H  | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ALL8H  = IBC_PREDICATE_ALL8H  | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ANY16H = IBC_PREDICATE_ANY16H | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ALL16H = IBC_PREDICATE_ALL16H | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ANY32H = IBC_PREDICATE_ANY32H | IBC_PREDICATE_INVERSE,
   IBC_PREDICATE_NOT_ALL32H = IBC_PREDICATE_ALL32H | IBC_PREDICATE_INVERSE,
};

static inline enum ibc_predicate
ibc_predicate_control(enum ibc_predicate pred)
{
   return (enum ibc_predicate)((int)pred & ~IBC_PREDICATE_INVERSE);
}

static inline bool
ibc_predicate_is_inverted(enum ibc_predicate pred)
{
   return pred & IBC_PREDICATE_INVERSE;
}

static inline enum ibc_predicate
ibc_predicate_invert(enum ibc_predicate pred)
{
   if (pred == IBC_PREDICATE_NONE)
      return pred;
   else
      return (enum ibc_predicate)((int)pred ^ IBC_PREDICATE_INVERSE);
}

enum ibc_instr_type {
   IBC_INSTR_TYPE_ALU,
   IBC_INSTR_TYPE_SEND,
   IBC_INSTR_TYPE_INTRINSIC,
   IBC_INSTR_TYPE_FLOW,
};

/** A structure representing an instruction */
typedef struct ibc_instr {
   /** The type of this instruction */
   enum ibc_instr_type type;

   /** Instruction index used for liveness etc. */
   uint32_t index;

   /** Link in ibc_shader::instrs */
   struct list_head link;

   uint8_t simd_group;
   uint8_t simd_width;
   bool we_all;

   /** Flag reference for predication or cmod */
   ibc_ref flag;

   /** If not IBC_PREDICATE_NONE, this instruction is predicated using the
    * predicate from flag.
    */
   enum ibc_predicate predicate;
} ibc_instr;

typedef bool (*ibc_ref_cb)(ibc_ref *ref,
                           int num_bytes, int num_comps,
                           uint8_t simd_group, uint8_t simd_width,
                           void *state);
bool ibc_instr_foreach_read(ibc_instr *instr, ibc_ref_cb cb, void *state);
bool ibc_instr_foreach_write(ibc_instr *instr, ibc_ref_cb cb, void *state);

typedef bool (*ibc_reg_write_cb)(ibc_reg_write *write,
                                 ibc_ref *ref, void *state);
bool ibc_instr_foreach_reg_write(ibc_instr *instr,
                                 ibc_reg_write_cb cb, void *state);

void ibc_instr_set_ref(ibc_instr *instr, ibc_ref *ref,
                       ibc_ref new_ref);
void ibc_instr_set_predicate(ibc_instr *instr, ibc_ref flag,
                             enum ibc_predicate predicate);


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

enum PACKED ibc_alu_op_prop {
   IBC_ALU_OP_PROP_NONE          = 0x0,
};

typedef struct ibc_alu_op_info {
   const char *name;

   unsigned num_srcs:2;

   enum ibc_alu_src_mod supported_src_mods;
   enum ibc_alu_op_prop props;
} ibc_alu_op_info;

extern const ibc_alu_op_info ibc_alu_op_infos[IBC_ALU_NUM_OPS];

/** A structure representing an ALU instruction source */
typedef struct ibc_alu_src {
   /** A register reference for non-immediate sources */
   ibc_ref ref;

   enum ibc_alu_src_mod mod;
} ibc_alu_src;


typedef struct ibc_alu_instr {
   ibc_instr instr;

   /** Opcode */
   enum ibc_alu_op op;

   enum brw_conditional_mod cmod;
   ibc_reg_write cmod_write;

   bool saturate;

   ibc_ref dest;
   ibc_reg_write dest_write;

   ibc_alu_src src[0];
} ibc_alu_instr;

IBC_DEFINE_CAST(ibc_instr_as_alu, ibc_instr, ibc_alu_instr, instr,
                type, IBC_INSTR_TYPE_ALU)

ibc_alu_instr *ibc_alu_instr_create(struct ibc_shader *shader,
                                    enum ibc_alu_op op,
                                    uint8_t simd_group,
                                    uint8_t simd_width);
void ibc_alu_instr_set_cmod(ibc_alu_instr *alu, ibc_ref flag,
                            enum brw_conditional_mod cmod);


typedef struct ibc_send_instr {
   ibc_instr instr;

   unsigned sfid:4;
   unsigned mlen:4;
   unsigned ex_mlen:4;
   unsigned rlen:4;
   bool has_header:1;
   bool can_reorder:1;
   bool has_side_effects:1;
   bool check_tdr:1;
   bool eot:1;

   uint32_t desc_imm;
   uint32_t ex_desc_imm;
   ibc_ref desc;
   ibc_ref ex_desc;

   ibc_ref dest;
   ibc_reg_write dest_write;

   ibc_ref payload[2];
} ibc_send_instr;

IBC_DEFINE_CAST(ibc_instr_as_send, ibc_instr, ibc_send_instr, instr,
                type, IBC_INSTR_TYPE_SEND)

ibc_send_instr *ibc_send_instr_create(struct ibc_shader *shader,
                                      uint8_t simd_group,
                                      uint8_t simd_width);


enum ibc_intrinsic_op {
   IBC_INTRINSIC_OP_INVALID,
   IBC_INTRINSIC_OP_UNDEF,
   IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL,
   IBC_INTRINSIC_OP_SIMD_BROADCAST,
   IBC_INTRINSIC_OP_SIMD_ZIP,
   IBC_INTRINSIC_OP_VEC,
   IBC_INTRINSIC_OP_LOAD_PAYLOAD,
   IBC_INTRINSIC_OP_PLN,
   IBC_INTRINSIC_OP_ALIGN16_DDX_FINE,
   IBC_INTRINSIC_OP_FB_WRITE,
   IBC_INTRINSIC_OP_URB_WRITE,

   /* Sources:
    *  [0] = HW_GRF if it's an initial push constant load
    *  [1] = BTI or NONE for push constant only
    *  [2] = Offset in bytes
    */
   IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO,

   /* Surface opcodes.  See ibc_surface_src */
   IBC_INTRINSIC_OP_BTI_TYPED_READ,
   IBC_INTRINSIC_OP_BTI_TYPED_WRITE,
   IBC_INTRINSIC_OP_BTI_TYPED_ATOMIC,
   IBC_INTRINSIC_OP_BTI_UNTYPED_READ,
   IBC_INTRINSIC_OP_BTI_UNTYPED_WRITE,
   IBC_INTRINSIC_OP_BTI_UNTYPED_ATOMIC,

   /* Texture opcodes.  See ibc_tex_src */
   IBC_INTRINSIC_OP_TEX,
   IBC_INTRINSIC_OP_TXB,
   IBC_INTRINSIC_OP_TXL,
   IBC_INTRINSIC_OP_TXD,
   IBC_INTRINSIC_OP_TXF,
   IBC_INTRINSIC_OP_TXF_MS,
   IBC_INTRINSIC_OP_TXF_MCS,
   IBC_INTRINSIC_OP_TXS,
   IBC_INTRINSIC_OP_LOD,
   IBC_INTRINSIC_OP_TG4,
   IBC_INTRINSIC_OP_TG4_OFFSET,
   IBC_INTRINSIC_OP_SAMPLEINFO,
};

/* Sources for [un]typed surface intrinsics */
enum ibc_surface_src {
   IBC_SURFACE_SRC_SURFACE_BTI,     /**< Surface BTI */
   IBC_SURFACE_SRC_SURFACE_HANDLE,  /**< Surface bindless handle */
   IBC_SURFACE_SRC_ADDRESS,         /**< Surface offset or coordinate */
   IBC_SURFACE_SRC_DATA0,           /**< Surface write/atomic data */
   IBC_SURFACE_SRC_DATA1,           /**< Surface write/atomic data */
   IBC_SURFACE_SRC_ATOMIC_OP,       /**< Surface atomic opcode */
   IBC_SURFACE_NUM_SRCS,
};

/* Sources for texturing intrinsics */
enum ibc_tex_src {
   IBC_TEX_SRC_SURFACE_BTI,      /**< Surface BTI */
   IBC_TEX_SRC_SAMPLER_BTI,      /**< Sampler BTI */
   IBC_TEX_SRC_SURFACE_HANDLE,   /**< Surface bindless handle */
   IBC_TEX_SRC_SAMPLER_HANDLE,   /**< Sampler bindless handle */
   IBC_TEX_SRC_COORD,            /**< Texture coordinates */
   IBC_TEX_SRC_SHADOW_C,         /**< Shadow comparator */
   IBC_TEX_SRC_LOD,              /**< Texture LOD */
   IBC_TEX_SRC_MIN_LOD,          /**< Min LOD */
   IBC_TEX_SRC_DDX,              /**< dPdx */
   IBC_TEX_SRC_DDY,              /**< dPdy */
   IBC_TEX_SRC_SAMPLE_INDEX,     /**< Sample index */
   IBC_TEX_SRC_MCS,              /**< MCS data */
   IBC_TEX_SRC_TG4_OFFSET,       /**< TG4 offset */
   IBC_TEX_SRC_HEADER_BITS,      /**< Bits to OR into the SEND header */
   IBC_TEX_NUM_SRCS,
};

typedef struct {
   ibc_ref ref;

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

   /* TODO: Should this be based on the opcode? */
   bool can_reorder;
   bool has_side_effects;

   ibc_ref dest;
   ibc_reg_write dest_write;
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

enum ibc_flow_op {
   IBC_FLOW_OP_START, /**< Start of program */
   IBC_FLOW_OP_END,   /**< End of program */
   IBC_FLOW_OP_IF,
   IBC_FLOW_OP_ELSE,
   IBC_FLOW_OP_ENDIF,
   IBC_FLOW_OP_DO,
   IBC_FLOW_OP_BREAK,
   IBC_FLOW_OP_CONT,
   IBC_FLOW_OP_WHILE,
   IBC_FLOW_OP_HALT_JUMP,
   IBC_FLOW_OP_HALT_MERGE,
};

typedef struct ibc_flow_instr ibc_flow_instr;

typedef struct ibc_flow_pred {
   /** Link in ibc_flow_instr::preds */
   struct list_head link;

   /** Control-flow instruction ending the predecessor block */
   ibc_flow_instr *instr;
} ibc_flow_pred;

/** A structure representing a control-flow instruction
 *
 * Semantically, control-flow instructions sit between blocks.  If the
 * control-flow instruction is predicated, the predicate evaluation is
 * considered to happen in the block before it.  Semantically, IBC
 * control-flow instructions are like portals (as in the game Portal.)
 * Execution flows in one control-flow instruction from the instructions
 * before it and out another control-flow instruction to the instructions
 * after it.  In some cases (when ibc_flow_instr_falls_through() returns
 * true), the in and out control flow instructions may be the same (the
 * logical jump goes nowhere).
 *
 * All control-flow paths fall into one of two categories: logical and
 * physical.  Logical control-flow paths are those which follow the normal
 * logic of NIR or the source language.  Physical control-flow paths are
 * those which the EU may take with the current set of channels disabled.
 * For instance, there is a physical path from the block before an ELSE to
 * the block after the ELSE.  This is because the ELSE is executed by
 * disabling all the channels from the then and going through the ELSE.
 * Another example is that there is a physical path from every continue
 * instruction to the WHILE of a loop even though the logical flow jumps
 * to the do at the top of the loop.
 *
 * Each control-flow instruction contains a list of predecessors.  When
 * execution logicall exits a control-flow instruction it will have come
 * from one of the control-flow instructions in the predecessors list with
 * ibc_flow_pred::logical == true.  A control-flow instruction's logical
 * predecessors are the block that follows the instruction (if
 * ibc_flow_instr_falls_through() returns true) and the block that follows
 * ibc_flow_instr::jump if non-NULL.  One must be careful when reasoning
 * about control-flow instructions because the list of predecessors is for
 * the block after the instruction and the successors are those of the block
 * before the instruction.  The WHILE instruction is particularly tricky to
 * think about in this regard.  For more details on the exact rules for each
 * instruction, see ibc_validate_flow_instr().
 */
struct ibc_flow_instr {
   ibc_instr instr;

   enum ibc_flow_op op;

   uint32_t block_index;

   /** Link in ibc_shader::flow_instrs */
   struct list_head flow_link;

   /** List of predecessors */
   struct list_head preds;

   /** Link for use in builder lists
    *
    * For some types of flow instructions such as loop breaks, the builder
    * needs to hold on to lists of previously emitted instructions so it can
    * link up CF edges after the fact.  This provides a link for that purpose.
    */
   struct list_head builder_link;

   /** The instruction to which this instruction logically jumps
    *
    * This is the path taken if the instruction does not fall through.
    */
   ibc_flow_instr *jump;

   /** The instruction where all paths from this instruction re-converge */
   ibc_flow_instr *merge;
};

IBC_DEFINE_CAST(ibc_instr_as_flow, ibc_instr, ibc_flow_instr, instr,
                type, IBC_INSTR_TYPE_FLOW)

ibc_flow_instr *ibc_flow_instr_create(struct ibc_shader *shader,
                                      enum ibc_flow_op op,
                                      uint8_t simd_width);
void ibc_flow_instr_add_pred(struct ibc_flow_instr *flow,
                             struct ibc_flow_instr *pred);

static inline void
ibc_flow_instr_set_jump(struct ibc_flow_instr *flow,
                        struct ibc_flow_instr *jump)
{
   flow->jump = jump;
   ibc_flow_instr_add_pred(jump, flow);
}

#define ibc_foreach_flow_instr(flow, shader) \
   list_for_each_entry(ibc_flow_instr, flow, &(shader)->flow_instrs, flow_link)

#define ibc_foreach_flow_instr_reverse(flow, shader) \
   list_for_each_entry_rev(ibc_flow_instr, flow, &(shader)->flow_instrs, flow_link)

static inline ibc_flow_instr *
ibc_flow_instr_next(const ibc_flow_instr *flow)
{
   assert(flow->op != IBC_FLOW_OP_END);
   return LIST_ENTRY(ibc_flow_instr, flow->flow_link.next, flow_link);
}

static inline ibc_flow_instr *
ibc_flow_instr_prev(const ibc_flow_instr *flow)
{
   assert(flow->op != IBC_FLOW_OP_START);
   return LIST_ENTRY(ibc_flow_instr, flow->flow_link.prev, flow_link);
}

#define ibc_foreach_flow_pred(pred, flow) \
   list_for_each_entry(ibc_flow_pred, pred, &(flow)->preds, link)

/**
 * Returns true of the flow instruction logically falls through to the
 * subsequent block of non-flow instructions.  All instructions may
 * physically fall through.
 */
static inline bool
ibc_flow_instr_falls_through(const ibc_flow_instr *flow)
{
   switch (flow->op) {
   case IBC_FLOW_OP_START:
   case IBC_FLOW_OP_END:
      return false;

   case IBC_FLOW_OP_IF:
      assert(flow->instr.predicate != IBC_PREDICATE_NONE);
      return true;

   case IBC_FLOW_OP_ELSE:
      return false;

   case IBC_FLOW_OP_ENDIF:
   case IBC_FLOW_OP_DO:
      return true;

   case IBC_FLOW_OP_BREAK:
   case IBC_FLOW_OP_CONT:
   case IBC_FLOW_OP_WHILE:
   case IBC_FLOW_OP_HALT_JUMP:
      return flow->instr.predicate != IBC_PREDICATE_NONE;

   case IBC_FLOW_OP_HALT_MERGE:
      return true;
   }
   unreachable("Invalid branch instruction opcode");
}


#define ibc_foreach_instr(instr, shader) \
   list_for_each_entry(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_reverse(instr, shader) \
   list_for_each_entry_rev(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_safe(instr, shader) \
   list_for_each_entry_safe(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_reverse_safe(instr, shader) \
   list_for_each_entry_safe_rev(ibc_instr, instr, &(shader)->instrs, link)

#define ibc_foreach_instr_from(instr, shader, start) \
   list_for_each_entry_from(ibc_instr, instr, start, &(shader)->instrs, link)

static inline ibc_instr *
ibc_instr_next(const ibc_instr *instr)
{
   assert(instr->type != IBC_INSTR_TYPE_FLOW ||
          ibc_instr_as_flow(instr)->op != IBC_FLOW_OP_END);
   return LIST_ENTRY(ibc_instr, instr->link.next, link);
}

static inline ibc_instr *
ibc_instr_prev(const ibc_instr *instr)
{
   assert(instr->type != IBC_INSTR_TYPE_FLOW ||
          ibc_instr_as_flow(instr)->op != IBC_FLOW_OP_START);
   return LIST_ENTRY(ibc_instr, instr->link.prev, link);
}

static inline bool
ibc_instr_writes_flag(const ibc_instr *instr)
{
   return instr->type == IBC_INSTR_TYPE_ALU &&
          ibc_instr_as_alu(instr)->cmod != BRW_CONDITIONAL_NONE;
}


typedef struct ibc_shader {
   const struct gen_device_info *devinfo;

   gl_shader_stage stage;
   uint8_t simd_width;
   bool use_vmask:1;
   bool has_packed_dispatch:1;

   /** Instructions */
   struct list_head instrs;

   /** Control flow instructions */
   struct list_head flow_instrs;

   /** Registers */
   struct list_head regs;

   /** HW_GRF reg representing g0
    *
    * Used for constructing message headers when lowering intrinsic to SEND
    * instructions.
    */
   ibc_reg *g0;

   /** Estimated cycle count from the scheduler */
   uint32_t cycles;
} ibc_shader;

ibc_shader *ibc_shader_create(void *mem_ctx,
                              const struct gen_device_info *devinfo,
                              gl_shader_stage stage,
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

bool ibc_should_print_shader(const ibc_shader *ibc);

#define IBC_PASS(progress, ibc, pass, ...)               \
   do {                                                  \
      if (pass((ibc), ##__VA_ARGS__)) {                  \
         if (unlikely(ibc_should_print_shader(ibc))) {   \
            fprintf(stderr, "IBC after %s\n", #pass);    \
            ibc_print_shader(ibc, stderr);               \
         }                                               \
         ibc_validate_shader(ibc);                       \
         progress = true;                                \
      }                                                  \
   } while(0)

#define IBC_PASS_V(ibc, pass, ...)                    \
   do {                                               \
      pass((ibc), ##__VA_ARGS__);                     \
      if (unlikely(ibc_should_print_shader(ibc))) {   \
         fprintf(stderr, "IBC after %s\n", #pass);    \
         ibc_print_shader(ibc, stderr);               \
      }                                               \
      ibc_validate_shader(ibc);                       \
   } while(0)

bool ibc_assign_and_lower_flags(ibc_shader *shader);
bool ibc_assign_logical_reg_strides(ibc_shader *shader);
void ibc_assign_regs_init(struct brw_compiler *compiler);
bool ibc_assign_regs(ibc_shader *shader,
                     const struct brw_compiler *compiler,
                     bool allow_spilling);

void ibc_lower_and_optimize(ibc_shader *ibc);

bool ibc_lower_fb_writes(ibc_shader *shader);
bool ibc_lower_gather_ops(ibc_shader *shader);
bool ibc_lower_io_to_sends(ibc_shader *shader);
void ibc_lower_io_urb_write_to_send(struct ibc_builder *b,
                                    ibc_send_instr *send,
                                    const ibc_intrinsic_instr *intrin);
void ibc_lower_io_fb_write_to_send(struct ibc_builder *b,
                                   ibc_send_instr *send,
                                   const ibc_intrinsic_instr *intrin);
bool ibc_lower_overlapping_send_payloads(ibc_shader *shader);
bool ibc_lower_phis(ibc_shader *shader);
bool ibc_lower_simd_width(ibc_shader *shader);
unsigned ibc_fb_write_instr_max_simd_width(const ibc_intrinsic_instr *write,
                                           const struct gen_device_info *devinfo);
unsigned ibc_tex_instr_max_simd_width(const ibc_intrinsic_instr *write,
                                      const struct gen_device_info *devinfo);

bool ibc_opt_copy_prop(ibc_shader *shader);
bool ibc_opt_cse(ibc_shader *shader);
bool ibc_opt_dead_code(ibc_shader *shader);

void ibc_print_shader(const ibc_shader *shader, FILE *fp);

void ibc_repair_wlr_order(ibc_shader *shader);

void ibc_schedule_instructions(ibc_shader *shader);
void ibc_schedule_instructions_post_ra(ibc_shader *shader);
bool ibc_split_regs(ibc_shader *shader);

const unsigned *ibc_to_binary(const ibc_shader *shader,
                              const struct shader_info *info,
                              const struct brw_compiler *compiler,
                              void *log_data,
                              void *mem_ctx,
                              unsigned *program_size);

#ifdef NDEBUG
static inline void
ibc_validate_shader(const ibc_shader *shader)
{
}
#else
void ibc_validate_shader(const ibc_shader *shader);
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* IBC_H */
