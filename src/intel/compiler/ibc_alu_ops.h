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

/* Define the following macro and include this file to get ALU op infos:
 *
 *    IBC_ALU_OP_DECL(opcode, num_srcs, src_mods)
 */

IBC_ALU_OP_DECL(NONE, 0, NONE)

IBC_ALU_OP_DECL(MOV, 1, NEG_ABS)
IBC_ALU_OP_DECL(SEL, 2, NEG_ABS)
IBC_ALU_OP_DECL(NOT, 1, NONE   )
IBC_ALU_OP_DECL(AND, 2, NOT    )
IBC_ALU_OP_DECL(OR,  2, NOT    )
IBC_ALU_OP_DECL(XOR, 2, NOT    )
IBC_ALU_OP_DECL(SHR, 2, NEG_ABS)
IBC_ALU_OP_DECL(SHL, 2, NEG_ABS)
IBC_ALU_OP_DECL(CMP, 2, NEG_ABS)
IBC_ALU_OP_DECL(BFE, 3, NONE   )
IBC_ALU_OP_DECL(BFI1,2, NONE   )
IBC_ALU_OP_DECL(BFI2,3, NONE   )
IBC_ALU_OP_DECL(ADD, 2, NEG_ABS)
IBC_ALU_OP_DECL(MUL, 2, NONE   )
IBC_ALU_OP_DECL(FRC, 1, NEG_ABS)
IBC_ALU_OP_DECL(RNDU,1, NEG_ABS)
IBC_ALU_OP_DECL(RNDD,1, NEG_ABS)
IBC_ALU_OP_DECL(RNDE,1, NEG_ABS)
IBC_ALU_OP_DECL(RNDZ,1, NEG_ABS)
IBC_ALU_OP_DECL(MAD, 3, NEG_ABS)
IBC_ALU_OP_DECL(LRP, 3, NEG_ABS)

/* Math function opcodes */
IBC_ALU_OP_DECL(RCP,  1, NEG_ABS)
IBC_ALU_OP_DECL(LOG2, 1, NEG_ABS)
IBC_ALU_OP_DECL(EXP2, 1, NEG_ABS)
IBC_ALU_OP_DECL(SQRT, 1, NEG_ABS)
IBC_ALU_OP_DECL(RSQ,  1, NEG_ABS)
IBC_ALU_OP_DECL(SIN,  1, NEG_ABS)
IBC_ALU_OP_DECL(COS,  1, NEG_ABS)
IBC_ALU_OP_DECL(POW,  2, NEG_ABS)
IBC_ALU_OP_DECL(IDIV, 2, NEG_ABS)
IBC_ALU_OP_DECL(IREM, 2, NEG_ABS)
