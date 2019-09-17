/*
 * Copyright © 2010 Intel Corporation
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
 *
 * Authors:
 *    Eric Anholt <eric@anholt.net>
 *
 */

#ifndef REGISTER_ALLOCATE_PRIV_H
#define REGISTER_ALLOCATE_PRIV_H

#include "register_allocate.h"
#include "bitset.h"

#ifdef __cplusplus
extern "C" {
#endif


struct ra_reg {
   BITSET_WORD *conflicts;
   unsigned int *conflict_list;
   unsigned int conflict_list_size;
   unsigned int num_conflicts;
};

struct ra_regs {
   struct ra_reg *regs;
   unsigned int count;

   struct ra_class **classes;
   unsigned int class_count;

   bool round_robin;
};

struct ra_class {
   /**
    * Bitset indicating which registers belong to this class.
    *
    * (If bit N is set, then register N belongs to this class.)
    */
   BITSET_WORD *regs;

   /**
    * p(B) in Runeson/Nyström paper.
    *
    * This is "how many regs are in the set."
    */
   unsigned int p;

   /**
    * q(B,C) (indexed by C, B is this register class) in
    * Runeson/Nyström paper.  This is "how many registers of B could
    * the worst choice register from C conflict with".
    */
   unsigned int *q;
};


#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* REGISTER_ALLOCATE_PRIV_H */
