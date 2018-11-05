SIR Design Principals
=====================

SIR (stands for Scalar Intermediate Representation) is a new back-end IR
for Intel GEN architecture.  This document contains the core design
principals that guide the design of SIR.


## Objectives and priorities

The core objectives of SIR (primarily where it differs from the old
back-end IRs) are as follows (roughly in order of priority):

 1. Forward-looking and designed for next-generation compute workloads:
     1. Gracefully handle multiple bit-sizes
     2. Easily handle exec sizes and groups
     3. Allow for unstructured control-flow
     4. Register allocate and "spill" the flag
     5. Don't make assumptions about binding tables (not sure what this
       means practically)
     6. Get rid of the MRF and similar old-hardware focused hacks
 2. Designed with future GEN hardware in mind
 3. Provide abstractions through the IR itself:
     1. Don't try to represent everything all the time; most instructions
        don't care about full regioning and it just gets in the way
     2. Keep the crazy contained as much as possible
     3. Allow for expressing almost anything in the final lowered form
 4. Sufficiently expressible that the final lowered IR can express almost
    anything the GPU can do
 5. Extremely dumb codegen; basically everything is done via lowering
 6. Easy to write basic optimizations like copy-prop, CSE, and DCE
 7. SSA-ish; i.e., keep as much of the SSA information as possible to help
    with basic optimizations but we likely don't need to be full SSA

The following are are explicit non-goals (roughly in order of how badly we
do NOT want to do them):

 1. Support old hardware
    * Development will start with Skylake
    * Gen8 and earlier support will be added only if it's easy and
      obviously useful.
 2. Code sharing with the old fs and vec4 IRs
    * If we can share the brw_eu.h code, great
    * If we share nothing, that's fine too
 3. Full expressibility of GEN hardware craziness at all times
 4. Complex optimizations
     * SIR will be designed to make copy-prop and various folding
       operations easy but we don't want to re-design nir_opt_algebraic; by
       and large, those types of optimizations should still be done in NIR
     * Optimizations like cmod propagation will still have to happen in SIR
 5. Single Static Assignment form
     * Many people are likely to be shocked to see this on the non-goals
       list.  Even though SSA has proved extremely useful and powerful in
       NIR, it's not clear that it's necessary or practical in a back-end
       IR.  We'll still be SSA-ish and we may only allow virtual regs to
       cross basic blocks and use phi nodes for them but there will
       definitely be multiple and partial writes somewhere.


## Logical Registers

One of the big pain-points of the current IR is in the handling of bit sizes
and register allocation of non-32-bit things.  One of the key design points of
SIR (as it currently stands) is the addition of logical registers to attempt to
address this problem.  A logical register in SIR has a 3-dimensional size in
terms of bit size, number of vector components, and number of SIMD invocations.
Each register is also assigned a SIMD group where the register is only allowed
to be written by instructions executing in that SIMD range.

The inclusion of a SIMD range is a significant divergence from current IRs so
it deserves a bit of additional explanation.  There are three primary reasons
for this:

 1. We need a SIMD width in order to compute the size in bytes or registers of
    the logical register.
 2. By specifying a SIMD range and requiring registers to only be read/written
    by instructions executing in that range, we can assert in the validator
    that logical registers are only read/written on the particular channel
    group and catch bugs that might arise from mixing up our execution groups.
 3. A far more subtle issue is that our current model of register interference
    is not sophisticated enough to do proper range tracking with control flow.
    In particular, we currently have to base register allocation based on
    a physical CFG which represents all paths that all SIMD channels may take;
    for instance, with an if statement, we treat all channels as if they go
    through both the if and the else.  In order to do a better job, we can use
    the logical CFG to determine interference between two registers but this is
    valid to do if and only if both registers have the same bit size and the
    same SIMD range.  If two registers have different SIMD groups, then they
    may stomp each other if we only apply the logical CFG so we have to use the physical CFG.

It's unclear exactly what affect this will have on the complexity of register
allocation and whether or not we can do SSA-based allocation (not sure if we
can do that to begin with).  However, it does appear to have the possibility of
decreasing register pressure.


## Language and Style

### To C++ or not to C++?

The first question is language.  Many mesa compilers (including the GLSL
compiler) are written in C++.  However, the majority of the Intel compiler
people prefer C as a language so we will attempt to write it in C.  This
has been fairly successful with NIR but has some obvious draw-backs.

### Code style

We will keep some things from NIR and chuck others that were probably not a
good idea.  In particular, we'll still typedef structs:

    typedef struct sir_knight {
        /* blah, blah, blah */
    } sir_knight;

Why?  Because it's way easier to type and `sir_knight *knight` has no
ambiguity as to what it is.  It's in a type declaration so it's a type.
We will not, however, typedef enums and their values will go back to
`UPPER_SNAKE_CASE`.  This is standard C and C++ convention and the fact
that we broke it for NIR is a bit weird.
