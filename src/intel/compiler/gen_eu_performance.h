/*
 * Copyright © 2020 Intel Corporation
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

#include "brw_eu.h"

/**
 * Enumeration representing the various asynchronous units that can run
 * computations in parallel on behalf of a shader thread.
 */
enum gen_eu_unit {
   /** EU front-end. */
   GEN_UNIT_FE,
   /** EU FPU0 (Note that co-issue to FPU1 is currently not modeled here). */
   GEN_UNIT_FPU,
   /** Extended Math unit (AKA FPU1 on Gen8-11, part of the EU on Gen6+). */
   GEN_UNIT_EM,
   /** Sampler shared function. */
   GEN_UNIT_SAMPLER,
   /** Pixel Interpolator shared function. */
   GEN_UNIT_PI,
   /** Unified Return Buffer shared function. */
   GEN_UNIT_URB,
   /** Data Port Data Cache shared function. */
   GEN_UNIT_DP_DC,
   /** Data Port Render Cache shared function. */
   GEN_UNIT_DP_RC,
   /** Data Port Constant Cache shared function. */
   GEN_UNIT_DP_CC,
   /** Message Gateway shared function. */
   GEN_UNIT_GATEWAY,
   /** Thread Spawner shared function. */
   GEN_UNIT_SPAWNER,
   /* UNIT_VME, */
   /* UNIT_CRE, */
   /** Number of asynchronous units currently tracked. */
   GEN_NUM_UNITS,
   /** Dummy unit for instructions that don't consume runtime from the above. */
   GEN_UNIT_NULL = GEN_NUM_UNITS
};

/**
 * Enumeration representing a computation result another computation can
 * potentially depend on.
 */
enum gen_eu_dependency_id {
   /* Register part of the GRF. */
   GEN_DEPENDENCY_ID_GRF0 = 0,
   /* Register part of the MRF.  Only used on Gen4-6. */
   GEN_DEPENDENCY_ID_MRF0 = GEN_DEPENDENCY_ID_GRF0 + BRW_MAX_GRF,
   /* Address register part of the ARF. */
   GEN_DEPENDENCY_ID_ADDR0 = GEN_DEPENDENCY_ID_MRF0 + 24,
   /* Accumulator register part of the ARF. */
   GEN_DEPENDENCY_ID_ACCUM0 = GEN_DEPENDENCY_ID_ADDR0 + 1,
   /* Flag register part of the ARF. */
   GEN_DEPENDENCY_ID_FLAG0 = GEN_DEPENDENCY_ID_ACCUM0 + 12,
   /* SBID token write completion.  Only used on Gen12+. */
   GEN_DEPENDENCY_ID_SBID_WR0 = GEN_DEPENDENCY_ID_FLAG0 + 8,
   /* SBID token read completion.  Only used on Gen12+. */
   GEN_DEPENDENCY_ID_SBID_RD0 = GEN_DEPENDENCY_ID_SBID_WR0 + 16,
   /* Number of computation dependencies currently tracked. */
   GEN_NUM_DEPENDENCY_IDS = GEN_DEPENDENCY_ID_SBID_RD0 + 16
};


/**
 * State of our modeling of the program execution.
 */
struct gen_eu_performance_state {
   /**
    * Time at which a given unit will be ready to execute the next
    * computation, in clock units.
    */
   unsigned unit_ready[GEN_NUM_UNITS];

   /**
    * Time at which an instruction dependent on a given dependency ID will
    * be ready to execute, in clock units.
    */
   unsigned dep_ready[GEN_NUM_DEPENDENCY_IDS];

   /**
    * Aggregated utilization of a given unit excluding idle cycles,
    * in clock units.
    */
   float unit_busy[GEN_NUM_UNITS];

   /**
    * Factor of the overhead of a computation accounted for in the
    * aggregated utilization calculation.
    */
   float weight;
};

static inline void
gen_eu_performance_state_init(struct gen_eu_performance_state *state)
{
   memset(state, 0, sizeof(*state));

   state->weight = 1.0f;
}

/**
 * Information derived from an IR instruction used to compute performance
 * estimates.  Allows the timing calculation to work on both FS and VEC4
 * instructions.
 */
struct gen_eu_instruction_info {
   /** Device information. */
   const struct gen_device_info *devinfo;
   /** Instruction opcode. */
   enum opcode op;
   /** Destination type. */
   enum brw_reg_type td;
   /** Destination size in GRF units. */
   unsigned sd;
   /** Execution type. */
   enum brw_reg_type tx;
   /** Execution size in GRF units. */
   unsigned sx;
   /** Source size. */
   unsigned ss;
   /** Bank conflict penalty size in GRF units (equal to sd if non-zero). */
   unsigned sc;
   /** Send message descriptor. */
   uint32_t desc;
   /** Send message shared function ID. */
   uint8_t sfid;
};

/**
 * Timing information of an instruction used to estimate the performance of
 * the program.
 */
struct gen_eu_perf_desc {
   /**
    * Back-end unit its runtime shall be accounted to, in addition to the
    * EU front-end which is always assumed to be involved.
    */
   enum gen_eu_unit u;
   /**
    * Overhead cycles from the time that the EU front-end starts executing
    * the instruction until it's ready to execute the next instruction.
    */
   int df;
   /**
    * Overhead cycles from the time that the back-end starts executing the
    * instruction until it's ready to execute the next instruction.
    */
   int db;
   /**
    * Latency cycles from the time that the back-end starts executing the
    * instruction until its sources have been read from the register file.
    */
   int ls;
   /**
    * Latency cycles from the time that the back-end starts executing the
    * instruction until its regular destination has been written to the
    * register file.
    */
   int ld;
   /**
    * Latency cycles from the time that the back-end starts executing the
    * instruction until its accumulator destination has been written to the
    * ARF file.
    *
    * Note that this is an approximation of the real behavior of
    * accumulating instructions in the hardware: Instead of modeling a pair
    * of back-to-back accumulating instructions as a first computation with
    * latency equal to ld followed by another computation with a
    * mid-pipeline stall (e.g. after the "M" part of a MAC instruction), we
    * model the stall as if it occurred at the top of the pipeline, with
    * the latency of the accumulator computation offset accordingly.
    */
   int la;
   /**
    * Latency cycles from the time that the back-end starts executing the
    * instruction until its flag destination has been written to the ARF
    * file.
    */
   int lf;
};

/**
 * Compute the timing information of an instruction based on any relevant
 * information from the IR and a number of parameters specifying a linear
 * approximation: Parameter X_Y specifies the derivative of timing X
 * relative to info field Y, while X_1 specifies the independent term of
 * the approximation of timing X.
 */
static inline struct gen_eu_perf_desc
calculate_desc(const struct gen_eu_instruction_info *info, enum gen_eu_unit u,
               int df_1, int df_sd, int df_sc,
               int db_1, int db_sx,
               int ls_1, int ld_1, int la_1, int lf_1,
               int l_ss, int l_sd)
{
   struct gen_eu_perf_desc desc = {
      .u = u,
      .df = df_1 + df_sd * (int)info->sd + df_sc * (int)info->sc,
      .db = db_1 + db_sx * (int)info->sx,
      .ls = ls_1 + l_ss * (int)info->ss,
      .ld = ld_1 + l_ss * (int)info->ss + l_sd * (int)info->sd,
      .la = la_1,
      .lf = lf_1,
   };
   return desc;
}

/**
 * Model the performance behavior of a stall on the specified dependency
 * ID.
 */
static inline void
stall_on_dependency(struct gen_eu_performance_state *st,
                    enum gen_eu_dependency_id id)
{
   if (id < ARRAY_SIZE(st->dep_ready)) {
      st->unit_ready[GEN_UNIT_FE] =
         MAX2(st->unit_ready[GEN_UNIT_FE], st->dep_ready[id]);
   }
}

/**
 * Model the performance behavior of the front-end and back-end while
 * executing an instruction with the specified timing information, assuming
 * all dependencies are already clear.
 */
static inline void
execute_instruction(struct gen_eu_performance_state *st,
                    const struct gen_eu_perf_desc *perf)
{
   /* Compute the time at which the front-end will be ready to execute the
    * next instruction.
    */
   st->unit_ready[GEN_UNIT_FE] += perf->df;

   if (perf->u < GEN_NUM_UNITS) {
      /* Wait for the back-end to be ready to execute this instruction. */
      st->unit_ready[GEN_UNIT_FE] = MAX2(st->unit_ready[GEN_UNIT_FE],
                                         st->unit_ready[perf->u]);

      /* Compute the time at which the back-end will be ready to execute
       * the next instruction, and update the back-end utilization.
       */
      st->unit_ready[perf->u] = st->unit_ready[GEN_UNIT_FE] + perf->db;
      st->unit_busy[perf->u] += perf->db * st->weight;
   }
}

/**
 * Model the performance behavior of a read dependency provided by an
 * instruction.
 */
static void
mark_read_dependency(struct gen_eu_performance_state *st,
                     const struct gen_eu_perf_desc *perf,
                     enum gen_eu_dependency_id id)
{
   if (id < ARRAY_SIZE(st->dep_ready))
      st->dep_ready[id] = st->unit_ready[GEN_UNIT_FE] + perf->ls;
}

/**
 * Model the performance behavior of a write dependency provided by an
 * instruction.
 */
static void
mark_write_dependency(struct gen_eu_performance_state *st,
                      const struct gen_eu_perf_desc *perf,
                      enum gen_eu_dependency_id id)
{
   if (id >= GEN_DEPENDENCY_ID_ACCUM0 && id < GEN_DEPENDENCY_ID_FLAG0)
      st->dep_ready[id] = st->unit_ready[GEN_UNIT_FE] + perf->la;
   else if (id >= GEN_DEPENDENCY_ID_FLAG0 && id < GEN_DEPENDENCY_ID_SBID_WR0)
      st->dep_ready[id] = st->unit_ready[GEN_UNIT_FE] + perf->lf;
   else if (id < ARRAY_SIZE(st->dep_ready))
      st->dep_ready[id] = st->unit_ready[GEN_UNIT_FE] + perf->ld;
}

/**
 * Return the dependency ID of flag register starting at offset \p i.
 */
static enum gen_eu_dependency_id
flag_dependency_id(unsigned i)
{
   assert(i < GEN_DEPENDENCY_ID_SBID_WR0 - GEN_DEPENDENCY_ID_FLAG0);
   return (enum gen_eu_dependency_id) (GEN_DEPENDENCY_ID_FLAG0 + i);
}

/**
 * Return the dependency ID corresponding to the SBID read completion
 * condition of a Gen12+ SWSB.
 */
static enum gen_eu_dependency_id
tgl_swsb_rd_dependency_id(struct tgl_swsb swsb)
{
   if (swsb.mode) {
      assert(swsb.sbid < GEN_NUM_DEPENDENCY_IDS - GEN_DEPENDENCY_ID_SBID_RD0);
      return (enum gen_eu_dependency_id)
             (GEN_DEPENDENCY_ID_SBID_RD0 + swsb.sbid);
   } else {
      return GEN_NUM_DEPENDENCY_IDS;
   }
}

/**
 * Return the dependency ID corresponding to the SBID write completion
 * condition of a Gen12+ SWSB.
 */
static enum gen_eu_dependency_id
tgl_swsb_wr_dependency_id(struct tgl_swsb swsb)
{
   if (swsb.mode) {
      assert(swsb.sbid <
             GEN_DEPENDENCY_ID_SBID_RD0 - GEN_DEPENDENCY_ID_SBID_WR0);
      return (enum gen_eu_dependency_id)
             (GEN_DEPENDENCY_ID_SBID_WR0 + swsb.sbid);
   } else {
      return GEN_NUM_DEPENDENCY_IDS;
   }
}

/**
 * Calculate the maximum possible throughput of the program compatible with
 * the cycle-count utilization estimated for each asynchronous unit, in
 * threads-per-cycle units.
 */
static float
calculate_thread_throughput(const struct gen_eu_performance_state *st,
                            float busy)
{
   for (unsigned i = 0; i < GEN_NUM_UNITS; i++)
      busy = MAX2(busy, st->unit_busy[i]);

   return 1.0 / busy;
}
