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
#include "ibc_live_intervals.h"

#include "util/rb_tree.h"

#include <stdio.h>

static unsigned
ibc_logical_reg_byte_size(const ibc_reg *reg)
{
   assert(reg->file == IBC_FILE_LOGICAL);
   if (reg->logical.bit_size == 1) {
      /* Assume W types for booleans */
      return 2;
   } else {
      assert(reg->logical.bit_size % 8 == 0);
      return reg->logical.bit_size / 8;
   }
}

static unsigned
ibc_alu_instr_is_compressed(const ibc_alu_instr *alu)
{
   switch (alu->dest.file) {
   case IBC_FILE_NONE: {
      /* TODO: Is this correct? */
      unsigned stride = alu->dest.type == IBC_TYPE_FLAG ?
                        2 : ibc_type_byte_size(alu->dest.type);
      unsigned bytes_written = alu->instr.simd_width * stride;
      return bytes_written > REG_SIZE;
   }

   case IBC_FILE_IMM:
      unreachable("Destinations cannot be IBC_FILE_IMM");

   case IBC_FILE_LOGICAL: {
      unsigned stride = alu->dest.reg->logical.stride > 0 ?
                        alu->dest.reg->logical.stride :
                        ibc_logical_reg_byte_size(alu->dest.reg);
      unsigned bytes_written = alu->instr.simd_width * stride;
      return bytes_written > REG_SIZE;
   }

   case IBC_FILE_HW_GRF: {
      unsigned dest_byte = alu->dest.hw_grf.byte;
      unsigned bytes_written =
         alu->dest.hw_grf.hstride * (alu->instr.simd_width %
                                     alu->dest.hw_grf.width) +
         alu->dest.hw_grf.vstride * (alu->instr.simd_width /
                                     alu->dest.hw_grf.width);
      return (dest_byte % REG_SIZE) + bytes_written > REG_SIZE;
   }

   case IBC_FILE_FLAG:
      return false;

   case IBC_FILE_ACCUM: {
      /* TODO: Is this correct? */
      unsigned bytes_written = ibc_type_byte_size(alu->dest.type) *
                               alu->instr.simd_width;
      return bytes_written > REG_SIZE;
   }
   }

   unreachable("Invalid register file");
}

static unsigned
ibc_alu_instr_num_chunks(const ibc_alu_instr *alu)
{
   return 1 + ibc_alu_instr_is_compressed(alu);
}

static unsigned
ibc_alu_latency(enum ibc_type exec_type,
                const struct gen_device_info *devinfo)
{
   assert(devinfo->gen >= 8);
   return exec_type == IBC_TYPE_DF ? 12 : 8;
}

static unsigned
ibc_instr_fe_cycles(const ibc_instr *instr,
                    const struct gen_device_info *devinfo)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      return 2 * ibc_alu_instr_num_chunks(ibc_instr_as_alu(instr));

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      unsigned fe_cycles = 2; /* TODO */

      /* An indirect descriptor means an OR to fill out addr0.0-1 */
      if (send->desc.file != IBC_FILE_NONE &&
          send->desc.file != IBC_FILE_IMM)
         fe_cycles += 2 + ibc_alu_latency(IBC_TYPE_UD, devinfo);

      /* An indirect extended descriptor means an OR to fill out addr0.2-3 */
      if (send->ex_desc.file != IBC_FILE_NONE &&
          send->ex_desc.file != IBC_FILE_IMM)
         fe_cycles += 2 + ibc_alu_latency(IBC_TYPE_UD, devinfo);

      return fe_cycles;
   }

   case IBC_INSTR_TYPE_INTRINSIC:
      switch (ibc_instr_as_intrinsic(instr)->op) {
      case IBC_INTRINSIC_OP_UNDEF:
      case IBC_INTRINSIC_OP_LOAD_PAYLOAD:
      case IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO:
         return 0;

      case IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL:
         return 2;

      case IBC_INTRINSIC_OP_FLOAT_CONTROL_MODE:
         return 4;

      case IBC_INTRINSIC_OP_SIMD_SHUFFLE:
      case IBC_INTRINSIC_OP_MOV_INDIRECT:
         /* This does an ALU op to fill out the indirect register file plus an
          * indirect fetch.  This is 100% an approximation.
          */
         return 2 + ibc_alu_latency(IBC_TYPE_UD, devinfo) +
                2 * (1 + instr->simd_width > 8) + 2;

      case IBC_INTRINSIC_OP_PLN:
      case IBC_INTRINSIC_OP_ALIGN16_DDX_FINE:
         return 2 * (1 + instr->simd_width > 8);

      case IBC_INTRINSIC_OP_WAIT:
         return 2; /* TODO */

      case IBC_INTRINSIC_OP_STALL_REG:
         return 2;

      case IBC_INTRINSIC_OP_SIMD_ZIP:
         /* TODO: Should these really get here? */
         return 2;

      default:
         unreachable("Invalid intrinsic this late in compilation");
      }
      break;

   case IBC_INSTR_TYPE_FLOW:
      switch (ibc_instr_as_flow(instr)->op) {
      case IBC_FLOW_OP_START:
      case IBC_FLOW_OP_END:
         return 0;
      default:
         return 2; /* TODO */
      }
   }

   unreachable("Invalid instruction type");
}

static unsigned
ibc_instr_dest_latency(const ibc_instr *instr,
                       const struct gen_device_info *devinfo)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU: {
      ibc_alu_instr *alu = ibc_instr_as_alu(instr);
      return ibc_alu_latency(ibc_alu_instr_exec_type(alu), devinfo);
   }

   case IBC_INSTR_TYPE_SEND: {
      ibc_send_instr *send = ibc_instr_as_send(instr);
      switch (send->sfid) {
      case BRW_SFID_SAMPLER:
         switch (brw_sampler_desc_msg_type(devinfo, send->desc_imm)) {
         case GEN5_SAMPLER_MESSAGE_SAMPLE_RESINFO:
         case GEN6_SAMPLER_MESSAGE_SAMPLE_SAMPLEINFO:
            /* Testing textureSize(sampler2D, 0), one load was 420 +/- 41
             * cycles (n=15):
             * mov(8)   g114<1>UD  0D                  { align1 WE_normal 1Q };
             * send(8)  g6<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 10, 1) mlen 1 rlen 4  { align1 WE_normal 1Q };
             * mov(16)  g6<1>F     g6<8,8,1>D          { align1 WE_normal 1Q };
             *
             *
             * Two loads was 535 +/- 30 cycles (n=19):
             * mov(16)   g114<1>UD  0D                 { align1 WE_normal 1H };
             * send(16)  g6<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 10, 2) mlen 2 rlen 8  { align1 WE_normal 1H };
             * mov(16)   g114<1>UD  0D                 { align1 WE_normal 1H };
             * mov(16)   g6<1>F     g6<8,8,1>D         { align1 WE_normal 1H };
             * send(16)  g8<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 10, 2) mlen 2 rlen 8  { align1 WE_normal 1H };
             * mov(16)   g8<1>F     g8<8,8,1>D         { align1 WE_normal 1H };
             * add(16)   g6<1>F     g6<8,8,1>F   g8<8,8,1>F  { align1 WE_normal 1H };
             *
             * Since the only caches that should matter are just the
             * instruction/state cache containing the surface state, assume
             * that we always have hot caches.
             */
            return 100;

         default:
            /* 18 cycles:
             * mov(8)  g115<1>F   0F                   { align1 WE_normal 1Q };
             * mov(8)  g114<1>F   0F                   { align1 WE_normal 1Q };
             * send(8) g4<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 0, 1) mlen 2 rlen 4   { align1 WE_normal 1Q };
             *
             * 697 +/-49 cycles (min 610, n=26):
             * mov(8)  g115<1>F   0F                   { align1 WE_normal 1Q };
             * mov(8)  g114<1>F   0F                   { align1 WE_normal 1Q };
             * send(8) g4<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 0, 1) mlen 2 rlen 4   { align1 WE_normal 1Q };
             * mov(8)  null       g4<8,8,1>F           { align1 WE_normal 1Q };
             *
             * So the latency on our first texture load of the batchbuffer
             * takes ~700 cycles, since the caches are cold at that point.
             *
             * 840 +/- 92 cycles (min 720, n=25):
             * mov(8)  g115<1>F   0F                   { align1 WE_normal 1Q };
             * mov(8)  g114<1>F   0F                   { align1 WE_normal 1Q };
             * send(8) g4<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 0, 1) mlen 2 rlen 4   { align1 WE_normal 1Q };
             * mov(8)  null       g4<8,8,1>F           { align1 WE_normal 1Q };
             * send(8) g4<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 0, 1) mlen 2 rlen 4   { align1 WE_normal 1Q };
             * mov(8)  null       g4<8,8,1>F           { align1 WE_normal 1Q };
             *
             * On the second load, it takes just an extra ~140 cycles, and
             * after accounting for the 14 cycles of the MOV's latency, that
             * makes ~130.
             *
             * 683 +/- 49 cycles (min = 602, n=47):
             * mov(8)  g115<1>F   0F                   { align1 WE_normal 1Q };
             * mov(8)  g114<1>F   0F                   { align1 WE_normal 1Q };
             * send(8) g4<1>UW    g114<8,8,1>F
             *   sampler (10, 0, 0, 1) mlen 2 rlen 4   { align1 WE_normal 1Q };
             * send(8) g50<1>UW   g114<8,8,1>F
             *   sampler (10, 0, 0, 1) mlen 2 rlen 4   { align1 WE_normal 1Q };
             * mov(8)  null       g4<8,8,1>F           { align1 WE_normal 1Q };
             *
             * The unit appears to be pipelined, since this matches up with
             * the cache-cold case, despite there being two loads here.  If
             * you replace the g4 in the MOV to null with g50, it's still 693
             * +/- 52 (n=39).
             *
             * So, take some number between the cache-hot 140 cycles and the
             * cache-cold 700 cycles.  No particular tuning was done on this.
             *
             * I haven't done significant testing of the non-TEX opcodes.  TXL
             * at least looked about the same as TEX.
             */
            return 200;
         }
         break;

      case BRW_SFID_MESSAGE_GATEWAY:
         assert(send->desc_imm == BRW_MESSAGE_GATEWAY_SFID_BARRIER_MSG);
         return 200; /* TODO */

      case GEN7_SFID_PIXEL_INTERPOLATOR:
         return 90; /* TODO */

      case BRW_SFID_URB:
         switch (brw_urb_desc_msg_type(devinfo, send->desc_imm)) {
         case GEN8_URB_OPCODE_SIMD8_READ:
            return 200; /* TODO */

         case GEN8_URB_OPCODE_SIMD8_WRITE:
            /* Assume writes are pretty cheap */
            return 2 * (send->mlen + send->ex_mlen);

         default:
            unreachable("Unknown URB message type");
         }
         break;

      case BRW_SFID_THREAD_SPAWNER:
         assert(send->eot);
         return 2;

      case GEN6_SFID_DATAPORT_RENDER_CACHE:
         /* Assume RT writes are pretty cheap */
         return 2 * (send->mlen + send->ex_mlen);

      case GEN6_SFID_DATAPORT_CONSTANT_CACHE:
         return 200; /* TODO */

      case GEN7_SFID_DATAPORT_DATA_CACHE:
         switch (brw_dp_desc_msg_type(devinfo, send->desc_imm)) {
         case HSW_DATAPORT_DC_PORT0_BYTE_SCATTERED_READ:
         case HSW_DATAPORT_DC_PORT0_BYTE_SCATTERED_WRITE:
            /* We have no data for this but assume it's roughly the same as
             * untyped surface read/write.
             */
            return 600;

         case GEN7_DATAPORT_DC_OWORD_BLOCK_READ:
         case GEN7_DATAPORT_DC_OWORD_BLOCK_WRITE:
         case GEN7_DATAPORT_DC_MEMORY_FENCE:
            return 100; /* TODO */

         default:
            unreachable("Unknown data cache message");
         }

      case HSW_SFID_DATAPORT_DATA_CACHE_1:
         switch (brw_dp_desc_msg_type(devinfo, send->desc_imm)) {
         case HSW_DATAPORT_DC_PORT1_UNTYPED_SURFACE_READ:
         case HSW_DATAPORT_DC_PORT1_UNTYPED_SURFACE_WRITE:
         case HSW_DATAPORT_DC_PORT1_TYPED_SURFACE_READ:
         case HSW_DATAPORT_DC_PORT1_TYPED_SURFACE_WRITE:
         case GEN8_DATAPORT_DC_PORT1_A64_UNTYPED_SURFACE_WRITE:
         case GEN8_DATAPORT_DC_PORT1_A64_UNTYPED_SURFACE_READ:
         case GEN8_DATAPORT_DC_PORT1_A64_SCATTERED_WRITE:
         case GEN9_DATAPORT_DC_PORT1_A64_SCATTERED_READ:
            /* Test code:
             *   mov(8)    g112<1>UD       0x00000000UD    { align1 WE_all 1Q };
             *   mov(1)    g112.7<1>UD     g1.7<0,1,0>UD   { align1 WE_all };
             *   mov(8)    g113<1>UD       0x00000000UD    { align1 WE_normal 1Q };
             *   send(8)   g4<1>UD         g112<8,8,1>UD
             *             data (38, 6, 5) mlen 2 rlen 1   { align1 WE_normal 1Q };
             *   .
             *   . [repeats 8 times]
             *   .
             *   mov(8)    g112<1>UD       0x00000000UD    { align1 WE_all 1Q };
             *   mov(1)    g112.7<1>UD     g1.7<0,1,0>UD   { align1 WE_all };
             *   mov(8)    g113<1>UD       0x00000000UD    { align1 WE_normal 1Q };
             *   send(8)   g4<1>UD         g112<8,8,1>UD
             *             data (38, 6, 5) mlen 2 rlen 1   { align1 WE_normal 1Q };
             *
             * Running it 100 times as fragment shader on a 128x128 quad
             * gives an average latency of 583 cycles per surface read,
             * standard deviation 0.9%.
             */
            return 600;

         case HSW_DATAPORT_DC_PORT1_UNTYPED_ATOMIC_OP:
         case HSW_DATAPORT_DC_PORT1_UNTYPED_ATOMIC_OP_SIMD4X2:
         case HSW_DATAPORT_DC_PORT1_TYPED_ATOMIC_OP_SIMD4X2:
         case HSW_DATAPORT_DC_PORT1_TYPED_ATOMIC_OP:
         case GEN9_DATAPORT_DC_PORT1_UNTYPED_ATOMIC_FLOAT_OP:
         case GEN8_DATAPORT_DC_PORT1_A64_UNTYPED_ATOMIC_OP:
         case GEN9_DATAPORT_DC_PORT1_A64_UNTYPED_ATOMIC_FLOAT_OP:
            /* Test code:
             *   mov(8)    g112<1>ud       0x00000000ud    { align1 WE_all 1Q };
             *   mov(1)    g112.7<1>ud     g1.7<0,1,0>ud   { align1 WE_all };
             *   mov(8)    g113<1>ud       0x00000000ud    { align1 WE_normal 1Q };
             *   send(8)   g4<1>ud         g112<8,8,1>ud
             *             data (38, 5, 6) mlen 2 rlen 1   { align1 WE_normal 1Q };
             *
             * Running it 100 times as fragment shader on a 128x128 quad
             * gives an average latency of 13867 cycles per atomic op,
             * standard deviation 3%.  Note that this is a rather
             * pessimistic estimate, the actual latency in cases with few
             * collisions between threads and favorable pipelining has been
             * seen to be reduced by a factor of 100.
             */
            return 14000;

         default:
            unreachable("Unknown data cache message");
         }
         break;

      default:
         unreachable("Unknown SFID");
      }
   }

   case IBC_INSTR_TYPE_INTRINSIC:
      switch (ibc_instr_as_intrinsic(instr)->op) {
      case IBC_INTRINSIC_OP_UNDEF:
      case IBC_INTRINSIC_OP_LOAD_PAYLOAD:
      case IBC_INTRINSIC_OP_BTI_BLOCK_LOAD_UBO:
         return 0;

      case IBC_INTRINSIC_OP_FIND_LIVE_CHANNEL:
      case IBC_INTRINSIC_OP_SIMD_SHUFFLE:
      case IBC_INTRINSIC_OP_MOV_INDIRECT:
      case IBC_INTRINSIC_OP_PLN:
      case IBC_INTRINSIC_OP_ALIGN16_DDX_FINE:
      case IBC_INTRINSIC_OP_STALL_REG:
      case IBC_INTRINSIC_OP_FLOAT_CONTROL_MODE:
         return ibc_alu_latency(IBC_TYPE_F, devinfo);

      case IBC_INTRINSIC_OP_WAIT:
         return 0; /* TODO */

      case IBC_INTRINSIC_OP_SIMD_ZIP:
         /* TODO: Should these really get here? */
         return 0;

      default:
         unreachable("Invalid intrinsic this late in compilation");
      }
      break;

   case IBC_INSTR_TYPE_FLOW:
      return 0; /* TODO */
   }

   unreachable("Invalid instruction type");
}

static bool
ibc_instr_can_reorder(const ibc_instr *instr)
{
   switch (instr->type) {
   case IBC_INSTR_TYPE_ALU:
      return true;
   case IBC_INSTR_TYPE_SEND:
      return ibc_instr_as_send(instr)->can_reorder;
   case IBC_INSTR_TYPE_INTRINSIC:
      return ibc_instr_as_intrinsic(instr)->can_reorder;
   case IBC_INSTR_TYPE_FLOW:
      return false;
   }
   unreachable("Invalid IBC instruction type");
}

static bool
ibc_instr_is_full_barrier(const ibc_instr *instr)
{
   /* Consider LOAD_PAYLOAD to be a full barrier so nothing gets scheduled
    * before any of them and they remain at the top of the program.
    */
   return instr->type == IBC_INSTR_TYPE_FLOW ||
          ibc_instr_is_load_payload(instr);
}

enum ibc_sched_direction {
   IBC_SCHED_DIRECTION_TOP_DOWN,
   IBC_SCHED_DIRECTION_BOTTOM_UP,
};

typedef struct ibc_sched_dep        ibc_sched_dep;
typedef struct ibc_sched_node       ibc_sched_node;
typedef struct ibc_sched_reg_chunk  ibc_sched_reg_chunk;
typedef struct ibc_sched_graph      ibc_sched_graph;
typedef struct ibc_schedule         ibc_schedule;

struct ibc_sched_dep {
   /** The node on which this node depends */
   ibc_sched_node *node;

   /** Bytes of data transfered along the dependency
    *
    * For non-data dependencies, this is zero.
    */
   uint16_t num_bytes;

   /** Latency (in cycles) between the start of the two nodes */
   uint16_t latency;
};

struct ibc_sched_node {
   /** Instruction represented by this node */
   const ibc_instr *instr;

   /** The start of the block in which this node lives */
   const ibc_flow_instr *block_start;

   /** Array of source dependencies */
   ibc_sched_dep *src_deps;
   uint32_t num_src_deps;
   uint32_t src_deps_array_len;

   /** Array of destination dependencies */
   ibc_sched_dep *dest_deps;
   uint32_t num_dest_deps;
   uint32_t dest_deps_array_len;

   /** Mutable count of yet-to-be-scheduled nodes with this as a dependency */
   uint32_t ref_count;

   struct {
      /** Time spent in the front-end */
      uint16_t fe_time;

      /** Latency from the end of the FE time to the destination */
      uint16_t fe_to_dest;

      /** Minimum latency between the start of the program and this node */
      uint32_t min_to_start;

      /** Minimum latency between the end of the program and this node */
      uint32_t min_to_end;
   } latency;

   struct {
      /** Estimated number of GRF bytes written by this node */
      uint16_t grf_bytes;

      /** Estimated number of FLAG bits potentially written by this node */
      uint16_t flag_bits;

      /** True if this node writes an accumulator */
      bool accum;
   } writes;

   struct {
      /** Cycle at which this node was scheduled */
      uint32_t cycle;

      /** Count of yet-to-be-scheduled nodes with this as a data dependency */
      uint32_t data_ref_count;
   } data;

   /** Link for storing an ibc_sched_node in a list */
   struct list_head link;

   /** Node for storing an ibc_sched_node in a red-black tree */
   struct rb_node rb_node;
};

struct ibc_sched_reg_chunk {
   /** Number of FLAG bits in this chunk */
   uint16_t flag_bits;

   /** Number of GRF bytes in this chunk */
   uint16_t grf_bytes;

   /** Number of nodes which read or write this chunk */
   uint32_t num_refs;

   /** Mutable per-block reference count for this chunk */
   uint32_t ref_count;
};

struct ibc_sched_graph {
   /** Live sets (not full intervals) if pre-RA, NULL if post-RA */
   ibc_live_intervals *live;

   uint32_t num_nodes;
   ibc_sched_node *nodes;

   ibc_sched_reg_chunk *reg_chunks;
};

struct ibc_schedule {
   uint32_t cycles;
   uint32_t max_grf_bytes;
   uint32_t max_flag_bits;

   uint32_t order[0];
};


#define TOTAL_GRF_BYTES (128 * 32)
#define TOTAL_FLAG_BITS 64

/* We track at the nibble granularity */
#define FLAG_CHUNK_BITS 4
#define TOTAL_FLAG_CHUNKS (TOTAL_FLAG_BITS / FLAG_CHUNK_BITS)

struct ibc_sched_graph_builder {
   ibc_sched_graph *graph;

   /* Current node when iterating */
   ibc_sched_node *iter_node;

   /* Context for chunk_last_write and grf_last_write arrays */
   void *mem_ctx;

   bool top_down;

   ibc_sched_node **chunk_last_write;
   ibc_sched_node **grf_last_write;
   ibc_sched_node *flag_last_write[TOTAL_FLAG_CHUNKS];
   ibc_sched_node *accum_last_write;
};

static void
ibc_sched_node_dep_arr_add(uint32_t *num_deps, uint32_t *deps_array_len,
                           ibc_sched_dep **deps, ibc_sched_node *dep_node,
                           uint32_t num_bytes, uint32_t latency,
                           ibc_sched_graph *graph)
{
   /* Sometime dependencies get added multiple times.  We don't actually need
    * multiple copies and can condense down to one.
    */
   for (int i = *num_deps - 1; i >= 0; i--) {
      if ((*deps)[i].node == dep_node) {
         (*deps)[i].num_bytes += num_bytes;
         if ((*deps)[i].latency < latency)
            (*deps)[i].latency = latency;
         return;
      }
   }

   if (*num_deps >= *deps_array_len) {
      *deps_array_len = MAX2(8, *deps_array_len * 2);
      *deps = reralloc(graph, *deps, ibc_sched_dep, *deps_array_len);
   }

   (*deps)[(*num_deps)++] = (ibc_sched_dep) {
      .node = dep_node,
      .num_bytes = num_bytes,
      .latency = latency,
   };
}

static void
ibc_sched_graph_add_dep(ibc_sched_graph *graph,
                        ibc_sched_node *use, ibc_sched_node *def,
                        uint16_t num_bytes, uint32_t latency)
{
   assert(use > def);
   ibc_sched_node_dep_arr_add(&use->num_src_deps, &use->src_deps_array_len,
                              &use->src_deps, def, num_bytes, latency, graph);
   ibc_sched_node_dep_arr_add(&def->num_dest_deps, &def->dest_deps_array_len,
                              &def->dest_deps, use, num_bytes, latency, graph);
}

struct set_hw_grf_dep_state {
   struct ibc_sched_graph_builder *b;
   ibc_sched_node *node;
};

static void
set_hw_grf_dep(unsigned byte, void *_state)
{
   struct set_hw_grf_dep_state *state = _state;
   state->b->grf_last_write[byte] = state->node;
}

static void
set_dep_node(ibc_ref *ref, ibc_sched_node *node,
             int num_bytes, int num_comps,
             uint8_t simd_group, uint8_t simd_width,
             struct ibc_sched_graph_builder *b)
{
   assert(ref->file != IBC_FILE_NONE && ref->file != IBC_FILE_IMM);
   ibc_sched_graph *g = b->graph;

   if (ref->reg != NULL) {
      if (b->chunk_last_write == NULL) {
         b->chunk_last_write = rzalloc_array(b->mem_ctx, ibc_sched_node *,
                                             g->live->num_chunks);
      }

      assert(ref->reg->index < b->graph->live->num_regs);
      const unsigned num_chunks = g->live->regs[ref->reg->index].num_chunks;
      const unsigned chunk_idx = g->live->regs[ref->reg->index].chunk_idx;

      BITSET_DECLARE(chunks, IBC_REG_LIVE_MAX_CHUNKS);
      memset(chunks, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
      ibc_live_intervals_ref_chunks(g->live, ref, num_bytes, num_comps,
                                    simd_group, simd_width, chunks);

      for (unsigned i = 0; i < num_chunks; i++) {
         if (BITSET_TEST(chunks, i))
            b->chunk_last_write[chunk_idx + i] = node;
      }
   } else if (ref->file == IBC_FILE_HW_GRF) {
      if (b->grf_last_write == NULL) {
         b->grf_last_write = rzalloc_array(b->mem_ctx, ibc_sched_node *,
                                           TOTAL_GRF_BYTES);
      }

      if (num_bytes > 0) {
         /* This is a send */
         for (unsigned i = 0; i < num_bytes; i++) {
            assert(ref->hw_grf.byte + i < 4096);
            b->grf_last_write[ref->hw_grf.byte + i] = node;
         }
      } else {
         ibc_hw_grf_ref_foreach_byte(*ref, num_comps, simd_width,
                                     set_hw_grf_dep,
                                     &(struct set_hw_grf_dep_state) {
                                        .b = b,
                                        .node = node,
                                     });
      }
   } else if (ref->file == IBC_FILE_FLAG) {
      if (ref->type != IBC_TYPE_FLAG) {
         assert(ref->flag.bit % ibc_type_bit_size(ref->type) == 0);
         simd_width = ibc_type_bit_size(ref->type);
      }

      const unsigned num_chunks = DIV_ROUND_UP(simd_width, FLAG_CHUNK_BITS);
      assert(ref->flag.bit % FLAG_CHUNK_BITS == 0);
      const unsigned chunk_idx = ref->flag.bit / FLAG_CHUNK_BITS;

      for (unsigned i = 0; i < num_chunks; i++)
         b->flag_last_write[chunk_idx + i] = node;
   } else if (ref->file == IBC_FILE_ACCUM) {
      b->accum_last_write = node;
   } else {
      unreachable("Invalid register file for NULL reg");
   }
}

struct foreach_hw_grf_dep_state {
   const ibc_ref *ref;
   ibc_sched_node *last;
   void (*cb)(const ibc_ref *, uint16_t num_bytes,
              ibc_sched_node *,
              struct ibc_sched_graph_builder *);
   struct ibc_sched_graph_builder *b;
};

static void
foreach_hw_grf_dep(unsigned byte, void *_state)
{
   struct foreach_hw_grf_dep_state *state = _state;
   if (state->b->grf_last_write[byte] == NULL ||
       state->b->grf_last_write[byte] == state->last)
      return;

   state->last = state->b->grf_last_write[byte];
   state->cb(state->ref, 1, state->last, state->b);
}

static void
foreach_dep_node(ibc_ref *ref,
                 int num_bytes, int num_comps,
                 uint8_t simd_group, uint8_t simd_width,
                 void (*cb)(const ibc_ref *, uint16_t num_bytes,
                            ibc_sched_node *,
                            struct ibc_sched_graph_builder *),
                 struct ibc_sched_graph_builder *b)
{
   assert(ref->file != IBC_FILE_NONE && ref->file != IBC_FILE_IMM);
   ibc_sched_graph *g = b->graph;

   if (ref->reg != NULL) {
      if (b->chunk_last_write == NULL)
         return;

      assert(ref->reg->index < b->graph->live->num_regs);
      const unsigned num_chunks = g->live->regs[ref->reg->index].num_chunks;
      const unsigned chunk_idx = g->live->regs[ref->reg->index].chunk_idx;

      BITSET_DECLARE(chunks, IBC_REG_LIVE_MAX_CHUNKS);
      memset(chunks, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
      ibc_live_intervals_ref_chunks(g->live, ref, num_bytes, num_comps,
                                    simd_group, simd_width, chunks);

      ibc_sched_node *last = NULL;
      for (unsigned i = 0; i < num_chunks; i++) {
         if (!BITSET_TEST(chunks, i))
            continue;

         if (b->chunk_last_write[chunk_idx + i] == NULL ||
             b->chunk_last_write[chunk_idx + i] == last)
            continue;

         last = b->chunk_last_write[chunk_idx + i];
         cb(ref, g->reg_chunks[chunk_idx + i].grf_bytes, last, b);
      }
   } else if (ref->file == IBC_FILE_HW_GRF) {
      if (b->grf_last_write == NULL)
         return;

      if (num_bytes > 0) {
         /* This is a send */
         ibc_sched_node *last = NULL;
         for (unsigned i = 0; i < num_bytes; i++) {
            assert(ref->hw_grf.byte + i < 4096);
            if (b->grf_last_write[ref->hw_grf.byte + i] == NULL ||
                b->grf_last_write[ref->hw_grf.byte + i] == last)
               continue;

            last = b->grf_last_write[ref->hw_grf.byte + i];
            cb(ref, 1, last, b);
         }
      } else {
         ibc_hw_grf_ref_foreach_byte(*ref, num_comps, simd_width,
                                     foreach_hw_grf_dep,
                                     &(struct foreach_hw_grf_dep_state) {
                                        .ref = ref,
                                        .cb = cb,
                                        .b = b,
                                     });
      }
   } else if (ref->file == IBC_FILE_FLAG) {
      if (ref->type != IBC_TYPE_FLAG) {
         assert(ref->flag.bit % ibc_type_bit_size(ref->type) == 0);
         simd_width = ibc_type_bit_size(ref->type);
      }

      const unsigned num_chunks = DIV_ROUND_UP(simd_width, FLAG_CHUNK_BITS);
      assert(ref->flag.bit % FLAG_CHUNK_BITS == 0);
      const unsigned chunk_idx = ref->flag.bit / FLAG_CHUNK_BITS;

      ibc_sched_node *last = NULL;
      for (unsigned i = 0; i < num_chunks; i++) {
         if (b->flag_last_write[chunk_idx + i] == NULL ||
             b->flag_last_write[chunk_idx + i] == last)
            continue;

         last = b->flag_last_write[chunk_idx + i];
         cb(ref, 1, last, b);
      }
   } else if (ref->file == IBC_FILE_ACCUM) {
      /* In theory, we could probably track HW flags at a finer granularity so
       * that flag writes can move past each other in post-RA scheduling.
       * However, doing so is really tricky and would require tracking which
       * types are used because a write to acc1 as IBC_TYPE_F might interfere
       * with access of acc0 as IBC_TYPE_D.
       */
      if (b->accum_last_write != NULL)
         cb(ref, 1, b->accum_last_write, b);
   } else {
      unreachable("Invalid register file for NULL reg");
   }
}

static void
add_raw_dep_cb(const ibc_ref *ref, uint16_t num_bytes,
               ibc_sched_node *dep_node,
               struct ibc_sched_graph_builder *b)
{
   /* We record the node latency as being from the start of one instruction
    * to the start of the other so we need both FE time and latency.
    */
   unsigned raw_latency = dep_node->latency.fe_time +
                          dep_node->latency.fe_to_dest;

   /* If it's getting read as a flag, assume that the write wrote it as a
    * flag and so there's an extra four cycles of latency for the cmod
    * comparison.
    */
   if (ref->type == IBC_TYPE_FLAG)
      raw_latency += 4;

   ibc_sched_graph_add_dep(b->graph, b->iter_node, dep_node,
                           num_bytes, raw_latency);
}

static bool
add_raw_ref_dep(ibc_ref *ref,
                int num_bytes, int num_comps,
                uint8_t simd_group, uint8_t simd_width,
                void *_state)
{
   struct ibc_sched_graph_builder *b = _state;
   ibc_sched_graph *g = b->graph;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   foreach_dep_node(ref, num_bytes, num_comps, simd_group, simd_width,
                    add_raw_dep_cb, b);

   if (ref->reg != NULL && ref->reg->is_wlr) {
      /* For write-lock-read registers, we have an additional requirement that
       * all the reads have to come after all the writes.  Reads can still
       * move past reads and writes can still move past writes as long as they
       * touch independent chunk.
       */
      const unsigned num_chunks = g->live->regs[ref->reg->index].num_chunks;
      const unsigned chunk_idx = g->live->regs[ref->reg->index].chunk_idx;
      for (unsigned i = 0; i < num_chunks; i++) {
         if (b->chunk_last_write[chunk_idx + i]) {
            ibc_sched_graph_add_dep(g, b->iter_node,
                                    b->chunk_last_write[chunk_idx + i],
                                    0 /* num_bytes */, 0 /* latency */);
         }
      }
   }

   return true;
}

static void
add_waw_dep_cb(const ibc_ref *ref, uint16_t num_bytes,
               ibc_sched_node *dep_node,
               struct ibc_sched_graph_builder *b)
{
   /* We record the node latency as being from the start of one instruction
    * to the start of the other so we need both FE time and latency.  Write
    * stalls happen after FE has finished firing off register loads so we
    * don't have quite as much latency for RaW hazards.
    */
   unsigned waw_latency = dep_node->latency.fe_time +
                          dep_node->latency.fe_to_dest -
                          b->iter_node->latency.fe_time;

   /* If it's getting written as a flag, assume that the previous write
    * also wrote it as a flag and so there's an extra four cycles of
    * latency for the cmod comparison.
    */
   if (ref->type == IBC_TYPE_FLAG)
      waw_latency += 4;

   ibc_sched_graph_add_dep(b->graph, b->iter_node, dep_node,
                           0 /* num_bytes */, waw_latency);
}

static bool
add_waw_ref_dep(ibc_ref *ref,
                int num_bytes, int num_comps,
                uint8_t simd_group, uint8_t simd_width,
                void *_state)
{
   struct ibc_sched_graph_builder *b = _state;

   assert(ref->file != IBC_FILE_IMM);
   if (ref->file == IBC_FILE_NONE)
      return true;

   foreach_dep_node(ref, num_bytes, num_comps, simd_group, simd_width,
                    add_waw_dep_cb, b);
   return true;
}

static bool
record_ref_write(ibc_ref *ref,
                 int num_bytes, int num_comps,
                 uint8_t simd_group, uint8_t simd_width,
                 void *_state)
{
   struct ibc_sched_graph_builder *b = _state;
   ibc_sched_graph *g = b->graph;

   assert(ref->file != IBC_FILE_IMM);
   if (ref->file == IBC_FILE_NONE)
      return true;

   set_dep_node(ref, b->iter_node, num_bytes, num_comps,
                simd_group, simd_width, b);

   if (ref->file == IBC_FILE_ACCUM)
      b->iter_node->writes.accum = true;

   /* Only record the number of bytes written by the node if we're going
    * top-down so we don't double-count.
    */
   if (ref->reg == NULL || !b->top_down)
      return true;

   assert(ref->reg->index < g->live->num_regs);
   const unsigned num_chunks = g->live->regs[ref->reg->index].num_chunks;
   const unsigned chunk_idx = g->live->regs[ref->reg->index].chunk_idx;

   BITSET_DECLARE(chunks, IBC_REG_LIVE_MAX_CHUNKS);
   memset(chunks, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_ref_chunks(g->live, ref, num_bytes, num_comps,
                                 simd_group, simd_width, chunks);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (BITSET_TEST(chunks, i)) {
         b->iter_node->writes.grf_bytes +=
            g->reg_chunks[chunk_idx + i].grf_bytes;
         b->iter_node->writes.flag_bits +=
            g->reg_chunks[chunk_idx + i].flag_bits;
      }
   }

   return true;
}

static void
add_war_dep_cb(const ibc_ref *ref, UNUSED uint16_t num_bytes,
               ibc_sched_node *dep_node,
               struct ibc_sched_graph_builder *b)
{
   if (b->iter_node == dep_node)
      return;

   ibc_sched_graph_add_dep(b->graph, dep_node, b->iter_node,
                           0 /* num_bytes */, 0 /* latency */);
}

static bool
add_war_ref_dep(ibc_ref *ref,
                int num_bytes, int num_comps,
                uint8_t simd_group, uint8_t simd_width,
                void *_state)
{
   struct ibc_sched_graph_builder *b = _state;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   foreach_dep_node(ref, num_bytes, num_comps, simd_group, simd_width,
                    add_war_dep_cb, b);

   return true;
}

static bool
all_regs(const ibc_reg *reg)
{
   return true;
}

static ibc_sched_graph *
ibc_sched_graph_create(const ibc_shader *shader, void *mem_ctx)
{
   struct ibc_sched_graph_builder b = {
      .mem_ctx = mem_ctx,
      .top_down = true,
   };

   ibc_sched_graph *g = rzalloc(mem_ctx, ibc_sched_graph);
   b.graph = g;

   g->live = ibc_compute_live_sets(shader, all_regs, g);
   if (g->live->num_regs == 0) {
      ralloc_free(g->live);
      g->live = NULL;
   }

   /* Initialize per-chunk GRF and FLAG estimates */
   if (g->live) {
      g->reg_chunks = rzalloc_array(g, ibc_sched_reg_chunk,
                                    g->live->num_chunks);
      ibc_foreach_reg(reg, shader) {
         assert(reg->index < g->live->num_regs);
         const ibc_reg_live_intervals *rli = &g->live->regs[reg->index];

         uint32_t grf_bytes = 0, flag_bits = 0;
         switch (reg->file) {
         case IBC_FILE_LOGICAL:
            if (rli->chunk_simd_width > 32) {
               /* This only happens for registers that are never written.  We
                * assume they don't contribute to register pressure.
                */
               assert(rli->num_chunks == 1);
            } else if (reg->logical.bit_size == 1) {
               /* Assume a W type vector representation */
               grf_bytes = 2 * rli->chunk_simd_width;
               flag_bits = rli->chunk_simd_width;
            } else {
               grf_bytes = rli->chunk_simd_width * rli->chunk_byte_size;
            }
            break;

         case IBC_FILE_HW_GRF:
            grf_bytes = rli->chunk_byte_size;
            break;

         case IBC_FILE_FLAG:
            /* Assume a scalar representation.  Unfortunately, that doesn't
             * chunk well so we just do the best we can.
             */
            grf_bytes = DIV_ROUND_UP(rli->chunk_simd_width, 8);
            flag_bits = rli->chunk_simd_width;
            break;

         case IBC_FILE_ACCUM:
            break;

         default:
            unreachable("Unsupported register file");
         }

         for (unsigned c = 0; c < rli->num_chunks; c++) {
            g->reg_chunks[rli->chunk_idx + c] = (ibc_sched_reg_chunk) {
               .flag_bits = flag_bits,
               .grf_bytes = grf_bytes,
            };
         }
      }
   }

   /* Allocate an array for the nodes */
   assert(g->num_nodes == 0);
   ibc_foreach_instr(instr, shader)
      instr->index = g->num_nodes++;
   g->nodes = rzalloc_array(g, ibc_sched_node, g->num_nodes);

   /* A few temporaries to use while iterating */
   ibc_flow_instr *block_start = NULL;
   ibc_sched_node *last_cant_reorder = NULL;
   struct list_head next_barrier_deps;
   list_inithead(&next_barrier_deps);

   ibc_foreach_instr(instr, shader) {
      ibc_sched_node *node = b.iter_node = &g->nodes[instr->index];

      /* Initialize the node */
      node->instr = instr;
      node->block_start = block_start;
      node->latency.fe_time = ibc_instr_fe_cycles(instr, shader->devinfo);
      node->latency.fe_to_dest = ibc_instr_dest_latency(instr, shader->devinfo);

      /* First, record all the dependencies of this instruction.
       *
       * Don't bother to register source dependencies for LOAD_PAYLOAD
       * intrinsics because that's just going to cause the grf array to be
       * allocated and waste memory.  They're not allowed to move anyway.
       */
      if (!ibc_instr_is_load_payload(instr))
         ibc_instr_foreach_read(instr, add_raw_ref_dep, &b);

      ibc_instr_foreach_write(instr, add_waw_ref_dep, &b);

      if (last_cant_reorder != NULL && !ibc_instr_can_reorder(instr)) {
         ibc_sched_graph_add_dep(g, node, last_cant_reorder,
                                 0 /* num_bytes */, 0 /* latency */);
      }

      /* Anything on which this node depends no longer needs to be a
       * dependency of the next barrier because it will transitively be a
       * barrier thanks to this node.
       */
      for (unsigned i = 0; i < node->num_src_deps; i++) {
         ibc_sched_node *dep_node = node->src_deps[i].node;
         if (dep_node->link.next)
            list_del(&dep_node->link);
      }

      if (ibc_instr_is_full_barrier(instr)) {
         /* Add all previous instructions which don't have anything which
          * depends on them yet.
          */
         list_for_each_entry_safe(ibc_sched_node, dep_node,
                                  &next_barrier_deps, link) {
            ibc_sched_graph_add_dep(g, node, dep_node,
                                    0 /* num_bytes */, 0 /* latency */);
            list_del(&dep_node->link);
         }
         assert(list_is_empty(&next_barrier_deps));
      }

      /* Figure out our top-down longest-path latency */
      for (unsigned i = 0; i < node->num_src_deps; i++) {
         const uint32_t latency_to_start =
            node->src_deps[i].node->latency.min_to_start +
            node->src_deps[i].latency;
         if (node->latency.min_to_start < latency_to_start)
            node->latency.min_to_start = latency_to_start;
      }

      /* Now that we've added all this node's dependencies, set up the state
       * so that things which depend on this node can find it.
       */
      ibc_instr_foreach_write(instr, record_ref_write, &b);

      if (!ibc_instr_can_reorder(instr))
         last_cant_reorder = node;

      list_addtail(&node->link, &next_barrier_deps);

      /* Flow instructions are considered to live in the previous block */
      if (instr->type == IBC_INSTR_TYPE_FLOW)
         block_start = ibc_instr_as_flow(instr);
   }

   assert(list_is_singular(&next_barrier_deps));
   list_inithead(&next_barrier_deps);

   b.top_down = false;

   /* Reset the register sets to NULL for the bottom-up pass */
   if (b.chunk_last_write) {
      memset(b.chunk_last_write, 0,
             g->live->num_chunks * sizeof(*b.chunk_last_write));
   }
   if (b.grf_last_write) {
      memset(b.grf_last_write, 0,
             TOTAL_GRF_BYTES * sizeof(*b.grf_last_write));
   }
   memset(b.flag_last_write, 0, sizeof(b.flag_last_write));
   b.accum_last_write = NULL;

   /* Walk bottom-up to fill out WaR dependencies */
   for (int i = g->num_nodes - 1; i >= 0; i--) {
      ibc_sched_node *node = b.iter_node = &g->nodes[i];

      ibc_instr_foreach_write((ibc_instr *)node->instr, record_ref_write, &b);
      ibc_instr_foreach_read((ibc_instr *)node->instr, add_war_ref_dep, &b);

      /* Anything on which this node depends no longer needs to be a
       * dependency of the next barrier because it will transitively be a
       * barrier thanks to this node.
       */
      for (unsigned i = 0; i < node->num_dest_deps; i++) {
         ibc_sched_node *dep_node = node->dest_deps[i].node;
         if (dep_node->link.next)
            list_del(&dep_node->link);
      }

      if (ibc_instr_is_full_barrier(node->instr)) {
         /* Add all previous instructions which don't have anything which
          * depends on them yet.
          */
         list_for_each_entry_safe(ibc_sched_node, dep_node,
                                  &next_barrier_deps, link) {
            ibc_sched_graph_add_dep(g, dep_node, node,
                                    0 /* num_bytes */, 0 /* latency */);
            list_del(&dep_node->link);
         }
         assert(list_is_empty(&next_barrier_deps));
      }

      /* Figure out our bottom-up longest-path latency */
      for (unsigned i = 0; i < node->num_dest_deps; i++) {
         const uint32_t latency_to_end =
            node->dest_deps[i].node->latency.min_to_end +
            node->dest_deps[i].latency;
         if (node->latency.min_to_end < latency_to_end)
            node->latency.min_to_end = latency_to_end;
      }

      list_addtail(&node->link, &next_barrier_deps);
   }

   ralloc_free(b.chunk_last_write);
   ralloc_free(b.grf_last_write);

   return g;
}

UNUSED static void
ibc_sched_graph_print(ibc_sched_graph *g, FILE *fp)
{
   for (unsigned i = 0; i < g->num_nodes; i++) {
      ibc_sched_node *node = &g->nodes[i];
      fprintf(fp, "Node %u:\n", i);

      fprintf(fp, "   ");
      for (unsigned i = 0; i < node->num_src_deps; i++) {
         assert(node > node->src_deps[i].node);
         fprintf(fp, " > %u", (unsigned)(node->src_deps[i].node - g->nodes));
      }
      fprintf(fp, "\n");

      fprintf(fp, "   ");
      for (unsigned i = 0; i < node->num_dest_deps; i++) {
         assert(node < node->dest_deps[i].node);
         fprintf(fp, " < %u", (unsigned)(node->dest_deps[i].node - g->nodes));
      }
      fprintf(fp, "\n");
   }
}

static ibc_schedule *
ibc_schedule_create(ibc_sched_graph *graph, void *mem_ctx)
{
   size_t size = sizeof(ibc_schedule) + graph->num_nodes * sizeof(uint32_t);
   return rzalloc_size(mem_ctx, size);
}

static void
ibc_shader_apply_schedule(ibc_shader *shader,
                          ibc_sched_graph *g,
                          ibc_schedule *sched)
{
   /* Clear out the instruction list and prep for scheduling */
   list_inithead(&shader->instrs);

#ifndef NDEBUG
   /* Reset reference counts and set cycles to 0 */
   for (unsigned i = 0; i < g->num_nodes; i++) {
      ibc_sched_node *node = &g->nodes[i];
      node->ref_count = node->num_src_deps;
   }
#endif

   shader->cycles = 0;
   for (unsigned i = 0; i < g->num_nodes; i++) {
      ibc_sched_node *node = &g->nodes[sched->order[i]];
      ibc_instr *instr = (ibc_instr *)node->instr;
      list_addtail(&instr->link, &shader->instrs);

      node->data.cycle = shader->cycles;
      for (unsigned j = 0; j < node->num_src_deps; j++) {
         uint32_t dep_end_cycle = node->src_deps[j].node->data.cycle +
                                  node->src_deps[j].latency;
         node->data.cycle = MAX2(node->data.cycle, dep_end_cycle);
      }
      shader->cycles = node->data.cycle + node->latency.fe_time;

#ifndef NDEBUG
      assert(node->ref_count == 0);

      for (unsigned j = 0; j < node->num_dest_deps; j++) {
         ibc_sched_node *dep = node->dest_deps[j].node;
         assert(dep->ref_count > 0);
         dep->ref_count--;
      }
#endif
   }

   /* Cycle count sanity check. */
   assert(shader->cycles == sched->cycles);

   /* We just re-ordered the universe */
   ibc_repair_wlr_order(shader);
}

typedef struct ibc_schedule_builder {
   ibc_sched_graph *graph;
   ibc_schedule *sched;

   enum ibc_sched_direction direction;

   uint32_t next_idx;

   /** Estimated HW_GRF pressure */
   uint32_t grf_bytes;
   /** Estimated FLAG pressure */
   uint32_t flag_bits;
} ibc_schedule_builder;

static void
ibc_schedule_builder_init(ibc_schedule_builder *b,
                          ibc_sched_graph *graph,
                          ibc_schedule *sched,
                          enum ibc_sched_direction direction)
{
   memset(b, 0, sizeof(*b));
   b->graph = graph;
   b->sched = sched;
   b->direction = direction;

   if (b->direction == IBC_SCHED_DIRECTION_TOP_DOWN) {
      b->next_idx = 0;
   } else {
      assert(b->direction == IBC_SCHED_DIRECTION_BOTTOM_UP);
      b->next_idx = graph->num_nodes - 1;
   }
}

static void
ibc_schedule_builder_init_new(ibc_schedule_builder *b,
                              ibc_sched_graph *graph,
                              enum ibc_sched_direction direction,
                              void *mem_ctx)
{
   ibc_schedule *sched = ibc_schedule_create(graph, mem_ctx);
   ibc_schedule_builder_init(b, graph, sched, direction);
}

static bool
add_reg_chunk_refs(ibc_ref *ref,
                   int num_bytes, int num_comps,
                   uint8_t simd_group, uint8_t simd_width,
                   void *_state)
{
   ibc_schedule_builder *b = _state;
   ibc_sched_graph *g = b->graph;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg == NULL)
      return true;

   assert(ref->reg->index < g->live->num_regs);
   const unsigned num_chunks = g->live->regs[ref->reg->index].num_chunks;
   const unsigned chunk_idx = g->live->regs[ref->reg->index].chunk_idx;

   BITSET_DECLARE(chunks, IBC_REG_LIVE_MAX_CHUNKS);
   memset(chunks, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_ref_chunks(g->live, ref, num_bytes, num_comps,
                                 simd_group, simd_width, chunks);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (BITSET_TEST(chunks, i)) {
         assert(g->reg_chunks[chunk_idx + i].num_refs ==
                g->reg_chunks[chunk_idx + i].ref_count);
         g->reg_chunks[chunk_idx + i].num_refs++;
         g->reg_chunks[chunk_idx + i].ref_count++;
      }
   }

   return true;
}

static bool
decref_reg_chunks(ibc_ref *ref,
                  int num_bytes, int num_comps,
                  uint8_t simd_group, uint8_t simd_width,
                  void *_state)
{
   ibc_schedule_builder *b = _state;
   ibc_sched_graph *g = b->graph;

   if (ref->file == IBC_FILE_NONE || ref->file == IBC_FILE_IMM)
      return true;

   if (ref->reg == NULL)
      return true;

   assert(ref->reg->index < g->live->num_regs);
   const unsigned num_chunks = g->live->regs[ref->reg->index].num_chunks;
   const unsigned chunk_idx = g->live->regs[ref->reg->index].chunk_idx;

   BITSET_DECLARE(chunks, IBC_REG_LIVE_MAX_CHUNKS);
   memset(chunks, 0, BITSET_WORDS(num_chunks) * sizeof(BITSET_WORD));
   ibc_live_intervals_ref_chunks(g->live, ref, num_bytes, num_comps,
                                 simd_group, simd_width, chunks);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (BITSET_TEST(chunks, i)) {
         ibc_sched_reg_chunk *chunk = &g->reg_chunks[chunk_idx + i];
         assert(chunk->ref_count > 0);

         /* If this is our first dropped reference, it just went live. */
         if (chunk->num_refs == chunk->ref_count) {
            b->grf_bytes += chunk->grf_bytes;
            b->flag_bits += chunk->flag_bits;
         }
      }
   }

   b->sched->max_grf_bytes = MAX2(b->sched->max_grf_bytes, b->grf_bytes);
   b->sched->max_flag_bits = MAX2(b->sched->max_flag_bits, b->flag_bits);

   for (unsigned i = 0; i < num_chunks; i++) {
      if (BITSET_TEST(chunks, i)) {
         ibc_sched_reg_chunk *chunk = &g->reg_chunks[chunk_idx + i];
         assert(chunk->ref_count > 0);
         chunk->ref_count--;

         /* If this is our last reference, it just went dead */
         if (chunk->ref_count == 0) {
            assert(b->grf_bytes >= chunk->grf_bytes);
            assert(b->flag_bits >= chunk->flag_bits);
            b->grf_bytes -= chunk->grf_bytes;
            b->flag_bits -= chunk->flag_bits;

            /* Also reset num_refs so we don't have to have a separate pass
             * over all of the chunks on every block just to reset reference
             * counts.
             */
            chunk->num_refs = 0;
         }
      }
   }

   return true;
}

static void
ibc_schedule_start_block(ibc_schedule_builder *b,
                         const ibc_flow_instr *block_start)
{
   ibc_sched_graph *g = b->graph;

   assert(block_start->op != IBC_FLOW_OP_END);

   /* Flow instructions should be scheduled in the block above the flow
    * instruction.
    */
   if (b->direction == IBC_SCHED_DIRECTION_TOP_DOWN) {
      b->next_idx = block_start->instr.index + 1;
   } else {
      assert(b->direction == IBC_SCHED_DIRECTION_BOTTOM_UP);
      const ibc_flow_instr *block_end = ibc_flow_instr_next(block_start);
      b->next_idx = block_end->instr.index;
   }

   /* Pressure estimates are only useful for pre-RA cheduling */
   if (b->graph->live == NULL)
      return;

   const ibc_instr *instr = &block_start->instr;
   do {
      instr = ibc_instr_next(instr);

      ibc_instr_foreach_read((ibc_instr *)instr, add_reg_chunk_refs, b);
      ibc_instr_foreach_write((ibc_instr *)instr, add_reg_chunk_refs, b);
   } while (instr->type != IBC_INSTR_TYPE_FLOW);

   assert(block_start->block_index < g->live->num_blocks);
   const ibc_block_live_sets *bls = &g->live->blocks[block_start->block_index];
   const uint32_t bitset_words = BITSET_WORDS(g->live->num_chunks);

   BITSET_WORD *live = (b->direction == IBC_SCHED_DIRECTION_TOP_DOWN) ?
                       bls->livein : bls->liveout;

   b->grf_bytes = 0;
   b->flag_bits = 0;
   for (uint32_t w = 0; w < bitset_words; w++) {
      BITSET_WORD live_scan = live[w];
      while (live_scan) {
         int i = u_bit_scan(&live_scan);
         ibc_sched_reg_chunk *chunk = &g->reg_chunks[w * BITSET_WORDBITS + i];

         b->grf_bytes += chunk->grf_bytes;
         b->flag_bits += chunk->flag_bits;

         /* Consider any liveout registers to have a reference that
          * has already been used.  This way we get the "free"
          * trigger when ref_count hits 0 but not the "allocated"
          * trigger when ref_count first drops below num_refs.
          */
         if (chunk->ref_count > 0) {
            assert(chunk->num_refs == chunk->ref_count);
            chunk->num_refs++;
         }
      }
   }

   b->sched->max_grf_bytes = MAX2(b->sched->max_grf_bytes, b->grf_bytes);
   b->sched->max_flag_bits = MAX2(b->sched->max_flag_bits, b->flag_bits);
}

static void
ibc_schedule_add_node(ibc_schedule_builder *b,
                      ibc_sched_node *node)
{
   const ibc_instr *instr = node->instr;

   if (b->direction == IBC_SCHED_DIRECTION_TOP_DOWN) {
      b->sched->order[b->next_idx++] = instr->index;

      if (b->graph->live != NULL) {
         ibc_instr_foreach_read((ibc_instr *)instr, decref_reg_chunks, b);
         ibc_instr_foreach_write((ibc_instr *)instr, decref_reg_chunks, b);
      }

      node->data.cycle = b->sched->cycles;
      for (unsigned i = 0; i < node->num_src_deps; i++) {
         uint32_t dep_cycle = node->src_deps[i].node->data.cycle +
                              node->src_deps[i].latency;
         node->data.cycle = MAX2(node->data.cycle, dep_cycle);
      }
      b->sched->cycles = node->data.cycle + node->latency.fe_time;

      if (instr->type == IBC_INSTR_TYPE_FLOW &&
          ibc_instr_as_flow(instr)->op != IBC_FLOW_OP_END) {
         ASSERTED uint32_t next_idx = b->next_idx;
         ibc_schedule_start_block(b, ibc_instr_as_flow(instr));
         assert(b->next_idx == next_idx);
      }
   } else {
      assert(b->direction == IBC_SCHED_DIRECTION_BOTTOM_UP);

      if (instr->type == IBC_INSTR_TYPE_FLOW &&
          ibc_instr_as_flow(instr)->op != IBC_FLOW_OP_START) {
         ASSERTED uint32_t next_idx = b->next_idx;
         ibc_flow_instr *block_start =
            ibc_flow_instr_prev(ibc_instr_as_flow(instr));
         ibc_schedule_start_block(b, block_start);
         assert(b->next_idx == next_idx);
      }

      node->data.cycle = b->sched->cycles + node->latency.fe_time;
      for (unsigned i = 0; i < node->num_dest_deps; i++) {
         uint32_t dep_cycle = node->dest_deps[i].node->data.cycle +
                              node->dest_deps[i].latency;
         node->data.cycle = MAX2(node->data.cycle, dep_cycle);
      }
      b->sched->cycles = node->data.cycle;

      const ibc_instr *instr = node->instr;
      if (b->graph->live != NULL) {
         ibc_instr_foreach_read((ibc_instr *)instr, decref_reg_chunks, b);
         ibc_instr_foreach_write((ibc_instr *)instr, decref_reg_chunks, b);
      }

      b->sched->order[b->next_idx--] = instr->index;
   }
}

static ibc_schedule *
noop_schedule(const ibc_shader *shader, ibc_sched_graph *g, void *mem_ctx)
{
   ibc_schedule_builder b;
   ibc_schedule_builder_init_new(&b, g, IBC_SCHED_DIRECTION_TOP_DOWN, mem_ctx);

   for (unsigned i = 0; i < g->num_nodes; i++)
      ibc_schedule_add_node(&b, &g->nodes[i]);

   return b.sched;
}

static int
rb_cmp_nodes_by_cycle(const struct rb_node *_a, const struct rb_node *_b)
{
   ibc_sched_node *a = rb_node_data(ibc_sched_node, _a, rb_node);
   ibc_sched_node *b = rb_node_data(ibc_sched_node, _b, rb_node);
   return (int)b->data.cycle - (int)a->data.cycle;
}

static int
rb_cmp_nodes_by_min_to_start(const struct rb_node *_a,
                             const struct rb_node *_b)
{
   ibc_sched_node *a = rb_node_data(ibc_sched_node, _a, rb_node);
   ibc_sched_node *b = rb_node_data(ibc_sched_node, _b, rb_node);
   return(int)b->latency.min_to_start - (int)a->latency.min_to_start;
}

static int
rb_cmp_nodes_by_min_to_end(const struct rb_node *_a,
                           const struct rb_node *_b)
{
   ibc_sched_node *a = rb_node_data(ibc_sched_node, _a, rb_node);
   ibc_sched_node *b = rb_node_data(ibc_sched_node, _b, rb_node);
   return (int)b->latency.min_to_end - (int)a->latency.min_to_end;
}

static ibc_schedule *
bottom_up_latency_schedule(const ibc_shader *shader, ibc_sched_graph *g,
                           void *mem_ctx)
{
   ibc_schedule_builder b;
   ibc_schedule_builder_init_new(&b, g, IBC_SCHED_DIRECTION_BOTTOM_UP, mem_ctx);

   /* Reset reference counts and set cycles to 0 */
   for (unsigned i = 0; i < g->num_nodes; i++) {
      ibc_sched_node *node = &g->nodes[i];
      node->ref_count = node->num_dest_deps;
      node->data.cycle = 0;
   }

   struct rb_tree ready, leader;
   rb_tree_init(&ready);
   rb_tree_init(&leader);

   rb_tree_insert(&ready, &g->nodes[g->num_nodes - 1].rb_node,
                  rb_cmp_nodes_by_min_to_start);

   while (!rb_tree_is_empty(&ready) || !rb_tree_is_empty(&leader)) {
      ibc_sched_node *node;
      if (!rb_tree_is_empty(&ready)) {
         node = rb_node_data(ibc_sched_node, rb_tree_last(&ready), rb_node);
         rb_tree_remove(&ready, &node->rb_node);
      } else {
         assert(!rb_tree_is_empty(&leader));
         node = rb_node_data(ibc_sched_node, rb_tree_first(&leader), rb_node);
         rb_tree_remove(&leader, &node->rb_node);
      }

      ibc_schedule_add_node(&b, node);

      for (unsigned i = 0; i < node->num_src_deps; i++) {
         ibc_sched_node *dep = node->src_deps[i].node;

         uint32_t dep_cycles = node->data.cycle + node->src_deps[i].latency;
         dep->data.cycle = MAX2(dep->data.cycle, dep_cycles);

         assert(dep->ref_count > 0);
         dep->ref_count--;

         /* A node's cycle count won't be altered between the time that its
          * last destination reference is gone and the time it gets put in the
          * schedule.  Therefore, it's safe to use it as a keep in the leaders
          * red-black tree.
          */
         if (dep->ref_count == 0)
            rb_tree_insert(&leader, &dep->rb_node, rb_cmp_nodes_by_cycle);
      }

      while (!rb_tree_is_empty(&leader)) {
         ibc_sched_node *node =
            rb_node_data(ibc_sched_node, rb_tree_first(&leader), rb_node);
         if (node->data.cycle > b.sched->cycles)
            break;

         rb_tree_remove(&leader, &node->rb_node);
         rb_tree_insert(&ready, &node->rb_node, rb_cmp_nodes_by_min_to_start);
      }
   }

   return b.sched;
}

static ibc_schedule *
top_down_latency_schedule(const ibc_shader *shader, ibc_sched_graph *g,
                          void *mem_ctx)
{
   ibc_schedule_builder b;
   ibc_schedule_builder_init_new(&b, g, IBC_SCHED_DIRECTION_TOP_DOWN, mem_ctx);

   /* Reset reference counts and set cycles to 0 */
   for (unsigned i = 0; i < g->num_nodes; i++) {
      ibc_sched_node *node = &g->nodes[i];
      node->ref_count = node->num_src_deps;
      node->data.cycle = 0;
   }

   struct rb_tree ready, leader;
   rb_tree_init(&ready);
   rb_tree_init(&leader);

   rb_tree_insert(&ready, &g->nodes[0].rb_node,
                  rb_cmp_nodes_by_min_to_end);

   while (!rb_tree_is_empty(&ready) || !rb_tree_is_empty(&leader)) {
      ibc_sched_node *node;
      if (!rb_tree_is_empty(&ready)) {
         node = rb_node_data(ibc_sched_node, rb_tree_last(&ready), rb_node);
         rb_tree_remove(&ready, &node->rb_node);
      } else {
         assert(!rb_tree_is_empty(&leader));
         node = rb_node_data(ibc_sched_node, rb_tree_first(&leader), rb_node);
         rb_tree_remove(&leader, &node->rb_node);
      }

      ibc_schedule_add_node(&b, node);

      for (unsigned i = 0; i < node->num_dest_deps; i++) {
         ibc_sched_node *dep = node->dest_deps[i].node;

         uint32_t dep_cycles = node->data.cycle + node->dest_deps[i].latency;
         dep->data.cycle = MAX2(dep->data.cycle, dep_cycles);

         assert(dep->ref_count > 0);
         dep->ref_count--;

         /* A node's cycle count won't be altered between the time that its
          * last destination reference is gone and the time it gets put in the
          * schedule.  Therefore, it's safe to use it as a keep in the leaders
          * red-black tree.
          */
         if (dep->ref_count == 0)
            rb_tree_insert(&leader, &dep->rb_node, rb_cmp_nodes_by_cycle);
      }

      while (!rb_tree_is_empty(&leader)) {
         ibc_sched_node *node =
            rb_node_data(ibc_sched_node, rb_tree_first(&leader), rb_node);
         if (node->data.cycle > b.sched->cycles)
            break;

         rb_tree_remove(&leader, &node->rb_node);
         rb_tree_insert(&ready, &node->rb_node, rb_cmp_nodes_by_min_to_end);
      }
   }

   return b.sched;
}

static int
hsu_grf_bytes_freed(struct ibc_sched_node *node, bool statistical)
{
   int grf_bytes_freed = -(int)node->writes.grf_bytes;

   for (unsigned i = 0; i < node->num_src_deps; i++) {
      if (node->src_deps[i].num_bytes > 0) {
         assert(node->src_deps[i].node->data.data_ref_count > 0);
         /* If we're the last remaining data use then we will free the data
          * written by this node.
          */
         if (node->src_deps[i].node->data.data_ref_count == 1) {
            grf_bytes_freed += node->src_deps[i].node->writes.grf_bytes;
         } else if (statistical) {
            grf_bytes_freed += node->src_deps[i].node->writes.grf_bytes /
                               node->src_deps[i].node->data.data_ref_count;
         }
      }
   }

   return grf_bytes_freed;
}

static ibc_sched_node *
dynamic_ra_sensitive_choose_node(ibc_schedule_builder *b,
                                 struct rb_tree *ready,
                                 struct rb_tree *leader,
                                 uint32_t threshold)
{
   if (b->grf_bytes < threshold) {
      /* If we're within the threshold, choose the node with the best schedule
       * as per one of the latency sensitive schedulers.
       */
      rb_tree_foreach_rev(ibc_sched_node, node, ready, rb_node) {
         rb_tree_remove(ready, &node->rb_node);
         return node;
      }
      rb_tree_foreach(ibc_sched_node, node, leader, rb_node) {
         rb_tree_remove(leader, &node->rb_node);
         return node;
      }

      unreachable("One of ready or leader must have a node");
   } else {
      /* First, try to find a node that frees registers or, at the very least
       * doesn't hurt our register pressure.
       */
      int highest_grf_bytes_stat_freed = INT_MIN;
      ibc_sched_node *highest_grf_bytes_stat_freed_node = NULL;
      struct rb_tree *highest_grf_bytes_stat_freed_tree = NULL;

      rb_tree_foreach_rev(ibc_sched_node, node, ready, rb_node) {
         int grf_bytes_stat_freed =
            hsu_grf_bytes_freed(node, false /* statistical */);
         if (highest_grf_bytes_stat_freed < grf_bytes_stat_freed) {
            highest_grf_bytes_stat_freed = grf_bytes_stat_freed;
            highest_grf_bytes_stat_freed_node = node;
            highest_grf_bytes_stat_freed_tree = ready;
         }
      }

      if (highest_grf_bytes_stat_freed > 0) {
         /* If a ready instructions cuts pressure, pick that. */
         rb_tree_remove(highest_grf_bytes_stat_freed_tree,
                        &highest_grf_bytes_stat_freed_node->rb_node);
         return highest_grf_bytes_stat_freed_node;
      }

      rb_tree_foreach(ibc_sched_node, node, leader, rb_node) {
         int grf_bytes_stat_freed =
            hsu_grf_bytes_freed(node, false /* statistical */);
         if (highest_grf_bytes_stat_freed < grf_bytes_stat_freed) {
            highest_grf_bytes_stat_freed = grf_bytes_stat_freed;
            highest_grf_bytes_stat_freed_node = node;
            highest_grf_bytes_stat_freed_tree = leader;
         }
      }

      if (highest_grf_bytes_stat_freed >= 0) {
         /* If a ready or leader instruction isn't worse, pick that. */
         rb_tree_remove(highest_grf_bytes_stat_freed_tree,
                        &highest_grf_bytes_stat_freed_node->rb_node);
         return highest_grf_bytes_stat_freed_node;
      }

      rb_tree_foreach_rev(ibc_sched_node, node, ready, rb_node) {
         int grf_bytes_stat_freed =
            hsu_grf_bytes_freed(node, true /* statistical */);
         if (highest_grf_bytes_stat_freed < grf_bytes_stat_freed) {
            highest_grf_bytes_stat_freed = grf_bytes_stat_freed;
            highest_grf_bytes_stat_freed_node = node;
            highest_grf_bytes_stat_freed_tree = ready;
         }
      }

      rb_tree_foreach(ibc_sched_node, node, leader, rb_node) {
         int grf_bytes_stat_freed =
            hsu_grf_bytes_freed(node, true /* statistical */);
         if (highest_grf_bytes_stat_freed < grf_bytes_stat_freed) {
            highest_grf_bytes_stat_freed = grf_bytes_stat_freed;
            highest_grf_bytes_stat_freed_node = node;
            highest_grf_bytes_stat_freed_tree = leader;
         }
      }

      rb_tree_remove(highest_grf_bytes_stat_freed_tree,
                     &highest_grf_bytes_stat_freed_node->rb_node);
      return highest_grf_bytes_stat_freed_node;
   }
}

static ibc_schedule *
dynamic_ra_sensitive_schedule(const ibc_shader *shader, ibc_sched_graph *g,
                              uint32_t threshold, void *mem_ctx)
{
   ibc_schedule_builder b;
   ibc_schedule_builder_init_new(&b, g, IBC_SCHED_DIRECTION_TOP_DOWN, mem_ctx);

   /* Reset reference counts and set cycles to 0 */
   for (unsigned i = 0; i < g->num_nodes; i++) {
      ibc_sched_node *node = &g->nodes[i];
      node->ref_count = node->num_src_deps;
      node->data.cycle = 0;

      /* Because we're walking top-down, this will be initial before we see
       * anything which has this as a source dependency to increment it in the
       * loop below.
       */
      node->data.data_ref_count = 0;
      for (unsigned j = 0; j < node->num_src_deps; j++) {
         if (node->src_deps[j].num_bytes > 0)
            node->src_deps[j].node->data.data_ref_count++;
      }
   }

   struct rb_tree ready, leader;
   rb_tree_init(&ready);
   rb_tree_init(&leader);

   rb_tree_insert(&ready, &g->nodes[0].rb_node,
                  rb_cmp_nodes_by_min_to_end);

   while (!rb_tree_is_empty(&ready) || !rb_tree_is_empty(&leader)) {
      ibc_sched_node *node =
         dynamic_ra_sensitive_choose_node(&b, &ready, &leader, threshold);

      ibc_schedule_add_node(&b, node);

      for (unsigned i = 0; i < node->num_dest_deps; i++) {
         ibc_sched_node *dep = node->dest_deps[i].node;

         uint32_t dep_cycles = node->data.cycle + node->dest_deps[i].latency;
         dep->data.cycle = MAX2(dep->data.cycle, dep_cycles);

         assert(dep->ref_count > 0);
         dep->ref_count--;

         /* A node's cycle count won't be altered between the time that its
          * last destination reference is gone and the time it gets put in the
          * schedule.  Therefore, it's safe to use it as a keep in the leaders
          * red-black tree.
          */
         if (dep->ref_count == 0)
            rb_tree_insert(&leader, &dep->rb_node, rb_cmp_nodes_by_cycle);
      }

      for (unsigned i = 0; i < node->num_src_deps; i++) {
         if (node->src_deps[i].num_bytes > 0)
            node->src_deps[i].node->data.data_ref_count--;
      }

      while (!rb_tree_is_empty(&leader)) {
         ibc_sched_node *node =
            rb_node_data(ibc_sched_node, rb_tree_first(&leader), rb_node);
         if (node->data.cycle > b.sched->cycles)
            break;

         rb_tree_remove(&leader, &node->rb_node);
         rb_tree_insert(&ready, &node->rb_node, rb_cmp_nodes_by_min_to_end);
      }
   }

   return b.sched;
}

void
ibc_schedule_instructions(ibc_shader *shader)
{
   void *mem_ctx = ralloc_context(shader);
   ibc_sched_graph *graph = ibc_sched_graph_create(shader, mem_ctx);

   ibc_schedule *sched[] = {
      noop_schedule(shader, graph, mem_ctx),
      dynamic_ra_sensitive_schedule(shader, graph, 2048, mem_ctx),
   };

   /* Use the noop schedule as the base-line */
   ibc_schedule *best = sched[0];
   for (unsigned i = 1; i < ARRAY_SIZE(sched); i++) {
      /* Only pick non-noop schedules which have a reasonable register
       * pressure where reasonable is defined as 3/4 of the GRF.
       */
      if (sched[i]->max_grf_bytes > (4096 * 3) / 4)
         continue;

      if (best->cycles > sched[i]->cycles)
         best = sched[i];
   }

   ibc_shader_apply_schedule(shader, graph, best);

   ralloc_free(mem_ctx);
}

void
ibc_schedule_instructions_post_ra(ibc_shader *shader)
{
   void *mem_ctx = ralloc_context(shader);

   ibc_sched_graph *graph = ibc_sched_graph_create(shader, mem_ctx);

   ibc_schedule *sched[] = {
      noop_schedule(shader, graph, mem_ctx),
      bottom_up_latency_schedule(shader, graph, mem_ctx),
      top_down_latency_schedule(shader, graph, mem_ctx),
   };

   ibc_schedule *best = NULL;
   for (unsigned i = 0; i < ARRAY_SIZE(sched); i++) {
      if (best == NULL || best->cycles > sched[i]->cycles)
         best = sched[i];
   }

   ibc_shader_apply_schedule(shader, graph, best);

   ralloc_free(mem_ctx);
}
