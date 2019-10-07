/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2014,2018 Tommy Thorn
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Goal is RV64G, booting Linux
 *
 * Status: Is booting Linux!
 * TODO:
 *   FD - single and double precision floating point
 */


#ifndef __APPLE__
#define _XOPEN_SOURCE 500
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>
#include <fcntl.h>

#include "bitfields.h"
#include "sim.h"
#include "arch.h"
#include "riscv.h"

static const bool debug_cache = false;

static const int debug   = 0;
static const int info    = 0;
static const int error   = 1;

#define DEBUG(...)   ({ if (debug)  fprintf(stderr, __VA_ARGS__); })
#define INFO(...)    ({ if (info)   fprintf(stderr, __VA_ARGS__); })
#define IOINFO(...)  ({ if (ioinfo) fprintf(stderr, __VA_ARGS__); })
#define ERROR(...)   ({ if (error)  fprintf(stderr, __VA_ARGS__); })

static const char *csr_name[0x1000] = {
    [CSR_USTATUS] = "ustatus",
    [CSR_UIE] = "uie",
    [CSR_UTVEC] = "utvec",
    [CSR_USCRATCH] = "uscratch",
    [CSR_UEPC] = "uepc",
    [CSR_UCAUSE] = "ucause",
    [CSR_UTVAL] = "utval",
    [CSR_UIP] = "uip",
    [CSR_FFLAGS] = "fflags",
    [CSR_FRM] = "frm",
    [CSR_FCSR] = "fcsr",
    [CSR_CYCLE] = "cycle",
    [CSR_TIME] = "time",
    [CSR_INSTRET] = "instret",
    [CSR_CYCLEH] = "cycleh",
    [CSR_TIMEH] = "timeh",
    [CSR_INSTRETH] = "instreth",
    [CSR_SSTATUS] = "sstatus",
    [CSR_SEDELEG] = "sedeleg",
    [CSR_SIDELEG] = "sideleg",
    [CSR_SIE] = "sie",
    [CSR_STVEC] = "stvec",
    [CSR_SCOUNTEREN] = "scounteren",
    [CSR_SSCRATCH] = "sscratch",
    [CSR_SEPC] = "sepc",
    [CSR_SCAUSE] = "scause",
    [CSR_STVAL] = "stval",
    [CSR_SIP] = "sip",
    [CSR_SATP] = "satp",
    [CSR_MVENDORID] = "mvendorid",
    [CSR_MARCHID] = "marchid",
    [CSR_MIMPID] = "mimpid",
    [CSR_MHARTID] = "mhartid",
    [CSR_MSTATUS] = "mstatus",
    [CSR_MEDELEG] = "medeleg",
    [CSR_MIDELEG] = "mideleg",
    [CSR_MIE] = "mie",
    [CSR_MTVEC] = "mtvec",
    [CSR_MCOUNTEREN] = "mcounteren",
    [CSR_MSCRATCH] = "mscratch",
    [CSR_MEPC] = "mepc",
    [CSR_MCAUSE] = "mcause",
    [CSR_MTVAL] = "mtval",
    [CSR_MIP] = "mip",
    [CSR_MCYCLE] = "mcycle",
    [CSR_MINSTRET] = "minstret",
    [CSR_MCYCLEH] = "mcycleh",
    [CSR_MINSTRETH] = "minstreth",
};

/* Per CSR bit mask of csrXX settable bits (in addition to other constraints) */
static const uint64_t csr_mask[0x1000] = {
    [CSR_MSCRATCH]      = 0xFFFFFFFF,

    [CSR_FFLAGS]        = 0x1F,
    [CSR_FRM]           = 7,
    [CSR_FCSR]          = 0xFF,

    [CSR_MTVEC]         = -4LL,
    [CSR_STVEC]         = -4LL,

    [CSR_MIE]           = 0xFFFFFFFF,
    [CSR_MSTATUS]       = (BF_PACK(~0, CSR_STATUS_MPP_BF)  |
                           BF_PACK(~0, CSR_STATUS_MPIE_BF) |
                           BF_PACK(~0, CSR_STATUS_MIE_BF)),
    [CSR_MEPC]          = -4LL,

    [CSR_MCYCLE]        = 0xFFFFFFFF,
    [CSR_MINSTRET]      = 0xFFFFFFFF,
    [CSR_MCYCLEH]       = 0xFFFFFFFF,
    [CSR_MINSTRETH]     = 0xFFFFFFFF,
};

static const char *reg_name[32] = {
    //  0     1     2     3     4     5     6     7     8     9    10    11
    "0",   "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1",
    // 12    13    14    15    16    17    18    19    20    21    22    23
    "a2",   "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    // 24    25    26    27    28    29    30    31
    "s8",   "s9","s10","s11", "t3", "t4", "t5", "t6"
};

static const char *opcode_name[32] = {
    "LOAD", "LOAD_FP", "CUSTOM0", "MISC_MEM", "OP_IMM", "AUIPC", "OP_IMM_32", "EXT0",
    "STORE", "STORE_FP", "CUSTOM1", "AMO", "OP", "LUI", "OP_32", "EXT1",
    "MADD", "MSUB", "NMSUB", "NMADD", "OP_FP", "RES1", "CUSTOM2", "EXT2",
    "BRANCH", "JALR", "RES0", "JAL", "SYSTEM", "RES2", "CUSTOM3", "EXT3",
};

const char *opcode_imm_name[8] = {
    "addi", "slli", "slti", "sltiu", "xori", "sr_i", "ori", "andi",
};

const char *opcode_load_op_name[8] = {
    "lb", "lh", "lw", "ld", "lbu", "lhu", "lwu", "?",
};

const char *opcode_op_op_name[16] = {
    "add", "sll", "slt", "sltu", "xor", "srl", "or", "and",
    "sub", "?",   "?",   "?",    "?",   "sra", "?", "?",
};

const char *opcode_op_div_name[8] = {
    "mul", "mulh", "mulhsu", "mulhu", "div", "divu", "rem", "remu",
};

const char *opcode_op_branch_name[8] = {
    "beq", "bne", "?", "?", "blt", "bge", "bltu", "bgeu",
};

const char *opcode_amo_name[32] = {
    [AMOADD]    = "amoadd",
    [AMOAND]    = "amoand",
    [AMOMAX]    = "amomax",
    [AMOMAXU]   = "amomaxu",
    [AMOMIN]    = "amomin",
    [AMOMINU]   = "amominu",
    [AMOOR]     = "amoor",
    [AMOSWAP]   = "amoswap",
    [AMOXOR]    = "amoxor",
    [LR]        = "lr",
    [SC]        = "sc",
};

#define MAX_WAYS 4
#define MAX_SETS 4096
#define MAX_CACHE_CONFIGS 0 // (4*3*3*5*10)
enum policy_e { LRU, RANDOM, MRULRU };
static const char * const policy_s[] = {"LRU", "random", "MRULRU"};
enum dirties_e { PR_LINE, PR_BYTE };
static const char * const dirties_s[] = {"prline", "prbyte"};

typedef struct riscv_state_st {
    struct cache_st {
        struct line_st {
            uint32_t tag;
            unsigned last_access; // cycle of access, for eviction decisions
            uint64_t valid_bytes; // Can actually be summarized with a single bit
            uint64_t dirty_bytes;
        } line[MAX_WAYS][MAX_SETS];
        unsigned nways, nsetslg2, linesizelg2;
        uint64_t nhits, nmiss, nfetches, ndirty_evicts, ndirty_evict_bytes;
        enum policy_e policy;
        enum dirties_e dirties;
        uint64_t alllinebytes;
    } cache[MAX_CACHE_CONFIGS];
    int num_caches;
    uint64_t load_count[8+9];
} riscv_state_t;

static void dump_cache_stats(riscv_state_t *);

static const char *
csr_fmt(unsigned csrno)
{
    if (csr_name[csrno])
        return csr_name[csrno];

    static char buf[6];
    snprintf(buf, sizeof buf, "0x%03x", csrno);
    return (const char *)buf;
}

static void
disass_insn(uint64_t pc, uint32_t insn, char *buf, size_t buf_size)
{
    insn_t i = {.raw = insn };
    const char **N = reg_name;

    switch (i.r.opcode) {
    case LOAD:
        snprintf(buf, buf_size, "%-11s%s,%d(%s)",
                 opcode_load_op_name[i.i.funct3], N[i.i.rd], i.i.imm11_0, N[i.i.rs1]);
        break;


    case MISC_MEM:
        snprintf(buf, buf_size, "%s", i.r.funct3 ? "fence.i" : "fence");
        break;

    case LOAD_FP: {
        char *name =
            i.s.funct3 == FLW ? "lsw" :
            i.s.funct3 == FLD ? "lsd" :
            "??? load_fp ???";
        int imm = i.s.imm4_0 | i.s.imm11_5 << 5;
        snprintf(buf, buf_size, "%-11s%s,%d(%s)", name, N[i.s.rs2], imm, N[i.s.rs1]);
        break;
    }

//  case CUSTOM0:
    case OP_IMM:
        if (i.i.funct3 == ADDI && i.i.rs1 == 0 && i.i.rd == 0 && i.i.imm11_0 == 0)
            // nop pseudo instruction
            snprintf(buf, buf_size, "nop");
        else if (i.i.funct3 == ADDI && N[i.i.rs1] == 0)
            // li pseudo instruction
            snprintf(buf, buf_size, "%-11s%s,%d",
                     "li", N[i.i.rd], i.i.imm11_0);
        else if (i.i.funct3 == SR_I) {
            const char *opcode = i.i.imm11_0 & 1 << 10 ? "srai" : "srli";
            snprintf(buf, buf_size, "%-11s%s,%s,%d",
                     opcode, N[i.i.rd], N[i.i.rs1], i.i.imm11_0);
        } else
            snprintf(buf, buf_size, "%-11s%s,%s,%d",
                     opcode_imm_name[i.i.funct3], N[i.i.rd], N[i.i.rs1], i.i.imm11_0);
        break;

    case AUIPC:
        snprintf(buf, buf_size, "%-11s%s,0x%x",
                 "auipc", N[i.u.rd], i.u.imm31_12);
        break;

    case OP_IMM_32: {
        char op[16];
        snprintf(op, sizeof op, "%sw",opcode_imm_name[i.r.funct3]);

        snprintf(buf, buf_size, "%-11s%s,%s,%d", op, N[i.r.rd], N[i.r.rs1], i.i.imm11_0);
        break;
    }

//  case EXT0:
    case STORE: {
        int imm = i.s.imm4_0 | i.s.imm11_5 << 5;
        snprintf(buf, buf_size, "s%-10s%s,%d(%s)",
                 1 + opcode_load_op_name[i.s.funct3], N[i.s.rs2], imm, N[i.s.rs1]);
        break;
    }

    case STORE_FP: {
        char *name =
            i.s.funct3 == FLW ? "fsw" :
            i.s.funct3 == FLD ? "fsd" :
            "??? store_fp ???";
        int imm = i.s.imm4_0 | i.s.imm11_5 << 5;
        snprintf(buf, buf_size, "%-11s%s,%d(%s)", name, N[i.s.rs2], imm, N[i.s.rs1]);
        break;
    }

//  case CUSTOM1:
    case AMO: {
        char insn[16];
        char *suffix = i.r.funct3 & 1 ? ".d" : ".w";
        char *aq = i.r.funct7 & 2 ? ".aq" : "";
        char *rl = i.r.funct7 & 1 ? ".rl" : "";

        switch (i.r.funct7 >> 2) {
        case LR:
            snprintf(insn, sizeof insn, "lr%s%s%s", suffix, aq, rl);
            snprintf(buf, buf_size, "%-11s%s,(%s)", insn,
                     N[i.r.rd], N[i.r.rs1]);
            break;
        case SC:
        case AMOSWAP:
        case AMOADD:
        case AMOXOR:
        case AMOAND:
        case AMOOR:
        case AMOMIN:
        case AMOMAX:
        case AMOMINU:
        case AMOMAXU:
            snprintf(insn, sizeof insn, "%s%s%s%s",
                     opcode_amo_name[i.r.funct7 >> 2], suffix, aq, rl);
            snprintf(buf, buf_size, "%-11s%s,%s,(%s)", insn,
                     N[i.r.rd], N[i.r.rs2], N[i.r.rs1]);
            break;
        }
        }
        break;

    case OP:
        if (i.r.funct7 == 1)
            snprintf(buf, buf_size, "%-11s%s,%s,%s",
                     opcode_op_div_name[i.r.funct3],
                     N[i.r.rd], N[i.r.rs1], N[i.r.rs2]);
        else
            snprintf(buf, buf_size, "%-11s%s,%s,%s",
                     opcode_op_op_name[i.r.funct3 + 8 * (i.i.imm11_0 >> 10 & 1)],
                     N[i.r.rd], N[i.r.rs1], N[i.r.rs2]);
        break;
    case LUI:
        snprintf(buf, buf_size, "%-11s%s,0x%x",
                 "lui", N[i.u.rd], i.u.imm31_12);
        break;

  case OP_32: {
        char op[16];
        snprintf(op, sizeof op, "%sw",
                 i.r.funct7 == 1
                 ? opcode_op_div_name[i.r.funct3]
                 : opcode_op_op_name[i.r.funct3 + 8 * (i.i.imm11_0 >> 10 & 1)]);

        snprintf(buf, buf_size, "%-11s%s,%s,%s", op, N[i.r.rd], N[i.r.rs1], N[i.r.rs2]);
        break;
    }

//  ..
    case BRANCH: {
        int imm =
            i.sb.imm12 << 12 | i.sb.imm11 << 11 |
            i.sb.imm10_5 << 5 | i.sb.imm4_1 << 1;

        snprintf(buf, buf_size, "%-11s%s,%s,0x%08"PRIx32,
                 opcode_op_branch_name[i.r.funct3],
                 N[i.r.rs1], N[i.r.rs2], (uint32_t)(pc + imm));
        break;
    }

    case JALR:
        if (i.i.rd == 0 && i.i.imm11_0 == 0 && i.i.rs1 == 1)
            // ret pseudo instruction
            snprintf(buf, buf_size, "ret");
        else
            snprintf(buf, buf_size, "%-11s%s,%s,%d",
                     "jalr", N[i.i.rd], N[i.i.rs1], i.i.imm11_0);
        break;

//  case RES0:
    case JAL: {
        int imm =
            i.uj.imm20    << 20 |
            i.uj.imm19_12 << 12 |
            i.uj.imm11    << 11 |
            i.uj.imm10_1  << 1;

        // Sign-extend:
        imm = (imm << 11) >> 11;

        uint64_t addr = pc + imm;

        if (i.uj.rd == 0)
            // Pseudo "j" instruction
            snprintf(buf, buf_size, "%-11s0x%08"PRIx32,
                     "j", (uint32_t)addr);
        else if (i.uj.rd == 1)
            // Pseudo "jal" instruction (without destination)
            snprintf(buf, buf_size, "%-11s0x%08"PRIx32,
                     "jal", (uint32_t)addr);
        else
            snprintf(buf, buf_size, "%-11s%s,0x%08"PRIx32,
                     "jal", N[i.uj.rd], (uint32_t)addr);

        break;
    }

  case SYSTEM:
      switch (i.r.funct3) {
      case ECALLEBREAK:
          switch (i.i.imm11_0) {
          case ECALL:  snprintf(buf, buf_size, "%s", "ecall"); return;
          case EBREAK: snprintf(buf, buf_size, "%s", "ebreak"); return;
          case URET:   snprintf(buf, buf_size, "%s", "uret"); return;
          case SRET:   snprintf(buf, buf_size, "%s", "sret"); return;
          case WFI:    snprintf(buf, buf_size, "%s", "wfi"); return;
          case MRET:   snprintf(buf, buf_size, "%s", "mret"); return;
          default:
              goto unhandled;
          }
          break;

      case CSRRS:
          if (i.i.rs1 == 0) {
              switch ((unsigned)i.i.imm11_0) {
              case 0xC00: snprintf(buf, buf_size, "%-11s%s", "rdcycle",   N[i.i.rd]); return;
              case 0xC01: snprintf(buf, buf_size, "%-11s%s", "rdtime",    N[i.i.rd]); return;
              case 0xC02: snprintf(buf, buf_size, "%-11s%s", "rdinstret", N[i.i.rd]); return;
              default:    snprintf(buf, buf_size, "%-11s%s,%s", "csrr",  N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU)); return;
              }
          } else
              snprintf(buf, buf_size, "%-11s%s,%s,%s",  "csrrs", N[i.i.rd], csr_name[i.i.imm11_0 & 0xFFFU], N[i.i.rs1]);
          return;

      case CSRRSI: snprintf(buf, buf_size, "%-11s%s,%s,%d", "csrrsi", N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), i.i.rs1); return;
      case CSRRC:  snprintf(buf, buf_size, "%-11s%s,%s,%s", "csrrc",  N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), N[i.i.rs1]); return;
      case CSRRCI: snprintf(buf, buf_size, "%-11s%s,%s,%d", "csrrci", N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), i.i.rs1); return;
      case CSRRW:  snprintf(buf, buf_size, "%-11s%s,%s,%s", "csrrw",  N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), N[i.i.rs1]); return;
      case CSRRWI: snprintf(buf, buf_size, "%-11s%s,%s,%d", "csrrwi", N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), i.i.rs1); return;

      default:
          goto unhandled;
      }
      break;

    case OP_FP:
        switch (i.r.funct7) {
        case FMV_D_X:
            snprintf(buf, buf_size, "%-11sf%s,f%s",
                     "fmv.d.x", N[i.r.rd], N[i.r.rs1]); break;
        default: goto unhandled;
        }
        break;

//  ...

    default:
unhandled:
        warn("Opcode %s isn't handled, insn 0x%08x\n", opcode_name[i.r.opcode], insn);
        *buf = 0;
        return;
    }
}

static isa_decoded_t
decode(uint64_t insn_addr, uint32_t insn)
{
    insn_t i = { .raw = insn };
    isa_decoded_t dec = { .insn_addr = insn_addr, .insn = insn };

    dec.dest_reg     = ISA_NO_REG;
    dec.source_reg_a = 0;
    dec.source_reg_b = 0;
    dec.dest_msr     = ISA_NO_REG;
    dec.source_msr_a = ISA_NO_REG;
    dec.class        = isa_insn_class_illegal;
    dec.system       = false;

    switch (i.r.opcode) {
    case LOAD:
        dec.class        = isa_insn_class_load;
        dec.loadstore_size = 1 << (i.i.funct3 & 3);
        // LB, LH, [LW in 64-bit] needs sign-extension.
        if (i.i.funct3 < LW)
            dec.loadstore_size = -dec.loadstore_size;
        dec.source_reg_a = i.i.rs1;
        dec.dest_reg     = i.i.rd;
        break;

    case LOAD_FP:
        // XXX Pretend it's a nop
        dec.class        = isa_insn_class_alu;
        break;

    case MISC_MEM:
        dec.class        = isa_insn_class_branch;
        break;

    case OP_IMM:
    case OP_IMM_32:
        dec.source_reg_a = i.i.rs1;
    case AUIPC:
    case LUI:
        dec.dest_reg     = i.i.rd;
        dec.class        = isa_insn_class_alu;
        break;

    case STORE:
        dec.class        = isa_insn_class_store;
        dec.loadstore_size = 1 << (i.s.funct3 & 3);
        dec.source_reg_a = i.s.rs1;
        dec.source_reg_b = i.s.rs2;
        break;

    case STORE_FP:
        dec.class        = isa_insn_class_alu; // nop
        break;

    case AMO: {
        switch (i.r.funct7 >> 2) {
        case LR:
            dec.class        = isa_insn_class_load;
            dec.loadstore_size = i.r.funct3 & 1 ? 8 : -4;
            dec.source_reg_a = i.r.rs1;
            dec.dest_reg     = i.r.rd;
            break;

        case SC:
            dec.class        = isa_insn_class_store;
            dec.loadstore_size = i.r.funct3 & 1 ? 8 : 4;
            dec.source_reg_a = i.r.rs1;
            dec.source_reg_b = i.r.rs2;
            dec.dest_reg     = i.r.rd;
            break;

        case AMOADD:
        case AMOAND:
        case AMOMAX:
        case AMOMAXU:
        case AMOMIN:
        case AMOMINU:
        case AMOOR:
        case AMOSWAP:
        case AMOXOR:
            dec.class          = isa_insn_class_atomic;
            dec.loadstore_size = i.r.funct3 & 1 ? 8 : -4;
            dec.source_reg_a   = i.s.rs1;
            dec.source_reg_b   = i.s.rs2;
            dec.dest_reg       = i.i.rd;
            break;
        };
        break;
    }

    case OP:
    case OP_32:
        /* RV32M */
        if (i.r.funct7 != 1)
            if (!((i.r.funct3 == ADDSUB || i.r.funct3 == SR_) && i.r.funct7 == 0x20 ||
                  i.r.funct7 == 0x00))
                goto unhandled;
        dec.dest_reg     = i.r.rd;
        dec.source_reg_a = i.r.rs1;
        dec.source_reg_b = i.r.rs2;
        dec.class        = isa_insn_class_alu;
        break;

    case BRANCH: {
        int imm =
            i.sb.imm12 << 12 | i.sb.imm11 << 11 |
            i.sb.imm10_5 << 5 | i.sb.imm4_1 << 1;

        dec.class             = isa_insn_class_branch;
        dec.jumpbranch_target = insn_addr + imm;
        dec.source_reg_a      = i.r.rs1;
        dec.source_reg_b      = i.r.rs2;
        break;
    }

    case JALR:
        dec.class        = isa_insn_class_compjump;
        dec.source_reg_a = i.i.rs1;
        dec.dest_reg     = i.i.rd;
        break;

    case JAL: {
        int imm =
            i.uj.imm20    << 20 |
            i.uj.imm19_12 << 12 |
            i.uj.imm11    << 11 |
            i.uj.imm10_1  << 1;

        dec.class        = isa_insn_class_jump;
        dec.dest_reg     = i.uj.rd;
        dec.jumpbranch_target = insn_addr + imm;
        break;
    }

    case SYSTEM:
      dec.system = true;;
      switch (i.r.funct3) {
      case ECALLEBREAK:
          switch (i.i.imm11_0) {
          case ECALL:
          case EBREAK:
          case SRET:
          case MRET:
              // XXX but it has a speculation barrier not accounted for
              dec.class = isa_insn_class_compjump;
              break;
          }
          break;

      case CSRRS:
          if (i.i.rs1 == 0) {
              dec.source_msr_a = 0xFFF & (unsigned) i.i.imm11_0;
              dec.dest_reg     = i.i.rd;
              dec.class        = isa_insn_class_alu;
              break;
          }
          /* Fall-through */

      case CSRRC:
          dec.source_reg_a = i.i.rs1;
      case CSRRSI:
      case CSRRCI:
          dec.source_msr_a = 0xFFF & (unsigned) i.i.imm11_0;
          dec.dest_msr     = 0xFFF & (unsigned) i.i.imm11_0;
          dec.dest_reg     = i.i.rd;
          dec.class        = isa_insn_class_alu;
          break;

      case CSRRW:
          dec.source_reg_a = i.i.rs1;
      case CSRRWI:
          dec.dest_msr     = 0xFFF & (unsigned) i.i.imm11_0;
          dec.dest_reg     = i.i.rd;
          dec.class        = isa_insn_class_alu;

          if (i.i.rd)
              dec.source_msr_a = 0xFFF & (unsigned) i.i.imm11_0;
          break;

      default:
          goto unhandled;
      }
      break;

    case OP_FP:
        switch (i.r.funct7) {
        case FMV_D_X:
            dec.class        = isa_insn_class_alu; // XXX alu_fp?
            //dec.source_freg_a = i.r.rs1;
            //dec.dest_freg     = i.r.rd;
            break;
        default: goto unhandled;
        }
        break;

    default:
    unhandled:
        warn("Opcode %s not decoded, insn %08"PRIx32":%08x\n",
             opcode_name[i.r.opcode], (uint32_t)insn_addr, i.raw);
        break;
    }

    if (dec.dest_reg == 0)
        dec.dest_reg = ISA_NO_REG;

    return dec;
}


static isa_result_t
raise_exception(int cause, uint64_t info, isa_exception_t *exc)
{
    exc->raised = true;
    exc->code = cause;
    exc->info = info;

    isa_result_t res  = { 0 };
    return res;
}


static isa_result_t
insn_exec(int xlen, isa_decoded_t dec, uint64_t op_a_u, uint64_t op_b_u,
          uint64_t msr_a, isa_exception_t *exc)
{
    int64_t op_a      = (int64_t) op_a_u;
    int64_t op_b      = (int64_t) op_b_u;
    int32_t op_a_32   = (int32_t) op_a;
    int32_t op_b_32   = (int32_t) op_b;
    uint32_t op_a_u_32= (uint32_t) op_a_u;
    uint32_t op_b_u_32= (uint32_t) op_b_u;
    insn_t i          = { .raw = dec.insn };
    isa_result_t res  = { 0 };
    uint64_t ea_load  = op_a + i.i.imm11_0;
    uint64_t ea_store = op_a + (i.s.imm11_5 << 5 | i.s.imm4_0);

    switch (i.r.opcode) {
    case LOAD:
        res.load_addr = ea_load;
        return res;

    case LOAD_FP:
        return res;

    case OP_IMM_32:
        switch (i.i.funct3) {
        case ADDI: // ADDIW
            res.result = op_a_32 + i.i.imm11_0;
            break;
        case SLLI: // SLLIW
            res.result = op_a_32 << (i.i.imm11_0 & (xlen - 1));
            break;
        case SR_I: // SRLIW/SRAIW
            if (i.i.imm11_0 & 1 << 10)
                res.result = op_a_32 >> (i.i.imm11_0 & (xlen - 1));
            else
                res.result = op_a_u_32 >> (i.i.imm11_0 & (xlen - 1));
            res.result = (int32_t)res.result;
            break;
        default:
            assert(0);
        }
        return res;

    case OP_IMM:
        switch (i.i.funct3) {
        case ADDI:
            res.result = op_a + i.i.imm11_0;
            break;
        case SLLI:
            res.result = op_a << (i.i.imm11_0 & (xlen - 1));
            break;
        case SLTI:
            res.result = op_a < i.i.imm11_0;
            break;
        case SLTIU:
            res.result = op_a_u < (uint64_t) (int64_t) i.i.imm11_0;
            break;
        case XORI:
            res.result = op_a ^ i.i.imm11_0;
            break;
        case SR_I:
            if (i.i.imm11_0 & 1 << 10)
                res.result = op_a >> (i.i.imm11_0 & (xlen - 1));
            else
                res.result = (uint32_t) op_a_u >> (i.i.imm11_0 & (xlen - 1)); // XXX RV32
            break;
        case ORI:
            res.result = op_a | i.i.imm11_0;
            break;
        case ANDI:
            res.result = op_a & i.i.imm11_0;
            break;
        default:
            assert(0);
        }
        return res;

    case AUIPC:
        res.result = dec.insn_addr + (i.u.imm31_12 << 12);
        return res;

    case STORE:
        res.store_value = op_b;
        res.store_addr = ea_store;
        return res;

    case STORE_FP:
        return res;

    case AMO: {
        switch (i.r.funct7 >> 2) {
        case LR:
            res.load_addr = op_a;
            break;

        case SC:
            res.store_value = op_b;
            res.store_addr = op_a;
            res.result = 0; // 0 = success
            break;

        case AMOADD:    res.result = op_a + op_b; break;
        case AMOAND:    res.result = op_a & op_b; break;
        case AMOMAX:    res.result = op_a   < op_b   ? op_b : op_a; break;
        case AMOMAXU:   res.result = op_a_u < op_b_u ? op_b : op_a; break;
        case AMOMIN:    res.result = op_a   > op_b   ? op_b : op_a; break;
        case AMOMINU:   res.result = op_a_u > op_b_u ? op_b : op_a; break;
        case AMOOR:     res.result = op_a | op_b; break;
        case AMOSWAP:   res.result = op_b; break;
        case AMOXOR:    res.result = op_a ^ op_b; break;
        }

        return res;
    }

    case OP:
        if (i.r.funct7 == 1)
            switch (i.r.funct3) {
            case MUL:
                res.result = op_a * op_b;
                return res;
            case MULH:
                res.result = (op_a * op_b) >> 32; // XXX RV32
                return res;
            case MULHSU:
                res.result = (op_a * op_b_u) >> 32;
                return res;
            case MULHU:
                res.result = (op_a_u * op_b_u) >> 32;
                return res;
            case DIV:
                if (op_b == 0)
                    res.result = -1LL;
                else if (op_b == -1 && op_a == (1LL << (xlen - 1)))
                    res.result = -1LL << (xlen - 1);
                else
                    res.result = op_a / (int32_t) op_b;
                return res;
            case DIVU:
                if (op_b == 0)
                    res.result = -1LL;
                else
                    res.result = op_a_u / (uint32_t) op_b_u;
                return res;
            case REM:
                if (op_b == 0)
                    res.result = op_a;
                else if (op_b == -1 && op_a == (1LL << (xlen - 1)))
                    res.result = 0;
                else
                    res.result = op_a % (int32_t) op_b;
                return res;
            case REMU:
                if (op_b == 0)
                    res.result = op_a;
                else
                    res.result = op_a_u % (uint32_t) op_b_u;
                return res;
            }
        else
            switch (i.r.funct3) {
            case ADDSUB:
                res.result = i.i.imm11_0 >> 10 & 1 ? op_a - op_b : op_a + op_b;
                break;
            case SLL:
                res.result = op_a << (op_b & (xlen - 1));
                break;
            case SLT:
                res.result = op_a < op_b;
                break;
            case SLTU:
                res.result = op_a_u < op_b_u;
                break;
            case XOR:
                res.result = op_a ^ op_b;
                break;
            case SR_:
                if (i.i.imm11_0 & 1 << 10)
                    res.result = op_a >> (op_b & (xlen - 1));
                else
                    res.result = (uint32_t) op_a_u >> (op_b & (xlen - 1)); // XXX RV32
                break;
            case OR:
                res.result = op_a | op_b;
                break;
            case AND:
                res.result = op_a & op_b;
                break;
            default:
                assert(0);
            }
        return res;

    case OP_32:
        if (i.r.funct7 == 1)
            switch (i.r.funct3) {
            case MUL: // MULW
                res.result = op_a_32 * op_b_32;
                return res;
            case DIV: // DIVW
                if (op_b_32 == 0)
                    res.result = -1LL;
                else if (op_b_32 == -1 && op_a_32 == (1 << 31))
                    res.result = 0xffffffff80000000ULL; // -1LL << 31 is undefined, sigh
                else
                    res.result = op_a_32 / op_b_32;
                return res;
            case DIVU: // DIVW
                if (op_b_32 == 0)
                    res.result = -1LL;
                else
                    res.result = (int32_t) (op_a_u_32 / op_b_u_32);
                return res;
            case REM: // REMW
                if (op_b_32 == 0)
                    res.result = op_a_32;
                else if (op_b_32 == -1 && op_a_32 == (1 << 31))
                    res.result = 0;
                else
                    res.result = (int32_t) (op_a_32 % op_b_32);
                return res;
            case REMU: // REMUW
                if (op_b_32 == 0)
                    res.result = op_a_32;
                else
                    res.result = (int32_t) (op_a_u % op_b_u);
                return res;
            }
        else
            switch (i.r.funct3) {
            case ADDSUB: // ADDSUBW
                res.result = i.i.imm11_0 >> 10 & 1 ? op_a_32 - op_b_32 : op_a_32 + op_b_32;
                break;
            case SLL:
                res.result = op_a_32 << (op_b & 31);
                break;
            case SR_:
                if (i.i.imm11_0 & 1 << 10)
                    res.result = op_a_32 >> (op_b & 31);
                else
                    res.result = op_a_u_32 >> (op_b & 31);
                res.result = (int32_t)res.result;
                break;
            default:
                assert(0);
            }
        return res;

    case LUI:
        res.result = i.u.imm31_12 << 12;
        return res;

    case MISC_MEM:
        break;

    case BRANCH:
        switch (i.sb.funct3) {
        case BEQ:
        case BNE:
            res.branch_taken = (i.sb.funct3 & 1) ^ (op_a == op_b);
            break;
        case BLT:
        case BGE:
            res.branch_taken = (i.sb.funct3 & 1) ^ (op_a < op_b);
            break;
        case BLTU:
        case BGEU:
            res.branch_taken = (i.sb.funct3 & 1) ^ (op_a_u < op_b_u);
            break;
        default:
            assert(0);
        }

        if (res.branch_taken && dec.jumpbranch_target & 3)
            return raise_exception(EXCP_INSN_MISALIGN, dec.jumpbranch_target, exc);

        return res;

    case JALR:
        res.result = dec.insn_addr + 4;
        res.compjump_target = (op_a + i.i.imm11_0) & -2LL;

        if (res.compjump_target & 3)
            return raise_exception(EXCP_INSN_MISALIGN, res.compjump_target, exc);

        return res;

    case JAL:
        res.result = dec.insn_addr + 4;

        if (dec.jumpbranch_target & 3)
            return raise_exception(EXCP_INSN_MISALIGN, dec.jumpbranch_target, exc);

        return res;

    default:
        warn("Opcode %s exec not implemented, insn %08"PRIx32":%08x "
             "i{%x,%s,%x,%s,%s,%x}\n",
             opcode_name[i.r.opcode], (uint32_t)dec.insn_addr, i.raw,
             i.i.imm11_0, reg_name[i.r.rs1], i.r.funct3,
             reg_name[i.r.rd],
             opcode_name[i.r.opcode], i.r.opext);
        return raise_exception(EXCP_INSN_ILLEGAL, 0, exc);
    }

    res.result = -1;
    return res;
}


static isa_result_t
insn_exec_system(cpu_state_t *s, isa_decoded_t dec, uint64_t op_a_u, uint64_t op_b_u,
                 uint64_t msr_a, isa_exception_t *exc)
{
    int64_t op_a      = (int64_t) op_a_u;
    insn_t i          = { .raw = dec.insn };
    isa_result_t res  = { 0 };

    switch (i.r.opcode) {
    case SYSTEM:
        res.result = msr_a; // XXX is this needed?

        switch (i.r.funct3) {
        case ECALLEBREAK:
            switch (i.i.imm11_0) {
            case ECALL: return raise_exception(EXCP_UMODE_CALL + s->priv, 0, exc);
            case EBREAK:return raise_exception(EXCP_BREAKPOINT, 0, exc);
            case MRET:
                /* Copy down MPIE to MIE and set MPIE */
                BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_MIE_BF,
                       BF_GET(s->msr[CSR_MSTATUS], CSR_STATUS_MPIE_BF));
                BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_MPIE_BF, ~0);

                s->priv = BF_GET(s->msr[CSR_MSTATUS], CSR_STATUS_MPP_BF);
                BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_MPP_BF, 0);
                res.compjump_target = s->msr[CSR_MEPC];

                if (res.compjump_target & 3)
                    return raise_exception(EXCP_INSN_MISALIGN, res.compjump_target, exc);

                return res;

            case SRET:
                /* Copy down SPIE to SIE and set SPIE */
                BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_SPIE_BF,
                       BF_GET(s->msr[CSR_MSTATUS], CSR_STATUS_SPIE_BF));
                BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_SPIE_BF, ~0);

                s->priv = BF_GET(s->msr[CSR_MSTATUS], CSR_STATUS_SPP_BF);
                BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_SPP_BF, 0);
                res.compjump_target = s->msr[CSR_SEPC];

                if (res.compjump_target & 3)
                    return raise_exception(EXCP_INSN_MISALIGN, res.compjump_target, exc);

                return res;

            case WFI:
                /*
                 * Wait for interrupts.  Is semantically equivalent to
                 * a branch backwards to same OR nop, dealers choice.
                 */
                break;

            default:
                fprintf(stderr, "Unhandled SYSTEM/ECALLEBREAK instruction %08x (%d)\n",
                        i.raw, i.i.imm11_0);
                dump_cache_stats((riscv_state_t *)s->arch_specific);
                fflush(stdout);
                assert(0);
                break;
            }

        case CSRRS:  res.msr_result = msr_a |  op_a;    return res;
        case CSRRC:  res.msr_result = msr_a &~ op_a;    return res;
        case CSRRW:  res.msr_result =          op_a;    return res;
        case CSRRSI: res.msr_result = msr_a |  i.i.rs1; return res;
        case CSRRCI: res.msr_result = msr_a &~ i.i.rs1; return res;
        case CSRRWI: res.msr_result =          i.i.rs1; return res;

        default:
            assert(0);
        }
        break;

    default:
        warn("Opcode %s exec not implemented, insn %08"PRIx32":%08x "
             "i{%x,%s,%x,%s,%s,%x}\n",
             opcode_name[i.r.opcode], (uint32_t)dec.insn_addr, i.raw,
             i.i.imm11_0, reg_name[i.r.rs1], i.r.funct3,
             reg_name[i.r.rd],
             opcode_name[i.r.opcode], i.r.opext);
        return raise_exception(EXCP_INSN_ILLEGAL, 0, exc);
    }

    res.result = -1;
    return res;
}

static uint64_t
handle_exception(cpu_state_t *s, uint64_t insn_addr, isa_exception_t exc)
{
    s->msr[CSR_MEPC]   = insn_addr;
    s->msr[CSR_MCAUSE] = exc.code;
    s->msr[CSR_MTVAL]  = exc.info;

    /* When a trap is taken from privilege mode y into privilege
       mode x, xPIE is set to the value of xIE; xIE is set to 0;
       and xPP is set to y.

       Here x = M, thus MPIE = MIE; MIE = 0; MPP = priv
    */

    BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_MPIE_BF,
           BF_GET(s->msr[CSR_MSTATUS], CSR_STATUS_MIE_BF));
    BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_MIE_BF, 0);
    BF_SET(s->msr[CSR_MSTATUS], CSR_STATUS_MPP_BF, s->priv);
    s->priv = 3;
    return s->msr[CSR_MTVEC];
}

static isa_result_t
insn_exec32(isa_decoded_t dec, uint64_t op_a_u, uint64_t op_b_u, uint64_t msr_a, isa_exception_t *exc)
{
    return insn_exec(32, dec, op_a_u, op_b_u, msr_a, exc);
}

static isa_result_t
insn_exec64(isa_decoded_t dec, uint64_t op_a_u, uint64_t op_b_u, uint64_t msr_a, isa_exception_t *exc)
{
    return insn_exec(64, dec, op_a_u, op_b_u, msr_a, exc);
}

#define HTIF_DEV_SHIFT      (56)

static void check_for_interrupts(cpu_state_t *s) {
    bool intr_globally_enabled = s->priv < 3 || BF_GET(s->msr[CSR_MSTATUS], CSR_STATUS_MIE_BF);
    uint32_t pending = s->msr[CSR_MIP] & s->msr[CSR_MIE];

    if (pending != 0 && intr_globally_enabled) {
        isa_exception_t exc = {
            .code = (1ULL << 31) | __builtin_ctz(pending),
            .info = 0 }; // XXX 0?

         // XXX this is broken; we need to have a cleaner way to
         // servicing interrupts
        s->pc = handle_exception(s, s->pc, exc);}}


/* executed every cycle */
static void tick(cpu_state_t *s, int instret)
{
    s->counter += 1;
    // XXX for now, an instruction per tick
    s->msr[CSR_MINSTRET] = (uint32_t) (s->msr[CSR_MINSTRET] + instret);
    s->msr[CSR_MCYCLE]   = (uint32_t) (s->msr[CSR_MCYCLE]   + 1);

    // mtime on QEMU runs at 10 MHz.  Let's pretend our processors runs at 1 MHz
    if ((s->counter & 0) == 0) {
        s->mtimereg[0] += 10;
        if (s->mtimereg[0] >= s->mtimereg[1])
            s->msr[CSR_MIP] |= MIP_MTIP;}

    check_for_interrupts(s);}

static uint64_t read_msr(cpu_state_t *s, unsigned csrno, isa_exception_t *exc)
{
    if (s->priv < ((csrno >> 8) & 3)) {
        ERROR("  Illegal Read of CSR %3x\n", csrno);
        raise_exception(EXCP_INSN_ILLEGAL, 0, exc); // XXX is that legal?
        return 0;
    }

    switch (csrno) {
    case CSR_TIME: {
     struct timeval tv;
     gettimeofday(&tv, NULL);
     uint64_t now = tv.tv_sec * 1000000LL + tv.tv_usec;
     INFO("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], now);
     return now;
    }

    case CSR_CYCLE:
        INFO("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], s->msr[csrno]);
        return s->msr[csrno];

    default:
        DEBUG("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], s->msr[csrno]);
        return s->msr[csrno];
    }
}

static void write_msr(cpu_state_t *s, unsigned csrno, uint64_t value, isa_exception_t *exc)
{
    if (s->priv < ((csrno >> 8) & 3) || (csrno & 0xc00) == 0xc00) {
        ERROR("  Illegal Write of CSR %3x\n", csrno);
        raise_exception(EXCP_INSN_ILLEGAL, 0, exc); // XXX is that legal?
        return;
    }

    s->msr[csrno] = value & csr_mask[csrno] | s->msr[csrno] & ~csr_mask[csrno];

    DEBUG("  Write CSR %s <- %"PRIx64"\n", csr_name[csrno], s->msr[csrno]);

    if (csrno == CSR_CYCLE)
        ERROR("  Write CSR %s <- %"PRIx64"\n", csr_name[csrno], s->msr[csrno]);

    if (value & ~s->msr[csrno])
        DEBUG("    NB: bits %"PRIx64" are masked off\n", value & ~s->msr[csrno]);

    switch (csrno) {
    default:
        break;

    case CSR_FFLAGS:
        /* FS->MSR[7:0] <---> {FRM[3:0], FFLAGS[4:0]} */
        BF_SET(s->msr[CSR_FCSR], 4:0, BF_GET(s->msr[CSR_FFLAGS], 4:0));
        break;

    case CSR_FRM:
        /* FS->MSR[7:0] <---> {FRM[2:0], FFLAGS[4:0]} */
        BF_SET(s->msr[CSR_FCSR], 7:5, BF_GET(s->msr[CSR_FRM], 2:0));
        break;

    case CSR_FCSR:
        /* FS->MSR[7:0] <---> {FRM[3:0], FFLAGS[4:0]} */
        BF_SET(s->msr[CSR_FFLAGS], 4:0, BF_GET(s->msr[CSR_FCSR], 4:0));
        BF_SET(s->msr[CSR_FRM],    2:0, BF_GET(s->msr[CSR_FCSR], 7:5));
        break;
    }
}

static void make_cache(riscv_state_t *s,
                       unsigned nways, unsigned nsetslg2, unsigned linesizelg2,
                       enum policy_e policy, enum dirties_e dirties)
{
    if (s->num_caches >= MAX_CACHE_CONFIGS)
        return;

    struct cache_st *c = s->cache + s->num_caches;
    c->nways = nways;
    c->nsetslg2 = nsetslg2;
    c->linesizelg2 = linesizelg2;
    c->policy = policy;
    c->dirties = dirties;
    unsigned size = c->nways << (c->nsetslg2 + c->linesizelg2);
    c->alllinebytes = (1LL << (1LL << c->linesizelg2)) - 1;
    if (c->alllinebytes == 0) // Sigh, 1 << 64 doesn't make 0
        c->alllinebytes = ~0ULL;

    if (8192 <= size && size <= 16384) {
        if (0)
            fprintf(stderr, "Cache Creation: config #%d: <%d,%d,%d>, %d KiB %6s %s\n",
                    s->num_caches,
                    nways, 1 << nsetslg2, 1 << linesizelg2,
                    (c->nways << (c->nsetslg2 + c->linesizelg2)) / 1024,
                    policy_s[policy], dirties_s[dirties]);
        ++s->num_caches;
    }
}

static void dump_cache_stats(riscv_state_t *s)
{
    const int fetchoverhead = 24; // XXX Guess

    for (int i = 0; i < s->num_caches; ++i) {
        struct cache_st *c = s->cache + i;

        fprintf(stderr,
                "Cache %3d %5d KiB %d ways %4d sets %3d Bline %6s %s %8"PRId64" "
                "hits, %8"PRId64" misses, %8"PRId64" Bfetches, %8"PRId64" (%8"PRId64" B) WB\n",
                i, c->nways << (c->nsetslg2 + c->linesizelg2), c->nways, 1 << c->nsetslg2, 1 << c->linesizelg2,
                policy_s[c->policy], dirties_s[c->dirties],
                c->nhits, c->nmiss, c->nfetches * (fetchoverhead + (1 << c->linesizelg2)), c->ndirty_evicts, c->ndirty_evict_bytes);
    }

    if (0)
    for (int i = 0; i < 8+9; ++i)
        if (s->load_count[i])
            fprintf(stderr, "L%2d %8"PRId64"\n", i-8, s->load_count[i]);

    fflush(stderr);
}

static void cache_sim(cpu_state_t *cpu, uint64_t address, bool isStore, int mem_access_size)
{
    riscv_state_t *s = cpu->arch_specific;

    if (debug_cache)
    fprintf(stderr, "%s ACCESS address %08"PRIx64"..%08"PRIx64"\n", isStore ? "ST" : "LD",
           address, address + mem_access_size - 1);

    for (int i = 0; i < s->num_caches; ++i) {
        struct cache_st *c = s->cache + i;

        unsigned cache_index = address                >> c->linesizelg2;
        unsigned line_offset = address - (cache_index << c->linesizelg2);
        unsigned cache_tag   = cache_index              >> c->nsetslg2;
        unsigned cache_set   = cache_index - (cache_tag << c->nsetslg2);
        uint64_t bytes_mask = ((1LL << mem_access_size) - 1) << line_offset;
        int w;

        if (debug_cache)
            fprintf(stderr, " Cache #%3d <%6x,%3x,%2x> ", i, cache_tag, cache_set, line_offset);

        for (w = 0; w < c->nways; ++w)
            if (c->line[w][cache_set].tag == cache_tag)
                break;

        if (w < c->nways) {
            if (isStore && c->dirties == PR_BYTE ||
                (c->line[w][cache_set].valid_bytes & bytes_mask) == bytes_mask) {

                if (debug_cache)
                    fprintf(stderr, "HIT way %d  (line V %"PRIx64", D %"PRIx64")\n",
                            w,
                            c->line[w][cache_set].valid_bytes,
                            c->line[w][cache_set].dirty_bytes);

                c->nhits += 1;
                c->line[w][cache_set].last_access = cpu->counter;

                if (isStore)
                    switch (c->dirties) {
                    case PR_LINE:
                        c->line[w][cache_set].dirty_bytes = c->alllinebytes;
                        break;
                    case PR_BYTE:
                        c->line[w][cache_set].valid_bytes |= bytes_mask;
                        c->line[w][cache_set].dirty_bytes |= bytes_mask;
                        break;
                    }

                continue;
            }
        }

        /* We have a miss, so evict something (assume cache is full) */
        c->nmiss += 1;
        if (c->policy == RANDOM)
            w = random() % c->nways;
        else {
            w = 0;
            for (int j = 1; j < c->nways; ++j)
                if (c->line[j][cache_set].last_access < c->line[w][cache_set].last_access)
                    w = j;
        }

        if (debug_cache)
            fprintf(stderr, "MISS evict way %d  (line V %"PRIx64", D %"PRIx64")\n",
                    w,
                    c->line[w][cache_set].valid_bytes,
                    c->line[w][cache_set].dirty_bytes);

        if (c->line[w][cache_set].dirty_bytes) {
            c->ndirty_evicts += 1;
            c->ndirty_evict_bytes += __builtin_popcount(c->line[w][cache_set].dirty_bytes);
            c->line[w][cache_set].dirty_bytes = 0;
        }

        c->line[w][cache_set].tag = cache_tag;
        switch (c->policy) {
        case MRULRU:
            c->line[w][cache_set].last_access = 0;
            break;
        case LRU:
        case RANDOM:
            c->line[w][cache_set].last_access = cpu->counter;
            break;
        }

        switch (c->dirties) {
        case PR_BYTE:
            if (isStore) {
                c->line[w][cache_set].valid_bytes = bytes_mask;
                c->line[w][cache_set].dirty_bytes = bytes_mask;
            } else {
                c->nfetches += 1;
                c->line[w][cache_set].valid_bytes = c->alllinebytes;
                c->line[w][cache_set].dirty_bytes = 0;
            }
            break;
        case PR_LINE:
            c->nfetches += 1;
            c->line[w][cache_set].valid_bytes = c->alllinebytes;
            if (isStore)
                c->line[w][cache_set].dirty_bytes = c->alllinebytes;
            break;
        }
    }
}

static uint64_t
load(cpu_state_t *s, uint64_t address, int mem_access_size, isa_exception_t *exc)
{
    bool ifetch = false;
    if (mem_access_size == 0)
        mem_access_size = 4, ifetch = true;

    if (address & (abs(mem_access_size) - 1)) {
        ERROR("  load from unaligned physical address %08"PRIx32"\n", (uint32_t)address);
        raise_exception(EXCP_LOAD_MISALIGN, address, exc);
        return 0;
    }

    void *p = 0;

    /* QEMU */
    if ((address & 0xF0000000) == 0x40000000) {
        address &= 0xFFFFFFF;
        if (address < 0x10)
            p = ((void *) &s->mtimereg[0]) + address;
/*
        else {
             if (0x2000 <= address && address < 0x3000) { // XXX QEMU UARTS goes how far?

            p = ((void *) &s->mtimereg[0]) + address;
        }
*/
    } else if (address == 0x70001010) {    // Hack for Mi-V
        uint32_t iodata = 1;
        p = (void*)&iodata + (address & 3);
    } else if (0 && address & 1 << 31) {
        /* We follow Altera's JTAG UART interface:

           The core has two registers, data (addr 0) and control (addr 1):

           data    (R/W): RAVAIL:16        RVALID:1 RSERV:7          DATA:8
           control (R/W): WSPACE:16        RSERV:5 AC:1 WI:1 RI:1    RSERV:6 WE:1 RE:1
        */
        uint32_t iodata = 0;
        if ((address & 4) == 0) {
            int ch = getchar();
            iodata = (0 <= ch) * (1 << 16 | 1 << 15) + (uint8_t) ch;
        }
        else
            iodata = 1 << 16;

        p = (void *)&iodata + (address & 3);
    } else
        p = memory_physical(s->mem, address, abs(mem_access_size));

    if (!p) {
        ERROR("  load from illegal physical address %08"PRIx32"\n", (uint32_t)address);
        raise_exception(EXCP_LOAD_ACCESS_FAULT, address, exc);
        return 0;
    }

    if (ifetch)
        cache_sim(s, address, 0, abs(mem_access_size));

    if (!ifetch)
        ((riscv_state_t *)s->arch_specific)->load_count[mem_access_size + 8] += 1;

    switch (mem_access_size) {
    case -1: return *( int8_t  *)p;
    case  1: return *(uint8_t  *)p;
    case -2: return *( int16_t *)p;
    case  2: return *(uint16_t *)p;
    case -4: return *( int32_t *)p;
    case  4: return *(uint32_t *)p;
    case -8:
    case  8: return *(uint64_t *)p;
    default: assert(mem_access_size == mem_access_size + 1);
    }

    return 0;
}

static void
store(cpu_state_t *s, uint64_t address, uint64_t value, int mem_access_size, isa_exception_t *exc)
{
    if (address & (mem_access_size - 1)) {
        ERROR("  store to unaligned physical address %08"PRIx32"\n", (uint32_t)address);
        raise_exception(EXCP_STORE_MISALIGN, address, exc);
        return;
    }

    void *p = 0;
    /* QEMU */
    if ((address & 0xF0000000) == 0x40000000) {
        address &= 0xFFFFFFF;
        if (address < 0x10) {
            //fprintf(stderr, "Writing mtimereg %x <- %x\n", (uint32_t)address, (uint32_t)value);
            p = ((void *) &s->mtimereg[0]) + address;
            s->msr[CSR_MIP] &= ~MIP_MTIP; } // writing the mtime clear the interrup pendning bit
        else if (address == 0x2000) {
            if (s->verbosity & VERBOSE_CONSOLE)
                putchar(value & 255);
            return;
        }
    } else if (address == 0x0000000010000000) {
        // XXX Hack for Dhrystone
        if (s->verbosity & VERBOSE_CONSOLE)
            putchar(value & 255);
        return;
    } else if (address == 0x70001000) {
        // Hack for Mi-V
        if (s->verbosity & VERBOSE_CONSOLE)
            putchar(value & 255);
        return;
    } else
        p = memory_physical(s->mem, address, mem_access_size);

    if (!p) {
        ERROR("  store to illegal physical memory %08"PRIx32"\n", (uint32_t)address);
        raise_exception(EXCP_STORE_ACCESS_FAULT, address, exc);
        return;
    }

    //cache_sim(s, address, 1, mem_access_size);

    switch (mem_access_size) {
    case 1: *(uint8_t  *)p = value; return;
    case 2: *(uint16_t *)p = value; return;
    case 4: *(uint32_t *)p = value; return;
    case 8: *(uint64_t *)p = value; return;
    default:
        assert(mem_access_size > 0);
        assert(mem_access_size == mem_access_size + 1);
    }
}

static void
setup(cpu_state_t *state, elf_info_t *info, verbosity_t verbosity)
{
    riscv_state_t *s = calloc(1, sizeof (riscv_state_t));
    state->arch_specific = s;
    state->verbosity = verbosity;
    memset(state->r, 0, sizeof state->r);
    state->pc = info->program_entry;
    if (!info->is_64bit)
        state->pc = (int32_t)state->pc;

    state->priv = 3; // M

    // This is all the memory we have, code and data
    memory_ensure_mapped_range(state->mem,
                               0x80000000, 0x80000000 + 32*1024-1);

    state->r[2] = 0x80000000 + 32*1024-4;

    if (0)
    for (unsigned setslg2 = 5; setslg2 <= 12; setslg2 += 1)
        for (unsigned ways = 1; ways <= 4; ++ways)
            for (unsigned linesizelg2 = 4; linesizelg2 <= 6; ++linesizelg2)
                for (enum dirties_e d = PR_LINE; d <= PR_BYTE; ++d)
                {
                    make_cache(s, ways, setslg2, linesizelg2, LRU, d);
                    make_cache(s, ways, setslg2, linesizelg2, RANDOM, d);
                    make_cache(s, ways, setslg2, linesizelg2, MRULRU, d);
                }

    for (unsigned setslg2 = 5; setslg2 <= 18; setslg2 += 1)
        for (unsigned ways = 1; ways <= 1; ++ways)
            for (unsigned linesizelg2 = 4; linesizelg2 <= 6; ++linesizelg2)
                for (enum dirties_e d = PR_LINE; d <= PR_LINE; ++d)
                    make_cache(s, ways, setslg2, linesizelg2, RANDOM, d);
}

#if 1
const arch_t arch_riscv32 = {
    .zero_reg = 0,
    .reg_name = reg_name,
    .is_64bit = false,
    .setup = setup,
    .decode = decode,
    .insn_exec = insn_exec32,
    .insn_exec_system = insn_exec_system,
    .disass_insn = disass_insn,
    .tick = tick,
    .read_msr = read_msr,
    .write_msr = write_msr,
    .load = load,
    .store = store,
    .handle_exception = handle_exception,
};
#endif

const arch_t arch_riscv64 = {
    .zero_reg = 0,
    .reg_name = reg_name,
    .is_64bit = true,
    .setup = setup,
    .decode = decode,
    .insn_exec = insn_exec64,
    .insn_exec_system = insn_exec_system,
    .disass_insn = disass_insn,
    .tick = tick,
    .read_msr = read_msr,
    .write_msr = write_msr,
    .load = load,
    .store = store,
};

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
