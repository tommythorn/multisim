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

// 100 MHz -> 1 / 100e6 = 1e-8 s = 1e-2 us
const double us_per_cycle = 1e-2;

static int debug   = 0;
static int info    = 0;
static int error   = 1;

#define DEBUG(...)   ({ if (debug)  fprintf(stderr, __VA_ARGS__); })
#define INFO(...)    ({ if (info)   fprintf(stderr, __VA_ARGS__); })
#define IOINFO(...)  ({ if (ioinfo) fprintf(stderr, __VA_ARGS__); })
#define ERROR(...)   ({ if (error)  fprintf(stderr, __VA_ARGS__); })

verbosity_t verbosity_override = 0;

int leaf_allocations = 0;
int previous_stack_delta = 0;
int max_leaf_allocation = 0;

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
    [CSR_FFLAGS]        = 0x1F,
    [CSR_FRM]           = 7,
    [CSR_FCSR]          = 0xFF,

    [CSR_MTVEC]         = 0xFFFFFFFC,
    [CSR_STVEC]         = 0xFFFFFFFC,

    [CSR_MIE]           = 0xFFFFFFFF,
};

static uint64_t csr[0x1000];

static unsigned priv = 3; // Current priviledge level M=3, S=1, U=0

#define HTIF_CMD_READ       (0x00UL)
#define HTIF_CMD_WRITE      (0x01UL)
#define HTIF_CMD_IDENTITY   (0xFFUL)

const uint64_t memory_start = 0x00000000;
const uint64_t memory_size  = 128 * 1024;

const char *reg_name[32] = {
    //  0     1     2     3     4     5     6     7     8     9    10    11
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1",
    // 12    13    14    15    16    17    18    19    20    21    22    23
    "a2",   "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    // 24    25    26    27    28    29    30    31
    "s8",   "s9","s10","s11", "t3", "t4", "t5", "t6"
};

const char *opcode_name[32] = {
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

/*
static const char *status_field_name[] = {
    "S",
    "PS",
    "EI",
    "PEI",
    "EF",
    "U64",
    "S64",
    "VM",
    "",
    "IM",
    "IP",
};

static int status_field_size[] = {
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    8,
    8,
    8,
    0,
};

static void print_status(uint32_t status)
{
    for (int i = 0; status_field_size[i];
         ++i, status >>= status_field_size[i]) {

        if (!status_field_name[i][0])
            continue;

        int v = status & ((1 << status_field_size[i]) - 1);

        if (status_field_size[i] == 1) {
            if (v)
                printf(" %s", status_field_name[i]);
        } else
            printf(" %s=0x%x", status_field_name[i], v);
    }
}
*/

int disk_fd;

void set_interrupt(int irq, int val)
{
    if (val)
        csr[CSR_MIP] |= 1U << (24+irq);
    else
        csr[CSR_MIP] &= ~(1U << (24+irq));
}

static void push_priv(void)
{
    /* xPIE holds the value of the interrupt-enable bit active prior to
     * the trap, and xPP holds the previous privilege mode.  The xPP
     * fields can only hold privilege modes up to x, so MPP is two bits
     * wide, SPP is one bit wide, and UPP is implicitly zero.  When a trap
     * is taken from privilege mode y into privilege mode x, xPIE is set
     * to the value of xIE; xIE is set to 0; and xPP is set to y. */

#if 0 // TBD
    /* Save S in PS, and set S */
    BF_SET(csr[CSR_STATUS], CSR_STATUS_PS_BF,
           BF_GET(csr[CSR_STATUS], CSR_STATUS_S_BF));
    csr[CSR_STATUS] |= BF_PACK(1, CSR_STATUS_S_BF);

    /* Save EI in PEI and clear EI */
    BF_SET(csr[CSR_STATUS], CSR_STATUS_PEI_BF,
           BF_GET(csr[CSR_STATUS], CSR_STATUS_EI_BF));
    csr[CSR_STATUS] &= ~BF_PACK(1, CSR_STATUS_EI_BF);
#endif

    priv = 3;
}

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
disass_inst(uint64_t pc, uint32_t inst, char *buf, size_t buf_size)
{
    inst_t i = {.raw = inst };
    const char **N = reg_name;

    snprintf(buf, buf_size, "%08x ", inst);
    buf += 9;
    buf_size -= 9;

    switch (i.r.opcode) {
    case LOAD:
        snprintf(buf, buf_size, "%-11s%s,%d(%s)",
                 opcode_load_op_name[i.i.funct3], N[i.i.rd], i.i.imm11_0, N[i.i.rs1]);
        break;


    case MISC_MEM:
        snprintf(buf, buf_size, "%-11s", i.r.funct3 ? "fence.i" : "fence");
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
        if (i.i.funct3 == ADDI && N[i.i.rs1] == 0)
            // li pseudo instruction
            snprintf(buf, buf_size, "%-11s%s,%d",
                     "li", N[i.i.rd], i.i.imm11_0);
        else
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
        char inst[16];
        char *suffix = i.r.funct3 & 1 ? ".d" : ".w";
        char *aq = i.r.funct7 & 2 ? ".aq" : "";
        char *rl = i.r.funct7 & 1 ? ".rl" : "";

        switch (i.r.funct7 >> 2) {
        case LR:
            snprintf(inst, sizeof inst, "lr%s%s%s", suffix, aq, rl);
            snprintf(buf, buf_size, "%-11s%s,(%s)", inst,
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
            snprintf(inst, sizeof inst, "%s%s%s%s",
                     opcode_amo_name[i.r.funct7 >> 2], suffix, aq, rl);
            snprintf(buf, buf_size, "%-11s%s,%s,(%s)", inst,
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

        snprintf(buf, buf_size, "%-11s%s,%s,0x%08"PRIx64,
                 opcode_op_branch_name[i.r.funct3],
                 N[i.r.rs1], N[i.r.rs2], pc + imm);
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

        assert(-i.uj.imm20 == (imm < 0));

        uint64_t addr = pc + imm;

        if (i.uj.rd == 0)
            // Pseudo "j" instruction
            snprintf(buf, buf_size, "%-11s0x%08"PRIx64,
                     "j", addr);
        else if (i.uj.rd == 1)
            // Pseudo "jal" instruction (without destination)
            snprintf(buf, buf_size, "%-11s0x%08"PRIx64,
                     "jal", addr);
        else
            snprintf(buf, buf_size, "%-11s%s,0x%08"PRIx64,
                     "jal", N[i.uj.rd], addr);

        break;
    }

  case SYSTEM:
      switch (i.r.funct3) {
      case ECALLEBREAK:
          switch (i.i.imm11_0) {
          case 0: snprintf(buf, buf_size, "%-11s", "ecall"); return;
          case 1: snprintf(buf, buf_size, "%-11s", "ebreak"); return;
          case -0x800: snprintf(buf, buf_size, "%-11s", "eret"); return;
          default: assert(0);
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
          break;

      case CSRRSI: snprintf(buf, buf_size, "%-11s%s,%s,%d", "csrrsi", N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), i.i.rs1); return;
      case CSRRC:  snprintf(buf, buf_size, "%-11s%s,%s,%s", "csrrc",  N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), N[i.i.rs1]); return;
      case CSRRCI: snprintf(buf, buf_size, "%-11s%s,%s,%d", "csrrci", N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), i.i.rs1); return;
      case CSRRW:  snprintf(buf, buf_size, "%-11s%s,%s,%s", "csrrw",  N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), N[i.i.rs1]); return;
      case CSRRWI: snprintf(buf, buf_size, "%-11s%s,%s,%d", "csrrwi", N[i.i.rd], csr_fmt(i.i.imm11_0 & 0xFFFU), i.i.rs1); return;

      default:
          assert(0); // XXX more to implement
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
        warn("Opcode %s isn't handled, inst 0x%08x\n", opcode_name[i.r.opcode], inst);
        *buf = 0;
        return;
    }
}

static isa_decoded_t
decode(uint64_t inst_addr, uint32_t inst)
{
    inst_t i = { .raw = inst };
    isa_decoded_t dec = { .inst_addr = inst_addr, .inst = inst };

    dec.dest_reg     = ISA_NO_REG;
    dec.source_reg_a = 0;
    dec.source_reg_b = 0;
    dec.dest_msr     = ISA_NO_REG;
    dec.source_msr_a = ISA_NO_REG;
    dec.class        = isa_inst_class_alu;

    switch (i.r.opcode) {
    case LOAD:
        dec.class        = isa_inst_class_load;
        dec.loadstore_size = 1 << (i.i.funct3 & 3);
        // LB, LH, LW needs sign-extension.
        if (i.i.funct3 <= LW)
            dec.loadstore_size = -dec.loadstore_size;
        dec.source_reg_a = i.i.rs1;
        dec.dest_reg     = i.i.rd;
        break;

    case LOAD_FP:
        // XXX Pretend it's a nop
        dec.class        = isa_inst_class_alu;
        break;

    case MISC_MEM:
        dec.class        = isa_inst_class_branch;
        break;

    case OP_IMM:
    case OP_IMM_32:
    case AUIPC:
    case LUI:
        dec.dest_reg     = i.i.rd;
        dec.source_reg_a = i.i.rs1;
        dec.class        = isa_inst_class_alu;

        if (0) {
        // XXX HACK
        if (i.r.opcode == OP_IMM && i.i.rd == 14 && i.i.rs1 == 14) {
            // stack adjustment
            if (previous_stack_delta < 0 && i.i.imm11_0 == -previous_stack_delta) {
                ++leaf_allocations;
                if (i.i.imm11_0 > max_leaf_allocation)
                    max_leaf_allocation = i.i.imm11_0;
                printf("Leaf allocation insts #%d (of %ld total) of %d bytes (max %d)\n",
                       2 * leaf_allocations,
                       (long) csr[CSR_INSTRET], i.i.imm11_0, max_leaf_allocation);
            }
            previous_stack_delta = i.i.imm11_0;
        }
        }
        break;

    case STORE:
        dec.class        = isa_inst_class_store;
        dec.loadstore_size = 1 << (i.s.funct3 & 3);
        dec.source_reg_a = i.s.rs1;
        dec.source_reg_b = i.s.rs2;
        break;

    case STORE_FP:
//verbosity_override |= VERBOSE_DISASS;

        dec.class        = isa_inst_class_alu; // nop
        break;

    case AMO: {
        switch (i.r.funct7 >> 2) {
        case LR:
            dec.class        = isa_inst_class_load;
            dec.loadstore_size = i.r.funct3 & 1 ? 8 : -4;
            dec.source_reg_a = i.r.rs1;
            dec.dest_reg     = i.r.rd;
            break;

        case SC:
            dec.class        = isa_inst_class_store;
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
            dec.class          = isa_inst_class_atomic;
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
            assert((i.r.funct3 == ADDSUB || i.r.funct3 == SR_) && i.r.funct7 == 0x20 ||
                   i.r.funct7 == 0x00);
        dec.dest_reg     = i.r.rd;
        dec.source_reg_a = i.r.rs1;
        dec.source_reg_b = i.r.rs2;
        dec.class        = isa_inst_class_alu;
        break;

    case BRANCH: {
        int imm =
            i.sb.imm12 << 12 | i.sb.imm11 << 11 |
            i.sb.imm10_5 << 5 | i.sb.imm4_1 << 1;

        dec.class             = isa_inst_class_branch;
        dec.jumpbranch_target = inst_addr + imm;
        dec.source_reg_a      = i.r.rs1;
        dec.source_reg_b      = i.r.rs2;
        break;
    }

    case JALR:
        dec.class        = isa_inst_class_compjump;
        dec.source_reg_a = i.i.rs1;
        dec.dest_reg     = i.i.rd;
        break;

    case JAL: {
        int imm =
            i.uj.imm20    << 20 |
            i.uj.imm19_12 << 12 |
            i.uj.imm11    << 11 |
            i.uj.imm10_1  << 1;

        dec.class        = isa_inst_class_jump;
        dec.dest_reg     = i.uj.rd;
        dec.jumpbranch_target = inst_addr + imm;
        break;
    }


  case SYSTEM:
      switch (i.r.funct3) {
      case ECALLEBREAK:
          switch (i.i.imm11_0) {
          case 0: // SCALL
          case 1: // SBREAK
          case -0x800: // SRET
              // XXX but it has a speculation barrier not accounted for
              dec.class = isa_inst_class_compjump;
              break;
          default:
              assert(0);
          }
          break;

      case CSRRS:
          if (i.i.rs1 == 0) {
              dec.source_msr_a = 0xFFF & (unsigned) i.i.imm11_0;
              dec.dest_reg     = i.i.rd;
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
          break;

      case CSRRW:
          dec.source_reg_a = i.i.rs1;
      case CSRRWI:
          dec.dest_msr     = 0xFFF & (unsigned) i.i.imm11_0;
          dec.dest_reg     = i.i.rd;

          if (i.i.rd)
              dec.source_msr_a = 0xFFF & (unsigned) i.i.imm11_0;
          break;

      default:
          assert(0);
      }
      break;

    case OP_FP:
        switch (i.r.funct7) {
        case FMV_D_X:
            dec.class        = isa_inst_class_alu; // XXX alu_fp?
            //dec.source_freg_a = i.r.rs1;
            //dec.dest_freg     = i.r.rd;
            break;
        default: goto unhandled;
        }
        break;



    default:
    unhandled:
        warn("Opcode %s not decoded, inst %016"PRIx64":%08x\n",
             opcode_name[i.r.opcode], inst_addr, i.raw);
        break;
    }

    if (dec.dest_reg == 0)
        dec.dest_reg = ISA_NO_REG;

    return dec;
}

static isa_result_t
inst_exec(isa_decoded_t dec, uint64_t op_a_u, uint64_t op_b_u, uint64_t msr_a)
{
    int64_t op_a      = (int64_t) op_a_u;
    int64_t op_b      = (int64_t) op_b_u;
    int32_t op_a_32   = (int32_t) op_a;
    int32_t op_b_32   = (int32_t) op_b;
    uint32_t op_a_u_32= (uint32_t) op_a_u;
    uint32_t op_b_u_32= (uint32_t) op_b_u;
    inst_t i          = { .raw = dec.inst };
    isa_result_t res  = { 0 };
    uint64_t ea_load  = op_a + i.i.imm11_0;
    uint64_t ea_store = op_a + (i.s.imm11_5 << 5 | i.s.imm4_0);
    res.fatal_error   = false;
    int xlen          = 64; // XXX need a way to support both 32- and 64-bit here

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
                res.result = op_a_u >> (i.i.imm11_0 & (xlen - 1));
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
        res.result = dec.inst_addr + (i.u.imm31_12 << 12);
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
                res.result = ((__int128) op_a * op_b) >> 64;
                return res;
            case MULHSU:
                res.result = ((__int128) op_a * op_b_u) >> 64;
                return res;
            case MULHU:
                res.result = ((unsigned __int128) op_a_u * op_b_u) >> 64;
                return res;
            case DIV:
                if (op_b == 0)
                    res.result = -1LL;
                else if (op_b == -1 && op_a == (1LL << (xlen - 1)))
                    res.result = -1LL << (xlen - 1);
                else
                    res.result = op_a / op_b;
                return res;
            case DIVU:
                if (op_b == 0)
                    res.result = -1LL;
                else
                    res.result = op_a_u / op_b_u;
                return res;
            case REM:
                if (op_b == 0)
                    res.result = op_a;
                else if (op_b == -1 && op_a == (1LL << (xlen - 1)))
                    res.result = 0;
                else
                    res.result = op_a % op_b;
                return res;
            case REMU:
                if (op_b == 0)
                    res.result = op_a;
                else
                    res.result = op_a_u % op_b_u;
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
                    res.result = op_a_u >> (op_b & (xlen - 1));
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
        return res;

    case JALR:
        res.result = dec.inst_addr + 4;
        res.compjump_target = (op_a + i.i.imm11_0) & -2LL;
        return res;

    case JAL:
        res.result = dec.inst_addr + 4;
        return res;

    case SYSTEM:
        res.result = msr_a; // XXX is this needed?

        switch (i.r.funct3) {
        case ECALLEBREAK:
            switch (i.i.imm11_0) {
            case 0: {
                res.compjump_target = csr[priv * 0x100 + CSR_UTVEC];
                csr[priv * 0x100 + CSR_UEPC] = dec.inst_addr;
                csr[priv * 0x100 + CSR_UCAUSE] = EXCP_UMODE_CALL + priv;
                push_priv();
#if 0
                uint32_t orig_status = csr[CSR_STATUS];
                // XXX because sharing raise() cause problems
                                DEBUG("  %016"PRIx64":ECALL: status was %08x, now %08x, will vector to %016"PRIx64"\n",
                      dec.inst_addr, orig_status,
                      (uint32_t) csr[CSR_STATUS],
                      csr[CSR_EVEC]);

//verbosity_override |= VERBOSE_DISASS;
#endif
                return res;
            }

            case 1: assert(0); printf("  SBREAK");
                res.compjump_target = csr[priv * 0x100 + CSR_UTVEC];
                csr[priv * 0x100 + CSR_UEPC] = dec.inst_addr;
                csr[priv * 0x100 + CSR_UCAUSE] = EXCP_BREAKPOINT;
                push_priv();
                return res;

            case -0x800: // SRET
            {
#if 0
                uint32_t orig_status = csr[CSR_STATUS];

                res.compjump_target = csr[priv * 0x100 + CSR_UEPC];


                priv = BF_GET(csr[priv * 0x100 + CSR_USTATUS], ;

                /* Pop the S and EI bits from PS and PEI respectively */

                BF_SET(csr[CSR_STATUS], CSR_STATUS_S_BF,
                       BF_GET(csr[CSR_STATUS], CSR_STATUS_PS_BF));

                BF_SET(csr[CSR_STATUS], CSR_STATUS_EI_BF,
                       BF_GET(csr[CSR_STATUS], CSR_STATUS_PEI_BF));

                DEBUG("  %016"PRIx64":SRET:  status was %08x, now %08x, will vector to %016"PRIx64"\n",
                      dec.inst_addr, orig_status,
                      (uint32_t) csr[CSR_STATUS],
                      csr[CSR_EPC]);
#endif
                              }
                return res;
            }
            assert(0);
            return res;

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
        warn("Opcode %s exec not implemented, inst %016"PRIx64":%08x "
             "i{%x,%s,%x,%s,%s,%x}\n",
             opcode_name[i.r.opcode], dec.inst_addr, i.raw,
             i.i.imm11_0, reg_name[i.r.rs1], i.r.funct3,
             reg_name[i.r.rd],
             opcode_name[i.r.opcode], i.r.opext);
        res.fatal_error = true;
        res.result = 0;
        return res;
    }

    res.result = -1;
    return res;
}

int exception_raised; // XXX hack


static void raise(cpu_state_t *s, uint64_t cause)
{
#if 0
    if (cause & (1ULL << 63)) {
        int intr_pend = BF_GET(csr[CSR_STATUS], CSR_STATUS_IP_BF);
        int intr_mask = BF_GET(csr[CSR_STATUS], CSR_STATUS_IM_BF);

        intr_pend |= 1 << (cause & 7);

        BF_SET(csr[CSR_STATUS], CSR_STATUS_IP_BF, intr_pend);

        if (!BF_GET(csr[CSR_STATUS], CSR_STATUS_EI_BF) ||
            (intr_pend & intr_mask) == 0) {

            /*
            ERROR("  Interrupt %d ignored (for now) as it's disabled\n", (int) cause);
            ERROR("    (EI = %d IP = 0x%x, IM = 0x%x, pc = %"PRIx64")\n",
                   BF_GET(csr[CSR_STATUS], CSR_STATUS_EI_BF),
                  intr_pend, intr_mask, s->pc);

            verbosity_override |= VERBOSE_DISASS;
            */

            return;
        }
    }

    INFO("  Raised 0x%"PRIx64"\n", cause);

    csr[CSR_EPC] = s->pc;
    s->pc = csr[CSR_EVEC];
    // XXX not correct in case of instruction fetches
    csr[CSR_CAUSE] = cause;
    push_priv();

    exception_raised = 1;
#endif
}

#define HTIF_DEV_SHIFT      (56)

/* executed every cycle */
static void tick(cpu_state_t *s)
{
    // XXX for now, an instruction per tick
    csr[CSR_INSTRET] = (uint32_t) (csr[CSR_INSTRET] + 1);
    csr[CSR_CYCLE]   = (uint32_t) (csr[CSR_CYCLE]   + 1);

#if 0
    int pending =
        BF_GET(csr[CSR_STATUS], CSR_STATUS_IP_BF) &
        BF_GET(csr[CSR_STATUS], CSR_STATUS_IM_BF);

    if (BF_GET(csr[CSR_STATUS], CSR_STATUS_EI_BF) && pending) {
        if (0)
            ERROR("\npc=%"PRIx64" status write of EI enables these pending (mask) 0x%x (priority %d)\n",
              s->pc,
              pending, __builtin_ctz(pending));
        raise(s, (1ULL << 63) | __builtin_ctz(pending));
    }
#endif
}

static uint64_t read_msr(cpu_state_t *s, unsigned csrno)
{
    if (priv < ((csrno >> 8) & 3)) {
        ERROR("  Illegal Read of CSR %3x\n", csrno);
        raise(s, EXCP_INST_ILLEGAL);
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
        INFO("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], csr[csrno]);
        return csr[csrno];

    default:
        DEBUG("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], csr[csrno]);
        return csr[csrno];
    }
}

static void write_msr(cpu_state_t *s, unsigned csrno, uint64_t value)
{
    if (priv < ((csrno >> 8) & 3) || (csrno & 0xc00) == 0xc00) {
        ERROR("  Illegal Write of CSR %3x\n", csrno);
        raise(s, EXCP_INST_ILLEGAL);
        return;
    }

    csr[csrno] = value & csr_mask[csrno] | csr[csrno] & ~csr_mask[csrno];

    DEBUG("  Write CSR %s <- %"PRIx64"\n", csr_name[csrno], csr[csrno]);

    if (csrno == CSR_CYCLE)
        ERROR("  Write CSR %s <- %"PRIx64"\n", csr_name[csrno], csr[csrno]);

    if (value & ~csr[csrno])
        DEBUG("    NB: bits %"PRIx64" are masked off\n", value & ~csr[csrno]);

    switch (csrno) {
    default:
        break;

    case CSR_FFLAGS:
        /* FCSR[7:0] <---> {FRM[3:0], FFLAGS[4:0]} */
        BF_SET(csr[CSR_FCSR], 4:0, BF_GET(csr[CSR_FFLAGS], 4:0));
        break;

    case CSR_FRM:
        /* FCSR[7:0] <---> {FRM[2:0], FFLAGS[4:0]} */
        BF_SET(csr[CSR_FCSR], 7:5, BF_GET(csr[CSR_FRM], 2:0));
        break;

    case CSR_FCSR:
        /* FCSR[7:0] <---> {FRM[3:0], FFLAGS[4:0]} */
        BF_SET(csr[CSR_FFLAGS], 4:0, BF_GET(csr[CSR_FCSR], 4:0));
        BF_SET(csr[CSR_FRM],    2:0, BF_GET(csr[CSR_FCSR], 7:5));
        break;
    }
}

bool debug_vm = false; // to be enabled in the debugger

static uint64_t
load(cpu_state_t *s, uint64_t address, int mem_access_size, memory_exception_t *error)
{
    memory_t *m = s->mem;
    void *p;
    uint32_t iodata;
    int except = EXCP_LOAD_ACCESS_FAULT;

    *error = MEMORY_SUCCESS;

#if 0
    int except = EXCP_LOAD_FAULT;
    uint32_t access = BF_PACK(1, PTE_UR);

    if (mem_access_size == 44)
        mem_access_size = 4, except = EXCP_INST_ADDR, access = BF_PACK(1, PTE_UX);

    if (BF_GET(csr[CSR_STATUS], CSR_STATUS_S_BF))
        access <<= 3;

    *error = MEMORY_SUCCESS;

    if (BF_GET(csr[CSR_STATUS], CSR_STATUS_VM_BF)) {
        address = virt2phys(s, address, access, error);
        if (*error != MEMORY_SUCCESS) {
            raise(s, except);
            csr[CSR_BADVADDR] = address;

            return 0;
        }
    }
#endif

    if (0 && address & 1 << 31) {
        /* We follow Altera's JTAG UART interface:

           The core has two registers, data (addr 0) and control (addr 1):

           data    (R/W): RAVAIL:16        RVALID:1 RSERV:7          DATA:8
           control (R/W): WSPACE:16        RSERV:5 AC:1 WI:1 RI:1    RSERV:6 WE:1 RE:1
        */
        iodata = 0;
        if ((address & 4) == 0) {
            int ch = getchar();
            iodata = (0 <= ch) * (1 << 16 | 1 << 15) + (uint8_t) ch;
        }
        else
            iodata = 1 << 16;

        p = (void *)&iodata + (address & 3);
    }
    else
        p = memory_physical(m, address,
                            mem_access_size > 0 ? mem_access_size : -mem_access_size);

    if (!p) {
        raise(s, except);
        csr[CSR_STVAL] = address;
        ERROR("  load from illegal physical memory %08"PRIx64"\n", address);
        //XXX s->fatal_error = true;
        return 0;
    }

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
store(cpu_state_t *s, uint64_t address, uint64_t value, int mem_access_size, memory_exception_t *error)
{
    *error = MEMORY_SUCCESS;

#if 0
    if (BF_GET(csr[CSR_STATUS], CSR_STATUS_S_BF))
        access <<= 3;

    if (BF_GET(csr[CSR_STATUS], CSR_STATUS_VM_BF)) {
        address = virt2phys(s, address, access, error);
        if (*error != MEMORY_SUCCESS) {
            raise(s, EXCP_STORE_FAULT);
            csr[CSR_BADVADDR] = address;
            return;
        }
    }
#endif

    if (0 && address & 1 << 31) {
        if (address == (1U << 31))
            putchar(value & 255);
        else
            fprintf(stderr, "  IGNORED: store to unmapped IO memory %08"PRIx64"\n", address);
        return;
    }

    memory_t *m = s->mem;
    void *p = memory_physical(m, address, mem_access_size);

    if (!p) {
        raise(s, EXCP_STORE_ACCESS_FAULT);
        csr[CSR_STVAL] = address; // XXX shouldn't that depend on priv?
        ERROR("  store to illegal physical memory %08"PRIx64"\n", address);
        return;
    }

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
setup(cpu_state_t *state, elf_info_t *info)
{
    memset(state->r, 0, sizeof state->r);
    state->r[14] = 0x8400;
    memory_ensure_mapped_range(state->mem, 0x8000, 0x83FF); // XXX hack
    state->pc = 1 ? info->program_entry : 0x2000;

    if (disk_image) {
        disk_fd = open(disk_image, O_RDWR);
        if (disk_fd < 0)
            perror(disk_image), exit(1);
    }

    fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
}

#if 1
const arch_t arch_riscv32 = {
    .zero_reg = 0,
    .reg_name = reg_name,
    .is_64bit = false,
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .disass_inst = disass_inst,
    .tick = tick,
    .read_msr = read_msr,
    .write_msr = write_msr,
    .load = load,
    .store = store,
};
#endif

const arch_t arch_riscv64 = {
    .zero_reg = 0,
    .reg_name = reg_name,
    .is_64bit = true,
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .disass_inst = disass_inst,
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
