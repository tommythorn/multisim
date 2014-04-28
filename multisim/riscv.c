/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2014 Tommy Thorn
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
 * Question:
 * - how does the timer interrupt work if IE = 0? Does it stay pending
 *   and fire when IE is set, or is it gone forever?
 *
 * Status:
 *
 * I - Implemented all,
 *   SYSTEM (SCALL, SBREAK) and all the CSRs -- Partially done
 *
 * M - Fix the implementation of
 *   MULH, MULHSU, MULHU, REM, REMW -- DONE
 *
 * A - Atomics -- DONE
 *
 * FD - Floating point, Just Do It!
 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <sys/time.h>
#include "bitfields.h"
#include "sim.h"
#include "arch.h"
#include "riscv.h"

static const char *csr_name[0x1000] = {
    [CSR_FFLAGS]	= "fflags",
    [CSR_FRM]		= "frm",
    [CSR_FCSR]		= "fcsr",

    [CSR_SUP0]		= "sup0",
    [CSR_SUP1]		= "sup1",
    [CSR_EPC]		= "epc",
    [CSR_BADVADDR]	= "badvaddr",
    [CSR_PTBR]		= "ptbr",
    [CSR_ASID]		= "asid",
    [CSR_COUNT]		= "count",
    [CSR_COMPARE]	= "compare",
    [CSR_EVEC]		= "evec",
    [CSR_CAUSE]		= "cause",
    [CSR_STATUS]	= "status",
    [CSR_HARTID]	= "hartid",
    [CSR_IMPL]		= "impl",
    [CSR_FATC]		= "fatc",
    [CSR_SEND_IPI]	= "send_ipi",
    [CSR_CLEAR_IPI]	= "clear_ipi",
    [CSR_TOHOST]	= "tohost",
    [CSR_FROMHOST]	= "fromhost",

    [CSR_CYCLE]		= "cycle",
    [CSR_TIME]		= "time",
    [CSR_INSTRET]	= "instret",
};

/* Per CSR bit mask of csrXX settable bits (in addition to other constraints) */
static const uint64_t csr_mask[0x1000] = {
    [CSR_FFLAGS]	= 0x1F,
    [CSR_FRM]   	= 7,
    [CSR_FCSR]		= 0xFF,

    [CSR_SUP0]		= ~0ULL,
    [CSR_SUP1]		= ~0ULL,

    [CSR_EPC]		= ~0ULL,
    [CSR_BADVADDR]	= 0, //  ?? writable?
    [CSR_PTBR]		= ~0ULL,
    [CSR_ASID]		= 0,
    [CSR_COUNT]		= 0xFFFFFFFF,
    [CSR_COMPARE]	= 0xFFFFFFFF,
    [CSR_EVEC]		= ~0ULL,
    [CSR_CAUSE]		= 0, // Read-only
    [CSR_STATUS]	= 0xFF008F, // IM,  VM,  PEI, EI, PS, S
    [CSR_HARTID]	= 0, // Read-only
    [CSR_IMPL]		= 0, // Read-only
    [CSR_FATC]		= 0,
    [CSR_SEND_IPI]	= 0, // Single processor
    [CSR_CLEAR_IPI]	= 1, // Write only!
    [CSR_TOHOST]	= ~0ULL,
    [CSR_FROMHOST]	= ~0ULL,

    [CSR_CYCLE]		= 0xFFFFFFFF,
    [CSR_TIME]		= 0xFFFFFFFF,
    [CSR_INSTRET]	= 0xFFFFFFFF,
};

static uint64_t csr[0x1000] = {
    [CSR_EVEC] = 0x2000,
    [CSR_STATUS] =
    BF_PACK(CSR_STATUS_U64_BF, 1) |
    BF_PACK(CSR_STATUS_S64_BF, 1) |
    BF_PACK(CSR_STATUS_S_BF,   1),
};

const uint64_t memory_start = 0x00000000;
const uint64_t memory_size  = 128 * 1024;

const char *reg_name[32] = {
    //  0     1     2     3     4     5     6     7     8     9    10    11
    "zero", "ra", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9",
    //12     13    14    15    16    17    18    19    20    21    22    23
    "s10", "s11", "sp", "tp", "v0", "v1", "a0", "a1", "a2", "a3", "a4", "a5",
    //24   25    26    27    28    29    30    31
    "a6", "a7", "t0", "t1", "t2", "t3", "t4", "gp",
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

static void
disass_inst(uint64_t pc, uint32_t inst, char *buf, size_t buf_size)
{
    inst_t i = {.raw = inst };

    switch (i.r.opcode) {
    case LOAD:
        snprintf(buf, buf_size, "%-11sr%d,%d(r%d)",
                 opcode_load_op_name[i.i.funct3], i.i.rd, i.i.imm11_0, i.i.rs1);
        break;


    case MISC_MEM:
        snprintf(buf, buf_size, "%-11s", i.r.funct3 ? "fence.i" : "fence");
        break;

//  case LOAD_FP:
//  case CUSTOM0:
//  case MISC_MEM:
    case OP_IMM:
        if (i.i.funct3 == ADDI && i.i.rs1 == 0)
            // li pseudo instruction
            snprintf(buf, buf_size, "%-11sr%d,%d",
                     "li", i.i.rd, i.i.imm11_0);
        else
            snprintf(buf, buf_size, "%-11sr%d,r%d,%d",
                     opcode_imm_name[i.i.funct3], i.i.rd, i.i.rs1, i.i.imm11_0);
        break;

    case AUIPC:
        snprintf(buf, buf_size, "%-11sr%d,0x%x",
                 "auipc", i.u.rd, i.u.imm31_12);
        break;

    case OP_IMM_32: {
        char op[16];
        snprintf(op, sizeof op, "%sw",opcode_imm_name[i.r.funct3]);

        snprintf(buf, buf_size, "%-11sr%d,r%d,%d", op, i.r.rd, i.r.rs1, i.i.imm11_0);
        break;
    }

//  case EXT0:
    case STORE: {
        int imm = i.s.imm4_0 | i.s.imm11_5 << 5;
        snprintf(buf, buf_size, "s%-10sr%d,%d(r%d)",
                 1 + opcode_load_op_name[i.s.funct3], i.s.rs2, imm, i.s.rs1);
        break;
    }

//  case STORE_FP:
//  case CUSTOM1:
    case AMO: {
        char inst[16];
        char *suffix = i.r.funct3 & 1 ? ".d" : ".w";
        char *aq = i.r.funct7 & 2 ? ".aq" : "";
        char *rl = i.r.funct7 & 1 ? ".rl" : "";

        switch (i.r.funct7 >> 2) {
        case LR:
            snprintf(inst, sizeof inst, "lr%s%s%s", suffix, aq, rl);
            snprintf(buf, buf_size, "%-11sr%d,(r%d)", inst,
                     i.r.rd, i.r.rs1);
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
            snprintf(buf, buf_size, "%-11sr%d,r%d,(r%d)", inst,
                     i.r.rd, i.r.rs2, i.r.rs1);
            break;
        }
        }
        break;

    case OP:
        if (i.r.funct7 == 1)
            snprintf(buf, buf_size, "%-11sr%d,r%d,r%d",
                     opcode_op_div_name[i.r.funct3],
                     i.r.rd, i.r.rs1, i.r.rs2);
        else
            snprintf(buf, buf_size, "%-11sr%d,r%d,r%d",
                     opcode_op_op_name[i.r.funct3 + 8 * (i.i.imm11_0 >> 10 & 1)],
                     i.r.rd, i.r.rs1, i.r.rs2);
        break;
    case LUI:
        snprintf(buf, buf_size, "%-11sr%d,0x%x",
                 "lui", i.u.rd, i.u.imm31_12);
        break;

  case OP_32: {
        char op[16];
        snprintf(op, sizeof op, "%sw",
                 i.r.funct7 == 1
                 ? opcode_op_div_name[i.r.funct3]
                 : opcode_op_op_name[i.r.funct3 + 8 * (i.i.imm11_0 >> 10 & 1)]);

        snprintf(buf, buf_size, "%-11sr%d,r%d,r%d", op, i.r.rd, i.r.rs1, i.r.rs2);
        break;
    }

//  ..
    case BRANCH: {
        int imm =
            i.sb.imm12 << 12 | i.sb.imm11 << 11 |
            i.sb.imm10_5 << 5 | i.sb.imm4_1 << 1;

        snprintf(buf, buf_size, "%-11sr%d,r%d,0x%08"PRIx64,
                 opcode_op_branch_name[i.r.funct3],
                 i.r.rs1, i.r.rs2, pc + imm);
        break;
    }

    case JALR:
        if (i.i.rd == 0 && i.i.imm11_0 == 0)
            // ret pseudo instruction
            snprintf(buf, buf_size, "ret");
        else
            snprintf(buf, buf_size, "%-11sr%d,r%d,%d",
                     "jalr", i.i.rd, i.i.rs1, i.i.imm11_0);
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
            snprintf(buf, buf_size, "%-11sr%d,0x%08"PRIx64,
                     "jal", i.uj.rd, addr);

        break;
    }

  case SYSTEM:
      switch (i.r.funct3) {
      case SCALLSBREAK:
          switch (i.i.imm11_0) {
          case 0: snprintf(buf, buf_size, "%-11s", "scall"); return;
          case 1: snprintf(buf, buf_size, "%-11s", "sbreak"); return;
          case -0x800: snprintf(buf, buf_size, "%-11s", "sret"); return;
          default: assert(0);
          }
          break;

      case CSRRS:
          if (i.i.rs1 == 0) {
              switch ((unsigned)i.i.imm11_0) {
              case 0xC00: snprintf(buf, buf_size, "%-11sr%d", "rdcycle",   i.i.rd); return;
              case 0xC01: snprintf(buf, buf_size, "%-11sr%d", "rdtime",    i.i.rd); return;
              case 0xC02: snprintf(buf, buf_size, "%-11sr%d", "rdinstret", i.i.rd); return;
              default:    snprintf(buf, buf_size, "%-11sr%d,$%03x", "csrr",  i.i.rd, i.i.imm11_0); return;
              }
          } else
              snprintf(buf, buf_size, "%-11sr%d,$%03x,r%d",  "csrrs", i.i.rd, i.i.imm11_0, i.i.rs1); return;
          break;

      case CSRRSI: snprintf(buf, buf_size, "%-11sr%d,$%03x,%d",  "csrrsi", i.i.rd, i.i.imm11_0, i.i.rs1); return;
      case CSRRC:  snprintf(buf, buf_size, "%-11sr%d,$%03x,r%d", "csrrc",  i.i.rd, i.i.imm11_0, i.i.rs1); return;
      case CSRRCI: snprintf(buf, buf_size, "%-11sr%d,$%03x,%d",  "csrrci", i.i.rd, i.i.imm11_0, i.i.rs1); return;
      case CSRRW:  snprintf(buf, buf_size, "%-11sr%d,$%03x,r%d", "csrrw",  i.i.rd, i.i.imm11_0, i.i.rs1); return;
      case CSRRWI: snprintf(buf, buf_size, "%-11sr%d,$%03x,%d",  "csrrwi", i.i.rd, i.i.imm11_0, i.i.rs1); return;

      default:
          assert(0); // XXX more to implement
      }
      break;

//  ...

    default:
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
        break;

    case STORE:
        dec.class        = isa_inst_class_store;
        dec.loadstore_size = 1 << (i.s.funct3 & 3);
        dec.source_reg_a = i.s.rs1;
        dec.source_reg_b = i.s.rs2;
        break;

    case AMO: {
        switch (i.r.funct7 >> 2) {
        case LR:
            dec.class        = isa_inst_class_load;
            dec.loadstore_size = i.r.funct3 & 1 ? 8 : 4;
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
      case SCALLSBREAK:
          switch (i.i.imm11_0) {
          case 0: // SCALL
              assert(0);
          case 1: // SBREAK
              assert(0);
          case -0x800: // SRET
              // XXX but it has a speculation barrier not accounted for
              dec.class = isa_inst_class_compjump;
              dec.source_msr_a = CSR_STATUS;
              dec.dest_msr = CSR_STATUS;
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

      case CSRRC:
      case CSRRW:
          dec.source_reg_a = i.i.rs1;
      case CSRRSI:
      case CSRRCI:
      case CSRRWI:
          dec.dest_msr     = 0xFFF & (unsigned) i.i.imm11_0;
          dec.source_msr_a = 0xFFF & (unsigned) i.i.imm11_0;
          dec.dest_reg     = i.i.rd;
              break;
      default:
          assert(0);
      }
      break;

    default:
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

        case AMOADD:	res.result = op_a + op_b; break;
        case AMOAND:	res.result = op_a & op_b; break;
        case AMOMAX:	res.result = op_a   < op_b   ? op_b : op_a; break;
        case AMOMAXU:	res.result = op_a_u < op_b_u ? op_b : op_a; break;
        case AMOMIN:	res.result = op_a   > op_b   ? op_b : op_a; break;
        case AMOMINU:	res.result = op_a_u > op_b_u ? op_b : op_a; break;
        case AMOOR:	res.result = op_a | op_b; break;
        case AMOSWAP:	res.result = op_b; break;
        case AMOXOR:	res.result = op_a ^ op_b; break;
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
                    res.result = -1LL << 31;
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
        case SCALLSBREAK:
            switch (i.i.imm11_0) {
            case 0: printf("  SCALL"); break;
            case 1: printf("  SBREAK"); break;
            case -0x800:
                res.compjump_target = csr[CSR_EPC];

                /* Pop the S and EI bits from PS and PEI respectively */
                res.msr_result =
                    BF_SET(msr_a, CSR_STATUS_S_BF,
                           BF_GET(CSR_STATUS_PS_BF, msr_a));

                res.msr_result =
                    BF_SET(res.msr_result, CSR_STATUS_EI_BF,
                           BF_GET(CSR_STATUS_PEI_BF, msr_a));

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

static void raise(cpu_state_t *s, uint64_t cause)
{
    if (cause & (1ULL << 63)) {
        int intr_pend = BF_GET(CSR_STATUS_IP_BF, csr[CSR_STATUS]);
        int intr_mask = BF_GET(CSR_STATUS_IM_BF, csr[CSR_STATUS]);

        intr_pend |= 1 << (cause & 7);

        csr[CSR_STATUS] =
            BF_SET(csr[CSR_STATUS], CSR_STATUS_IP_BF, intr_pend);

        if (!BF_GET(CSR_STATUS_EI_BF, csr[CSR_STATUS]) ||
            (intr_pend & intr_mask) == 0) {

            printf("  Interrupt 0x%"PRIx64" ignored (for now) as it's disabled\n", cause & ~(1ULL << 63));
            printf("    (EI = %d IP = 0x%x, IM = 0x%x)\n",
                   BF_GET(CSR_STATUS_EI_BF, csr[CSR_STATUS]),
                   intr_pend, intr_mask);
            return;
        }
    }

    printf("  Raised 0x%"PRIx64"\n", cause);

    csr[CSR_EPC] = s->pc;
    s->pc = csr[CSR_EVEC];
    // XXX not correct in case of instruction fetches
    csr[CSR_CAUSE] = cause;

    /* Save S in PS, and set S */
    csr[CSR_STATUS] =
        BF_SET(csr[CSR_STATUS], CSR_STATUS_PS_BF,
               BF_GET(CSR_STATUS_S_BF, csr[CSR_STATUS]))
        | BF_PACK(CSR_STATUS_S_BF, 1);

    /* Save EI in PEI and clear EI */
    csr[CSR_STATUS] =
        BF_SET(csr[CSR_STATUS], CSR_STATUS_PEI_BF,
               BF_GET(CSR_STATUS_EI_BF, csr[CSR_STATUS]));
    csr[CSR_STATUS] &= ~BF_PACK(CSR_STATUS_EI_BF, 1);
}

/* executed every cycle */
static void tick(cpu_state_t *s)
{
    // XXX for now, an instruction per tick
    csr[CSR_INSTRET] = (uint32_t) (csr[CSR_INSTRET] + 1);
    csr[CSR_CYCLE]   = (uint32_t) (csr[CSR_CYCLE]   + 1);
    csr[CSR_COUNT]   = (uint32_t) (csr[CSR_COUNT]   + 1);

    if (csr[CSR_COUNT] == csr[CSR_COMPARE])
        raise(s, (1ULL << 63) | TRAP_INTR_TIMER);
}

static uint64_t read_msr(cpu_state_t *s, unsigned csrno)
{
    int top_priv = BF_GET(11:10, csrno);
    int bot_priv = BF_GET( 9: 8, csrno);
    int user_ok  = top_priv == 0 || top_priv == 3 && bot_priv == 0;

    if (!BF_GET(CSR_STATUS_S_BF, csr[CSR_STATUS]) && !user_ok) {
        printf("  Illegal Read of CSR %3x in user mode\n", csrno);
        raise(s, TRAP_INST_PRIVILEGE);
        return 0;
    }

    switch (csrno) {
    case CSR_TIME: {
     struct timeval tv;
     gettimeofday(&tv, NULL);
     uint64_t now = tv.tv_sec * 1000000LL + tv.tv_usec;
     printf("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], now);
     return now;
    }

    case CSR_FROMHOST:
        printf("  Read  CSR fromhost -> ??\n");
        assert(0);
        break;

    default:
        printf("  Read  CSR %s -> %"PRIx64"\n", csr_name[csrno], csr[csrno]);
        return csr[csrno];
    }
}

static void write_msr(cpu_state_t *s, unsigned csrno, uint64_t value)
{
    int top_priv = BF_GET(11:10, csrno);
    int bot_priv = BF_GET( 9: 8, csrno);
    int user_ok  = top_priv == 0 && bot_priv == 0;

    if (!BF_GET(CSR_STATUS_S_BF, csr[CSR_STATUS]) && !user_ok) {
        printf("  Illegal Write of CSR %3x in user mode\n", csrno);
        raise(s, TRAP_INST_PRIVILEGE);
        return;
    }

    if (top_priv == 3) {
        printf("  Illegal Write of CSR %3x\n", csrno);
        raise(s, TRAP_INST_PRIVILEGE);
        return;
    }

    uint64_t oldvalue = csr[csrno];

    csr[csrno] = value & csr_mask[csrno] | csr[csrno] & ~csr_mask[csrno];

    printf("  Write CSR %s <- %"PRIx64"\n", csr_name[csrno], csr[csrno]);

    if (value & ~csr[csrno])
        printf("    NB: bits %"PRIx64" are masked off\n", value & ~csr[csrno]);

    switch (csrno) {
    default:
        break;

    case CSR_STATUS: {
        int pending =
            BF_GET(CSR_STATUS_IP_BF, value) & BF_GET(CSR_STATUS_IM_BF, value);

        if (BF_GET(CSR_STATUS_EI_BF, ~oldvalue & value) && pending)
            raise(s, (1ULL << 63) | __builtin_clz(pending));
        break;
    }

    case CSR_COMPARE:
        csr[CSR_STATUS] &= ~BF_PACK(CSR_STATUS_IP_BF, 1 << TRAP_INTR_TIMER);
        break;

    case CSR_TOHOST: // tohost
        printf("  HOST RESULT %"PRId64"\n", value);
        if ((value >> 48) == 0x101) {
            csr[CSR_TOHOST] = 0; // ACK it
            // csr[CSR_FROMHOST] = 1; // ACK it
            fprintf(stderr, "%c", (char)value);
        } else
            assert(0);
        //exit(0);
    }

}

bool debug_vm = false; // to be enabled in the debugger

static uint64_t
virt2phys(cpu_state_t *s, uint64_t address, int mem_access_size, memory_exception_t *error)
{
    int vpn[3] = {
        address >> 13 & 1023,
        address >> 23 & 1023,
        address >> 33 & 1023,
    };

    //XXX
    //if (address == 0x4000) debug_vm = true;

    *error = MEMORY_SUCCESS;

    if (debug_vm)
    printf("  virt2phys(%016"PRIx64" = <%d,%d,%d,%d>)\n", address, vpn[2], vpn[1], vpn[0],
         (int)address & 8191);

    uint64_t a = csr[CSR_PTBR];

    for (int i = 2; 0 <= i; --i) {

        if (0) { // XXX I could walk the complete table, counting virtual and physical pages, looking
            // for aliases and virtual and physical memory holes...

            printf("  Level %d page table @ %016"PRIx64"\n", i, a);
            for (int j = 0; j < 1024; ++j) {
                uint64_t pte = *(uint64_t *)memory_physical(s->mem, a + j * 8, 8);

                printf("   %016"PRIx64" = %4d %4d %4d Perm 0%o%s%s%s\n",
                     pte,
                     (int)(pte >> 33 & 1023),
                     (int)(pte >> 23 & 1023),
                     (int)(pte >> 13 & 1023),

                     (int)(pte >> 3 & 077),

                     pte >> 2 & 1 ? "G" : " ",
                     pte >> 1 & 1 ? "T" : " ",
                     pte >> 0 & 1 ? "V" : " ");
            }
        }

        uint64_t pte = *(uint64_t *)memory_physical(s->mem, a + vpn[i] * 8, 8);

        if (debug_vm)
        printf("  %016"PRIx64"[%d]: pte[%d] = %016"PRIx64" = %4d %4d %4d Perm 0%o%s%s%s\n",
             a, vpn[i],
             i, pte,
             (int)(pte >> 33 & 1023),
             (int)(pte >> 23 & 1023),
             (int)(pte >> 13 & 1023),

             (int)(pte >> 3 & 077),

             pte >> 2 & 1 ? "G" : " ",
             pte >> 1 & 1 ? "T" : " ",
             pte >> 0 & 1 ? "V" : " ");

        if ((pte >> 0 & 1) == 0) { // V
            if (debug_vm) {
                printf("  Invalid mapping: %016"PRIx64"[%d]: pte[%d] = %016"PRIx64" = %4d %4d %4d Perm 0%o%s%s%s\n",
                       a, vpn[i],
                       i, pte,
                       (int)(pte >> 33 & 1023),
                       (int)(pte >> 23 & 1023),
                       (int)(pte >> 13 & 1023),

                       (int)(pte >> 3 & 077),

                       pte >> 2 & 1 ? "G" : " ",
                       pte >> 1 & 1 ? "T" : " ",
                       pte >> 0 & 1 ? "V" : " ");

                goto exception;
            }
            else {
                debug_vm = 1;
                return virt2phys(s, address, mem_access_size, error);
            }
        }

        if ((pte >> 1 & 1) == 0) { // T
            uint64_t ppn[3];

            for (int j = 0; j < 3; ++j) {
                ppn[j] = pte >> (10 * j + 13) & 1023;
                if (j < i)
                    ppn[j] = vpn[j];
            }

            uint64_t phys =
                ppn[2] << 33 |
                ppn[1] << 23 |
                ppn[0] << 13 |
                address & 8191;

            if (debug_vm)
            printf("  final physical address = %016"PRIx64"\n", phys);

            return phys;
        }

        a = pte & ~8191ULL;
    }

    printf("  Impossible?\n");

exception:
    *error = MEMORY_EXCEPTION;

    return 0;
}

static uint64_t
load(cpu_state_t *s, uint64_t address, int mem_access_size, memory_exception_t *error)
{
    memory_t *m = s->mem;
    void *p;
    uint32_t iodata;

    *error = MEMORY_SUCCESS;

    if (BF_GET(CSR_STATUS_VM_BF, csr[CSR_STATUS])) {
        address = virt2phys(s, address, mem_access_size, error);
        if (*error != MEMORY_SUCCESS) {
            raise(s, TRAP_LOAD_FAULT);
            csr[CSR_BADVADDR] = address;

            return 0;
        }
    }

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
        raise(s, TRAP_LOAD_FAULT);
        csr[CSR_BADVADDR] = address;
        printf("  load from illegal physical memory %08"PRIx64"\n", address);
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

    if (BF_GET(CSR_STATUS_VM_BF, csr[CSR_STATUS])) {
        address = virt2phys(s, address, mem_access_size, error);
        if (*error != MEMORY_SUCCESS) {
            raise(s, TRAP_STORE_FAULT);
            csr[CSR_BADVADDR] = address;
            return;
        }
    }

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
        raise(s, TRAP_STORE_FAULT);
        csr[CSR_BADVADDR] = address;
        printf("  store to illegal physical memory %08"PRIx64"\n", address);
        // s->fatal_error = true;
        return;
    }

    switch (mem_access_size) {
    case 1: case -1: *(uint8_t  *)p = value; return;
    case 2: case -2: *(uint16_t *)p = value; return;
    case 4: case -4: *(uint32_t *)p = value; return;
    case 8: *(uint64_t *)p = value; return;
    default:
        assert(mem_access_size > 0);
        assert(mem_access_size == mem_access_size + 1);
    }
}

static void
dump(cpu_state_t *s, const char *filename, unsigned width, unsigned shift)
{
    FILE *f = fopen(filename, "w");

    uint32_t mask = (1ULL << width) - 1;

    if (!f) {
        perror(filename);
        return;
    }

    memory_t *m = s->mem;
    for (int i = 0; i < 128 * 1024; i += 4) {
        uint32_t *p = memory_physical(m, memory_start + i, 4);
        if (!p)
            break;
        fprintf(f, "%0*x\n", width / 4, (*p >> shift) & mask);
    }

    fclose(f);
}

static void
setup(cpu_state_t *state, elf_info_t *info)
{
    memset(state->r, 0, sizeof state->r);
    state->pc = info->program_entry;



    // <HACK>
    const int memory_size       = 256 * 1024 * 1024; // 256 MiB
    const uint32_t memory_start = 0;
    memory_ensure_mapped_range(state->mem, memory_start, memory_size);
    state->r[31] = memory_start + memory_size / 2; // GP
    state->r[14] = memory_start + memory_size - 4; // SP
    memory_exception_t dummy;
    store(state, state->r[14], 0, 4, &dummy);
    assert(dummy == MEMORY_SUCCESS);
    // </HACK>

    // <HACK> <HACK>
    dump(state, "program.txt", 32, 0);
    dump(state, "mem0.txt", 8,  0);
    dump(state, "mem1.txt", 8,  8);
    dump(state, "mem2.txt", 8, 16);
    dump(state, "mem3.txt", 8, 24);
}

const arch_t arch_riscv32 = {
    .zero_reg = 0,
    .is_64bit = true,
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .disass_inst = disass_inst,
    .tick = tick,
    .read_msr = read_msr,
    .write_msr = write_msr,
    .load = 0 /*load*/,
    .store = 0 /*store*/,
};

const arch_t arch_riscv64 = {
    .zero_reg = 0,
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
