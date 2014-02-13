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
 * TODO:
 *
 * Implement *ALL* of RV32I:
 *   SYSTEM (SCALL, SBREAK, RDCYCLE, RDTIME, RDINSTRET)
 *   FENCE[.I],
 *
 * possibly RV32IM
 *   MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU
 *
 * eventually RV32G...
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "sim.h"
#include "arch.h"
#include "riscv.h"

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
    "lb", "lh", "lw", "?", "lbu", "lhu", "?", "?",
};

const char *opcode_op_op_name[16] = {
    "add", "sll", "slt", "sltu", "xor", "srl", "or", "and",
    "sub", "?",   "?",   "?",    "?",   "sra", "?", "?",
};

const char *opcode_op_branch_name[8] = {
    "beq", "bne", "?", "?", "blt", "bge", "bltu", "bgeu",
};

static void
disass_inst(uint64_t pc, uint32_t inst, char *buf, size_t buf_size)
{
    inst_t i = {.raw = inst };

    switch (i.r.opcode) {
    case LOAD:
        snprintf(buf, buf_size, "%-11sr%d,%d(r%d)",
                 opcode_load_op_name[i.i.funct3], i.i.rd, i.i.imm11_0, i.i.rs1);
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

//  case OP_IMM_32
//  case EXT0:
    case STORE: {
        int imm = i.s.imm4_0 | i.s.imm11_5 << 5;
        snprintf(buf, buf_size, "s%-10sr%d,%d(r%d)",
                 1 + opcode_load_op_name[i.s.funct3], i.s.rs2, imm, i.s.rs1);
        break;
    }

//  case STORE_FP:
//  case CUSTOM1:
//  case AMO:
    case OP:
        snprintf(buf, buf_size, "%-11sr%d,r%d,r%d",
                 opcode_op_op_name[i.r.funct3 + 8 * (i.i.imm11_0 >> 10 & 1)],
                 i.r.rd, i.r.rs1, i.r.rs2);
        break;
    case LUI:
        snprintf(buf, buf_size, "%-11sr%d,0x%x",
                 "lui", i.u.rd, i.u.imm31_12);
        break;

//  case OP_32:
//  case EXT1:
//  ..
    case BRANCH: {
        int imm =
            i.sb.imm12 << 12 | i.sb.imm11 << 11 |
            i.sb.imm10_5 << 5 | i.sb.imm4_1 << 1;

        snprintf(buf, buf_size, "%-11sr%d,r%d,0x%08x",
                 opcode_op_branch_name[i.r.funct3],
                 i.r.rs1, i.r.rs2, (uint32_t) pc + imm);
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
        uint32_t addr = pc + imm;

        if (i.uj.rd == 0)
            // Pseudo "j" instruction
            snprintf(buf, buf_size, "%-11s0x%08x",
                     "j", addr);
        else if (i.uj.rd == 1)
            // Pseudo "jal" instruction (without destination)
            snprintf(buf, buf_size, "%-11s0x%08x",
                     "jal", addr);
        else
            snprintf(buf, buf_size, "%-11sr%d,0x%08x",
                     "jal", i.uj.rd, addr);

        break;
    }

  case SYSTEM:
      switch (i.s.imm11_5) {
      case 0:
          switch (i.s.rs2) {
          case 0:
              snprintf(buf, buf_size, "%-11s", "scall");
              return;
          case 1:
              snprintf(buf, buf_size, "%-11s", "sbreak");
              return;
          }
      case 0x60:
          switch (i.s.rs2) {
          case 0:
              snprintf(buf, buf_size, "%-11sr%d", "rdcycle", i.i.rs1);
              return;
          case 1:
              snprintf(buf, buf_size, "%-11sr%d", "rdtime", i.i.rs1);
              return;
          case 2:
              snprintf(buf, buf_size, "%-11sr%d", "rdinstret", i.i.rs1);
              return;
          }
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
        dec.source_reg_a = i.i.rs1;
        dec.dest_reg     = i.i.rd;
        break;

    case OP_IMM:
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

    case OP:
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
        dec.source_reg_a = i.r.rs1;
        dec.source_reg_b = i.r.rs2;
        break;
    }

    case JALR:
        dec.class             = isa_inst_class_compjump;
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
      switch (i.s.imm11_5) {
      case 0:
          switch (i.s.rs2) {
          case 0:
              // scall
              assert(0); // Not handling system calls yet
              break;
          case 1:
              // snprintf(buf, buf_size, "%-11s", "sbreak");
              break;
          }
      case 0x60:
          switch (i.s.rs2) {
          case 0:
              assert(0);
              // snprintf(buf, buf_size, "%-11sr%d", "rdcycle", i.i.rs1);
              break;
          case 1:
              assert(0);
              // snprintf(buf, buf_size, "%-11sr%d", "rdtime", i.i.rs1);
              break;
          case 2:
              assert(0);
              // snprintf(buf, buf_size, "%-11sr%d", "rdinstret", i.i.rs1);
              break;
          }
      }
      break;

    default:
        warn("Opcode %s not decoded, inst 0x%08x\n", opcode_name[i.r.opcode], i.raw);
        break;
    }

    if (dec.dest_reg == 0)
        dec.dest_reg = ISA_NO_REG;

    return dec;
}

static isa_result_t
inst_exec(isa_decoded_t dec, uint64_t op_a, uint64_t op_b, uint64_t msr_a)
{
    inst_t i = { .raw = dec.inst };
    isa_result_t res = { 0 };
    uint32_t ea_load = op_a + i.i.imm11_0;
    uint32_t ea_store = op_a + (i.s.imm11_5 << 5 | i.s.imm4_0);
    res.fatal_error = false;

    switch (i.r.opcode) {
    case LOAD:
        res.result = ea_load;
        return res;

    case OP_IMM:
        switch (i.i.funct3) {
        case ADDI:
            res.result = op_a + i.i.imm11_0;
            break;
        case SLLI:
            res.result = op_a << (i.i.imm11_0 & 31);
            break;
        case SLTI:
            res.result = op_a < i.i.imm11_0;
            break;
        case SLTIU:
            res.result = (uint32_t) op_a < (uint32_t) (int) i.i.imm11_0;
            break;
        case XORI:
            res.result = op_a ^ i.i.imm11_0;
            break;
        case SR_I:
            if (i.i.imm11_0 & 1 << 10)
                res.result = op_a >> (i.i.imm11_0 & 31);
            else
                res.result = (uint32_t) op_a >> (i.i.imm11_0 & 31);
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
        res.result = (int32_t) res.result;
        return res;

    case AUIPC:
        res.result = dec.inst_addr + (i.u.imm31_12 << 12);
        res.result = (int32_t) res.result;
        return res;

    case STORE:
        res.store_value = op_b;
        res.result = ea_store;
        return res;

    case OP:
        switch (i.r.funct3) {
        case ADDSUB:
            res.result = i.i.imm11_0 >> 10 & 1 ? op_a - op_b : op_a + op_b;
            break;
        case SLL:
            res.result = op_a << (op_b & 31);
            break;
        case SLT:
            res.result = op_a < op_b;
            break;
        case SLTU:
            res.result = (uint32_t) op_a < (uint32_t) op_b;
            break;
        case XOR:
            res.result = op_a ^ op_b;
            break;
        case SR_:
            if (i.i.imm11_0 & 1 << 10)
                res.result = op_a >> (op_b & 31);
            else
                res.result = (uint32_t) op_a >> (op_b & 31);
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
        res.result = (int32_t) res.result;
        return res;

    case LUI:
        res.result = i.u.imm31_12 << 12;
        res.result = (int32_t) res.result;
        return res;

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
            res.branch_taken = (i.sb.funct3 & 1) ^ ((uint32_t) op_a < (uint32_t) op_b);
            break;
        default:
            assert(0);
        }
        return res;

    case JALR:
        res.result = dec.inst_addr + 4;
        res.result = (int32_t) res.result;
        res.compjump_target = (int32_t) (op_a + i.i.imm11_0) & -2;
        return res;

    case JAL:
        res.result = dec.inst_addr + 4;
        res.result = (int32_t) res.result;
        return res;

    default:
        warn("Opcode %s exec not implemented, inst 0x%08x\n", opcode_name[i.r.opcode], i.raw);
        res.fatal_error = true;
        res.result = 0;
        return res;
    }

    res.result = -1;
    return res;
}

/* executed every cycle */
static void
tick(cpu_state_t *state)
{
}

static uint64_t
load(cpu_state_t *s, uint64_t address, int mem_access_size)
{
    memory_t *m = s->mem;
    void *p = memory_physical(m, (uint32_t)address, mem_access_size);

    if (!p) {
        fprintf(stderr, "SEGFAULT, load from unmapped memory %08"PRIx64"\n", address);
        s->fatal_error = true;
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
store(cpu_state_t *s, uint64_t address, uint64_t value, int mem_access_size)
{
    memory_t *m = s->mem;
    void *p = memory_physical(m, (uint32_t)address, mem_access_size);

    if (!p) {
        fprintf(stderr, "SEGFAULT, store to unmapped memory %08"PRIx64"\n", address);
        s->fatal_error = true;
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
    state->pc = info->program_entry;

    // <HACK>
    memory_ensure_mapped_range(state->mem, 0x20000000, 1024*1024);
    state->r[31] = 0x20010000;
    // </HACK>

    uint32_t stack_size = 1*1024*1024;
    uint32_t stack_start = 0x80000000;
    memory_ensure_mapped_range(state->mem, stack_start, stack_size);
    state->r[14] = stack_start + stack_size - 4;
    store(state, state->r[14], 0, 4);
}

const arch_t arch_riscv = {
    .zero_reg = 0,
    .is_64bit = false,
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .disass_inst = disass_inst,
    .tick = tick,
    .write_msr = 0, // XXX My RISC-V has no CSRs yet
    .load = load,
    .store = store,
};

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
