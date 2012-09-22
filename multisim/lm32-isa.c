/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2012 Tommy Thorn
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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "sim.h"
#include "isa.h"
#include "lm32_opcode.h"

#define _LM32_MK_NAME(O) #O,

static const char *lm32_opcode_name[] = {
    _LM32_OP_F(_LM32_MK_NAME)
};

static void
disass(uint64_t addr, uint32_t inst)
{
    inst = htonl(inst);
    lm32_instruction_t i = {.raw = inst };
    char op_buf[16], s[30];
    uint32_t imm16 = i.ri.imm16;

    switch (i.ri.op) {
    case ANDHI: case ANDI: case CMPGEUI: case CMPGUI:
    case NORI: case ORHI: case ORI: case XNORI: case XORI:
        imm16 = (uint16_t) imm16;
        break;
    default:
        ;
    }

    strcpy(op_buf, lm32_opcode_name[i.i.op]);

    for (int i = 0; op_buf[i]; ++i)
        op_buf[i] = tolower(op_buf[i]);

    switch (i.ri.op) {
    case ORHI: case ORI:
        if (i.ri.r0 == 0) {
            snprintf(s, sizeof s, "%-7s" "r%d=0x%04x",
                     i.ri.op == ORI ? "mvi" : "mvhi", i.ri.rd, imm16);
            break;
        }
        /* fall-through */
    case ADDI:
        if (i.ri.rd == 0 && i.ri.r0 == 0 && imm16 == 0) {
            strcpy(s, "nop");
            break;
        }
    case SRUI: case NORI: case MULI: case SRI:
    case XORI: case ANDI: case XNORI:
    case SLI:
    case CMPEI: case CMPNEI:
    case CMPGI: case CMPGEI: case CMPGEUI: case CMPGUI:
    case ANDHI:
        snprintf(s, sizeof s,
                 "%-7s" "r%d=r%d,0x%04x", op_buf, i.ri.rd, i.ri.r0, imm16);
        break;

    case OR:
        if (i.rr.r1 == 0) {
            snprintf(s, sizeof s,
                     "%-7s" "r%d=r%d", "mv", i.rr.rd, i.rr.r0);
            break;
        }
    case SRU: case NOR: case MUL: case SR:
    case XOR: case AND: case XNOR: case ADD:
    case SL:
    case CMPE: case CMPNE:
    case CMPG: case CMPGE: case CMPGEU: case CMPGU:
    case MODU: case SUB: case MOD:
    case DIVU: case DIV:
        snprintf(s, sizeof s,
                 "%-7s" "r%d=r%d,r%d", op_buf, i.rr.rd, i.rr.r0, i.rr.r1);
        break;

    case SEXTB:
    case SEXTH:
        snprintf(s, sizeof s,
                 "%-7s" "r%d=r%d", op_buf, i.rr.rd, i.rr.r0);
        break;

    case LB: case LH: case LW: case LHU: case LBU:
        snprintf(s, sizeof s,
                 "%-7s" "r%d=(r%d+%d)", op_buf, i.ri.rd, i.ri.r0, imm16);
        break;
    case SH: case SB: case SW:
        snprintf(s, sizeof s,
                 "%-7s" "(r%d+%d)=r%d", op_buf, i.ri.r0, imm16, i.ri.rd);
        break;

    case BE: case BG: case BGE: case BGEU: case BGU: case BNE:
        snprintf(s, sizeof s,
                 "%-7s" "r%d,r%d,0x%08x", op_buf, i.ri.r0, i.ri.rd,
                 (uint32_t) (addr + imm16 * 4));
        break;


    case SCALL: case RES1: case RES2:
        snprintf(s, sizeof s, "%s", op_buf);
        break;

    case RCSR:
        snprintf(s, sizeof s, "%-7s" "r%d=c%d", op_buf, i.rr.rd, i.rr.r0);
        break;

    case WCSR:
        snprintf(s, sizeof s, "%-7s" "c%d=r%d", op_buf, i.rr.r0, i.rr.r1);
        break;

    case B: case CALL:
        snprintf(s, sizeof s, "%-7s" "r%d", op_buf, i.ri.r0);
        break;

    case BI: case CALLI:
        snprintf(s, sizeof s, "%-7s" "0x%08x", op_buf, (uint32_t) (addr + i.i.imm26 * 4));
        break;
    }

    printf("%08llx %08x %s", addr, inst, s);
}

static void
decode(uint32_t inst,
       int *dest_reg, int *source_reg_a, int *source_reg_b,
       bool *b_is_imm, uint64_t *imm,
       bool *is_load, bool *is_store, bool *is_branch)
{
#if 0
    lm32_instruction_t i = { .raw = inst };
    *dest_reg     = NO_REG;
    *source_reg_a = 31;
    *source_reg_b = 31;
    *b_is_imm     = false;
    *imm          = i.iop_imm.lit;
    *is_load      = false;
    *is_store      = false;
    *is_branch      = false;

    switch (i.iop.opcode) {
    case OP_LDQ:
    case OP_LDQ_U:
    case OP_LDBU:
        *is_load      = i.iop.ra != 31;
    case OP_LDAH:
    case OP_LDA:
        *source_reg_a = i.iop.rb;
        *dest_reg     = i.iop.ra;
        break;

    case OP_STB:
    case OP_STL:
        *is_store = true;
        *source_reg_a = i.iop.rb;
        *source_reg_b = i.iop.ra;
        break;


    case OP_INTA_:
    case OP_INTL_:
        *b_is_imm     = i.iop.isimm;
        *dest_reg     = i.iop.rc;
        *source_reg_b = i.iop.rb;
        *source_reg_a = i.iop.ra;
        break;

    case OP_BEQ:
    case OP_BNE:
        *is_branch = true;
        *source_reg_a = i.iop.ra;
        break;
    }

    if (*dest_reg == 31)
        *dest_reg = NO_REG;
#endif
}

static uint64_t
inst_loadalign(uint32_t instruction, uint64_t address, uint64_t result)
{
#if 0
    inst_t i = { .raw = instruction };

    switch (i.iop.opcode) {
    case OP_LDBU: return (uint8_t) (result >> 8 * (address & 7));
    default: return result;
    }
#endif
    return result;
}

static uint64_t
inst_exec(uint32_t instruction, uint64_t op_a, uint64_t op_b,
          uint64_t *storev, uint64_t *storemask, uint64_t *pc,
          bool *fatal)
{
#if 0
    inst_t i = { .raw = instruction };
    uint64_t ea = op_a + i.mem.disp;
    *fatal = false;

    switch (i.iop.opcode) {
    case OP_LDAH: return op_a + i.mem.disp * 65536;
    case OP_LDBU:
    case OP_LDA:
    case OP_LDQ:  return ea;
    case OP_LDQ_U:return ea & ~7;

    case OP_STB:
        *storev = (uint8_t)op_b * 0x0101010101010101ULL;
        *storemask = 0xFFULL << 8 * (ea & 7);
        return ea;

    case OP_STL: {
        if (ea & 4) {
            *storev = op_b << 32;
            *storemask = 0xFFFFFFFF00000000ULL;
        }
        else {
            *storev = op_b;
            *storemask = 0xFFFFFFFFULL;
        }

        return ea;
    }

    case OP_INTL_:
        switch (i.iop.func) {
        case OP_INTL_BIS: return op_a | op_b;
        default:
            warn("%s not implemented", intl_opcode_name[i.iop.func]);
            *fatal = true;
            return 0;
        }
        break;

    case OP_INTA_:
        switch (i.iop.func) {
        case OP_INTA_ADDQ: return op_a + op_b;
        case OP_INTA_ADDL: return (int32_t)(op_a + op_b);
        case OP_INTA_CMPEQ: return op_a == op_b;
        case OP_INTA_S4ADDQ: return op_a * 4 + op_b;
        case OP_INTA_CMPULT: return op_a < op_b;
        default:
            warn("%s not implemented", inta_opcode_name[i.iop.func]);
            *fatal = true;
            return 0;
        }
        break;

    case OP_BEQ:
        if (op_a == 0) {
            *pc = *pc + 4 + i.br.disp * 4;
            return 1;
        }
        return 0;

    case OP_BNE:
        if (op_a != 0) {
            *pc = *pc + 4 + i.br.disp * 4;
            return 1;
        }
        return 0;

    default:
        warn("Opcode %s not implemented, inst 0x%08x", opcode_name[i.iop.opcode], i.raw);
        *fatal = true;
        return 0;
    }

#endif

    return -1;
}

static void
setup(cpu_state_t *state, elf_info_t *info)
{
    state->pc = info->program_entry;
    memory_ensure_mapped_range(state->mem, 0x200103f0, 1024*1024);
}

const isa_t lm32_isa = {
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .inst_loadalign = inst_loadalign,
    .disass = disass,
};

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
