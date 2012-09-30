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

static bool
is_imm16_signed(lm32_opcode_t op)
{
    switch (op) {
    case ANDHI: case ANDI: case CMPGEUI: case CMPGUI:
    case NORI: case ORHI: case ORI: case XNORI: case XORI:
        return false;
    default:
        return true;
    }
}

static void
disass(uint64_t addr, uint32_t inst)
{
    lm32_instruction_t i = {.raw = inst };
    char op_buf[16], s[30];
    uint32_t imm16 = is_imm16_signed(i.ri.op) ? i.ri.imm16 : (uint16_t) i.ri.imm16;
    strcpy(op_buf, lm32_opcode_name[i.ri.op]);

    for (int i = 0; op_buf[i]; ++i)
        op_buf[i] = tolower(op_buf[i]);

    switch (i.ri.op) {
    case ORHI: case ORI:
        if (i.ri.rY == 0) {
            snprintf(s, sizeof s, "%-7s" "r%d=0x%04x",
                     i.ri.op == ORI ? "mvi" : "mvhi", i.ri.rX, imm16);
            break;
        }
        /* fall-through */
    case ADDI:
        if (i.ri.rX == 0 && i.ri.rY == 0 && imm16 == 0) {
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
                 "%-7s" "r%d=r%d,0x%04x", op_buf, i.ri.rX, i.ri.rY, imm16);
        break;

    case OR:
        if (i.rr.rZ == 0) {
            snprintf(s, sizeof s,
                     "%-7s" "r%d=r%d", "mv", i.rr.rX, i.rr.rY);
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
                 "%-7s" "r%d=r%d,r%d", op_buf, i.rr.rX, i.rr.rY, i.rr.rZ);
        break;

    case SEXTB:
    case SEXTH:
        snprintf(s, sizeof s,
                 "%-7s" "r%d=r%d", op_buf, i.rr.rX, i.rr.rY);
        break;

    case LB: case LH: case LW: case LHU: case LBU:
        snprintf(s, sizeof s,
                 "%-7s" "r%d=(r%d+%d)", op_buf, i.ri.rX, i.ri.rY, imm16);
        break;
    case SH: case SB: case SW:
        snprintf(s, sizeof s,
                 "%-7s" "(r%d+%d)=r%d", op_buf, i.ri.rY, imm16, i.ri.rX);
        break;

    case BE: case BG: case BGE: case BGEU: case BGU: case BNE:
        snprintf(s, sizeof s,
                 "%-7s" "r%d,r%d,0x%08x", op_buf, i.ri.rY, i.ri.rX,
                 (uint32_t) (addr + imm16 * 4));
        break;


    case SCALL: case RES1: case RES2:
        snprintf(s, sizeof s, "%s", op_buf);
        break;

    case RCSR:
        snprintf(s, sizeof s, "%-7s" "r%d=c%d", op_buf, i.rr.rX, i.rr.rY);
        break;

    case WCSR:
        snprintf(s, sizeof s, "%-7s" "c%d=r%d", op_buf, i.rr.rY, i.rr.rZ);
        break;

    case B: case CALL:
        snprintf(s, sizeof s, "%-7s" "r%d", op_buf, i.ri.rY);
        break;

    case BI: case CALLI:
        snprintf(s, sizeof s, "%-7s" "0x%08x", op_buf, (uint32_t) (addr + i.i.imm26 * 4));
        break;
    }

    printf("%08llx %08x %s\n", addr, inst, s);
}

static isa_decoded_t
decode(uint64_t inst_addr, uint32_t inst)
{
    lm32_instruction_t i = { .raw = inst };
    isa_decoded_t dec = { .inst_addr = inst_addr, .inst = inst };

    dec.dest_reg     = i.ri.op < 32 ? i.ri.rX : i.rr.rX;
    dec.source_reg_a = i.ri.op < 32 ? i.ri.rY : i.rr.rY; // NB: same field
    dec.source_reg_b = i.ri.op < 32 ? 0       : i.rr.rZ;
    dec.dest_msr     = ISA_NO_REG;
    dec.source_msr_a = ISA_NO_REG;
    dec.class        = isa_inst_class_alu;

    switch (i.ri.op) {
    case RES1: case RES2:
        assert(0);

    case BI:
        dec.class            = isa_inst_class_jump;
        dec.jumpbranch_target= inst_addr + i.i.imm26 * 4;
        dec.source_reg_a     = 0;
        dec.source_reg_b     = 0;
        dec.dest_reg         = ISA_NO_REG;
        break;

    case CALLI:
        dec.class            = isa_inst_class_jump;
        dec.jumpbranch_target= inst_addr + i.i.imm26 * 4;
        dec.source_reg_a     = 0;
        dec.source_reg_b     = 0;
        dec.dest_reg         = RA;
        break;

    case SCALL:
        dec.class            = isa_inst_class_jump;
        dec.jumpbranch_target= -1; // XXX
        dec.source_reg_a     = 0;
        dec.source_reg_b     = 0;
        dec.dest_reg         = EA;
        break;

    case B:
        dec.class            = isa_inst_class_compjump;
        dec.dest_reg         = ISA_NO_REG;
        assert(dec.source_reg_b == 0);
        break;

    case CALL:
        /* Odd that RA is hardcoded */
        dec.class            = isa_inst_class_compjump;
        dec.dest_reg         = RA;
        assert(dec.source_reg_b == 0);
        break;

    case BE: case BG: case BGE: case BGEU: case BGU: case BNE:
        dec.class            = isa_inst_class_branch;
        dec.jumpbranch_target= inst_addr + i.ri.imm16 * 4;
        dec.source_reg_b     = i.ri.rX;
        dec.dest_reg         = ISA_NO_REG;
        break;

    case SB:
        dec.class            = isa_inst_class_store;
        dec.loadstore_size   = 1;
        dec.source_reg_b     = i.ri.rX;
        dec.dest_reg         = ISA_NO_REG;
        break;

    case SH:
        dec.class            = isa_inst_class_store;
        dec.loadstore_size   = 2;
        dec.source_reg_b     = i.ri.rX;
        dec.dest_reg         = ISA_NO_REG;
        break;
    case SW:
        dec.class            = isa_inst_class_store;
        dec.loadstore_size   = 4;
        dec.source_reg_b     = i.ri.rX;
        dec.dest_reg         = ISA_NO_REG;
        break;

    case LB:
        dec.class           = isa_inst_class_load;
        dec.loadstore_size  = -1;
        break;

    case LH:
        dec.class           = isa_inst_class_load;
        dec.loadstore_size  = -2;
        break;
    case LBU:
        dec.class           = isa_inst_class_load;
        dec.loadstore_size  = 1;
        break;
    case LHU:
        dec.class           = isa_inst_class_load;
        dec.loadstore_size  = 2;
        break;
    case LW:
        dec.class           = isa_inst_class_load;
        dec.loadstore_size  = 4;
        break;

    case WCSR:
        dec.dest_msr        = i.ri.rY;
        dec.dest_reg        = ISA_NO_REG;
        break;

    case RCSR:
        dec.source_msr_a    = i.ri.rY;
        dec.source_reg_a    = 0;
        break;

    default:
        break;
    }

    return dec;
}

static isa_result_t
inst_exec(isa_decoded_t dec, uint64_t op_Y, uint64_t op_ZX, uint64_t msr_a)
{
    isa_result_t res     = { 0 };
    lm32_instruction_t i = { .raw = dec.inst };
    lm32_opcode_t op     = i.ri.op;
    uint32_t      uy     = (uint32_t)op_Y;
    uint32_t      uzx    = (uint32_t)op_ZX;
    uint32_t      uz_imm = op < 32 ? (uint16_t) i.ri.imm16 : (uint32_t)op_ZX;
    int32_t       sy     = (int32_t)op_Y;
    int32_t       szx    = (int32_t)op_ZX;
    int32_t       sz_imm = op < 32 ? i.ri.imm16 : (int32_t)op_ZX;

    res.fatal_error      = false;

    switch (op) {
    case ADDI:    case ADD:     res.result = uy + uz_imm; break;
    case SUB:                   res.result = uy - uzx; break;

    case MULI:    case MUL:     res.result = uy * uz_imm; break;
    case DIV:                   res.result = sy / szx; break; // undocumented!
    case MOD:                   res.result = sy % szx; break; // undocumented!
    case DIVU:                  res.result = uy / uzx; break;
    case MODU:                  res.result = uy % uzx; break;

    case ANDHI:                 assert((uz_imm & 0xFFFF) == 0); // XXX just a check
    case ANDI:    case AND:     res.result = uy & uz_imm; break;
    case ORHI:                  res.result = uy | (i.ri.imm16 << 16); break;
    case ORI:     case OR:      res.result = uy | uz_imm; break;
    case XORI:    case XOR:     res.result = uy ^ uz_imm; break;
    case NORI:    case NOR:     res.result = ~(uy | uz_imm); break;
    case XNORI:   case XNOR:    res.result = ~uy ^ uz_imm; break;

    case SRUI:    case SRU:     res.result = uy >> (uz_imm & 31); break;
    case SRI:     case SR:      res.result = sy >> (uz_imm & 31); break;
    case SLI:     case SL:      res.result = sy << (uz_imm & 31); break;

    case RCSR:                  res.result = msr_a; break;
    case WCSR:                  res.msr_result = uy; break; // XXX mask of the valid bits
    case RES1:                  assert(0);
    case RES2:                  assert(0);
    case SCALL:                 assert(0); // XXX TODO

    case SEXTB:                 res.result = (int8_t) sy; break;
    case SEXTH:                 res.result = (int16_t) sy; break;

    case CALL: case B:          res.result = dec.inst_addr + 4; break;
    case CALLI: case BI:        res.result = dec.inst_addr + 4; break;

    case BE:                    res.branch_taken = szx == sy; break;
    case BNE:                   res.branch_taken = uzx != uy; break;
    case BG:                    res.branch_taken = szx >  sy; break;
    case BGE:                   res.branch_taken = szx >= sy; break;
    case BGU:                   res.branch_taken = uzx >  uy; break;
    case BGEU:                  res.branch_taken = uzx >= uy; break;

    case CMPEI:   case CMPE:    res.result = sy == sz_imm; break;
    case CMPNEI:  case CMPNE:   res.result = uy != uz_imm; break;
    case CMPGI:   case CMPG:    res.result = sy >  sz_imm; break;
    case CMPGEI:  case CMPGE:   res.result = sy >= sz_imm; break;
    case CMPGUI:  case CMPGU:   res.result = uy >  uz_imm; break;
    case CMPGEUI: case CMPGEU:  res.result = uy >= uz_imm; break;

    case LB: case LBU:
    case LH: case LHU: case LW: res.result = uy + i.ri.imm16; break;

    case SB: case SH: case SW:  res.result = uy + i.ri.imm16; break;
                                res.store_value = uzx; break;
    }

    return res;
}

static void
setup(cpu_state_t *state, elf_info_t *info)
{
    state->pc = info->program_entry;
    memory_ensure_mapped_range(state->mem, 0x200103f0, 1024*1024);
}

const isa_t lm32_isa = {
    .zero_reg = 0,
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .disass = disass,
};

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
