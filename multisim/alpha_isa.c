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
#include "sim.h"
#include "isa.h"
#include "alpha_opcode.h"

static void
mfmt(char *buf, uint64_t pc, char *inst_string, inst_t i)
{
    sprintf(buf, "%-11sr%d,%d(r%d)", inst_string, i.mem.ra, i.mem.disp, i.mem.rb);
}

/*static*/ void
brfmt(char *buf, uint64_t pc, char *inst_string, inst_t i)
{
    if (i.mem.ra == 31)
        sprintf(buf, "%-11s0x%016llx", inst_string,
                pc + 4 + i.br.disp * 4);
    else
        sprintf(buf, "%-11s0x%016llx,r%d", inst_string,
                pc + 4 + i.br.disp * 4, i.mem.ra);
}

static void
cbrfmt(char *buf, uint64_t pc, char *inst_string, inst_t i)
{
    sprintf(buf, "%-11sr%d,0x%016llx", inst_string, i.mem.ra, pc + 4 + i.br.disp * 4);
}

static void
opfmt(char *buf, uint64_t pc, char *inst_string, inst_t i)
{
    if (i.iop.isimm)
        sprintf(buf, "%-11sr%d,%d,r%d", inst_string, i.iop.ra, i.iop_imm.lit, i.iop.rc);
    else
        sprintf(buf, "%-11sr%d,r%d,r%d", inst_string, i.iop.ra, i.iop.rb, i.iop.rc);
}

static void
barefmt(char *buf, uint64_t pc, char *inst_string, inst_t i)
{
    strcpy(buf, inst_string);
}

/*static*/ void
palfmt(char *buf, uint64_t pc, char *inst_string, inst_t i)
{
    sprintf(buf, "%-11s0x%x", inst_string, i.pal.number);
}

#define mk_opcode_name(O) #O,
static char *opcode_name[] = {
    all_opcode(mk_opcode_name)
};

#define mk_sub_opcode_name(sub,O) [0x##sub] = #O,
static char *intl_opcode_name[] = {
    all_intl_opcode(mk_sub_opcode_name)
};

static char *inta_opcode_name[] = {
    all_inta_opcode(mk_sub_opcode_name)
};

static void
disass(uint64_t pc, uint32_t inst)
{
    inst_t i = {.raw = inst };
    unsigned wbr;
    char *inst_string;
    void (*fmt)(char *, uint64_t, char *, inst_t) = 0;

#define DIS(name, format, rd) inst_string = #name; fmt = format; wbr = rd; break

    switch (i.iop.opcode) {
    default:
        warn("Opcode %s not implemented, inst 0x%08x\n", opcode_name[i.iop.opcode], inst);
        return;
    case OP_LDAH: DIS(ldah,  mfmt, i.iop.ra);
    case OP_LDA:  DIS(lda,   mfmt, i.iop.ra);
    case OP_LDQ:  DIS(ldq,   mfmt, i.iop.ra);
    case OP_LDQ_U:DIS(ldq_u, mfmt, i.iop.ra);
    case OP_INTL_:
        switch (i.iop.func) {
        case OP_INTL_BIS:
            if (i.raw ==  0x47ff041f) {
                DIS(nop, barefmt, 31);
            }
            else {
                DIS(bis, opfmt, i.iop.rc);
            }
        default:
            warn("%s not implemented", intl_opcode_name[i.iop.func]);
            return;
        }
        break;

    case OP_INTA_:
        switch (i.iop.func) {
        default:
            warn("%s not implemented", inta_opcode_name[i.iop.func]);
            return;
        case OP_INTA_ADDQ: DIS(addq,opfmt, i.iop.rc);
        case OP_INTA_ADDL: DIS(addl,opfmt, i.iop.rc);
        case OP_INTA_CMPEQ: DIS(cmpeq,opfmt, i.iop.rc);
        case OP_INTA_S4ADDQ: DIS(s4addl,opfmt, i.iop.rc);
        case OP_INTA_CMPULT: DIS(cmpult,opfmt, i.iop.rc);
        }
        break;
    case OP_LDBU: DIS(ldbu,mfmt, i.iop.ra);
    case OP_BEQ: DIS(beq,cbrfmt, 31);
    case OP_BNE: DIS(bne,cbrfmt, 31);
    case OP_STB: DIS(stb,mfmt, 31);
    case OP_STL: DIS(stl,mfmt, 31);
    }

    char buf[99];
    if (fmt) {
        fmt(buf, pc, inst_string, i);
        printf("%016llx %s\n", pc, buf);
    }
}

static isa_decoded_t
decode(uint64_t inst_addr, uint32_t inst)
{
    inst_t i = { .raw = inst };
    isa_decoded_t dec = { .inst_addr = inst_addr, .inst = inst };

    dec.dest_reg     = NO_REG;
    dec.source_reg_a = 31;
    dec.source_reg_b = 31;
    dec.b_is_imm     = false;
    dec.imm          = i.iop_imm.lit;
    dec.is_load      = false;
    dec.is_store     = false;
    dec.is_branch    = false;
    dec.mem_access_size = 1;

    switch (i.iop.opcode) {
    case OP_LDQ:
    case OP_LDQ_U:
        dec.mem_access_size = 8;
    case OP_LDBU:
        dec.is_load      = i.iop.ra != 31;
    case OP_LDAH:
    case OP_LDA:
        dec.source_reg_a = i.iop.rb;
        dec.dest_reg     = i.iop.ra;
        break;

    case OP_STL:
        dec.mem_access_size = 4;
    case OP_STB:
        dec.is_store     = true;
        dec.source_reg_a = i.iop.rb;
        dec.source_reg_b = i.iop.ra;
        break;


    case OP_INTA_:
    case OP_INTL_:
        dec.b_is_imm     = i.iop.isimm;
        dec.dest_reg     = i.iop.rc;
        dec.source_reg_b = i.iop.rb;
        dec.source_reg_a = i.iop.ra;
        break;

    case OP_BEQ:
    case OP_BNE:
        dec.is_branch    = true;
        dec.is_unconditional = false;
        dec.source_reg_a = i.iop.ra;
        break;
    }

    if (dec.dest_reg == 31)
        dec.dest_reg = NO_REG;

    return dec;
}

static isa_result_t
inst_exec(isa_decoded_t dec, uint64_t op_a, uint64_t op_b)
{
    inst_t i = { .raw = dec.inst };
    uint64_t ea = op_a + i.mem.disp;
    isa_result_t res = { 0 };
    res.fatal_error = false;

    switch (i.iop.opcode) {
    case OP_LDAH:
        res.result = op_a + i.mem.disp * 65536;
        return res;
    case OP_LDBU:
    case OP_LDA:
    case OP_LDQ:
        res.result = ea;
        return res;
    case OP_LDQ_U:
        res.result = ea & ~7;
        return res;

    case OP_STB:
    case OP_STL:
        res.store_value = op_b;
        res.result = ea;
        return res;

    case OP_INTL_:
        switch (i.iop.func) {
        case OP_INTL_BIS:
             res.result = op_a | op_b;
             return res;
        default:
            warn("%s not implemented", intl_opcode_name[i.iop.func]);
            res.fatal_error = true;
            res.result = 0;
            return res;
        }
        break;

    case OP_INTA_:
        switch (i.iop.func) {
        case OP_INTA_ADDQ:
            res.result = op_a + op_b;
            return res;
        case OP_INTA_ADDL: res.result = (int32_t)(op_a + op_b);
            return res;
        case OP_INTA_CMPEQ: res.result = op_a == op_b;
            return res;
        case OP_INTA_S4ADDQ: res.result = op_a * 4 + op_b;
            return res;
        case OP_INTA_CMPULT: res.result = op_a < op_b;
            return res;
        default:
            warn("%s not implemented", inta_opcode_name[i.iop.func]);
            res.fatal_error = true;
            res.result = 0;
            return res;
        }
        break;

    case OP_BEQ:
        res.branch_taken = op_a == 0;
        res.branch_target = dec.inst_addr + (i.br.disp + 1) * 4;
        return res;

    case OP_BNE:
        res.branch_taken = op_a != 0;
        res.branch_target = dec.inst_addr + (i.br.disp + 1) * 4;
        return res;

    default:
        warn("Opcode %s not implemented, inst 0x%08x\n", opcode_name[i.iop.opcode], i.raw);
        res.fatal_error = true;
        res.result = 0;
        return res;
    }

    res.result = -1;
    return res;
}

static void
setup(cpu_state_t *state, elf_info_t *info)
{
    state->r[27] = state->pc = info->program_entry;
    memory_ensure_mapped_range(state->mem, 0x200103f0, 1024*1024);
}

const isa_t alpha_isa = {
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
