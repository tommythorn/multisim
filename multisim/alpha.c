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
#include "arch.h"
#include "alpha.h"

static void
mfmt(char *buf, size_t n, uint64_t pc, char *inst_string, inst_t i)
{
    snprintf(buf, n, "%-11sr%d,%d(r%d)", inst_string, i.mem.ra, i.mem.disp, i.mem.rb);
}

#if 0
static void
brfmt(char *buf, size_t n, uint64_t pc, char *inst_string, inst_t i)
{
    if (i.mem.ra == 31)
        snprintf(buf, n, "%-11s0x%016"PRIx64"", inst_string,
                pc + 4 + i.br.disp * 4);
    else
        snprintf(buf, n, "%-11s0x%016"PRIx64",r%d", inst_string,
                pc + 4 + i.br.disp * 4, i.mem.ra);
}
#endif

static void
cbrfmt(char *buf, size_t n, uint64_t pc, char *inst_string, inst_t i)
{
    snprintf(buf, n, "%-11sr%d,0x%016"PRIx64"", inst_string, i.mem.ra, pc + 4 + i.br.disp * 4);
}

static void
opfmt(char *buf, size_t n, uint64_t pc, char *inst_string, inst_t i)
{
    if (i.iop.isimm)
        snprintf(buf, n, "%-11sr%d,%d,r%d", inst_string, i.iop.ra, i.iop_imm.lit, i.iop.rc);
    else
        snprintf(buf, n, "%-11sr%d,r%d,r%d", inst_string, i.iop.ra, i.iop.rb, i.iop.rc);
}

static void
barefmt(char *buf, size_t n, uint64_t pc, char *inst_string, inst_t i)
{
    strncpy(buf, inst_string, n);
}

/*static*/ void
palfmt(char *buf, size_t n, uint64_t pc, char *inst_string, inst_t i)
{
    snprintf(buf, n, "%-11s0x%x", inst_string, i.pal.number);
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
disass_inst(uint64_t pc, uint32_t inst, char *buf, size_t buf_size)
{
    inst_t i = {.raw = inst };
    char *inst_string;
    void (*fmt)(char *, size_t, uint64_t, char *, inst_t) = 0;

#define DIS(name, format, rd) inst_string = #name; fmt = format; break

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

    if (fmt)
        fmt(buf, buf_size, pc, inst_string, i);
}

static isa_decoded_t
decode(uint64_t inst_addr, uint32_t inst)
{
    inst_t i = { .raw = inst };
    isa_decoded_t dec = { .inst_addr = inst_addr, .inst = inst };

    dec.dest_reg     = ISA_NO_REG;
    dec.source_reg_a = 31;
    dec.source_reg_b = 31;
    dec.dest_msr     = ISA_NO_REG;
    dec.source_msr_a = ISA_NO_REG;
    dec.class        = isa_inst_class_alu;

    switch (i.iop.opcode) {
    case OP_LDQ:
    case OP_LDQ_U:
        if (i.iop.ra != 31)
            dec.class = isa_inst_class_load;
        dec.loadstore_size = 8;
        dec.source_reg_a = i.iop.rb;
        dec.dest_reg     = i.iop.ra;
        break;

    case OP_LDAH:
    case OP_LDA:
        dec.source_reg_a = i.iop.rb;
        dec.dest_reg     = i.iop.ra;
        break;

    case OP_LDBU:
        if (i.iop.ra != 31)
            dec.class = isa_inst_class_load;
        dec.loadstore_size = 1;
        dec.source_reg_a = i.iop.rb;
        dec.dest_reg     = i.iop.ra;
        break;

    case OP_STL:
    case OP_STB:
        dec.class          = isa_inst_class_store;
        dec.loadstore_size = i.iop.opcode == OP_STL ? 4 : 1;
        dec.source_reg_a = i.iop.rb;
        dec.source_reg_b = i.iop.ra;
        break;


    case OP_INTA_:
    case OP_INTL_:
        dec.dest_reg     = i.iop.rc;
        dec.source_reg_b = i.iop.rb;
        dec.source_reg_a = i.iop.ra;
        break;

    case OP_BEQ:
    case OP_BNE:
        dec.class = isa_inst_class_branch;
        dec.jumpbranch_target = inst_addr + (i.br.disp + 1) * 4;
        dec.source_reg_a = i.iop.ra;
        break;
    }

    if (dec.dest_reg == 31)
        dec.dest_reg = ISA_NO_REG;

    return dec;
}

static isa_result_t
inst_exec(isa_decoded_t dec, uint64_t op_a, uint64_t op_b, uint64_t msr_a)
{
    inst_t i = { .raw = dec.inst };
    uint64_t imm = i.iop_imm.lit;
    uint64_t ea = op_a + i.mem.disp;
    uint64_t op_b_imm = i.iop.isimm ? imm : op_b;
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
            res.result = op_a | op_b_imm;
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
            res.result = op_a + op_b_imm;
            return res;
        case OP_INTA_ADDL: res.result = (int32_t)(op_a + op_b_imm);
            return res;
        case OP_INTA_CMPEQ: res.result = op_a == op_b_imm;
            return res;
        case OP_INTA_S4ADDQ: res.result = op_a * 4 + op_b_imm;
            return res;
        case OP_INTA_CMPULT: res.result = op_a < op_b_imm;
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
        return res;

    case OP_BNE:
        res.branch_taken = op_a != 0;
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
    }

    switch (mem_access_size) {
    case -1: return *(int8_t *)p;
    case  1: return *(uint8_t *)p;
    case -2: return (int16_t)memory_endian_fix16(m, *(uint16_t *)p);
    case  2: return memory_endian_fix16(m, *(uint16_t *)p);
    case -4: return (int32_t)memory_endian_fix32(m, *(uint32_t *)p);
    case  4: return memory_endian_fix32(m, *(uint32_t *)p);
    case -8:
    case  8: return memory_endian_fix64(m, *(uint64_t *)p);
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
    }

    switch (mem_access_size) {
    case 1: *(uint8_t *)p = value; return;
    case 2: *(uint16_t *)p = memory_endian_fix16(m, value); return;
    case 4: *(uint32_t *)p = memory_endian_fix32(m, (uint32_t)value); return;
    case 8: *(uint64_t *)p = memory_endian_fix64(m, (uint64_t)value); return;
    default:
        assert(mem_access_size > 0);
        assert(mem_access_size == mem_access_size + 1);
    }
}

const arch_t arch_alpha = {
    .zero_reg = 31,
    .setup = setup,
    .decode = decode,
    .inst_exec = inst_exec,
    .disass_inst = disass_inst,
    .tick = tick,
    .write_msr = 0, // XXX My alpha has no MSRs yet
    .load = load,
    .store = store,
};

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
