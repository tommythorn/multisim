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

static void
decode(uint32_t inst,
       int *dest_reg, int *source_reg_a, int *source_reg_b,
       bool *b_is_imm, uint64_t *imm,
       bool *is_load, bool *is_store, bool *is_branch)
{
        inst_t i = { .raw = inst };

        *dest_reg     = NO_REG;
        *source_reg_a = NO_REG;
        *source_reg_b = NO_REG;
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

        if (*source_reg_a == 31) *source_reg_a = NO_REG;
        if (*source_reg_b == 31) *source_reg_b = NO_REG;
        if (*dest_reg == 31) *dest_reg = NO_REG;
}

static uint64_t
inst_loadalign(uint32_t instruction, uint64_t address, uint64_t result)
{
        inst_t i = { .raw = instruction };

        switch (i.iop.opcode) {
        case OP_LDBU: return (uint8_t) (result >> 8 * (address & 7));
        default: return result;
        }
}

static uint64_t
inst_exec(uint32_t instruction, uint64_t op_a, uint64_t op_b,
          uint64_t *storev, uint64_t *storemask, uint64_t *pc,
          bool *fatal)
{
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
            warn("Opcode %s not implemented, inst 0x%08x\n", opcode_name[i.iop.opcode], i.raw);
            *fatal = true;
            return 0;
        }

        return -1;
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
    .inst_loadalign = inst_loadalign,
    .disass = disass,
};
