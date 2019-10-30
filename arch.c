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

#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include "arch.h"

const char *class_names[] = {
    "illegal",
    "alu",
    "load",
    "store",
    "jump",
    "branch",
    "compjump",
    "atomic",
};

void
isa_disass(FILE *out, const arch_t *arch, isa_decoded_t dec, isa_result_t res)
{
    char dis_buf[99];

    fprintf(out, "%08x %08x ", (uint32_t) dec.insn_addr, dec.insn);

    if (dec.dest_reg == ISA_NO_REG)
        fprintf(out, "%8s ", "");
    else
        fprintf(out, "%08x ", (uint32_t)res.result);

    arch->disass_insn(dec.insn_addr, dec.insn, dis_buf, sizeof dis_buf);

    if (0) {
        fprintf(out, "%s\n", dis_buf);
        return;
    }

    switch (dec.class) {
    case isa_insn_class_load:
        if (dec.dest_reg != ISA_NO_REG)
            fprintf(out, "%-36s %s <~ 0x%08x [0x%08x]",
                    dis_buf, arch->reg_name[dec.dest_reg], (uint32_t)res.result, (uint32_t)res.load_addr);
        else
            fprintf(out, "%-36s  <~ 0x%08x [0x%08x]",
                    dis_buf, (uint32_t)res.result, (uint32_t)res.load_addr);
        break;

    case isa_insn_class_store:
        fprintf(out, "%-36s [0x%08x] <- 0x%08x", dis_buf, (uint32_t)res.store_addr, (uint32_t)res.store_value);
        break;

    default:
        if (dec.dest_reg != ISA_NO_REG || dec.dest_msr != ISA_NO_REG)
            fprintf(out, "%-36s", dis_buf);
        else
            fprintf(out, "%s", dis_buf);

        if (dec.dest_reg != ISA_NO_REG)
            fprintf(out, " %s <- 0x%08x", arch->reg_name[dec.dest_reg], (uint32_t)res.result);

        if (dec.dest_msr != ISA_NO_REG)
            fprintf(out, " MSR%04x <- 0x%08x", dec.dest_msr, (uint32_t)res.msr_result);
        break;
    }

    fputc('\n', out);
}

extern const arch_t arch_alpha, arch_lm32, arch_riscv32, arch_riscv64;

const arch_t *
get_arch(uint16_t machine, bool is_64bit)
{
    if (machine == EM_RISCV && is_64bit)
        return &arch_riscv64;

    if (machine == EM_RISCV && !is_64bit)
        return &arch_riscv32;
        // err(1, "32-bit RISC-V unsupported");

    err(1, "unsupported architecture %d", machine);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
