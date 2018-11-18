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

void
isa_disass(const arch_t *arch, isa_decoded_t dec, isa_result_t res)
{
    char dis_buf[99];

    fprintf(stderr, "%016"PRIx64" ", dec.insn_addr);

    arch->disass_insn(dec.insn_addr, dec.insn, dis_buf, sizeof dis_buf);

    switch (dec.class) {
    case isa_insn_class_load:
        if (dec.dest_reg != ISA_NO_REG)
            fprintf(stderr, "%-32s %s <~ 0x%016"PRIx64" [0x%016"PRIx64"]",
		    dis_buf, arch->reg_name[dec.dest_reg], res.result, res.load_addr);
        else
            fprintf(stderr, "%-32s  <~ 0x%016"PRIx64" [0x%016"PRIx64"]",
		    dis_buf, res.result, res.load_addr);
        break;

    case isa_insn_class_store:
        fprintf(stderr, "%-32s [0x%016"PRIx64"] <- 0x%016"PRIx64, dis_buf, res.store_addr, res.store_value);
        break;

    default:
        if (dec.dest_reg != ISA_NO_REG || dec.dest_msr != ISA_NO_REG)
            fprintf(stderr, "%-32s", dis_buf);
        else
            fprintf(stderr, "%s", dis_buf);

        if (dec.dest_reg != ISA_NO_REG)
            fprintf(stderr, " %s <- 0x%016"PRIx64"", arch->reg_name[dec.dest_reg], res.result);

        if (dec.dest_msr != ISA_NO_REG)
            fprintf(stderr, " MSR%04x <- 0x%016"PRIx64"", dec.dest_msr, res.msr_result);
        break;
    }

    // XXX
    fprintf(stderr, "\n");
    fflush(stderr);
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