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

#include <stdio.h>
#include <stdlib.h>
#include "arch.h"

void
isa_disass(const arch_t *arch, isa_decoded_t dec, isa_result_t res)
{
    char dis_buf[99];
    uint64_t mask = arch->is_64bit ? ~0ULL : ~0U;

    printf("%016"PRIx64" ", dec.inst_addr);

    arch->disass_inst(dec.inst_addr, dec.inst, dis_buf, sizeof dis_buf);

    switch (dec.class) {
    case isa_inst_class_load:
        if (dec.dest_reg != ISA_NO_REG)
            printf("%-32s %s <- 0x%016"PRIx64" [0x%016"PRIx64"]\n",
                   dis_buf, arch->reg_name[dec.dest_reg], res.result, res.load_addr);
        else
            printf("%-32s  <- 0x%016"PRIx64" [0x%016"PRIx64"]\n",
                   dis_buf, res.result, res.load_addr);
        break;

    case isa_inst_class_store:
        printf("%-32s [0x%016"PRIx64"] <- 0x%016"PRIx64"\n", dis_buf, res.store_addr, res.store_value);
        break;

    default:
        if (dec.dest_reg != ISA_NO_REG || dec.dest_msr != ISA_NO_REG)
            printf("%-32s", dis_buf);
        else
            printf("%s", dis_buf);

        if (dec.dest_reg != ISA_NO_REG)
            printf(" %s <- 0x%016"PRIx64"", arch->reg_name[dec.dest_reg], mask & res.result);

        if (dec.dest_msr != ISA_NO_REG)
            printf(" MSR%04x <- 0x%016"PRIx64"", dec.dest_msr, mask & res.msr_result);

        printf("\n");
    }

    // XXX
    fflush(stdout);
}

extern const arch_t arch_alpha, arch_lm32, arch_riscv32, arch_riscv64;

const arch_t *
get_arch(uint16_t machine, bool is_64bit)
{
    if (machine == EM_ALPHA)
        return &arch_alpha;
    if (machine == EM_LM32 || machine == EM_LM32_ALT)
        return &arch_lm32;
    if (machine == EM_RISCV)
        return is_64bit ? &arch_riscv64 : &arch_riscv32;

    fprintf(stderr, "error: unsupported architecture %d", machine);

    exit(1);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
