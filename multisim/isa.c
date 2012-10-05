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
#include "isa.h"

void
isa_disass(const isa_t *isa, isa_decoded_t dec, isa_result_t res, uint64_t loadaddress)
{
    char dis_buf[99];

    printf("%08"PRIx64" ", dec.inst_addr);

    isa->disass_inst(dec.inst_addr, dec.inst, dis_buf, sizeof dis_buf);

    switch (dec.class) {
    case isa_inst_class_load:
        printf("%-32s r%d <- 0x%08"PRIx64" [0x%08"PRIx64"]\n",
               dis_buf, dec.dest_reg, res.result, loadaddress);
        break;

    case isa_inst_class_store:
        printf("%-32s [0x%08"PRIx64"] <- 0x%08"PRIx64"\n", dis_buf, res.result, res.store_value);
        break;

    default:
        if (dec.dest_reg != ISA_NO_REG || dec.dest_msr != ISA_NO_REG)
            printf("%-32s", dis_buf);
        else
            printf("%s", dis_buf);

        if (dec.dest_reg != ISA_NO_REG)
            printf(" r%d <- 0x%08"PRIx64"", dec.dest_reg, res.result);

        if (dec.dest_msr != ISA_NO_REG)
            printf(" MSR%d <- 0x%08"PRIx64"", dec.dest_msr, res.msr_result);

        printf("\n");
    }
}

extern const isa_t alpha_isa;
extern const isa_t lm32_isa;

const isa_t *
get_isa(uint16_t machine)
{
    if (machine == EM_ALPHA)
        return &alpha_isa;
    if (machine == EM_LM32)
        return &lm32_isa;
    return NULL;
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
