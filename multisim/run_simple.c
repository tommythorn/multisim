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
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "sim.h"
#include "run_simple.h"
#include "loadelf.h"

bool
step_simple(const isa_t *isa, cpu_state_t *state, bool verbose)
{
    uint64_t pc       = state->pc;
    uint32_t inst     = load32(state->mem, pc);

    if (verbose)
        isa->disass(pc, inst);

    isa_decoded_t dec = isa->decode(pc, inst);
    uint64_t op_a     = state->r[dec.source_reg_a];
    uint64_t op_b     = dec.b_is_imm ? dec.imm : state->r[dec.source_reg_b];

    isa_result_t res  = isa->inst_exec(dec, op_a, op_b);

    if (res.fatal_error)
        return true;

    if (dec.is_load) {
        if (verbose)
            printf("\t\t\t\t\t\t[0x%llx]\n", res.result);
        res.result = isa->inst_loadalign(dec, res.result, load64(state->mem, res.result &~ 7));
    }

    if (dec.is_store) {
        uint64_t oldvalue = load64(state->mem, res.result &~ 7);
        uint64_t newvalue = oldvalue & ~res.storemask | res.storev & res.storemask;

        if (verbose)
            printf("\t\t\t\t\t\t[0x%llx] = 0x%llx & 0x%llx\n",
                   res.result, res.storev, res.storemask);

        store64(state->mem, res.result &~ 7, newvalue);
    }

    if (dec.is_branch & res.result)
        state->pc = res.pc;
    else
        state->pc += 4;

    if (dec.dest_reg != NO_REG) {
        if (verbose)
            printf("\t\t\t\t\t\tr%d <- 0x%08llx\n", dec.dest_reg, res.result);
        state->r[dec.dest_reg] = res.result;
    }

    return false;
}

void run_simple(int num_images, char *images[])
{
    cpu_state_t *state = state_create();
    const isa_t *isa = &alpha_isa;
    elf_info_t info;

    int r = loadelfs(state->mem, num_images, images, &info);
    if (r != num_images)
        fatal("error: loading %s failed", images[r]);

    isa->setup(state, &info);

    int cycle;
    for (cycle = 0;; ++cycle) {
        printf("Cycle #%d:\n", cycle);
        if (step_simple(isa, state, true))
            break;
    }

    printf("IPC = %.2f\n", (double) state->n_issue / cycle);

    state_destroy(state);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
