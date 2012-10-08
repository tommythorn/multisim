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

#define TRACING 1

bool
step_simple(const isa_t *isa, cpu_state_t *state, bool verbose)
{
    static int cycle = 0;
    uint64_t orig_r[32];
    uint64_t loadaddress = 0;
    uint64_t pc       = state->pc;
    uint32_t inst     = load32(state->mem, pc);
    isa_decoded_t dec = isa->decode(pc, inst);

    if (TRACING)
        memcpy(orig_r, state->r, sizeof orig_r);

    assert(dec.source_reg_a < ISA_NO_REG);
    assert(dec.source_reg_b < ISA_NO_REG);
    assert(dec.dest_reg     < ISA_REGISTERS);
    assert(dec.dest_msr     < ISA_MSRS);
    assert(dec.source_msr_a < ISA_MSRS);

    uint64_t op_a     = state->r[dec.source_reg_a];
    uint64_t op_b     = state->r[dec.source_reg_b];
    uint64_t msr_a    = dec.source_msr_a != ISA_NO_REG
        ? state->r[dec.source_msr_a] : 0;
    isa_result_t res  = isa->inst_exec(dec, op_a, op_b, msr_a);

    if (res.fatal_error)
        return true;

    switch (dec.class) {
    case isa_inst_class_load:
        loadaddress = res.result;
        res.result = load(state->mem, res.result, dec.loadstore_size);
        state->pc += 4;
        break;

    case isa_inst_class_store:
        store(state->mem, res.result, res.store_value, dec.loadstore_size);
        state->pc += 4;
        break;

    case isa_inst_class_branch:
        state->pc = res.branch_taken ? dec.jumpbranch_target : state->pc + 4;
        break;

    case isa_inst_class_jump:
        state->pc = dec.jumpbranch_target;
        break;

    case isa_inst_class_compjump:
        state->pc = op_a;
        break;

    default:
        state->pc += 4;
        break;
    }

    if (dec.dest_reg != ISA_NO_REG)
        state->r[dec.dest_reg] = res.result;

    if (dec.dest_msr != ISA_NO_REG)
        isa->write_msr(state, dec.dest_msr, res.msr_result);

    isa->tick(state);

    if (TRACING) {
        printf("%d:0x%08"PRIx64"\n", cycle, dec.inst_addr);
        if (dec.dest_reg != ISA_NO_REG && orig_r[dec.dest_reg] != res.result)
            printf("%d:r%d=0x%08"PRIx64"\n", cycle, dec.dest_reg,
                   res.result);
        ++cycle;
    } else if (verbose)
        isa_disass(isa, dec, res, loadaddress);

    return false;
}

void run_simple(int num_images, char *images[])
{
    cpu_state_t *state = state_create();
    const isa_t *isa;
    elf_info_t info;

    int r = loadelfs(state->mem, num_images, images, &info);
    if (r != num_images)
        fatal("error: loading %s failed", images[r]);

    isa = get_isa(info.machine);
    if (!isa)
        fatal("error: unsupported architecture");
    isa->setup(state, &info);

    int cycle;
    for (cycle = 0;; ++cycle) {
        //printf("%5d ", cycle);
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
