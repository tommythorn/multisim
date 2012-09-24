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

/* Reservation station - a window over instructions are in program order
 * Life of an entry: invalid -> valid & not issued -> valid & issued -> invalid
 */
typedef struct {
    bool          valid;
    bool          issued;
    unsigned      number;
    isa_decoded_t dec;
    uint64_t      op_a, op_b;
} reservation_station_t;

#define WINDOW_SIZE 128

/* Counting instructions in program order */
unsigned fetch_number;
unsigned issue_number;

// XXX probably all state should be in "state" but I'm prototyping here...
reservation_station_t reservation_stations[WINDOW_SIZE];
bool scoreboard[NO_REG + 1];

bool
step_sscalar_in_order(const isa_t *isa, cpu_state_t *state, cpu_state_t *costate)
{
    uint64_t *r = state->r;
    int n_load = 0;
    int n_store = 0;

    /*
     * Issue instructions as long as they can get their arguments.
     * Issue at most one control flow instruction as we don't
     * speculate yet.
     */

    while ((fetch_number + 1) % WINDOW_SIZE != issue_number % WINDOW_SIZE) {
        uint32_t i = load32(state->mem, state->pc);

        isa_decoded_t dec = isa->decode(state->pc, i);

        n_load += dec.is_load;
        n_store += dec.is_store;

        /* do not */
        if (1 < n_store || 0 < n_store && 0 < n_load) {
            printf("Notice: bailing issue before we got %d loads and %d stores simultaneously\n",
                   n_load, n_store);
            break;
        }

        if (!scoreboard[dec.dest_reg] ||
            !scoreboard[dec.source_reg_a] ||
            !scoreboard[dec.source_reg_b] && !dec.b_is_imm)
            break;

        int p = fetch_number % WINDOW_SIZE;
        reservation_station_t *rs = reservation_stations + p;
        rs->valid = true;
        rs->issued = false;
        rs->number = fetch_number;
        rs->dec = dec;
        rs->op_a = r[dec.source_reg_a];
        rs->op_b = dec.b_is_imm ? dec.imm : r[dec.source_reg_b];

        ++fetch_number;

        state->pc += 4;

        if (dec.dest_reg != NO_REG)
            scoreboard[dec.dest_reg] = false;

        ++state->n_issue;

        if (rs->dec.is_branch)
            break;
    }

    assert(fetch_number != issue_number);

    /*
     * Execute all issued instructions. This assume everything can
     * retire in a single cycle.
     */

    while (fetch_number % WINDOW_SIZE != issue_number % WINDOW_SIZE) {
        reservation_station_t *rs = reservation_stations + issue_number++ % WINDOW_SIZE;

        isa->disass(rs->dec.inst_addr, rs->dec.inst);

        isa_result_t res = isa->inst_exec(rs->dec, rs->op_a, rs->op_b);

        if (res.fatal_error)
            return true;

        if (rs->dec.is_load) {
            printf("\t\t\t\t\t\t[0x%llx]\n", res.result);
            res.result = load(state->mem, res.result, rs->dec.mem_access_size);
        }

        if (rs->dec.is_store) {
            printf("\t\t\t\t\t\t[0x%llx](%d) = 0x%llx\n",
                   res.result, rs->dec.mem_access_size, res.storev);

            store(state->mem, res.result, res.storev, rs->dec.mem_access_size);
        }

        if (rs->dec.is_branch & res.result) {
            state->pc = res.pc;
        }

        if (rs->dec.dest_reg != NO_REG) {
            printf("\t\t\t\t\t\tr%d <- 0x%08llx\n", rs->dec.dest_reg, res.result);
            r[rs->dec.dest_reg] = res.result;
            scoreboard[rs->dec.dest_reg] = true;
        }

        /* Co-simulate */
        assert(rs->dec.inst_addr == costate->pc);
        step_simple(isa, costate, false);
        assert(state->r[rs->dec.dest_reg] == costate->r[rs->dec.dest_reg]);
    }

    return false;
}

void run_sscalar_io(int num_images, char *images[])
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    const isa_t *isa = &alpha_isa;
    elf_info_t info;

    memset(scoreboard, 1, sizeof scoreboard);

    int r = loadelfs(state->mem, num_images, images, &info);
    if (r != num_images)
        fatal("error: loading %s failed", images[r]);
    loadelfs(costate->mem, num_images, images, &info);

    isa->setup(state, &info);
    isa->setup(costate, &info);

    int cycle;
    for (cycle = 0;; ++cycle) {
        printf("Cycle #%d:\n", cycle);
        if (step_sscalar_in_order(isa, state, costate))
            break;
    }

    printf("IPC = %.2f\n", (double) state->n_issue / cycle);

    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
