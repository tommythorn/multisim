/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2012,2018 Tommy Thorn
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
// XXX hack to make -1 (ISA_NO_REG) a legal reference.
bool scoreboard0[ISA_REGISTERS], *scoreboard = scoreboard0 + 1;

bool
step_sscalar_in_order(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    memory_exception_t error;
    uint64_t *r = state->r;
    int n_load = 0;
    int n_store = 0;

    /*
     * Issue instructions as long as they can get their arguments.
     * Issue at most one control flow instruction as we don't
     * speculate yet.
     */

    while ((fetch_number + 1) % WINDOW_SIZE != issue_number % WINDOW_SIZE) {
        memory_exception_t error;
        uint32_t i = arch->load(state, state->pc, 4, &error);

        if (error == MEMORY_FATAL)
            return true;

        if (error != MEMORY_SUCCESS)
            break;

        isa_decoded_t dec = arch->decode(state->pc, i);

        n_load += dec.class == isa_inst_class_load;
        n_store += dec.class == isa_inst_class_store;

        /*
         * Only fetch instructions that can safely be reordered.  It's
         * safe to reorder arbitrary (non-IO) loads in the absence of
         * stores, but two stores can't be reordered (without knowning
         * if they can alias).
         */
        if (1 < n_store || 0 < n_store && 0 < n_load) {
            if (verbosity)
                printf(
                    "Notice: bailing issue before we got "
                    "%d loads and %d stores simultaneously\n",
                    n_load, n_store);
            break;
        }

        if (!scoreboard[dec.dest_reg] ||
            !scoreboard[dec.source_reg_a] ||
            !scoreboard[dec.source_reg_b])
            break;

        int p = fetch_number % WINDOW_SIZE;
        reservation_station_t *rs = reservation_stations + p;
        rs->valid = true;
        rs->issued = false;
        rs->number = fetch_number;
        rs->dec = dec;
        rs->op_a = dec.source_reg_a == ISA_NO_REG ? 0 : r[dec.source_reg_a];
        rs->op_b = dec.source_reg_b == ISA_NO_REG ? 0 : r[dec.source_reg_b];

        ++fetch_number;

        state->pc += 4;
        state->pc = CANONICALIZE(state->pc);

        if (dec.dest_reg != ISA_NO_REG)
            scoreboard[dec.dest_reg] = false;

        ++state->n_issue;

        if (rs->dec.class == isa_inst_class_jump ||
            rs->dec.class == isa_inst_class_branch ||
            rs->dec.class == isa_inst_class_compjump)
            // XXX continue fetching across unconditional branches
            break;
    }

    assert(fetch_number != issue_number);

    /*
     * Execute all issued instructions. This assume everything can
     * retire in a single cycle.
     */

    while (fetch_number % WINDOW_SIZE != issue_number % WINDOW_SIZE) {
        reservation_station_t *rs = reservation_stations + issue_number++ % WINDOW_SIZE;
        isa_result_t res = arch->inst_exec(rs->dec, rs->op_a, rs->op_b, 0);
        res.result = CANONICALIZE(res.result);

        if (res.fatal_error)
            return true;

        switch (rs->dec.class) {
        case isa_inst_class_atomic:
            assert(0); // This would require a bit more thought

        case isa_inst_class_alu:
            break;

        case isa_inst_class_load:
            res.result = arch->load(state, res.result, rs->dec.loadstore_size, &error);
            res.result = CANONICALIZE(res.result);

            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);

            break;

        case isa_inst_class_store:
            arch->store(state, res.result, res.store_value, rs->dec.loadstore_size, &error);

            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);

            break;

        case isa_inst_class_branch:
            if (res.branch_taken)
                state->pc = rs->dec.jumpbranch_target;
            break;

        case isa_inst_class_jump:
            state->pc = rs->dec.jumpbranch_target;
            break;

        case isa_inst_class_compjump:
            state->pc = rs->op_a;
            break;
        }

        if (rs->dec.dest_reg != ISA_NO_REG) {
            r[rs->dec.dest_reg] = res.result;
            scoreboard[rs->dec.dest_reg] = true;
        }

        isa_disass(arch, rs->dec, res);

        /* Co-simulate */
        assert(rs->dec.inst_addr == costate->pc);
        step_simple(arch, costate, 0);
        if (rs->dec.dest_reg != ISA_NO_REG)
            assert(state->r[rs->dec.dest_reg] == costate->r[rs->dec.dest_reg]);
    }

    return false;
}

void run_sscalar_io(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    const arch_t *arch;
    elf_info_t info;

    memset(scoreboard0, 1, sizeof scoreboard0);

    loadelfs(state->mem, num_images, images, &info);
    loadelfs(costate->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info);
    arch->setup(costate, &info);

    int cycle;
    for (cycle = 0;; ++cycle) {
        if (verbosity && (verbosity & VERBOSE_TRACE) == 0)
            printf("Cycle #%d:\n", cycle);
        if (step_sscalar_in_order(arch, state, costate, verbosity))
            break;
    }

    if (verbosity)
        printf("IPC = %.2f\n", (double) state->n_issue / cycle);

    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
