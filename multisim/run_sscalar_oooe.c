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

#define DEBUG_SB 0

/* Reservation station - a window over instructions in program order
 * Life of an entry: invalid -> valid & not issued -> valid & issued -> invalid
 */
typedef struct {
    bool     valid;
    bool     issued;
    unsigned number;
    isa_decoded_t dec;

    unsigned pr_a;
    unsigned pr_b;
    unsigned pr_wb;

    uint64_t wbv;
} reservation_station_t;

#define WINDOW_SIZE 512
#define PHYSICAL_REGS (32+WINDOW_SIZE)

/* Counting instructions in program order */
static unsigned fetch_number;
static unsigned issue_number;
static uint64_t n_issue;

// XXX probably all state should be in "state" but I'm prototyping here...
static reservation_station_t reservation_stations[WINDOW_SIZE];
static unsigned rs_size, rs_start;
static bool     scoreboard[PHYSICAL_REGS+1]; // The +1 is for r31
static bool     scoreboard_next[PHYSICAL_REGS+1]; // emulate flip-flops
static uint64_t prf[PHYSICAL_REGS+1];
static unsigned prf_free[PHYSICAL_REGS];
static unsigned prf_free_wp, prf_free_rp;
static unsigned map[32];

static bool stop_fetching = false;

static void free_reg(unsigned pr)
{
    if (pr != PHYSICAL_REGS) {
        prf_free[prf_free_wp] = pr;
        prf_free_wp = (prf_free_wp + 1) % PHYSICAL_REGS;
        assert(prf_free_rp != prf_free_wp);
    }
}

static unsigned alloc_reg(void)
{
    assert(prf_free_rp != prf_free_wp);

    unsigned pr = prf_free[prf_free_rp];
    prf_free_rp = (prf_free_rp + 1) % PHYSICAL_REGS;
    scoreboard[pr] = false;

    return pr;
}

static bool
step_sscalar_oooe(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    int n_load = 0;
    int n_store = 0;

    /*
     * Fetch instructions, but at most one control flow
     * instruction.
     */

    while (!stop_fetching && rs_size < WINDOW_SIZE) {
        uint32_t i = (uint32_t)arch->load(state, state->pc, 4);

        if (state->fatal_error)
            // XXX We should be able to poison the instruction instead
            return true;

        reservation_station_t *rs =
            reservation_stations + (rs_size++ + rs_start) % WINDOW_SIZE;
        isa_decoded_t dec = arch->decode(state->pc, i);

        n_load += dec.class == isa_inst_class_load;
        n_store += dec.class == isa_inst_class_store;

        /*
         * Only fetch instructions that can safely be reordered.  It's
         * safe to reorder arbitrary (non-IO) loads in the absence of
         * stores, but two stores can't be reordered (without knowning
         * if they can alias).
         */

        // XXX debug out why this break cosimulation
        if (0)
        if (1 < n_store || 0 < n_store && 0 < n_load) {
            if (verbosity)
                printf(
                    "Notice: bailing issue before we got "
                    "%d loads and %d stores simultaneously\n",
                    n_load, n_store);
            break;
        }

        rs->dec = dec;
        rs->valid = true;
        rs->issued = false;
        rs->number = fetch_number;
        rs->pr_a = map[rs->dec.source_reg_a];
        rs->pr_b = map[rs->dec.source_reg_b];

        if (rs->dec.dest_reg != ISA_NO_REG) {
            free_reg(map[rs->dec.dest_reg]); // XXX this is too soon!
            rs->pr_wb = map[rs->dec.dest_reg] = alloc_reg();
        }
        else {
            rs->pr_wb = PHYSICAL_REGS;
        }

        ++fetch_number;

        if (rs->dec.class == isa_inst_class_jump ||
            rs->dec.class == isa_inst_class_branch ||
            rs->dec.class == isa_inst_class_compjump) { // XXX continue on unconditional branches
            if (DEBUG_SB)
                printf("stop fetching past %08"PRIx64"\n", state->pc);

            stop_fetching = true;
            break;
        }

        state->pc += 4;
        state->pc = CANONICALIZE(state->pc);
    }

    assert(fetch_number != issue_number);

    /*
     * Execute all issued instructions. This assume everything can
     * retire in a single cycle.
     */

    memcpy(scoreboard_next, scoreboard, sizeof scoreboard_next);

    int issued = 0;
    unsigned k = rs_start;
    assert(rs_size);
    assert(!reservation_stations[rs_start].issued);

    for (int n = rs_size; n; --n, k = (k + 1) % WINDOW_SIZE) {
        reservation_station_t *rs = &reservation_stations[k];
        assert(rs->valid);

        if (rs->issued)
            continue;

        if (!scoreboard[rs->pr_a]) {
            if (DEBUG_SB)
                printf("... skipping %08"PRIx64" because op a (R%d) isn't ready\n",
                       rs->dec.inst_addr, rs->pr_a);
            continue;
        }

        if (!scoreboard[rs->pr_b]) {
            if (DEBUG_SB)
                printf("... skipping %08"PRIx64" because op b (R%d) isn't ready\n",
                       rs->dec.inst_addr, rs->pr_b);
            continue;
        }

        assert(rs->valid);

        uint64_t op_a = prf[rs->pr_a];
        uint64_t op_b = prf[rs->pr_b];
        unsigned pwbr = rs->pr_wb;
        unsigned wbr  = rs->dec.dest_reg;
        uint64_t loadaddress = 0;

        rs->issued  = true;

        isa_result_t res = arch->inst_exec(rs->dec, op_a, op_b, 0);
        res.result = CANONICALIZE(res.result);

        if (res.fatal_error)
            return true;

        switch (rs->dec.class) {
        case isa_inst_class_alu:
            break;

        case isa_inst_class_load:
            loadaddress = res.result;
            res.result = arch->load(state, res.result, rs->dec.loadstore_size);
            res.result = CANONICALIZE(res.result);

            if (state->fatal_error)
                // XXX We should be able to poison the instruction instead
                return true;

            break;

        case isa_inst_class_store:
            arch->store(state, res.result, res.store_value, rs->dec.loadstore_size);

            if (state->fatal_error)
                // XXX We should be able to poison the instruction instead
                return true;

            break;

        case isa_inst_class_jump:
            state->pc = rs->dec.jumpbranch_target;
            break;

        case isa_inst_class_branch:
            if (res.branch_taken)
                state->pc = rs->dec.jumpbranch_target;
            else
                state->pc += 4;
            break;

        case isa_inst_class_compjump:
            state->pc = op_a;
            break;
        }

        if (isa_inst_class_jump <= rs->dec.class) {
            if (DEBUG_SB)
                printf("resume fetching from %08"PRIx64"\n", state->pc);
            stop_fetching = false;
        }

        rs->wbv = res.result;

        if (wbr != ISA_NO_REG) {
            prf[pwbr] = res.result;
            scoreboard_next[pwbr] = true;
        }

        printf("\t");
        isa_disass(arch, rs->dec, res, loadaddress);

        ++issued;
        ++n_issue;
    }

    /* Retire */
    while (reservation_stations[rs_start].issued) {
        reservation_station_t *rs = &reservation_stations[rs_start];
        /* Co-simulate */
        assert(rs->dec.inst_addr == costate->pc);

        if (step_simple(arch, costate, false))
            break;

        if (rs->dec.dest_reg != ISA_NO_REG &&
            rs->wbv != costate->r[rs->dec.dest_reg]) {
            printf("%08"PRIx64" got r%d <- %016"PRIx64", expected r%d <- %016"PRIx64"\n",
                   rs->dec.inst_addr,
                   rs->dec.dest_reg, rs->wbv,
                   rs->dec.dest_reg, costate->r[rs->dec.dest_reg]);

            assert(0);
        }

        rs->valid = false;
        rs->issued = false;
        rs_start = (rs_start + 1) % WINDOW_SIZE;
        --rs_size;
    }



    assert(issued);

    memcpy(scoreboard, scoreboard_next, sizeof scoreboard);

    return false;
}

void
run_sscalar_oooe(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    const arch_t *arch;
    elf_info_t info;

    int cycle;

    loadelfs(state->mem, num_images, images, &info);
    loadelfs(costate->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info);
    arch->setup(costate, &info);


    /* Scoreboard and reservation station initialization */
    memset(scoreboard, 0, sizeof scoreboard);
    memset(reservation_stations, 0, sizeof reservation_stations);

    for (int i = 0; i < 32; ++i)
        map[i] = i, scoreboard[map[i]] = true;

    prf_free_wp = prf_free_rp = 0;
    for (unsigned pr = 32; pr < PHYSICAL_REGS; ++pr)
        free_reg(pr);

    rs_size = rs_start = 0;

    memcpy(prf, state->r, sizeof state->r[0] * 32);


    for (cycle = 0;; ++cycle) {
        if (verbosity && (verbosity & VERBOSE_TRACE) == 0)
            printf("Cycle #%d (%d):\n", cycle, rs_size);
        if (step_sscalar_oooe(arch, state, costate, verbosity))
            break;
    }

    printf("IPC = %.2f\n", (double) n_issue / cycle);

    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
