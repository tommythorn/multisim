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

/* Reservation station - a window over instructions are in program order
 * Life of an entry: invalid -> valid & not issued -> valid & issued -> invalid
*/
typedef struct {
        bool     valid;
        bool     issued;
        uint64_t number;
        uint64_t addr; // Address of the instruction
        uint64_t pc;   // pc *after* the instruction (not necessarily addr + 4 for branches)
        uint32_t i;
        uint64_t spex;
        bool     branch_taken_prediction;
        bool     is_load, is_store, is_branch;

        int      wbr;
        bool     op_b_is_imm;
        uint64_t op_imm;

        unsigned pr_a;
        unsigned pr_b;
        unsigned pr_wb;

        uint64_t op_a;
        uint64_t op_b;

        uint64_t wbv;
} reservation_station_t;

#define WINDOW_SIZE 1
#define PHYSICAL_REGS (32+WINDOW_SIZE)
#define MAX_SPECULATION 0

/* Counting instructions in program order */
static uint64_t fetch_number;
static uint64_t issue_number;
static uint64_t n_issue;

// XXX probably all state should be in "state" but I'm prototyping here...
static reservation_station_t rs[WINDOW_SIZE];
static int      rs_size, rs_start;
static bool     scoreboard[PHYSICAL_REGS+1]; // The +1 is for r31
static bool     scoreboard_next[PHYSICAL_REGS+1]; // emulate flip-flops
static uint64_t prf[PHYSICAL_REGS+1];
static unsigned prf_free[PHYSICAL_REGS];
static unsigned prf_free_wp, prf_free_rp;
static unsigned map[32];
static uint64_t checkpoint_number;
static uint64_t checkpoint_spex; // Speculation index
static uint64_t checkpoint_pc;
static unsigned checkpoint_map[32];

static uint64_t valid_spex = 0;
static uint64_t fetch_spex = 0;

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

        return pr;
}

void rollback(cpu_state_t *state)
{
        printf("ROLLBACK to %4lld/%2lld/%08llx\n",
               checkpoint_number + 1, checkpoint_spex, checkpoint_pc);

        // XXX Check the edge case, eg. if last checkpointed instruction is a branch
        memcpy(map, checkpoint_map, sizeof map);
        state->pc = checkpoint_pc;
        valid_spex = fetch_spex = checkpoint_spex;
        fetch_number = checkpoint_number + 1;
        rs_size = 0;
}

static bool
step_sscalar_oooe_spec(const isa_t *isa,
                       cpu_state_t *state,
                       cpu_state_t *costate)
{
restart:
        /*
         * Fetch instructions, but at most one control flow
         * instruction.
         */

        while (!stop_fetching && rs_size < WINDOW_SIZE &&
               valid_spex + MAX_SPECULATION >= fetch_spex) {

                bool branch_taken_prediction = false;
                printf("\tfetching %4lld/%08llx\n", fetch_number, state->pc);
                uint32_t i = load32(state->mem, state->pc);

                int      wbr, ra, rb;
                bool     op_b_is_imm;
                uint64_t op_imm;
                uint64_t br_target;
                int      p = (rs_size++ + rs_start) % WINDOW_SIZE;

                isa->decode(i, state->pc, &wbr, &ra, &rb, &op_b_is_imm, &op_imm,
                            &rs[p].is_load, &rs[p].is_store, &rs[p].is_branch,
                            &br_target);

                if (rs[p].is_branch) {
                        ++fetch_spex;
                        branch_taken_prediction = random() & 1;
                        printf("\tPredicting branch at %4lld/%2lld/%08llx to%s taken\n",
                               fetch_number, fetch_spex, state->pc,
                               branch_taken_prediction ? "" : " not");
                }

                if (rs[p].is_store && fetch_spex != valid_spex) {
                        printf("\tNot fetching speculative store instruction %4lld/%08llx  (valid %lld != spex %lld)\n",
                               fetch_number, state->pc,
                               valid_spex, fetch_spex);
                        // XXX Speculative store require heavy machinery
                        // XXX Even non-speculative stores aren't serialized correctly
                        break;
                }

                rs[p].valid = true;
                rs[p].issued = false;
                rs[p].spex = fetch_spex;
                rs[p].number = fetch_number;
                rs[p].addr = state->pc;

                state->pc += 4;

                if (rs[p].is_branch && branch_taken_prediction)
                        state->pc = br_target;

                rs[p].pc = state->pc;
                rs[p].i = i;
                rs[p].wbr = wbr;
                rs[p].op_b_is_imm = op_b_is_imm;
                rs[p].op_imm = op_imm;
                rs[p].branch_taken_prediction = branch_taken_prediction;

                rs[p].pr_a = map[ra];
                rs[p].pr_b = map[rb];
                if (wbr != NO_REG) {
                        free_reg(map[wbr]); // XXX this is too soon!
                        rs[p].pr_wb = map[wbr] = alloc_reg();
                }
                else {
                        rs[p].pr_wb = PHYSICAL_REGS;
                }

                ++fetch_number;

                if (rs[p].is_branch) {
                        if (DEBUG_SB)
                                printf("stop fetching past %08llx\n", state->pc);

                        stop_fetching = true;
                }
        }

        assert(fetch_number != issue_number);

        /*
         * Execute all issued instructions. This assume everything can
         * retire in a single cycle.
         */

        memcpy(scoreboard_next, scoreboard, sizeof scoreboard_next);

        int issued = 0;
        unsigned k = rs_start;
        assert(rs_size == 0 || !rs[rs_start].issued);
        for (int n = 1 /*rs_size*/; n; --n, k = (k + 1) % WINDOW_SIZE) {
                assert(rs_size >= 0);
                assert(rs[k].valid);

                if (rs[k].issued)
                        continue;

                if (!scoreboard[rs[k].pr_a]) {
                        if (DEBUG_SB)
                        printf("... skipping %4lld/%2lld/%08llx because op a (pr%d) isn't ready\n",
                               rs[k].number, rs[k].spex, rs[k].addr, rs[k].pr_a);
                        continue;
                }

                if (!scoreboard[rs[k].pr_b]) {
                        if (DEBUG_SB)
                        printf("... skipping %4lld/%2lld/%08llx because op b (pr%d) isn't ready\n",
                               rs[k].number, rs[k].spex, rs[k].addr, rs[k].pr_b);
                        continue;
                }

                assert(rs[k].valid);

                uint32_t i    = rs[k].i;
                uint64_t pc   = rs[k].pc;
                uint64_t op_a = prf[rs[k].pr_a];
                uint64_t op_b = rs[k].op_b_is_imm ? rs[k].op_imm :
                                prf[rs[k].pr_b];
                unsigned pwbr = rs[k].pr_wb;
                unsigned wbr  = rs[k].wbr;

                rs[k].issued  = true;

                printf("\t");
                isa->disass(pc, i);

                bool     fatal;
                uint64_t storev, storemask;
                uint64_t wbv = isa->inst_exec(i, op_a, op_b, &storev, &storemask, &pc, &fatal);

                if (fatal)
                        return true;

                if (rs[k].is_load) {
                        uint64_t loaded = load64(state->mem, wbv &~ 7);
                        printf("\t\t\t\t\t\t[0x%llx]\n", wbv);
                        wbv = isa->inst_loadalign(i, wbv, loaded);
                }

                if (rs[k].is_store) {
                        uint64_t oldvalue = load64(state->mem, wbv &~ 7);
                        uint64_t newvalue = oldvalue & ~storemask | storev & storemask;

                        printf("\t\t\t\t\t\t[0x%llx] = 0x%llx & 0x%llx\n",
                               wbv, storev, storemask);

                        store64(state->mem, wbv &~ 7, newvalue);
                }

                if (rs[k].is_branch) {
                        if (wbv)
                                state->pc = pc;
                        if (wbv == rs[k].branch_taken_prediction) {
                                ++valid_spex;
                                printf("\t\t\tBranch predicted correctly, spex %lld\n",
                                       valid_spex);
                                if (DEBUG_SB)
                                    printf("resume fetching from %08llx\n", state->pc);
                        } else {
                                rollback(state);
                                goto restart;
                        }

                        stop_fetching = false;
                }

                rs[k].wbv = wbv;

                if (wbr != NO_REG) {
                        printf("\t\t\t\t\t\tr%d/R%d <- 0x%08llx\n",
                               rs[k].wbr, pwbr, wbv);
                        prf[pwbr] = wbv;
                        scoreboard_next[pwbr] = true;
                }

                ++issued;
                ++n_issue;
        }

        /* Retire */
        while (rs_size && rs[rs_start].issued) {
                assert(rs_size >= 0);
                printf("\tRetiring %4lld/%2lld/", rs[rs_start].number, rs[rs_start].spex);
                isa->disass(rs[rs_start].addr, rs[rs_start].i);

                assert(rs[rs_start].spex <= valid_spex);

                /* Co-simulate */
                step_simple(isa, costate, false);
                if (rs[rs_start].wbr != NO_REG &&
                    rs[rs_start].wbv != costate->r[rs[rs_start].wbr]) {
                    printf("%08llx got r%d <- %016llx, expected r%d <- %016llx\n",
                           rs[rs_start].pc,
                           rs[rs_start].wbr, rs[rs_start].wbv,
                           rs[rs_start].wbr, costate->r[rs[rs_start].wbr]);

                    assert(0);
                }

                checkpoint_map[rs[rs_start].wbr] = rs[rs_start].pr_wb;
                checkpoint_number = rs[rs_start].number;
                checkpoint_spex = rs[rs_start].spex;
                checkpoint_pc = rs[rs_start].pc;

                /*
                if (rs[rs_start].i.iop.opcode == OP_JSR_)
                        return true;
                */

                rs[rs_start].valid = false;
                rs[rs_start].issued = false;
                rs_start = (rs_start + 1) % WINDOW_SIZE;
                --rs_size;
        }

        assert(issued);

        memcpy(scoreboard, scoreboard_next, sizeof scoreboard);

        return false;
}

void
run_sscalar_oooe_spec(int num_images, char *images[])
{
        cpu_state_t *state = state_create();
        cpu_state_t *costate = state_create();
        const isa_t *isa = &alpha_isa;
        elf_info_t info;

        int cycle;

        int r = loadelfs(state->mem, num_images, images, &info);
        if (r != num_images)
                fatal("error: loading %s failed", images[r]);
        loadelfs(costate->mem, num_images, images, &info);

        isa->setup(state, &info);
        isa->setup(costate, &info);


        /* Scoreboard and reservation station initialization */
        memset(scoreboard, 0, sizeof scoreboard);
        memset(rs, 0, sizeof rs);

        for (int i = 0; i < 32; ++i)
                map[i] = i, scoreboard[map[i]] = true;

        memcpy(checkpoint_map, map, sizeof checkpoint_map);

        prf_free_wp = prf_free_rp = 0;
        for (unsigned pr = 32; pr < PHYSICAL_REGS; ++pr)
                free_reg(pr);

        rs_size = rs_start = 0;

        memcpy(prf, state->r, sizeof state->r[0] * 32);

        for (cycle = 0;; ++cycle) {
                assert(rs_size >= 0);
                printf("Cycle #%d (%d):\n", cycle, rs_size);
                if (step_sscalar_oooe_spec(isa, state, costate))
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
