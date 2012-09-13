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
        bool     valid;
        bool     issued;
        unsigned number;
        uint64_t pc;
        uint32_t i;
        uint64_t op_a;
        uint64_t op_b;
        int      wbr;
        bool     is_load, is_store, is_branch;
} reservation_station_t;

#define WINDOW_SIZE 128

/* Counting instructions in program order */
unsigned fetch_number;
unsigned issue_number;

// XXX probably all state should be in "state" but I'm prototyping here...
reservation_station_t rs[WINDOW_SIZE];
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

                int      wbr, ra, rb;
                bool     op_b_is_imm;
                uint64_t op_imm;
                bool     is_load, is_store, is_branch;

                isa->decode(i, &wbr, &ra, &rb, &op_b_is_imm, &op_imm,
                            &is_load, &is_store, &is_branch);

                n_load += is_load;
                n_store += is_store;

                /* do not */
                if (1 < n_store || 0 < n_store && 0 < n_load) {
                        printf("Notice: bailing issue before we got %d loads and %d stores simultaneously\n",
                               n_load, n_store);
                        break;
                }

                if (!scoreboard[wbr] || !scoreboard[ra] || !scoreboard[rb] && !op_b_is_imm)
                        break;

                int p = fetch_number % WINDOW_SIZE;
                rs[p].valid = true;
                rs[p].issued = false;
                rs[p].number = fetch_number;
                rs[p].pc = state->pc;
                rs[p].i  = i;
                rs[p].op_a = r[ra];
                rs[p].op_b = op_b_is_imm ? op_imm : r[rb];
                rs[p].wbr  = wbr;
                rs[p].is_load = is_load;
                rs[p].is_store = is_store;
                rs[p].is_branch = is_branch;

                ++fetch_number;

                state->pc += 4;

                if (wbr != NO_REG)
                        scoreboard[wbr] = false;

                ++state->n_issue;

                if (rs[p].is_branch)
                        break;
        }

        assert(fetch_number != issue_number);

        /*
         * Execute all issued instructions. This assume everything can
         * retire in a single cycle.
         */

        while (fetch_number % WINDOW_SIZE != issue_number % WINDOW_SIZE) {
                unsigned k = issue_number++ % WINDOW_SIZE;
                uint32_t i    = rs[k].i;
                uint64_t pc   = rs[k].pc;
                uint64_t op_a = rs[k].op_a;
                uint64_t op_b = rs[k].op_b;
                int      wbr  = rs[k].wbr;
                bool     is_load = rs[k].is_load;
                bool     is_store = rs[k].is_store;
                bool     is_branch = rs[k].is_branch;
                uint64_t storev;
                uint64_t storemask;
                bool     fatal;

                isa->disass(pc, i);

                uint64_t wbv = isa->inst_exec(i, op_a, op_b, &storev, &storemask, &pc, &fatal);
                if (fatal)
                        return true;

                if (is_load) {
                        uint64_t loaded = load64(state->mem, wbv &~ 7);
                        printf("\t\t\t\t\t\t[0x%llx]\n", wbv);
                        wbv = isa->inst_loadalign(i, wbv, loaded);
                }

                if (is_store) {
                        uint64_t oldvalue = load64(state->mem, wbv &~ 7);
                        uint64_t newvalue = oldvalue & ~storemask | storev & storemask;

                        printf("\t\t\t\t\t\t[0x%llx] = 0x%llx & 0x%llx\n",
                               wbv, storev, storemask);

                        store64(state->mem, wbv &~ 7, newvalue);
                }

                if (is_branch & wbv) {
                        state->pc = pc;
                }

                if (wbr != NO_REG) {
                        printf("\t\t\t\t\t\tr%d <- 0x%08llx\n", wbr, wbv);
                        r[wbr] = wbv;
                        scoreboard[wbr] = true;
                }

                /* Co-simulate */
                step_simple(isa, costate, false);
                assert(state->r[wbr] == costate->r[wbr]);
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
