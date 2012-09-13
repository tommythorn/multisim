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
        uint64_t pc = state->pc;
        int      wbr, ra, rb;
        bool     op_b_is_imm;
        uint64_t op_imm;
        bool     is_load, is_store, is_branch;
        uint64_t storev, storemask;

        uint32_t inst = load32(state->mem, pc);

        if (verbose)
            isa->disass(pc, inst);

        isa->decode(inst, &wbr, &ra, &rb, &op_b_is_imm, &op_imm,
                    &is_load, &is_store, &is_branch);

        uint64_t op_a = state->r[ra];
        uint64_t op_b = op_b_is_imm ? op_imm : state->r[rb];
        bool     fatal;

        uint64_t wbv = isa->inst_exec(inst, op_a, op_b, &storev, &storemask, &pc, &fatal);

        if (fatal)
                return true;

        if (is_load) {
                if (verbose)
                        printf("\t\t\t\t\t\t[0x%llx]\n", wbv);
                wbv = isa->inst_loadalign(inst, wbv, load64(state->mem, wbv &~ 7));
        }

        if (is_store) {
                uint64_t oldvalue = load64(state->mem, wbv &~ 7);
                uint64_t newvalue = oldvalue & ~storemask | storev & storemask;

                if (verbose)
                        printf("\t\t\t\t\t\t[0x%llx] = 0x%llx & 0x%llx\n",
                               wbv, storev, storemask);

                store64(state->mem, wbv &~ 7, newvalue);
        }

        if (is_branch & wbv)
                state->pc = pc;
        else
                state->pc = pc + 4;

        if (wbr != NO_REG) {
                if (verbose)
                        printf("\t\t\t\t\t\tr%d <- 0x%08llx\n", wbr, wbv);
                state->r[wbr] = wbv;
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
