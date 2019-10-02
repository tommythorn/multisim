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

#define DEBUG_SB 0

typedef struct fetch_parcel_st {
    uint64_t    addr;
    int         seqno;
    uint32_t    insn;
} fetch_parcel_t;

typedef struct micro_op_st {
    fetch_parcel_t fetched;
    isa_decoded_t dec;

    unsigned pr_a;
    unsigned pr_b;
    unsigned pr_wb;
} micro_op_t;



#define BUFFER_SIZE 512
#define PHYSICAL_REGS (32+BUFFER_SIZE)

/* Fetch buffer */
#define FETCH_BUFFER_SIZE 8
#define FETCH_WIDTH       2 // Insns fetched per cycle

static uint64_t fetch_seqno;
static fetch_parcel_t fb[FETCH_BUFFER_SIZE];
static int fb_head = 0, fb_tail = 0, fb_size = 0;

/* Scheduler window */
#define SCHED_BUFFER_SIZE 8
static int sched_size;
static micro_op_t sched_buffer[SCHED_BUFFER_SIZE];

/* Execute window */
#define EX_BUFFER_SIZE 8
static int ex_size;
static micro_op_t ex_buffer[EX_BUFFER_SIZE];

static bool     reg_ready[PHYSICAL_REGS+1]; // The +1 is for r31
static uint64_t prf[PHYSICAL_REGS+1];
static unsigned prf_free[PHYSICAL_REGS];
static unsigned prf_free_wp, prf_free_rp;
static unsigned map[32];

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
    reg_ready[pr] = false;

    return pr;
}

static void
ooo_fetch(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    isa_exception_t exc = { 0 };
    int n = 0;

    /*
     * Fetch (branch prediction would happen here, eventually)
     */
    while (fb_size < FETCH_BUFFER_SIZE && n++ < FETCH_WIDTH) {
        uint64_t addr = state->pc;
        uint32_t insn = (uint32_t)arch->load(state, addr, 0 /* = ifetch */, &exc);
        state->pc += 4;
        assert(!exc.raised);

        fb[fb_tail++] = (fetch_parcel_t){ .seqno = fetch_seqno++, .addr = addr, .insn = insn };
        fb_tail &= FETCH_BUFFER_SIZE - 1;
        fb_size++;
    }
}

static void
ooo_decode_rename(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    /*
     * Decode and rename
     */
    if (fb_size > 0 && sched_size < SCHED_BUFFER_SIZE) {

        fetch_parcel_t fetched = fb[fb_head++];
        fb_head &= FETCH_BUFFER_SIZE - 1;
        fb_size--;

        isa_decoded_t dec = arch->decode(fetched.addr, fetched.insn);

        // Rename (XXX backpressure)
        micro_op_t mop = {
            .fetched = fetched,
            .dec     = dec,
            .pr_a    = map[dec.source_reg_a],
            .pr_b    = map[dec.source_reg_b],
            .pr_wb   = dec.dest_reg == ISA_NO_REG ? 0 : alloc_reg()
        };

        if (dec.dest_reg != ISA_NO_REG)
            map[dec.dest_reg] = mop.pr_wb;

        /* Load scheduler (we only have one functional unit for now) */
        sched_buffer[sched_size++] = mop;
    }
}

static void
ooo_schedule(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    /* Schedule */
    if (ex_size < EX_BUFFER_SIZE)
        for (int i = 0; i < sched_size; ++i) {
            micro_op_t mop = sched_buffer[i];
            if (reg_ready[mop.pr_a] && reg_ready[mop.pr_b]) {
                // issue this mop and compress it out;
                ex_buffer[ex_size++] = sched_buffer[i];
                sched_buffer[i] = sched_buffer[--sched_size];
                break;
        }
    }
}

static void
flush_and_redirect(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity, uint32_t new_pc)
{
    state->pc = new_pc;
    // mispredicted = true;

    // Flush
    fb_size = 0;
    fb_head = fb_tail;
    sched_size = 0;
    ex_size = 0;
}

static void
ooo_execute(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    isa_exception_t exc = { 0 };

    /* Execute */
    for (int i = 0; i < ex_size; ++i) {
        micro_op_t mop = ex_buffer[i];
        uint64_t op_a  = prf[mop.pr_a];
        uint64_t op_b  = prf[mop.pr_b];
        uint64_t msr_a =
            mop.dec.source_msr_a != ISA_NO_REG
            ? arch->read_msr(state, mop.dec.source_msr_a, &exc)
            : 0;
        assert(!exc.raised);

        isa_result_t res = arch->insn_exec(mop.dec, op_a, op_b, msr_a, &exc);
        res.result = CANONICALIZE(res.result);

        switch (mop.dec.class) {
        case isa_insn_class_illegal:
        case isa_insn_class_atomic:
            assert(0); // This would require a bit more thought

        case isa_insn_class_alu:
            break;

        case isa_insn_class_load:
            res.result = arch->load(state, res.result, mop.dec.loadstore_size, &exc);
            res.result = CANONICALIZE(res.result);
            assert(!exc.raised);
            break;

        case isa_insn_class_store:
            arch->store(state, res.result, res.store_value, mop.dec.loadstore_size, &exc);
            assert(!exc.raised);
            break;

        case isa_insn_class_jump:
            flush_and_redirect(arch, state, verbosity, mop.dec.jumpbranch_target);
            break;

        case isa_insn_class_branch:
            if (res.branch_taken)
                flush_and_redirect(arch, state, verbosity, mop.dec.jumpbranch_target);
            break;

        case isa_insn_class_compjump:
            flush_and_redirect(arch, state, verbosity, res.compjump_target);
            break;
        }

  //exception:
        if (state->verbosity & VERBOSE_DISASS) {
            fprintf(stderr, "%d ", state->priv);
            isa_disass(arch, mop.dec, res);
        }

        if (exc.raised) {
            if (state->verbosity & VERBOSE_DISASS)
                fprintf(stderr,
                        "                  EXCEPTION %"PRId64" (%08"PRId64") RAISED\n",
                        exc.code, exc.info);

            flush_and_redirect(arch, state, verbosity, arch->handle_exception(state, mop.dec.insn_addr, exc));
        }
    }

    ex_size = 0;
}

static bool
step_ooo(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    ooo_execute(arch, state, verbosity);
    ooo_schedule(arch, state, verbosity);
    ooo_decode_rename(arch, state, verbosity);
    ooo_fetch(arch, state, verbosity); // XXX execute affects pc in the same cycle


#if 0

    assert(fetch_number != issue_number);

    /*
     * Execute all issued instructions. This assume everything can
     * retire in a single cycle.
     */

    memcpy(reg_ready_next, reg_ready, sizeof reg_ready_next);

    int issued = 0;
    unsigned k = rs_start;
    assert(rs_size);
    assert(!reservation_stations[rs_start].issued);

    for (int n = rs_size; n; --n, k = (k + 1) % BUFFER_SIZE) {
        reservation_station_t *rs = &reservation_stations[k];
        assert(mop.valid);

        if (mop.issued)
            continue;

        if (!reg_ready[mop.pr_a]) {
            if (DEBUG_SB)
                printf("... skipping %08"PRIx64" because op a (R%d) isn't ready\n",
                       mop.dec.insn_addr, mop.pr_a);
            continue;
        }

        if (!reg_ready[mop.pr_b]) {
            if (DEBUG_SB)
                printf("... skipping %08"PRIx64" because op b (R%d) isn't ready\n",
                       mop.dec.insn_addr, mop.pr_b);
            continue;
        }

        assert(mop.valid);

        uint64_t op_a = prf[mop.pr_a];
        uint64_t op_b = prf[mop.pr_b];
        unsigned pwbr = mop.pr_wb;
        unsigned wbr  = mop.dec.dest_reg;
        uint64_t msr_a= mop.dec.source_msr_a != ISA_NO_REG ?
            arch->read_msr(state, mop.dec.source_msr_a, &exc) : 0;
        assert(!exc.raised);

        mop.issued  = true;



        switch (mop.dec.class) {
        case isa_insn_class_illegal:
        case isa_insn_class_atomic:
            assert(0); // This would require a bit more thought

        case isa_insn_class_alu:
            break;

        case isa_insn_class_load:
            res.result = arch->load(state, res.result, mop.dec.loadstore_size, &exc);
            res.result = CANONICALIZE(res.result);
            assert(!exc.raised);
            break;

        case isa_insn_class_store:
            arch->store(state, res.result, res.store_value, mop.dec.loadstore_size, &exc);
            assert(!exc.raised);
            break;

        case isa_insn_class_jump:
            state->pc = mop.dec.jumpbranch_target;
            break;

        case isa_insn_class_branch:
            if (res.branch_taken)
                state->pc = mop.dec.jumpbranch_target;
            else
                state->pc += 4;
            break;

        case isa_insn_class_compjump:
            state->pc = op_a;
            break;
        }

        if (isa_insn_class_jump <= mop.dec.class) {
            if (DEBUG_SB)
                printf("resume fetching from %08"PRIx64"\n", state->pc);
            stop_fetching = false;
        }

        mop.wbv = res.result;

        if (wbr != ISA_NO_REG) {
            prf[pwbr] = res.result;
            reg_ready_next[pwbr] = true;
        }

        printf("\t");
        isa_disass(arch, mop.dec, res);

        ++issued;
        ++n_issue;
    }

    /* Retire */
    while (reservation_stations[rs_start].issued) {
        reservation_station_t *rs = &reservation_stations[rs_start];
        /* Co-simulate */
        assert(mop.dec.insn_addr == costate->pc);

        if (step_simple(arch, costate))
            break;

        if (mop.dec.dest_reg != ISA_NO_REG &&
            mop.wbv != costate->r[mop.dec.dest_reg]) {
            printf("%08"PRIx64" got r%d <- %016"PRIx64", expected r%d <- %016"PRIx64"\n",
                   mop.dec.insn_addr,
                   mop.dec.dest_reg, mop.wbv,
                   mop.dec.dest_reg, costate->r[mop.dec.dest_reg]);

            assert(0);
        }

        mop.valid = false;
        mop.issued = false;
        rs_start = (rs_start + 1) % BUFFER_SIZE;
        --rs_size;
    }



    assert(issued);

    memcpy(reg_ready, reg_ready_next, sizeof reg_ready);
#endif

    arch->tick(state, 1);

    return false;
}

void
run_ooo(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    const arch_t *arch;
    elf_info_t info;

    int cycle;

    loadelfs(state->mem, num_images, images, &info);
    loadelfs(costate->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);
    arch->setup(costate, &info, verbosity);


    /* Reg_Ready and reservation station initialization */
    memset(reg_ready, 0, sizeof reg_ready);

    for (int i = 0; i < 32; ++i)
        map[i] = i, reg_ready[map[i]] = true;

    prf_free_wp = prf_free_rp = 0;
    for (unsigned pr = 32; pr < PHYSICAL_REGS; ++pr)
        free_reg(pr);

    memcpy(prf, state->r, sizeof state->r[0] * 32);

    for (cycle = 0;; ++cycle) {
        if (verbosity)
            printf("Cycle #%d:\n", cycle);
        if (step_ooo(arch, state, costate, verbosity))
            break;
    }

    //printf("IPC = %.2f\n", (double) n_issue / cycle);

    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
