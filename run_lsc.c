/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2019 Tommy Thorn
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


typedef struct fetch_parcel_st {
    unsigned    seqno;
    uint64_t    addr;
    uint32_t    insn;
} fetch_parcel_t;

/* The essence of support for speculation is the register renaming and
 * the reorder buffer.  The former enables us a overlap future and
 * past values of logical registers whereas the reorder buffer is
 * allocated in program order and enables us to restore the sequential
 * view and roll-back mis-speculation.
 *
 * Renaming is just mapping logical registers to physical.  The
 * reorder buffer tracks instructions in flight, what the _previous_
 * register was that was assigned to a logical register, and whether
 * the instruction has committed.
 *
 * Committed instructions from the tail of the ROB can be retired,
 * which means the old register is freed and the entry disappears from
 * the ROB.
 *
 * Rolling back means traversing backwards from the head, undoing the
 * renames and freeing the registers.
 */

static unsigned rat[32];

#define ROB_SIZE 512

typedef struct rob_entry_st {
    int r;	/* Logical register written by this instruction */
    int pr_old; /* Physical register that r was _previously_ mapped to */
    bool committed;

    // For debugging (well, I do use the seqno, but we could avoid that)
    fetch_parcel_t fp;
    isa_decoded_t dec;
    isa_result_t res;
} rob_entry_t;

rob_entry_t rob[ROB_SIZE];

static unsigned alloc_reg(void);
static void free_reg(unsigned pr);

static unsigned rob_wp = 0, rob_rp = 0;

static void show_rob(const char *msg)
{
    unsigned p = rob_rp;

    fprintf(stderr, "ROB%s:\n", msg);
    while (p != rob_wp) {
        fetch_parcel_t fp = rob[p].fp;
        fprintf(stderr, "  rob[%02d] = %c %d:%08" PRIx64 " %08x\n",
                p, "UC"[rob[p].committed], fp.seqno, fp.addr, fp.insn);
        if (++p == ROB_SIZE)
            p = 0;
    }
}

static unsigned allocate_rob(int r, int pr, fetch_parcel_t fp)
{
    unsigned rob_index = rob_wp;

    rob[rob_index] = (rob_entry_t) { r, pr, false, fp };
    if (++rob_wp == ROB_SIZE)
        rob_wp = 0;
    assert(rob_wp != rob_rp);

    //show_rob(" after allocate");

    return rob_index;
}

/*
 * Example ROB:
 *
 * #0: J	<--- oldest uncommitted/rp
 * #1: CSRR
 * #2: BEQ
 * #3: ADDI
 * #4: LW     	<--- most recent
 * #5: 		<--- wp
 */

static void
lsc_retire(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    while (rob_rp != rob_wp && rob[rob_rp].committed) {
        fetch_parcel_t fp = rob[rob_rp].fp;

        // retired_reg[rob[rob_rp].r] = rat[rob[rob_rp].r];
        fprintf(stderr, "Retired rob[%02d] %5d %d ",
                rob_rp,
                fp.seqno, state->priv);
        isa_disass(arch, rob[rob_rp].dec, rob[rob_rp].res);

        free_reg(rob[rob_rp].pr_old);

        if (++rob_rp == ROB_SIZE)
            rob_rp = 0;
    }

    //show_rob(" after retire");
}

/*
 * Example roll-back: we determine that the #2:BEQ was mispredicted so
 * we have to flush the pipe and roll ROB back to #2.  That means
 * undoing #4 and #3 in that order.
 *
 * #0: J	<--- oldest uncommitted/rp
 * #1: CSRR
 * #2: BEQ   	<--- most recent
 * #3: 		<--- wp
 */

static void rollback_rob(unsigned keep_seqno)
{
    while (rob_rp != rob_wp) {
        unsigned p = rob_wp;
        if (p-- == 0)
            p = ROB_SIZE - 1;
        if (rob[p].fp.seqno == keep_seqno)
            break;
        rob_wp = p;

        fprintf(stderr, "Rollback #%d:%08" PRIx64 "now r%d -> pr%d\n",
                rob[p].fp.seqno, rob[p].fp.addr,
                rob[p].r, rob[p].pr_old);
        free_reg(rat[rob[p].r]);
        rat[rob[p].r] = rob[p].pr_old;
    }

    //show_rob(" after rollback");
}

typedef struct micro_op_st {
    fetch_parcel_t fetched;
    isa_decoded_t dec;

    unsigned pr_a;
    unsigned pr_b;
    unsigned pr_wb;
    unsigned rob_index;
} micro_op_t;

#define PHYSICAL_REGS 64
#define PR_ZERO 0
#define PR_SINK (PHYSICAL_REGS-1)
static uint64_t prf[PHYSICAL_REGS];

/* Fetch buffer */
#define FETCH_BUFFER_SIZE 8
#define FETCH_WIDTH       2 // Insns fetched per cycle

static unsigned fetch_seqno;
static fetch_parcel_t fb[FETCH_BUFFER_SIZE];
static int fb_head = 0, fb_tail = 0, fb_size = 0;

/* Execute window */
#define EX_BUFFER_SIZE 8
static int ex_size;
static micro_op_t ex_buffer[EX_BUFFER_SIZE];

static bool     pr_ready[PHYSICAL_REGS+1]; // The +1 is for r31
static unsigned freelist[PHYSICAL_REGS];
static unsigned freelist_wp, freelist_rp;

static void free_reg(unsigned pr)
{
    if (pr != PR_ZERO && pr != PR_SINK) {
        pr_ready[pr] = false;
        freelist[freelist_wp++] = pr;
        if (freelist_wp == sizeof freelist / sizeof *freelist)
            freelist_wp = 0;
        assert(freelist_rp != freelist_wp);
    }
}

static unsigned alloc_reg(void)
{
    assert(freelist_rp != freelist_wp);

    unsigned pr = freelist[freelist_rp++];
    if (freelist_rp == sizeof freelist / sizeof *freelist)
        freelist_rp = 0;

    return pr;
}

static void
lsc_fetch(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    isa_exception_t exc = { 0 };
    int n = 0;

    /*
     * Fetch (branch prediction would happen here, eventually)
     */
    while (fb_size < FETCH_BUFFER_SIZE && n++ < FETCH_WIDTH) {
        uint32_t addr = state->pc;
        uint32_t insn = (uint32_t)arch->load(state, addr, 0 /* = ifetch */, &exc);
        state->pc += 4;
        assert(!exc.raised);

        fb[fb_tail] = (fetch_parcel_t){ .seqno = fetch_seqno++, .addr = addr, .insn = insn };
        if (++fb_tail == FETCH_BUFFER_SIZE)
            fb_tail = 0;
        fb_size++;
    }
}

static void
show_fb(void)
{
    unsigned p = fb_head;
    fprintf(stderr, "FB:\n");
    for (int i = 0; i < fb_size; ++i) {
        fetch_parcel_t fp = fb[p];

        fprintf(stderr, "  %d:%08" PRIx64 " %08x\n",
                fp.seqno, fp.addr, fp.insn);

        if (++p == FETCH_BUFFER_SIZE)
            p = 0;
    }
}

static void
lsc_decode_rename(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    /*
     * Decode and rename
     */
    if (fb_size > 0 && ex_size < EX_BUFFER_SIZE) {

        fetch_parcel_t fetched = fb[fb_head];
        if (++fb_head == FETCH_BUFFER_SIZE)
            fb_head = 0;
        fb_size--;

        isa_decoded_t dec = arch->decode(fetched.addr, fetched.insn);

        unsigned rob_index = allocate_rob(dec.dest_reg, rat[dec.dest_reg], fetched);

        // Rename (XXX backpressure)
        micro_op_t mop = {
            .fetched = fetched,
            .dec     = dec,
            .pr_a    = rat[dec.source_reg_a],
            .pr_b    = rat[dec.source_reg_b],
            .pr_wb   = dec.dest_reg == ISA_NO_REG ? PR_SINK : alloc_reg(),
            .rob_index = rob_index
        };
        rob[rob_index].dec = dec;

        ex_buffer[ex_size++] = mop;
    }
}

static void
flush_and_redirect(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity,
                   unsigned seqno, uint32_t new_pc)
{
    // Flush
    fb_size = 0;
    fb_head = fb_tail;
    ex_size = 0;

    rollback_rob(seqno);

    state->pc = new_pc;
    fetch_seqno = seqno + 1;
    // mispredicted = true;

}

static void
show_ex(void)
{
    fprintf(stderr, "EX:\n");
    for (int i = 0; i < ex_size; ++i) {
        micro_op_t mop = ex_buffer[i];
        fetch_parcel_t fp = mop.fetched;

        fprintf(stderr, "  rob[%02d] %d:%08" PRIx64 " %08x pr%d, pr%d -> pr%d\n",
                mop.rob_index,
                fp.seqno, fp.addr, fp.insn,
                mop.pr_a, mop.pr_b, mop.pr_wb);
    }
}

static void
lsc_exec1(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity, micro_op_t mop)
{
    isa_exception_t exc = { 0 };
    uint64_t op_a  = prf[mop.pr_a];
    uint64_t op_b  = prf[mop.pr_b];
    uint64_t msr_a =
        mop.dec.source_msr_a != ISA_NO_REG
        ? arch->read_msr(state, mop.dec.source_msr_a, &exc)
        : 0;

    uint64_t atomic_load_addr = op_a;

    if (exc.raised)
        goto exception;

    if (mop.dec.class == isa_insn_class_atomic)
        op_a = arch->load(state, atomic_load_addr, mop.dec.loadstore_size, &exc);

    if (exc.raised)
        goto exception;

    isa_result_t res;

    if (!mop.dec.system)
        res = arch->insn_exec(mop.dec, op_a, op_b, msr_a, &exc);
    else
        res = arch->insn_exec_system(state, mop.dec, op_a, op_b, msr_a, &exc);
    res.result = CANONICALIZE(res.result);
    rob[mop.rob_index].res = res;

    if (exc.raised)
        goto exception;

    switch (mop.dec.class) {
    case isa_insn_class_load:
        res.load_addr = CANONICALIZE(res.load_addr);
        res.result = arch->load(state, res.load_addr, mop.dec.loadstore_size, &exc);
        res.result = CANONICALIZE(res.result);

        if (exc.raised)
            goto exception;

        break;

    case isa_insn_class_store:
        res.store_addr = CANONICALIZE(res.store_addr);
        res.store_value = CANONICALIZE(res.store_value);
        arch->store(state, res.store_addr, res.store_value, mop.dec.loadstore_size, &exc);

        if (exc.raised)
            goto exception;

        break;

    case isa_insn_class_atomic:
        // XXX ??
        res.load_addr = CANONICALIZE(res.load_addr);
        arch->store(state, atomic_load_addr, res.result, mop.dec.loadstore_size, &exc);

        if (exc.raised)
            goto exception;

        res.result = op_a;
        break;

    case isa_insn_class_illegal:
        assert(0); // This would require a bit more thought

    case isa_insn_class_alu:
        break;

    case isa_insn_class_branch:
        if (res.branch_taken)
            flush_and_redirect(arch, state, verbosity, mop.fetched.seqno, mop.dec.jumpbranch_target);
        break;

    case isa_insn_class_jump:
        flush_and_redirect(arch, state, verbosity, mop.fetched.seqno, mop.dec.jumpbranch_target);
        break;

    case isa_insn_class_compjump:
        flush_and_redirect(arch, state, verbosity, mop.fetched.seqno, res.compjump_target);
        break;
    }

    if (mop.dec.dest_reg != ISA_NO_REG) {
        prf[mop.pr_wb] = res.result;
        pr_ready[mop.pr_wb] = true;
    }

    if (mop.dec.dest_msr != ISA_NO_REG)
        arch->write_msr(state, mop.dec.dest_msr, res.msr_result, &exc);

    rob[mop.rob_index].committed = true;

exception:
    if (state->verbosity & VERBOSE_DISASS) {
/*
        fprintf(stderr, "\t\t\t\t\tpr%d = %08"PRIx64" pr%d = %08"PRIx64" -> pr%d\n",
                mop.pr_a, op_a,
                mop.pr_b, op_b,
                mop.pr_wb);
*/
    }

    if (exc.raised) {
        if (state->verbosity & VERBOSE_DISASS)
            fprintf(stderr,
                    "                  EXCEPTION %"PRId64" (%08"PRId64") RAISED\n",
                    exc.code, exc.info);

        // XXX do this at retirement
        flush_and_redirect(arch, state, verbosity,
                           mop.fetched.seqno - 1, // Do _not_ keep instruction with exception
                           arch->handle_exception(state, mop.dec.insn_addr, exc));
    }
}

static void
lsc_execute(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    /* Execute */
    for (int i = 0; i < ex_size; ++i)
        lsc_exec1(arch, state, verbosity, ex_buffer[i]);
    ex_size = 0;
}

static bool
step_lsc(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    lsc_retire(arch, state, verbosity);
    lsc_execute(arch, state, verbosity);
    lsc_decode_rename(arch, state, verbosity);
    lsc_fetch(arch, state, verbosity); // XXX execute affects pc in the same cycle

    arch->tick(state, 1);

    return false;
}

void
run_lsc(int num_images, char *images[], verbosity_t verbosity)
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


    /* Pr_Ready and reservation station initialization */
    memset(pr_ready, 0, sizeof pr_ready);
    pr_ready[PR_ZERO] = true;

    freelist_wp = freelist_rp = 0;
    for (unsigned pr = 0; pr < PHYSICAL_REGS; ++pr)
        free_reg(pr);

    rat[0] = PR_ZERO;
    for (int i = 1; i < 32; ++i) {
        int pr       = alloc_reg();
        rat[i]       = pr;
        prf[pr]      = state->r[i];
        pr_ready[pr] = true;
    }

    for (cycle = 0;; ++cycle) {
        if (verbosity) {
            fprintf(stderr, "\nCycle #%d:\n", cycle);
            show_fb();
            show_ex();
            show_rob("");
        }

        if (step_lsc(arch, state, costate, verbosity))
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
