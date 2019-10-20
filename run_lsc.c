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
 *
 *  ** TODO **
 *
 * Correctness:
 *
 * - Introduce store buffers and only write store data to memory at retirement
 * - Force cosim match on IO memory locations (pick one to do IO and
 *   let the other follow)
 *
 * Perf:
 *
 * - add an ART and flash restore the RAT on roll-back
 * - Allow loads to execute in the prescence of unretired, but non-overlapping stores
 * - .... Further, allow loads to execute as long as all earlier stores have committed
 *   (and forward as needed)
 * - crack stores into store data and store address
 *
 * - Predict branches
 * - LSC
 *
 * Cleanup:
 *
 * - Don't depend on seqno outside of self-checking and visualization
 * - Review what's tracked in data structures
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

//////// Configuration and magic numbers

#define FETCH_BUFFER_SIZE       2
#define FETCH_WIDTH             1
#define ROB_SIZE              128
#define PHYSICAL_REGS         128
#define EX_BUFFER_SIZE          1
#define ME_BUFFER_SIZE          1

/*
 * Early release frees the old physical register at allocation time
 * and roll-back then have to undo that (but this is cheap).
 */
//#define EARLY_RELEASE           1

// Two special physical registers: the constant zero and the sink for
// for r0 destination (is never read)

#define PR_ZERO                 0
#define PR_SINK (PHYSICAL_REGS-1)

//////// Types

typedef struct fetch_parcel_st {
    unsigned            seqno;
    uint64_t            addr;
    uint32_t            insn;

    // For visualization
    uint64_t            fetch_ts;
    uint64_t            decode_ts;
    uint64_t            issue_ts;
    uint64_t            execute_ts;
    uint64_t            commit_ts;
//  uint64_t            retire_ts; // This is implicit
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
typedef struct rob_entry_st {
    int                 r;      /* Logical register written by this instruction */
    int                 pr;     /* Physical register that r is mapped to */
    int                 pr_old; /* Physical register that r was _previously_ mapped to */
    bool                committed; /* The instruction is done executing and has written the result, if any, to the
                                    * register file */

    bool                restart;
    uint64_t            restart_pc;
    bool                exception;


    uint64_t		store_addr;
    uint64_t		store_data;
    uint8_t		store_size;
    // For cosim
    bool                mmio; // force reference model to follow us

    // For debugging (XXX well, I do use the seqno, but we could avoid that)
    fetch_parcel_t      fp;
    isa_decoded_t       dec;
} rob_entry_t;

typedef struct micro_op_st {
    fetch_parcel_t      fetched; // XXX redundant?
    isa_decoded_t       dec;

    unsigned            pr_a;
    unsigned            pr_b;
    unsigned            pr_wb;
    unsigned            rob_index;
} micro_op_t;

//////// State

/* The arrays */
static fetch_parcel_t   fb[FETCH_BUFFER_SIZE];
static rob_entry_t      rob[ROB_SIZE];
static unsigned         rat[32];
static unsigned         art[32];
static int64_t          prf[PHYSICAL_REGS];
static bool             pr_ready[PHYSICAL_REGS]; // Scoreboard
static unsigned         freelist[PHYSICAL_REGS];
static micro_op_t       ex_buffer[EX_BUFFER_SIZE];
static micro_op_t       me_buffer[EX_BUFFER_SIZE];

static const arch_t    *arch;
static unsigned         fetch_seqno;
static int              fb_head = 0, fb_tail = 0, fb_size = 0;
static unsigned         rob_wp = 0, rob_rp = 0;
static unsigned         freelist_wp = 0, freelist_rp = 0;
static int              ex_size;
static int              me_size;
static uint64_t         exception_seqno = ~0ULL;
static isa_exception_t  exception_info;
static int              cycle;

static char             letter_size[256] = {
    [1] = 'B',
    [2] = 'H',
    [4] = 'W',
    [8] = 'D',
};

///////////////////////////////////////////////////////

static bool
is_rob_full(void)
{
    return (rob_wp + 1) % ROB_SIZE == rob_rp;
}

static void
free_reg(unsigned pr)
{
    assert((unsigned) pr < PHYSICAL_REGS);
    // DEBUG: Make sure pr isn't already on the freelist
    {
        int p = freelist_rp;
        while (p != freelist_wp) {
            //assert(freelist[p] != pr);
            if (++p == sizeof freelist / sizeof *freelist)
                p = 0;
        }
    }

    if (pr != PR_ZERO && pr != PR_SINK) {
        freelist[freelist_wp] = pr;
        if (++freelist_wp == sizeof freelist / sizeof *freelist)
            freelist_wp = 0;
        assert(freelist_rp != freelist_wp);
    }
}

static unsigned
alloc_reg(void)
{
    assert(freelist_rp != freelist_wp);

    unsigned pr = freelist[freelist_rp++];
    if (freelist_rp == sizeof freelist / sizeof *freelist)
        freelist_rp = 0;

    pr_ready[pr] = false;

    return pr;
}

static void
show_rob(const char *msg)
{
    unsigned p = rob_rp;

    fprintf(stderr, "ROB%s:\n", msg);
    while (p != rob_wp) {
        fetch_parcel_t fp = rob[p].fp;
        fprintf(stderr, "  rob[%02d] = %c %d:%08x %08x\n",
                p, "UC"[rob[p].committed], fp.seqno, (uint32_t)fp.addr, fp.insn);
        if (++p == ROB_SIZE)
            p = 0;
    }
}

static void
show_ex(void)
{
    if (ex_size)
        fprintf(stderr, "EX:\n");
    for (int i = 0; i < ex_size; ++i) {
        micro_op_t mop = ex_buffer[i];
        fetch_parcel_t fp = mop.fetched;

        fprintf(stderr, "  rob[%02d] %d:%08x %08x pr%d%c, pr%d%c -> pr%d %s ",
                mop.rob_index,
                fp.seqno, (uint32_t)fp.addr, fp.insn,
                mop.pr_a, "WR"[pr_ready[mop.pr_a]],
                mop.pr_b, "WR"[pr_ready[mop.pr_b]],
                mop.pr_wb, pr_ready[mop.pr_a] & pr_ready[mop.pr_b] ? "READY" : "     ");
        isa_disass(arch, mop.dec, (isa_result_t) { .result = 0xEEEEEEEE });
    }
}

static void
show_me(void)
{
    if (me_size)
        fprintf(stderr, "ME:\n");
    for (int i = 0; i < me_size; ++i) {
        micro_op_t mop = me_buffer[i];
        fetch_parcel_t fp = mop.fetched;

        fprintf(stderr, "  rob[%02d] %d:%08x %08x pr%d%c, pr%d%c -> pr%d %s ",
                mop.rob_index,
                fp.seqno, (uint32_t)fp.addr, fp.insn,
                mop.pr_a, "WR"[pr_ready[mop.pr_a]],
                mop.pr_b, "WR"[pr_ready[mop.pr_b]],
                mop.pr_wb, pr_ready[mop.pr_a] & pr_ready[mop.pr_b] ? "READY" : "     ");
        isa_disass(arch, mop.dec, (isa_result_t) { .result = 0xEEEEEEEE });
    }
}

static unsigned
allocate_rob(int r, int pr, int pr_old, fetch_parcel_t fp)
{
    unsigned rob_index = rob_wp;

    assert(r == ISA_NO_REG || (unsigned) r < 32);  // XXX should be part of the arch
    assert((unsigned) pr < PHYSICAL_REGS);

    rob[rob_index] = (rob_entry_t) {
        .r = r, .pr = pr, .pr_old = pr_old, .committed = false,
        .fp = fp
    };

    if (is_rob_full()) {
        show_rob(" full");
        show_ex();
        show_me();
    }

    if (++rob_wp == ROB_SIZE)
        rob_wp = 0;

    assert(rob_wp != rob_rp);

    return rob_index;
}

static void
visualize_retirement(cpu_state_t *state, rob_entry_t rob)
{
#define WIDTH 32

    static int next_seqno = 0;
    char line[WIDTH+1];
    fetch_parcel_t fp = rob.fp;
    isa_decoded_t dec = rob.dec;
    int            pr = rob.pr;

    memset(line, '.', WIDTH);
    line[WIDTH] = '\0';

    line[fp.fetch_ts   % WIDTH] = 'F';
    line[fp.decode_ts  % WIDTH] = 'D';
    //line[fp.issue_ts   % WIDTH] = 'I';
    line[fp.execute_ts % WIDTH] = 'E';
    line[fp.commit_ts  % WIDTH] = 'C';
    line[cycle         % WIDTH] = 'R';

    fprintf(stderr, "%6d %s ", next_seqno++, line);
    isa_disass(arch, dec, (isa_result_t) { .result = prf[pr] });
}


/*
 * Example roll-back: we determine that the #2:BEQ was mispredicted so
 * we have to flush the pipe and roll ROB back to #2.  That means
 * undoing #4 and #3 in that order.
 *
 * #0: J        <--- oldest uncommitted/rp
 * #1: CSRR
 * #2: BEQ      <--- most recent
 * #3:          <--- wp
 */

static void
rollback_rob(int keep_rob_index, verbosity_t verbosity)
{
    do {
        unsigned p = rob_wp;
        if (p-- == 0)
            p = ROB_SIZE - 1;

        assert((unsigned)p < ROB_SIZE);
        if (p == keep_rob_index)
            break;
        rob_wp = p;

        if (verbosity & VERBOSE_DISASS)
            if (rob[p].dec.class == isa_insn_class_store && rob[p].store_size) {
                fprintf(stderr,
                        "Rolling back %08"PRIx64" S%c (%08"PRIx64") = %08"PRIx64"\n",
                        rob[p].fp.addr & 0xFFFFFFFF,
                        letter_size[rob[p].store_size],
                        rob[p].store_addr & 0xFFFFFFFF,
                        rob[p].store_data & 0xFFFFFFFF);
            }

        if (rob[p].r == ISA_NO_REG)
            continue;

#ifdef EARLY_RELEASE
        // Undo the free'd register and the allocation
        assert(freelist_wp != freelist_rp);
        if (freelist_wp-- == 0)
            freelist_wp = sizeof freelist / sizeof *freelist - 1;
        if (freelist_rp-- == 0)
            freelist_rp = sizeof freelist / sizeof *freelist - 1;
#else
        free_reg(rat[rob[p].r]);
#endif

        if (rob[p].r != ISA_NO_REG) {
            //fprintf(stderr, "%5d:rat[%d]: %d <- %d\n", rob[p].fp.seqno, rob[p].r, rob[p].pr_old, rat[rob[p].r]);
            rat[rob[p].r] = rob[p].pr_old;
        }
    } while (rob_rp != rob_wp);

    if (memcmp(rat, art, sizeof rat) != 0) {
        fprintf(stderr, "Uh oh:\n");
        for (int i = 0; i < 32; ++i)
            if (rat[i] != art[i])
                fprintf(stderr, "  rat[%d] = %d, but art[%d] = %d\n", i, rat[i], i, art[i]);
    }
    assert(memcmp(rat, art, sizeof rat) == 0);

    exception_seqno = ~0ULL;
}


static void
flush_and_redirect(cpu_state_t *state, verbosity_t verbosity, int rob_index, unsigned seqno, uint64_t new_pc)
{
    // Flush
    fb_size = 0;
    fb_head = fb_tail;
    ex_size = 0;
    me_size = 0;

    rollback_rob(rob_index, verbosity);

    state->pc = new_pc;
    fetch_seqno = seqno + 1;
    // mispredicted = true;

}


/*
 * The ReOrder Buffer holds instructions in fetch (= program) order.
 * Its purpose is to enable the illusion of sequential instruction
 * semantics.  We do this by traversing the ROB in order and
 * "retiring" committed instructions.  Retired instructions are
 * irrevokably done and will never be restarted, thus we can release
 * any resources that was held in case we needed to revert it, eg. the
 * physical register previously mapped to our target register.
 *
 * There are two special cases to consider: branch mispredicted and
 * exceptions.  Both require flushing the pipeline, undoing all
 * instructions in the ROB, and restarting the pipeline.  In case of
 * an exception, the excepting instruction is undone as well, instead
 * of being retired.
 *
 *
 * Example ROB:
 *
 * #0: XOR      <--- rp
 * #1: AND
 * #2: J        <--- oldest uncommitted
 * #3: CSRR
 * #4: BEQ
 * #5: ADDI
 * #6: LW       <--- most recent
 * #7:          <--- wp
 */
static void
lsc_retire(cpu_state_t *state, cpu_state_t *costate, verbosity_t verbosity)
{
    while (rob_rp != rob_wp && rob[rob_rp].committed) {
        rob_entry_t re = rob[rob_rp];

        if (verbosity & VERBOSE_DISASS) {
            visualize_retirement(state, re);
        }

        if (re.exception) {
            if (state->verbosity & VERBOSE_DISASS)
                fprintf(stderr,
                        "                  EXCEPTION %"PRId64" (%08"PRId64") RAISED\n",
                        exception_info.code, exception_info.info);

            int prev_rob_pr = rob_rp == 0 ? ROB_SIZE - 1 : rob_rp - 1;
            flush_and_redirect(state, verbosity, prev_rob_pr, re.fp.seqno - 1,
                               arch->handle_exception(state, re.dec.insn_addr, exception_info));
            return;
        }

        if (re.r != ISA_NO_REG) {
            art[re.r] = re.pr;
        }

        if (re.restart) {
            flush_and_redirect(state, verbosity, rob_rp, re.fp.seqno, re.restart_pc);
        }

        /* Co-simulate retired instructions.  A complication is that
         * costate->pc might not be the next instuction retired (if
         * the instruction traps, then the next retired instruction
         * will be the first from the trap handler
         */

        uint64_t copc;
        do copc = costate->pc; while (step_simple(arch, costate) == 0);

        bool override = re.mmio;

        if (override && re.r != ISA_NO_REG)
            costate->r[re.r] = prf[re.pr];

        if (re.dec.insn_addr != copc) {
            printf("COSIM: REF PC %08"PRIx64" != LSC PC %08"PRIx64"\n",
		   copc & 0xFFFFFFFF, re.dec.insn_addr & 0xFFFFFFFF);
            assert(0);
        }

        if (re.r != ISA_NO_REG && prf[re.pr] != costate->r[re.r]) {
            printf("COSIM: REF RES %08"PRIx64" != LSC RES %08"PRIx64"\n",
		   costate->r[re.r] & 0xFFFFFFFF, prf[re.pr] & 0xFFFFFFFF);
            assert(0);
        }

#ifndef EARLY_RELEASE
        free_reg(re.pr_old);
#endif

        if (++rob_rp == ROB_SIZE)
            rob_rp = 0;
    }
}

static void
lsc_fetch(cpu_state_t *state, verbosity_t verbosity)
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

        fb[fb_tail] = (fetch_parcel_t){
            .seqno = fetch_seqno++,
            .addr = addr,
            .insn = insn,
            .fetch_ts = cycle
        };

        if (++fb_tail == FETCH_BUFFER_SIZE)
            fb_tail = 0;
        fb_size++;
    }
}

static void
show_fb(void)
{
    unsigned p = fb_head;
    if (fb_size)
        fprintf(stderr, "FB: %d\n", fb_size);
    if (0)
    for (int i = 0; i < fb_size; ++i) {
        fetch_parcel_t fp = fb[p];

        fprintf(stderr, "  %d:%08" PRIx64 " %08x\n",
                fp.seqno, fp.addr, fp.insn);

        if (++p == FETCH_BUFFER_SIZE)
            p = 0;
    }
}


static void
lsc_decode_rename(cpu_state_t *state, verbosity_t verbosity)
{
    /*
     * Decode and rename
     */
    while (fb_size > 0 &&
           ex_size < EX_BUFFER_SIZE &&
           me_size < ME_BUFFER_SIZE &&
           !is_rob_full()) {

        fetch_parcel_t fetched = fb[fb_head];
        isa_decoded_t dec      = arch->decode(fetched.addr, fetched.insn);

        // Serialize system instruction; block issuing until all
        // previous instructions have executed (really, retired) and then only allow a single one in

        if (dec.system && (ex_size > 0 || me_size > 0))
            break;

        if (++fb_head == FETCH_BUFFER_SIZE)
            fb_head = 0;
        fb_size--;

        fetched.decode_ts = cycle;

        int           old_pr    = dec.dest_reg != ISA_NO_REG ? rat[dec.dest_reg] : PR_SINK;
        int           pr        = dec.dest_reg != ISA_NO_REG ? alloc_reg()       : PR_SINK;
        unsigned      rob_index = allocate_rob(dec.dest_reg, pr, old_pr, fetched);

        // Rename (XXX backpressure)
        micro_op_t mop = {
            .fetched = fetched,
            .dec     = dec,
            .pr_a    = rat[dec.source_reg_a],
            .pr_b    = rat[dec.source_reg_b],
            .pr_wb   = pr,
            .rob_index = rob_index
        };

#ifdef EARLY_RELEASE
        free_reg(old_pr);
#endif

        if (dec.dest_reg != ISA_NO_REG) {
            //fprintf(stderr, "%5d:rat[%d]: %d -> %d\n", fetched.seqno, dec.dest_reg, rat[dec.dest_reg], mop.pr_wb);
            rat[dec.dest_reg] = mop.pr_wb;
        }

        rob[rob_index].dec = dec;
        rob[rob_index].pr = mop.pr_wb;

        if (dec.class == isa_insn_class_load || dec.class == isa_insn_class_store)
            me_buffer[me_size++] = mop;
        else
            ex_buffer[ex_size++] = mop;
    }
}


// XXX This should be architecture and platform specfic
static bool
is_mmio_space(cpu_state_t *state, uint64_t addr)
{
    return addr < 0x80000000;
}

static void
lsc_exec1(cpu_state_t *state, verbosity_t verbosity, micro_op_t mop)
{
    assert(mop.fetched.addr == mop.dec.insn_addr);

    isa_exception_t exc = { 0 };
    uint64_t op_a  = prf[mop.pr_a];
    uint64_t op_b  = prf[mop.pr_b];
    uint64_t msr_a =
        mop.dec.source_msr_a != ISA_NO_REG
        ? arch->read_msr(state, mop.dec.source_msr_a, &exc)
        : 0;

    uint64_t atomic_load_addr = op_a;
    bool mmio = false;

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

    if (exc.raised)
        goto exception;

    switch (mop.dec.class) {
    case isa_insn_class_load:
        res.load_addr = CANONICALIZE(res.load_addr);
        res.result = arch->load(state, res.load_addr, mop.dec.loadstore_size, &exc);
        res.result = CANONICALIZE(res.result);
        mmio = is_mmio_space(state, res.load_addr);

        if (exc.raised)
            goto exception;

        break;

    case isa_insn_class_store:
        res.store_addr = CANONICALIZE(res.store_addr);
        res.store_value = CANONICALIZE(res.store_value);
	rob[mop.rob_index].store_addr = res.store_addr;
	rob[mop.rob_index].store_data = res.store_value;
	rob[mop.rob_index].store_size = mop.dec.loadstore_size;
	assert(mop.dec.loadstore_size);
	assert(rob[mop.rob_index].store_size);

	int p = mop.rob_index;

        if (verbosity & VERBOSE_DISASS)
            fprintf(stderr,
                    "%08"PRIx64" S%c (%08"PRIx64") = %08"PRIx64"\n",
                    rob[p].fp.addr & 0xFFFFFFFF,
                    letter_size[rob[p].store_size],
                    rob[p].store_addr & 0xFFFFFFFF,
                    rob[p].store_data & 0xFFFFFFFF);

	// XXX Obviously we can't do this here ... unless we can undo it later?
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
        if (res.branch_taken) {
            rob[mop.rob_index].restart = true;
            rob[mop.rob_index].restart_pc = mop.dec.jumpbranch_target;
        }
        break;

    case isa_insn_class_jump: {
            rob[mop.rob_index].restart = true;
            rob[mop.rob_index].restart_pc = mop.dec.jumpbranch_target;
        }
        break;

    case isa_insn_class_compjump: {
            rob[mop.rob_index].restart = true;
            rob[mop.rob_index].restart_pc = res.compjump_target;
        }
        break;
    }

    prf[mop.pr_wb] = res.result;
    pr_ready[mop.pr_wb] = true;

    if (mop.dec.dest_msr != ISA_NO_REG)
        arch->write_msr(state, mop.dec.dest_msr, res.msr_result, &exc);

    // Flush the pipe on system instructions unless is a compjump
    if (mop.dec.system && mop.dec.class != isa_insn_class_compjump) {
        rob[mop.rob_index].restart = true;
        rob[mop.rob_index].restart_pc = mop.dec.insn_addr + 4;
    }

exception:
    rob[mop.rob_index].committed = true;
    rob[mop.rob_index].mmio = mmio;

    if (0 && state->verbosity & VERBOSE_DISASS) {
        char buf[20];
        snprintf(buf, sizeof buf, "[pr%d=pr%d,pr%d]", mop.pr_wb, mop.pr_a, mop.pr_b);
        fprintf(stderr, "%5d  EX %-16s %5d %d ", cycle, buf, mop.fetched.seqno, state->priv);
        isa_disass(arch, mop.dec, (isa_result_t) { .result = prf[mop.pr_wb] });
    }

    if (exc.raised) {
        rob[mop.rob_index].exception = true;

        if (mop.fetched.seqno < exception_seqno) {
            exception_seqno = mop.fetched.seqno;
            exception_info  = exc;
        }
    }
}

static void
lsc_execute(cpu_state_t *state, verbosity_t verbosity)
{
    bool ex_ready[EX_BUFFER_SIZE];
    bool me_ready[ME_BUFFER_SIZE];
    for (int i = 0; i < ex_size; ++i) {
        micro_op_t mop = ex_buffer[i];
        ex_ready[i] = pr_ready[mop.pr_a] & pr_ready[mop.pr_b];
    }

    for (int i = 0; i < me_size; ++i) {
        micro_op_t mop = me_buffer[i];
        me_ready[i] = pr_ready[mop.pr_a] & pr_ready[mop.pr_b];
    }

    /* Schedule and Execute */
    int ex_size_next = 0;
    for (int i = 0; i < ex_size; ++i)
        if (ex_ready[i]) {
            lsc_exec1(state, verbosity, ex_buffer[i]);
            rob[ex_buffer[i].rob_index].fp.execute_ts = cycle;
            rob[ex_buffer[i].rob_index].fp.commit_ts = cycle;
        } else
            ex_buffer[ex_size_next++] = ex_buffer[i];

    ex_size = ex_size_next;

    // ME has to be in order
    int p;
    for (p = 0; p < me_size && me_ready[p]; ++p) {
        lsc_exec1(state, verbosity, me_buffer[p]);
        rob[me_buffer[p].rob_index].fp.execute_ts = cycle;
        rob[me_buffer[p].rob_index].fp.commit_ts = cycle;
    }

    if (p == 0)
        return;

    // Compress out executed instructions

    for (int j = 0; j + p < me_size; ++j)
        me_buffer[j] = me_buffer[j + p];
    me_size -= p;
}

static bool
step_lsc(
    cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    lsc_retire(state, costate, verbosity);
    lsc_execute(state, verbosity);
    lsc_decode_rename(state, verbosity);
    lsc_fetch(state, verbosity); // XXX execute affects pc in the same cycle

    arch->tick(state, 1);

    return false;
}

void
run_lsc(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    elf_info_t info;

    memory_ensure_mapped_range(state->mem,
                               0x80000000, 0x80000000 + 32*1024-1);

    memory_ensure_mapped_range(costate->mem,
                               0x80000000, 0x80000000 + 32*1024-1);

    loadelfs(state->mem, num_images, images, &info);
    loadelfs(costate->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);
    arch->setup(costate, &info, 0);

    uint64_t tohost = 0;
    getelfsym(&info, "tohost", &tohost);

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

    memcpy(art, rat, sizeof art);
    for (cycle = 0;; ++cycle) {
        if (verbosity) {
            //fprintf(stderr, "\nCycle #%d:\n", cycle);
            if (0) show_fb();
            if (0) show_ex();
            if (0) show_rob("");
        }

        if (step_lsc(state, costate, verbosity))
            break;

        if (simple_htif(arch, state, verbosity, tohost))
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
