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
 * - Force interrupts on cosim model
 *
 * Perf:
 *
 * - Predict branches
 * - Misspeculated stores corrupt the memory.  Solutions from simplest to most advanced:
 *   1. Same but, block loads only if there are overlapping stores.
 *   2. Same but, forward completely overlapping stores with known data.
 *   ...
 *   ?. Track unresolved stores
 *   ?. ... and loads (full OOO)
 * - Allow loads to execute in the presence of unretired, but non-overlapping stores
 * - .... Further, allow loads to execute as long as all earlier stores have committed
 *   (and forward as needed)
 * - crack stores into store data and store address
 *
 * - LSC
 *
 * Cleanup:
 *
 * - Don't depend on seqno outside of self-checking and visualization
 * - Review what's tracked in data structures
 * - fp vs fetched
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

#define FETCH_BUFFER_SIZE      4
#define FETCH_WIDTH            4
#define ROB_SIZE               16 // <= 64 for now (using uint64_t as bitmasks)
#define PHYSICAL_REGS          73 // XXX Should exclude the two reserved ones
#define EX_BUFFER_SIZE         ROB_SIZE // XXX same, for now
#define ME_BUFFER_SIZE         28

/*
 * Early release frees the old physical register at allocation time
 * and roll-back then have to undo that (but this is cheap).
 */

static const bool debug_freelist = false;

bool CONFIG_EARLY_RELEASE;
//#define EARLY_RELEASE           1

// Two special physical registers: the constant zero and the sink for
// for r0 destination (it's never read)

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


    uint64_t            store_addr;
    int                 store_data_pr;
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
static unsigned         map[32];
static unsigned         art[32];
static int64_t          prf[PHYSICAL_REGS];
static bool             pr_ready[PHYSICAL_REGS]; // Scoreboard
static bool             pr_ready_next[PHYSICAL_REGS]; // Scoreboard
static unsigned         freelist[PHYSICAL_REGS];
static micro_op_t       ex_buffer[EX_BUFFER_SIZE];
static micro_op_t       me_buffer[ME_BUFFER_SIZE];

/*
 * A CAM-free, constant-time, RAM based scheduler organizes the
 * waitlist (wait set) in three parts based on the number of operands
 * that are still outstanding:
 *
 * - needs0 (AKA, ready, waiting for nothing)
 * - needs1
 * - needs2
 *
 * When a physical register gets defined, all of the depending
 * instructions moves up in the hierarchy: the ones in needs1 become
 * ready, the somes in needs2 moves to needs1.
 *
 * We maintain a map from phys. registers to dependent instructions.
 * As instructions can start in needs2 migrate to needs1, we'd need to
 * update this map both rename time and execute.
 *
 * For now we implement the sets with bitmaps (and thus require
 * EX_BUFFER_SIZE <= 64), but eventually we'll make everything RAM
 * based.
 */
static uint64_t         ex_needs[3];

// depends[pr], the set of instructions in ex_needsX waiting on pr
static uint64_t         depends[PHYSICAL_REGS];

static const arch_t    *arch;
static unsigned         fetch_seqno;
static int              fb_rp = 0, fb_wp = 0, fb_size = 0;
static unsigned         rob_wp = 0, rob_rp = 0;
static unsigned         freelist_wp = 0, freelist_rp = 0;
static int              ex_size;
static int              me_size;

static uint64_t         exception_seqno = ~0ULL;
static isa_exception_t  exception_info;

static int              n_cycles;
static int              n_pending_stores;
static int              n_free_regs;
static int              n_regs_in_flight;

static char             letter_size[256] = {
    [1] = 'B',
    [2] = 'H',
    [4] = 'W',
    [8] = 'D',
};

#define popcount __builtin_popcount

///////////////////////////////////////////////////////

#define EX_INV() ex_check(__FILE__, __LINE__)

static void ex_check(const char *file, unsigned line)
{
    // INV: Disjunctive union of ex_size bits
    if (popcount(ex_needs[0] | ex_needs[1] | ex_needs[2]) != ex_size ||
        (ex_needs[0] & ex_needs[1]) || (ex_needs[1] & ex_needs[2]) || (ex_needs[0] & ex_needs[2])) {

        printf("%s:%d: EX %d %08lx/%08lx/%08lx\n", file, line, ex_size,
               ex_needs[0], ex_needs[1], ex_needs[2]);

        fflush(stdout);
        exit(-1);
    }
}


static bool
is_rob_full(void)
{
    return (rob_wp + 1) % ROB_SIZE == rob_rp;
}

static void
assert_not_in_freelist(int x)
{
    int p = freelist_rp;
    while (p != freelist_wp) {
        if (freelist[p] == x)
            fprintf(stderr, "%05d [FAILURE p%d in already free at pos %d\n",
                    n_cycles, x, p);
        assert(freelist[p] != x);
	if (++p == sizeof freelist / sizeof *freelist)
	    p = 0;
    }
}

#define assert_ne(a, b)                                 \
    ({long long __a = (a), __b = (b);                   \
    if (__a == __b) {                                   \
        fprintf(stderr, "%lld == %lld\n", __a, __b);    \
    assert(a != b); }})

static void
free_reg(unsigned pr)
{
    assert((unsigned) pr < PHYSICAL_REGS);
    if (pr == PR_ZERO || pr == PR_SINK)
	return;

    if (debug_freelist)
    fprintf(stderr, "%05d [free  p%02d; %d/%d FL %d-%d]\n",
            n_cycles, pr, n_free_regs, n_regs_in_flight,
            freelist_rp, freelist_wp);

    assert_not_in_freelist(pr);

    freelist[freelist_wp] = pr;
    if (++freelist_wp == sizeof freelist / sizeof *freelist)
        freelist_wp = 0;
    ++n_free_regs;

    assert(freelist_rp != freelist_wp);
    assert(0 <= n_free_regs && n_free_regs <= PHYSICAL_REGS - 2);
    assert((PHYSICAL_REGS + freelist_wp - freelist_rp) % PHYSICAL_REGS == n_free_regs);
}

static unsigned
alloc_reg(void)
{
    assert(freelist_rp != freelist_wp);

    unsigned pr = freelist[freelist_rp++];
    if (freelist_rp == sizeof freelist / sizeof *freelist)
        freelist_rp = 0;
    pr_ready_next[pr] = false;
    --n_free_regs;

    if (debug_freelist)
    fprintf(stderr, "%05d [alloc p%02d; %d/%d FL %d-%d]\n",
            n_cycles, pr, n_free_regs, n_regs_in_flight,
            freelist_rp, freelist_wp);
    assert(0 <= n_free_regs && n_free_regs <= PHYSICAL_REGS - 2);
    assert((PHYSICAL_REGS + freelist_wp - freelist_rp) % PHYSICAL_REGS == n_free_regs);

    return pr;
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

    if (++rob_wp == ROB_SIZE)
        rob_wp = 0;

    assert(rob_wp != rob_rp);

    return rob_index;
}

static void
visualize_retirement(cpu_state_t *state, rob_entry_t rob)
{
#define WIDTH 32

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
    line[n_cycles         % WIDTH] = 'R';

    printf("%5d ", n_cycles);

#if 0
    printf("%5d ", fp.seqno);
#endif

    printf("%s ", line);

#if 1
    if (pr != PR_SINK)
        printf("r%02d:p%02d>p%02d ", dec.dest_reg, rob.pr_old, pr);
    else
        printf("            ");
#endif
    isa_disass(stdout, arch, dec, (isa_result_t) { .result = prf[pr] });

#if 0
    for (int p = freelist_rp; p != freelist_wp;) {
        printf(" %d", freelist[p]);
        if (++p == sizeof freelist / sizeof *freelist) p = 0;
    }
    printf("\n");
#endif
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
        if (0)
        fprintf(stderr, "%05d [ROB colla, %d rob entries, regs: %d free, %d in-flight; FL %d-%d]\n",
                n_cycles,
                (ROB_SIZE + rob_wp - rob_rp) % ROB_SIZE,
                n_free_regs, n_regs_in_flight,
                freelist_rp, freelist_wp);

        unsigned p = rob_wp;
        if (p-- == 0)
            p = ROB_SIZE - 1;

        assert((unsigned)p < ROB_SIZE);
        if (p == keep_rob_index)
            break;
        rob_wp = p;

        if (rob[p].r == ISA_NO_REG)
            continue;

        if (CONFIG_EARLY_RELEASE) {
            // Undo the free'd register and the allocation
            assert(freelist_wp != freelist_rp);
            if (freelist_wp-- == 0)
                freelist_wp = sizeof freelist / sizeof *freelist - 1;

            assert_not_in_freelist(map[rob[p].r]);

            if (freelist_rp-- == 0)
                freelist_rp = sizeof freelist / sizeof *freelist - 1;

            if (map[rob[p].r] != freelist[freelist_rp])
                fprintf(stderr, "      map[r%d] (p%d) != freelist[%d] (p%d)\n",
                        rob[p].r, map[rob[p].r], freelist_rp, freelist[freelist_rp]);

            assert(map[rob[p].r] == freelist[freelist_rp]);

            --n_regs_in_flight;

            if (debug_freelist)
            fprintf(stderr, "%05d [rollback: unalloc p%02d/unfree p%02d; %2d/%2d; %2d-%2d]\n",
                    n_cycles,
                    freelist[freelist_rp],
                    freelist[freelist_wp],
                    n_free_regs, n_regs_in_flight,
                    freelist_rp, freelist_wp);
            assert(0 <= n_regs_in_flight && n_free_regs - n_regs_in_flight <= PHYSICAL_REGS - 2);
            assert((PHYSICAL_REGS + freelist_wp - freelist_rp) % PHYSICAL_REGS == n_free_regs);
        } else {
            // free_reg(map[rob[p].r]);
            // We _should_ be able to just unallocate it?
            if (freelist_rp-- == 0)
                freelist_rp = sizeof freelist / sizeof *freelist - 1;
            ++n_free_regs;
            assert(map[rob[p].r] == freelist[freelist_rp]);

    if (debug_freelist)
            fprintf(stderr,
                    "%05d [rollback: unalloc p%02d; %2d/%2d; %2d-%2d]\n",
                    n_cycles,
                    freelist[freelist_rp],
                    n_free_regs, n_regs_in_flight,
                    freelist_rp, freelist_wp);
        }

        map[rob[p].r] = rob[p].pr_old;
    } while (rob_rp != rob_wp);

    if (memcmp(map, art, sizeof map) != 0) {
        fprintf(stderr, "Uh oh:\n");
        for (int i = 0; i < 32; ++i)
            if (map[i] != art[i])
                fprintf(stderr, "  map[%d] = %d, but art[%d] = %d\n", i, map[i], i, art[i]);
    }
    assert(memcmp(map, art, sizeof map) == 0);

    exception_seqno = ~0ULL;
    n_pending_stores = 0;
}


static void
flush_and_redirect(cpu_state_t *state, verbosity_t verbosity, int rob_index, unsigned seqno, uint64_t new_pc)
{
    // Flush
    fb_size = 0;
    fb_rp = fb_wp;
    ex_size = 0;
    me_size = 0;
    ex_needs[0] = ex_needs[1] = ex_needs[2] = 0;

    EX_INV();

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
    int n_retired = 0;

    while (rob_rp != rob_wp && rob[rob_rp].committed) {
        rob_entry_t re = rob[rob_rp];

        if (!re.dec.system && arch->get_interrupt_exception(state, &exception_info))
            re.exception = true;

        if (verbosity & VERBOSE_DISASS)
            visualize_retirement(state, re);

        if (re.dec.class == isa_insn_class_store && !re.exception) {

            assert(pr_ready[re.store_data_pr]);
            assert(0 < n_pending_stores);

            isa_exception_t exc = { 0 };
            arch->store(state, re.store_addr, prf[re.store_data_pr],
                        re.dec.loadstore_size, &exc);
            --n_pending_stores;

            if (exc.raised) {
                re.exception = true;
                exception_info = exc;
            }
        }

        if (re.exception) {
            if (state->verbosity & VERBOSE_DISASS)
                fprintf(stderr, "                  EXCEPTION %"PRId64" (%08"PRId64") RAISED\n",
                        exception_info.code, exception_info.info);

            int prev_rob_rp = rob_rp == 0 ? ROB_SIZE - 1 : rob_rp - 1;
            //assert(state->msr[0x300] == costate->msr[0x300]);
            flush_and_redirect(state, verbosity, prev_rob_rp, re.fp.seqno - 1,
                               arch->handle_exception(state, re.dec.insn_addr, exception_info));

            costate->pc = arch->handle_exception(costate, costate->pc, exception_info);
            //assert(state->msr[0x300] == costate->msr[0x300]);
            break;
        }

        if (re.r != ISA_NO_REG) {
            art[re.r] = re.pr;
        }

        if (re.restart) {
            flush_and_redirect(state, verbosity, rob_rp, re.fp.seqno, re.restart_pc);
        }

        ++n_retired;

        /* Co-simulate retired instructions.  A complication is that
         * costate->pc might not be the next instuction retired (if
         * the instruction traps, then the next retired instruction
         * will be the first from the trap handler
         */

        uint64_t copc;
        do copc = costate->pc; while (step_simple(arch, costate, state) == 0);

        bool override = re.mmio;

        if (override && re.r != ISA_NO_REG)
            costate->r[re.r] = prf[re.pr];

        if (re.dec.insn_addr != copc) {
            fprintf(stderr, "COSIM: REF PC %08"PRIx64" != LSC PC %08"PRIx64"\n",
                    copc & 0xFFFFFFFF, re.dec.insn_addr & 0xFFFFFFFF);
            assert(re.dec.insn_addr == copc);
        }

        if (re.r != ISA_NO_REG && prf[re.pr] != costate->r[re.r]) {
            fprintf(stderr, "COSIM: REF RES %08"PRIx64" != LSC RES %08"PRIx64"\n",
                    costate->r[re.r] & 0xFFFFFFFF, prf[re.pr] & 0xFFFFFFFF);
            assert(prf[re.pr] == costate->r[re.r]);
        }

        if (CONFIG_EARLY_RELEASE) {
            if (re.r != ISA_NO_REG) {
                --n_regs_in_flight;
                assert(0 <= n_regs_in_flight && n_free_regs - n_regs_in_flight <= PHYSICAL_REGS - 2);
                assert((PHYSICAL_REGS + freelist_wp - freelist_rp) % PHYSICAL_REGS == n_free_regs);
            }
        } else {
            free_reg(re.pr_old);
        }

        if (++rob_rp == ROB_SIZE)
            rob_rp = 0;
    }

    static int deadcycles = 0;

    if (n_retired == 0) ++deadcycles; else deadcycles = 0;

    if (deadcycles > 5) {
        /* Why aren't we retiring?  Let's look at what's outstanding
         * in the ROB and where the various instructions are at.


         * Problem is that for latency 1 instructions we need to
         * update the scoreboard earlier
         */

        printf("%5d CONCERING LACK OF RETIREMENT\n", n_cycles);

        // FB
        printf("         FB size %d, ", fb_size);
        printf("EX size %d: %08lx/%08lx/%08lx, ", ex_size, ex_needs[0], ex_needs[1], ex_needs[2]);
        printf("ME size %d, ", me_size);
        printf("Regs free %d (- in flight = %d)\n", n_free_regs, n_free_regs - n_regs_in_flight - 1);

        // EX

        for (int i = 0; i < EX_BUFFER_SIZE; ++i)
            if ((ex_needs[0] | ex_needs[1] | ex_needs[2]) & (1l << i)) {
                micro_op_t mop = ex_buffer[i];
                int missing = 2 * ((ex_needs[2] >> i) & 1) + ((ex_needs[1] >> i) & 1);
                printf("         EX[%02d] (missing %d {", i, missing);
                for (int j = 0; j < PHYSICAL_REGS; ++j)
                    if (depends[j] & (1ULL << i))
                        printf(" x%d", j);
                printf("} ");

                isa_disass(stdout, arch, mop.dec, (isa_result_t) { .result = 0 });
            }

        // ROB
        for (int p = rob_rp; p != rob_wp;) {
            rob_entry_t    re = rob[p];
            isa_decoded_t dec = re.dec;

            printf("         ROB[%02d] = %c", p, re.committed ? 'C' : ' ');
            isa_disass(stdout, arch, dec, (isa_result_t) { .result = 0 });

            if (++p == ROB_SIZE)
                p = 0;
        }
    }

    arch->tick(state, n_retired, NULL);
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

        fb[fb_wp] = (fetch_parcel_t){
            .seqno = fetch_seqno++,
            .addr = addr,
            .insn = insn,
            .fetch_ts = n_cycles
        };

        if (++fb_wp == FETCH_BUFFER_SIZE)
            fb_wp = 0;
        fb_size++;
    }

    if (0)
    fprintf(stderr, "%05d [fetched %d, buffer %d]\n", n_cycles, n, fb_size);
}

static void
lsc_decode_rename(cpu_state_t *state, verbosity_t verbosity)
{
    /*
     * Decode and rename
     */
    while (0 < fb_size &&
           ex_size < EX_BUFFER_SIZE &&
           me_size < ME_BUFFER_SIZE &&
           !is_rob_full() &&
           n_regs_in_flight + 1 < n_free_regs) {

        fetch_parcel_t fetched = fb[fb_rp];
        isa_decoded_t dec      = arch->decode(fetched.addr, fetched.insn);

        // Serialize system instruction; block issuing until all
        // previous instructions have executed (really, retired) and then only allow a single one in

        if (dec.system && (ex_size > 0 || me_size > 0))
            break;

        if (++fb_rp == FETCH_BUFFER_SIZE)
            fb_rp = 0;
        fb_size--;

        fetched.decode_ts = n_cycles;

        int           old_pr    = dec.dest_reg != ISA_NO_REG ? map[dec.dest_reg] : PR_SINK;
        int           pr        = dec.dest_reg != ISA_NO_REG ? alloc_reg()       : PR_SINK;
        unsigned      rob_index = allocate_rob(dec.dest_reg, pr, old_pr, fetched);

        // Rename (XXX backpressure)
        micro_op_t mop = {
            .fetched = fetched,
            .dec     = dec,
            .pr_a    = map[dec.source_reg_a],
            .pr_b    = map[dec.source_reg_b],
            .pr_wb   = pr,
            .rob_index = rob_index
        };

        if (CONFIG_EARLY_RELEASE) {
            if (old_pr != PR_SINK && old_pr != PR_ZERO) {
                free_reg(old_pr);
                ++n_regs_in_flight;
            }
        }

        if (dec.dest_reg != ISA_NO_REG) {
            map[dec.dest_reg] = mop.pr_wb;
        }

        rob[rob_index].dec = dec;
        rob[rob_index].pr = mop.pr_wb;

        if (dec.class == isa_insn_class_load || dec.class == isa_insn_class_store)
            me_buffer[me_size++] = mop;
        else {
            EX_INV();
            ex_size++;

            // XXX for now, just organize the ex_buffer by rob id, to
            // avoid having to many indicies to worry about.
            ex_buffer[rob_index] = mop;
            if (!pr_ready[mop.pr_a]) depends[mop.pr_a] |= 1ULL << rob_index;
            if (!pr_ready[mop.pr_b]) depends[mop.pr_b] |= 1ULL << rob_index;

            int dep = !pr_ready[mop.pr_a];
            dep += !pr_ready[mop.pr_b] && mop.pr_b != mop.pr_a;
            ex_needs[dep] |= 1ULL << rob_index;

            EX_INV();
        }

        if (0)
        fprintf(stderr, "%05d [ROB alloc, %d rob entries, regs: %d free, %d in-flight; FL %d-%d]\n",
                n_cycles,
                (ROB_SIZE + rob_wp - rob_rp) % ROB_SIZE,
                n_free_regs, n_regs_in_flight,
                freelist_rp, freelist_wp);
        assert((PHYSICAL_REGS + freelist_wp - freelist_rp) % PHYSICAL_REGS == n_free_regs);
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
        assert(0);
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
    pr_ready_next[mop.pr_wb] = true;

    ex_needs[0] |= ex_needs[1] & depends[mop.pr_wb];
    ex_needs[1] &= ~depends[mop.pr_wb];

    ex_needs[1] |= ex_needs[2] & depends[mop.pr_wb];
    ex_needs[2] &= ~depends[mop.pr_wb];

    EX_INV();

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
    uint64_t ready_to_run = ex_needs[0];

    /* Schedule and Execute */
    for (int i = 0; i < EX_BUFFER_SIZE; ++i)
        if ((1ULL << i) & ready_to_run) {
            ex_needs[0] &= ~(1ULL << i);

            lsc_exec1(state, verbosity, ex_buffer[i]);
            rob[i].fp.execute_ts = n_cycles;
            rob[i].fp.commit_ts = n_cycles;
        }

    // ME has to be in order
    int p;
    for (p = 0; p < me_size; ++p) {
        micro_op_t mop = me_buffer[p];

        if (mop.dec.class == isa_insn_class_store) {
            if (!pr_ready[mop.pr_a])
                break;

            uint64_t op_a = prf[mop.pr_a];
            isa_exception_t exc = { 0 };
            isa_result_t res = arch->insn_exec(mop.dec, op_a, 0, 0, &exc);

            // Don't actually perform the store until retirement
            rob[mop.rob_index].store_addr = CANONICALIZE(res.store_addr);
            rob[mop.rob_index].store_data_pr = mop.pr_b;
            rob[mop.rob_index].fp.execute_ts = n_cycles;
            rob[mop.rob_index].fp.commit_ts = n_cycles;
            rob[mop.rob_index].committed = true;

            if (exc.raised) {
                rob[mop.rob_index].exception = true;

                if (mop.fetched.seqno < exception_seqno) {
                    exception_seqno = mop.fetched.seqno;
                    exception_info  = exc;
                }
            }

            ++n_pending_stores;
            continue;
        }

        if (mop.dec.class == isa_insn_class_load && 0 < n_pending_stores) {
            if (0 & verbosity & VERBOSE_DISASS)
                fprintf(stderr, "%08x L%c blocked by %d pending stores\n",
                       (uint32_t) mop.fetched.addr,
                       letter_size[mop.dec.loadstore_size],
                       n_pending_stores);
            break;
        }

        if (!pr_ready[mop.pr_a] | !pr_ready[mop.pr_b])
            break;

        lsc_exec1(state, verbosity, me_buffer[p]);
        rob[mop.rob_index].fp.execute_ts = n_cycles;
        rob[mop.rob_index].fp.commit_ts = n_cycles;
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
    memcpy(pr_ready, pr_ready_next, sizeof pr_ready);

    lsc_retire(state, costate, verbosity);
    lsc_execute(state, verbosity);
    lsc_decode_rename(state, verbosity);
    lsc_fetch(state, verbosity); // XXX execute affects pc in the same n_cycles

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

    /* pr_ready and reservation station initialization */
    memset(pr_ready_next, 0, sizeof pr_ready_next);
    pr_ready_next[PR_ZERO] = true;

    freelist_wp = freelist_rp = n_free_regs = n_regs_in_flight = 0;

    for (unsigned pr = 1; pr < PR_SINK; ++pr) {
        free_reg(pr);
    }

    map[0] = PR_ZERO;
    for (int i = 1; i < 32; ++i) {
        int pr       = alloc_reg();
        map[i]       = pr;
        prf[pr]      = state->r[i];
        pr_ready_next[pr] = true;
    }

    memcpy(art, map, sizeof art);
    for (n_cycles = 0;; ++n_cycles) {
        if (step_lsc(state, costate, verbosity))
            break;

        if (simple_htif(arch, state, verbosity, tohost))
            break;
    }

    //fprintf(stderr, "IPC = %.2f\n", (double) n_issue / n_cycles);

    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
