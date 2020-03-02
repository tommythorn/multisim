/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2019,2020 Tommy Thorn
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
 *
 * Most significant bit: the purpose of this model is to be a IPC
 * limit study so many things are completely unrealistic.  That's not
 * an issue [yet].
 *
 * The code changes to fast any kind of coherent TO-DO list.
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
#include "riscv.h" // We are increasingly including RISC-V specifics

//////// Configuration and magic numbers

#define FETCH_WIDTH            32
#define FETCH_BUFFER_SIZE      64
#define ROB_SIZE               128
#define PRF_SIZE               (2*ROB_SIZE)

// RAS entries are split between retired free entries, non-retired
// free, and in-RAS entries.  RAS_ENTRIES - RAS_MAX_SIZE will
// constrain speculation.

#define RAS_ENTRIES            32
#define RAS_MAX_SIZE           (RAS_ENTRIES / 2)

//////// Types

typedef struct fetch_parcel_st {
    unsigned            seqno;
    uint64_t            addr;
    uint64_t            addr_next_predicted; // XXX crude and expensive
    uint32_t            insn;

    unsigned            ras_top;
    unsigned            ras_free_rp;
    int                 ras_popped_entry;
    int                 ras_pushed_entry;

    // For visualization
    uint64_t            fetch_ts;
    uint64_t            decode_ts;
    uint64_t            issue_ts;
    uint64_t            execute_ts;
    uint64_t            commit_ts;
//  uint64_t            retire_ts; // This is implicit
} fetch_parcel_t;

// Instructions state in the ROB
typedef enum {
    IS_INVALID,
    IS_FETCHED,
    IS_EXECUTED,
    IS_EXCEPTION,
    // IS_DISPATCHED,
    IS_COMMITTED,
} insn_state_t;

static char insn_state_to_char[] = {
    [IS_INVALID]   = '?',
    [IS_FETCHED]   = ' ',
    [IS_EXECUTED]  = 'E',
    [IS_EXCEPTION] = 'X',
    [IS_COMMITTED] = 'C',
};

typedef struct rob_entry_st {
    insn_state_t        insn_state;
    uint64_t            msr_result;

    int                 prd, prs1, prs2;

    bool                restart;
    uint64_t            restart_pc;

    uint64_t            store_addr;

    bool                mispredicted_br;
    bool                mispredicted_jump;
    bool                mispredicted_compjump;

    // For cosim
    bool                mmio; // force reference model to follow us

    fetch_parcel_t      fp;
    isa_decoded_t       dec;
} rob_entry_t;


//////// State

static const arch_t    *arch;

static fetch_parcel_t   fb[FETCH_BUFFER_SIZE];
static unsigned         fetch_seqno;
static bool             allow_fetch = true;
static int              fb_rp = 0, fb_wp = 0, fb_size = 0;

/*
 * The Return Address Stack tracks calls and returns as seen in the
 * frontend.
 *
 * The problem is that the frontend is often on a speculative path and
 * if it turns out to be the wrong path, then the RAS needs to be
 * restored as we restart on the correct path.  Storing the whole RAS
 * in snapshots is the traditional way, but that's very expensive and
 * thus limits the size of the RAS.
 *
 * We instead use a highly unusual solution that scales much better.
 *
 * RAS entries are always split between a set of free elements from
 * retired pops, a singly linked RAS, and elements in flight in the
 * ROB.
 *
 * If the free set is empty then fetch blocks.
 *
 * Push pulls a fresh free RAS entry from the free sets and links it
 * into the RAS, replacing the top.  If we need to restart, restoring
 * the top pointer will undo the push (and the free sets is restored).
 *
 * Pop unlinks the top element and signals that should this
 * instruction retire, then the unlinked element can be reused.  If we
 * restart the the top element change is undone and the element is of
 * course never reused.
 *
 * Since restarts only adjust the RAS pointer and the free list read
 * pointer, we will return both the RAS and the free set to the
 * original state (Proof pending).
 *
 * Complication: eventually the RAS will consume all free entries and
 * the fetch will deadlock.  To avoid this, we maintain a shadow RAS
 * at retirement and when it reaches a certain size, the oldest
 * element is freed.  For this to work, we need to block the frontend
 * from accessing this element.  One way to do this is to maintain the
 * RAS end-element in at retirement.
 */

static uint64_t         ras[RAS_ENTRIES];
static unsigned         ras_older[RAS_ENTRIES];
static unsigned         ras_top;
static unsigned         ras_end;
static unsigned         ras_size;

static unsigned         ras_free[RAS_ENTRIES];
static unsigned         ras_free_wp, ras_free_rp;

static unsigned         rras[RAS_MAX_SIZE];
static unsigned         rras_size;
static unsigned         rras_sp;

static bool             allocation_stopped = false;
static unsigned         allocation_stopped_from;

static int              map[32]; // [tt1], rename table.  -1 means unmapped
static int64_t          art[32]; // register -> retired value

static rob_entry_t      rob[ROB_SIZE];
static unsigned         rob_wp = 0, rob_rp = 0;

static int64_t          prf[PRF_SIZE];
static bool             pr_ready[PRF_SIZE];
static int              pr_next = 0;

static uint64_t         exception_seqno = ~0ULL;
static isa_exception_t  exception_info;

static int              n_cycles;

static uint64_t         n_cycles_waiting_on_uncommitted_store_addr;
static uint64_t         n_cycles_waiting_on_uncommitted_store_data;
static uint64_t         n_loads_reordred;
static uint64_t         n_missed_store_forwardings;
static uint64_t         n_full_store_forwards;

///////////////////////////////////////////////////////
static bool
is_serializing(isa_decoded_t dec)
{
    if (!dec.system)
        return false;

    insn_t i = { .raw = dec.insn };

    return !(i.r.opcode == SYSTEM && i.r.funct3 == CSRRS && i.i.rs1 == 0);
}

/* Given the renamed pr or the original logical, return value */
static bool
readreg(int pr, int r, uint64_t *value)
{
    if (pr != -1) {
        *value = prf[pr];
        return pr_ready[pr];
    }

    *value = art[r];
    return true;
}

static bool
is_rob_full(void)
{
    return (rob_wp + 1) % ROB_SIZE == rob_rp;
}


static void
dump_microarch_state(void)
{
    // FB
    printf("         FB size %d\n", fb_size);
    printf("         ROB:\n");

    // ROB
    for (int p = rob_rp; p != rob_wp;) {
        rob_entry_t    re = rob[p];
        isa_decoded_t dec = re.dec;

        printf("         ROB[%02d] = %c", p, insn_state_to_char[re.insn_state]);
        isa_disass(stdout, arch, dec, (isa_result_t) { .result = 0 });

        if (++p == ROB_SIZE)
            p = 0;
    }
}

static void
visualize_retirement(cpu_state_t *state, unsigned rob_index, rob_entry_t re)
{
#define WIDTH 32

    char line[WIDTH+1];
    fetch_parcel_t fp = re.fp;
    isa_decoded_t dec = re.dec;

    static uint64_t last_cycles = 0;
    static uint64_t last_instret = 0;

    if (last_cycles + 100 < n_cycles) {
        printf("IPC %4.2f ", ((double)fp.seqno - last_instret) / (n_cycles - last_cycles));
        last_cycles = n_cycles;
        last_instret = fp.seqno;

        printf("LSU stats: STA stall %ld/STD stall %ld/Reordered %ld/full bypass %ld/bypass missed %ld\n",
               n_cycles_waiting_on_uncommitted_store_addr,
               n_cycles_waiting_on_uncommitted_store_data,
               n_loads_reordred,
               n_full_store_forwards,
               n_missed_store_forwardings);

        n_cycles_waiting_on_uncommitted_store_addr = 0;
        n_cycles_waiting_on_uncommitted_store_data = 0;
        n_loads_reordred                           = 0;
        n_missed_store_forwardings                 = 0;
        n_full_store_forwards                      = 0;
    }

    memset(line, '.', WIDTH);
    line[WIDTH] = '\0';

    line[fp.fetch_ts   % WIDTH] = 'F';
    line[fp.decode_ts  % WIDTH] = 'D';
  //line[fp.issue_ts   % WIDTH] = 'I';
    line[fp.execute_ts % WIDTH] = 'E';
    line[fp.commit_ts  % WIDTH] = 'C';
    line[n_cycles      % WIDTH] = 'R';

    printf("%5d ", fp.seqno);
    printf("%s ",  line);

    uint64_t store_data;
    bool ready = readreg(re.prs2, re.dec.source_reg_b, &store_data);
    assert(ready);

    isa_disass(stdout, arch, dec,
               (isa_result_t)
               { .result     = prf[re.prd],
                 .msr_result = re.msr_result,
                 .load_addr  = re.store_addr,
                 .store_value= store_data,
                 .store_addr = re.store_addr, });

    if (re.restart)
        putc('\n', stdout);
}

static bool debug_ras = true;

static void
ras_invariants(const char *headline)
{
    assert(ras_top < RAS_ENTRIES);
    assert(ras_end < RAS_ENTRIES);
    assert(ras_size < RAS_ENTRIES);
    assert(ras_free_wp < RAS_ENTRIES);
    assert(ras_free_rp < RAS_ENTRIES);

    if (debug_ras) printf("%-7s %5d => RAS ", headline, fetch_seqno);

    for (unsigned x = ras_top; x != ras_end; x = ras_older[x]) {
        if (debug_ras) printf(" %d", x);
    }

    if (debug_ras) printf("\n                 Free");

    for (unsigned i = ras_free_rp; i != ras_free_wp; i = (i + 1) % RAS_ENTRIES) {
        if (debug_ras) printf(" %d", ras_free[i]);
    }

    if (debug_ras) printf("\n");
}

static uint64_t
pop_ras(unsigned seqno, int *popped_entry)
{
    if (ras_top == ras_end) {
         /* RAS empty, can't predict (we have no prediction) */
        if (debug_ras) printf("POP  %5d ON EMPTY RAS\n", seqno);
        *popped_entry = -1;
        return 0;
    } else {

        uint64_t return_pc = ras[ras_top];

        *popped_entry = ras_top;
        ras_top = ras_older[ras_top];
        ras_invariants("POP");

        return return_pc;
    }
}

static void
push_ras(unsigned seqno, uint64_t pc, int *pushed_entry)
{
    unsigned next;

    assert(ras_free_wp != ras_free_rp);

    /* We take an element from the old end of the free queue */
    next = ras_free[ras_free_rp];
    ras_free_rp = (ras_free_rp + 1) % RAS_ENTRIES;

    ras_older[next] = ras_top;
    ras_top = next;
    ras[ras_top] = pc;

    ras_invariants("PUSH");

    *pushed_entry = ras_top;
}


static void
init_ras(void)
{
    ras_top = 0;
    ras_end = 0;
    ras_size = 0;
    ras_free_rp = ras_free_wp = 0;
    memset(ras_older,   -1, sizeof ras_older);
    for (unsigned i = 1; i < RAS_ENTRIES; ++i) {
        ras_free[ras_free_wp++] = i;
    }

    rras_sp = -1;
    rras_size = 0;

    ras_invariants("INIT");
}

static void
restart(cpu_state_t *state, unsigned seqno, uint64_t new_pc,
        unsigned new_ras_top, unsigned new_ras_free_rp)
{
    /* Oldest restart takes precedence */
    if (!allocation_stopped || seqno < allocation_stopped_from) {
        allocation_stopped = true;
        allocation_stopped_from = seqno;

        // Flush FB
        fb_size = 0;
        fb_rp = fb_wp;

        state->pc = new_pc;
        fetch_seqno = seqno + 1;
        allow_fetch = true;

        ras_top     = new_ras_top;
        ras_free_rp = new_ras_free_rp;

        ras_invariants("RESTART");
    }
}

static void
resume(void)
{
    // No speculative regs => clear MAP
    for (int r = 0; r < 32; ++r)
        map[r] = -1;

    // Empty ROB
    rob_wp = rob_rp;

    // Clear pending interrupt info
    exception_seqno = ~0ULL;

    allocation_stopped = false;
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
ooo_retire(cpu_state_t *state, cpu_state_t *costate, verbosity_t verbosity)
{
    int n_retired = 0;

    while (rob_rp != rob_wp && (rob[rob_rp].insn_state == IS_COMMITTED ||
                                rob[rob_rp].insn_state == IS_EXCEPTION)) {

        rob_entry_t re = rob[rob_rp];

        // We can't interrupt a system instruction as it may already
        // have modified the state and cannot be restarted.
        if (!is_serializing(re.dec) &&
            arch->get_interrupt_exception(state, &exception_info))
            goto exception;

        // Can't retire a committed store until the data is ready
        if (re.dec.class == isa_insn_class_store &&
            re.prs2 != -1 && !pr_ready[re.prs2])
            break;

        if (verbosity & VERBOSE_DISASS)
            visualize_retirement(state, rob_rp, re);

        if (re.insn_state != IS_EXCEPTION && re.dec.class == isa_insn_class_store) {
            isa_exception_t exc = { 0 };
            uint64_t store_data;
            bool ready = readreg(re.prs2, re.dec.source_reg_b, &store_data);
            assert(ready);
            arch->store(state, re.store_addr, store_data, re.dec.loadstore_size, &exc);

            if (exc.raised) {
                re.insn_state = IS_EXCEPTION;
                exception_info = exc;
            }
        }

        if (re.insn_state == IS_EXCEPTION) {
        exception:
            if (state->verbosity & VERBOSE_DISASS)
                fprintf(stdout, "                  %s %"PRId64" (%08"PRId64") RAISED\n",
                        exception_info.code &  (1 << 31) ? "INTERRUPT" : "EXCEPTION",
                        exception_info.code & ~(1 << 31), exception_info.info);

            // XXX What should happen if a pop or push caused an exception?

            restart(state, re.fp.seqno - 1,
                    arch->handle_exception(state, re.dec.insn_addr, exception_info),
                    re.fp.ras_top, re.fp.ras_free_rp);
            resume();

            costate->pc = arch->handle_exception(costate, costate->pc, exception_info);
            break;
        }

        // Return popped RAS entries to be reused (XXX keep RAS below a certain size)
        if (re.fp.ras_popped_entry != -1) {
            assert(rras_size > 0);
            assert(re.fp.ras_popped_entry == rras[rras_sp]);

            if (debug_ras) printf("FREEING %d\n", rras[rras_sp]);
            ras_free[ras_free_wp] = rras[rras_sp];
            ras_free_wp   = (ras_free_wp + 1) % RAS_ENTRIES;
            rras_sp       = (rras_sp - 1)     % RAS_MAX_SIZE;
            rras_size    -= 1;
        }

        if (re.fp.ras_pushed_entry != -1) {
            rras_sp       = (rras_sp + 1)     % RAS_MAX_SIZE;

            if (rras_size >= RAS_MAX_SIZE) {
                if (debug_ras) printf("OVERFLOW FREEING %d\n", rras[rras_sp]);
                ras_free[ras_free_wp] = rras[rras_sp];
                ras_free_wp   = (ras_free_wp + 1) % RAS_ENTRIES;
                rras_size    -= 1;
                ras_end       = rras[rras_sp];
            }

            rras[rras_sp] = re.fp.ras_pushed_entry;
            rras_size    += 1;
        }

        // Move the pf to the ART
        if (re.dec.dest_reg != ISA_NO_REG) {
            assert(pr_ready[re.prd] && re.prd != -1);
            art[re.dec.dest_reg] = prf[re.prd];
            if (map[re.dec.dest_reg] == re.prd)
                map[re.dec.dest_reg] = -1;
        }

        if (++rob_rp == ROB_SIZE)
            rob_rp = 0;

        if (re.restart)
            resume();

        ++n_retired;

        if (verbosity & VERBOSE_NO_COSIM)
            continue;

        /* Co-simulate retired instructions.  A complication is that
         * costate->pc might not be the next instuction retired (if
         * the instruction traps, then the next retired instruction
         * will be the first from the trap handler
         */

        uint64_t copc;
        do copc = costate->pc; while (step_simple(arch, costate, state) == 0);

        bool override = re.mmio ||
            re.dec.source_msr_a == CSR_MCYCLE ||
            re.dec.source_msr_a == CSR_MINSTRET;

        if (override && re.dec.dest_reg != ISA_NO_REG)
            costate->r[re.dec.dest_reg] = prf[re.prd];

        if (re.dec.insn_addr != copc) {
            fprintf(stderr, "COSIM: REF PC %08"PRIx64" != DUT PC %08"PRIx64"\n",
                    copc & 0xFFFFFFFF, re.dec.insn_addr & 0xFFFFFFFF);
            fflush(stdout);
            assert(re.dec.insn_addr == copc);

        }

        if (re.dec.dest_reg != ISA_NO_REG) {
            if (prf[re.prd] != costate->r[re.dec.dest_reg]) {
                fprintf(stderr, "COSIM: REF RES %08"PRIx64" != DUT RES %08"PRIx64"\n",
                        costate->r[re.dec.dest_reg] & 0xFFFFFFFF,
                        prf[re.prd]);
                fflush(stdout);
                assert(prf[re.prd] == costate->r[re.dec.dest_reg]);
            }
        }

        costate->msr[CSR_MCYCLE] = state->msr[CSR_MCYCLE];
        if (memcmp(state->msr, costate->msr, sizeof state->msr)) {
            for (unsigned csr = 0; csr < 0x1000; ++csr)
                if (costate->msr[csr] != state->msr[csr])
                    fprintf(stderr, "COSIM: CSR[0x%03x]: %08lx != %08lx\n",
                            csr, costate->msr[csr], state->msr[csr]);
            fflush(stdout);
            assert(0);
        }
    }

    static int deadcycles = 0;

    if (n_retired == 0) ++deadcycles; else deadcycles = 0;

    if (deadcycles > 10) {
        /* Why aren't we retiring?  Let's look at what's outstanding
         * in the ROB and where the various instructions are at.


         * Problem is that for latency 1 instructions we need to
         * update the scoreboard earlier
         */

        printf("%5d CONCERING LACK OF RETIREMENT\n", n_cycles);
        dump_microarch_state();
    }

    arch->tick(state, n_retired, NULL);
}

static void
ooo_fetch(cpu_state_t *state, verbosity_t verbosity)
{
    isa_exception_t exc = { 0 };
    int n = 0;

    if (!allow_fetch)
        return;

    /*
     * Fetch (branch prediction would happen here, eventually)
     */
    while (fb_size < FETCH_BUFFER_SIZE && n++ < FETCH_WIDTH) {

        if (ras_free_rp == ras_free_wp) {
            if (debug_ras) printf("Block fetch as we have no free RAS entries\n");
            return;
        }

        uint64_t addr = state->pc;
        uint32_t insn = (uint32_t)arch->load(state, addr, 0 /* = ifetch */, &exc);

        if (exc.raised) {
            // XXX technically should tag this as an illegal address
            // but we'll make do with an illegal instruction for now
            insn = 0;
        }

        /* XXX Cheating a bit here until I can get a real predictor */
        uint64_t pc_next = addr + 4;
        isa_decoded_t dec = arch->decode(addr, insn);
        int ras_popped_entry = -1;
        int ras_pushed_entry = -1;

        switch (dec.class) {
        case isa_insn_class_branch:
            if (dec.jumpbranch_target < addr)
                pc_next = dec.jumpbranch_target;
            break;

        case isa_insn_class_jump:
            if (dec.dest_reg == 1 || dec.dest_reg == 5) {
                push_ras(fetch_seqno, pc_next, &ras_pushed_entry);
            }
            pc_next = dec.jumpbranch_target;
            break;

        case isa_insn_class_compjump: {
            // Table 2.1
            bool rd_link = dec.dest_reg == 1 || dec.dest_reg == 5;
            bool rs1_link = dec.source_reg_a == 1 || dec.source_reg_a == 5;
            if (!rd_link && !rs1_link)
                break;

            if (!rd_link && rs1_link) {
                pc_next = pop_ras(fetch_seqno, &ras_popped_entry);
            } else if (rd_link && !rs1_link) {
                push_ras(fetch_seqno, pc_next, &ras_pushed_entry);
            } else if (dec.dest_reg != dec.source_reg_a) {
                uint64_t save = pop_ras(fetch_seqno, &ras_popped_entry);
                push_ras(fetch_seqno, pc_next, &ras_pushed_entry);
                pc_next = save;
            } else {
                push_ras(fetch_seqno, pc_next, &ras_pushed_entry);
            }
        }

        default:;
        }

        state->pc = pc_next;

        fb[fb_wp] = (fetch_parcel_t) {
            .seqno = fetch_seqno++,
            .addr = addr,
            .addr_next_predicted = state->pc,
            .insn = insn,
            .fetch_ts = n_cycles,
            .ras_top = ras_top,
            .ras_free_rp = ras_free_rp,
            .ras_popped_entry = ras_popped_entry,
            .ras_pushed_entry = ras_pushed_entry,
        };

        if (++fb_wp == FETCH_BUFFER_SIZE)
            fb_wp = 0;
        fb_size++;

        if (is_serializing(dec)) {
            allow_fetch = false;
            break;
        }
    }
}

static void
ooo_decode_rename(cpu_state_t *state, verbosity_t verbosity)
{
    /*
     * Decode and rename
     */

    if (allocation_stopped)
        return;

    while (0 < fb_size && !is_rob_full()) {

        fetch_parcel_t fetched = fb[fb_rp];
        isa_decoded_t dec      = arch->decode(fetched.addr, fetched.insn);

        // Serialize system instruction; block issuing until all
        // previous instructions have retired and then only allow a
        // single one in

        if (is_serializing(dec) && rob_rp != rob_wp)
            break;

        if (++fb_rp == FETCH_BUFFER_SIZE)
            fb_rp = 0;
        fb_size--;

        fetched.decode_ts = n_cycles;

        rob[rob_wp] = (rob_entry_t) {
            .insn_state = IS_FETCHED,
            .fp = fetched,
            .dec = dec,
            .prd = -1,
            .prs1 = (dec.source_reg_a == ISA_NO_REG
                     ? -1 : map[dec.source_reg_a]),
            .prs2 = (dec.source_reg_b == ISA_NO_REG
                     ? -1 : map[dec.source_reg_b]),
        };

        if (dec.dest_reg != ISA_NO_REG) {
            rob[rob_wp].prd = map[dec.dest_reg] = pr_next;
            pr_ready[pr_next] = false;
            pr_next = (pr_next + 1) % PRF_SIZE;
        }

        if (++rob_wp == ROB_SIZE)
            rob_wp = 0;

        assert(rob_wp != rob_rp);
    }
}


// XXX This should be architecture and platform specfic
static bool
is_mmio_space(cpu_state_t *state, uint64_t addr)
{
    return addr < 0x80000000;
}

// Checking for overlap (XXX there's a trick I need to dig up again)
static bool
does_overlap(uint64_t a, unsigned a_size, uint64_t b, unsigned b_size)
{
    return
        a <= b && b < a + a_size ||
        b <= a && a < b + b_size;
}

/*
 * Loads are the most interesting instruction in an OoO superscalar machine.
 *
 * We need to check all potentially overlapping older stores in the
 * ROB. If we can tell none can overlap, the load can proceed.  If all
 * overlapping stores have committed, then we can calculate the part
 * that needs to be forwarded and combined load data (if necessary).
 * Otherwise, we must wait.
 */
static bool
ooo_exec_load(cpu_state_t *state, verbosity_t verbosity, unsigned load_rob_index,
              uint64_t load_addr, int load_type, isa_exception_t *exc,
              uint64_t *res)
{
    unsigned reordered_load = 0;

    // XXX I'm only doing to dealed with aligned data; it's
    // interesting enough already
    unsigned size = abs(load_type); // XXX this is bad

    //assert(size == 1 || size == 2 || size == 4 || size == 8);
    //assert((load_addr & (size - 1)) == 0);
    // Forgot this bad speculation runs bad code

    if (!(size == 1 || size == 2 || size == 4 || size == 8)) {
        exc->raised = true;
        exc->code = EXCP_INSN_ILLEGAL;
        exc->info = 0;
        return true;
    }

    if ((load_addr & (size - 1)) != 0) {
        exc->raised = true;
        exc->code = EXCP_LOAD_MISALIGN;
        exc->info = 0;
        return true;
    }

    // XXX Assume 32-bit for now
    //uint64_t addr_word = load_addr & -4ll;
    //uint64_t data_mask = (1ll << (size * 8)) - 1;
    //data_mask <<= 8 * (load_addr & 3);

    unsigned p = load_rob_index;
    while (p != rob_rp) {
        if (p == 0)
            p = ROB_SIZE - 1;
        else
            --p;

        // We _only_ care about store(-like) instructions
        if (rob[p].dec.class != isa_insn_class_store)
            continue;

        // Uncommitted stores don't have a store address
        if (rob[p].insn_state != IS_COMMITTED) {
            // Without speculation, we can't know if this might overlap
            ++/*rob[p].stat.*/n_cycles_waiting_on_uncommitted_store_addr;
            return false;
        }

        // Unrelated stores can be skipped - this effectively reorders the load
        if (!does_overlap(load_addr, size,
                          rob[p].store_addr,
                          rob[p].dec.loadstore_size)) {
            // Puh, dodged this one
            reordered_load = true;
            continue;
        }

        // Uncommitted overlapping store data means we stop
        uint64_t store_data;
        if (!readreg(rob[p].prs2, rob[p].dec.source_reg_b, &store_data)) {
            ++/*rob[p].stat.*/n_cycles_waiting_on_uncommitted_store_data;
            return false;
        }

        // We have an overlapping store with data.  If it fully
        // overlaps, we take it and return.  XXX Only deal with same
        // sized data
        if (rob[p].dec.loadstore_size == load_type) {
            ++n_full_store_forwards;
            if (load_type < 0)
                // Sign-extended
                *res = (int64_t)store_data << (64-8*size) >> (64-8*size);
            else
                *res = (uint64_t)store_data << (64-8*size) >> (64-8*size);
            return true;
        }

        // XXX This is where we start collecting bits of the data to
        // forward.  I'm not going to do this for now, instead just
        // bail.

        ++n_missed_store_forwardings;

        return false; // Sorry, another time
    }

    n_loads_reordred += reordered_load;

    *res = arch->load(state, load_addr, load_type, exc);

    return true;
}

static bool pr_ready_next[PRF_SIZE];

static bool
ooo_exec1(cpu_state_t *state, verbosity_t verbosity, unsigned p,
          uint64_t op_a, uint64_t op_b)
{
    rob_entry_t re = rob[p];
    isa_decoded_t dec = re.dec;
    isa_exception_t exc = { 0 };
    uint64_t msr_a =
        dec.source_msr_a != ISA_NO_REG
        ? arch->read_msr(state, dec.source_msr_a, &exc)
        : 0;

    uint64_t atomic_load_addr = op_a;
    bool mmio = false;

    if (exc.raised)
        goto exception;

    if (dec.class == isa_insn_class_atomic) {
        exc.raised = true;
        exc.code = EXCP_INSN_ILLEGAL;
        goto exception;
        // XXX extend this to handle AMOs
        // op_a = arch->load(state, atomic_load_addr, dec.loadstore_size, &exc);
    }

    if (exc.raised)
        goto exception;

    isa_result_t res;

    if (!dec.system)
        res = arch->insn_exec(dec, op_a, op_b, msr_a, &exc);
    else {
        res = arch->insn_exec_system(state, dec, op_a, op_b, msr_a, &exc);
        if (!exc.raised && dec.dest_msr != ISA_NO_REG)
            // This works because this is the only active instruction in the pipeline now
            arch->write_msr(state, re.dec.dest_msr, res.msr_result, &exc);
    }

    res.result = CANONICALIZE(res.result);

    if (exc.raised)
        goto exception;

    uint64_t pc_next = dec.insn_addr + 4;

    switch (dec.class) {
    case isa_insn_class_load:
        rob[p].store_addr = res.load_addr = CANONICALIZE(res.load_addr);
        mmio = is_mmio_space(state, res.load_addr);

        if (!ooo_exec_load(state, verbosity, p, res.load_addr, dec.loadstore_size, &exc,
                           &res.result))
            return false;

        res.result = CANONICALIZE(res.result);

        if (exc.raised)
            goto exception;

        break;

    case isa_insn_class_store:
        // XXX could check the address for exceptions
        rob[p].store_addr = CANONICALIZE(res.store_addr);
        break;

    case isa_insn_class_atomic:
        // XXX ??
        res.load_addr = CANONICALIZE(res.load_addr);
        arch->store(state, atomic_load_addr, res.result, dec.loadstore_size, &exc);

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
            pc_next = dec.jumpbranch_target;

        if (pc_next != re.fp.addr_next_predicted) {
            rob[p].restart = true;
            rob[p].restart_pc = pc_next;
            rob[p].mispredicted_br = true;
        }
        break;

    case isa_insn_class_jump:
        pc_next = dec.jumpbranch_target;

        if (pc_next != re.fp.addr_next_predicted) {
            rob[p].restart = true;
            rob[p].restart_pc = pc_next;
            rob[p].mispredicted_jump = true;
        }
        break;

    case isa_insn_class_compjump:
        pc_next = res.compjump_target;

        if (pc_next != re.fp.addr_next_predicted) {
            rob[p].restart = true;
            rob[p].restart_pc = pc_next;
            rob[p].mispredicted_compjump = true;
        }
        break;
    }

    if (re.dec.dest_reg != ISA_NO_REG) {
        prf[re.prd] = res.result;
        pr_ready_next[re.prd] = true;
    }
    rob[p].msr_result = res.msr_result;

    // Flush the pipe on system instructions unless is a compjump
    if (!rob[p].restart && is_serializing(dec)) {
        pc_next = dec.insn_addr + 4;
        rob[p].restart = true;
        rob[p].restart_pc = pc_next;
    }

    if (rob[p].restart)
        restart(state, re.fp.seqno, pc_next,
                re.fp.ras_top, re.fp.ras_free_rp);

exception:
    rob[p].insn_state = IS_EXECUTED;
    rob[p].mmio = mmio;
    rob[p].fp.execute_ts = n_cycles;

    if (exc.raised) {
        rob[p].insn_state = IS_EXCEPTION;

        if (rob[p].fp.seqno < exception_seqno) {
            exception_seqno = rob[p].fp.seqno;
            exception_info  = exc;
        }
    }

    return true;
}

static void
ooo_execute(cpu_state_t *state, verbosity_t verbosity)
{
    memcpy(pr_ready_next, pr_ready, sizeof pr_ready_next);

    // Commit previously executed insns XXX redundant now
    for (unsigned p = rob_rp; p != rob_wp; p = p == ROB_SIZE - 1 ? 0 : p + 1)
        if (rob[p].insn_state == IS_EXECUTED) {
            rob[p].insn_state = IS_COMMITTED;
            rob[p].fp.commit_ts = n_cycles;
        }

    for (unsigned p = rob_rp; p != rob_wp; p = p == ROB_SIZE - 1 ? 0 : p + 1) {
        rob_entry_t re = rob[p];

        if (re.insn_state != IS_FETCHED)
            continue;

        uint64_t val_a, val_b = 0;

        if (!readreg(re.prs1, re.dec.source_reg_a, &val_a))
            continue;

        // Stores are special and execute the address calculation even
        // if the data isn't ready.  Loads have additional dependency
        // on stores.

        if (re.dec.class != isa_insn_class_store &&
            !readreg(re.prs2, re.dec.source_reg_b, &val_b))
            continue;

        if (!ooo_exec1(state, verbosity, p, val_a, val_b))
            // Loads may not be able to execute yet
            continue;
    }

    memcpy(pr_ready, pr_ready_next, sizeof pr_ready);
}

static bool
step_ooo(
    cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    ooo_retire(state, costate, verbosity);
    ooo_execute(state, verbosity);
    ooo_decode_rename(state, verbosity);
    ooo_fetch(state, verbosity); // XXX execute affects pc in the same n_cycles

    return false;
}

void
run_ooo(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    elf_info_t info;
    int M = 256;

    init_ras();

    memory_ensure_mapped_range(state->mem,
                               0x80000000, 0x80000000 + M*1024-1);

    memory_ensure_mapped_range(costate->mem,
                               0x80000000, 0x80000000 + M*1024-1);

    loadelfs(state->mem, num_images, images, &info);
    loadelfs(costate->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);
    arch->setup(costate, &info, 0);

    uint64_t tohost = 0;
    getelfsym(&info, "tohost", &tohost);

    uint64_t fromhost = 0;
    getelfsym(&info, "fromhost", &fromhost);

    for (int r = 0; r < 32; ++r)
        map[r] = -1;

    for (n_cycles = 0;; ++n_cycles) {
        if (step_ooo(state, costate, verbosity))
            break;

        if (tohost && fromhost && simple_htif(arch, state, verbosity, tohost, fromhost))
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
