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
 *
 * This branch goes in a different direction: what is the simplest
 * possible we can do?  Try to minimize the state; all instruction are
 * referred to by their sequence number.  A register is either retired
 * and lives in the architecture file (ARF), otherwise it lives in the
 * ROB (conceptually).
 * Scheduling work straight out of the ROB.
 *
 * This eliminates freelist, simplifies rollback, make scheduling much
 * simpler (for now).
 *
 * Once this model works, we can look at ways to improve it.
 *
 *  rob[rp] ... rob[wp-1] is the outstading non-retired instructions
 *  we retire from the rp and insert new ones at the wp

TODO:
- Rethink how interrupts are handled; maybe inserting them at decode?
  but how to get a non-speculative path?

Once this can run everything (in some order)

- Add some branch prediction.  Seems not that hard to add a RAS as
well.  CLEAR

- Accellerate the register lookup; a rename table is obvious but then
  we need to consider how to restore it upon rollback.

  1. The immediate options: the simple Chronis idea which is just
     stops fetch and waits for retirement to catch up.  MOSTLY CLEAR

  2. Checkpoints a la CPR; UNCLEAR

- Two dimensional ROB a la mc88120; UNCLEAR
- CPR; UNCLEAR

- Finally, piece de resistance: optimizing trace cache of packets.
  RESEARCH



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
#include "riscv.h" // Sigh, we need this for illegal and misaligned exceptions

//////// Configuration and magic numbers

#define FETCH_BUFFER_SIZE      4
#define FETCH_WIDTH            4
#define ROB_SIZE               16 // <= 64 for now (using uint64_t as bitmasks)

// Two special physical registers: the constant zero and the sink for
// for r0 destination (it's never read)

#define PR_ZERO                 0

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

// Instructions state in the ROB
typedef enum {
    IS_INVALID,
    // IS_SPECULATIVE,
    IS_FETCHED,
    // IS_READY,
    IS_EXECUTED,
    IS_EXCEPTION,
    // IS_DISPATCHED,
    IS_COMMITTED,
    // IS_RETIRED,
} insn_state_t;

static char insn_state_to_char[] = {
    [IS_INVALID]   = '?',
    [IS_FETCHED]   = ' ',
    [IS_EXECUTED]  = 'E',
    [IS_EXCEPTION]  = 'X',
    [IS_COMMITTED] = 'C',
};

typedef struct rob_entry_st {
    insn_state_t        insn_state;
    uint64_t            result; // ~ prf
    uint64_t            msr_result;

    bool                restart;
    uint64_t            restart_pc;


    uint64_t            store_addr;

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

static const arch_t    *arch;

static fetch_parcel_t   fb[FETCH_BUFFER_SIZE];
static unsigned         fetch_seqno;
static int              fb_rp = 0, fb_wp = 0, fb_size = 0;

static int64_t          art[32]; // register -> retired value

static rob_entry_t      rob[ROB_SIZE];
static unsigned         rob_wp = 0, rob_rp = 0;

static uint64_t         exception_seqno = ~0ULL;
static isa_exception_t  exception_info;

static int              n_cycles;

static uint64_t         n_cycles_waiting_on_uncommitted_store_addr;
static uint64_t         n_cycles_waiting_on_uncommitted_store_data;
static uint64_t         n_loads_reordred;
static uint64_t         n_missed_store_forwardings;


///////////////////////////////////////////////////////

static bool
is_rob_full(void)
{
    return (rob_wp + 1) % ROB_SIZE == rob_rp;
}


static void dump_microarch_state(void)
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

        printf("LSU stats: STA stall %ld/STD stall %ld/Reordered %ld/bypass missed %ld\n",
               n_cycles_waiting_on_uncommitted_store_addr,
               n_cycles_waiting_on_uncommitted_store_data,
               n_loads_reordred,
               n_missed_store_forwardings);

        n_cycles_waiting_on_uncommitted_store_addr = 0;
        n_cycles_waiting_on_uncommitted_store_data = 0;
        n_loads_reordred = 0;
        n_missed_store_forwardings = 0;
    }

    memset(line, '.', WIDTH);
    line[WIDTH] = '\0';

    line[fp.fetch_ts   % WIDTH] = 'F';
    line[fp.decode_ts  % WIDTH] = 'D';
  //line[fp.issue_ts   % WIDTH] = 'I';
    line[fp.execute_ts % WIDTH] = 'E';
    line[fp.commit_ts  % WIDTH] = 'C';
    line[n_cycles      % WIDTH] = 'R';

//  printf("%3d ", n_cycles);
    printf("%3d ", fp.seqno);
//  printf("%2d ", rob_index);
    printf("%s ",  line);

    isa_disass(stdout, arch, dec,
               (isa_result_t)
               { .result     = re.result,
                 .msr_result = re.msr_result,
                 .load_addr  = re.store_addr,
                 .store_value= re.result,
                 .store_addr = re.store_addr, });
}

static void
rollback_rob(int keep_rob_index, verbosity_t verbosity)
{
    do {
        unsigned p = rob_wp;
        if (p-- == 0) {
            p = ROB_SIZE - 1;
        }

        assert((unsigned)p < ROB_SIZE);
        if (p == keep_rob_index)
            break;
        rob_wp = p;
    } while (rob_rp != rob_wp);

    assert(rob_wp == (keep_rob_index == ROB_SIZE - 1 ? 0 : keep_rob_index + 1));

    exception_seqno = ~0ULL;
}

static void
flush_and_redirect(cpu_state_t *state, verbosity_t verbosity, int rob_index,
                   unsigned seqno, uint64_t new_pc)
{
    // Flush
    fb_size = 0;
    fb_rp = fb_wp;

    rollback_rob(rob_index, verbosity);

    state->pc = new_pc;
    fetch_seqno = seqno + 1;
    // mispredicted = true;
}


/* All register values can be found by just scanning the ROB backward
 * from the refering instruction's rob_index */
static bool
get_reg(unsigned rob_index, int r, uint64_t *value)
{
    if (r == 0) {
        *value = 0;
        return true;
    }

    unsigned p = rob_index;
    while (p != rob_rp) {
        if (p == 0)
            p = ROB_SIZE - 1;
        else
            --p;

        if (rob[p].dec.dest_reg == r) {
            *value = rob[p].result;
            return rob[p].insn_state == IS_COMMITTED;
        }
    }

    *value = art[r];
    return true;
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
        if (!re.dec.system &&
            arch->get_interrupt_exception(state, &exception_info))
            goto exception;

        if (re.dec.class == isa_insn_class_store) {
            if (!get_reg(rob_rp, re.dec.source_reg_b, &re.result))
                break;
        }

        if (verbosity & VERBOSE_DISASS)
            visualize_retirement(state, rob_rp, re);

        if (re.insn_state == IS_EXCEPTION)
            goto exception;

        if (re.dec.class == isa_insn_class_store) {
            isa_exception_t exc = { 0 };
            arch->store(state, re.store_addr, re.result, re.dec.loadstore_size, &exc);

            if (exc.raised) {
                exception_info = exc;
                goto exception;
            }
        }

        if (re.insn_state == IS_EXCEPTION) {
        exception:
            if (state->verbosity & VERBOSE_DISASS)
                fprintf(stdout, "                  EXCEPTION %"PRId64" (%08"PRId64") RAISED\n",
                        exception_info.code, exception_info.info);

            int prev_rob_rp = rob_rp - 1;

            if (rob_rp == 0) {
                prev_rob_rp = ROB_SIZE - 1;
            }

            //assert(state->msr[0x300] == costate->msr[0x300]);
            flush_and_redirect(state, verbosity, prev_rob_rp, re.fp.seqno - 1,
                               arch->handle_exception(state, re.dec.insn_addr, exception_info));

            costate->pc = arch->handle_exception(costate, costate->pc, exception_info);
            //assert(state->msr[0x300] == costate->msr[0x300]);
            break;
        }

        if (re.dec.dest_reg != ISA_NO_REG) {
            art[re.dec.dest_reg] = re.result;
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

        if (override && re.dec.dest_reg != ISA_NO_REG)
            costate->r[re.dec.dest_reg] = re.result;

        if (re.dec.insn_addr != copc) {
            fprintf(stderr, "COSIM: REF PC %08"PRIx64" != DUT PC %08"PRIx64"\n",
                    copc & 0xFFFFFFFF, re.dec.insn_addr & 0xFFFFFFFF);
            fflush(stdout);
            assert(re.dec.insn_addr == copc);

        }

        if (re.dec.dest_reg != ISA_NO_REG) {
            if (re.result != costate->r[re.dec.dest_reg]) {
                fprintf(stderr, "COSIM: REF RES %08"PRIx64" != DUT RES %08"PRIx64"\n",
                        costate->r[re.dec.dest_reg] & 0xFFFFFFFF,
                        re.result);
                fflush(stdout);
                assert(re.result == costate->r[re.dec.dest_reg]);
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

        if (++rob_rp == ROB_SIZE)
            rob_rp = 0;
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
ooo_decode_rename(cpu_state_t *state, verbosity_t verbosity)
{
    /*
     * Decode and rename
     */
    while (0 < fb_size && !is_rob_full()) {

        fetch_parcel_t fetched = fb[fb_rp];
        isa_decoded_t dec      = arch->decode(fetched.addr, fetched.insn);

        // Serialize system instruction; block issuing until all
        // previous instructions have retired and then only allow a single one in

        if (dec.system && rob_rp != rob_wp)
            break;

        if (++fb_rp == FETCH_BUFFER_SIZE)
            fb_rp = 0;
        fb_size--;

        fetched.decode_ts = n_cycles;

        rob[rob_wp] = (rob_entry_t) {
            .insn_state = IS_FETCHED,
            .fp = fetched,
            .dec = dec
        };

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
        if (!get_reg(p, rob[p].dec.source_reg_b, &store_data)) {
            ++/*rob[p].stat.*/n_cycles_waiting_on_uncommitted_store_data;
            return false;
        }

#if 0
        // We have an overlapping store with data.  If it fully
        // overlaps, we take it and return.  XXX Only deal with same
        // sized data
        if (rob[p].dec.loadstore_size == size) {
            ++n_full_store_forwards;
            *res.result = signed_extend(store_data, load_type);
            return true;
        }
#endif

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

    switch (dec.class) {
    case isa_insn_class_load:
        // XXXXXXXXXX
        // XXXX We need to examine pending stores and forward as necessary XXX
        // XXX THIS IS KNOWN WRONG
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
        if (res.branch_taken) {
            rob[p].restart = true;
            rob[p].restart_pc = dec.jumpbranch_target;
        }
        break;

    case isa_insn_class_jump: {
            rob[p].restart = true;
            rob[p].restart_pc = dec.jumpbranch_target;
        }
        break;

    case isa_insn_class_compjump: {
            rob[p].restart = true;
            rob[p].restart_pc = res.compjump_target;
        }
        break;
    }

    rob[p].result     = res.result;
    rob[p].msr_result = res.msr_result;

    // Flush the pipe on system instructions unless is a compjump
    if (dec.system && dec.class != isa_insn_class_compjump) {
        rob[p].restart = true;
        rob[p].restart_pc = dec.insn_addr + 4;
    }

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
    // Commit previously executed insns
    for (unsigned p = rob_rp; p != rob_wp; p = p == ROB_SIZE - 1 ? 0 : p + 1)
        if (rob[p].insn_state == IS_EXECUTED) {
            rob[p].insn_state = IS_COMMITTED;
            rob[p].fp.commit_ts = n_cycles;
        }

    for (unsigned p = rob_rp; p != rob_wp; p = p == ROB_SIZE - 1 ? 0 : p + 1) {
        rob_entry_t re = rob[p];

        if (re.insn_state != IS_FETCHED)
            continue;

        // Stores are special and execute the address calculation even
        // if the data isn't ready.  Loads have additional dependency
        // on stores.

        uint64_t val_a, val_b;
        if (re.dec.class == isa_insn_class_store && !get_reg(p, re.dec.source_reg_a, &val_a))
            continue;
        else if (!get_reg(p, re.dec.source_reg_a, &val_a) ||
                 !get_reg(p, re.dec.source_reg_b, &val_b))
            continue;

        if (!ooo_exec1(state, verbosity, p, val_a, val_b))
            // Loads may not be able to execute yet
            continue;
    }
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

    for (n_cycles = 0;; ++n_cycles) {
        if (step_ooo(state, costate, verbosity))
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
