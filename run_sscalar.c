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

const bool cosimulating = true;

// XXX The cache model doesn't really belong here, but we'll develop
// it along with the superscalar implementation

/*
 * Let's first model a simple five stage pipeline with a 16 KiB
 * directly mapped I$ and a 8 KiB directly mapped D$.  We'll use 32 B
 * lines, which bring us to 512 lines for the I$ and 256 for the D$.
 *
 * We'll also fix the address space at 128 MiB (for now), which means
 * 27 VA/PA bits and thus 13-14 bits wide tags, plus a valid bit.
 * Access to the arrays are 32-bit wide (possibly wider or banked in
 * future for wider fetch).  Note, that bit 31 must be valid, so we
 * have "holes" in the address bits and describe this with a mask.
 *
 * NB: normally we'd keep all this state in the `cpu_state`, but we'll
 * only ever run one insnance of this.
 */

#define LINESIZELG2 5
#define DC_NSETLG2  8
#define IC_NSETLG2  9
#define MAX_NSETLG2  9

#define LINE_VALID  (1 << 31)

/*
 * We don't know much about the memory, so I'll make persimistic
 * assumptions that we can only have a single transaction outstanding
 * of a certain latency.  However, I do assume we can transfer bursts
 * at the rate of 32-bit per cycle.
 */
#define MEMORY_CAS 6

/* Cache State Machine */
typedef enum {
    CSM_READY,

    CSM_WANT_TO_READ,
    CSM_FILLING,
    CSM_UPDATE_TAG,

    CSM_WANT_TO_WRITE,
    CSM_WRITING,
} cache_state_t;

typedef struct cache_st {
    int nsetlg2;
    uint32_t tag[1 << MAX_NSETLG2];
    uint32_t data[1 << (MAX_NSETLG2 + LINESIZELG2 - 2)];
    cache_state_t state;

    int fill_counter;
    uint32_t fill_address;
    uint32_t tag_bits;
    uint32_t set_index;
    uint32_t data_index;

    uint32_t orig_address;
} cache_t;

static struct {
    int busy_cycles;
    cache_t *busy_with;
} memory;

static void cache_fsm(const arch_t *arch, cpu_state_t *state, cache_t *c,
                       // Inputs
                       uint32_t address, bool readenable, bool reset_cache_access,
                       // Outputs
                       bool *ready, uint32_t *rdata, bool *rdata_valid)
{
    memory_exception_t error;

    uint32_t word_address = address >> 2; // address[:2]
    uint32_t data_index   = word_address & ((1 << (c->nsetlg2 + LINESIZELG2 - 2)) - 1); // address[I:2]

    uint32_t line_index   = address >> LINESIZELG2;
    uint32_t tag_bits     = line_index >> c->nsetlg2;
    uint32_t set_index    = line_index - (tag_bits << c->nsetlg2);

    *rdata                = c->data[data_index];
    *rdata_valid          = c->tag[set_index] == (tag_bits | LINE_VALID) && !reset_cache_access;

    if (reset_cache_access) {
        if (state->verbosity & VERBOSE_CACHE)
            fprintf(stderr, "I$ reset to %08x\n", address);
        // We cannot stop the FSM as we may already have a memory transaction going
        // c->state = CSM_READY;
    }

    switch (c->state) {
    case CSM_READY:
        if (!*rdata_valid) {

            if (state->verbosity & VERBOSE_CACHE)
                fprintf(stderr, "I$ MISS on %08x\n", address);

            if (state->verbosity & VERBOSE_CACHE && c->tag[set_index] & LINE_VALID)
                fprintf(stderr, "I$ EVICT  %08x\n",
                       ((c->tag[set_index] << c->nsetlg2) + set_index) << LINESIZELG2);

            c->fill_counter = 1 << (LINESIZELG2 - 2);
            c->fill_address = address & (-1 << LINESIZELG2);
            c->tag_bits = tag_bits;
            c->set_index = set_index;
            c->data_index = data_index;

            c->orig_address = address;

            c->state = CSM_WANT_TO_READ; // XXX we could skip this if memory is ready
        }
        break;

    case CSM_WANT_TO_READ:
        if (memory.busy_with == 0) {
            memory.busy_with = c;
            memory.busy_cycles = MEMORY_CAS;
            c->state = CSM_FILLING;
        }
        break;

    case CSM_FILLING:
        if (memory.busy_cycles == 0) {
            if (c->fill_counter) {
                int word_index      = (c->fill_address >> 2) & ((1 << (c->nsetlg2 + LINESIZELG2 - 2)) - 1);
                c->data[word_index] = (uint32_t)arch->load(state, c->fill_address, 4, &error);
                c->fill_counter    -= 1;
                c->fill_address    += 4;
                if (c->fill_counter == 0) {
                    memory.busy_with = 0;
                    c->state         = CSM_UPDATE_TAG;
                }
            }
        } else
            --memory.busy_cycles;
        break;

    case CSM_UPDATE_TAG:
        c->tag[c->set_index] = c->tag_bits | LINE_VALID;
        c->state = CSM_READY;
        break;

    default: assert(0); // XXX Not done yet
    }

    *ready = c->state == CSM_READY;
}


cache_t ic = { .nsetlg2 = IC_NSETLG2 };

static struct {
    // State
    uint32_t pc;
    struct {
        uint32_t is_taken_branch, branch_target;
    } bp;

    // Inputs
    bool ic_ready;

    // Outputs
    bool valid;
    bool reset_cache_access;
    uint32_t insn_addr;
    uint32_t insn;
    bool     predicted_taken_branch;
    uint32_t predicted_branch_target;
    memory_exception_t error;
} fetch, fetch_next;

static struct {
    // Outputs
    bool valid;
    bool     predicted_taken_branch;
    uint32_t predicted_branch_target;
    isa_decoded_t dec;
    uint64_t op_a;
    uint64_t op_b;
    uint64_t msr_a;
} decode, decode_next;

static struct {
    // Outputs
    bool valid;
    isa_decoded_t dec;
    isa_result_t res;
} execute, execute_next;

static struct {
    // Outputs
    bool valid;
    isa_decoded_t dec;
    isa_result_t res;
} mem, mem_next;

static struct {
    // Outputs
    bool valid;
    isa_decoded_t dec;
    isa_result_t res;
} commit, commit_next;

static void flush_pipe_and_restart_from(uint32_t new_pc)
{
    fetch_next.pc = new_pc;
    fetch_next.reset_cache_access = true;
    fetch_next.valid = false;
    decode_next.valid = false;
}

int branches;
int mispredicts;

static bool step_sscalar(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    memory_exception_t error = MEMORY_SUCCESS;
    int instret = 0;

    fetch   = fetch_next;
    decode  = decode_next;
    execute = execute_next;
    mem     = mem_next;
    commit  = commit_next;

    fetch_next.reset_cache_access = false;

    /* Fetch */
    cache_fsm(arch, state, &ic,
              fetch.pc, true, fetch.reset_cache_access,
              &fetch_next.ic_ready, &fetch_next.insn, &fetch_next.valid);

    fetch_next.insn_addr = fetch.pc;

    if (fetch_next.valid) {
        if (1 && fetch.pc == fetch.bp.is_taken_branch) {
            fetch_next.pc                      = fetch.bp.branch_target;
            fetch_next.predicted_branch_target = fetch.bp.branch_target;
            fetch_next.predicted_taken_branch  = true;
        } else {
            fetch_next.pc = CANONICALIZE(fetch.pc + 4);
            fetch_next.predicted_taken_branch = false;
        }
    }

    /* Decode */
    decode_next.valid = fetch.valid;
    if (fetch.valid) {
        decode_next.dec = arch->decode(fetch.insn_addr, fetch.insn);
        decode_next.op_a = state->r[decode_next.dec.source_reg_a];
        decode_next.op_b = state->r[decode_next.dec.source_reg_b];
        decode_next.msr_a = decode_next.dec.source_msr_a == ISA_NO_REG ? 0 :
            arch->read_msr(state, decode_next.dec.source_msr_a);
        decode_next.predicted_taken_branch = fetch.predicted_taken_branch;
        decode_next.predicted_branch_target = fetch.predicted_branch_target;
    }

    /* Execute */
    execute_next.valid = decode.valid;
    execute_next.dec   = decode.dec;
    if (decode.valid) {
        /* Forward */
        if (decode.dec.source_reg_a == execute.dec.dest_reg && execute.valid)
            decode.op_a = execute.res.result;
        else if (decode.dec.source_reg_a == mem.dec.dest_reg && mem.valid)
            decode.op_a = mem.res.result;
        else if (decode.dec.source_reg_a == commit.dec.dest_reg && commit.valid)
            decode.op_a = commit.res.result;

        if (decode.dec.source_reg_b == execute.dec.dest_reg && execute.valid)
            decode.op_b = execute.res.result;
        else if (decode.dec.source_reg_b == mem.dec.dest_reg && mem.valid)
            decode.op_b = mem.res.result;
        else if (decode.dec.source_reg_b == commit.dec.dest_reg && commit.valid)
            decode.op_b = commit.res.result;

        if (0)
        fprintf(stderr, " [%s=%08x%s, %s=%08x%s]",
                arch->reg_name[decode.dec.source_reg_a],
                (uint32_t)decode.op_a,
                decode.dec.source_reg_a == execute.dec.dest_reg ? " fwd" : "",

                arch->reg_name[decode.dec.source_reg_b],
                (uint32_t)decode.op_b, decode.dec.source_reg_b == execute.dec.dest_reg ? " fwd" : "");

        if (decode.dec.system) {
            if (decode.dec.insn == 0x100073) /* EBREAK */
                return true;

            execute_next.res = arch->insn_exec_system(state, decode.dec, decode.op_a, decode.op_b, decode.msr_a);
            flush_pipe_and_restart_from(decode.dec.insn_addr + 4); // XXX wrong for at least ECALL
        }
        else
            execute_next.res = arch->insn_exec(decode.dec, decode.op_a, decode.op_b, decode.msr_a);
        execute_next.res.result = CANONICALIZE(execute_next.res.result);

        switch (decode.dec.class) {
        case isa_insn_class_load:
            execute_next.res.load_addr = CANONICALIZE(execute_next.res.load_addr);
            execute_next.res.result = arch->load(state, execute_next.res.load_addr, decode.dec.loadstore_size, &error);
            execute_next.res.result = CANONICALIZE(execute_next.res.result);
            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);
            break;

        case isa_insn_class_store:
            execute_next.res.store_addr = CANONICALIZE(execute_next.res.store_addr);
            execute_next.res.store_value = CANONICALIZE(execute_next.res.store_value);
            arch->store(state, execute_next.res.store_addr, execute_next.res.store_value, decode.dec.loadstore_size, &error);
            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);
            break;

        case isa_insn_class_branch:
            execute_next.dec.jumpbranch_target = CANONICALIZE(execute_next.dec.jumpbranch_target);
            if (execute_next.res.branch_taken      != decode.predicted_taken_branch ||
                execute_next.res.branch_taken &&
                execute_next.dec.jumpbranch_target != decode.predicted_branch_target) {
                // Mispredicted
                fprintf(stderr, "Mispredicted: %08lx: predicted %staken, target %08x, was %staken, target %08lx\n",
                        decode.dec.insn_addr,
                        decode.predicted_taken_branch ? "" : "non-",
                        decode.predicted_branch_target,
                        execute_next.res.branch_taken ? "" : "non-",
                        execute_next.dec.jumpbranch_target);

                ++mispredicts;
                if (execute_next.res.branch_taken) {
                    fetch_next.bp.is_taken_branch = decode.dec.insn_addr;
                    fetch_next.bp.branch_target   = execute_next.dec.jumpbranch_target;
                    flush_pipe_and_restart_from(execute_next.dec.jumpbranch_target);
                    fprintf(stderr, "  Restarting from %08lx\n", execute_next.dec.jumpbranch_target);
                } else {
                    fetch_next.bp.is_taken_branch = 0;
                    flush_pipe_and_restart_from(decode.dec.insn_addr + 4);
                    fprintf(stderr, "  Restarting to  %08lx\n", decode.dec.insn_addr + 4);
                }
            } else {
                fprintf(stderr, "Good predicted: %08lx: predicted %staken, target %08x, was %staken, target %08lx\n",
                        decode.dec.insn_addr,
                        decode.predicted_taken_branch ? "" : "non-",
                        decode.predicted_branch_target,
                        execute_next.res.branch_taken ? "" : "non-",
                        execute_next.dec.jumpbranch_target);

            }
            ++branches;
            break;

        case isa_insn_class_jump:
            decode.dec.jumpbranch_target = CANONICALIZE(decode.dec.jumpbranch_target);
            flush_pipe_and_restart_from(decode.dec.jumpbranch_target);
            break;

        case isa_insn_class_compjump:
            execute_next.res.compjump_target = CANONICALIZE(execute_next.res.compjump_target);
            flush_pipe_and_restart_from(execute_next.res.compjump_target);
            break;

        default:
            break;
        }
    }
    mem_next.valid = execute.valid;
    mem_next.dec   = execute.dec;
    mem_next.res   = execute.res;

    commit_next.valid = mem.valid;
    commit_next.dec   = mem.dec;
    commit_next.res   = mem.res;


    if (state->verbosity & VERBOSE_PIPE) {
        if (fetch.valid)
            fprintf(stderr, " %08x", fetch.insn_addr);
        else
            fprintf(stderr, " --------");

        if (decode.valid)
            fprintf(stderr, " %08x", (uint32_t)decode.dec.insn_addr);
        else
            fprintf(stderr, " --------");

        if (execute.valid)
            fprintf(stderr, " %08x:%s=%08x", (uint32_t)execute.dec.insn_addr,
                    execute.dec.dest_reg == ISA_NO_REG ? "--" : arch->reg_name[execute.dec.dest_reg],
                    (uint32_t)execute.res.result);
        else
            fprintf(stderr, " --------------------");

        if (cosimulating)
            fprintf(stderr, " ");
        else
            fprintf(stderr, "\n");
    }


    /* Write back (= Commit stage) */
    if (mem.valid) {
        if (mem.dec.dest_reg != ISA_NO_REG)
            state->r[mem.dec.dest_reg] = mem.res.result;
        if (mem.dec.dest_msr != ISA_NO_REG)
            arch->write_msr(state, mem.dec.dest_msr, mem.res.msr_result);

        /* Check Co-simulation */
        if (cosimulating) {
            if (mem.dec.insn_addr != costate->pc) {
                fprintf(stderr, "my pc %08x != models %08x\n", (uint32_t)mem.dec.insn_addr, (uint32_t)costate->pc);
                assert(mem.dec.insn_addr == costate->pc);
            }

            uint32_t coinsn = (uint32_t)arch->load(costate, mem.dec.insn_addr, 4, &error);
            if (mem.dec.insn != coinsn)
                assert(mem.dec.insn == coinsn);

            step_simple(arch, costate);

            /* Need to override the simple model for things that involve the cycle csr */
            if (mem.dec.source_msr_a == /*CSR_CYCLE*/ 0xC00 ||  // XXX Breaks the abstraction!
                mem.dec.source_msr_a == /*CSR_INSTRET*/ 0xC02)
                costate->r[mem.dec.dest_reg] = state->r[mem.dec.dest_reg];

            if (mem.dec.dest_reg != ISA_NO_REG &&
                state->r[mem.dec.dest_reg] != costate->r[mem.dec.dest_reg]) {

                fprintf(stderr, "my r%d/%s = %08x != models %08x\n",
                        mem.dec.dest_reg,
                        arch->reg_name[mem.dec.dest_reg],
                        (uint32_t)state->r[mem.dec.dest_reg],
                        (uint32_t)costate->r[mem.dec.dest_reg]);

                assert(state->r[mem.dec.dest_reg] == costate->r[mem.dec.dest_reg]);
            }
        }

        instret += 1;
    } else if (cosimulating)
        fprintf(stderr, "\n");

#if 0
    static uint32_t dc_readdata  = 0;
    static bool dc_ready         = true;
    static bool dc_readdatavalid = false;
    static bool dc_readenable    = false;

    /* Memory */
    if (execute.valid && execute.dec.class == isa_insn_class_load) {
        ... scoreboard dest-reg as not-ready ...;
        ... enqueue a load ...;
    }

    if (execute.valid && execute.dec.class == isa_insn_class_store) {
        ... enqueue a store ...;
    }

    if (load_result is ready & !execute.valid) {
        ... send a load result down the pipe ...;
    }

    if (dc_ready) {
        dc_readenable  = execute.valid && execute.dec.class == isa_insn_class_load;
        dc_readaddress = CANONICALIZE(execute.res.load_addr);
    } else if (execute.valid && execute.dec.class == isa_insn_class_load)


    cache_fsm(arch, state, &dc,
              dc_readaddress, dc_readenable, false,
              &dc_ready, &dc_readdata, &dc_readdatavalid);

    if (execute.valid) {
        memory.dec = execute.dec;
        memory.res = execute.res;

        switch (execute.dec.class) {
        case isa_insn_class_load:
            memory.res.load_addr = CANONICALIZE(execute.res.load_addr);

            cache_fsm(arch, state, &dc,
                       CANONICALIZE(execute.res.load_addr), 1, false,
                       &fetch.ic_ready, &fetch.insn, &fetch.valid);

            memory.res.result = arch->load(state, execute.res.load_addr, execute.dec.loadstore_size, &error);
            memory.res.result = CANONICALIZE(memory.res.result);
            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);
            break;

        case isa_insn_class_store:
            execute.res.store_addr = CANONICALIZE(execute.res.store_addr);
            execute.res.store_value = CANONICALIZE(execute.res.store_value);
            arch->store(state, execute.res.store_addr, execute.res.store_value, decode.dec.loadstore_size, &error);
            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);
            break;
        }
    }
#endif

    arch->tick(state, instret);

    return false;
}

void run_sscalar(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    cpu_state_t *costate = state_create();
    const arch_t *arch;
    elf_info_t info;

    loadelfs(state->mem, num_images, images, &info);
    loadelfs(costate->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);
    arch->setup(costate, &info, verbosity & ~VERBOSE_CONSOLE);
    fetch_next.pc = state->pc;

    for (;;) {
        if (step_sscalar(arch, state, costate, verbosity))
            break;
    }

    if (verbosity) {
        printf("CPI = %.2f\n", state->counter / (double) costate->counter);
        printf("%d branches, %d mispredicts = %5.2f%% accurate\n", branches, mispredicts, 100 - 100.0*mispredicts/(double)branches);
    }
    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
