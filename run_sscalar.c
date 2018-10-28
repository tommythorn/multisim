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
const bool debug_icache = false;

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

static void cache_read(const arch_t *arch, cpu_state_t *state, cache_t *c,
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

    if (reset_cache_access && c->state != CSM_READY) {
        if (debug_icache)
            printf("I$ reset to %08x\n", address);
        c->state = CSM_READY;
    }

    switch (c->state) {
    case CSM_READY:
        *rdata = c->data[data_index];
        *rdata_valid = c->tag[set_index] == (tag_bits | LINE_VALID);

        if (!*rdata_valid) {

            if (debug_icache)
                printf("I$ MISS on %08x\n", address);

            if (debug_icache && c->tag[set_index] & LINE_VALID)
                printf("I$ EVICT  %08x\n",
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
                int word_index = (c->fill_address >> 2) & ((1 << (c->nsetlg2 + LINESIZELG2 - 2)) - 1);

                if (0)
                printf("FILLING I$ from %08x -> <%d,%d>\n",
                       c->fill_address,
                       word_index >> (LINESIZELG2 - 2),
                       word_index & ((1 << LINESIZELG2)/4 - 1));
                c->data[word_index] = (uint32_t)arch->load(state, c->fill_address, 4, &error);
                c->fill_counter -= 1;
                c->fill_address += 4;
                if (c->fill_counter == 0) {
                    memory.busy_with = 0;
                    c->state = CSM_UPDATE_TAG;
                }
            }
        } else
            --memory.busy_cycles;
        break;

    case CSM_UPDATE_TAG:
        c->tag[c->set_index] = c->tag_bits | LINE_VALID;
        c->state = CSM_READY;
        *rdata = c->data[c->data_index];

        if (address != c->orig_address)
            printf("I$ request address has changed! was %08x, now %08x\n",
                   c->orig_address, address);
        else
            *rdata_valid = true;
        break;

    default:
        assert(0); // XXX Not done yet
    }

    *ready = c->state == CSM_READY;
}


cache_t ic = { .nsetlg2 = IC_NSETLG2 };

static struct {
    // Inputs
    bool ic_ready;

    // Outputs
    bool valid;
    bool reset_cache_access;
    uint32_t pc;
    uint32_t insn;
    memory_exception_t error;
} fetch;

static struct {
    // Outputs
    bool valid;
    isa_decoded_t dec;
    uint64_t op_a;
    uint64_t op_b;
    uint64_t msr_a;
} decode;


static struct {
    // Outputs
    bool valid;
    isa_result_t res;
    isa_decoded_t dec;
} execute;

static bool step_sscalar(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    memory_exception_t error = MEMORY_SUCCESS;
    int instret = 0;

    /* Execute */

    execute.valid = decode.valid;
    execute.dec   = decode.dec;
    if (decode.valid) {
        uint64_t atomic_load_addr = decode.op_a;

        if (decode.dec.class == isa_insn_class_atomic)
            decode.op_a = arch->load(state, atomic_load_addr, decode.dec.loadstore_size, &error);

        // if (error != MEMORY_SUCCESS) return (error == MEMORY_FATAL);

        if (!decode.dec.system)
            execute.res = arch->insn_exec(decode.dec, decode.op_a, decode.op_b, decode.msr_a);
        else {
            execute.res = arch->insn_exec_system(state, decode.dec, decode.op_a, decode.op_b, decode.msr_a);
            /* Serializing instruction, flush the pipe */
            state->pc = decode.dec.insn_addr + 4;
            fetch.reset_cache_access = true;
            fetch.valid = false;
            decode.valid = false;
        }
        execute.res.result = CANONICALIZE(execute.res.result);

        if (execute.res.fatal_error)
            return true;

        switch (decode.dec.class) {
        case isa_insn_class_load:
            execute.res.load_addr = CANONICALIZE(execute.res.load_addr);
            execute.res.result = arch->load(state, execute.res.load_addr, decode.dec.loadstore_size, &error);
            execute.res.result = CANONICALIZE(execute.res.result);
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

        case isa_insn_class_atomic:
            // XXX ??
            execute.res.load_addr = CANONICALIZE(execute.res.load_addr);
            arch->store(state, atomic_load_addr, execute.res.result, decode.dec.loadstore_size, &error);
            if (error != MEMORY_SUCCESS)
                return (error == MEMORY_FATAL);
            execute.res.result = decode.op_a;
            break;

        case isa_insn_class_branch:
            decode.dec.jumpbranch_target = CANONICALIZE(decode.dec.jumpbranch_target);
            if (execute.res.branch_taken != false) {
                // Mispredict
                state->pc = decode.dec.jumpbranch_target;
                fetch.reset_cache_access = true;
                fetch.valid = false;
                decode.valid = false;
            }
            break;

        case isa_insn_class_jump:
            decode.dec.jumpbranch_target = CANONICALIZE(decode.dec.jumpbranch_target);
            state->pc = decode.dec.jumpbranch_target;
            // Mispredict
            fetch.reset_cache_access = true;
            fetch.valid = false;
            decode.valid = false;
            break;

        case isa_insn_class_compjump:
            execute.res.compjump_target = CANONICALIZE(execute.res.compjump_target);
            state->pc = execute.res.compjump_target;
            // Mispredict
            fetch.reset_cache_access = true;
            fetch.valid = false;
            decode.valid = false;
            break;

        default:
            break;
        }

        if (decode.dec.dest_reg != ISA_NO_REG)
            state->r[decode.dec.dest_reg] = execute.res.result;

        if (decode.dec.dest_msr != ISA_NO_REG)
            arch->write_msr(state, decode.dec.dest_msr, execute.res.msr_result);

        /* Co-simulate XXX Switch this to check at commit time */
        if (execute.valid && cosimulating) {
            if (execute.dec.insn_addr != costate->pc)
                assert(execute.dec.insn_addr == costate->pc);
            else {
                uint32_t coinsn = (uint32_t)arch->load(costate, execute.dec.insn_addr, 4, &error);
                if (execute.dec.insn != coinsn)
                    assert(execute.dec.insn == coinsn);
                else {
                    step_simple(arch, costate, verbosity);
                    if (execute.dec.dest_reg != ISA_NO_REG &&
                        state->r[execute.dec.dest_reg] != costate->r[execute.dec.dest_reg]) {

                        printf("my r%d/%s = %08x != models %08x\n",
                               execute.dec.dest_reg,
                               arch->reg_name[execute.dec.dest_reg],
                               (uint32_t)state->r[execute.dec.dest_reg],
                               (uint32_t)costate->r[execute.dec.dest_reg]);

                        assert(state->r[execute.dec.dest_reg] == costate->r[execute.dec.dest_reg]);
                    }
                }
            }
        }

        instret += 1;
    }

    /* Decode */
    if (fetch.valid) {
        decode.dec = arch->decode(fetch.pc, fetch.insn);

        assert(decode.dec.source_reg_a == ISA_NO_REG || decode.dec.source_reg_a < ISA_REGISTERS);
        assert(decode.dec.source_reg_b == ISA_NO_REG || decode.dec.source_reg_b < ISA_REGISTERS);
        assert(decode.dec.dest_reg     == ISA_NO_REG || decode.dec.dest_reg     < ISA_REGISTERS);
        assert(decode.dec.dest_msr     == ISA_NO_REG || decode.dec.dest_msr     < ISA_MSRS);
        assert(decode.dec.source_msr_a == ISA_NO_REG || decode.dec.source_msr_a < ISA_MSRS);

        decode.op_a     = (decode.dec.source_reg_a == execute.dec.dest_reg
                           ? execute.res.result
                           : state->r[decode.dec.source_reg_a]);

        decode.op_b     = (decode.dec.source_reg_b == execute.dec.dest_reg
                           ? execute.res.result
                           : state->r[decode.dec.source_reg_b]);

        decode.msr_a    = decode.dec.source_msr_a == ISA_NO_REG ? 0 :
            (cosimulating ?
             // NOTE, reading MSRs from the co-state to deal with divergences in MSRs
             arch->read_msr(costate, decode.dec.source_msr_a) :
             arch->read_msr(state, decode.dec.source_msr_a));
    }
    decode.valid = fetch.valid;

    /* Fetch */
    fetch.pc = state->pc;
    cache_read(arch, state, &ic,
               fetch.pc, 1, fetch.reset_cache_access,
               &fetch.ic_ready, &fetch.insn, &fetch.valid);

    if (fetch.valid) {
        state->pc += 4;
        state->pc = CANONICALIZE(state->pc);
    }

    fetch.reset_cache_access = false;

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
    arch->setup(state, &info);
    arch->setup(costate, &info);

    int cycle;
    for (cycle = 0;; ++cycle) {
        if (step_sscalar(arch, state, costate, verbosity))
            break;
    }

    if (verbosity)
        printf("IPC = %.2f\n", (double) state->n_issue / cycle);

    state_destroy(state);
    state_destroy(costate);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
