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
 * only ever run one instance of this.
 */

#define LINESIZELG2 5
#define DC_NSETLG2  8
#define IC_NSETLG2  9
#define MAX_NSETLG2  9

#define LINE_VALID  (1 << 31)

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
} cache_t;

/*
 * We don't know much about the memory, so I'll make persimistic
 * assumptions that we can only have a single transaction outstanding
 * of a certain latency.  However, I do assume we can transfer bursts
 * at the rate of 32-bit per cycle.
 */

#define MEMORY_CAS 6



static struct {
    int busy_cycles;
    cache_t *busy_with;
} memory;

static void cache_read(cache_t *c,
		       uint32_t address, bool readenable,
		       bool *ready,
		       uint32_t *rdata, bool *rdata_valid)
{
    memory_exception_t error;
    uint32_t word_address = address >> 2; // address[:2]
    uint32_t data_index   = word_address & ((1 << (c->nsetlg2 + LINESIZELG2 - 2)) - 1); // address[I:2]

    uint32_t line_index   = address >> LINESIZELG2;
    uint32_t line_offset  = address - (line_index << LINESIZELG2);
    uint32_t tag_bits     = line_index >> c->nsetlg2;
    uint32_t set_index    = line_index - (tag_bits << c->nsetlg2);

    switch (c->state) {
    case CSM_READY:
	*rdata = c->data[data_index];
	*rdata_valid = c->tag[set_index] == (tag_bits | LINE_VALID);

	if (!*rdata_valid) {
	    c->fill_counter = 1 << (LINESIZELG2 - 2);
	    c->fill_address = address & (-1 << LINESIZELG2);
	    c->tag_bits = tag_bits;
	    c->set_index = set_index;
	    c->data_index = data_index;
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

		printf("FILLING I$ from %08x -> <%d,%d>\n", 
		       c->fill_address,
		       word_index >> (LINESIZELG2 - 2),
		       word_index & ((1 << LINESIZELG2)/4 - 1));
		c->data[word_index] = (uint32_t)arch->load(state, ic_fill_address, 4, &error);
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
	*rdata_valid = true;
	break;

    default:
	assert(0); // XXX Not done yet
    }

    *ready = c->state == CSR_READY;
}


cache_t ic = { .nsetlg2 = IC_NSETLG2 };

#if 0
/*
static struct {
    bool valid;
    uint32_t pc;
    uint32_t insn;
    memory_exception_t error;
} fetch_output;


static void do_fetch(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    memory_exception_t error;

    fetch_output.valid = false;

    cache_read(&ic, 
	    
	
    }

    fetch.pc = state->pc;
        /* Using the world's simplest branch predictor: assume fall through */
        state->pc += 4;
        state->pc = CANONICALIZE(state->pc);
        fetch.insn = (uint32_t)arch->load(state, fetch.pc, 4, &fetch.error);
        fetch.valid = true;
    } else
        fetch.valid = false;
}
*/
#endif

bool
step_sscalar_in_order(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    static int cycle = 0;
    uint64_t orig_r[32];
    uint64_t pc       = state->pc;
    memory_exception_t error;
    uint32_t inst;
    bool ic_ready, inst_valid;

    cache_read(&ic, pc, 1, &ic_ready, &inst, &inst_valid);

    if (!inst_valid)
	goto skip;

    if (error != MEMORY_SUCCESS)
        return error == MEMORY_FATAL;

    /* Co-simulate */
    assert(pc == costate->pc);
    step_simple(arch, costate, 0);

    isa_decoded_t dec = arch->decode(pc, inst);

    if (verbosity & VERBOSE_TRACE)
        memcpy(orig_r, state->r, sizeof orig_r);

    assert(dec.source_reg_a == ISA_NO_REG || dec.source_reg_a < ISA_REGISTERS);
    assert(dec.source_reg_b == ISA_NO_REG || dec.source_reg_b < ISA_REGISTERS);
    assert(dec.dest_reg     == ISA_NO_REG || dec.dest_reg     < ISA_REGISTERS);
    assert(dec.dest_msr     == ISA_NO_REG || dec.dest_msr     < ISA_MSRS);
    assert(dec.source_msr_a == ISA_NO_REG || dec.source_msr_a < ISA_MSRS);

    uint64_t op_a     = state->r[dec.source_reg_a];
    uint64_t op_b     = state->r[dec.source_reg_b];
    uint64_t msr_a    = dec.source_msr_a != ISA_NO_REG ?
        arch->read_msr(state, dec.source_msr_a) : 0;

    uint64_t atomic_load_addr = op_a;

    if (dec.class == isa_inst_class_atomic)
        op_a = arch->load(state, atomic_load_addr, dec.loadstore_size, &error);

    if (error != MEMORY_SUCCESS)
        return (error == MEMORY_FATAL);

    /// XXX hack for now: we need to know if an exception was fired, just like with loads
    extern int exception_raised;
    exception_raised = 0;

    isa_result_t res;
    if (!dec.system)
	res = arch->inst_exec(dec, op_a, op_b, msr_a);
    else
	res = arch->inst_exec_system(state, dec, op_a, op_b, msr_a);
    res.result = CANONICALIZE(res.result);

    if (res.fatal_error)
        return true;

    if (exception_raised)
        return false;

    switch (dec.class) {
    case isa_inst_class_load:
	res.load_addr = CANONICALIZE(res.load_addr);
        res.result = arch->load(state, res.load_addr, dec.loadstore_size, &error);
        res.result = CANONICALIZE(res.result);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        state->pc += 4;
        break;

    case isa_inst_class_store:
	res.store_addr = CANONICALIZE(res.store_addr);
	res.store_value = CANONICALIZE(res.store_value);
        arch->store(state, res.store_addr, res.store_value, dec.loadstore_size, &error);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        state->pc += 4;
        break;

    case isa_inst_class_atomic:
	// XXX ??
	res.load_addr = CANONICALIZE(res.load_addr);
        arch->store(state, atomic_load_addr, res.result, dec.loadstore_size, &error);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        res.result = op_a;
        state->pc += 4;
        break;


    case isa_inst_class_branch:
	dec.jumpbranch_target = CANONICALIZE(dec.jumpbranch_target);
        state->pc = res.branch_taken ? dec.jumpbranch_target : state->pc + 4;
        break;

    case isa_inst_class_jump:
	dec.jumpbranch_target = CANONICALIZE(dec.jumpbranch_target);
        state->pc = dec.jumpbranch_target;
        break;

    case isa_inst_class_compjump:
	res.compjump_target = CANONICALIZE(res.compjump_target);
        state->pc = res.compjump_target;
        break;

    default:
        state->pc += 4;
        state->pc = CANONICALIZE(state->pc);
        break;
    }

    if (dec.dest_reg != ISA_NO_REG)
        state->r[dec.dest_reg] = res.result;

    if (dec.dest_msr != ISA_NO_REG)
        arch->write_msr(state, dec.dest_msr, res.msr_result);

    if (verbosity & VERBOSE_TRACE) {
        if (dec.dest_reg != ISA_NO_REG && orig_r[dec.dest_reg] != res.result)
            fprintf(stderr,"%d:r%d=0x%08"PRIx64"\n", cycle, dec.dest_reg,
                    res.result);
        fflush(stderr);
        ++cycle;
    }

    if (dec.dest_reg != ISA_NO_REG)
	assert(state->r[dec.dest_reg] == costate->r[dec.dest_reg]);

skip:
    arch->tick(state);

    return false;
}

void run_sscalar_io(int num_images, char *images[], verbosity_t verbosity)
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
        if (step_sscalar_in_order(arch, state, costate, verbosity))
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
