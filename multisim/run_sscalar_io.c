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

bool
step_sscalar_in_order(
    const arch_t *arch, cpu_state_t *state, cpu_state_t *costate,
    verbosity_t verbosity)
{
    static int cycle = 0;
    uint64_t orig_r[32];
    uint64_t pc       = state->pc;
    memory_exception_t error;
    uint32_t inst     = (uint32_t)arch->load(state, pc, 0 /* = ifetch */, &error);

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
