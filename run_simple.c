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
step_simple(const arch_t *arch, cpu_state_t *state)
{
    static int cycle = 0;
    uint64_t pc       = state->pc;
    memory_exception_t error;
    uint32_t insn     = (uint32_t)arch->load(state, pc, 0 /* = ifetch */, &error);

    if (error != MEMORY_SUCCESS)
	/* arch->raise_exception(...);
           return;  */
        return error == MEMORY_FATAL;

    isa_decoded_t dec = arch->decode(pc, insn);

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

    if (dec.class == isa_insn_class_atomic)
        op_a = arch->load(state, atomic_load_addr, dec.loadstore_size, &error);

    // XXX If (raised exception) return;

    if (error != MEMORY_SUCCESS)
        return (error == MEMORY_FATAL);

    /// XXX hack for now: we need to know if an exception was fired, just like with loads
    extern int exception_raised;
    exception_raised = 0;

    isa_result_t res;

    if (!dec.system)
	res = arch->insn_exec(dec, op_a, op_b, msr_a);
    else
	res = arch->insn_exec_system(state, dec, op_a, op_b, msr_a);
    res.result = CANONICALIZE(res.result);

    if (res.fatal_error) // XXX If (raised exception) return;
        return true;

    if (exception_raised)
        return false;

    switch (dec.class) {
    case isa_insn_class_load:
	res.load_addr = CANONICALIZE(res.load_addr);
        res.result = arch->load(state, res.load_addr, dec.loadstore_size, &error);
        res.result = CANONICALIZE(res.result);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        state->pc += 4;
        break;

    case isa_insn_class_store:
	res.store_addr = CANONICALIZE(res.store_addr);
	res.store_value = CANONICALIZE(res.store_value);
        arch->store(state, res.store_addr, res.store_value, dec.loadstore_size, &error);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        state->pc += 4;
        break;

    case isa_insn_class_atomic:
	// XXX ??
	res.load_addr = CANONICALIZE(res.load_addr);
        arch->store(state, atomic_load_addr, res.result, dec.loadstore_size, &error);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        res.result = op_a;
        state->pc += 4;
        break;


    case isa_insn_class_branch:
	dec.jumpbranch_target = CANONICALIZE(dec.jumpbranch_target);
        state->pc = res.branch_taken ? dec.jumpbranch_target : state->pc + 4;
        break;

    case isa_insn_class_jump:
	dec.jumpbranch_target = CANONICALIZE(dec.jumpbranch_target);
        state->pc = dec.jumpbranch_target;
        break;

    case isa_insn_class_compjump:
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

    if (state->verbosity & VERBOSE_TRACE) {
        fprintf(stderr, "%5d:%08"PRIx64" %08x ", cycle, pc, insn);
        if (dec.dest_reg != ISA_NO_REG)
            fprintf(stderr, "r%-2d = %08x\n", dec.dest_reg, (uint32_t)res.result);
        else
            fprintf(stderr, " .   .   .   .\n");
        ++cycle;
    }

    if (state->verbosity & VERBOSE_DISASS)
        isa_disass(arch, dec, res);

    arch->tick(state, 1);

    return false;
}

void run_simple(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    const arch_t *arch;
    elf_info_t info;
    int cycle;

    loadelfs(state->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);

    for (cycle = 0;; ++cycle) {
        if (step_simple(arch, state))
            break;
    }

    if (verbosity && (verbosity & VERBOSE_DISASS) == 0)
        printf("IPC = %.2f\n", (double) state->n_issue / cycle);

    state_destroy(state);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
