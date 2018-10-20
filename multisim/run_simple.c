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

extern verbosity_t verbosity_override;


bool
step_simple(const arch_t *arch, cpu_state_t *state, verbosity_t verbosity)
{
    static int cycle = 0;
    uint64_t orig_r[32];
    uint64_t pc       = state->pc;
    memory_exception_t error;
    uint32_t inst     = (uint32_t)arch->load(state, pc, 4, &error);

    if (error != MEMORY_SUCCESS)
        return (error == MEMORY_FATAL);

    if (verbosity & VERBOSE_TRACE)
        fprintf(stderr, "%5d:%08"PRIx64" %08x\n", cycle, pc, inst);

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

    isa_result_t res = arch->inst_exec(dec, op_a, op_b, msr_a);
    res.result = CANONICALIZE(res.result);

    if (res.fatal_error)
        return true;

    if (exception_raised)
        return false;

    switch (dec.class) {
    case isa_inst_class_load:
        res.result = arch->load(state, res.load_addr, dec.loadstore_size, &error);
        res.result = CANONICALIZE(res.result);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        state->pc += 4;
        break;

    case isa_inst_class_store:
        arch->store(state, res.store_addr, res.store_value, dec.loadstore_size, &error);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        state->pc += 4;
        break;

    case isa_inst_class_atomic:
        arch->store(state, atomic_load_addr, res.result, dec.loadstore_size, &error);

        if (error != MEMORY_SUCCESS)
            return (error == MEMORY_FATAL);

        res.result = op_a;
        state->pc += 4;
        break;


    case isa_inst_class_branch:
        state->pc = res.branch_taken ? dec.jumpbranch_target : state->pc + 4;
        break;

    case isa_inst_class_jump:
        state->pc = dec.jumpbranch_target;
        break;

    case isa_inst_class_compjump:
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

    if ((verbosity | verbosity_override) & VERBOSE_DISASS)
        isa_disass(arch, dec, res);

    arch->tick(state);

    return false;
}

void run_simple(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    const arch_t *arch;
    elf_info_t info;
    int cycle;

    loadelfs(state->mem, num_images, images, &info);

    // hack to support vmlinux which expects to be relocated?
    if (info.text_segments >= 1 &&
        (int64_t) info.text_start < 0)
        info.program_entry += info.text_start;

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info);

    for (cycle = 0;; ++cycle) {
        if (step_simple(arch, state, verbosity))
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
