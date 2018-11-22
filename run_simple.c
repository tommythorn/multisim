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
    isa_exception_t exc = { 0 };
    uint64_t pc = state->pc;
    uint32_t insn;

    insn = (uint32_t)arch->load(state, pc, 0 /* = ifetch */, &exc);
    assert(!exc.raised);

    isa_decoded_t dec = arch->decode(pc, insn);

    assert(dec.source_reg_a == ISA_NO_REG || dec.source_reg_a < ISA_REGISTERS);
    assert(dec.source_reg_b == ISA_NO_REG || dec.source_reg_b < ISA_REGISTERS);
    assert(dec.dest_reg     == ISA_NO_REG || dec.dest_reg     < ISA_REGISTERS);
    assert(dec.dest_msr     == ISA_NO_REG || dec.dest_msr     < ISA_MSRS);
    assert(dec.source_msr_a == ISA_NO_REG || dec.source_msr_a < ISA_MSRS);

    uint64_t op_a     = state->r[dec.source_reg_a];
    uint64_t op_b     = state->r[dec.source_reg_b];
    uint64_t msr_a    = dec.source_msr_a != ISA_NO_REG ?
        arch->read_msr(state, dec.source_msr_a, &exc) : 0;

    uint64_t atomic_load_addr = op_a;

    if (exc.raised)
        goto exception;

    if (dec.class == isa_insn_class_atomic)
        op_a = arch->load(state, atomic_load_addr, dec.loadstore_size, &exc);

    if (exc.raised)
        goto exception;

    isa_result_t res;

    if (!dec.system)
	res = arch->insn_exec(dec, op_a, op_b, msr_a, &exc);
    else
	res = arch->insn_exec_system(state, dec, op_a, op_b, msr_a, &exc);
    res.result = CANONICALIZE(res.result);

    if (exc.raised)
        goto exception;

    switch (dec.class) {
    case isa_insn_class_load:
        res.load_addr = CANONICALIZE(res.load_addr);
        res.result = arch->load(state, res.load_addr, dec.loadstore_size, &exc);
        res.result = CANONICALIZE(res.result);

        if (exc.raised)
            goto exception;

        state->pc += 4;
        break;

    case isa_insn_class_store:
        res.store_addr = CANONICALIZE(res.store_addr);
        res.store_value = CANONICALIZE(res.store_value);
        arch->store(state, res.store_addr, res.store_value, dec.loadstore_size, &exc);

        if (exc.raised)
            goto exception;

        state->pc += 4;
        break;

    case isa_insn_class_atomic:
        // XXX ??
        res.load_addr = CANONICALIZE(res.load_addr);
        arch->store(state, atomic_load_addr, res.result, dec.loadstore_size, &exc);

        if (exc.raised)
            goto exception;

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

    if (exc.raised)
        goto exception;

    if (dec.dest_reg != ISA_NO_REG)
        state->r[dec.dest_reg] = res.result;

    if (dec.dest_msr != ISA_NO_REG)
        arch->write_msr(state, dec.dest_msr, res.msr_result, &exc);

exception:
    if (state->verbosity & VERBOSE_DISASS)
        isa_disass(arch, dec, res);

    if (exc.raised) {
        if (state->verbosity & VERBOSE_DISASS)
            fprintf(stderr, "                  EXCEPTION %ld (%08lx) RAISED\n", exc.code, exc.info);

        state->pc = arch->handle_exception(state, dec.insn_addr, exc);
    }

    arch->tick(state, 1);

    return false;
}

void run_simple(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    const arch_t *arch;
    elf_info_t info;

    loadelfs(state->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);

    uint64_t tohost = 0;
    isa_exception_t exc = { 0 };
    getelfsym(&info, "tohost", &tohost);

    for (;;) {
        if (step_simple(arch, state))
            break;

        if (verbosity & VERBOSE_COMPLIANCE &&
            arch->load(state, tohost, 4, &exc))
            break;
    }

    if (verbosity & VERBOSE_DISASS)
        printf("IPC = %.2f\n", (double) state->n_issue / state->counter);

    if (verbosity & VERBOSE_COMPLIANCE) {
        uint64_t begin_signature;
        uint64_t end_signature;

        if (getelfsym(&info, "begin_signature", &begin_signature) &&
            getelfsym(&info, "end_signature", &end_signature))
            for (uint32_t a = begin_signature; a < end_signature; a += 4)
                printf("%08lx\n", arch->load(state, a, 4, &exc));
    }

    state_destroy(state);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
