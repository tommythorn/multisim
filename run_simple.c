/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2012,2018,2020 Tommy Thorn
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
 * Step_simple returns the number of instructions executed
 */
int
step_simple(cpu_state_t *state, cpu_state_t *cosimstate)
{
    const arch_t *arch = state->arch;
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

    uint64_t atomic_addr = CANONICALIZE(state, op_a);

    if (!cosimstate)
        arch->get_interrupt_exception(state, &exc);

    if (exc.raised)
        goto exception;

    if (dec.class == isa_insn_class_atomic)
        op_a = arch->load(state, atomic_addr, dec.loadstore_size, &exc);

    if (exc.raised)
        goto exception;

    isa_result_t res;

    if (!dec.system)
        res = arch->insn_exec(dec, op_a, op_b, msr_a, &exc);
    else
        res = arch->insn_exec_system(state, dec, op_a, op_b, msr_a, &exc);
    res.result = CANONICALIZE(state, res.result);

    if (exc.raised)
        goto exception;

    switch (dec.class) {
    case isa_insn_class_load:
        res.load_addr = CANONICALIZE(state, res.load_addr);
        res.result = arch->load(state, res.load_addr, dec.loadstore_size, &exc);
        res.result = CANONICALIZE(state, res.result);

        if (exc.raised)
            goto exception;

        state->pc += dec.insn_len;
        break;

    case isa_insn_class_store:
        res.store_addr = CANONICALIZE(state, res.store_addr);
        res.store_value = CANONICALIZE(state, res.store_value);
        arch->store(state, res.store_addr, res.store_value, dec.loadstore_size, &exc);

        if (exc.raised)
            goto exception;

        state->pc += dec.insn_len;
        break;

    case isa_insn_class_atomic:
        arch->store(state, atomic_addr, res.result, abs(dec.loadstore_size), &exc);

        if (exc.raised)
            goto exception;

        res.result = op_a;
        state->pc += dec.insn_len;
        break;


    case isa_insn_class_branch:
        dec.jumpbranch_target = CANONICALIZE(state, dec.jumpbranch_target);
        state->pc = res.branch_taken ? dec.jumpbranch_target : state->pc + dec.insn_len;
        break;

    case isa_insn_class_jump:
        dec.jumpbranch_target = CANONICALIZE(state, dec.jumpbranch_target);
        state->pc = dec.jumpbranch_target;
        break;

    case isa_insn_class_compjump:
        res.compjump_target = CANONICALIZE(state, res.compjump_target);
        state->pc = res.compjump_target;
        break;

    default:
        state->pc += dec.insn_len;
        state->pc = CANONICALIZE(state, state->pc);
        break;
    }

    if (exc.raised)
        goto exception;

    if (dec.dest_reg != ISA_NO_REG)
        state->r[dec.dest_reg] = res.result;

    if (dec.dest_msr != ISA_NO_REG)
        arch->write_msr(state, dec.dest_msr, res.msr_result, &exc);

exception:
    if (state->verbosity & VERBOSE_DISASS) {
        fprintf(stderr, "%d ", state->priv);
        isa_disass(stderr, arch, dec, res);
    }

    if (exc.raised) {
        if (state->verbosity & VERBOSE_DISASS)
            fprintf(stderr,
                    "                  EXCEPTION %"PRId64" (%08"PRId64") RAISED\n",
                    exc.code, exc.info);

        state->pc = arch->handle_exception(state, dec.insn_addr, exc);
    }

    arch->tick(state, 1, cosimstate);

    return exc.raised ? 0 : 1;
}

bool simple_htif(cpu_state_t *state)
{
    const arch_t *arch = state->arch;
    verbosity_t verbosity = state->verbosity;
    uint64_t tohost = state->tohost;
    uint64_t fromhost = state->fromhost;

    if (!tohost || !fromhost)
        return false;

    if (tohost && verbosity & VERBOSE_TOHOST)  {
        isa_exception_t exc = { 0 };
        uint32_t val = arch->load(state, tohost, 4, &exc);
        assert(exc.raised == false);

        if (val == 0)
            return false;

        arch->store(state, tohost, 0, 4, &exc);
        if (exc.raised)
            return false;

        if (val & 1) {
            if (val >> 1)
                fprintf(stderr, "  FAILED with %08x\n", val);
            else
                fprintf(stderr, "  SUCCESS\n");
            return true;
        }

        /* Val is an address of a command block */
        volatile uint64_t magic_memval[4];
        magic_memval[0] = arch->load(state, val,      8, &exc);
        if (exc.raised)
            return false;
        magic_memval[1] = arch->load(state, val +  8, 8, &exc);
        if (exc.raised)
            return false;
        magic_memval[2] = arch->load(state, val + 16, 8, &exc);
        if (exc.raised)
            return false;
        magic_memval[3] = arch->load(state, val + 24, 8, &exc);
        if (exc.raised)
            return false;

#define SYS_write 64
        if (magic_memval[0] == SYS_write) {
            // uint64_t fd = magic_memval[1];
            uint64_t s  = magic_memval[2];
            uint64_t n  = magic_memval[3];

            while (n > 0) {
                char ch = arch->load(state, s, 1, &exc);
                if (exc.raised)
                    break;

                fputc(ch, stderr);
                --n;
                ++s;
            }

            magic_memval[0] = magic_memval[3];
        }

        arch->store(state, fromhost, 1, 4, &exc);
    }

    return false;
}

void run_simple(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create(num_images, images, verbosity);

    for (;;) {
        if (!step_simple(state, NULL))
            continue;

        if (simple_htif(state))
            break;
    }

    if (verbosity & VERBOSE_DISASS)
        printf("IPC = %.2f\n", (double) state->n_issue / state->counter);

    if (verbosity & VERBOSE_COMPLIANCE) {
        isa_exception_t exc = { 0 };
        uint64_t begin_signature;
        uint64_t end_signature;

        if (getelfsym(&state->info, "begin_signature", &begin_signature) &&
            getelfsym(&state->info, "end_signature", &end_signature))
            for (uint32_t a = begin_signature; a < end_signature; a += 4)
                printf("%08"PRId64"\n", state->arch->load(state, a, 4, &exc));
    }

    state_destroy(state);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
