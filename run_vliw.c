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
#include "run_vliw.h"
#include "loadelf.h"

static bool inline
is_nop(isa_decoded_t dec)
{
    return dec.class == isa_insn_class_alu && dec.dest_reg == ISA_NO_REG;
}

static uint8_t slot_class_mask[8] = {
    1 << isa_insn_class_alu,
    1 << isa_insn_class_alu,

    1 << isa_insn_class_load,
    1 << isa_insn_class_load,

    1 << isa_insn_class_store,
    1 << isa_insn_class_store,

    1 << isa_insn_class_alu | 1 << isa_insn_class_branch,

    1 << isa_insn_class_alu | 1 << isa_insn_class_branch | 1 << isa_insn_class_jump | 1 << isa_insn_class_compjump,
};

bool
step_vliw(const arch_t *arch, cpu_state_t *state)
{
    isa_exception_t exc = { 0 };
    uint64_t pc = state->pc;
    uint32_t insn[8];

    if (pc & 31) {
        printf("FATAL: pc %08" PRIx64 " insn't aligned to 32-bytes\n", pc);
        exit(1);
    }

    for (int i = 0; i < 8; ++i) {
        insn[i] = (uint32_t)arch->load(state, pc + 4 * i, 0 /* = ifetch */, &exc);
        assert(!exc.raised);
    }

    isa_decoded_t dec[8];
    for (int i = 0; i < 8; ++i) {
        dec[i] = arch->decode(pc + i * 4, insn[i]); // XXX This implies that jal can only be in the final slot

        // Decoding sanity constraints
        assert(dec[i].source_reg_a == ISA_NO_REG || dec[i].source_reg_a < ISA_REGISTERS);
        assert(dec[i].source_reg_b == ISA_NO_REG || dec[i].source_reg_b < ISA_REGISTERS);
        assert(dec[i].dest_reg     == ISA_NO_REG || dec[i].dest_reg     < ISA_REGISTERS);
        assert(dec[i].dest_msr     == ISA_NO_REG || dec[i].dest_msr     < ISA_MSRS);
        assert(dec[i].source_msr_a == ISA_NO_REG || dec[i].source_msr_a < ISA_MSRS);
    }

    // VLIW Slot constraints
    for (int i = 0; i < 8; ++i)
        if (!is_nop(dec[i]) && (1 << dec[i].class) & ~slot_class_mask[i]) {
            printf("%08" PRIx64 ": slot %d can't have an instruction of class %s\n",
                   state->pc + 4 * i, i, class_names[dec[i].class]);
            exit(1);
        }

    assert(is_nop(dec[0]) || dec[0].class == isa_insn_class_alu);
    assert(is_nop(dec[1]) || dec[1].class == isa_insn_class_alu);

    assert(is_nop(dec[2]) || dec[2].class == isa_insn_class_load);
    assert(is_nop(dec[3]) || dec[3].class == isa_insn_class_load);

    assert(is_nop(dec[4]) || dec[4].class == isa_insn_class_store);
    assert(is_nop(dec[5]) || dec[5].class == isa_insn_class_store);

    assert(is_nop(dec[6]) ||
           dec[6].class == isa_insn_class_alu ||
           dec[6].class == isa_insn_class_branch);

    assert(is_nop(dec[7])                        ||
           dec[7].class == isa_insn_class_alu    ||
           dec[7].class == isa_insn_class_branch ||
           dec[7].class == isa_insn_class_jump   ||
           dec[7].class == isa_insn_class_compjump);

    // Dependency contraints (note, enforcing sequential is stricter than neccessary)
    uint32_t def = 0;
    for (int i = 0; i < 8; ++i) {
        uint32_t use1 = dec[i].source_reg_a != ISA_NO_REG ? 1 << dec[i].source_reg_a : 0;
        if (def & use1) {
            printf("%08" PRIx64 ": slot %d uses x%d which is defined in the molecule\n",
                   state->pc + 4 * i, i, dec[i].source_reg_a);
            exit(1);
        }

        uint32_t use2 = dec[i].source_reg_b != ISA_NO_REG ? 1 << dec[i].source_reg_b : 0;
        if (def & use2) {
            printf("%08" PRIx64 ": slot %d uses x%d which is defined in the molecule\n",
                   state->pc + 4 * i, i, dec[i].source_reg_b);
            exit(1);
        }

        uint32_t def1 = dec[i].dest_reg != ISA_NO_REG ? 1 << dec[i].dest_reg : 0;
        if (def & def1) {
            printf("%08" PRIx64 ": slot %d defines x%d which is already defined in the molecule\n",
                   state->pc + 4 * i, i, dec[i].dest_reg);
            exit(1);
        }

        def |= def1;
    }

    uint64_t op_a[8], op_b[8];

    // RF
    for (int i = 0; i < 8; ++i) {
        op_a[i] = state->r[dec[i].source_reg_a];
        op_b[i] = state->r[dec[i].source_reg_b];
    }

    isa_result_t res[8];

    for (int i = 0; i < 8; ++i) {
        assert(!dec[i].system);

        res[i] = arch->insn_exec(dec[i], op_a[i], op_b[i], 0, &exc);
        res[i].result = CANONICALIZE(res[i].result);

        assert(!exc.raised); // XXX handle eventually
    }


    for (int i = 2; i < 6; ++i) {
        if (dec[i].class == isa_insn_class_load) {
            res[i].load_addr = CANONICALIZE(res[i].load_addr);
            res[i].result = arch->load(state, res[i].load_addr, dec[i].loadstore_size, &exc);
            res[i].result = CANONICALIZE(res[i].result);
            assert(!exc.raised); // XXX handle eventually
        } else if (dec[i].class == isa_insn_class_store) {
            res[i].store_addr = CANONICALIZE(res[i].store_addr);
            res[i].store_value = CANONICALIZE(res[i].store_value);
            arch->store(state, res[i].store_addr, res[i].store_value, dec[i].loadstore_size, &exc);
        }

        assert(!exc.raised); // XXX handle eventually
    }

    state->pc += 32;
    if (dec[6].class == isa_insn_class_branch && res[6].branch_taken) {
        state->pc = dec[6].jumpbranch_target;
        // XXX Hacky way to annull the last slot
        dec[7].class = isa_insn_class_alu;
        dec[7].dest_reg = ISA_NO_REG;
    }

    if (dec[7].class == isa_insn_class_branch && res[7].branch_taken)
        state->pc = dec[7].jumpbranch_target;
    else if (dec[7].class == isa_insn_class_jump)
        state->pc = dec[7].jumpbranch_target;
    else if (dec[7].class == isa_insn_class_compjump) {
        res[7].compjump_target = CANONICALIZE(res[7].compjump_target);
        state->pc = res[7].compjump_target;
    }

    for (int i = 0; i < 8; ++i)
        if (dec[i].dest_reg != ISA_NO_REG)
            state->r[dec[i].dest_reg] = res[i].result;

    if (state->verbosity & VERBOSE_DISASS) {
        //fprintf(stderr, "\n%d ", state->priv);
        fprintf(stderr, "\n");
        for (int i = 0; i < 8; ++i)
            if (!is_nop(dec[i]))
                isa_disass(arch, dec[i], res[i]);
    }

    arch->tick(state, 1);

    return false;
}

void run_vliw(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = state_create();
    const arch_t *arch;
    elf_info_t info;

    memory_ensure_mapped_range(state->mem,
                               0x80000000, 0x80000000 + 32*1024-1);
    memory_ensure_mapped_range(state->mem,
                               0x10000, 0x10000 + 32*1024-1);

    loadelfs(state->mem, num_images, images, &info);

    arch = get_arch(info.machine, info.is_64bit);
    arch->setup(state, &info, verbosity);

    uint64_t tohost = 0;
    isa_exception_t exc = { 0 };
    getelfsym(&info, "tohost", &tohost);

    for (;;) {
        if (step_vliw(arch, state))
            break;

        if (verbosity & VERBOSE_COMPLIANCE)  {
            uint32_t val = arch->load(state, tohost, 4, &exc);
            if (val) {
                if (val != 1)
                    fprintf(stderr, "  FAILED with %d\n", val);
                break;
            }
        }
    }

    if (verbosity & VERBOSE_DISASS)
        printf("IPC = %.2f\n", (double) state->n_issue / state->counter);

    if (verbosity & VERBOSE_COMPLIANCE) {
        uint64_t begin_signature;
        uint64_t end_signature;

        if (getelfsym(&info, "begin_signature", &begin_signature) &&
            getelfsym(&info, "end_signature", &end_signature))
            for (uint32_t a = begin_signature; a < end_signature; a += 4)
                printf("%08"PRId64"\n", arch->load(state, a, 4, &exc));
    }

    state_destroy(state);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
