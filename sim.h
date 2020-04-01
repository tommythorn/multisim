/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2012,2018,2019 Tommy Thorn
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

#ifndef _SIM_H_
#define _SIM_H_ 1

#include <sys/time.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "memory.h"
#include "arch.h"

/* All the common architectual state */
struct cpu_state_st {
    verbosity_t     verbosity;

    const arch_t   *arch;
    uint64_t        tohost, fromhost;
    elf_info_t      info;

    memory_t       *mem;

    uint64_t        pc;

    /* CPU Registers */
    uint64_t        r[ISA_REGISTERS];

    /* All other resources should be cover here */
    uint64_t        msr[ISA_MSRS];

    /* Counter running at the CPU tick rate */
    uint64_t        counter;

    /* memory mapped mtime and mtimecmp */
    uint64_t        mtimereg[2];

    /* Statistics */
    uint64_t        n_issue;

    bool            fatal_error;
    int             priv;   // Priviledge level
    void           *arch_specific; // XXX Ok, C++ would have made this cleaner
};

#define fatal(msg...) ({fprintf(stderr, msg); exit(1);})
#define warn(msg...)  fprintf(stderr, msg)

void exception(char *kind);
void run_simple(int, char **, verbosity_t);
void run_ooo(int, char **, verbosity_t);

static inline cpu_state_t *
state_create(int num_images, char *images[], verbosity_t verbosity)
{
    cpu_state_t *state = calloc(1, sizeof *state);
    state->mem = memory_create();
    state->verbosity = verbosity;

    memory_ensure_mapped_range(state->mem, 0x80000000, 0x80000000 + 256*1024-1);

    loadelfs(state->mem, num_images, images, &state->info);

    state->arch = get_arch(state->info.machine, state->info.is_64bit);
    state->arch->setup(state);

    return state;
}

static inline void
state_destroy(cpu_state_t *state)
{
    memory_destroy(state->mem);
    free(state);
}

bool simple_htif(cpu_state_t *state);

/* Enforce that 32-bit architectures keep the register file in a
   canonical form, that is int32_t sign extended to int64_t/uint64_t */
#define CANONICALIZE(state, v) ((state)->arch->is_64bit ? (v) : (int32_t) (v))

// XXX hackish
char *disk_image;
bool CONFIG_EARLY_RELEASE;

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
