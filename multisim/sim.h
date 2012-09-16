/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2012 Tommy Thorn
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
#include "memory.h"

/* "no register" - a generalization of MIPS's r0 and Alpha's r31. */
#define NO_REG 32

/* All the architectual state */
typedef struct cpu_state_st {
    memory_t       *mem;

    uint64_t        pc;

    /* CPU Registers */
    uint64_t        r[NO_REG + 1];

    /* All other resources should be cover here */
    uint64_t        msr[64];

    /* Statistics */
    uint64_t        n_issue;
} cpu_state_t;

#define fatal(msg...) ({printf(msg); exit(1);})
#define warn(msg...)  printf(msg)

void exception(char *kind);
void run_simple(int, char **);
void run_sscalar_io(int, char **);
void run_sscalar_oooe(int, char **);
void run_sscalar_oooe_spec(int, char **);

static inline cpu_state_t *
state_create(void)
{
    cpu_state_t *state = calloc(1, sizeof *state);
    state->mem = memory_create();

    return state;
}

static inline void
state_destroy(cpu_state_t *state)
{
    memory_destroy(state->mem);
    free(state);
}

static inline uint32_t
load32(memory_t *m, uint64_t address)
{
    // assert((uint32_t)address == address);
    return *(uint32_t *)memory_physical(m, (uint32_t)address, sizeof(uint32_t));
}

static inline uint64_t
load64(memory_t *m, uint64_t address)
{
    // assert((uint32_t)address == address);
    return *(uint64_t *)memory_physical(m, (uint32_t)address, sizeof(uint64_t));
}

static inline void
store64(memory_t *m, uint64_t address, uint64_t v)
{
    // assert((uint32_t)address == address);
    *(uint64_t *)memory_physical(m, (uint32_t)address, sizeof(uint64_t)) = v;
}

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
