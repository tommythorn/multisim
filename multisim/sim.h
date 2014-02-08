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
#include <stdbool.h>
#include <assert.h>
#include "memory.h"
#include "arch.h"

/* All the architectual state */
struct cpu_state_st {
    memory_t       *mem;

    uint64_t        pc;

    /* CPU Registers */
    uint64_t        r[ISA_REGISTERS];

    /* All other resources should be cover here */
    uint64_t        msr[ISA_MSRS];

    /* Statistics */
    uint64_t        n_issue;

    bool            fatal_error;
};

typedef enum verbosity_e {
    VERBOSE_DISASS = 1 << 0,
    VERBOSE_TRACE  = 1 << 1,
} verbosity_t;

#define fatal(msg...) ({fprintf(stderr, msg); exit(1);})
#define warn(msg...)  fprintf(stderr, msg)

void exception(char *kind);
void run_simple(int, char **, verbosity_t);
void run_sscalar_io(int, char **, verbosity_t);
void run_sscalar_oooe(int, char **, verbosity_t);

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

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
