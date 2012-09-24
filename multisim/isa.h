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

/*
 * Copyright (C) 2012 Tommy Thorn
 *
 * generic Instruction Set Architecture
 */

#ifndef _ISA_H
#define _ISA_H 1

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>
#include "sim.h"
#include "loadelf.h"

typedef struct isa_decoded_st {
    uint64_t    inst_addr;
    uint32_t    inst;
    int         dest_reg, source_reg_a, source_reg_b;
    bool        b_is_imm;
    uint64_t    imm;
    bool        is_load, is_store, is_branch;
    int         access_size;
} isa_decoded_t;

typedef struct isa_result_st {
    uint64_t    result;
    uint64_t    storev;
    uint64_t    pc;
    bool        fatal_error;
} isa_result_t;

typedef struct isa_st {
    void (*setup)(cpu_state_t *, elf_info_t *);

    isa_decoded_t (*decode)(uint64_t inst_addr, uint32_t inst);

    isa_result_t (*inst_exec)(isa_decoded_t dec, uint64_t op_a, uint64_t op_b);

    uint64_t (*inst_loadalign)(isa_decoded_t dec, uint64_t address, uint64_t result);

    void (*disass)(uint64_t inst_addr, uint32_t inst);
} isa_t;

extern const isa_t alpha_isa;

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
