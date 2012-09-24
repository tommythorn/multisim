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

    /*
     * Call, returns, jumps, branches, computed jumps, conditional
     * branches, etc.  All control transfers are considered
     * "branches".  Those that are unconditional further sets
     * is_unconditional.
     */
    bool        is_branch;
    bool        is_unconditional;

    /*
     * loads encode the access size and signed as follows: unsigned
     * byte loads: 1, signed byte loads: -1, signed half word: -2,
     * unsigned 64-bit: -8, etc. Store work similarly, except negative
     * values are not meaningful and are thus not allowed.
     */
    bool        is_load, is_store;;
    int         mem_access_size;
} isa_decoded_t;

typedef struct isa_result_st {
    /*
     * ALU results etc. The write-back value. Includes fx. the return
     * address for a call instruction.
     */
    uint64_t    result;

    /*
     * Loads and stores are handled outside the execution stage, but
     * result holds the memory address.  In the case of stores we need
     * an extra value which is communited with store_value.
     */
    uint64_t    store_value;

    /*
     * Control transfers (calls, returns, jumps, branches, computed
     * jumps, conditional branches, etc) needs to communicate the
     * target address.  Conditional branches (and only those) further
     * communicates if the branch was taken (if not, the target
     * address should be ignored).
     */
    bool        branch_taken;
    uint64_t    branch_target;

    /*
     * Stop simulation on fatal errors.
     */
    bool        fatal_error;
} isa_result_t;

typedef struct isa_st {
    void (*setup)(cpu_state_t *, elf_info_t *);

    isa_decoded_t (*decode)(uint64_t inst_addr, uint32_t inst);

    isa_result_t (*inst_exec)(isa_decoded_t dec, uint64_t op_a, uint64_t op_b);

    void (*disass)(uint64_t inst_addr, uint32_t inst);
} isa_t;

extern const isa_t alpha_isa;

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
