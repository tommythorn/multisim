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
 * Generic Instruction Set Architecture
 */

#ifndef _ISA_H
#define _ISA_H 1

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>
#include "sim.h"
#include "loadelf.h"

/*
 * Instructions are classified into alu, load, store, jump, branch,
 * and computed jump.
 *
 * The distinction between the latter three is from the point
 * of view of the instruction fetch:
 *
 * - for jumps, fetching can proceed uninterrupted at targetpc,
 *
 * - for branches, fetching can speculatively proceed at targetpc
 *   based on branch prediction, and
 *
 * - for computed jumps, actual target is register source_reg_a.
 */
typedef enum isa_inst_class_e {
    isa_inst_class_alu,
    isa_inst_class_load,
    isa_inst_class_store,
    isa_inst_class_jump,
    isa_inst_class_branch,
    isa_inst_class_compjump,
} isa_inst_class_t;

/*
 * Decode enough of the instruction to guide the dynamic scheduling.
 *
 * Loads encode the access size and signed as follows: unsigned byte
 * loads: 1, signed byte loads: -1, signed half word: -2, unsigned
 * 64-bit: -8, etc. Store work similarly, except negative values are
 * not meaningful and are thus not allowed.
 */
typedef struct isa_decoded_st {
    uint64_t            inst_addr;
    uint32_t            inst;
    int                 dest_reg, source_reg_a, source_reg_b;
    isa_inst_class_t    class;
    union {
        int                 loadstore_size;
        uint64_t            jumpbranch_target;
        int                 compjump_reg;
    };
} isa_decoded_t;

typedef struct isa_result_st {
    /*
     * Results, AKA the write-back value, from ALU instructions,
     * etc.  Includes fx. the return address for a call instruction.
     */
    uint64_t    result;

    union {
        /*
         * Loads and stores are handled outside the execution stage, but
         * result holds the memory address.  In the case of stores we need
         * an extra value which is communited with store_value.
         */
        uint64_t    store_value;
        bool        branch_taken;
    };

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
extern const isa_t lm32_isa;

static inline const isa_t *
get_isa(uint16_t machine)
{
    if (machine == EM_ALPHA)
        return &alpha_isa;
    if (machine == EM_LM32)
        return &lm32_isa;
    return NULL;
}

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
