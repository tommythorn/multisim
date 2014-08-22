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
#include <inttypes.h>
#include <stdbool.h>
#include <sys/types.h>
#include "loadelf.h"

/*
 * The major objective of multisim is to fully expose the data flow in
 * the ISA, thus all* architectural CPU state outside the PC is
 * assumed to be held in registers, general purpose registers and
 * machine registers for everything else (we may expand this to
 * include more register classes as needed).
 *
 * When dest_reg is ISA_NO_REG then the result should be ignored.
 * Source registers must never be ISA_NO_REG.
 *
 * Similar for machine registers, but here sources should be marked
 * NO_REG when not used.
 *
 * (By making NO_REG an index within the range of REGISTERS, we can
 * safely use it to an index and can avoid checking for it in many
 * places).
 */

#define ISA_REGISTERS 64
#define ISA_MSRS      (1 << 12)
#define ISA_NO_REG    (-1)

typedef int8_t isa_reg_t;
typedef int isa_msr_t;

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
    isa_inst_class_atomic,
} isa_inst_class_t;

/*
 * Decode enough of the instruction to guide the dynamic scheduling.
 *
 * Loads encode the access size and signed as follows: unsigned byte
 * loads: 1, signed byte loads: -1, signed half word: -2, unsigned
 * 64-bit: -8, etc. Store work similarly, except negative values are
 * not meaningful and are thus not allowed.
 *
 * Machine Specific Registers are modeled as just another register
 * file, though it's expected that implementations will simply
 * serialize on such instructions.
 */
typedef struct isa_decoded_st {
    uint64_t            inst_addr;
    uint32_t            inst;
    isa_reg_t           dest_reg, source_reg_a, source_reg_b;
    isa_msr_t           dest_msr, source_msr_a;
    isa_inst_class_t    class;
    union {
        int                 loadstore_size;
        uint64_t            jumpbranch_target;
    };
} isa_decoded_t;

typedef struct isa_result_st {
    /*
     * Results, AKA the write-back value, from ALU instructions,
     * etc.  Includes fx. the return address for a call instruction.
     */
    uint64_t    result;
    uint64_t    load_addr;
    uint64_t    store_value;
    uint64_t    store_addr;

    union {
        /*
         * Loads and stores are handled outside the execution stage, but
         * result holds the memory address.  In the case of stores we need
         * an extra value which is communited with store_value.
         */
        bool        branch_taken;
        uint64_t    compjump_target;
    };

    uint64_t    msr_result;

    /*
     * Stop simulation on fatal errors.
     */
    bool        fatal_error;
} isa_result_t;

typedef struct cpu_state_st cpu_state_t;

typedef enum memory_exception_e {
    MEMORY_SUCCESS	= 0,	// normal
    MEMORY_EXCEPTION	= 1,	// load/store updated the state
    MEMORY_FATAL	= 2,	// cannot proceed simulation
} memory_exception_t;

typedef struct isa_st {
    /*
     * Many RISC architectures have a special register which reads as
     * zero and ignores all writes.  If the ISA doesn't have such, use
     * ISA_NO_REG.
     */
    const isa_reg_t zero_reg;

    const char **reg_name;

    const bool is_64bit;

    void (*setup)(cpu_state_t *, elf_info_t *);

    isa_decoded_t (*decode)(uint64_t inst_addr, uint32_t inst);

    isa_result_t (*inst_exec)(isa_decoded_t dec, uint64_t op_a, uint64_t op_b,
                              uint64_t msr_a);

    void (*disass_inst)(uint64_t pc, uint32_t inst, char *buf, size_t buf_size);

    /*
     * tick() advances the state of any devices that run
     * asynchronously with the CPU.
     */
    void (*tick)(cpu_state_t *state);

    /*
     * read_msr() is the MSR equivalent of the register read.  However
     * MSR reads can have side effects, thus the ISA function.
     */
    uint64_t (*read_msr)(cpu_state_t *state, unsigned msr);

    /*
     * write_msr() is the MSR equivalent of the register writeback.
     * However MSR writes can have side effects, thus the ISA
     * function.
     */
    void (*write_msr)(cpu_state_t *state, unsigned msr, uint64_t value);

    /*
     * Memory access - is responsible for dealing with any MMU, IO,
     * etc mapping and semantic.
     */
    uint64_t (*load)(cpu_state_t *, uint64_t address, int mem_access_size, memory_exception_t *e);
    void (*store)(cpu_state_t *, uint64_t address, uint64_t value, int mem_access_size, memory_exception_t *e);
} arch_t;

const arch_t *get_arch(uint16_t machine, bool is_64bit);

void isa_disass(const arch_t *, isa_decoded_t, isa_result_t);

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
