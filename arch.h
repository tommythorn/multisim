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

/*
 * XXX We separate instructions into system (serialized) and
 * non-system.  We really shouldn't talk about MSRs at the
 * architecture level but simply treat them as part of the serializing
 * instructions and keep all details of them internal.  Much simpler.
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
typedef enum isa_insn_class_e {
    isa_insn_class_illegal,
    isa_insn_class_alu,
    isa_insn_class_load,
    isa_insn_class_store,
    isa_insn_class_jump,
    isa_insn_class_branch,
    isa_insn_class_compjump,
    isa_insn_class_atomic,
} isa_insn_class_t;

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
    uint64_t            insn_addr;
    uint32_t            insn;
    int                 insn_len; // typically 4 or 2
    isa_reg_t           dest_reg, source_reg_a, source_reg_b;
    isa_msr_t           dest_msr, source_msr_a;
    isa_insn_class_t    class;
    bool                system; // system instruction are handled differently
    int64_t             imm;    // generalized optional immediate
    union {
        int                 loadstore_size;
        uint64_t            jumpbranch_target;
    };
} isa_decoded_t;

typedef struct isa_exception_st {
    bool        raised;
    uint64_t    code;
    uint64_t    info;
} isa_exception_t;

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
} isa_result_t;

typedef struct cpu_state_st cpu_state_t;

typedef enum memory_exception_e {
    MEMORY_SUCCESS      = 0,    // normal
    MEMORY_EXCEPTION    = 1,    // load/store updated the state
    MEMORY_FATAL        = 2,    // cannot proceed simulation
} memory_exception_t;

typedef enum verbosity_e {
    VERBOSE_CONSOLE    = 1 << 0,
    VERBOSE_DISASS     = 1 << 1, // Trace with disassembly
    VERBOSE_PIPE       = 1 << 2, // Pipe trace
    VERBOSE_CACHE      = 1 << 3,
    VERBOSE_COMPLIANCE = 1 << 4,
    VERBOSE_TOHOST     = 1 << 5,
    VERBOSE_NO_COSIM   = 1 << 6, // XXX Rename "verbose" to "RT config" or such
} verbosity_t;

typedef struct isa_st {
    /*
     * Many RISC architectures have a special register which reads as
     * zero and ignores all writes.  If the ISA doesn't have such, use
     * ISA_NO_REG.
     */
    const isa_reg_t zero_reg;

    const char **reg_name;

    const bool is_64bit;

    void (*setup)(cpu_state_t *);

    isa_decoded_t (*decode)(uint64_t insn_addr, uint32_t insn);

    isa_result_t (*insn_exec)(isa_decoded_t dec, uint64_t op_a, uint64_t op_b,
                              uint64_t msr_a, isa_exception_t *exc);

    /* System instructions are serializing and must be executed by
     * this, however they can affect system state.  */
    isa_result_t (*insn_exec_system)(cpu_state_t *state,
                                     isa_decoded_t dec, uint64_t op_a, uint64_t op_b,
                                     uint64_t msr_a, isa_exception_t *exc);

    void (*disass_insn)(uint64_t pc, uint32_t insn, char *buf, size_t buf_size);

    /*
     * tick() advances the state of any devices that run
     * asynchronously with the CPU.  Provide it with the number of
     * instruction retired this cycle.  If cosimstate is non-null, it
     * overrides the asynchronous part of the state (which can't be
     * cosimulated).
     */
    void (*tick)(cpu_state_t *state, int instret, cpu_state_t *cosimstate);

    /*
     * read_msr() is the MSR equivalent of the register read.  However
     * MSR reads can have side effects, thus the ISA function.
     */
    uint64_t (*read_msr)(cpu_state_t *state, unsigned msr, isa_exception_t *exc);

    /*
     * write_msr() is the MSR equivalent of the register writeback.
     * However MSR writes can have side effects, thus the ISA
     * function.
     */
    void (*write_msr)(cpu_state_t *state, unsigned msr, uint64_t value, isa_exception_t *exc);

    /*
     * Memory access - is responsible for dealing with any MMU, IO,
     * etc mapping and semantic.
     */
    uint64_t (*load)(cpu_state_t *, uint64_t address, int mem_access_size, isa_exception_t *exc);
    void (*store)(cpu_state_t *, uint64_t address, uint64_t value, int mem_access_size, isa_exception_t *exc);

    bool (*get_interrupt_exception)(cpu_state_t *state, isa_exception_t *exc);

    uint64_t (*handle_exception)(cpu_state_t *state, uint64_t insn_addr, isa_exception_t exc);
} arch_t;

const arch_t *get_arch(uint16_t machine, bool is_64bit);

void isa_disass(FILE *out, const arch_t *, isa_decoded_t, isa_result_t);

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
