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

typedef struct isa_st {
    void (*setup)(cpu_state_t *, elf_info_t *);

    void (*decode)(uint32_t inst,
                   int *dest_reg, int *source_reg_a, int *source_reg_b,
                   bool *b_is_imm, uint64_t *imm,
                   bool *is_load, bool *is_store, bool *is_branch);

    uint64_t (*inst_exec)(uint32_t instruction, uint64_t op_a, uint64_t op_b,
                          uint64_t *storev, uint64_t *storemask, uint64_t *pc,
                          bool *fatal);

    uint64_t (*inst_loadalign)(uint32_t instruction, uint64_t address, uint64_t result);

    void (*disass)(uint64_t pc, uint32_t inst);
} isa_t;

extern const isa_t alpha_isa;

#endif
