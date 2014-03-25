/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2014 Tommy Thorn
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
 *
 * Riscv opcode map, really just the RV32I part
 *
 */

#ifndef _RISCV_OPCODE_H
#define _RISCV_OPCODE_H 1

#include <inttypes.h>
#include <stdbool.h>
#include "memory.h"

typedef unsigned opcode_t;
/*
 * Note, this structure depends on bitfields being packed from least
 * significant bits and upwards.
 */
typedef union riscv_instruction {
    struct {
        opcode_t   opext    :  2;
        opcode_t   opcode   :  5;
        unsigned   rd       :  5;
        unsigned   funct3   :  3;
        unsigned   rs1      :  5;
        unsigned   rs2      :  5;
        unsigned   funct7   :  7;
    } r;
    struct {
        opcode_t   opext    :  2;
        opcode_t   opcode   :  5;
        unsigned   rd       :  5;
        unsigned   funct3   :  3;
        unsigned   rs1      :  5;
        int        imm11_0  : 12;
    } i;
    struct {
        opcode_t   opext    :  2;
        opcode_t   opcode   :  5;
        unsigned   imm4_0   :  5;
        unsigned   funct3   :  3;
        unsigned   rs1      :  5;
        unsigned   rs2      :  5;
        int        imm11_5  :  7;
    } s;
    struct {
        opcode_t   opext    :  2;
        opcode_t   opcode   :  5;
        unsigned   imm11    :  1;
        unsigned   imm4_1   :  4;
        unsigned   funct3   :  3;
        unsigned   rs1      :  5;
        unsigned   rs2      :  5;
        unsigned   imm10_5  :  6;
        int        imm12    :  1;
    } sb;
    struct {
        opcode_t   opext    :  2;
        opcode_t   opcode   :  5;
        unsigned   rd       :  5;
        int        imm31_12 : 20;
    } u;
    struct {
        opcode_t   opext    :  2;
        opcode_t   opcode   :  5;
        unsigned   rd       :  5;
        unsigned   imm19_12 :  8;
        unsigned   imm11    :  1;
        unsigned   imm10_1  : 10;
        int        imm20    :  1;
    } uj;
    u_int32_t raw;
} inst_t;

typedef enum riscv_opcode_e {
    LOAD,   LOAD_FP,  CUSTOM0, MISC_MEM, OP_IMM, AUIPC, OP_IMM_32, EXT0,
    STORE,  STORE_FP, CUSTOM1, AMO,      OP,     LUI,   OP_32,     EXT1,
    MADD,   MSUB,     NMSUB,   NMADD,    OP_FP,  RES1,  CUSTOM2,   EXT2,
    BRANCH, JALR,     RES0,    JAL,      SYSTEM, RES2,  CUSTOM3,   EXT3,
} riscv_opcode_t;

enum riscv_opcode_load_e {
    LB,  LH,  LW,  LD,
    LBU, LHU, LWU, UIMP_LOAD7,
};

enum riscv_opcode_op_imm_e {
    ADDI, SLLI, SLTI, SLTIU, XORI, SR_I, ORI, ANDI,
};

enum riscv_opcode_op_e {
    ADDSUB, SLL, SLT, SLTU, XOR, SR_, OR, AND,
};

enum riscv_opcode_op_branch_e {
    BEQ, BNE, UIMP_BR2, UIMP_BR3, BLT, BGE, BLTU, BGEU,
};

enum riscv_opcode_op_system_e {
    SCALLSBREAK, CSRRW, CSRRS, CSRRC, TBD, CSRRWI, CSRRSI, CSRRCI,
};

enum riscv_opcode_op_div_e {
    MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU,
};

/* CSRs */

enum riscv_csr_e {
    CSR_CYCLE = 0xC00, CSR_TIME, CSR_INSTRET,
};

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
