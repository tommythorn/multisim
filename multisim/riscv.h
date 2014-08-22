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

enum riscv_opcode_load_fp_e {
    FLW = 2, FLD = 3,
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

enum riscv_opcode_op_f_64 {
    FMV_D_X = 0x39,
};

enum riscv_opcode_amo_e {
    AMOADD      = 0,
    AMOSWAP     = 1,
    LR          = 2,
    SC          = 3,
    AMOXOR      = 4,

    AMOOR       = 8,
    AMOAND      =12,
    AMOMIN      =16,
    AMOMAX      =20,
    AMOMINU     =24,
    AMOMAXU     =28,
};

/* CSRs */

enum riscv_csr_e {
    CSR_FFLAGS		= 0x001,// fcsr[4:0] alias
    CSR_FRM,			// fcsr[7:5] alias
    CSR_FCSR,

    CSR_SUP0		= 0x500,
    CSR_SUP1		= 0x501,
    CSR_EPC		= 0x502,
    CSR_BADVADDR	= 0x503,
    CSR_PTBR		= 0x504,
    CSR_ASID		= 0x505,
    CSR_COUNT		= 0x506,
    CSR_COMPARE		= 0x507,
    CSR_EVEC		= 0x508,
    CSR_CAUSE		= 0x509,
    CSR_STATUS		= 0x50a,
    CSR_HARTID		= 0x50b,
    CSR_IMPL		= 0x50c,
    CSR_FATC		= 0x50d,
    CSR_SEND_IPI	= 0x50e,
    CSR_CLEAR_IPI	= 0x50f,
    CSR_TOHOST		= 0x51e,
    CSR_FROMHOST	= 0x51f,

    CSR_CYCLE		= 0xC00,
    CSR_TIME,
    CSR_INSTRET,

    CSR_CYCLEH		= 0xC80,  // only valid in 32-bit mode
    CSR_TIMEH,
    CSR_INSTRETH,
};

#define CSR_STATUS_S_BF    0:0
#define CSR_STATUS_PS_BF   1:1
#define CSR_STATUS_EI_BF   2:2
#define CSR_STATUS_PEI_BF  3:3
#define CSR_STATUS_EF_BF   4:4
#define CSR_STATUS_U64_BF  5:5
#define CSR_STATUS_S64_BF  6:6
#define CSR_STATUS_VM_BF   7:7
#define CSR_STATUS_IM_BF  23:16
#define CSR_STATUS_IP_BF  31:24


/* Interrupts */
enum {
    TRAP_INTR_IPI	= 5,
    TRAP_INTR_HOST	= 6,
    TRAP_INTR_TIMER	= 7,
};

/* Exceptions */
enum {
    TRAP_INST_MISALIGN	= 0,
    TRAP_INST_ADDR	= 1,
    TRAP_INST_ILLEGAL	= 2,
    TRAP_INST_PRIVILEGE = 3,
    TRAP_FP_DISABLED	= 4,
    TRAP_SYSTEM_CALL	= 6,
    TRAP_BREAKPOINT	= 7,
    TRAP_LOAD_MISALIGN	= 8,
    TRAP_STORE_MISALIGN	= 9,
    TRAP_LOAD_FAULT	= 10,
    TRAP_STORE_FAULT	= 11,
};

/* Virtual memory, page table entries */

#define PTE_V		0:0
#define PTE_T		1:1
#define PTE_G		2:2
#define PTE_UR		3:3
#define PTE_UW		4:4
#define PTE_UX		5:5
#define PTE_SR		6:6
#define PTE_SW		7:7
#define PTE_SX		8:8

#define PTE_PERMISSION  8:3

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
