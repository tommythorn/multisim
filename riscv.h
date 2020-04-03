/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2014,2018 Tommy Thorn
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
 * RISC-V opcode map, really just the RV32IMA 2.0 part
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
    uint32_t raw;
} insn_t;

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
    ECALLEBREAK, CSRRW, CSRRS, CSRRC, TBD, CSRRWI, CSRRSI, CSRRCI,
};

enum riscv_subcode_trap_return_e {
    ECALL       = 0x000,
    EBREAK      = 0x001,
    URET        = 0x002,
    SRET        = 0x102,
    WFI         = 0x105,
    SFENCE_VMA  = 0x120,
    MRET        = 0x302,
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


/* Compressed opcodes */
enum riscv_opcode_c0_e {
    C0_ADDI4SPN = 0,
    C0_FLD      = 1,
    C0_LW       = 2,
    C0_FLW      = 3, // RV64: c.ld
    C0_FSD      = 5,
    C0_SW       = 6,
    C0_FSW      = 7, // RV64: c.sd
};

enum riscv_opcode_c1_e {
    C1_ADDI     = 0, // + nop
    C1_JAL      = 1,
    C1_LI       = 2,
    C1_LUI      = 3, // + c.addi16sp
    C1_MISCALU  = 4, // srli, srai, andi, sub, xor, or, and
    C1_J        = 5,
    C1_BEQZ     = 6,
    C1_BNEZ     = 7,
};

enum riscv_opcode_c1_miscalu_e {
    C1_MISCALU_SRLI = 0,
    C1_MISCALU_SRAI = 1,
    C1_MISCALU_ANDI = 2,
    C1_MISCALU_EXT  = 3,
};

enum riscv_opcode_c1_miscalu_ext_e {
    C1_MISCALU_EXT_SUB = 0,
    C1_MISCALU_EXT_XOR = 1,
    C1_MISCALU_EXT_OR  = 2,
    C1_MISCALU_EXT_AND = 3,
};

enum riscv_opcode_c2_e {
    C2_SLLI     = 0,
    C2_FLDSP    = 1,
    C2_LWSP     = 2,
    C2_FLWSP    = 3,
    C2_ADD      = 4, // + c.mv c.jr c.ebreak c.jalr
    C2_FSDSP    = 5,
    C2_SWSP     = 6,
    C2_FSWSP    = 7,
};

/* CSRs */

enum riscv_csr_e {
// Table 2.2 User-level CSRs
    CSR_USTATUS         = 0x000,
    CSR_FFLAGS          = 0x001, // fcsr[4:0] alias
    CSR_FRM             = 0x002, // fcsr[7:5] alias
    CSR_FCSR            = 0x003,
    CSR_UIE             = 0x004,
    CSR_UTVEC           = 0x005,

    CSR_USCRATCH        = 0x040,
    CSR_UEPC            = 0x041,
    CSR_UCAUSE          = 0x042,
    CSR_UTVAL           = 0x043,
    CSR_UIP             = 0x044,

// Table 2.3 Supervisor-level CSRs
    CSR_SSTATUS         = 0x100,
    CSR_SEDELEG         = 0x102,
    CSR_SIDELEG         = 0x103,
    CSR_SIE             = 0x104,
    CSR_STVEC           = 0x105,
    CSR_SCOUNTEREN      = 0x106,

    CSR_SSCRATCH        = 0x140,
    CSR_SEPC            = 0x141,
    CSR_SCAUSE          = 0x142,
    CSR_STVAL           = 0x143,
    CSR_SIP             = 0x144,

    CSR_SATP            = 0x180,


    CSR_MCYCLE          = 0xB00,
    CSR_MINSTRET        = 0xB02,

    CSR_MCYCLEH         = 0xB80,  // only valid in 32-bit mode
    CSR_MINSTRETH       = 0xB82,


    CSR_CYCLE           = 0xC00,
    CSR_TIME,
    CSR_INSTRET,

    CSR_CYCLEH          = 0xC80,  // only valid in 32-bit mode
    CSR_TIMEH,
    CSR_INSTRETH,

// Table 2.4 Machine-level CSRs
    CSR_MVENDORID       = 0xF11,
    CSR_MARCHID         = 0xF12,
    CSR_MIMPID          = 0xF13,
    CSR_MHARTID         = 0xF14,

    CSR_MSTATUS         = 0x300,
    CSR_MEDELEG         = 0x302,
    CSR_MIDELEG         = 0x303,
    CSR_MIE             = 0x304,
    CSR_MTVEC           = 0x305,
    CSR_MCOUNTEREN      = 0x306,

    CSR_MSCRATCH        = 0x340,
    CSR_MEPC            = 0x341,
    CSR_MCAUSE          = 0x342,
    CSR_MTVAL           = 0x343,
    CSR_MIP             = 0x344,

    // Omitting the Machine Protection and Translation

// Table 2.5
};

// 32-bit version!
#define CSR_STATUS_UIE_BF       0:0
#define CSR_STATUS_SIE_BF       1:1

#define CSR_STATUS_MIE_BF       3:3
#define CSR_STATUS_UPIE_BF      4:4
#define CSR_STATUS_SPIE_BF      5:5

#define CSR_STATUS_MPIE_BF      7:7
#define CSR_STATUS_SPP_BF       8:8

#define CSR_STATUS_MPP_BF     12:11
#define CSR_STATUS_FS_BF      14:13
#define CSR_STATUS_XS_BF      16:15
#define CSR_STATUS_MPRV_BF    17:17
#define CSR_STATUS_SUM_BF     18:18
#define CSR_STATUS_MXR_BF     19:19
#define CSR_STATUS_TVM_BF     20:20
#define CSR_STATUS_TW_BF      21:21
#define CSR_STATUS_TSR_BF     22:22
#define CSR_STATUS_SD_BF      31:31

/* Interrupts */
enum {
    TRAP_INTR_TIMER     = 7,
};

enum {
    MIP_MTIP = 1 << TRAP_INTR_TIMER,
    MIE_MTIE = 1 << TRAP_INTR_TIMER,
};

/* Exceptions */
enum {
    EXCP_INSN_MISALIGN          = 0,
    EXCP_INSN_ACCESS_FAULT,
    EXCP_INSN_ILLEGAL,
    EXCP_BREAKPOINT,

    EXCP_LOAD_MISALIGN,
    EXCP_LOAD_ACCESS_FAULT,
    EXCP_STORE_MISALIGN,
    EXCP_STORE_ACCESS_FAULT,

    EXCP_UMODE_CALL,
    EXCP_SMODE_CALL,
    EXCP_RESERVED_10,
    EXCP_MMODE_CALL,

    EXCP_INSN_PAGE_FAULT,
    EXCP_LOAD_PAGE_FAULT,
    EXCP_RESERVED_14,
    EXCP_STORE_PAGE_FAULT,
};

/* Virtual memory, page table entries */

#define PTE_V           0:0
#define PTE_R           1:1
#define PTE_W           2:2
#define PTE_X           3:3
#define PTE_U           4:4
#define PTE_G           5:5
#define PTE_A           6:6
#define PTE_D           7:7

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
