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
  LM32 opcode map.

  Register Immediate (RI) format
    0 | op:5 | r0:5 | r1:5 | imm:16
  Register Register (RR) format
    1 | op:5 | r0:5 | r1:5 | r2:5 | 0:11
  Control Register (CR) format
    1 | op:5 | csr:5 | r:5 | 0:16
  Immediate (I) format
    1 | 1 1 0 0 0 | imm:26 (bi)
    1 | 1 1 1 1 0 | imm:26 (calli)


  For non-paired instruction, bit 31 selects between opi (RI) and op
  (RR) variant of the same op.  For paired instruction (opa/opb), bit
  31 == 0 selects the first which is always RI format.  The format of
  opb is RR, except for the rcsr and wcsr instructions which interpret
  these slightly differently.

     00              08              10              18
  00 sru             and             lbu/b           andhi/bi (I)
  01 nor             xnor            be/modu         cmpe
  02 mul             lw/reserved     bg/sub          cmpg
  03 sh/divu         lhu/scall       bge/reserved    cmpge
  04 lb/rcsr (CR)    sb/sextb        bgeu/wcsr (CR)  cmpgeu
  05 sr              add             bgu/mod         cmpgu
  06 xor             or              sw/call         orhi/calli (I)
  07 lh/div          sl              bne/sexth       cmpne

  XXX break, bret, eret

  (Extension: 0x00..00 (sru r0=r0,0) and 0xFF..FF (~ "cmpne r31,31,31")
  instruction should be illegal and trap)

  (NB: the opcode map is curiously irregular, notably the load/store
       instructions.

   NB: the RR format can be extended greatly by assigning meaning to
       the lower 11-bit bits.)

 */

#ifndef _LM32_OPCODE_H
#define _LM32_OPCODE_H 1

#include <inttypes.h>
#include <stdbool.h>
#include "memory.h"

#define _LM32_OP_F(F) \
    F(SRUI) F(NORI) F(MULI) F(SH) F(LB) F(SRI) F(XORI) F(LH) \
    F(ANDI) F(XNORI) F(LW) F(LHU) F(SB) F(ADDI) F(ORI) F(SLI) \
    F(LBU) F(BE) F(BG) F(BGE) F(BGEU) F(BGU) F(SW) F(BNE) \
    F(ANDHI) F(CMPEI) F(CMPGI) F(CMPGEI) F(CMPGEUI) F(CMPGUI) F(ORHI) F(CMPNEI) \
    F(SRU) F(NOR) F(MUL) F(DIVU) F(RCSR) F(SR) F(XOR) F(DIV) \
    F(AND) F(XNOR) F(RES1) F(SCALL) F(SEXTB) F(ADD) F(OR) F(SL) \
    F(B) F(MODU) F(SUB) F(RES2) F(WCSR) F(MOD) F(CALL) F(SEXTH) \
    F(BI) F(CMPE) F(CMPG) F(CMPGE) F(CMPGEU) F(CMPGU) F(CALLI) F(CMPNE)


#define _LM32_MK_OP(O) O,
typedef enum lm32_opcode_e {
    _LM32_OP_F(_LM32_MK_OP)
} lm32_opcode_t;

typedef enum lm32_reg_e {
    R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15,
    R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, GP, FP, SP, RA, EA,
} lm32_reg_t;

typedef union lm32_instruction_u {
    struct {
        int             imm16  : 16;
        lm32_reg_t      rX     :  5;
        lm32_reg_t      rY     :  5;
        lm32_opcode_t   op     :  6;
    } ri; // CR is exactly the same except r0 is a different domain
    struct {
        int             imm0   : 11;
        lm32_reg_t      rX     :  5;
        lm32_reg_t      rZ     :  5;
        lm32_reg_t      rY     :  5;
        lm32_opcode_t   op     :  6;
    } rr;
    struct {
        int             imm26  : 26;
        lm32_opcode_t   op     :  6;
    } i;
    uint32_t raw;
} lm32_instruction_t;;


/*

  Machine Registers (Control and Status Registers)

PC
IE R/W 0x0
IM R/W 0x1
IP R 0x2
ICC W 0x3
DCC W 0x4
CC R 0x5
CFG R 0x6
EBA R/W 0x7
CFG2 R 0xA
No Program counter
Yes Interrupt enable
Yes Interrupt mask
Yes Interrupt pending
Yes Instruction cache control
Yes Data cache control
Yes Cycle counter
No Configuration
No Exception base address
No Extended configuration

PC = <pc:30 | 0 | 0>
IE = <-  | BIE | EIE | IE>  (breakpoint saved IE, exception saved EI, Interrupt enable)
ICC = <- | I> (write 1 to invalidate the instruction cache)
DCC = <- | I> (write 1 to invalidate the instruction cache)
CC  = once-per-clock cycle counter
CFG = <rev:6 | wp:3 | br:4 | int:6 | j | r | h | g | IC | DC | CC | x | u | s | d | m>
M 0– 1 –
D 0– 1 –
S 0– 1 –
U
X 0– 1 –
CC 0– 1 –
Multiply is not implemented Multiply is implemented
Divide is not implemented Divide is implemented
Barrel shift is not implemented Barrel shift is implemented
Sign extend is not implemented Sign extend is implemented
Cycle counter is not implemented Cycle counter is implemented
IC 0–Instructioncacheisnotimplemented 1 – Instruction cache is implemented
DC 0 – Data cache is not implemented 1 – Data cache is implemented
G 0 – Debug is not implemented 1 – Data cache is implemented
H 0 – H/W debug is not implemented 1 – H/W debug is implemented
R 0 – ROM debug is not implemented 1 – ROM debug is implemented
J 0 – JTAG UART is not implemented 1 – JTAG UART is implemented
INT 0 – 32
BP 0–4
WP 0–4
REV 0 – 63
Indicates whether a hardware multiplier is implemented.
Indicates whether a hardware divider is implemented.
Indicates whether a hardware barrel-shifter is implemented.
Reserved.
Indicates whether the sign- extension instructions are implemented.
Indicates whether the CC CSR is implemented.
Indicates whether an instruction cache is implemented.
Indicates whether a data cache is implemented.
Indicates whether software-based debug support is implemented.
Indicates whether hardware-based debug support is implemented.
Indicates whether support for debugging ROM-based programs is implemented.
Indicates whether a JTAG UART is implemented.
Indicates the number of external interrupts.
Indicates the number of breakpoint CSRs.
Indicates the number of watchpoint CSRs.
Processor revision number. This is set automatically. You cannot reset this field.

CFG2 <cfg2:30 | IMM | DIM> ??? local memories?
EBA = <eba: 24 | 0:8>

 */

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
