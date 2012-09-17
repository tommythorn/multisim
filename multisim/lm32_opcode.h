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
 *
 * Lm32 opcode map
 *
 */

#ifndef _LM32_OPCODE_H
#define _LM32_OPCODE_H 1

#include <inttypes.h>
#include <stdbool.h>
#include "memory.h"

typedef unsigned opcode_t;
typedef union lm32_instruction {
    struct {
        unsigned   number : 26;
        opcode_t   opcode :  6;
    } pal;
    struct {
        int        disp   : 21;
        unsigned   ra     :  5;
        opcode_t opcode :  6;
    } br;
    struct {
        int        disp   : 16;
        unsigned   rb     :  5;
        unsigned   ra     :  5;
        opcode_t   opcode :  6;
    } mem;
    struct {
        unsigned   rc     :  5;
        unsigned   func   : 11;
        unsigned   rb     :  5;
        unsigned   ra     :  5;
        opcode_t   opcode :  6;
    } fop;
    struct {
        unsigned   rc     :  5;
        unsigned   func   :  7;
        unsigned   isimm  :  1;
        unsigned   sbz    :  3;
        unsigned   rb     :  5;
        unsigned   ra     :  5;
        opcode_t   opcode :  6;
    } iop;
    struct {
        unsigned   rc     :  5;
        unsigned   func   :  7;
        unsigned   isimm  :  1;
        unsigned   lit    :  8;
        unsigned   ra     :  5;
        opcode_t   opcode :  6;
    } iop_imm;
    u_int32_t raw;
} inst_t;

#define mk_enum(O) OP_##O,
#define all_opcode(F)                           \
    F(PAL)                                      \
    F(Res1)                                     \
    F(Res2)                                     \
    F(Res3)                                     \
    F(Res4)                                     \
    F(Res5)                                     \
    F(Res6)                                     \
    F(Res7)                                     \
    F(LDA)                                      \
    F(LDAH)                                     \
    F(LDBU)                                     \
    F(LDQ_U)                                    \
    F(LDWU)                                     \
    F(STW)                                      \
    F(STB)                                      \
    F(STQ_U)                                    \
    F(INTA_)                                    \
    F(INTL_)                                    \
    F(INTS_)                                    \
    F(INTM)                                     \
    F(ITFP_)                                    \
    F(FLTV_)                                    \
    F(FLTI_)                                    \
    F(FLTL_)                                    \
    F(MISC_)                                    \
    F(PAL19)                                    \
    F(JSR_)                                     \
    F(PAL1B)                                    \
    F(FPTI_)                                    \
    F(PAL20)                                    \
    F(PAL21)                                    \
    F(PAL22)                                    \
    F(LDF)                                      \
    F(LDG)                                      \
    F(LDS)                                      \
    F(LDT)                                      \
    F(STF)                                      \
    F(STG)                                      \
    F(STS)                                      \
    F(STT)                                      \
    F(LDL)                                      \
    F(LDQ)                                      \
    F(LDL_L)                                    \
    F(LDQ_L)                                    \
    F(STL)                                      \
    F(STQ)                                      \
    F(STL_C)                                    \
    F(STQ_C)                                    \
    F(BR)                                       \
    F(FBEQ)                                     \
    F(FBLT)                                     \
    F(FBLE)                                     \
    F(BSR)                                      \
    F(FBNE)                                     \
    F(FBGE)                                     \
    F(FBGT)                                     \
    F(BLBC)                                     \
    F(BEQ)                                      \
    F(BLT)                                      \
    F(BLE)                                      \
    F(BLBS)                                     \
    F(BNE)                                      \
    F(BGE)                                      \
    F(BGT)

enum lm32_opcode {
    all_opcode(mk_enum)
};

#define all_inta_opcode(F)                      \
    F(00,ADDL)                                  \
    F(02,S4ADDL)                                \
    F(09,SUBL)                                  \
    F(0B,S4SUBL)                                \
    F(0F,CMPBGE)                                \
    F(12,S8ADDL)                                \
    F(1B,S8SUBL)                                \
    F(1D,CMPULT)                                \
    F(20,ADDQ)                                  \
    F(22,S4ADDQ)                                \
    F(29,SUBQ)                                  \
    F(2B,S4SUBQ)                                \
    F(2D,CMPEQ)                                 \
    F(32,S8ADDQ)                                \
    F(3B,S8SUBQ)                                \
    F(3D,CMPULE)                                \
    F(40,ADDL_V)                                \
    F(49,SUBL_V)                                \
    F(4D,CMPLT)                                 \
    F(60,ADDQ_V)                                \
    F(69,SUBQ_V)                                \
    F(6D,CMPLE)

#define mk_inta_opcode_enum(sub,name) OP_INTA_##name = 0x##sub,
enum lm32_inta_opcode {
    all_inta_opcode(mk_inta_opcode_enum)
};

#define all_intl_opcode(F)                      \
    F(00,AND)                                   \
    F(08,BIC)                                   \
    F(14,CMOVLBS)                               \
    F(16,CMOVLBC)                               \
    F(20,BIS)                                   \
    F(24,CMOVEQ)                                \
    F(26,CMOVNE)                                \
    F(28,ORNOT)                                 \
    F(40,XOR)                                   \
    F(44,CMOVLT)                                \
    F(46,CMOVGE)                                \
    F(48,EQV)                                   \
    F(61,AMASK)                                 \
    F(64,CMOVLE)                                \
    F(66,CMOVGT)                                \
    F(6C,IMPLVER)

#define mk_intl_opcode_enum(sub,name) OP_INTL_##name = 0x##sub,
enum lm32_intl_opcode {
    all_intl_opcode(mk_intl_opcode_enum)
};

#define all_ints_opcode(F)                      \
    F(02,MSKBL)                                 \
    F(06,EXTBL)                                 \
    F(0B,INSBL)                                 \
    F(12,MSKWL)                                 \
    F(16,EXTWL)                                 \
    F(1B,INSWL)                                 \
    F(22,MSKLL)                                 \
    F(26,EXTLL)                                 \
    F(2B,INSLL)                                 \
    F(30,ZAP)                                   \
    F(31,ZAPNOT)                                \
    F(32,MSKQL)                                 \
    F(34,SRL)                                   \
    F(36,EXTQL)                                 \
    F(39,SLL)                                   \
    F(3B,INSQL)                                 \
    F(3C,SRA)                                   \
    F(52,MSKWH)                                 \
    F(57,INSWH)                                 \
    F(5A,EXTWH)                                 \
    F(62,MSKLH)                                 \
    F(67,INSLH)                                 \
    F(6A,EXTLH)                                 \
    F(72,MSKQH)                                 \
    F(77,INSQH)                                 \
    F(7A,EXTQH)

#define all_intm_opcode(F)                      \
    F(00,MULL)                                  \
    F(20,MULQ)                                  \
    F(30,UMULH)                                 \
    F(40,MULL_V)                                \
    F(60,MULQ_V)

#define all_itfp_opcode(F)                      \
    F(004,ITOFS)                                \
    F(00A,SQRTF_C)                              \
    F(00B,SQRTS_C)                              \
    F(014,ITOFF)                                \
    F(024,ITOFT)                                \
    F(02A,SQRTG_C)                              \
    F(02B,SQRTT_C)                              \
    F(04B,SQRTS_M)                              \
    F(06B,SQRTT_M)                              \
    F(08A,SQRTF)                                \
    F(08B,SQRTS)                                \
    F(0AA,SQRTG)                                \
    F(0AB,SQRTT)                                \
    F(0CB,SQRTS_D)                              \
    F(0EB,SQRTT_D)                              \
    F(10A,SQRTF_UC)                             \
    F(10B,SQRTS_UC)                             \
    F(12A,SQRTG_UC)                             \
    F(12B,SQRTT_UC)                             \
    F(14B,SQRTS_UM)                             \
    F(16B,SQRTT_UM)                             \
    F(18A,SQRTF_U)                              \
    F(18B,SQRTS_U)                              \
    F(1AA,SQRTG_U)                              \
    F(1AB,SQRTT_U)                              \
    F(1CB,SQRTS_UD)                             \
    F(1EB,SQRTT_UD)                             \
    F(40A,SQRTF_SC)                             \
    F(42A,SQRTG_SC)                             \
    F(48A,SQRTF_S)                              \
    F(4AA,SQRTG_S)                              \
    F(50A,SQRTF_SUC)                            \
    F(50B,SQRTS_SUC)                            \
    F(52A,SQRTG_SUC)                            \
    F(52B,SQRTT_SUC)                            \
    F(54B,SQRTS_SUM)                            \
    F(56B,SQRTT_SUM)                            \
    F(58A,SQRTF_SU)                             \
    F(58B,SQRTS_SU)                             \
    F(5AA,SQRTG_SU)                             \
    F(5AB,SQRTT_SU)                             \
    F(5CB,SQRTS_SUD)                            \
    F(5EB,SQRTT_SUD)                            \
    F(70B,SQRTS_SUIC)                           \
    F(72B,SQRTT_SUIC)                           \
    F(74B,SQRTS_SUIM)                           \
    F(76B,SQRTT_SUIM)                           \
    F(78B,SQRTS_SUI)                            \
    F(7AB,SQRTT_SUI)                            \
    F(7CB,SQRTS_SUID)                           \
    F(7EB,SQRTT_SUID)

// Note, I'm skipping the VAX opcodes, seriously!

#define all_flti_opcode(F)                      \
    F(000,ADDS_C)                               \
    F(001,SUBS_C)                               \
    F(002,MULS_C)                               \
    F(003,DIVS_C)                               \
    F(020,ADDT_C)                               \
    F(021,SUBT_C)                               \
    F(022,MULT_C)                               \
    F(023,DIVT_C)                               \
    F(02C,CVTTS_C)                              \
    F(02F,CVTTQ_C)                              \
    F(03C,CVTQS_C)                              \
    F(03E,CVTQT_C)                              \
    F(040,ADDS_M)                               \
    F(041,SUBS_M)                               \
    F(042,MULS_M)                               \
    F(043,DIVS_M)                               \
    F(060,ADDT_M)                               \
    F(061,SUBT_M)                               \
    F(062,MULT_M)                               \
    F(063,DIVT_M)                               \
    F(06C,CVTTS_M)                              \
    F(06F,CVTTQ_M)                              \
    F(07C,CVTQS_M)                              \
    F(07E,CVTQT_M)                              \
    F(080,ADDS)                                 \
    F(081,SUBS)                                 \
    F(082,MULS)                                 \
    F(083,DIVS)                                 \
    F(0A0,ADDT)                                 \
    F(0A1,SUBT)                                 \
    F(0A2,MULT)                                 \
    F(0A3,DIVT)                                 \
    F(0A4,CMPTUN)                               \
    F(0A5,CMPTEQ)                               \
    F(0A6,CMPTLT)                               \
    F(0A7,CMPTLE)                               \
    F(0AC,CVTTS)                                \
    F(0AF,CVTTQ)                                \
    F(0BC,CVTQS)                                \
    F(0BE,CVTQT)                                \
    F(0C0,ADDS_D)                               \
    F(0C1,SUBS_D)                               \
    F(0C2,MULS_D)                               \
    F(0C3,DIVS_D)                               \
    F(0E0,ADDT_D)                               \
    F(0E1,SUBT_D)                               \
    F(0E2,MULT_D)                               \
    F(0E3,DIVT_D)                               \
    F(0EC,CVTTS_D)                              \
    F(0EF,CVTTQ_D)                              \
    F(0FC,CVTQS_D)                              \
    F(0FE,CVTQT_D)                              \
    F(100,ADDS_UC)                              \
    F(101,SUBS_UC)                              \
    F(102,MULS_UC)                              \
    F(103,DIVS_UC)                              \
    F(120,ADDT_UC)                              \
    F(121,SUBT_UC)                              \
    F(122,MULT_UC)                              \
    F(123,DIVT_UC)                              \
    F(12C,CVTTS_UC)                             \
    F(12F,CVTTQ_VC)                             \
    F(140,ADDS_UM)                              \
    F(141,SUBS_UM)                              \
    F(142,MULS_UM)                              \
    F(143,DIVS_UM)                              \
    F(160,ADDT_UM)                              \
    F(161,SUBT_UM)                              \
    F(162,MULT_UM)                              \
    F(163,DIVT_UM)                              \
    F(16C,CVTTS_UM)                             \
    F(16F,CVTTQ_VM)                             \
    F(180,ADDS_U)                               \
    F(181,SUBS_U)                               \
    F(182,MULS_U)                               \
    F(183,DIVS_U)                               \
    F(1A0,ADDT_U)                               \
    F(1A1,SUBT_U)                               \
    F(1A2,MULT_U)                               \
    F(1A3,DIVT_U)                               \
    F(1AC,CVTTS_U)                              \
    F(1AF,CVTTQ_V)                              \
    F(1C0,ADDS_UD)                              \
    F(1C1,SUBS_UD)                              \
    F(1C2,MULS_UD)                              \
    F(1C3,DIVS_UD)                              \
    F(1E0,ADDT_UD)                              \
    F(1E1,SUBT_UD)                              \
    F(1E2,MULT_UD)                              \
    F(1E3,DIVT_UD)                              \
    F(1EC,CVTTS_UD)                             \
    F(1EF,CVTTQ_VD)                             \
    F(2AC,CVTST)                                \
    F(500,ADDS_SUC)                             \
    F(501,SUBS_SUC)                             \
    F(502,MULS_SUC)                             \
    F(503,DIVS_SUC)                             \
    F(520,ADDT_SUC)                             \
    F(521,SUBT_SUC)                             \
    F(522,MULT_SUC)                             \
    F(523,DIVT_SUC)                             \
    F(52C,CVTTS_SUC)                            \
    F(52F,CVTTQ_SVC)                            \
    F(540,ADDS_SUM)                             \
    F(541,SUBS_SUM)                             \
    F(542,MULS_SUM)                             \
    F(543,DIVS_SUM)                             \
    F(560,ADDT_SUM)                             \
    F(561,SUBT_SUM)                             \
    F(562,MULT_SUM)                             \
    F(563,DIVT_SUM)                             \
    F(56C,CVTTS_SUM)                            \
    F(56F,CVTTQ_SVM)                            \
    F(580,ADDS_SU)                              \
    F(581,SUBS_SU)                              \
    F(582,MULS_SU)                              \
    F(583,DIVS_SU)                              \
    F(5A0,ADDT_SU)                              \
    F(5A1,SUBT_SU)                              \
    F(5A2,MULT_SU)                              \
    F(5A3,DIVT_SU)                              \
    F(5A4,CMPTUN_SU)                            \
    F(5A5,CMPTEQ_SU)                            \
    F(5A6,CMPTLT_SU)                            \
    F(5A7,CMPTLE_SU)                            \
    F(5AC,CVTTS_SU)                             \
    F(5AF,CVTTQ_SV)                             \
    F(5C0,ADDS_SUD)                             \
    F(5C1,SUBS_SUD)                             \
    F(5C2,MULS_SUD)                             \
    F(5C3,DIVS_SUD)                             \
    F(5E0,ADDT_SUD)                             \
    F(5E1,SUBT_SUD)                             \
    F(5E2,MULT_SUD)                             \
    F(5E3,DIVT_SUD)                             \
    F(5EC,CVTTS_SUD)                            \
    F(5EF,CVTTQ_SVD)                            \
    F(6AC,CVTST_S)                              \
    F(700,ADDS_SUIC)                            \
    F(701,SUBS_SUIC)                            \
    F(702,MULS_SUIC)                            \
    F(703,DIVS_SUIC)                            \
    F(720,ADDT_SUIC)                            \
    F(721,SUBT_SUIC)                            \
    F(722,MULT_SUIC)                            \
    F(723,DIVT_SUIC)                            \
    F(72C,CVTTS_SUIC)                           \
    F(72F,CVTTQ_SVIC)                           \
    F(73C,CVTQS_SUIC)                           \
    F(73E,CVTQT_SUIC)                           \
    F(740,ADDS_SUIM)                            \
    F(741,SUBS_SUIM)                            \
    F(742,MULS_SUIM)                            \
    F(743,DIVS_SUIM)                            \
    F(760,ADDT_SUIM)                            \
    F(761,SUBT_SUIM)                            \
    F(762,MULT_SUIM)                            \
    F(763,DIVT_SUIM)                            \
    F(76C,CVTTS_SUIM)                           \
    F(76F,CVTTQ_SVIM)                           \
    F(77C,CVTQS_SUIM)                           \
    F(77E,CVTQT_SUIM)                           \
    F(780,ADDS_SUI)                             \
    F(781,SUBS_SUI)                             \
    F(782,MULS_SUI)                             \
    F(783,DIVS_SUI)                             \
    F(7A0,ADDT_SUI)                             \
    F(7A1,SUBT_SUI)                             \
    F(7A2,MULT_SUI)                             \
    F(7A3,DIVT_SUI)                             \
    F(7AC,CVTTS_SUI)                            \
    F(7AF,CVTTQ_SVI)                            \
    F(7BC,CVTQS_SUI)                            \
    F(7BE,CVTQT_SUI)                            \
    F(7C0,ADDS_SUID)                            \
    F(7C1,SUBS_SUID)                            \
    F(7C2,MULS_SUID)                            \
    F(7C3,DIVS_SUID)                            \
    F(7E0,ADDT_SUID)                            \
    F(7E1,SUBT_SUID)                            \
    F(7E2,MULT_SUID)                            \
    F(7E3,DIVT_SUID)                            \
    F(7EC,CVTTS_SUID)                           \
    F(7EF,CVTTQ_SVID)                           \
    F(7FC,CVTQS_SUID)                           \
    F(7FE,CVTQT_SUID)

#define all_fltl_opcode(F)                      \
    F(010,CVTLQ)                                \
    F(020,CPYS)                                 \
    F(021,CPYSN)                                \
    F(022,CPYSE)                                \
    F(024,MT_FPCR)                              \
    F(025,MF_FPCR)                              \
    F(02A,FCMOVEQ)                              \
    F(02B,FCMOVNE)                              \
    F(02C,FCMOVLT)                              \
    F(02D,FCMOVGE)                              \
    F(02E,FCMOVLE)                              \
    F(02F,FCMOVGT)                              \
    F(030,CVTQL)                                \
    F(130,CVTQL_V)                              \
    F(530,CVTQL_SV)


#define all_misc_opcode(F)                      \
    F(0000,TRAPB)                               \
    F(0400,EXCB)                                \
    F(4000,MB)                                  \
    F(4400,WMB)                                 \
    F(8000,FETCH)                               \
    F(A000,FETCH_M)                             \
    F(C000,RPCC)                                \
    F(E000,RC)                                  \
    F(E800,ECB)                                 \
    F(F000,RS)                                  \
    F(F800,WH64)                                \
    F(FC00,WH64EN)

#define all_jsr_opcode(F)                       \
    F(0,JMP)                                    \
    F(1,JSR)                                    \
    F(2,RET)                                    \
    F(3,JSR_COROUTINE)

#define all_fpti_opcode(F)                      \
    F(00,SEXTB)                                 \
    F(01,SEXTW)                                 \
    F(30,CTPOP)                                 \
    F(31,PERR)                                  \
    F(32,CTLZ)                                  \
    F(33,CTTZ)                                  \
    F(34,UNPKBW)                                \
    F(35,UNPKBL)                                \
    F(36,PKWB)                                  \
    F(37,PKLB)                                  \
    F(38,MINSB8)                                \
    F(39,MINSW4)                                \
    F(3A,MINUB8)                                \
    F(3B,MINUW4)                                \
    F(3C,MAXUB8)                                \
    F(3D,MAXUW4)                                \
    F(3E,MAXSB8)                                \
    F(3F,MAXSW4)                                \
    F(70,FTOIT)                                 \
    F(78,FTOIS)
#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
