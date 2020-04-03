#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "arch.h"
#include "rvc.h"

int main(int c, char **v)
{
    const arch_t *arch = get_arch(EM_RISCV, true);
    uint64_t addr = 0x80000000;

    bool gen = c == 2 && strcmp(v[1], "--gen") == 0;

    if (gen) {
        // 15:13    12:2   1:0
        // funct3     x     op

        printf("\t.file\t\"all_compressed.c\"\n");
        printf("\t.option nopic\n");
        printf("\t.text\n");
        printf("\t.align\t1\n");
        printf("\t.globl\t_start\n");
        printf("\t.type\t_start, @function\n");
        printf("_start:\n");
    }

    for (int op = 0; op < 3; ++op)
        for (int funct3 = 0; funct3 < 8; ++funct3)
            for (int x = 0; x < 2048; ++x) {
                uint32_t insn = (funct3 << 13) | (x << 2) | op;
                if (gen) {
                    printf("\t.half\t0x%04x\n", insn);
                } else {
                    printf("    %08" PRIx64 ":\t%04x                	", addr, insn);

                    char dis_buf[99];
                    arch->disass_insn(addr, insn, dis_buf, sizeof dis_buf);
                    printf("%s\n", dis_buf);

                    fprintf(stderr, "%04x -> %08x\n", insn, rvcdecoder(insn, 64));

                    addr += 2;
                }
            }

    if (gen) {
        printf("\t.size\t_start, .-_start\n");
        printf("\t.ident\t\"GCC: (Ubuntu 9.2.1-9ubuntu1) 9.2.1 20191008\"\n");
        printf("\t.section\t.note.GNU-stack,\"\",@progbits\n");
    }

    return 0;
}


// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
