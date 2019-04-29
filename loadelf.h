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

#ifndef _LOADELF_H_
#define _LOADELF_H_ 1

#include "memory.h"
#include <stdbool.h>

#include "elf.h"

#ifndef EM_RISCV
#define EM_RISCV          0xF3 /* Little endian RISC-V, 32- and 64-bit */
#endif

#define MAX_ELF_SECTIONS 32

typedef struct elf_info_st {
    uint32_t machine;
    bool     endian_is_big;
    bool     is_64bit;
    uint64_t program_entry;
    unsigned nsections;
    uint64_t section_start[MAX_ELF_SECTIONS];
    uint64_t section_size[MAX_ELF_SECTIONS];

    unsigned text_segments;
    uint64_t text_start;
    uint64_t text_size;

    /* Symbol table */
    void    *symtab;
    int      symtab_len;
    char    *strtab;
} elf_info_t;

int loadelf(memory_t *m, char *name, elf_info_t *program_entry);
bool getelfsym(elf_info_t *, const char *name, uint64_t *value);

/* loadelfs returns how many successfully loaded */
void loadelfs(memory_t *m, int n, char *name[], elf_info_t *last_info);
#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
