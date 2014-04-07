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

#include "loadelf.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <arpa/inet.h>
#include <inttypes.h>
#include <alloca.h>

static const bool enable_verb_prog_sec = false;
static const bool enable_verb_elf      = false;

static void
loadsection(FILE *f, unsigned f_offset, unsigned f_len,
            memory_t *m, uint64_t m_addr, uint64_t m_len, elf_info_t *info)
{
    memory_ensure_mapped_range(m, m_addr, m_addr + m_len);

    info->section_start[info->nsections]  = m_addr;
    info->section_size[info->nsections++] = m_len;
    assert(info->nsections <
           sizeof info->section_start / sizeof(unsigned));

    /*
     * We clear memory so that BBS doesn't need special
     * initialization
     */
    void *buf = memory_physical(m, m_addr, m_len);
    fseek(f, f_offset, SEEK_SET);
    memset(buf, 0, m_len);
    fread(buf, f_len, 1, f);
}

#ifdef _BIG_ENDIAN
#error "I don't support this"
#endif

#define NATIVE(x)                                                       \
    ((__typeof__(x)) (ehdr.e_ident[EI_DATA] == ELFDATA2LSB ? (x) :     \
                      sizeof(x) == 2                       ? htons(x) : \
                      sizeof(x) == 4                       ? htonl(x) : \
                      sizeof(x) == 8                       ? swp64(x) : \
                      (x)))

static uint64_t
swp64(uint64_t x)
{
    uint64_t lo = htonl(x);
    uint64_t hi = htonl(x >> 32);

    return (lo << 32) | hi;
}

#define SZ(x,y) x ## 32 ## y
#include "loadelf_temp.c"
#undef SZ
#define SZ(x,y) x ## 64 ## y
#include "loadelf_temp.c"

int loadelf(memory_t *m, char *name, elf_info_t *elf_info)
{
    Elf64_Ehdr ehdr;
    FILE *f = fopen(name, "r");

    if (!f) {
        perror(name);
        return 1;
    }

    if (fread(&ehdr, sizeof ehdr, 1, f) != 1) {
        fprintf(stderr, "%s: short header read, file corrupted?\n", name);
        return 2;
    }

    if (strncmp((char *)ehdr.e_ident, ELFMAG, SELFMAG)) {
        fprintf(stderr, "%s: Not an ELF file\n", name);
        return 3;
    }

    rewind(f);

    if (ehdr.e_ident[EI_CLASS] == ELFCLASS32)
        return loadelf32(m, name, f, elf_info);

    if (ehdr.e_ident[EI_CLASS] == ELFCLASS64)
        return loadelf64(m, name, f, elf_info);

    fprintf(stderr, "%s: Not an ELF file? (EI_CLASS = %d)\n", name,
            ehdr.e_ident[EI_CLASS]);

    return 4;
}

void loadelfs(memory_t *m, int n, char *name[], elf_info_t *last_info)
{
    for (int i = 0; i < n; ++i)
        if (loadelf(m, name[i], last_info)) {
            fprintf(stderr, "error: loading %s failed", name[i]);
            exit(1);
        }
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
