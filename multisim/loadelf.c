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
 * loadelf.c - binary image loader. Assumes only memory.h
 *
 * Copyright (C) 2004-2012 Tommy Thorn - All Rights Reserved
 *
 * This is propri

 */

#include "loadelf.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#if defined(__APPLE__)
#include "libelf/sys_elf.h"
#else
#include "elf.h"
#endif



static void
loadsection(FILE *f, unsigned f_offset, unsigned f_len,
            memory_t *m, uint32_t m_addr, size_t m_len, elf_info_t *info)
{
    memory_ensure_mapped_range(m, m_addr, m_len);

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

int loadelf(memory_t *m, char *name, elf_info_t *elf_info)
{
    const bool enable_verb_prog_sec = false;
    const bool enable_verb_elf = false;

    Elf64_Ehdr ehdr;
    Elf64_Phdr *ph;
    FILE *f = fopen(name, "r");

    if (!f)
        return 1;

    if (fread(&ehdr, sizeof ehdr, 1, f) != 1)
        return 2;

    if (strncmp((char *)ehdr.e_ident, ELFMAG, SELFMAG))
        return 3;

    memset(elf_info, 0, sizeof *elf_info);

    elf_info->endian_is_big = ehdr.e_ident[EI_DATA] == 2;
    memory_set_endian(m, elf_info->endian_is_big);

    if (ehdr.e_type != ET_EXEC)
        return 4;

    // MacPorts' libelf gets this wrong, Linux elf.h gets it right
    if (ehdr.e_machine != 0x9026 /*EM_ALPHA*/)
        return 5;

    if (elf_info->endian_is_big)
        return 6;

    if (enable_verb_prog_sec) {
        printf("%s:\n", name);
        printf("%sendian\n", elf_info->endian_is_big ? "big" : "little");
        printf("Entry:             %016lx\n", ehdr.e_entry); /* Entry point virtual address */
        printf("Proc Flags:        %08x\n", ehdr.e_flags); /* Processor-specific flags */
        printf("Phdr.tbl entry cnt % 8d\n", ehdr.e_phnum);    /*Program header table entry count */
        printf("Shdr.tbl entry cnt % 8d\n", ehdr.e_shnum);    /*Section header table entry count */
        printf("Shdr.str tbl idx   % 8d\n", ehdr.e_shstrndx); /*Section header string table index */
    }

    elf_info->program_entry = ehdr.e_entry;

    if (ehdr.e_ehsize != sizeof ehdr) {
        return 7;
    }

    if (ehdr.e_shentsize != sizeof(Elf64_Shdr)) {
        return 8;
    }

    // Allocate program headers
    ph = alloca(sizeof *ph * ehdr.e_phnum);

    for (int i = 0; i < ehdr.e_phnum; ++i) {

        fseek(f, ehdr.e_phoff + i * ehdr.e_phentsize, SEEK_SET);

        if (fread(ph + i, sizeof *ph, 1, f) != 1)
            return 9;

        if (enable_verb_prog_sec) {
            printf("\nProgram header #%d (%lx)\n", i, ftell(f));
            printf(" type             %08x\n",   ph[i].p_type);
            printf(" filesz           %016lx\n", ph[i].p_filesz);
            printf(" offset           %016lx\n", ph[i].p_offset);
            printf(" vaddr            %016lx\n", ph[i].p_vaddr);
            printf(" paddr            %016lx\n", ph[i].p_paddr);
            printf(" memsz            %016lx\n", ph[i].p_memsz);
            printf(" flags            %08x\n",   ph[i].p_flags);
            printf(" align            %016lx\n", ph[i].p_align);
        }

        if (ph[i].p_type == PT_LOAD && ph[i].p_filesz) {

            if (enable_verb_prog_sec)
                fprintf(stderr, "Loading section [%016lx; %016lx]\n",
                        ph[i].p_vaddr, ph[i].p_vaddr + ph[i].p_memsz - 1);

            // XXX memory_t only supports 32-bit address space - this depends on luck
            // to work.
            loadsection(f, (unsigned)ph[i].p_offset, (unsigned)ph[i].p_filesz,
                        m, (uint32_t)ph[i].p_vaddr, (size_t)ph[i].p_memsz,
                        elf_info);
        }

        if (ph[i].p_flags & 1) {
            elf_info->text_segments++;
            elf_info->text_start = ph[i].p_vaddr;
            elf_info->text_size  = ph[i].p_memsz;
        }
    }

    if (enable_verb_elf) {
        printf("\n");

        fseek(f, ehdr.e_shoff, SEEK_SET);

        for (int i = 0; i < ehdr.e_shnum; ++i) {
            Elf64_Shdr sh;

            if (fread(&sh, sizeof sh, 1, f) != 1)
                return 10;

            printf("\nSection header #%d (%lx)\n", i, ftell(f));
            printf(" name            %08x\n", sh.sh_name);
            printf(" type            %08x\n", sh.sh_type);
            printf(" flags           %016lx\n", sh.sh_flags);
            printf(" addr            %016lx\n", sh.sh_addr);
            printf(" offset          %016lx\n", sh.sh_offset);
            printf(" size            %016lx\n", sh.sh_size);
            printf(" link            %08x\n", sh.sh_link);
            printf(" info            %08x\n", sh.sh_info);
            printf(" addralign       %016lx\n", sh.sh_addralign);
            printf(" entsize         %016lx\n", sh.sh_entsize);
        }

        printf(" (now at %lx)\n", ftell(f));
    }

    return 0;
}

int loadelfs(memory_t *m, int n, char *name[], elf_info_t *last_info)
{
    int i;
    for (i = 0; i < n; ++i) {
        int r = loadelf(m, name[i], last_info);
        if (r)
            break;
    }

    return i;
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
