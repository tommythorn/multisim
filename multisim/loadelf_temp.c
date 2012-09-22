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

int SZ(loadelf,)(memory_t *m, char *name, FILE *f, elf_info_t *elf_info)
{
    memset(elf_info, 0, sizeof *elf_info);

    SZ(Elf,_Ehdr) ehdr;
    SZ(Elf,_Phdr) *ph;

    if (fread(&ehdr, sizeof ehdr, 1, f) != 1) {
        fprintf(stderr, "%s: short header read, file corrupted?\n", name);
        return 10;
    }

    if (ehdr.e_ident[EI_DATA] != ELFDATA2MSB && ehdr.e_ident[EI_DATA] != ELFDATA2LSB) {
        fprintf(stderr, "%s: Unsupported endian (%d)\n",
                name, ehdr.e_ident[EI_DATA]);
        return 11;
    }

    elf_info->endian_is_big = ehdr.e_ident[EI_DATA] == ELFDATA2MSB;

    if (NATIVE(ehdr.e_type) != ET_EXEC) {
        fprintf(stderr, "%s: Need a fully linked ELF executable, not type %d\n",
                name, NATIVE(ehdr.e_type));
        return 12;
    }

    elf_info->machine = NATIVE(ehdr.e_machine);

    if (elf_info->machine != EM_ALPHA &&
        elf_info->machine != EM_LM32) {
        fprintf(stderr, "%s: Unsupported machine architecture %d\n",
                name, NATIVE(ehdr.e_machine));
        return 13;
    }

    if (enable_verb_prog_sec) {
        printf("%s:\n", name);
        printf("%sendian\n", elf_info->endian_is_big ? "big" : "little");
        printf("Entry:             %016llx\n",
               (uint64_t) NATIVE(ehdr.e_entry)); /* Entry point virtual address */
        printf("Proc Flags:        %08x\n",
               NATIVE(ehdr.e_flags)); /* Processor-specific flags */
        printf("Phdr.tbl entry cnt % 8d\n",
               NATIVE(ehdr.e_phnum));    /*Program header table entry count */
        printf("Shdr.tbl entry cnt % 8d\n",
               NATIVE(ehdr.e_shnum));    /*Section header table entry count */
        printf("Shdr.str tbl idx   % 8d\n",
               NATIVE(ehdr.e_shstrndx)); /*Section header string table index */
    }

    elf_info->program_entry = NATIVE(ehdr.e_entry);

    if (NATIVE(ehdr.e_ehsize) != sizeof ehdr) {
        return 14;
    }

    if (NATIVE(ehdr.e_shentsize) != sizeof(SZ(Elf,_Shdr))) {
        return 15;
    }

    // Allocate program headers
    ph = alloca(sizeof *ph * NATIVE(ehdr.e_phnum));

    int phnum = NATIVE(ehdr.e_phnum);

    for (int i = 0; i < phnum; ++i) {

        fseek(f, NATIVE(ehdr.e_phoff) + i * NATIVE(ehdr.e_phentsize), SEEK_SET);

        if (fread(ph + i, sizeof *ph, 1, f) != 1)
            return 16;

        if (enable_verb_prog_sec) {
            printf("\nProgram header #%d (%lx)\n", i, ftell(f));
            printf(" type             %08x\n",   NATIVE(ph[i].p_type));
            printf(" filesz           %016llx\n", (uint64_t)NATIVE(ph[i].p_filesz));
            printf(" offset           %016llx\n", (uint64_t)NATIVE(ph[i].p_offset));
            printf(" vaddr            %016llx\n", (uint64_t)NATIVE(ph[i].p_vaddr));
            printf(" paddr            %016llx\n", (uint64_t)NATIVE(ph[i].p_paddr));
            printf(" memsz            %016llx\n", (uint64_t)NATIVE(ph[i].p_memsz));
            printf(" flags            %08x\n",    NATIVE(ph[i].p_flags));
            printf(" align            %016llx\n", (uint64_t)NATIVE(ph[i].p_align));
        }

        if (NATIVE(ph[i].p_type) == PT_LOAD && NATIVE(ph[i].p_filesz)) {

            if (enable_verb_prog_sec)
                fprintf(stderr, "Loading section [%016llx; %016llx]\n",
                        (uint64_t)NATIVE(ph[i].p_vaddr),
                        (uint64_t)NATIVE(ph[i].p_vaddr) + NATIVE(ph[i].p_memsz) - 1);

            // XXX memory_t only supports 32-bit address space - this depends on luck
            // to work.
            loadsection(f,
                        (unsigned)NATIVE(ph[i].p_offset),
                        (unsigned)NATIVE(ph[i].p_filesz),
                        m,
                        (uint32_t)NATIVE(ph[i].p_vaddr),
                        (size_t)NATIVE(ph[i].p_memsz),
                        elf_info);
        }

        if (ph[i].p_flags & 1) {
            elf_info->text_segments++;
            elf_info->text_start = NATIVE(ph[i].p_vaddr);
            elf_info->text_size  = NATIVE(ph[i].p_memsz);
        }
    }

    if (enable_verb_elf) {
        printf("\n");

        fseek(f, NATIVE(ehdr.e_shoff), SEEK_SET);

        int shnum = NATIVE(ehdr.e_shnum);
        for (int i = 0; i < shnum; ++i) {
            SZ(Elf,_Shdr) sh;

            if (fread(&sh, sizeof sh, 1, f) != 1)
                return 17;

            printf("\nSection header #%d (%lx)\n", i, ftell(f));
            printf(" name            %08x\n", NATIVE(sh.sh_name));
            printf(" type            %08x\n", NATIVE(sh.sh_type));
            printf(" flags           %016llx\n", (uint64_t)NATIVE(sh.sh_flags));
            printf(" addr            %016llx\n", (uint64_t)NATIVE(sh.sh_addr));
            printf(" offset          %016llx\n", (uint64_t)NATIVE(sh.sh_offset));
            printf(" size            %016llx\n", (uint64_t)NATIVE(sh.sh_size));
            printf(" link            %08x\n", NATIVE(sh.sh_link));
            printf(" info            %08x\n", NATIVE(sh.sh_info));
            printf(" addralign       %016llx\n", (uint64_t)NATIVE(sh.sh_addralign));
            printf(" entsize         %016llx\n", (uint64_t)NATIVE(sh.sh_entsize));
        }

        printf(" (now at %lx)\n", ftell(f));
    }

    return 0;
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
