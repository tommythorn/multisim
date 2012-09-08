#ifndef _LOADELF_H_
#define _LOADELF_H_ 1

#include "memory.h"
#include <stdbool.h>

#define MAX_ELF_SECTIONS 32

typedef struct elf_info_st {
    bool     endian_is_big;
    uint64_t program_entry;
    unsigned nsections;
    uint64_t section_start[MAX_ELF_SECTIONS];
    uint64_t   section_size[MAX_ELF_SECTIONS];

    unsigned text_segments;
    uint64_t text_start;
    uint64_t text_size;
} elf_info_t;

int loadelf(memory_t *m, char *name, elf_info_t *program_entry);

#endif
