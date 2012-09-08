/*
 * Copyright (C) 2012 Tommy Thorn
 *
 * generic simulated memory
 */

#ifndef _MEMORY_H
#define _MEMORY_H 1

#include <stdint.h>
#include <sys/types.h>

typedef struct memory_st memory_t;

memory_t *memory_create(void);
void memory_ensure_mapped_range(memory_t *m, uint32_t addr, size_t len);
void *memory_physical(memory_t *m, uint32_t addr, size_t len);
void memory_destroy(memory_t *);

#endif
