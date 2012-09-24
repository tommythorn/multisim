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
 * Copyright (C) 2012 Tommy Thorn
 *
 * generic simulated memory
 */

#ifndef _MEMORY_H
#define _MEMORY_H 1

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>

typedef struct memory_st memory_t;

memory_t *memory_create(void);
void memory_ensure_mapped_range(memory_t *m, uint32_t addr, size_t len);
void *memory_physical(memory_t *m, uint32_t addr, size_t len);
void memory_destroy(memory_t *);

void memory_set_endian(memory_t *m, bool bigendian);
uint64_t memory_endian_fix64(memory_t *m, uint64_t v);
uint32_t memory_endian_fix32(memory_t *m, uint32_t v);
uint16_t memory_endian_fix16(memory_t *m, uint16_t v);

#endif

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
