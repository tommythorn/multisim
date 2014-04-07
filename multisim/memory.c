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

#include "memory.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <arpa/inet.h>

/*
  The simulation space address to physical address translation is a
  key operation.

  We simply use a list of translations.
*/

#define MEMORY_NSEGMENT 8

struct entry {
    void *phys;
    uint64_t start, end;  // [start; end)
};

struct memory_st {
    struct entry segment[MEMORY_NSEGMENT];
    int segments;
    bool endian_is_big;
};

struct entry *memory_lookup(memory_t *m, uint64_t addr)
{
    for (int i = 0; i < m->segments; ++i)
        if (m->segment[i].start <= addr && addr < m->segment[i].end)
            return m->segment + i;

    return NULL;
}

void *memory_physical(memory_t *m, uint64_t addr, uint64_t size)
{
    struct entry *e = memory_lookup(m, addr);

    assert(e == memory_lookup(m, addr + size - 1));

    if (e)
        return e->phys + (addr - e->start);
    else
        return NULL;
}

memory_t *memory_create(void)
{
    return calloc(sizeof (memory_t), 1);
}

void memory_destroy(memory_t *m)
{
    free(m);
}

void memory_set_endian(memory_t *m, bool bigendian)
{
    m->endian_is_big = bigendian;
}

uint64_t memory_endian_fix64(memory_t *m, uint64_t v)
{
    assert(!m->endian_is_big);
    return v;
}

uint32_t memory_endian_fix32(memory_t *m, uint32_t v)
{
    return m->endian_is_big ? htonl(v) : v;
}

uint16_t memory_endian_fix16(memory_t *m, uint16_t v)
{
    return m->endian_is_big ? htons(v) : v;
}

static struct entry
mk_entry(uint64_t start, uint64_t end)
{
    return (struct entry) {calloc(end - start, 1), start, end};
}


static void test(memory_t *m);

/*
 * There is little doubt that this could be implemented more elegantly
 * and more efficiently.
 */
void
memory_ensure_mapped_range(memory_t *m, uint64_t start, uint64_t end)
{
    static int test_it = 0;

    if (test_it) {
        test_it = 0;
        test(m);
    }

    if (end <= start)
        return;

    struct entry *s0 = memory_lookup(m, start - 1);
    struct entry *s1 = memory_lookup(m, end   + 1);

    if (!s0 && !s1) {
        // new entry
        m->segment[m->segments++] = mk_entry(start, end);
        return;
    }

    if (s0 == s1)
        return;

    if (s0) {
        // start in [s0.start; s0.end) => grow s0 to [s0.start; end)
        if (s1) {
            // Merge 2nd
            struct entry e = mk_entry(s0->start, s1->end);
            memcpy(e.phys, s0->phys, s0->end - s0->start);
            memcpy(e.phys + (s1->start - s0->start),
                   s1->phys, s1->end - s1->start);
            free(s0->phys);
            free(s1->phys);
            *s1 = m->segment[--m->segments];
            *s0 = e;
            return;
        }

        struct entry e = mk_entry(start = s0->start, end);
        memcpy(e.phys, s0->phys, s0->end - start);
        free(s0->phys);
        *s0 = e;

        return;
    }

    if (s1) {
        // end in [s1.start; s1.end) => grow s1 to [start; s1.end)
        struct entry e = mk_entry(start, end = s1->end);
        memcpy(e.phys + (s1->start - start),
               s1->phys,
               end - s1->start);
        free(s1->phys);
        *s1 = e;
        return;
    }

    assert(s0 && s1 && s0 != s1);

    // Grow and merge two segments
    struct entry e = mk_entry(s0->start, s1->end);

    memcpy(e.phys, s0->phys, s0->end - s0->start);
    memcpy(e.phys + (s1->start - s0->start),
           s1->phys, s1->end - s1->start);

    free(s0->phys);
    *s0 = e;
    free(s1->phys);
    if (m->segments > 2)
        *s1 = m->segment[--m->segments];
}

static void
list_segments(memory_t *m)
{
    printf("Segments\n");
    for (int i = 0; i < m->segments; ++i)
        printf("  [%016"PRIx64"; %016"PRIx64")\n",
               m->segment[i].start,
               m->segment[i].end);
}

static void
test(memory_t *m)
{
    static uint64_t starts[] = {
        0x1000, 0x4000, 0x5000, 0x2000, 0x3000, 0
    };

    for (int i = 0; starts[i]; ++i) {
        printf("Add segment [%016"PRIx64"; %016"PRIx64")\n",
               starts[i], starts[i] + 0x1000);

        memory_ensure_mapped_range(m, starts[i], starts[i] + 0x1000);
        list_segments(m);
    }

    exit(0);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
