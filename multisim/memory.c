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
#include <stdlib.h>

/*
  The simulation space address to physical address translation is a
  key operation, so it has been optimized slightly.

  First off, we can't implement a 1-1 mapping as the virtual address
  space on the host is smaller than the target (both are 32-bit), so
  we have to live with some sort of segmentation.  An obvious first
  approach is to divide the simulation space into 2^S segments with a
  mapping from segments to physical segments:

  N = 32 - S
  segment(x)   = x >> N
  offset(x)    = x & ((1 << N) - 1)
  Inv: for all x, (segment(x) << N) + offset(x) == x

  addr2phys(x) = memory_segment[segment(x)] + offset(x)

  As we can't map all segments, we represent the "holes" as areas
  laying outside the segment space:

  addr_mapped(x) = offset(x) < memory_segment_size[segment(x)]

  OPTIMIZATION

  Note, the offset(x) can also be written

  offset(x) = x - (segment(x) << N)

  thus

  addr2phys(x) = memory_segment[segment(x)] + x - (segment(x) << N)

  or by arranging for memory_segment'[s] = memory_segment[s] - (s << N)

  addr2phys(x) = memory_segment'[segment(x)] + x

  BUT we don't do it like that below, for clairity.
*/

#define MEMORY_SEGMENTBITS 4
#define MEMORY_OFFSETBITS (32 - MEMORY_SEGMENTBITS)
#define MEMORY_NSEGMENT (1 << MEMORY_SEGMENTBITS)

struct memory_st {
    void    *segment[MEMORY_NSEGMENT];
    unsigned segment_size[MEMORY_NSEGMENT];
    bool     endian_is_big;
};

#define memory_segment(m, x)     (((unsigned)(x)) >> MEMORY_OFFSETBITS)
#define memory_seg2virt(m, s)    (((unsigned)(s)) << MEMORY_OFFSETBITS)
#define memory_offset(m, x)      (((unsigned)(x)) & ((1 << MEMORY_OFFSETBITS) - 1))
#define memory_addr2phys(m, x)   ((m)->segment[memory_segment(m, x)] + memory_offset(m, x))
#define memory_addr_mapped(m, x) (memory_offset(m, x) < (m)->segment_size[memory_segment(m, x)])

void
memory_ensure_mapped_range(memory_t *m, uint32_t addr, size_t len)
{
    unsigned seg;

    // Split it up
    while (memory_segment(m, addr) != memory_segment(m, addr + len - 1)) {
        memory_ensure_mapped_range(m, addr, (1 << MEMORY_OFFSETBITS) - memory_offset(m, addr));
        addr += (1 << MEMORY_OFFSETBITS) - memory_offset(m, addr);
        assert(len >= (1 << MEMORY_OFFSETBITS) - memory_offset(m, addr));
        len  -= (1 << MEMORY_OFFSETBITS) - memory_offset(m, addr);
    }

    seg = memory_segment(m, addr);
    if (!memory_addr_mapped(m, addr + len - 1)) {
        m->segment_size[seg] = 1 + memory_offset(m, addr + len - 1);
        m->segment[seg] = realloc(m->segment[seg], m->segment_size[seg]);

        // Make sure it's good
        *(char*)(m->segment[seg] + m->segment_size[seg] - 1) = 0;
    }

    assert(memory_addr_mapped(m, addr + len - 1));
}

void *memory_physical(memory_t *m, uint32_t addr, size_t len)
{
    void *phys = memory_addr2phys(m, addr);

    if (!memory_addr_mapped(m, addr))
        return NULL;

    if (!memory_addr_mapped(m, addr + len - 1))
        return NULL;

    if (memory_segment(m, addr) != memory_segment(m, addr + len - 1))
        return NULL;

    return phys;
}

memory_t *memory_create()
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


// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
