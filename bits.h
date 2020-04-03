/*
 * Generic bit manipulation
 *
 * Copyright (C) 2020 Tommy Thorn
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

#ifndef _BITS_
#define _BITS_ 1

#include <stdint.h>

typedef struct bits_s {
    uint32_t val, size;
} bits_t;

static inline bits_t bitsfromuint32(uint32_t val, uint32_t hi, uint32_t lo)
{
    uint32_t size = hi - lo + 1;
    return (bits_t) { val >> lo & ((1 << size) - 1), size };
}

static inline uint32_t uint32frombits(bits_t b)
{
    return b.val;
}

static inline bits_t concatenamebits(bits_t a, bits_t b)
{
    return (bits_t) { a.val << b.size | b.val, a.size + b.size };
}

// An n-string of ones if a is non-zero, and 0 otherwise
static inline bits_t fillbits(uint32_t n, bits_t a)
{
    return bitsfromuint32(uint32frombits(a) ? (1 << n) - 1 : 0, n-1, 0);
}

static inline bits_t mux(bits_t c, bits_t a, bits_t b)
{
    assert(a.size == b.size);
    assert(c.size == 1);

    return (bits_t) { c.val ? a.val : b.val, a.size };
}

static inline bits_t orR(bits_t v)
{
    return (bits_t) { !! v.val, 1 };
}

static inline bits_t or(bits_t a, bits_t b)
{
    return (bits_t) { a.val | b.val, a.size < b.size ? b.size : a.size };
}

#endif
