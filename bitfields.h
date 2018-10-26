/*
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

#ifndef _BITFIELDS_H
#define _BITFIELDS_H

#define BF_LO(bf)          (0 ? bf)
#define BF_HI(bf)          (1 ? bf)
#define BF_SIZE(bf)        (BF_HI(bf) - BF_LO(bf) + 1)

#define BF_MASK(bf)        (~0U >> (32 - BF_SIZE(bf)))
#define BF_PACK(v, bf)     (((uint32_t)(v) & BF_MASK(bf)) << BF_LO(bf))
#define BF_GET(w, bf)      (((uint32_t)(w) >> BF_LO(bf)) & BF_MASK(bf))
#define BF_SET(w, bf, v)   ((w) = (((w) & ~BF_PACK(~0U, bf)) | BF_PACK(v, bf)))

#define BF_MASK64(bf)      (~0ULL >> (64 - BF_SIZE(bf)))
#define BF_PACK64(v, bf)   (((uint64_t)(v) & BF_MASK64(bf)) << BF_LO(bf))
#define BF_GET64(w, bf)    (((uint64_t)(w) >> BF_LO(bf)) & BF_MASK64(bf))
#define BF_SET64(w, bf, v) ((w) = (((w) & ~BF_PACK64(~0ULL, bf)) | BF_PACK64(v, bf)))

#endif
