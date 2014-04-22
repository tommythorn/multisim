#ifndef _BITFIELDS_H
#define _BITFIELDS_H

#define BF_LO(bf)          (0 ? bf)
#define BF_HI(bf)          (1 ? bf)
#define BF_SIZE(bf)        (BF_HI(bf) - BF_LO(bf) + 1)

#define BF_MASK(bf)        (~0U >> (32 - BF_SIZE(bf)))
#define BF_PACK(bf, v)     (((uint32_t)(v) & BF_MASK(bf)) << BF_LO(bf))
#define BF_GET(bf, w)      (((uint32_t)(w) >> BF_LO(bf)) & BF_MASK(bf))
#define BF_SET(w, bf, v)   (((w) & ~BF_PACK(bf, ~0U)) | BF_PACK(bf, v))

#define BF_MASK64(bf)      (~0ULL >> (64 - BF_SIZE(bf)))
#define BF_PACK64(bf, v)   (((uint64_t)(v) & BF_MASK64(bf)) << BF_LO(bf))
#define BF_GET64(bf, w)    (((uint64_t)(w) >> BF_LO(bf)) & BF_MASK64(bf))
#define BF_SET64(w, bf, v) (((w) & ~BF_PACK64(bf, ~0ULL)) | BF_PACK64(bf, v))

#endif
