#ifndef _SAFE_ALLOC_H
#define _SAFE_ALLOC_H 1

#include <stdlib.h>

static inline void *safe_malloc(size_t size) {
    void *p = malloc(size);
    assert(p);
    return p; }

static inline void *safe_calloc(size_t nmemb, size_t size) {
    void *p = calloc(nmemb, size);
    assert(p);
    return p; }

static inline void *safe_realloc(void *ptr, size_t size) {
    void *p = realloc(ptr, size);
    assert(p);
    return p; }

#endif
