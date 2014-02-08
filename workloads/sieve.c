#ifdef TEST
#include <stdio.h>
void sieve(void);
int main() { sieve(); }
#endif

#define N 256


static int prime[N] = { 0 };
static char cand[N] = { 0 };

void sieve(void)
{
    int i, ii;
    int *p = prime;

    for (i = 2, ii = 4; i < N; ii += 2*i+1, ++i)
        if (!cand[i]) {
            *p++ = i;
#ifdef TEST
            printf(" %d", i);
#endif
            for (char *j = cand + ii; j < cand + N; j += i)
                *j = 1;
        }
}
