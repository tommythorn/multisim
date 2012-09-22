#define N 160

int prime[N*N];
char cand[N*N];

void sieve(void)
{
    int i, ii, p = 0;

    for (i = 2, ii = 4; i < N*N; ii += 2*i+1, ++i)
        if (cand[i]) {
            prime[p++] = i;
            for (char *j = cand + ii; j < cand + N*N; j += i)
                *j = 0;
        }
}
