#include <stdio.h>
#include <math.h>

int main(int c, char **v)
{
    int i;

    for (i = -180; i <= 180; i += 5) {
        float r = M_PI * i / 180;
        printf("(%7.2f, %7.2f)\n", 100.0 * cos(r), 100.0 * sin(r));
    }

    return 0;
}
