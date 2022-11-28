/*@@@

The C implementation of Fizzbuzz uses an array of string formats
used by `sprintf`{.c} to produce `"fizz"`, `"buzz"`, `"fizzbuzz"`
or the function argument.

The array index is a 2-bit integer, each bit being the divisilibity
of the argument by 3 or 5.

```{.c include="fizzbuzz.c" pattern="[c]onst.-%b{}"}
```
@@@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *fizzbuzz(int i, char *s)
{
    static const char *fmt[] = {
        [0|(0<<1)] = "%d",
        [1|(0<<1)] = "fizz",
        [0|(1<<1)] = "buzz",
        [1|(1<<1)] = "fizzbuzz",
    };
    const int fizz = (i%3 == 0) << 0;
    const int buzz = (i%5 == 0) << 1;
    sprintf(s, fmt[fizz|buzz], i);
    return s;
}

int main(int argc, const char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "argument expected");
        exit(1);
    }
    const int n = atoi(argv[1]);
    char s[64];
    for (int i = 1; i <= n; i++)
    {
        printf("%d\t%s\n", i, fizzbuzz(i, s));
    }
    return EXIT_SUCCESS;
}
