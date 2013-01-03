
#include <stdio.h>
#include <fenv.h>

int main(long ac, char **av)
{
   printf("sizeof(fenv_t) = %d\n", sizeof(fenv_t));
   fenv_t sav;
   printf("ans = %d\n", fegetenv(&sav));
   printf("ans = %d\n", fesetenv(&sav));
}
