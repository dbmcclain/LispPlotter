
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>


extern "C"
off_t fd_get_file_size(int fd)
{
  struct stat statInfo;
  fstat(fd, &statInfo);
  return statInfo.st_size;
}

#if 0
// test to be certain that LW understands long-long.
// Yes it does! DM 11/08 LWM 5.1.1
#include <stdio.h>

extern "C"
off_t fd_test_long_long(off_t x)
{
  fprintf(stderr, "sizeof(off_t) = %d\n", sizeof(off_t));
  fprintf(stderr, "x = %llx\n", x);
  return x;
}
#endif

