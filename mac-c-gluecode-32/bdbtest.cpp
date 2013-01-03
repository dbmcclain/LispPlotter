// test

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <memory.h>
#include <db.h>

extern "C"
{
  void *lwbdb_open(const char *filename);
  long lwbdb_get(void *db, char *key, long keylen, char *data, long datalen);
  void lwbdb_close(void *db);
  void lwbdb_err(long err, char *buf, long buflen);
};

void chkerr(long err)
{
  if(0 != err)
    {
      char buf[256];

      lwbdb_err(errno, buf, (long)sizeof(buf));
      std::cerr << buf << std::endl;
      exit(1);
    }
}

int main(int argc, char **argv)
{
  char *fname = argv[1];
  std::cout << "Opening: " << fname << std::endl;
  long ret;
#if 0
  DB *dbp;
  if(0 != (ret = db_create(&dbp, NULL, 0)))
    {
      chkerr(ret);
      exit(0);
    }
  else if(0 != (ret = dbp->open(dbp, NULL, fname, NULL, DB_BTREE, 0, 0)))
    {
      dbp->close(dbp, 0);
      chkerr(ret);
    }
#else
  void *dbp = (void*)lwbdb_open(fname);
  if(0 == dbp)
    {
      std::cerr << "Can't open database: " << fname << std::endl;
      exit(1);
    }
#endif
  char data[256];
  chkerr(lwbdb_get(dbp, "dog",3,data,sizeof(data)));
  std::cout << "dog -> " << data << std::endl;
  chkerr(lwbdb_get(dbp, "here",4,data,sizeof(data)));
  std::cout << "here -> " << data << std::endl;
  lwbdb_close(dbp);
  exit(0);
}
