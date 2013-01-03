// cbdb.h -- C/C++ Interface to Berkeley DB
//
// DM/MCFA  08/04
//

#ifndef __CBDB_H__
#define __CBDB_H__

#include <db.h>

// ----------------------------------------------------------
// Basic (Thread-Safe) Berkeley DB Interface with Error Checking
//
// All keys are assumed to be normal C strings with no embedded nulls.
// Data is returned as char* pointers but these may have embedded nulls
// if the data is really of binary character. When long *pdatalen args are
// provided and not NULL, then the length of the data is returned in
// what they point to.
//
/* --- Open Flags --- from Berkeley DB (db.h) --- */
/* DB_RDONLY */
/* DB_CREATE */
/* DB_EXCL   */  /* only makes sense with DB_CREATE */

extern "C" {
  extern DB   *cbdb_open(char *filename, int flags, int perms);
  extern void  cbdb_close(DB *pdb);
  extern void  cbdb_get(DB *pdb, char *key, long keylen,
			char **pdata, long *pdatalen);
  extern void  cbdb_get_partial(DB *pdb, char *key, long keylen,
				char **pdata, long *pdatalen,
				long off, long len);
  extern void  cbdb_put(DB *pdb, char *key, long keylen,
			char *data, long datalen);
  extern void  cbdb_put_partial(DB *pdb, char *key, long keylen,
				char *data, long datalen, long off);
  extern void  cbdb_add(DB *pdb, char *key, long keylen,
			char *data, long datalen);
  extern void  cbdb_del(DB *pdb, char *key, long keylen);
  extern void  cbdb_trunc(DB *pdb);
  extern void  cbdb_sync(DB *pdb);

  /* Cursor flags for cbdb_cget() -- from db.h */
  /* DB_FIRST   */
  /* DB_CURRENT */
  /* DB_LAST    */
  /* DB_NEXT    */
  /* DB_PREV    */
  extern DBC  *cbdb_make_cursor(DB *pdb);
  extern void  cbdb_cget(DBC *pcurs, int flag, char **pkey, long *pkeylen,
			 char **pdata, long *pdatalen);
  extern void  cbdb_cfind(DBC *pcurs, char *partial_key, long partial_key_len,
			  char **pkey, long *pkeylen,
			  char **pdata, long *pdatalen);
  extern void  cbdb_cclose(DBC *pcurs);

  extern void  cbdb_free(void *p);
};

#endif // __CBDB_H__

// -- end of cbdb.h -- //
