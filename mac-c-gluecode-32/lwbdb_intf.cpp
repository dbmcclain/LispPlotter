// LispBDB_Intf.cpp -- Interface for Berkeley DB and Lisworks
// DM/MCFA  06/04
//

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <db.h>
#include <cscids.h>

// ---------------------------------------

extern "C"
long lwbdb_open(const char *file, DB **dbp)
{
  DB *pdb;
  long ret = db_create(&pdb, NULL, 0);
  if(0 == ret)
    {
      ret = pdb->open(pdb, NULL, file, NULL,
			 DB_BTREE, DB_CREATE, 0666);
      if(0 != ret)
	pdb->close(pdb, 0);
      else
	*dbp = pdb;
    }
  return ret;
}

extern "C"
long lwbdb_close(DB *aDb)
{
  return aDb->close(aDb, 0);
}

extern "C"
long lwbdb_get(DB *aDb, void *key, long keylen,
               void **data, long *datalen)
{
  DBT k, d;

  memset(&k, 0, sizeof(k));
  memset(&d, 0, sizeof(d));
  k.data = key;
  k.size = keylen;
  long ret = aDb->get(aDb, NULL, &k, &d, 0);
  *datalen = d.size;
  *data    = d.data;
  return ret;
}

extern "C"
long lwbdb_put(DB *aDb, void *key, long keylen,
               void *data, long datalen)
{
  DBT k, d;

  memset(&k, 0, sizeof(k));
  memset(&d, 0, sizeof(d));
  k.data = key;;
  k.size = keylen;
  d.data = data;
  d.size = datalen;

  return aDb->put(aDb,NULL,&k,&d,0);
}

extern "C"
long lwbdb_del(DB *aDb, void *key, long keylen)
{
  DBT k;
  memset(&k, 0, sizeof(k));
  k.data = key;
  k.size = keylen;
  return aDb->del(aDb,NULL,&k,0);
}

extern "C"
long lwbdb_sync(DB *aDb)
{
  return aDb->sync(aDb, 0);
}

extern "C"
long lwbdb_truncate(DB *aDb)
{
  u_int32_t count;

  return aDb->truncate(aDb,NULL,&count,0);
}

extern "C"
void lwbdb_err(long errno, const char **ans, long *anslen)
{
  const char *msg = db_strerror(errno);
  *anslen = strlen(msg);
  *ans = msg;
}

extern "C"
long lwbdb_cursor(DB *aDb, DBC **pCurs)
{
  return aDb->cursor(aDb, NULL, pCurs, 0);
}

extern "C"
long lwbdb_c_get(DBC *pCurs, long flag,
		 void *srchkey, long srchkeylen,
		 void **retkey, long *retkeylen,
		 void **data, long *datalen)
{
  DBT k,d;
  memset(&k,0,sizeof(k));
  memset(&d,0,sizeof(d));

  switch(flag)
    {
    case DB_SET:
    case DB_SET_RANGE:
      k.size = srchkeylen;
      k.data = srchkey;
      break;

    default:
      break;
    }
  
  // flag: DB_FIRST, DB_LAST, DB_CURRENT, DB_NEXT, DB_PREV,
  //       DB_SET, DB_SET_RANGE
  long ret = pCurs->c_get(pCurs, &k, &d, flag);
  if(0 == ret)
    {
      *retkeylen = k.size;
      *retkey    = k.data;
      *datalen   = d.size;
      *data      = d.data;
    }
  return ret;
}

extern "C"
long lwbdb_c_close(DBC *pCurs)
{
  return pCurs->c_close(pCurs);
}

// ----------------------------------------------
// Data marshaling
// We use network byte ordering (big endian?)
// ----------------------------------------------
// Decoding...
//

#include <arpa/inet.h>

extern "C"
long lwbdb_decode_int(char *str, long off)
{
  union {
    char c[4];
    long l;
  } v;

  memcpy(v.c, str+off, sizeof(long));
  return ntohl(v.l);
}

extern "C"
double lwbdb_decode_float(char *str, long off)
{
  union {
    char c[8];
    long l[2];
    double d;
  } v;

  memcpy(v.c, str+off, sizeof(double));
  if(0x100 == htons(1))
    {
      // we are swapped from network order
      long tmp = ntohl(v.l[0]);
      v.l[0] = ntohl(v.l[1]);
      v.l[1] = tmp;
    }
  return v.d;
}

// ----------------------------------------------
// Data marshaling
// We use network byte ordering (big endian?)
// ----------------------------------------------
// Encoding...
//
extern "C"
void lwbdb_encode_int(char *buf, long off, long val)
{
  union {
    char  c[4];
    long  l;
  } v;

  v.l = htonl(val);
  memcpy(buf + off, v.c, sizeof(long));
}

extern "C"
void lwbdb_encode_float(char *buf, long off, double val)
{
  union {
    char   c[8];
    long   l[2];
    double d;
  } v;

  v.d = val;
  if(0x100 == htons(1))
    {
      // we are swapped from network order
      long tmp = htonl(v.l[0]);
      v.l[0] = htonl(v.l[1]);
      v.l[1] = tmp;
    }
  memcpy(buf + off, v.c, sizeof(double));
}

// -----------------------------------------------
extern "C"
long lwbdb_get_array(DB *db, char *key, void **pans, const char **errmsg)
{
  long ans = 0;
  
  try {
    *pans = cscids_get_array(db, key);
  }
  catch(char const* msg)
    {
      *errmsg = msg;
      ans = 1;
    }
  return ans;
}

extern "C"
long lwbdb_put_array(DB *db, char *key,
		     long rank, long *dims,
		     long nel,  double *data, const char **errmsg)
{
  long ans = 0;
  foreign_array *p = 0;
  
  try {
    p = new foreign_array(FOREIGN_DOUBLE,
			  rank,
			  dims);
    memcpy(p->arena(), data, nel * sizeof(double));
    cscids_put_array(db, key, p);
  }
  catch(char const* msg)
    {
      *errmsg = msg;
      ans = 1;
    }
  if(p)
    delete p;
  return ans;
}

// -----------------------------------------------

extern "C"
long lwfa_get_type(foreign_array *p)
{
  return p->element_type();
}

extern "C"
long lwfa_get_rank(foreign_array *p)
{
  return p->rank();
}

extern "C"
long lwfa_get_dimension(foreign_array *p, long dim)
{
  return p->dimension(dim);
}

extern "C"
void lwfa_get_dimensions(foreign_array *p, long *dims)
{
  int ndims = p->rank();
  long *pdims = p->dimensions();
  while(--ndims >= 0)
    dims[ndims] = pdims[ndims];
}

extern "C"
long lwfa_get_array_length(foreign_array *p)
{
  return p->length();
}

extern "C"
void lwfa_f_to_lisp(foreign_array *p, double *pd)
{
  switch(p->element_type())
    {
    case FOREIGN_DCOMPLEX:
      memcpy(pd, p->arena(), p->length() * 2 * sizeof(double));
      break;
      
    case FOREIGN_DOUBLE:
      memcpy(pd, p->arena(), p->length() * sizeof(double));
      break;

    case FOREIGN_COMPLEX:
      {
	foreign_array *px = new foreign_array(FOREIGN_DCOMPLEX,
					      p->rank(),
					      p->dimensions());
	array_blit(p, 0, px, 0, px->length());
	memcpy(pd, px->arena(), px->length() * 2 * sizeof(double));
      }
      break;

    default:
      {
	foreign_array *px = new foreign_array(FOREIGN_DCOMPLEX,
					      p->rank(),
					      p->dimensions());
	array_blit(p, 0, px, 0, px->length());
	memcpy(pd, px->arena(), px->length() * sizeof(double));
      }
      break;
    }
}

extern "C"
void lwfa_release(foreign_array *p)
{
  delete p;
}

