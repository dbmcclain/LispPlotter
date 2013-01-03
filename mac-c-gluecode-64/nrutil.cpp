#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include "nrutil.h"
#include <memory.h>

// ---------------------------------------------------
char *nr_error_text = 0;

extern "C"
char *nr_get_last_error_text()
{
  return nr_error_text;
}

void failwith(char *error_text)
{
  nr_error_text = error_text;
  throw(error_text);
}

void nrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
  extern void nr_free_all();
  
  nr_free_all();
  failwith(error_text);
}

// -------------------------------------------------

#define NR_END 1
#define FREE_ARG char*

typedef struct heap_block
{
  struct heap_block *prev;
  long               data[1];
} heap_block;

static heap_block *heap = 0;

void *nr_unsafe_alloc(size_t nb)
{
  heap_block *p = (heap_block*)malloc(nb + sizeof(heap_block*));
  if(p)
    {
      p->prev = heap;
      heap = p;
      return &p->data;
    }
  else
    return 0;
}

void *nr_alloc(size_t nb)
{
  void *p = nr_unsafe_alloc(nb);
  if(p)
    memset(p, 0, nb);
  return p;
}

void nr_free(void *p)
{
  heap_block *q, *qf;
  long addr;

  addr = (long)p - sizeof(heap_block*);
  for(qf = 0, q = heap; q; qf = q, q = q->prev)
    {
      if((long)q == addr)
	{
	  if(qf)
	    qf->prev = q->prev;
	  else
	    heap = q->prev;
	  free(q);
	  return;
	}
    }
  // cost of failure is simple memory leak...
}

void nr_free_all()
{
  heap_block *p;

  while(p = heap)
    {
      heap = p->prev;
      free(p);
    }
}

float *vector(int nl, int nh)
/* allocate a float vector with subscript range v[nl..nh] */
{
  float *v;

  v=(float *)nr_unsafe_alloc((size_t) ((nh-nl+1+NR_END)*sizeof(float)));
  if (!v)
    nrerror("allocation failure in vector()");
  return v-nl+NR_END;
}

int *ivector(int nl, int nh)
/* allocate an int vector with subscript range v[nl..nh] */
{
  int *v;

  v=(int *)nr_unsafe_alloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)));
  if (!v)
    nrerror("allocation failure in ivector()");
  return v-nl+NR_END;
}

unsigned char *cvector(int nl, int nh)
/* allocate an unsigned char vector with subscript range v[nl..nh] */
{
  unsigned char *v;

  v=(unsigned char *)nr_unsafe_alloc((size_t) ((nh-nl+1+NR_END)*sizeof(unsigned char)));
  if (!v)
    nrerror("allocation failure in cvector()");
  return v-nl+NR_END;
}

unsigned int *lvector(int nl, int nh)
/* allocate an unsigned int vector with subscript range v[nl..nh] */
{
  unsigned int *v;

  v=(unsigned int *)nr_unsafe_alloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)));
  if (!v)
    nrerror("allocation failure in lvector()");
  return v-nl+NR_END;
}

double *dvector(int nl, int nh)
/* allocate a double vector with subscript range v[nl..nh] */
{
  double *v;

  v=(double *)nr_unsafe_alloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
  if (!v)
    nrerror("allocation failure in dvector()");
  return v-nl+NR_END;
}

float **matrix(int nrl, int nrh, int ncl, int nch)
/* allocate a float matrix with subscript range m[nrl..nrh][ncl..nch] */
{
  int i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
  float **m;

  /* allocate pointers to rows */
  m=(float **) nr_alloc((size_t)((nrow+NR_END)*sizeof(float*)));
  if (!m)
    nrerror("allocation failure 1 in matrix()");
  m += NR_END;
  m -= nrl;
  
  /* allocate rows and set pointers to them */
  m[nrl]=(float *) nr_unsafe_alloc((size_t)((nrow*ncol+NR_END)*sizeof(float)));
  if (!m[nrl])
    nrerror("allocation failure 2 in matrix()");
  m[nrl] += NR_END;
  m[nrl] -= ncl;

  for(i=nrl+1;i<=nrh;i++)
    m[i]=m[i-1]+ncol;

  /* return pointer to array of pointers to rows */
  return m;
}

double **dmatrix(int nrl, int nrh, int ncl, int nch)
/* allocate a double matrix with subscript range m[nrl..nrh][ncl..nch] */
{
  int i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
  double **m;

  /* allocate pointers to rows */
  m=(double **) nr_alloc((size_t)((nrow+NR_END)*sizeof(double*)));
  if (!m)
    nrerror("allocation failure 1 in matrix()");
  m += NR_END;
  m -= nrl;

  /* allocate rows and set pointers to them */
  m[nrl]=(double *) nr_unsafe_alloc((size_t)((nrow*ncol+NR_END)*sizeof(double)));
  if (!m[nrl])
    nrerror("allocation failure 2 in matrix()");
  m[nrl] += NR_END;
  m[nrl] -= ncl;

  for(i=nrl+1;i<=nrh;i++)
    m[i]=m[i-1]+ncol;

  /* return pointer to array of pointers to rows */
  return m;
}

int **imatrix(int nrl, int nrh, int ncl, int nch)
/* allocate a int matrix with subscript range m[nrl..nrh][ncl..nch] */
{
  int i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
  int **m;

  /* allocate pointers to rows */
  m=(int **) nr_alloc((size_t)((nrow+NR_END)*sizeof(int*)));
  if (!m)
    nrerror("allocation failure 1 in matrix()");
  m += NR_END;
  m -= nrl;


  /* allocate rows and set pointers to them */
  m[nrl]=(int *) nr_unsafe_alloc((size_t)((nrow*ncol+NR_END)*sizeof(int)));
  if (!m[nrl])
    nrerror("allocation failure 2 in matrix()");
  m[nrl] += NR_END;
  m[nrl] -= ncl;

  for(i=nrl+1;i<=nrh;i++)
    m[i]=m[i-1]+ncol;

  /* return pointer to array of pointers to rows */
  return m;
}

float **submatrix(float **a, int oldrl, int oldrh, int oldcl, int oldch,
		  int newrl, int newcl)
/* point a submatrix [newrl..][newcl..] to a[oldrl..oldrh][oldcl..oldch] */
{
  int i,j,nrow=oldrh-oldrl+1,ncol=oldcl-newcl;
  float **m;

  /* allocate array of pointers to rows */
  m=(float **) nr_alloc((size_t) ((nrow+NR_END)*sizeof(float*)));
  if (!m)
    nrerror("allocation failure in submatrix()");
  m += NR_END;
  m -= newrl;

  /* set pointers to rows */
  for(i=oldrl,j=newrl;i<=oldrh;i++,j++)
    m[j]=a[i]+ncol;

  /* return pointer to array of pointers to rows */
  return m;
}

float **convert_matrix(float *a, int nrl, int nrh, int ncl, int nch)
/* allocate a float matrix m[nrl..nrh][ncl..nch] that points to the matrix
   declared in the standard C manner as a[nrow][ncol], where nrow=nrh-nrl+1
   and ncol=nch-ncl+1. The routine should be called with the address
   &a[0][0] as the first argument. */
{
  int i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1;
  float **m;

  /* allocate pointers to rows */
  m=(float **) nr_alloc((size_t) ((nrow+NR_END)*sizeof(float*)));
  if (!m)
    nrerror("allocation failure in convert_matrix()");
  m += NR_END;
  m -= nrl;

  /* set pointers to rows */
  m[nrl]=a-ncl;
  for(i=1,j=nrl+1;i<nrow;i++,j++)
    m[j]=m[j-1]+ncol;
  /* return pointer to array of pointers to rows */
  return m;
}

double **convert_dmatrix(double *a, int nrl, int nrh, int ncl, int nch)
/* allocate a float matrix m[nrl..nrh][ncl..nch] that points to the matrix
   declared in the standard C manner as a[nrow][ncol], where nrow=nrh-nrl+1
   and ncol=nch-ncl+1. The routine should be called with the address
   &a[0][0] as the first argument. */
{
  int i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1;
  double **m;

  /* allocate pointers to rows */
  m=(double **) nr_alloc((size_t) ((nrow+NR_END)*sizeof(double*)));
  if (!m)
    nrerror("allocation failure in convert_dmatrix()");
  m += NR_END;
  m -= nrl;

  /* set pointers to rows */
  m[nrl]=a-ncl;
  for(i=1,j=nrl+1;i<nrow;i++,j++)
    m[j]=m[j-1]+ncol;
  /* return pointer to array of pointers to rows */
  return m;
}

float ***f3tensor(int nrl, int nrh, int ncl, int nch, int ndl, int ndh)
/* allocate a float 3tensor with range t[nrl..nrh][ncl..nch][ndl..ndh] */
{
  int i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1,ndep=ndh-ndl+1;
  float ***t;

  /* allocate pointers to pointers to rows */
  t=(float ***) nr_alloc((size_t)((nrow+NR_END)*sizeof(float**)));
  if (!t)
    nrerror("allocation failure 1 in f3tensor()");
  t += NR_END;
  t -= nrl;

  /* allocate pointers to rows and set pointers to them */
  t[nrl]=(float **) nr_alloc((size_t)((nrow*ncol+NR_END)*sizeof(float*)));
  if (!t[nrl])
    nrerror("allocation failure 2 in f3tensor()");
  t[nrl] += NR_END;
  t[nrl] -= ncl;

  /* allocate rows and set pointers to them */
  t[nrl][ncl]=(float *) nr_unsafe_alloc((size_t)((nrow*ncol*ndep+NR_END)*sizeof(float)));
  if (!t[nrl][ncl])
    nrerror("allocation failure 3 in f3tensor()");
  t[nrl][ncl] += NR_END;
  t[nrl][ncl] -= ndl;

  for(j=ncl+1;j<=nch;j++)
    t[nrl][j]=t[nrl][j-1]+ndep;
  for(i=nrl+1;i<=nrh;i++) {
    t[i]=t[i-1]+ncol;
    t[i][ncl]=t[i-1][ncl]+ncol*ndep;
    for(j=ncl+1;j<=nch;j++)
      t[i][j]=t[i][j-1]+ndep;
  }

  /* return pointer to array of pointers to rows */
  return t;
}

void free_vector(float *v, int nl, int nh)
/* free a float vector allocated with vector() */
{
  if(v)
    nr_free((FREE_ARG) (v+nl-NR_END));
}

void free_ivector(int *v, int nl, int nh)
/* free an int vector allocated with ivector() */
{
  if(v)
    nr_free((FREE_ARG) (v+nl-NR_END));
}

void free_cvector(unsigned char *v, int nl, int nh)
/* free an unsigned char vector allocated with cvector() */
{
  if(v)
    nr_free((FREE_ARG) (v+nl-NR_END));
}

void free_lvector(unsigned int *v, int nl, int nh)
/* free an unsigned int vector allocated with lvector() */
{
  if(v)
    nr_free((FREE_ARG) (v+nl-NR_END));
}

void free_dvector(double *v, int nl, int nh)
/* free a double vector allocated with dvector() */
{
  if(v)
    nr_free((FREE_ARG) (v+nl-NR_END));
}

void free_matrix(float **m, int nrl, int nrh, int ncl, int nch)
/* free a float matrix allocated by matrix() */
{
  if(m)
    {
      if(m[nrl])
	nr_free((FREE_ARG) (m[nrl]+ncl-NR_END));
      nr_free((FREE_ARG) (m+nrl-NR_END));
    }
}

void free_dmatrix(double **m, int nrl, int nrh, int ncl, int nch)
/* free a double matrix allocated by dmatrix() */
{
  if(m)
    {
      if(m[nrl])
	nr_free((FREE_ARG) (m[nrl]+ncl-NR_END));
      nr_free((FREE_ARG) (m+nrl-NR_END));
    }
}

void free_imatrix(int **m, int nrl, int nrh, int ncl, int nch)
/* free an int matrix allocated by imatrix() */
{
  if(m)
    {
      if(m[nrl])
	nr_free((FREE_ARG) (m[nrl]+ncl-NR_END));
      nr_free((FREE_ARG) (m+nrl-NR_END));
    }
}

void free_submatrix(float **b, int nrl, int nrh, int ncl, int nch)
/* free a submatrix allocated by submatrix() */
{
  if(b)
    nr_free((FREE_ARG) (b+nrl-NR_END));
}

void free_convert_matrix(float **b, int nrl, int nrh, int ncl, int nch)
/* free a matrix allocated by convert_matrix() */
{
  if(b)
    nr_free((FREE_ARG) (b+nrl-NR_END));
}

void free_convert_dmatrix(double **b, int nrl, int nrh, int ncl, int nch)
/* free a matrix allocated by convert_matrix() */
{
  if(b)
    nr_free((FREE_ARG) (b+nrl-NR_END));
}

void free_f3tensor(float ***t, int nrl, int nrh, int ncl, int nch,
		   int ndl, int ndh)
/* free a float f3tensor allocated by f3tensor() */
{
  if(t)
    {
      if(t[nrl])
	{
	  if(t[nrl][ncl])
	    nr_free((FREE_ARG) (t[nrl][ncl]+ndl-NR_END));
	  nr_free((FREE_ARG) (t[nrl]+ncl-NR_END));
	}
      nr_free((FREE_ARG) (t+nrl-NR_END));
    }
}

// ------------------------------------------------------------------

double **dpvector(int nl, int nh)
/* allocate a double pointers-vector with subscript range v[nl..nh] */
{
  double **v;

  v=(double **)nr_unsafe_alloc((size_t) ((nh-nl+1+NR_END)*sizeof(double*)));
  if (!v)
    nrerror("allocation failure in dpvector()");
  return v-nl+NR_END;
}

void free_dpvector(double **v, int nl, int nh)
/* free a double pointers-vector allocated with dpvector() */
{
  if(v)
    nr_free((FREE_ARG) (v+nl-NR_END));
}

