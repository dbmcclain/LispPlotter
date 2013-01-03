// cforeign_array.h -- OCaml-style foreign arrays for C/C++
//
// DM/MCFA  01/99,08/04
// -------------------------------------------------------------------

#ifndef __FOREIGN_ARRAY__
#define __FOREIGN_ARRAY__

#ifdef WIN32
#ifdef LIBFA_EXPORTS
#define LIBFA_API __declspec(dllexport)
#else
#define LIBFA_API __declspec(dllimport)
#endif
#else
#define LIBFA_API
#endif

typedef signed char    schar;
typedef unsigned char  uchar;
typedef unsigned short ushort;
typedef unsigned long  ulong;

typedef signed char    sbyte;
typedef unsigned char  ubyte;

// ---------------------------------------
// Standard SciDS Array element types
//
enum {
	FOREIGN_BYTE,
	FOREIGN_UBYTE,
	FOREIGN_SHORT,
	FOREIGN_USHORT,
	FOREIGN_LONG,
	FOREIGN_ULONG,
	FOREIGN_FLOAT,
	FOREIGN_DOUBLE,
	FOREIGN_COMPLEX,
	FOREIGN_DCOMPLEX };
	
// --------------------------------------------------------------------
// Standard array descriptor -- allows disk based arrays to be self-describing.
// Also permits easy storage in a SciDS database, since the arrays
// describe themselves to the SciDS system. You don't have to do any
// fancy footwork. Just store with cscids_put_array() using a handle
// to the open database, a key string, and the foreign array itself.
//
struct foreign_array_header
{
	short  magic;			/* used for endian detection */
	char   type;			/* one of the FOREIGN_XXX types above */
	char   ndims;			/* No more than 7 dims */
	long   dim[7];		/* C-order dimensions */
};

// compute total number of elements in an array with these specs
extern long LIBFA_API array_total_length(int ndims, long *pdims);

// -------------------------------------------------
// Paired Complex formats... with apologies to all...
//
// These two classes (TComplex and TDComplex) are defined here
// simply to allow storage in the array.
//
// It is expected that you will typecast to a more computationally
// useful data type before actually using them...
//
struct TComplex
{
	float m_re;
	float m_im;
	
	TComplex(float re = 0.0, float im = 0.0)
		: m_re(re),m_im(im) {}
};

struct TDComplex
{
	double m_re;
	double m_im;
	
	TDComplex(double re = 0.0, double im = 0.0)
		: m_re(re),m_im(im) {}
};

// --------------------------------------------------------------------
struct dynamic_proxy;		/* you don't want to know... */

// ---------------------------------------------------------------------
// foreign_array -- multidimensional arrays of a variety of element types
// compatible with SciDS database Array formats.
// Corresponding deref classes permit convienient array element indexing.
//
class LIBFA_API foreign_array
{
public:
	foreign_array_header m_hdr;	/* standard array descriptor */
	void           *m_data;		/* ptr to actual array bytes */
	dynamic_proxy  *m_proxy;	/* allows overlay arrays */
	int             m_flags;	/* STATIC or MANAGED */
	void (*m_pfree)(void*);		/* function for special arena deletion */
	long            m_nel;		/* cached length */
	
	// foreign_array flags
	enum {
		STATIC  = 0x000,	/* arena owned by static area - not deleted */
		MANAGED = 0x100,	/* class will discard arena on deletion */
	};
	
	// constructing from a raw memory region if pdata is specified,
	// or to have it automatically allocated if pdata is NULL
	// If flags indicate MANAGED then the arena will be dealocated when
	// this object and all overlays are destroyed. Otherwise, STATIC
	// should be used to indicate that we are overlaying on static memory.
	foreign_array(int type, int ndims, long *pdims,
		void *pdata = 0,
		int flags   = MANAGED,
		void(*pfree)(void*) = 0);
	
	virtual ~foreign_array();
	
	foreign_array_header *header()
	{ return &m_hdr; }
	
	int element_type()		/* FOREIGN_BYTE .. FOREIGN_DCOMPLEX */
	{ return m_hdr.type; }
	
	int rank()			/* return number of dimensions */
	{ return m_hdr.ndims; }
	
	long dimension(int ix)	/* return nth dimension, starting from 0 */
	{ return m_hdr.dim[ix]; }
	
	long *dimensions()		/* return ptr to vector dimensions in C order */
	{ return m_hdr.dim; }
	
	long length()			/* total number of elements in array */
	{ return m_nel; }
	
	void *arena()			/* ptr to array bytes */
	{ return m_data; }
	
	long row_major_index(long index, ...);
	long row_major_index(long* pixs);
	
	// ref and set take a list of indices that must agree in number
	// with rank of array.
	long      iref(long ix, ...);	/* always returns a long */
	double    fref(long ix, ...);	/* always returns a double */
	TDComplex cref(long ix, ...);	/* always returns a TDComplex */
	
	void      aset(long newval,      long ix, ...);
	void      aset(double newval,    long ix, ...);
	void      aset(TDComplex newval, long ix, ...);
	
	long      row_major_iref(long ix); /* always returns a long */
	double    row_major_fref(long ix); /* always returns a double */
	TDComplex row_major_cref(long ix); /* always returns a TDComplex */
	
	void      row_major_aset(long newval,      long ix);
	void      row_major_aset(double newval,    long ix);
	void      row_major_aset(TDComplex newval, long ix);
	
	void fill(long off, long len, long val);
	void fill(long off, long len, double val);
	void fill(long off, long len, TDComplex val);
	
	foreign_array *create_overlay(long offset, long nel); /* shared overlay */
	foreign_array *copy();	/* construct a completely unshared copy */
	
	// for internal use... and to avoid namespace pollution
	static void bounds_violation();
	static void chkix(long index, long limit);
	static void chkoffset_len(long off, long len, long limit);
	static long element_size[];
};

// ---------------------------
class LIBFA_API foreign_array_of_schar : public foreign_array
{
 public:
  foreign_array_of_schar(int ndims, long *pdims,
			 schar* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_BYTE, ndims, pdims, pdata, flags, pfree)
    {}

  schar& operator[](long ix)
    {
      chkix(ix, length());
      return ((schar*)arena())[ix];
    }

  schar& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((schar*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_uchar : public foreign_array
{
 public:
  foreign_array_of_uchar(int ndims, long *pdims,
			 uchar* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_UBYTE, ndims, pdims, pdata, flags, pfree)
    {}

  uchar& operator[](long ix)
    {
      chkix(ix, length());
      return ((uchar*)arena())[ix];
    }

  uchar& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((uchar*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_short : public foreign_array
{
 public:
  foreign_array_of_short(int ndims, long *pdims,
			 short* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_SHORT, ndims, pdims, pdata, flags, pfree)
    {}

  short& operator[](long ix)
    {
      chkix(ix, length());
      return ((short*)arena())[ix];
    }

  short& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((short*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_ushort : public foreign_array
{
 public:
  foreign_array_of_ushort(int ndims, long *pdims,
			 ushort* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_USHORT, ndims, pdims, pdata, flags, pfree)
    {}

  ushort& operator[](long ix)
    {
      chkix(ix, length());
      return ((ushort*)arena())[ix];
    }

  ushort& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((ushort*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_long : public foreign_array
{
 public:
  foreign_array_of_long(int ndims, long *pdims,
			 long* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_LONG, ndims, pdims, pdata, flags, pfree)
    {}

  long& operator[](long ix)
    {
      chkix(ix, length());
      return ((long*)arena())[ix];
    }

  long& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((long*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_ulong : public foreign_array
{
 public:
  foreign_array_of_ulong(int ndims, long *pdims,
			 ulong* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_ULONG, ndims, pdims, pdata, flags, pfree)
    {}

  ulong& operator[](long ix)
    {
      chkix(ix, length());
      return ((ulong*)arena())[ix];
    }

  ulong& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((ulong*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_float : public foreign_array
{
 public:
  foreign_array_of_float(int ndims, long *pdims,
			 float* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_FLOAT, ndims, pdims, pdata, flags, pfree)
    {}

  float& operator[](long ix)
    {
      chkix(ix, length());
      return ((float*)arena())[ix];
    }

  float& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((float*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_double : public foreign_array
{
 public:
  foreign_array_of_double(int ndims, long *pdims,
			 double* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_DOUBLE, ndims, pdims, pdata, flags, pfree)
    {}

  double& operator[](long ix)
    {
      chkix(ix, length());
      return ((double*)arena())[ix];
    }

  double& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((double*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_complex : public foreign_array
{
 public:
  foreign_array_of_complex(int ndims, long *pdims,
			 TComplex* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_COMPLEX, ndims, pdims, pdata, flags, pfree)
    {}

  TComplex& operator[](long ix)
    {
      chkix(ix, length());
      return ((TComplex*)arena())[ix];
    }

  TComplex& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((TComplex*)arena())[rmix];
    }
};

// ---------------------------
class LIBFA_API foreign_array_of_dcomplex : public foreign_array
{
 public:
  foreign_array_of_dcomplex(int ndims, long *pdims,
			 TDComplex* pdata = 0,
			 int flags = MANAGED,
			 void (*pfree)(void*) = 0)
    : foreign_array(FOREIGN_BYTE, ndims, pdims, pdata, flags, pfree)
    {}

  TDComplex& operator[](long ix)
    {
      chkix(ix, length());
      return ((TDComplex*)arena())[ix];
    }

  TDComplex& operator()(long ix, ...)
    {
      long rmix = row_major_index(&ix);
      return ((TDComplex*)arena())[rmix];
    }
};

// ----------------------------------------------------------------------
// array_blit for moving data between foreign_arrays. The movement includes
// type-coercion of the elements if the two arrays are of different
// element types.
//
extern void LIBFA_API array_blit(foreign_array *src, long srcoff,
	foreign_array *dst, long dstoff, long nel);

#endif // __FOREIGN_ARRAY__

