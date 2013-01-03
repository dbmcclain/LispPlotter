// fftglue.cpp -- CAML Interface to FFAST 2-D FFT Routines
//
// DM/MCFA  12/98
// ------------------------------------------------------------
// Create a dynamic lib with:
//
// gcc -dynamiclib -o libLispFFT.dylib fft_glue.cpp \
//      -lstdc++ -lfftw3 -framework VecLib
//

#include <vecLib/vecLib.h>
#include <strings.h>
#include "vdsp_intf.h"

#define RADIX_2     0  // currently the only supported Radix
#define FWD         0
#define INV         1

template<class __T>
inline __T max(__T a, __T b)
{
  return (a >= b ? a : b);
}

template<class __T>
inline __T min(__T a, __T b)
{
  return (a <= b ? a : b);
}

struct twids {
  int            log2n;
  OpaqueFFTSetup *ptwid;
} twids;

struct twidsD {
  int             log2n;
  OpaqueFFTSetupD *ptwid;
} twidsD;

static int ilog2(unsigned int x)
{
  int   p;
  double v = frexp((double)x, &p);
  return (v > 0.5 ? p : p-1);
}

static int next_pow2(int x)
{
  int   p;
  double v = frexp((double)x, &p);
  return (int)ldexp(ceil(2.0*v),p-1);
}

static void check_twidsD(unsigned int nel)
{
  // should only be called from inside of a guarded critical region
  
  int log2n = ilog2(nel);

  if(log2n > twidsD.log2n || twidsD.log2n - log2n > 1)
    {
      if(twidsD.ptwid)
	vDSP_destroy_fftsetupD(twidsD.ptwid);
      twidsD.log2n = log2n;
      twidsD.ptwid = vDSP_create_fftsetupD(log2n, RADIX_2);
    }
}

inline void blit_doubles(double *src, int srcoff,
			double *dst, int dstoff,
			int nel)
{
  memcpy(dst + dstoff, src + srcoff, nel * sizeof(double));
}

inline void zap_doubles(double *dst, int dstoff, int nel)
{
  memset(dst + dstoff, 0, nel*sizeof(double));
}

// -----------------------------------------------------
// sticky buffers

void *gpr = 0;
void *gpi = 0;
int  grsize = 0;
int  gisize = 0;

void* get_rbuf(int size)
{
  if(size <= 4096 * sizeof(double))
    {
      if(0 == gpr || grsize < size)
	{
	  if(gpr)
	    free_align16(gpr);
	  gpr = must_alloc_align16(size);
	  grsize = size;
	}
      return gpr;
    }
  else
    return must_alloc_align16(size);
}

void release_rbuf(void *p)
{
  if(p != gpr)
    free_align16(p);
}

void* get_ibuf(int size)
{
  if(size <= 4096 * sizeof(double))
    {
      if(0 == gpi || gisize < size)
	{
	  if(gpi)
	    free_align16(gpi);
	  gpi = must_alloc_align16(size);
	  gisize = size;
	}
      return gpi;
    }
  else
    return must_alloc_align16(size);
}
      
void release_ibuf(void *p)
{
  if(p != gpi)
    free_align16(p);
}
      
// -----------------------------------------------------
// vDSP Double-Precision Routines
//
extern "C"
void unsafe_z2zfft(int xsize, double *srcr, int roff,
		   double *srci, int ioff, int dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes 2^3 <= xsize, ysize <= 2^20
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array

  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  
  double *pr = srcr + roff;
  double *pi = srci + ioff;
  bool allocr = false;
  bool alloci = false;

  VDSP_CRITICAL_SECTION {

    if(15 & (long)pr)
      {
	pr = (double*)get_rbuf(xsize*sizeof(double));
	allocr = true;
	blit_doubles(srcr, roff, pr, 0, xsize);
      }
    
    if(15 & (long)pi)
      {
	pi = (double*)get_ibuf(xsize * sizeof(double));
	alloci = true;
	blit_doubles(srci, ioff, pi, 0, xsize);
      }
    
    DSPDoubleSplitComplex ioData = {pr, pi};
    
    check_twidsD(xsize);
    // only 1-D FFT
    vDSP_fft_ziptD(twidsD.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize),
		   (FWD == dir) ? kFFTDirection_Forward
		                : kFFTDirection_Inverse);
    
    // convert result back to DPFP in the user supplied dst array
    if (INV == dir)
      {
	double norm = 1.0 / xsize;
	vDSP_vsmulD(pr, 1, &norm, pr, 1, xsize);
	vDSP_vsmulD(pi, 1, &norm, pi, 1, xsize);
      }
    if(allocr)
      {
	blit_doubles(pr, 0, srcr, roff, xsize);
	release_rbuf(pr);
      }
    if(alloci)
      {
	blit_doubles(pi, 0, srci, ioff, xsize);
	release_ibuf(pi);
      }
  } END_VDSP_CRITICAL_SECTION;
}

void check_1d_size(int xsize)
{
  if(xsize < 8)
    throw("FFT size too small");
  if(xsize > (1 << 20))
    throw("FFT size too large");
}

extern "C"
void z2zfft(int xsize, double *src, double *dst, int dir)
{
  // NOTE: already assumes xsize and ysize are power of 2
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  check_1d_size(xsize);

  VDSP_CRITICAL_SECTION {

    // fixup source to separate re and im parts
    double *pr = (double*)get_rbuf(xsize*sizeof(double));
    double *pi = (double*)get_ibuf(xsize*sizeof(double));
    
    // convert src to split single FP complex format
    for(int ix = xsize; --ix >= 0;)
      {
	int ix2 = ix+ix;
	pr[ix] = src[ix2];
	pi[ix] = src[ix2+1];
      }
    
    unsafe_z2zfft(xsize, pr, 0, pi, 0, dir);
    
    for(int ix = xsize; --ix >= 0;)
      {
	int ix2 = ix+ix;
	dst[ix2]   = pr[ix];
	dst[ix2+1] = pi[ix];
      }
    release_rbuf(pr);
    release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// -----------------------------------------------------------
extern "C"
void d2zfft(int xsize, double *src, double *dst)
{
  check_1d_size(xsize);

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  double *pr = (double*)get_rbuf(xsize*sizeof(double));
  double *pi = (double*)get_ibuf(xsize*sizeof(double));
  
  // convert src to split single FP complex format
  blit_doubles(src, 0, pr, 0, xsize);
  zap_doubles(pi, 0, xsize);

  unsafe_z2zfft(xsize, pr, 0, pi, 0, FWD);
  
  // convert result back to DPFP in the user supplied dst array
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// ------------------------------------------------------------
//
extern "C"
void z2dfft(int xsize, double *src, double *dst)
{
  check_1d_size(xsize);

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  double *pr = (double*)get_rbuf(xsize*sizeof(double));
  double *pi = (double*)get_ibuf(xsize*sizeof(double));
  
  // convert src to split single FP complex format
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft(xsize, pr, 0, pi, 0, INV);
  
  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  blit_doubles(pr, 0, dst, 0, xsize);
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// -----------------------------------------------------------
inline void check_2dtwidsD(unsigned int nxel, unsigned int nyel)
{
  check_twidsD(max(nxel, nyel));
}

extern "C"
void unsafe_z2zfft2d(int xsize, int ysize,
		     double *srcr, int roff,
		     double *srci, int ioff, int dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes 2^3 <= xsize, ysize <= 2^10
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array

  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  
  double *pr = srcr + roff;
  double *pi = srci + ioff;
  bool allocr = false;
  bool alloci = false;
  int totsize = xsize * ysize;

  VDSP_CRITICAL_SECTION {

  if(15 & (long)pr)
    {
      pr = (double*)get_rbuf(totsize*sizeof(double));
      blit_doubles(srcr, roff, pr, 0, totsize);
      allocr = true;
    }
  if(15 & (long)pi)
    {
      pi = (double*)get_ibuf(totsize*sizeof(double));
      blit_doubles(srci, ioff, pi, 0, totsize);
      alloci = true;
    }

  DSPDoubleSplitComplex ioData = {pr, pi};

  check_2dtwidsD(xsize, ysize);
  if(1 == ysize)
    // only 1-D FFT
    vDSP_fft_ziptD(twidsD.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize),
		   (FWD == dir) ? kFFTDirection_Forward
		                : kFFTDirection_Inverse);
  else
    // 2-D FFT
    vDSP_fft2d_ziptD(twidsD.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
		     ilog2(xsize), ilog2(ysize),
		     (FWD == dir) ? kFFTDirection_Forward
		                  : kFFTDirection_Inverse);
  
  // convert result back to DPFP in the user supplied dst array
  if (INV == dir)
    {
      double norm = 1.0 / totsize;
      vDSP_vsmulD(pr, 1, &norm, pr, 1, totsize);
      vDSP_vsmulD(pi, 1, &norm, pi, 1, totsize);
    }
  if(allocr)
    {
      blit_doubles(pr, 0, srcr, roff, totsize);
      release_rbuf(pr);
    }
  if(alloci)
    {
      blit_doubles(pi, 0, srci, ioff, totsize);
      release_ibuf(pi);
    }
  } END_VDSP_CRITICAL_SECTION;
}

void check_2d_sizes(int xsize, int ysize)
{
  if (ysize > 1)
    {
      if(xsize < 8)
	throw("X dimension too small");
      if(xsize > 1024)
	throw("X dimension too large");
      
      if(ysize < 8)
	throw("Y dimension too small");
      if(ysize > 1024)
	throw("Y dimension too large");
    }
  else
    check_1d_size(xsize);
}

extern "C"
void z2zfft2d(int xsize, int ysize, double *src, double *dst, int dir)
{
  // NOTE: already assumes xsize and ysize are power of 2
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  check_2d_sizes(xsize, ysize);
  
  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  int totsize = xsize*ysize;
  double *pr = (double*)get_rbuf(totsize*sizeof(double));
  double *pi = (double*)get_ibuf(totsize*sizeof(double));

  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft2d(xsize, ysize, pr, 0, pi, 0, dir);

  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// -----------------------------------------------------------
extern "C"
void d2zfft2d(int xsize, int ysize, double *src, double *dst)
{
  check_2d_sizes(xsize, ysize);

  int totsize = xsize*ysize;

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  double *pr = (double*)get_rbuf(totsize*sizeof(double));
  double *pi = (double*)get_ibuf(totsize*sizeof(double));
  
  // convert src to split single FP complex format
  blit_doubles(src, 0, pr, 0, totsize);
  zap_doubles(pi, 0, totsize);

  unsafe_z2zfft2d(xsize, ysize, pr, 0, pi, 0, FWD);

  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// ------------------------------------------------------------
//
extern "C"
void z2dfft2d(int xsize, int ysize, double *src, double *dst)
{
  check_2d_sizes(xsize, ysize);

  int totsize = xsize*ysize;

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  double *pr = (double*)get_rbuf(totsize*sizeof(double));
  double *pi = (double*)get_ibuf(totsize*sizeof(double));
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft2d(xsize, ysize, pr, 0, pi, 0, INV);

  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  blit_doubles(pr, 0, dst, 0, totsize);
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// ---------------------------------------------------------------
// Altivec Single-Precision Routines

static void check_twids(unsigned int nel)
{
  // should only be called from inside of a guarded critical region
  
  int log2n = ilog2(nel);

  if(log2n > twids.log2n || twids.log2n - log2n > 1)
    {
      if(twids.ptwid)
	vDSP_destroy_fftsetup(twids.ptwid);
      twids.log2n = log2n;
      twids.ptwid = vDSP_create_fftsetup(log2n, RADIX_2);
    }
}

inline void check_2dtwids(unsigned int nxel, unsigned int nyel)
{
  check_twids(max(nxel, nyel));
}

inline void blit_floats(float *src, int srcoff,
			float *dst, int dstoff,
			int nel)
{
  memcpy(dst + dstoff, src + srcoff, nel * sizeof(float));
}

inline void zap_floats(float *dst, int dstoff, int nel)
{
  memset(dst + dstoff, 0, nel * sizeof(float));
}

extern "C"
void unsafe_c2cfft(int xsize, float *srcr, int roff,
		   float *srci, int ioff, int dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes 2^3 <= xsize, ysize <= 2^20
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  float *pr = srcr + roff;
  float *pi = srci + ioff;
  bool allocr = false;
  bool alloci = false;

  VDSP_CRITICAL_SECTION {

  if(15 & (long)pr)
    {
      pr = (float*)get_rbuf(xsize * sizeof(float));
      allocr = true;
      blit_floats(srcr, roff, pr, 0, xsize);
    }
  
  if(15 & (long)pi)
    {
      pi = (float*)get_ibuf(xsize * sizeof(float));
      alloci = true;
      blit_floats(srci, ioff, pi, 0, xsize);
    }
  
  DSPSplitComplex ioData = {pr, pi};

  check_twids(xsize);
  // only 1-D FFT
  vDSP_fft_zipt(twids.ptwid, &ioData, 1, (DSPSplitComplex*)&vDSPBufferTemp, ilog2(xsize),
		(FWD == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
  
  // convert result back to user supplied dst array
  if (INV == dir)
    {
      float norm = 1.0f / xsize;
      vDSP_vsmul(pr, 1, &norm, pr, 1, xsize);
      vDSP_vsmul(pi, 1, &norm, pi, 1, xsize);
    }
  if(allocr)
    {
      blit_floats(pr, 0, srcr, roff, xsize);
      release_rbuf(pr);
    }
  if(alloci)
    {
      blit_floats(pi, 0, srci, ioff, xsize);
      release_ibuf(pi);
    }
  } END_VDSP_CRITICAL_SECTION;
}

extern "C"
void c2cfft(int xsize, float *src, float *dst, int dir)
{
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  check_1d_size(xsize);

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  float *pr = (float*)get_rbuf(xsize*sizeof(float));
  float *pi = (float*)get_ibuf(xsize*sizeof(float));

  // convert src to split single FP complex format
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_c2cfft(xsize, pr, 0, pi, 0, dir);

  // convert result back to user supplied dst array
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

extern "C"
void r2cfft(int xsize, float *src, float *dst)
{
  check_1d_size(xsize);

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  float *pr = (float*)get_rbuf(xsize*sizeof(float));
  float *pi = (float*)get_ibuf(xsize*sizeof(float));
  
  // convert src to split single FP complex format
  blit_floats(src, 0, pr, 0, xsize);
  zap_floats(pi, 0, xsize);

  unsafe_c2cfft(xsize, pr, 0, pi, 0, FWD);
  
  // convert result back to DPFP in the user supplied dst array
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

extern "C"
void c2rfft(int xsize, float *src, float *dst)
{
  check_1d_size(xsize);

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  float *pr = (float*)get_rbuf(xsize*sizeof(float));
  float *pi = (float*)get_ibuf(xsize*sizeof(float));
  
  // convert src to split single FP complex format
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_c2cfft(xsize, pr, 0, pi, 0, INV);
  
  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  blit_floats(pr, 0, dst, 0, xsize);
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// ---------------------------------------------------------
// Single precision 2D routines

extern "C"
void unsafe_c2cfft2d(int xsize, int ysize, float *srcr, int roff,
		     float *srci, int ioff, int dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes 2^3 <= xsize, ysize <= 2^10
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //

  float *pr = srcr + roff;
  float *pi = srci + ioff;
  bool allocr = false;
  bool alloci = false;
  int totsize = xsize * ysize;
  
  VDSP_CRITICAL_SECTION {

  if(15 & (long)pr)
    {
      pr = (float*)get_rbuf(totsize*sizeof(float));
      blit_floats(srcr, roff, pr, 0, totsize);
      allocr = true;
    }
  if(15 & (long)pi)
    {
      pi = (float*)get_ibuf(totsize*sizeof(float));
      blit_floats(srci, ioff, pi, 0, totsize);
      alloci = true;
    }
  
  DSPSplitComplex ioData = {pr, pi};

  check_2dtwids(xsize, ysize);
  if(1 == ysize)
    // only 1-D FFT
    vDSP_fft_zipt(twids.ptwid, &ioData, 1, (DSPSplitComplex*)&vDSPBufferTemp, ilog2(xsize),
		  (FWD == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
  else
    // 2-D FFT
    vDSP_fft2d_zipt(twids.ptwid, &ioData, 1, 0, (DSPSplitComplex*)&vDSPBufferTemp,
		    ilog2(xsize), ilog2(ysize),
		    (FWD == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
  
  // convert result back to user supplied dst array
  if (INV == dir)
    {
      float norm = 1.0f / totsize;
      vDSP_vsmul(pr, 1, &norm, pr, 1, totsize);
      vDSP_vsmul(pi, 1, &norm, pi, 1, totsize);
    }
  if(allocr)
    {
      blit_floats(pr, 0, srcr, roff, totsize);
      release_rbuf(pr);
    }
  if(alloci)
    {
      blit_floats(pi, 0, srci, ioff, totsize);
      release_ibuf(pi);
    }
  } END_VDSP_CRITICAL_SECTION;
}

extern "C"
void c2cfft2d(int xsize, int ysize, float *src, float *dst, int dir)
{
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  check_2d_sizes(xsize, ysize);

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  int totsize = xsize*ysize;
  float *pr = (float*)get_rbuf(totsize*sizeof(float));
  float *pi = (float*)get_ibuf(totsize*sizeof(float));

  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_c2cfft2d(xsize, ysize, pr, 0, pi, 0, dir);

  // convert result back to user supplied dst array
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

extern "C"
void r2cfft2d(int xsize, int ysize, float *src, float *dst)
{
  check_2d_sizes(xsize, ysize);

  int totsize = xsize * ysize;

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  float *pr = (float*)get_rbuf(totsize*sizeof(float));
  float *pi = (float*)get_ibuf(totsize*sizeof(float));
  
  // convert src to split single FP complex format
  blit_floats(src, 0, pr, 0, totsize);
  zap_floats(pi, 0, totsize);

  unsafe_c2cfft2d(xsize, ysize, pr, 0, pi, 0, FWD);

  // convert result back to DPFP in the user supplied dst array
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

extern "C"
void c2rfft2d(int xsize, int ysize, float *src, float *dst)
{
  check_2d_sizes(xsize, ysize);

  int totsize = xsize*ysize;

  VDSP_CRITICAL_SECTION {

  // fixup source to separate re and im parts
  float *pr = (float*)get_rbuf(totsize*sizeof(float));
  float *pi = (float*)get_ibuf(totsize*sizeof(float));
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_c2cfft2d(xsize, ysize, pr, 0, pi, 0, INV);

  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  blit_floats(pr, 0, dst, 0, totsize);
  release_rbuf(pr);
  release_ibuf(pi);
  } END_VDSP_CRITICAL_SECTION;
}

// ---------------------------------------------------------
//
extern "C"
void siglab_sbFFT(float *rsrc, float *cdst, int nfft)
{
  // special 1-D real->complex Forward FFT for SigLab
  // NOTE: From the caller's perspective, it is not safe
  //       for src and dst arrays to overlap.
  if(nfft < 8)
    throw("FFT size too small");

  int nfft2 = nfft/2;

  DSPSplitComplex ioData = {cdst, cdst+nfft2};
  vDSP_ctoz((DSPComplex*)rsrc, 2, &ioData, 1, nfft2);
  
  VDSP_CRITICAL_SECTION {
    check_twids(nfft);
    // in-place routine
    vDSP_fft_zript(twids.ptwid, &ioData, 1,
		   (DSPSplitComplex*)&vDSPBufferTemp,
		   ilog2(nfft), kFFTDirection_Forward);
  } END_VDSP_CRITICAL_SECTION;
  
    // ztoc(&ioData, 1, (DSPComplex*)cdst, 2, nfft2);

  // NOTE: Normalized so that unit DC signal produces unit delta at 0 freq
  float norm = 0.5f / nfft;
  vDSP_vsmul(cdst,1,&norm,cdst,1,nfft);
}

// ------------------------------------------------------------
//

extern "C"
int get_align16_offset(void *buf)
{
  int rem = ((long)buf & 15);
  return (rem ? 16 - rem : 0);
}

// ------------------------------------------------------------
//

extern "C"
void GetFFTVersionString(char *buf, int nmax)
{
  static char *ver = 
    "Carbon vDSP Library";
  strncpy(buf, ver, nmax);
}

// -- end of fft_glue.cpp -- //

