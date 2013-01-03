// fftglue.cpp -- CAML Interface to FFAST 2-D FFT Routines
//
// DM/MCFA  12/98
// ------------------------------------------------------------
// Create a dynamic lib with:
//
// gcc -dynamiclib -o libLispFFT.dylib fft_glue.cpp \
//      -lstdc++ -lfftw3 -framework VecLib
//

#define USE_FFTW  0


#if USE_FFTW
extern "C" {
#include <fftw3.h>
};

#include <vecLib/vecLib.h>
#include <strings.h>
#include "vdsp_intf.h"

// FFTW Double-Precision Routines
//
// NOTE: The FFTW Manual incorrectly states the order of dimensions
//  to provide to the fftw_plan_dft_2d routine. Instead of x then y,
//  it actually needs y, then x.
//  
extern "C"
void z2zfft(long xsize, long ysize,
	    fftw_complex *src, fftw_complex *dst, long dir)
{
	// Direction dir is FORWARD = 0, or INVERSE = 1.
	// taking (2 * dir - 1) turns this into FORWARD -> -1, INVERSE -> +1
  fftw_plan p = fftw_plan_dft_2d(ysize,xsize,src,dst,
				 (2*dir-1),FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);
}

extern "C"
void d2zfft(long xsize, long ysize, double *src, fftw_complex *dst)
{
  long tsize = xsize*ysize;
  fftw_complex *in =
    (fftw_complex*)must_alloc_align16(sizeof(fftw_complex)*tsize);
  for(long ix = tsize; --ix >= 0;)
    {
      in[ix][0] = src[ix];
      in[ix][1] = 0.0;
    }
  fftw_plan p = fftw_plan_dft_2d(ysize,xsize,in,dst,
				 FFTW_FORWARD,FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);
  free_align16(in);
}

extern "C"
void z2dfft(long xsize, long ysize, fftw_complex *src, double *dst)
{
  long tsize = xsize*ysize;
  fftw_complex *out =
    (fftw_complex*)must_alloc_align16(sizeof(fftw_complex)*tsize);
  fftw_plan p = fftw_plan_dft_2d(ysize,xsize,src,out,
				 FFTW_BACKWARD,FFTW_ESTIMATE);
  fftw_execute(p);
  for(long ix = tsize; --ix >= 0;)
      dst[ix] = out[ix][0];
  fftw_destroy_plan(p);
  free_align16(out);
}

// ---------------------------------------------------------------
// Altivec Single-Precision Routines

#include "vdsp_intf.h"

#define RADIX_2     0  // currently the only supported Radix

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

struct {
  long            log2n;
  OpaqueFFTSetup *ptwid;
} twids;

static long ilog2(unsigned long x)
{
  int   p;
  float v = frexpf((float)x, &p);
  return (v > 0.5 ? p : p-1);
}

static long next_pow2(long x)
{
  int   p;
  float v = frexpf((float)x, &p);
  return (long)ldexpf(ceilf(2.0*v),p-1);
}

static void check_twids(unsigned long nel)
{
  // should only be called from inside of a guarded critical region
  
  long log2n = ilog2(nel);

  if(log2n > twids.log2n || twids.log2n - log2n > 1)
    {
      if(twids.ptwid)
	destroy_fftsetup(twids.ptwid);
      twids.log2n = log2n;
      twids.ptwid = create_fftsetup(log2n, RADIX_2);
    }
}

inline void check_2dtwids(unsigned long nxel, unsigned long nyel)
{
  check_twids(max(nxel, nyel));
}

inline void blit_floats(float *src, long srcoff,
			float *dst, long dstoff,
			long nel)
{
  memcpy(dst + dstoff, src + srcoff, nel * sizeof(float));
}

extern "C"
void c2cfft(long xsize, long ysize, float *src, float *dst, long dir)
{
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  long totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(fftw_complex)*totsize);
  float *pi = &pr[totsize];

  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  DSPSplitComplex ioData = {pr, pi};

  VDSP_CRITICAL_SECTION {
    check_2dtwids(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	fft_zipt(twids.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize),
		 (0 == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
      }
    else
      {
	// 2-D FFT
	fft2d_zipt(twids.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
		   ilog2(xsize), ilog2(ysize),
		   (0 == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

extern "C"
void r2cfft(long xsize, long ysize, float *src, float *dst)
{
  long totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(fftw_complex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      pr[ix] = src[ix];
      pi[ix] = 0.0;
    }

  DSPSplitComplex ioData = {pr, pi};
      
  VDSP_CRITICAL_SECTION {
    check_2dtwids(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	fft_zipt(twids.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize), 
		 kFFTDirection_Forward);
      }
    else
      {
	// 2-D FFT
	fft2d_zipt(twids.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
		   ilog2(xsize), ilog2(ysize), kFFTDirection_Forward);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

// ---------------------------------------------------------
//
extern "C"
void siglab_sbFFT(float *rsrc, float *cdst, long nfft)
{
  // special 1-D real->complex Forward FFT for SigLab
  // NOTE: From the caller's perspective, it is not safe
  //       for src and dst arrays to overlap.
  if(nfft < 8)
    throw("FFT size too small");

  long nfft2 = nfft/2;

  DSPSplitComplex ioData = {cdst, cdst+nfft2};
  ctoz((DSPComplex*)rsrc, 2, &ioData, 1, nfft2);
  
  VDSP_CRITICAL_SECTION {
    check_twids(nfft);
    // in-place routine
    fft_zript(twids.ptwid, &ioData, 1,
	      &vDSPBufferTemp, ilog2(nfft), kFFTDirection_Forward);
  } END_VDSP_CRITICAL_SECTION;
  
    // ztoc(&ioData, 1, (DSPComplex*)cdst, 2, nfft2);

  // NOTE: Normalized so that unit DC signal produces unit delta at 0 freq
  float norm = 1.0f / nfft;
  vsmul(cdst,1,&norm,cdst,1,nfft);
}

// ------------------------------------------------------------
//
extern "C"
void c2rfft(long xsize, long ysize, float *src, float *dst)
{
  long totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(fftw_complex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  DSPSplitComplex ioData = {pr, pi};

  VDSP_CRITICAL_SECTION {
    check_2dtwids(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	fft_zipt(twids.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize), 
		 FFT_INVERSE);
      }
    else
      {
	// 2-D FFT
	fft2d_zipt(twids.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
		   ilog2(xsize), ilog2(ysize), kFFTDirection_Inverse);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  for(long ix = totsize; --ix >= 0;)
      dst[ix] = pr[ix];
  free_align16(pr);
}

extern "C"
void GetFFTVersionString(char *buf, long nmax)
{
  static char *ver = 
     "Carbon vDSP Altivec & FFTW 3.01 (Fused-MAC) Libraries";
  strncpy(buf, ver, nmax);
}

// ---------------------------------------------------------------
#else // Not FFTW but rather vDSP

#include <vecLib/vecLib.h>
#include <strings.h>
#include "vdsp_intf.h"

#define RADIX_2     0  // currently the only supported Radix


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
  long            log2n;
  OpaqueFFTSetup *ptwid;
} twids;

struct twidsD {
  long             log2n;
  OpaqueFFTSetupD *ptwid;
} twidsD;

static long ilog2(unsigned long x)
{
  int   p;
  double v = frexp((double)x, &p);
  return (v > 0.5 ? p : p-1);
}

static long next_pow2(long x)
{
  int   p;
  double v = frexp((double)x, &p);
  return (long)ldexp(ceil(2.0*v),p-1);
}

static void check_twidsD(unsigned long nel)
{
  // should only be called from inside of a guarded critical region
  
  long log2n = ilog2(nel);

  if(log2n > twidsD.log2n || twidsD.log2n - log2n > 1)
    {
      if(twidsD.ptwid)
	vDSP_destroy_fftsetupD(twidsD.ptwid);
      twidsD.log2n = log2n;
      twidsD.ptwid = vDSP_create_fftsetupD(log2n, RADIX_2);
    }
}

// vDSP Double-Precision Routines
//
extern "C"
void unsafe_z2zfft(long xsize, double *srcr, double *srci, long dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes xsize, ysize >= 4
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array

  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  DSPDoubleSplitComplex ioData = {srcr, srci};

  VDSP_CRITICAL_SECTION {
    check_twidsD(xsize);
    // only 1-D FFT
    vDSP_fft_ziptD(twidsD.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize),
		   (0 == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  if (0 != dir)
    {
      double norm = 1.0 / xsize;
      for(long ix = xsize; --ix >= 0;)
	{
	  srcr[ix] *= norm;
	  srci[ix] *= norm;
	}
    }
}

extern "C"
void z2zfft(long xsize, double *src, double *dst, long dir)
{
  // NOTE: already assumes xsize and ysize are power of 2
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  if(xsize < 8)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*xsize);
  double *pi = &pr[xsize];

  // convert src to split single FP complex format
  for(long ix = xsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft(xsize, pr, pi, dir);

  for(long ix = xsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

// -----------------------------------------------------------
inline void check_2dtwidsD(unsigned long nxel, unsigned long nyel)
{
  check_twidsD(max(nxel, nyel));
}

extern "C"
void unsafe_z2zfft2d(long xsize, long ysize, double *srcr, double *srci, long dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes xsize, ysize both power of 2 between 8 and 1024 inclusive
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array

  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  DSPDoubleSplitComplex ioData = {srcr, srci};

  VDSP_CRITICAL_SECTION {
    check_2dtwidsD(xsize, ysize);
    // 2-D FFT
    vDSP_fft2d_ziptD(twidsD.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
		     ilog2(xsize), ilog2(ysize),
		     (0 == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  if (0 != dir)
    {
      long    totsize = xsize*ysize;
      double norm = 1.0 / totsize;
      for(long ix = totsize; --ix >= 0;)
	{
	  srcr[ix] *= norm;
	  srci[ix] *= norm;
	}
    }
}

void check_2d_sizes(long xsize, long ysize)
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

extern "C"
void z2zfft2d(long xsize, long ysize, double *src, double *dst, long dir)
{
  // NOTE: already assumes xsize and ysize are power of 2
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  check_2d_sizes(xsize, ysize);
  
  // fixup source to separate re and im parts
  long totsize = xsize*ysize;
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*totsize);
  double *pi = &pr[totsize];

  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft2d(xsize, ysize, pr, pi, dir);

  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

// -----------------------------------------------------------
extern "C"
void d2zfft2d(long xsize, long ysize, double *src, double *dst)
{
  check_2d_sizes(xsize, ysize);

  long totsize = xsize*ysize;

  // fixup source to separate re and im parts
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*totsize);
  double *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      pr[ix] = src[ix];
      pi[ix] = 0.0;
    }

  DSPDoubleSplitComplex ioData = {pr, pi};
      
  VDSP_CRITICAL_SECTION {
    check_2dtwidsD(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	vDSP_fft_ziptD(twidsD.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize), 
		       kFFTDirection_Forward);
      }
    else
      {
	// 2-D FFT
	vDSP_fft2d_ziptD(twidsD.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
			 ilog2(xsize), ilog2(ysize), kFFTDirection_Forward);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  double norm = 1.0 / (xsize * ysize);
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = norm * pr[ix];
      dst[ix2+1] = norm * pi[ix];
    }
  free_align16(pr);
}

// ------------------------------------------------------------
//
extern "C"
void z2dfft2d(long xsize, long ysize, float *src, float *dst)
{
  check_2d_sizes(xsize, ysize);

  long totsize = xsize*ysize;

  // fixup source to separate re and im parts
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*totsize);
  double *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  DSPDoubleSplitComplex ioData = {pr, pi};

  VDSP_CRITICAL_SECTION {
    check_2dtwidsD(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	vDSP_fft_ziptD(twidsD.ptwid, &ioData, 1, &vDSPBufferTemp, ilog2(xsize), 
		      FFT_INVERSE);
      }
    else
      {
	// 2-D FFT
	vDSP_fft2d_ziptD(twidsD.ptwid, &ioData, 1, 0, &vDSPBufferTemp,
			ilog2(xsize), ilog2(ysize), kFFTDirection_Inverse);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  for(long ix = totsize; --ix >= 0;)
      dst[ix] = pr[ix];
  free_align16(pr);
}

// ---------------------------------------------------------------
// Altivec Single-Precision Routines

static void check_twids(unsigned long nel)
{
  // should only be called from inside of a guarded critical region
  
  long log2n = ilog2(nel);

  if(log2n > twids.log2n || twids.log2n - log2n > 1)
    {
      if(twids.ptwid)
	vDSP_destroy_fftsetup(twids.ptwid);
      twids.log2n = log2n;
      twids.ptwid = vDSP_create_fftsetup(log2n, RADIX_2);
    }
}

inline void check_2dtwids(unsigned long nxel, unsigned long nyel)
{
  check_twids(max(nxel, nyel));
}

inline void blit_floats(float *src, long srcoff,
			float *dst, long dstoff,
			long nel)
{
  memcpy(dst + dstoff, src + srcoff, nel * sizeof(float));
}

extern "C"
void split_complex_unsafe_c2cfft(long xsize, long ysize, float *srcr, float *srci, long dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes xsize, ysize >= 4
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  DSPSplitComplex ioData = {srcr, srci};

  VDSP_CRITICAL_SECTION {
    check_2dtwids(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	vDSP_fft_zipt(twids.ptwid, &ioData, 1, (DSPSplitComplex*)&vDSPBufferTemp, ilog2(xsize),
		 (0 == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
      }
    else
      {
	// 2-D FFT
	vDSP_fft2d_zipt(twids.ptwid, &ioData, 1, 0, (DSPSplitComplex*)&vDSPBufferTemp,
		   ilog2(xsize), ilog2(ysize),
		   (0 == dir) ? kFFTDirection_Forward : kFFTDirection_Inverse);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to user supplied dst array
  if (0 != dir)
    {
      long totsize = xsize*ysize;
      float norm = 1.0f / totsize;
      for(long ix = totsize; --ix >= 0;)
	{
	  srcr[ix] *= norm;
	  srci[ix] *= norm;
	}
    }
}

extern "C"
void unsafe_c2cfft(long xsize, long ysize, float *src, long dir)
{
  // Unsafe routine -- no checking of table size (assumed to be power of 2)
  // incoming array already split between real and imaginary parts
  // assumed align to 16 bytes
  // assumes xsize, ysize >= 4
  // assumes src array is safe to use directly (from static allocation in Lisp)
  // FFT will be performed in place in src array
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  split_complex_unsafe_c2cfft(xsize, ysize, src, src+xsize*ysize, dir);
}

extern "C"
void c2cfft(long xsize, long ysize, float *src, float *dst, long dir)
{
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  if(xsize < 4 || (ysize > 1 && ysize < 4))
    throw("FFT size too small");

  // fixup source to separate re and im parts
  long totsize = xsize*ysize;
  float *pr = (float*)must_alloc_align16(sizeof(DSPComplex)*totsize);
  float *pi = &pr[totsize];

  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  split_complex_unsafe_c2cfft(xsize, ysize, pr, pi, dir);

  // convert result back to user supplied dst array
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

extern "C"
void r2cfft(long xsize, long ysize, float *src, float *dst)
{
  long totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(DSPComplex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      pr[ix] = src[ix];
      pi[ix] = 0.0;
    }

  DSPSplitComplex ioData = {pr, pi};
      
  VDSP_CRITICAL_SECTION {
    check_2dtwids(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	vDSP_fft_zipt(twids.ptwid, &ioData, 1, (DSPSplitComplex*)&vDSPBufferTemp, ilog2(xsize), 
		 kFFTDirection_Forward);
      }
    else
      {
	// 2-D FFT
	vDSP_fft2d_zipt(twids.ptwid, &ioData, 1, 0, (DSPSplitComplex*)&vDSPBufferTemp,
		   ilog2(xsize), ilog2(ysize), kFFTDirection_Forward);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  float norm = 1.0f / (xsize * ysize);
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      dst[ix2]   = norm * pr[ix];
      dst[ix2+1] = norm * pi[ix];
    }
  free_align16(pr);
}

// ---------------------------------------------------------
//
extern "C"
void siglab_sbFFT(float *rsrc, float *cdst, long nfft)
{
  // special 1-D real->complex Forward FFT for SigLab
  // NOTE: From the caller's perspective, it is not safe
  //       for src and dst arrays to overlap.
  if(nfft < 8)
    throw("FFT size too small");

  long nfft2 = nfft/2;

  DSPSplitComplex ioData = {cdst, cdst+nfft2};
  ctoz((DSPComplex*)rsrc, 2, &ioData, 1, nfft2);
  
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
  vsmul(cdst,1,&norm,cdst,1,nfft);
}

// ------------------------------------------------------------
//
extern "C"
void c2rfft(long xsize, long ysize, float *src, float *dst)
{
  long totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(DSPComplex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(long ix = totsize; --ix >= 0;)
    {
      long ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  DSPSplitComplex ioData = {pr, pi};

  VDSP_CRITICAL_SECTION {
    check_2dtwids(xsize, ysize);
    if(ysize < 2)
      {
	// only 1-D FFT
	vDSP_fft_zipt(twids.ptwid, &ioData, 1, (DSPSplitComplex*)&vDSPBufferTemp, ilog2(xsize), 
		      FFT_INVERSE);
      }
    else
      {
	// 2-D FFT
	vDSP_fft2d_zipt(twids.ptwid, &ioData, 1, 0, (DSPSplitComplex*)&vDSPBufferTemp,
			ilog2(xsize), ilog2(ysize), kFFTDirection_Inverse);
      }
  } END_VDSP_CRITICAL_SECTION;
  
  // convert result back to DPFP in the user supplied dst array
  // trust that the result really is real valued and that the
  // imaginary components are all zero or merely noise...
  for(long ix = totsize; --ix >= 0;)
      dst[ix] = pr[ix];
  free_align16(pr);
}

extern "C"
void GetFFTVersionString(char *buf, long nmax)
{
  static char *ver = 
    "Carbon vDSP Library";
  strncpy(buf, ver, nmax);
}

#endif // USE_FFTW

// -- end of fft_glue.cpp -- //

