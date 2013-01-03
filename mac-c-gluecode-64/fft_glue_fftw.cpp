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
void z2zfft(int xsize, int ysize,
	    fftw_complex *src, fftw_complex *dst, int dir)
{
	// Direction dir is FORWARD = 0, or INVERSE = 1.
	// taking (2 * dir - 1) turns this into FORWARD -> -1, INVERSE -> +1
  fftw_plan p = fftw_plan_dft_2d(ysize,xsize,src,dst,
				 (2*dir-1),FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);
}

extern "C"
void d2zfft(int xsize, int ysize, double *src, fftw_complex *dst)
{
  int tsize = xsize*ysize;
  fftw_complex *in =
    (fftw_complex*)must_alloc_align16(sizeof(fftw_complex)*tsize);
  for(int ix = tsize; --ix >= 0;)
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
void z2dfft(int xsize, int ysize, fftw_complex *src, double *dst)
{
  int tsize = xsize*ysize;
  fftw_complex *out =
    (fftw_complex*)must_alloc_align16(sizeof(fftw_complex)*tsize);
  fftw_plan p = fftw_plan_dft_2d(ysize,xsize,src,out,
				 FFTW_BACKWARD,FFTW_ESTIMATE);
  fftw_execute(p);
  for(int ix = tsize; --ix >= 0;)
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
  int            log2n;
  OpaqueFFTSetup *ptwid;
} twids;

static int ilog2(unsigned int x)
{
  int   p;
  float v = frexpf((float)x, &p);
  return (v > 0.5 ? p : p-1);
}

static int next_pow2(int x)
{
  int   p;
  float v = frexpf((float)x, &p);
  return (int)ldexpf(ceilf(2.0*v),p-1);
}

static void check_twids(unsigned int nel)
{
  // should only be called from inside of a guarded critical region
  
  int log2n = ilog2(nel);

  if(log2n > twids.log2n || twids.log2n - log2n > 1)
    {
      if(twids.ptwid)
	destroy_fftsetup(twids.ptwid);
      twids.log2n = log2n;
      twids.ptwid = create_fftsetup(log2n, RADIX_2);
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

extern "C"
void c2cfft(int xsize, int ysize, float *src, float *dst, int dir)
{
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  int totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(fftw_complex)*totsize);
  float *pi = &pr[totsize];

  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
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
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

extern "C"
void r2cfft(int xsize, int ysize, float *src, float *dst)
{
  int totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(fftw_complex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
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
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
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
void c2rfft(int xsize, int ysize, float *src, float *dst)
{
  int totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(fftw_complex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
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
  for(int ix = totsize; --ix >= 0;)
      dst[ix] = pr[ix];
  free_align16(pr);
}

extern "C"
void GetFFTVersionString(char *buf, int nmax)
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

// vDSP Double-Precision Routines
//
extern "C"
void unsafe_z2zfft(int xsize, double *srcr, double *srci, int dir)
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
      for(int ix = xsize; --ix >= 0;)
	{
	  srcr[ix] *= norm;
	  srci[ix] *= norm;
	}
    }
}

extern "C"
void z2zfft(int xsize, double *src, double *dst, int dir)
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
  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft(xsize, pr, pi, dir);

  for(int ix = xsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

// -----------------------------------------------------------
inline void check_2dtwidsD(unsigned int nxel, unsigned int nyel)
{
  check_twidsD(max(nxel, nyel));
}

extern "C"
void unsafe_z2zfft2d(int xsize, int ysize, double *srcr, double *srci, int dir)
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
      int    totsize = xsize*ysize;
      double norm = 1.0 / totsize;
      for(int ix = totsize; --ix >= 0;)
	{
	  srcr[ix] *= norm;
	  srci[ix] *= norm;
	}
    }
}

void check_2d_sizes(int xsize, int ysize)
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
void z2zfft2d(int xsize, int ysize, double *src, double *dst, int dir)
{
  // NOTE: already assumes xsize and ysize are power of 2
  
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  check_2d_sizes(xsize, ysize);
  
  // fixup source to separate re and im parts
  int totsize = xsize*ysize;
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*totsize);
  double *pi = &pr[totsize];

  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  unsafe_z2zfft2d(xsize, ysize, pr, pi, dir);

  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

// -----------------------------------------------------------
extern "C"
void d2zfft2d(int xsize, int ysize, double *src, double *dst)
{
  check_2d_sizes(xsize, ysize);

  int totsize = xsize*ysize;

  // fixup source to separate re and im parts
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*totsize);
  double *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
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
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = norm * pr[ix];
      dst[ix2+1] = norm * pi[ix];
    }
  free_align16(pr);
}

// ------------------------------------------------------------
//
extern "C"
void z2dfft2d(int xsize, int ysize, float *src, float *dst)
{
  check_2d_sizes(xsize, ysize);

  int totsize = xsize*ysize;

  // fixup source to separate re and im parts
  double *pr = (double*)must_alloc_align16(sizeof(DSPDoubleComplex)*totsize);
  double *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
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
  for(int ix = totsize; --ix >= 0;)
      dst[ix] = pr[ix];
  free_align16(pr);
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

extern "C"
void split_complex_unsafe_c2cfft(int xsize, int ysize, float *srcr, float *srci, int dir)
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
      int totsize = xsize*ysize;
      float norm = 1.0f / totsize;
      for(int ix = totsize; --ix >= 0;)
	{
	  srcr[ix] *= norm;
	  srci[ix] *= norm;
	}
    }
}

extern "C"
void unsafe_c2cfft(int xsize, int ysize, float *src, int dir)
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
void c2cfft(int xsize, int ysize, float *src, float *dst, int dir)
{
  // Direction dir is FORWARD = 0, or INVERSE = 1.
  // taking (2 * dir - 1) turns this into 
  // FORWARD -> -1, INVERSE -> +1
  //
  if(xsize < 4 || (ysize > 1 && ysize < 4))
    throw("FFT size too small");

  // fixup source to separate re and im parts
  int totsize = xsize*ysize;
  float *pr = (float*)must_alloc_align16(sizeof(DSPComplex)*totsize);
  float *pi = &pr[totsize];

  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      pr[ix] = src[ix2];
      pi[ix] = src[ix2+1];
    }

  split_complex_unsafe_c2cfft(xsize, ysize, pr, pi, dir);

  // convert result back to user supplied dst array
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = pr[ix];
      dst[ix2+1] = pi[ix];
    }
  free_align16(pr);
}

extern "C"
void r2cfft(int xsize, int ysize, float *src, float *dst)
{
  int totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(DSPComplex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
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
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
      dst[ix2]   = norm * pr[ix];
      dst[ix2+1] = norm * pi[ix];
    }
  free_align16(pr);
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
void c2rfft(int xsize, int ysize, float *src, float *dst)
{
  int totsize = xsize*ysize;
  if(totsize < 4)
    throw("FFT size too small");

  // fixup source to separate re and im parts
  float *pr = (float*)must_alloc_align16(sizeof(DSPComplex)*totsize);
  float *pi = &pr[totsize];
  
  // convert src to split single FP complex format
  for(int ix = totsize; --ix >= 0;)
    {
      int ix2 = ix+ix;
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
  for(int ix = totsize; --ix >= 0;)
      dst[ix] = pr[ix];
  free_align16(pr);
}

extern "C"
void GetFFTVersionString(char *buf, int nmax)
{
  static char *ver = 
    "Carbon vDSP Library";
  strncpy(buf, ver, nmax);
}

#endif // USE_FFTW

// -- end of fft_glue.cpp -- //

