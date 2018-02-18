// siglab_glue.cpp -- misc routines to help SigLab
// DM/MCFA 06/04
//

#include <math.h>
#include <Accelerate/Accelerate.h>
#include <memory.h>

extern "C" {

// ---------------------------------------------
// Workaround routines to prevent halting on divzero
//
double siglab_float_divide(double num, double den)
{
  return num/den;
}

float siglab_float_dividef(float num, float den)
{
  return num/den;
}

// ---------------------------------------------
// Disable/Enable Denormal Processing
//

#if 1 // __i386__
// our compiler does ALL floating point with SSE
#define GETCSR()       ({ int _result; asm volatile ("stmxcsr %0" : "=m" (*&_result) ); /*return*/ _result; })
#define SETCSR( a )    { int _temp = a; asm volatile( "ldmxcsr %0" : : "m" (*&_temp ) ); }

int siglab_disable_denormals()
{
  int _savemxcsr = GETCSR();
  SETCSR(_savemxcsr | 0x8040);
  return _savemxcsr;
}

void siglab_restore_denormals(int _savemxcsr)
{
  SETCSR(_savemxcsr);
}

#else
int siglab_disable_denormals()
{
  return 999;
}

void siglab_restore_denormals(int _savemxcsr)
{
}
#endif // __i386__

// ---------------------------------------------
// vector math
//
void siglab_sbMpy3(float *src1, float *src2, float *dst, int nel)
{
  vDSP_vmul(src1, 1, src2, 1, dst, 1, nel);
}

void siglab_sbMpy1(float kval, float *src, float *dst, int nel)
{
  vDSP_vsmul(src, 1, &kval, dst, 1, nel);
}

void siglab_sbAdd1(float kval, float *src, float *dst, int nel)
{
  while(--nel >= 0)
   *dst++ = kval + *src++;
}

void siglab_sbSub1(float kval, float *src, float *dst, int nel)
{
  while(--nel >= 0)
    *dst++ = *src++ - kval;
}

void siglab_sbNormalize(float *src, float *dst, int nel,
                        float factor, float offset)
{
  // scale the src array, copying to dst, such that
  // maximum absolute value is factor, then add offset
  float *p = src;
  float vmax = fabsf(*p++);
  for(int ix = nel; --ix > 0;)
  {
    float v = fabsf(*p++);
    if(v > vmax) vmax = v;
  }
  float sf = factor/vmax;
  while(--nel >= 0)
    *dst = sf * *src++ + offset;
}

// ----------------------------------------------------
// moving and setting
//
void siglab_sbSet(float kval, float *dst, int nel)
{
  while(--nel >= 0)
    *dst++ = kval;
}

void siglab_sbzero(float *dst, int nel)
{
  memset(dst, 0, nel*sizeof(float));
}

void siglab_sbcopy(float *src, float *dst, int nel)
{
  memcpy(dst, src, nel*sizeof(float));
}

void siglab_copy_to_column(float *arr, float *img,
                           int xdim, int ydim)
{
  while(--ydim >= 0)
  {
    *img = *arr++;
    img += xdim;
  }
}

// -------------------------------------------------
// FFT post-processing
//
void siglab_sbDB(float *src, float *dst, int nfft)
{
  // requires src to be power instead of amplitude
  // already processed for pwr from complex format.

  static float dbsf = 10.0f/logf(10.0f);

  int nfft2 = nfft/2;
  float *pdst = dst;
  for(int ix = nfft2; --ix >= 0;)
    *pdst++ = logf(*src++);
  vDSP_vsmul(dst,1,&dbsf,dst,1,nfft2);
}

void siglab_cbPwr(float *src, float *dst, int nfft)
{
  // requires dst to be nel/2+1 element or more
  // src is in separated Re and Im arrays, A(Im) = A(Re) + NFFT/2
  vDSP_vsq(src,1,dst,1,nfft);
  int nfft2 = nfft/2;
  vDSP_vadd(dst+1,1,dst+nfft2+1,1,dst+1,1,nfft2-1);
}

void siglab_cbMag(float *src, float *dst, int nfft)
{
  // requires dst to be nel/2+1 element or more
  // src is in separated Re and Im arrays, A(Im) = A(Re) + NFFT/2

  siglab_cbPwr(src, dst, nfft);
  for(int ix = nfft/2; --ix >= 0;)
    *dst++ = sqrt(*dst);
}

void siglab_cbPhase(float *src, float *dst, int nfft)
{
  // requires dst to be nel/2+1 element or more
  // src is in separated Re and Im arrays, A(Im) = A(Re) + NFFT/2
  int nfft2 = nfft/2;
  float *srci = src + nfft2;

  dst[0] = 0.0f;
  --nfft2;
  while(--nfft2 >= 0) // one more than N/2 to pick up Nyquist component
  {
    float re = *++src;
    float im = *++srci;
    *++dst = atan2f(im,re);
  }
}

void siglab_cbPhaseDeg(float *src, float *dst, int nfft)
{
  // requires dst to be nel/2+1 element or more
  // src is in separated Re and Im arrays, A(Im) = A(Re) + NFFT/2

  static float phsf = 45.0f/atan2f(1.0f,1.0f);

  siglab_cbPhase(src,dst,nfft);
  vDSP_vsmul(dst,1,&phsf,dst,1,nfft/2);
}

}; // extern "C"
