// fwt2d.cpp -- Fast Wavelet 2-D Transforms using the Altivec Engine
// DM/Avisere  07/04

#include "vdsp_intf.h"
#include "fwt2d.h"
#include <strings.h>

// --------------------------------------------------------------------
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

// -------------------------------------------------------------------
//
inline void blit_floats(float *src, long srcoff,
			float *dst, long dstoff,
			long nel)
{
  memcpy(dst + dstoff, src + srcoff, nel * sizeof(float));
}

inline void erase_floats(float *dst, long offset, long nel)
{
  memset(dst+offset, 0, nel*sizeof(float));
}
  
void fill_floats(float *dst, long offset, float val, long nel)
{
  for(long ix = nel; --ix >= 0;)
    dst[offset+ix] = val;
}
  
// -------------------------------------------------------------------
// -------------------------------------------------------------------
__TFWT2D_TransformerRep::~__TFWT2D_TransformerRep()
{
  discard();
}

void __TFWT2D_TransformerRep::discard()
{
  // not critical
  // have to use free_floats() since we allocated with alloc_floats()
  if(m_pkrnlmask)
    {
      free_floats((float*)m_pkrnlmask);
      m_pkrnlmask = 0;
    }
  if(m_pinpbuf)
    {
      free_floats(m_pinpbuf);
      m_pinpbuf = 0;
    }
  if(m_setup)
    {
      destroy_fftsetup(m_setup);
      m_setup = 0;
    }
}

void __TFWT2D_TransformerRep::setup_split_complex_outbuf()
{
  // use only from inside of critical section
  m_outdata.realp = *(float**)m_poutbuf;
  m_outdata.imagp = *(float**)m_poutbuf + m_tsize2;
}

void __TFWT2D_TransformerRep::implant_kernel(float *pkrnl, long nkrows, long nkcols)
{
  // use only from inside of critical section

  // implant the kernel image with zero padded boundaries
  // (input buffer assumed previously zero filled *)
  // and simultaneously separate the even/odd data in prep for 2-D FFT
  for(long iy = nkrows; --iy >= 0;)
    blit_floats(pkrnl,iy*nkcols,m_pinpbuf,iy*m_nicols,nkcols);
  
  // split complex (even,odd separation) -> outbuf
  ctoz((DSPComplex*)m_pinpbuf,2,&m_outdata,1,m_tsize2);
}

void __TFWT2D_TransformerRep::implant_image(float *pimg)
{
  // use only from inside of critical section

  // implant the user image with zero padded boundaries
  // (input buffer assumed previously zero filled *)
  // and simultaneously separate the even/odd data in prep for 2-D FFT

  if(m_nurows == m_nirows && m_nucols == m_nicols)
    // direct user image to split complex -> outbuf
    ctoz((DSPComplex*)pimg,2,&m_outdata,1,m_tsize2);
  else
    {
      // user image -> input buffer with zero padding
      for(long iy = m_nurows; --iy >= 0;)
	blit_floats(pimg,iy*m_nucols,m_pinpbuf,iy*m_nicols,m_nucols);

      // now create split complex (even,odd) separation -> outbuf
      ctoz((DSPComplex*)m_pinpbuf,2,&m_outdata,1,m_tsize2);
    }
}

void __TFWT2D_TransformerRep::fwd_fft()
{
  // use only from inside of critical section

  // in-place: outbuf -> outbuf
  fft2d_zript(m_setup, &m_outdata, 1, 0,
	      &vDSPBufferTemp, m_clog2, m_rlog2, kFFTDirection_Forward);
}

void __TFWT2D_TransformerRep::inv_fft()
{
  // use only from inside of critical section

  // in-place: outbuf -> outbuf
  fft2d_zript(m_setup, &m_outdata, 1, 0,
	      &vDSPBufferTemp, m_clog2, m_rlog2, kFFTDirection_Inverse);
}

float **create_output_buffer(long tsize)
{
  // Shared output buffer to conserve memory usage
  // Must use indirection so that clients won't be troubled if
  // the region is moved by reallocation.
  static long    stbufsiz = 0;
  static float  *stbuf    = 0;
  
  VDSP_CRITICAL_SECTION {
    if(stbufsiz < tsize)
      {
	if(0 != stbuf)
	  free_floats(stbuf);
	stbuf = must_alloc_floats(tsize);
	stbufsiz = tsize;
      }
  } END_VDSP_CRITICAL_SECTION;
  return &stbuf;
}

__TFWT2D_TransformerRep::__TFWT2D_TransformerRep()
  : TObject()
{
  bare_initialize();
}

void __TFWT2D_TransformerRep::bare_initialize()
{
  // in prep for possible failure
  m_pkrnlmask = 0;
  m_pinpbuf   = 0;
  m_setup     = 0;  
}

__TFWT2D_TransformerRep::__TFWT2D_TransformerRep(long nirows, long nicols)
  : TObject()
{
  bare_initialize();
  try {
    //
    // Now, even though initialize() is declared as virtual, 
    // this does not call any other than the immediately available 
    // function, which follows this one. Strange but true....
    // So if you need an outer one to be called, your best bet is to 
    // create the mother instance as a default constructor and then
    // call your own initialize which then calls the one below...
    //
    initialize(nirows, nicols);
  }
  catch(...)
    {
      discard();
      throw;
    }
}

void __TFWT2D_TransformerRep::initialize(long nirows, long nicols)
{
  // first release any existing buffers and reset to bare state
  // this accommodates repeated initialization
  discard();
 
  // record user spec'd image sizes for future implanting
  m_nucols = nicols;
  m_nurows = nirows;
  m_nucols2 = nicols/2; // cached value
  
  // adjust user image sizes to next power of two size
  // for internal buffer and FFT sizes
  m_nicols = next_pow2(nicols);
  m_nirows = next_pow2(nirows);
  
  m_nicols2 = m_nicols/2;  // cached value
  
  // compute the useful log2 quantities for FFT
  m_clog2 = ilog2(m_nicols);
  m_rlog2 = ilog2(m_nirows);

  // check size limitations
  if(m_rlog2 < 3 || m_clog2 < 3)
    throw("TFWT2D_Transformer: Image size too small (each axis must be >= 5)");

  if(m_rlog2 > 8)
    throw("TFWT2D_Transformer: Row Size must be 256 or smaller");

  // get the FFT twiddles for the larger dimension
  m_setup = create_fftsetup(max(m_rlog2, m_clog2), RADIX_2);
  if(0 == m_setup)
    throw("TFWT2D_Transformer: unable to construct FFT twiddle table");
  
  // allocate our carefully aligned buffers
  // use fftw_malloc to get favorable alignment
  long tsize  = m_nicols * m_nirows;
  m_tsize2    = tsize/2;
  m_pkrnlmask = (DSPComplex*)must_alloc_floats(tsize);
  m_pinpbuf   = must_alloc_floats(tsize);
  m_poutbuf   = (DSPComplex**)create_output_buffer(tsize);
  
  // zero out the input buffer for kernel implant
  // as long as image size boundaries are respected
  // there will be no further need to pre-zero this buffer
  erase_floats(m_pinpbuf, 0, tsize);
}

void __TFWT2D_TransformerRep::check_valid_transformer()
{
  if(0 == m_setup   ||
     0 == m_pinpbuf ||
     0 == m_pkrnlmask)
    throw("TFWT2D_Transformer: transformer object not initialized");
}

void __TFWT2D_TransformerRep::init_kernel(float *pkrnl, long nkrows, long nkcols)
{
  // should never happen... but disallow kernel sizes larger
  // than the expected image size. Even though we could theoretically
  // accommodate to the ceiling power of two in size that would destroy
  // the zero fill needed for user image padding...

  check_valid_transformer();
  
  if(nkcols > m_nucols || nkrows > m_nurows)
    throw("TFWT2D_Transformer: kernel size too large.");

  VDSP_CRITICAL_SECTION {
    // prep output buffer for this run
    setup_split_complex_outbuf();
    
    // just move the kernel into the input buffer without regard
    // for its phase center. We will correct afterward.
    implant_kernel(pkrnl, nkcols, nkrows);
    
    // perform the forward FFT of the kernel
    fwd_fft();
    
    // Correct the phase center. We only nead real-valued amplitudes.
    // Prep for use as an element-by-element real-real vector multipy
    // this uses a bit more memory but it is faster in the face of the
    // 2-D packed output format of the real-to-complex FFT
    
    // first get back to packed format from this wierd output format provided
    ztoc(&m_outdata, 1, m_pkrnlmask, 2, m_tsize2);
    
    // then, for each row and every complex column except the first...
    for(long iy = m_nirows; --iy >= 0; )
      {
	long off = iy * m_nicols2;
	for(long ix = m_nicols2; --ix > 0; )
	  {
	    long   k  = off + ix;
	    double re = (double)m_pkrnlmask[k].real;
	    double im = (double)m_pkrnlmask[k].imag;
	    float v   = (float)hypot(re,im);
	    (*m_poutbuf)[k].real = v;
	    (*m_poutbuf)[k].imag = v;
	  }
      }
    // Then in the first complex column, for all rows except the first two...
    float *pdst = *(float**)m_poutbuf;
    float *psrc = (float*)m_pkrnlmask;
    for(long iy = 2; iy < m_nirows; iy += 2)
      {
	long kre = iy*m_nicols;
	long kim = kre + m_nicols;
	double re = (double)psrc[kre];
	double im = (double)psrc[kim];
	float v   = (float)hypot(re,im);
	pdst[kre] = v;
	pdst[kim] = v;
	
	++kre;
	++kim;
	re = psrc[kre];
	im = psrc[kim];
	v = (float)hypot(re,im);
	pdst[kre] = v;
	pdst[kim] = v;
      }
    // finally, copy over the remaining untouched 4 cells
    for(long iy = 2; --iy >= 0;)
      {
	long off = iy * m_nicols;
	for(long ix = 2; --ix >= 0;)
	  {
	  long k = ix + off;
	  pdst[k] = psrc[k];
	  }
      }
    // now refold the kernel so that we can do straight multiplies in the filter
    DSPSplitComplex kdata = {(float*)m_pkrnlmask,
			     (float*)m_pkrnlmask + m_tsize2};
    ctoz(*m_poutbuf, 2, &kdata, 1, m_tsize2);

  } END_VDSP_CRITICAL_SECTION;

  // apply descaling for round-trip amplification
  // = *2 for forward FFT on image
  // = *tsize for inverse FFT on image
  // = *2 for forward FFT on kernel
  long tsize = m_nicols * m_nirows;
  float sf = 0.25/tsize;
  
  vsmul((const float*)m_pkrnlmask, 1, &sf,
	(float*)m_pkrnlmask, 1, tsize);
}

void __TFWT2D_TransformerRep::filter()
{
  // use only from inside of critical section

  // multiply the forward FFT of the image by the filter amplitudes
  // but treat the whole operation as one large real*real vector multiply
  vmul((float*)m_pkrnlmask, 1,  // src1
       *(float**)m_poutbuf, 1,    // src2
       *(float**)m_poutbuf, 1,    // dst
       m_nicols*m_nirows);      // total size
}

void __TFWT2D_TransformerRep::extract_decimated_image(float *pdst)
{
  // use only from inside of critical section

  // result is now a split-complex real valued filtered-image in outbuf
  // which means that even and odd samples have been separated.
  // Hence to decimate, just copy the even values half of the image array

  // for every other row...
  for(long iy = m_nurows/2; --iy >= 0; )
    blit_floats(*(float**)m_poutbuf, iy*m_nicols,
		pdst, iy*m_nucols2,
		m_nucols2);
}

void __TFWT2D_TransformerRep::transform(float *pimg, float *pdst)
{
  // perform the 2D FWT on the incoming image
  // and store the decimated transform image in user destination

  check_valid_transformer();
  
  VDSP_CRITICAL_SECTION {

    // prep output buffer for this run...
    setup_split_complex_outbuf();
    
    // implant the user image with zero padded boundaries
    // and simultaneously separate the even/odd data in prep for 2-D FFT
    // user image -> inpbuf
    implant_image(pimg);
    
    // perform the forward FFT
    fwd_fft();
    
    // perform FWT filtering
    filter();
    
    // now inverse transform the filtered image spectrum
    inv_fft();
    
    // return result in user destination
    extract_decimated_image(pdst);

  } END_VDSP_CRITICAL_SECTION;
}

// --- testing ---
float* __TFWT2D_TransformerRep::get_fkernel(long *pnrows, long *pncols)
{
  // not critical
  
  *pncols = m_nicols;
  *pnrows = m_nirows;
  return (float*)m_pkrnlmask;
}

// -------------------------------------------------------------------
// Daubechies 9 and 7 forward QMF's as per Dr. Tinku Acharya/Avisere
//
static double Daub9L[9] = {
  0.026748757410810,
  -0.016864118442875,
  -0.078223266528988,
  0.266864118442872,
  0.602949018236358,
  0.266864118442872,
  -0.078223266528988,
  -0.016864118442875,
  0.026748757410810 };

static double Daub7H[7] = {
  0.0912717631142495,
  -0.057543526228500,
  -0.591271763114247,
  1.115087052456994,
  -0.591271763114247,
  -0.057543526228500,
  0.0912717631142495 };

// -------------------------------------------------------------------
TFWT2D_Transformer::TFWT2D_Transformer()
  :THandle<__TFWT2D_TransformerRep>(new __TFWT2D_TransformerRep)
{}

TFWT2D_Transformer::TFWT2D_Transformer(long nirows, long nicols)
  :THandle<__TFWT2D_TransformerRep>(new __TFWT2D_TransformerRep)
{
  rep()->initialize(nirows, nicols);
}
  
// -------------------------------------------------------------------
TFWT2D_Daub97_HL::TFWT2D_Daub97_HL()
  :THandle<__TFWT2D_TransformerRep>(new __TFWT2D_TransformerRep)
{}

TFWT2D_Daub97_HL::TFWT2D_Daub97_HL(long nirows, long nicols)
  :THandle<__TFWT2D_TransformerRep>(new __TFWT2D_TransformerRep)
{
  initialize(nirows, nicols);
}

void TFWT2D_Daub97_HL::initialize(long nirows, long nicols)
{
  float krnl[KHEIGHT*KWIDTH];

  for(long iy = KHEIGHT; --iy >= 0;)
    for(long ix = KWIDTH; --ix >= 0; )
      krnl[iy*KWIDTH+ix] = (float)(Daub7H[iy]*Daub9L[ix]);

  rep()->initialize(nirows, nicols);
  rep()->init_kernel(krnl, KWIDTH, KHEIGHT);
}

// -------------------------------------------------------------------
TFWT2D_Daub97_LH::TFWT2D_Daub97_LH()
  :THandle<__TFWT2D_TransformerRep>(new __TFWT2D_TransformerRep)
{}

TFWT2D_Daub97_LH::TFWT2D_Daub97_LH(long nirows, long nicols)
  :THandle<__TFWT2D_TransformerRep>(new __TFWT2D_TransformerRep)
{
  initialize(nirows, nicols);
}

void TFWT2D_Daub97_LH::initialize(long nirows, long nicols)
{
  float krnl[KHEIGHT*KWIDTH];

  for(long iy = KHEIGHT; --iy >= 0;)
    for(long ix = KWIDTH; --ix >= 0; )
      krnl[iy*KWIDTH+ix] = (float)(Daub9L[iy]*Daub7H[ix]);

  rep()->initialize(nirows, nicols);
  rep()->init_kernel(krnl, KWIDTH, KHEIGHT);
}

// -----------------------------------------------------------
// C User API
//
extern "C"
{
  FWTSetup fwt2d_make_transformer(long nirows, long nicols)
  {
    // args are anticipated image size
    TFWT2D_Transformer xform(nirows, nicols);
    return xform.get_rep();
  }
  
  FWTSetup fwt2d_make_daub97_hl_transformer(long nirows, long nicols)
  {
    // args are anticipated image size
    TFWT2D_Daub97_HL xform(nirows, nicols);
    return xform.get_rep();
  }
  
  FWTSetup fwt2d_make_daub97_lh_transformer(long nirows, long nicols)
  {
    // args are anticipated image size
    TFWT2D_Daub97_LH xform(nirows, nicols);
    return xform.get_rep();
  }
  
  void  fwt2d_discard_transformer(FWTSetup transformer)
  {
    // call this function when finished with all FWT's
    Release(transformer);
  }
  
  void  fwt2d_init_transformer_kernel(FWTSetup transformer, float *pkernel, 
				      long nkrows, long nkcols)
  {
    // install or reinstall a transform filter kernel
    transformer->init_kernel(pkernel, nkrows, nkcols);
  }
  
  void  fwt2d_transform_image(FWTSetup transformer, float *pimage, float *pdst)
  {
    // perform the FWT transforming image to decimated result in pdst
    transformer->transform(pimage, pdst);
  }
  
  // testing...
  float *fwt2d_get_fkernel(FWTSetup transformer, long *pnrows, long *pncols)
  {
    return transformer->get_fkernel(pnrows, pncols);
  }
}; // extern "C"

// -- end of fwt2d.cpp -- //
