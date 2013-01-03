/*
 *  fwt2d.h -- Fast Altivec 2-D Wavelet Transforms
 *
 *  Created by David McClain on Sun Jul 18 2004.
 *  Copyright (c) 2004 Avisere. All rights reserved.
 *
 */

#ifndef __FWT2D_H__
#define __FWT2D_H__

#include <vecLib/vecLib.h>
#include "TObject.h"

// -------------------------------------------------------------------
// 2-D Fast Wavelet Transforms via FFT Convolution
//
// NOTE: This class is an internal representation class. It should not
// be directly instantiated. Use TFWT2D_Transform instead, which acts
// like a pointer to one of these representations.
//
class __TFWT2D_TransformerRep: public TObject
{
public:
  // ----- C++ User API ---------------------
  // setup transformer for a specific image size
  __TFWT2D_TransformerRep();
  __TFWT2D_TransformerRep(long nirows, long nicols);
  virtual ~__TFWT2D_TransformerRep();
  
  // this must be called immediately after object construction
  // and before any other methods are called.
  virtual void initialize(long nirows, long nicols);
  
  // install a transform kernel
  void init_kernel(double *pkrnl, long nkrows, long nkcols);
  
  // perform the 2-D FWT
  void transform(double *pimg, double *pdst);
  // ----- End of C++ User API ---------------------
  
  // --- testing ---
  double* get_fkernel(long *pnrows, long *pncols);
	
protected:
  DSPDoubleComplex  *m_pkrnlmask;  // scalar filter amplitudes
  double       *m_pinpbuf;    // split-complex input working buffer
  DSPDoubleComplex **m_poutbuf;    // packed 2-D complex output working buffer

  OpaqueFFTSetupD *m_setup; // FFT twiddles for largest dimension

  long m_nucols; // user image sizes
  long m_nurows;
  
  long m_nicols; // internal next pwr of 2 image sizes
  long m_nirows;
  
  long m_clog2;  // log2 of internal image size
  long m_rlog2;

  DSPDoubleSplitComplex m_outdata; // cached precomps
  long m_nucols2; // cached value
  long m_nicols2; // cached value
  long m_tsize2;  // cached value

	// worker routines for this class and its children
  void bare_initialize();
  void check_valid_transformer();
  void discard();
  void implant_kernel(double *pimg, long nkrows, long nkcols);
  void implant_image(double *pimg);
  void fwd_fft();
  void filter();
  void inv_fft();
  void extract_decimated_image(double *pdst);  
  void setup_split_complex_outbuf();
  void fill_in(long kre, long kim);
  
private:
  // these are private so that they can't be used...
  __TFWT2D_TransformerRep(__TFWT2D_TransformerRep &aSetup);
  __TFWT2D_TransformerRep& operator=(__TFWT2D_TransformerRep &aSetup);
};

// -------------------------------------------------------------------
// The THolder class for an FWT Transformer representation
class TFWT2D_Transformer: public THandle<__TFWT2D_TransformerRep>
{
 public:
  TFWT2D_Transformer();
  TFWT2D_Transformer(long nirows, long nicols);
};

// -------------------------------------------------------------------
// Specialization for the Daub9,7 HL Transform
class TFWT2D_Daub97_HL : public THandle<__TFWT2D_TransformerRep>
{
public:
  TFWT2D_Daub97_HL();
  TFWT2D_Daub97_HL(long nirows, long nicols);
  
  void initialize(long nirows, long nicols);
};

// -------------------------------------------------------------------
// Specialization for the Daub9,7 LH Transform
class TFWT2D_Daub97_LH : public THandle<__TFWT2D_TransformerRep>
{
public:
  TFWT2D_Daub97_LH();
  TFWT2D_Daub97_LH(long nirows, long nicols);

  void initialize(long nirows, long nicols);

private:
  enum {KWIDTH = 7, KHEIGHT = 9};
};

// -------------------------------------------------------------------
// C User API
//
extern "C"
{
  typedef __TFWT2D_TransformerRep* FWTSetupD;
  FWTSetupD fwt2d_make_transformer(long nirows, long nicols);
  FWTSetupD fwt2d_make_daub97_hl_transformer(long nirows, long nicols);
  FWTSetupD fwt2d_make_daub97_lh_transformer(long nirows, long nicols);
  void      fwt2d_discard_transformer(FWTSetupD transformer);
  void      fwt2d_init_transformer_kernel(FWTSetupD transformer, double *pkernel, 
					 long nkrows, long nkcols);
  void     fwt2d_transform_image(FWTSetupD transformer, 
                                 double *pimage, double *pdst);
								 
  // testing
  double *fwt2d_get_fkernel(FWTSetupD transformer, long *pnrows, long *pncols);
};

#endif // __FWT2D_H__

// -- end of fwt2d.h -- //
