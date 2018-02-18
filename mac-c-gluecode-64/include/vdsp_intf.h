// vdsp_intf.h -- routines used to prep for vDSP FFT routines.
// DM/Avisere  07/04
//
// This routine is needed by both the FFT glue code for Altivec
// and by the FWT library. No point duplicating the memory buffer
// ------------------------------------------------------------------

#ifndef __VDSP_INTF_H__
#define __VDSP_INTF_H__

#include <Accelerate/Accelerate.h>
#include "tlocker.h"

// ----------------------------------------

extern "C"
DSPDoubleSplitComplex vDSPBufferTemp;

// Routines to allocate floats on a guaranteed alignment boundary of 16 bytes.
// Note: if you allocate with alloc_floats() or must_alloc_floats()
// then you absolutely must free with free_floats().
// Doing otherwise will cause fatal exceptions
extern "C"
void *alloc_align16(int nbytes);

extern "C"
void *must_alloc_align16(int nbytes);

extern "C"
void free_align16(void *p);

extern "C"
void *alloc_align32(int nbytes);

extern "C"
void *must_alloc_align32(int nbytes);

extern "C"
void free_align32(void *p);

// ----------------------------------------------
inline float* alloc_floats(int nel)
{ return (float*)alloc_align16(nel * sizeof(float)); }

inline float* must_alloc_floats(int nel)
{ return (float*)must_alloc_align16(nel * sizeof(float)); }

inline void free_floats(float *p)
{ free_align16(p); }

// ----------------------------------------------
inline double* alloc_doubles(int nel)
{ return (double*)alloc_align16(nel * sizeof(double)); }

inline double* must_alloc_doubles(int nel)
{ return (double*)must_alloc_align16(nel * sizeof(double)); }

inline void free_doubles(double *p)
{ free_align16(p); }

// -------------------------------------------------------------------------
// Designing a class object to do the lock/unlock of the mutex
// is much safer because the destructor will be called no matter
// how the enclosing block is exited -- whether by normal procession,
// or by early exit (break, return), or by exception (throw).
//
// To use this properly, simply enclose any critical section by braces "{ }"
// and create a TLocker object at the top:
// E.g.:
//
//            ...
//            {
//               TvDSP_Lock lock;
//               <critical code>
//            }
//            ...
//
class TvDSP_Locker : public TLocker
{
 public:
  TvDSP_Locker();
};

#define VDSP_CRITICAL_SECTION     { TvDSP_Locker __locker;
#define END_VDSP_CRITICAL_SECTION }

#endif  // __VDSP_INTF_H__

// -- end of vdsp_intf.h -- //
