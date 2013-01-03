// vdsp_intf.cpp -- routines used to prep for vDSP FFT routines.
// DM/Avisere  07/04
//
// This routine is needed by both the FFT glue code for Altivec
// and by the FWT library. No point duplicating the memory buffer
// ------------------------------------------------------------------

#include "vdsp_intf.h"
#include "pthread.h"

static TLock vdsp_mutex(TLock::RECURSIVE);

TvDSP_Locker::TvDSP_Locker()
  :TLocker(vdsp_mutex)
{}

// -------------------------------------------------------------------
extern "C"
void *alloc_align16(int nbytes)
{
  // allocate to 16-byte alignment

  // need to allocate enough room so that there will
  // be space for a pointer back to the allocation base address
  // just in front of the actual float array.
  void *p = malloc(nbytes+19);
  if(0 == p) return 0;
  void *q = (void*)(((unsigned long)p+19) & ~15);
  ((void**)q)[-1] = p;
  return q;
}

extern "C"
void free_align16(void *p)
{
  // only works on regions allocated by alloc_align16
  if(0 != p)
    {
      void *q = ((void**)p)[-1];
      free(q);
    }
}
  
extern "C"
void *must_alloc_align16(int nbytes)
{
  void *p = alloc_align16(nbytes);
  if(0 == p)
    throw("vdsp_intf::must_alloc_align16 - memory allocation failure");
  return p;
}

// -------------------------------------------------------------------

extern "C"
void *alloc_align32(int nbytes)
{
  // allocate to 32-byte alignment

  // need to allocate enough room so that there will
  // be space for a pointer back to the allocation base address
  // just in front of the actual float array.
  void *p = malloc(nbytes+35);
  if(0 == p) return 0;
  void *q = (void*)(((unsigned long)p+35) & ~31);
  ((void**)q)[-1] = p;
  return q;
}

extern "C"
void free_align32(void *p)
{
  // only works on regions allocated by alloc_align16 or alloc_align32
  free_align16(p);
}
  
extern "C"
void *must_alloc_align32(int nbytes)
{
  void *p = alloc_align32(nbytes);
  if(0 == p)
    throw("vdsp_intf::must_alloc_align32 - memory allocation failure");
  return p;
}

// -------------------------------------------------------------------
// Temp working area for Altivec
// min of 16KB or 4*n -- just use 16KB
// will be filled in on first use
//
DSPDoubleSplitComplex vDSPBufferTemp = {0,0};

static int ensure_vdsp_init()
{
  // first time only
  // min of 16KB or 4*n -- just use 16KB
  vDSPBufferTemp.realp = (double*)must_alloc_align32(16384);
  vDSPBufferTemp.imagp = (double*)must_alloc_align32(16384);
  return 1;
}

int vdsp_inited = ensure_vdsp_init();

// -- end of vdsp_intf.cpp -- //
