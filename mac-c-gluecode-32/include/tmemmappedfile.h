// tmemmappedfile.h -- Interface for Memory Mapped Datafiles
// DM/Avisere  07/04
//

#ifndef __TMEMMAPEDFILE_H__
#define __TMEMMAPEDFILE_H__

#include "tobject.h"
#include <fcntl.h>

class __TMemMappedFileRep;
// ---------------------------------------------------------------
class TMemMappedFile: public THandle<__TMemMappedFileRep>
{
 public:
  TMemMappedFile(const char *filename,
		 int flags = (O_RDWR | O_CREAT),
		 mode_t mode = 0666);

  // construct a dup mapping of the argument mapped file.
  TMemMappedFile(TMemMappedFile &aFile);
};
  
// ---------------------------------------------------------------
// Representation object for memory mapped files.
// Instances of this class should never be directly instantiated.
// Use TMemMappedFile instead, which behaves as a pointer to one of these.
//
class __TFileRep;
class __TMemMappedFileRep: public TObject
{
 public:
  __TMemMappedFileRep(const char *filename, int flags, mode_t mode);

  virtual ~__TMemMappedFileRep();

  // create a duplicate mapping of the underlying file
  // this mapping is independent from the original,
  // but Mach ensures that pages of the file remain coherent
  // in the two mappings...
  __TMemMappedFileRep* dup();
  
  virtual void* addr(ulong ix);
    
  inline schar&  schar_at(ulong ix)
    { return *(schar*)addr(ix); }
    
  inline uchar&  uchar_at(ulong ix)
    { return *(uchar*)addr(ix); }
    
  inline short&  short_at(ulong ix)
    { return *(short*)addr(ix); }
    
  inline ushort& ushort_at(ulong ix)
    { return *(ushort*)addr(ix); }
    
  inline long&   long_at(ulong ix)
    { return *(long*)addr(ix); }
    
  inline ulong&  ulong_at(ulong ix)
    { return *(ulong*)addr(ix); }
    
  inline float&  float_at(ulong ix)
    { return *(float*)addr(ix); }
    
  inline double& double_at(ulong ix)
    { return *(double*)addr(ix); }
    
  // All addressing is relative to this offset,
  // and since addresses are always unsigned,
  // they cannot peek below this lower limit.
  // Initially zero, but might want to change
  // to skip headers, etc.
  inline void set_base(ulong base)
    { m_fbase = base; }
  
  // set how much to map at any one time
  // this is also the increment of file growth
  void set_regionsize(ulong size);

  // return the current expected region size
  ulong get_regionsize()
  { return m_vframes; }
    
  // how many bytes remain to end of mapped region?
  inline ulong room_left(ulong offset)
    { return (m_limit - (offset + m_fbase)); }
    
  // is this offset mapped into memory?
  inline bool in_core(ulong offset)
    { ulong actual = offset + m_fbase;
      return (m_foff <= actual && actual < m_limit); }

  // return physical address of offset and the room remaining
  // to the end of the mapped region from that offset.
  void get_safe_limits(ulong offset, void* &p, ulong &size);
    
 protected:
  virtual void sync_vmem();

  __TFileRep   *m_file;
  ulong         m_fbase;        /* addressing lower limit */
  ulong         m_foff;		/* pagesize aligned file offset */
  int           m_mflags;	/* mmap access flags */
  int           m_mmode;        /* mmap mode flags */
  uchar        *m_vbase;	/* addr of mmaped region */
  ulong         m_vsize;	/* size of mmaped ragion */
  ulong         m_vframes;	/* size of region to map (multiple of pgsize) */
    
  ulong         m_limit;        /* cached limit = m_foff + m_vsize */
  ulong         m_pgsiz;        /* cached system page size */
    
  enum {LOGVSIZ = 20};  // 1 MB vmsize
    
 private:
  __TMemMappedFileRep();
  __TMemMappedFileRep(__TMemMappedFileRep &aTMFile);
  __TMemMappedFileRep& operator=(__TMemMappedFileRep &aTMFile);
};

// ---------------------------------------------------------------
struct TMemMappedFileError
{
  const char *m_msg1;
  const char *m_msg2;

  void init(const char *msg1);
};

// ---------------------------------------------------------------
class TMMPtr: public THandle<__TMemMappedFileRep>
{
 public:
  inline TMMPtr(TMemMappedFile &pfile, ulong offset = 0)
    : THandle<__TMemMappedFileRep>(pfile), m_off(offset)
    {}
	
    TMMPtr(TMMPtr& aPtr)
      : THandle<__TMemMappedFileRep>(aPtr)
      { m_off = aPtr.m_off; }

    inline TMMPtr& operator=(TMMPtr &aPtr)
      { THandle<__TMemMappedFileRep>::operator=(aPtr);
	m_off  = aPtr.m_off;
	return *this; }
    
    inline ulong base()
      { return m_off; }
	
    inline __TMemMappedFileRep *file()
      { return rep(); }
	
    inline bool operator<(ulong limit)
      { return (m_off < limit); }

    inline bool operator<(int limit)
      { return (m_off < (ulong)limit); }

    inline bool operator<=(ulong limit)
      { return (m_off <= limit); }

    inline bool operator<=(int limit)
      { return (m_off <= (ulong)limit); }

    inline bool operator>=(ulong limit)
      { return (m_off >= limit); }

    inline bool operator>=(int limit)
      { return (m_off >= (ulong)limit); }

    inline bool operator>(ulong limit)
      { return (m_off > limit); }

    inline bool operator>(int limit)
      { return (m_off > (ulong)limit); }

    inline bool operator==(ulong limit)
      { return (m_off == limit); }

    inline bool operator==(int limit)
      { return (m_off == (ulong)limit); }

    inline bool operator!=(ulong limit)
      { return (m_off != limit); }
	
    inline bool operator!=(int limit)
      { return (m_off != (ulong)limit); }

    inline bool operator!()
      { return (!m_off); }
		
    inline operator unsigned long()
      { return m_off; }

 protected:
  ulong  m_off;
};

// ---------------------------------------------------------------
inline void copybytes(void *dst, void *src, ulong nbytes)
{ memcpy(dst, src, nbytes); }

void copybytes(TMMPtr &dst, void *src, ulong nbytes);
void copybytes(void *dst, TMMPtr &src, ulong nbytes);
void copybytes(TMMPtr &dst, TMMPtr &src, ulong nbytes);

// ---------------------------------------------------------------
class TMMPtrI1 : public TMMPtr
{
 public:
  inline TMMPtrI1(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtr(pfile,offset)
    {}

    inline TMMPtrI1(TMMPtrI1 &aPtr)
      : TMMPtr(aPtr)
      {}
      
    inline ulong operator=(TMMPtrI1& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }

    inline schar& operator[](ulong ix)
      { return (rep()->schar_at(m_off + ix)); }
    
    inline ulong operator++()
      { return (m_off += 1); }
	
    inline ulong operator++(int)
      { ulong val = m_off++; return val; }

    inline ulong operator--()
      { return (m_off -= 1); }
	
    inline ulong operator--(int)
      { ulong val = m_off--; return val; }

    inline ulong operator+=(long dix)
      { return (m_off += dix); }
	
    inline ulong operator-=(long dix)
      { return (m_off -= dix); }
	
    inline ulong operator-(TMMPtr &aPtr)
      { return (m_off - aPtr.base()); }
	
    inline ulong operator+(long dix)
      { return (m_off + dix); }

    inline schar& operator*()
      { return (rep()->schar_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrU1 : public TMMPtrI1
{
 public:
  inline TMMPtrU1(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtrI1(pfile,offset)
    {}
	
    inline TMMPtrU1(TMMPtrU1 &aPtr)
      : TMMPtrI1(aPtr)
      {}
      
    inline ulong operator=(TMMPtrU1& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline uchar& operator[](ulong ix)
      { return (rep()->uchar_at(m_off + ix)); }

    inline uchar& operator*()
      { return (rep()->uchar_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrI2 : public TMMPtr
{
 public:
  inline TMMPtrI2(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtr(pfile,offset)
    {}
	
    inline TMMPtrI2(TMMPtrI2 &aPtr)
      : TMMPtr(aPtr)
      {}
      
    inline ulong operator=(TMMPtrI2& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline short& operator[](ulong ix)
      { return (rep()->short_at(m_off + 2*ix)); }

    inline ulong operator++()
      { return (m_off += 2); }

    inline ulong operator++(int)
      { ulong val = m_off; m_off += 2; return val; }

    inline ulong operator--()
      { return (m_off -= 2); }

    inline ulong operator--(int)
      { ulong val = m_off; m_off -= 2; return val; }

    inline ulong operator+=(long dix)
      { return (m_off += 2*dix); }

    inline ulong operator-=(long dix)
      { return (m_off -= 2*dix); }

    inline ulong operator-(TMMPtr &aPtr)
      { return ((m_off - aPtr.base())/2); }

    inline ulong operator+(long dix)
      { return (m_off + 2*dix); }

    inline short& operator*()
      { return (rep()->short_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrU2 : public TMMPtrI2
{
 public:
  inline TMMPtrU2(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtrI2(pfile,offset)
    {}
	
    inline TMMPtrU2(TMMPtrU2 &aPtr)
      : TMMPtrI2(aPtr)
      {}
      
    inline ulong operator=(TMMPtrU2& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline ushort& operator[](ulong ix)
      { return (rep()->ushort_at(m_off + 2*ix)); }

    inline ushort& operator*()
      { return (rep()->ushort_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrI4 : public TMMPtr
{
 public:
  inline TMMPtrI4(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtr(pfile,offset)
    {}
	
    inline TMMPtrI4(TMMPtrI4 &aPtr)
      : TMMPtr(aPtr)
      {}
      
    inline ulong operator=(TMMPtrI4& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline long& operator[](ulong ix)
      { return (rep()->long_at(m_off + 4*ix)); }

    inline ulong operator++()
      { return (m_off += 4); }

    inline ulong operator++(int)
      { ulong val = m_off; m_off += 8; return val; }

    inline ulong operator--()
      { return (m_off -= 4); }

    inline ulong operator--(int)
      { ulong val = m_off; m_off -= 4; return val; }

    inline ulong operator+=(long dix)
      { return (m_off += 4*dix); }

    inline ulong operator-=(long dix)
      { return (m_off -= 4*dix); }

    inline ulong operator-(TMMPtr &aPtr)
      { return ((m_off - aPtr.base())/4); }

    inline ulong operator+(long dix)
      { return (m_off + 4*dix); }

    inline long& operator*()
      { return (rep()->long_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrU4 : public TMMPtrI4
{
 public:
  inline TMMPtrU4(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtrI4(pfile,offset)
    {}
	
    inline TMMPtrU4(TMMPtrU4 &aPtr)
      : TMMPtrI4(aPtr)
      {}
      
    inline ulong operator=(TMMPtrU4& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline ulong& operator[](ulong ix)
      { return (rep()->ulong_at(m_off + 4*ix)); }

    inline ulong& operator*()
      { return (rep()->ulong_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrF4 : public TMMPtrI4
{
 public:
  inline TMMPtrF4(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtrI4(pfile,offset)
    {}
	
    inline TMMPtrF4(TMMPtrF4 &aPtr)
      : TMMPtrI4(aPtr)
      {}
      
    inline ulong operator=(TMMPtrF4& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline float& operator[](ulong ix)
      { return (rep()->float_at(m_off + 4*ix)); }

    inline float& operator*()
      { return (rep()->float_at(m_off)); }
};

// ---------------------------------------------------------------
class TMMPtrF8 : public TMMPtr
{
 public:
  inline TMMPtrF8(TMemMappedFile &pfile, ulong offset = 0)
    :TMMPtr(pfile,offset)
    {}
    
    inline TMMPtrF8(TMMPtrF8 &aPtr)
      : TMMPtr(aPtr)
      {}
      
    inline ulong operator=(TMMPtrF8& aPtr)
      { return TMMPtr::operator=(aPtr); }

    inline ulong operator=(ulong offset)
      { return (m_off = offset); }
	
    inline ulong operator=(int offset)
      { return (m_off = (ulong)offset); }
	
    inline double& operator[](ulong ix)
      { return (rep()->double_at(m_off + 8*ix)); }

    inline ulong operator++()
      { return (m_off += 8); }
	
    inline ulong operator++(int)
      { ulong val = m_off; m_off += 8; return val; }

    inline ulong operator--()
      { return (m_off -= 8); }

    inline ulong operator--(int)
      { ulong val = m_off; m_off -= 8; return val; }

    inline ulong operator+=(long dix)
      { return (m_off += 8*dix); }

    inline ulong operator-=(long dix)
      { return (m_off -= 8*dix); }

    inline ulong operator-(TMMPtr &aPtr)
      { return ((m_off - aPtr.base())/8); }

    inline ulong operator+(long dix)
      { return (m_off + 8*dix); }

    inline double& operator*()
      { return (rep()->double_at(m_off)); }
};

// ---------------------------------------------------------------

#endif // __TMEMMAPEDFILE_H__

// -- end of tmemmappedfile.h -- //
