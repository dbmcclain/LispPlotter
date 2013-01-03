// tobject.h -- Top Class for Reference Counted Objects
// DM/Avisere  07/04
//
#ifndef __TOBJECT_H__
#define __TOBJECT_H__

typedef signed char    schar;
typedef unsigned char  uchar;
typedef unsigned short ushort;
typedef unsigned long  ulong;

// ---------------------------------------------------------------
// TObject -- generic topclass with reference counting

class TObject
{
 public:
  inline TObject()
    { m_refc = 1; }
  
  inline virtual ~TObject() {}

  inline long incref()
  { return ++m_refc; }

  inline long decref()
  { return --m_refc; }
  
  inline TObject& operator=(TObject &anObject)
  { return *this; }		/* should be disallowed */

 protected:
  long   m_refc;

 private:
  TObject(TObject &anObject);
};

// ---------------------------------------------------------------
// for TObject pointers
inline void AddRef(TObject *anObject)
{ if(0 !=anObject)
    anObject->incref(); }

inline void Release(TObject *anObject)
{ if(0 != anObject && 0 >= anObject->decref())
    delete anObject; }

// ---------------------------------------------------------------
// TObjectHandle - counted pointers to TObjects
class TObjectHandle
{
 public:
  inline TObjectHandle()
    { m_rep = 0; }

  inline TObjectHandle(TObjectHandle &aHandle)
    { m_rep = aHandle.m_rep;
      AddRef(m_rep); }
  
  inline virtual ~TObjectHandle()
    { Release(m_rep); }

  inline TObjectHandle& operator=(TObjectHandle &aHandle)
    { AddRef(aHandle.m_rep);
      Release(m_rep);
      m_rep = aHandle.m_rep;
      return *this; }

 protected:
  TObject *m_rep;

  inline TObjectHandle(TObject *aRep)
    { m_rep = aRep; }
  
};

// -------------------------------------------------------------
// Template class THandle. Its templated type must derive from TObject.
//
template <class __TRep>
class THandle: public TObjectHandle
{
 public:
  inline THandle<__TRep>()
    :TObjectHandle()
    {}

  inline THandle<__TRep>(__TRep *aRep)
    :TObjectHandle(aRep)
    {}
  
  inline THandle<__TRep>(THandle<__TRep> &aHandle)
    :TObjectHandle(aHandle)
    {}
  
  inline THandle<__TRep>& operator=(THandle<__TRep> &aHandle)
    { TObjectHandle::operator=(aHandle);
      return *this;}

  inline __TRep* operator->() const
    { return (__TRep*)m_rep; }

  // caller should call Release on this returned object when finished.
  inline __TRep* get_rep()
    { AddRef(m_rep);
      return (__TRep*)m_rep; }

 protected:
  // used by derived classes to get m_rep specifically typed.
  inline __TRep* rep() const
    { return (__TRep*)m_rep; }
};

// -------------------------------------------------------

#endif // __TOBJECT_H__

// -- end of tobject.h -- //
