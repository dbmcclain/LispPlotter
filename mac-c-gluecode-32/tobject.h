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
// for TObject refs
inline void AddRef(TObject &anObject)
{ if(0 != &anObject)
    anObject.incref(); }

inline void Release(TObject &anObject)
{ if(0 != &anObject && 0 >= anObject.decref())
    delete &anObject; }
  
// ---------------------------------------------------------------
// THandle - counted pointers to TObjects
template <class __TRep>
class THandle
{
 public:
  inline THandle<__TRep>()
    { m_rep = 0; }

  inline THandle<__TRep>(__TRep *aRep)
    { m_rep = aRep;
      AddRef(aRep); }
  
  inline THandle<__TRep>(THandle<__TRep> &aHandle)
    { m_rep = aHandle.m_rep;
      AddRef(m_rep); }
  
  inline virtual ~THandle<__TRep>()
    { Release(m_rep); }

  inline THandle<__TRep>& operator=(THandle<__TRep> &aHandle)
    { AddRef(aHandle.m_rep);
      Release(m_rep);
      m_rep = aHandle.m_rep;
      return *this; }

  inline __TRep* operator->()
    { return m_rep; }

  // caller should call Release on this returned object when finished.
  inline __TRep* get_rep()
    { AddRef(m_rep);
      return m_rep; }
  
 protected:
  __TRep *m_rep;
};

#endif // __TOBJECT_H__

// -- end of tobject.h -- //
