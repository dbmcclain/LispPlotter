// tlocker.h -- generally useful POSIX Mutex control for critical code sections
// DM/Avisere  07/04
//
#ifndef __TLOCKER_H__
#define __TLOCKER_H__

#include "tobject.h"
#include <pthread.h>

// -----------------------------------------
class TLockRep;

class TLock: public THandle<TLockRep>
{
 public:
  enum {DEFAULT, RECURSIVE};
  
  TLock(int lockType = DEFAULT);
    
  TLock& operator=(TLock& aLock);
};

// -----------------------------------------
class TLockRep: public TObject
{
 public:
  TLockRep(int lockType);
  
  inline virtual ~TLockRep()
    { pthread_mutex_destroy(&m_mutex); }
  
  inline long lock()
    { return pthread_mutex_lock(&m_mutex); }
  
  inline long trylock()
    { return pthread_mutex_trylock(&m_mutex); }
  
  inline long unlock()
    { return pthread_mutex_unlock(&m_mutex); }
  
 protected:
  pthread_mutex_t m_mutex;
  
 private:
  TLockRep();
  TLockRep(TLockRep &aLock);
  TLockRep& operator=(TLockRep &aLock);
};

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
//               TLocker lock;
//               <critical code>
//            }
//            ...
//
class TLocker
{
 public:
  TLocker(TLock &aLock);
  TLocker(pthread_mutex_t *mutex);
  virtual ~TLocker();
  
 protected:
  union {
    pthread_mutex_t *m_mutex;
    TLockRep        *m_lock;
  } m_lock;
  
  enum {RAW, CLASS};
  int  m_kind;
  
 private:
  TLocker();
  TLocker(TLocker &aLocker);
  TLocker& operator=(TLocker &aLocker);
};

#define CRITICAL_SECTION(mutex)     { TLocker __locker(mutex);
#define END_CRITICAL_SECTION        }

#endif // __TLOCKER_H__

// -- end of tlocker.h -- //
