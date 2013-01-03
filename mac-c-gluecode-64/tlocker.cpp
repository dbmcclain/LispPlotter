// tlocker.cpp -- generally useful POSIX Mutex control
// for critical code sections
//
// DM/Avisere  07/04
//

#include "tlocker.h"

// ---------------------------------------------------------------------
TLock::TLock(int lockType)
  :THandle<TLockRep>(new TLockRep(lockType))
{}
    
TLock& TLock::operator=(TLock& aLock)
{
  THandle<TLockRep>::operator=(aLock);
  return *this;
}

// ----------------------------------------------------------------
TLockRep::TLockRep(int lockType)
{
  switch(lockType)
    {
    case TLock::DEFAULT:
      pthread_mutex_init(&m_mutex, NULL);
      break;

    case TLock::RECURSIVE:
      {
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&m_mutex, &attr);
	pthread_mutexattr_destroy(&attr);
      }
      break;
    }
}

// ---------------------------------------------------------------------
TLocker::TLocker(pthread_mutex_t *mutex)
{
  m_lock.m_mutex = mutex;
  m_kind = RAW;
  pthread_mutex_lock(mutex);
}

TLocker::TLocker(TLock &aLock)
{
  m_lock.m_lock = aLock.get_rep();
  m_kind = CLASS;
  aLock->lock();
}

TLocker::~TLocker()
{
  switch(m_kind)
    {
    case RAW:
      pthread_mutex_unlock(m_lock.m_mutex);
      break;

    case CLASS:
      m_lock.m_lock->unlock();
      Release(m_lock.m_lock);
      break;
    }
}

// -- end of tlocker.cpp -- //
