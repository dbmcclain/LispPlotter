# 1 "fdfilestuff.cpp"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "fdfilestuff.cpp"

# 1 "/usr/include/sys/types.h" 1 3 4
# 66 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/sys/appleapiopts.h" 1 3 4
# 67 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 70 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/machine/types.h" 1 3 4
# 32 "/usr/include/machine/types.h" 3 4
# 1 "/usr/include/i386/types.h" 1 3 4
# 64 "/usr/include/i386/types.h" 3 4
# 1 "/usr/include/i386/_types.h" 1 3 4
# 31 "/usr/include/i386/_types.h" 3 4
typedef signed char __int8_t;



typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef long long __int64_t;
typedef unsigned long long __uint64_t;

typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
# 64 "/usr/include/i386/_types.h" 3 4
typedef int __darwin_ct_rune_t;





typedef union {
 char __mbstate8[128];
 long long _mbstateL;
} __mbstate_t;

typedef __mbstate_t __darwin_mbstate_t;


typedef int __darwin_ptrdiff_t;





typedef long unsigned int __darwin_size_t;





typedef __builtin_va_list __darwin_va_list;





typedef int __darwin_wchar_t;




typedef __darwin_wchar_t __darwin_rune_t;


typedef int __darwin_wint_t;




typedef unsigned long __darwin_clock_t;
typedef __uint32_t __darwin_socklen_t;
typedef long __darwin_ssize_t;
typedef long __darwin_time_t;
# 65 "/usr/include/i386/types.h" 2 3 4







typedef signed char int8_t;

typedef unsigned char u_int8_t;


typedef short int16_t;

typedef unsigned short u_int16_t;


typedef int int32_t;

typedef unsigned int u_int32_t;


typedef long long int64_t;

typedef unsigned long long u_int64_t;




typedef int32_t register_t;




typedef __darwin_intptr_t intptr_t;



typedef unsigned long int uintptr_t;





typedef u_int64_t user_addr_t;
typedef u_int64_t user_size_t;
typedef int64_t user_ssize_t;
typedef int64_t user_long_t;
typedef u_int64_t user_ulong_t;
typedef int64_t user_time_t;




typedef u_int64_t syscall_arg_t;
# 33 "/usr/include/machine/types.h" 2 3 4
# 73 "/usr/include/sys/types.h" 2 3 4
# 1 "/usr/include/sys/_types.h" 1 3 4
# 27 "/usr/include/sys/_types.h" 3 4
# 1 "/usr/include/machine/_types.h" 1 3 4
# 28 "/usr/include/sys/_types.h" 2 3 4



struct mcontext;
struct mcontext64;
# 60 "/usr/include/sys/_types.h" 3 4
struct __darwin_pthread_handler_rec
{
 void (*__routine)(void *);
 void *__arg;
 struct __darwin_pthread_handler_rec *__next;
};
struct _opaque_pthread_attr_t { long __sig; char __opaque[36]; };
struct _opaque_pthread_cond_t { long __sig; char __opaque[24]; };
struct _opaque_pthread_condattr_t { long __sig; char __opaque[4]; };
struct _opaque_pthread_mutex_t { long __sig; char __opaque[40]; };
struct _opaque_pthread_mutexattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_once_t { long __sig; char __opaque[4]; };
struct _opaque_pthread_rwlock_t { long __sig; char __opaque[124]; };
struct _opaque_pthread_rwlockattr_t { long __sig; char __opaque[12]; };
struct _opaque_pthread_t { long __sig; struct __darwin_pthread_handler_rec *__cleanup_stack; char __opaque[596]; };
# 96 "/usr/include/sys/_types.h" 3 4
typedef __int64_t __darwin_blkcnt_t;
typedef __int32_t __darwin_blksize_t;
typedef __int32_t __darwin_dev_t;
typedef unsigned int __darwin_fsblkcnt_t;
typedef unsigned int __darwin_fsfilcnt_t;
typedef __uint32_t __darwin_gid_t;
typedef __uint32_t __darwin_id_t;
typedef __uint32_t __darwin_ino_t;
typedef __darwin_natural_t __darwin_mach_port_name_t;
typedef __darwin_mach_port_name_t __darwin_mach_port_t;

typedef struct mcontext *__darwin_mcontext_t;
typedef struct mcontext64 *__darwin_mcontext64_t;



typedef __uint16_t __darwin_mode_t;
typedef __int64_t __darwin_off_t;
typedef __int32_t __darwin_pid_t;
typedef struct _opaque_pthread_attr_t
   __darwin_pthread_attr_t;
typedef struct _opaque_pthread_cond_t
   __darwin_pthread_cond_t;
typedef struct _opaque_pthread_condattr_t
   __darwin_pthread_condattr_t;
typedef unsigned long __darwin_pthread_key_t;
typedef struct _opaque_pthread_mutex_t
   __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t
   __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_once_t
   __darwin_pthread_once_t;
typedef struct _opaque_pthread_rwlock_t
   __darwin_pthread_rwlock_t;
typedef struct _opaque_pthread_rwlockattr_t
   __darwin_pthread_rwlockattr_t;
typedef struct _opaque_pthread_t
   *__darwin_pthread_t;
typedef __uint32_t __darwin_sigset_t;
typedef __int32_t __darwin_suseconds_t;
typedef __uint32_t __darwin_uid_t;
typedef __uint32_t __darwin_useconds_t;
typedef unsigned char __darwin_uuid_t[16];



struct sigaltstack



{
 void *ss_sp;
 __darwin_size_t ss_size;
 int ss_flags;
};

typedef struct sigaltstack __darwin_stack_t;






struct ucontext



{
 int uc_onstack;
 __darwin_sigset_t uc_sigmask;
 __darwin_stack_t uc_stack;

 struct ucontext *uc_link;



 __darwin_size_t uc_mcsize;
 __darwin_mcontext_t uc_mcontext;
};

typedef struct ucontext __darwin_ucontext_t;





struct ucontext64 {
 int uc_onstack;
 __darwin_sigset_t uc_sigmask;
 __darwin_stack_t uc_stack;
 struct ucontext64 *uc_link;
 __darwin_size_t uc_mcsize;
 __darwin_mcontext64_t uc_mcontext64;
};
typedef struct ucontext64 __darwin_ucontext64_t;
# 74 "/usr/include/sys/types.h" 2 3 4

# 1 "/usr/include/machine/endian.h" 1 3 4
# 32 "/usr/include/machine/endian.h" 3 4
# 1 "/usr/include/i386/endian.h" 1 3 4
# 92 "/usr/include/i386/endian.h" 3 4
# 1 "/usr/include/sys/_endian.h" 1 3 4
# 88 "/usr/include/sys/_endian.h" 3 4
# 1 "/usr/lib/gcc/i686-apple-darwin8/4.0.1/include/stdint.h" 1 3 4
# 34 "/usr/lib/gcc/i686-apple-darwin8/4.0.1/include/stdint.h" 3 4
typedef unsigned char uint8_t;




typedef unsigned short uint16_t;




typedef unsigned int uint32_t;




typedef unsigned long long uint64_t;



typedef int8_t int_least8_t;
typedef int16_t int_least16_t;
typedef int32_t int_least32_t;
typedef int64_t int_least64_t;
typedef uint8_t uint_least8_t;
typedef uint16_t uint_least16_t;
typedef uint32_t uint_least32_t;
typedef uint64_t uint_least64_t;



typedef int8_t int_fast8_t;
typedef int16_t int_fast16_t;
typedef int32_t int_fast32_t;
typedef int64_t int_fast64_t;
typedef uint8_t uint_fast8_t;
typedef uint16_t uint_fast16_t;
typedef uint32_t uint_fast32_t;
typedef uint64_t uint_fast64_t;
# 91 "/usr/lib/gcc/i686-apple-darwin8/4.0.1/include/stdint.h" 3 4
typedef long long int intmax_t;
# 100 "/usr/lib/gcc/i686-apple-darwin8/4.0.1/include/stdint.h" 3 4
typedef long long unsigned int uintmax_t;
# 89 "/usr/include/sys/_endian.h" 2 3 4
# 1 "/usr/include/libkern/OSByteOrder.h" 1 3 4
# 54 "/usr/include/libkern/OSByteOrder.h" 3 4
# 1 "/usr/include/libkern/i386/OSByteOrder.h" 1 3 4
# 40 "/usr/include/libkern/i386/OSByteOrder.h" 3 4
static inline
uint16_t
_OSSwapInt16(
    uint16_t data
)
{
    return ((data << 8) | (data >> 8));
}

static inline
uint32_t
_OSSwapInt32(
    uint32_t data
)
{
    __asm__ ("bswap   %0" : "+r" (data));
    return data;
}


static inline
uint64_t
_OSSwapInt64(
    uint64_t data
)
{
    __asm__ ("bswap   %%eax\n\t"
             "bswap   %%edx\n\t"
             "xchgl   %%eax, %%edx"
             : "+A" (data));
    return data;
}
# 88 "/usr/include/libkern/i386/OSByteOrder.h" 3 4
static inline
uint16_t
OSReadSwapInt16(
    const volatile void * base,
    uintptr_t byteOffset
)
{
    uint16_t result;

    result = *(volatile uint16_t *)((uintptr_t)base + byteOffset);
    return _OSSwapInt16(result);
}

static inline
uint32_t
OSReadSwapInt32(
    const volatile void * base,
    uintptr_t byteOffset
)
{
    uint32_t result;

    result = *(volatile uint32_t *)((uintptr_t)base + byteOffset);
    return _OSSwapInt32(result);
}

static inline
uint64_t
OSReadSwapInt64(
    const volatile void * base,
    uintptr_t byteOffset
)
{
    uint64_t result;

    result = *(volatile uint64_t *)((uintptr_t)base + byteOffset);
    return _OSSwapInt64(result);
}



static inline
void
OSWriteSwapInt16(
    volatile void * base,
    uintptr_t byteOffset,
    uint16_t data
)
{
    *(volatile uint16_t *)((uintptr_t)base + byteOffset) = _OSSwapInt16(data);
}

static inline
void
OSWriteSwapInt32(
    volatile void * base,
    uintptr_t byteOffset,
    uint32_t data
)
{
    *(volatile uint32_t *)((uintptr_t)base + byteOffset) = _OSSwapInt32(data);
}

static inline
void
OSWriteSwapInt64(
    volatile void * base,
    uintptr_t byteOffset,
    uint64_t data
)
{
    *(volatile uint64_t *)((uintptr_t)base + byteOffset) = _OSSwapInt64(data);
}
# 55 "/usr/include/libkern/OSByteOrder.h" 2 3 4
# 80 "/usr/include/libkern/OSByteOrder.h" 3 4
enum {
    OSUnknownByteOrder,
    OSLittleEndian,
    OSBigEndian
};

static inline
int32_t
OSHostByteOrder(void) {

    return OSLittleEndian;





}
# 109 "/usr/include/libkern/OSByteOrder.h" 3 4
static inline
uint16_t
_OSReadInt16(
    const volatile void * base,
    uintptr_t byteOffset
)
{
    return *(volatile uint16_t *)((uintptr_t)base + byteOffset);
}

static inline
uint32_t
_OSReadInt32(
    const volatile void * base,
    uintptr_t byteOffset
)
{
    return *(volatile uint32_t *)((uintptr_t)base + byteOffset);
}

static inline
uint64_t
_OSReadInt64(
    const volatile void * base,
    uintptr_t byteOffset
)
{
    return *(volatile uint64_t *)((uintptr_t)base + byteOffset);
}



static inline
void
_OSWriteInt16(
    volatile void * base,
    uintptr_t byteOffset,
    uint16_t data
)
{
    *(volatile uint16_t *)((uintptr_t)base + byteOffset) = data;
}

static inline
void
_OSWriteInt32(
    volatile void * base,
    uintptr_t byteOffset,
    uint32_t data
)
{
    *(volatile uint32_t *)((uintptr_t)base + byteOffset) = data;
}

static inline
void
_OSWriteInt64(
    volatile void * base,
    uintptr_t byteOffset,
    uint64_t data
)
{
    *(volatile uint64_t *)((uintptr_t)base + byteOffset) = data;
}
# 90 "/usr/include/sys/_endian.h" 2 3 4

extern "C" {
uint16_t ntohs(uint16_t);
uint16_t htons(uint16_t);
uint32_t ntohl(uint32_t);
uint32_t htonl(uint32_t);
}
# 93 "/usr/include/i386/endian.h" 2 3 4
# 33 "/usr/include/machine/endian.h" 2 3 4
# 76 "/usr/include/sys/types.h" 2 3 4


typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;

typedef unsigned long u_long;


typedef unsigned short ushort;
typedef unsigned int uint;


typedef u_int64_t u_quad_t;
typedef int64_t quad_t;
typedef quad_t * qaddr_t;

typedef char * caddr_t;
typedef int32_t daddr_t;


typedef __darwin_dev_t dev_t;



typedef u_int32_t fixpt_t;


typedef __darwin_blkcnt_t blkcnt_t;




typedef __darwin_blksize_t blksize_t;




typedef __darwin_gid_t gid_t;





typedef __uint32_t in_addr_t;




typedef __uint16_t in_port_t;



typedef __darwin_ino_t ino_t;





typedef __int32_t key_t;



typedef __darwin_mode_t mode_t;




typedef __uint16_t nlink_t;





typedef __darwin_id_t id_t;



typedef __darwin_pid_t pid_t;




typedef __darwin_off_t off_t;



typedef int32_t segsz_t;
typedef int32_t swblk_t;


typedef __darwin_uid_t uid_t;
# 185 "/usr/include/sys/types.h" 3 4
typedef __darwin_clock_t clock_t;






typedef __darwin_size_t size_t;




typedef __darwin_ssize_t ssize_t;




typedef __darwin_time_t time_t;




typedef __darwin_useconds_t useconds_t;




typedef __darwin_suseconds_t suseconds_t;
# 224 "/usr/include/sys/types.h" 3 4
typedef __int32_t fd_mask;
# 248 "/usr/include/sys/types.h" 3 4
extern "C" {
typedef struct fd_set {
 __int32_t fds_bits[(((1024) + (((sizeof(__int32_t) * 8)) - 1)) / ((sizeof(__int32_t) * 8)))];
} fd_set;
}
# 296 "/usr/include/sys/types.h" 3 4
typedef __darwin_pthread_attr_t pthread_attr_t;



typedef __darwin_pthread_cond_t pthread_cond_t;



typedef __darwin_pthread_condattr_t pthread_condattr_t;



typedef __darwin_pthread_mutex_t pthread_mutex_t;



typedef __darwin_pthread_mutexattr_t pthread_mutexattr_t;



typedef __darwin_pthread_once_t pthread_once_t;



typedef __darwin_pthread_rwlock_t pthread_rwlock_t;



typedef __darwin_pthread_rwlockattr_t pthread_rwlockattr_t;



typedef __darwin_pthread_t pthread_t;






typedef __darwin_pthread_key_t pthread_key_t;





typedef __darwin_fsblkcnt_t fsblkcnt_t;




typedef __darwin_fsfilcnt_t fsfilcnt_t;
# 3 "fdfilestuff.cpp" 2
# 1 "/usr/include/sys/stat.h" 1 3 4
# 129 "/usr/include/sys/stat.h" 3 4
struct timespec {
 time_t tv_sec;
 long tv_nsec;
};
# 147 "/usr/include/sys/stat.h" 3 4
struct ostat {
 __uint16_t st_dev;
 ino_t st_ino;
 mode_t st_mode;
 nlink_t st_nlink;
 __uint16_t st_uid;
 __uint16_t st_gid;
 __uint16_t st_rdev;
 __int32_t st_size;
 struct timespec st_atimespec;
 struct timespec st_mtimespec;
 struct timespec st_ctimespec;
 __int32_t st_blksize;
 __int32_t st_blocks;
 __uint32_t st_flags;
 __uint32_t st_gen;
};






struct stat {
 dev_t st_dev;
 ino_t st_ino;
 mode_t st_mode;
 nlink_t st_nlink;
 uid_t st_uid;
 gid_t st_gid;
 dev_t st_rdev;

 struct timespec st_atimespec;
 struct timespec st_mtimespec;
 struct timespec st_ctimespec;
# 190 "/usr/include/sys/stat.h" 3 4
 off_t st_size;
 blkcnt_t st_blocks;
 blksize_t st_blksize;
 __uint32_t st_flags;
 __uint32_t st_gen;
 __int32_t st_lspare;
 __int64_t st_qspare[2];
};
# 340 "/usr/include/sys/stat.h" 3 4
extern "C" {

int chmod(const char *, mode_t);
int fchmod(int, mode_t);
int fstat(int, struct stat *);
int lstat(const char *, struct stat *);
int mkdir(const char *, mode_t);
int mkfifo(const char *, mode_t);
int stat(const char *, struct stat *);
int mknod(const char *, mode_t, dev_t);
mode_t umask(mode_t);



struct _filesec;
typedef struct _filesec *filesec_t;


int chflags(const char *, __uint32_t);
int chmodx_np(const char *, filesec_t);
int fchflags(int, __uint32_t);
int fchmodx_np(int, filesec_t);
int fstatx_np(int, struct stat *, filesec_t);
int lstatx_np(const char *, struct stat *, filesec_t);
int mkdirx_np(const char *, filesec_t);
int mkfifox_np(const char *, filesec_t);
int statx_np(const char *, struct stat *, filesec_t);
int umaskx_np(filesec_t);

}
# 4 "fdfilestuff.cpp" 2
# 1 "/usr/include/sys/mman.h" 1 3 4
# 187 "/usr/include/sys/mman.h" 3 4
extern "C" {

int mlockall(int);
int munlockall(void);

int mlock(const void *, size_t);



void * mmap(void *, size_t, int, int, int, off_t) ;


int mprotect(void *, size_t, int) ;

int msync(void *, size_t, int) ;

int munlock(const void *, size_t);

int munmap(void *, size_t) ;

int shm_open(const char *, int, ...);
int shm_unlink(const char *);

int posix_madvise(void *, size_t, int);


int madvise(void *, size_t, int);
int mincore(const void *, size_t, char *);
int minherit(void *, size_t, int);

}
# 5 "fdfilestuff.cpp" 2
# 1 "/usr/include/sys/errno.h" 1 3 4
# 68 "/usr/include/sys/errno.h" 3 4
extern "C" {
extern int * __error(void);

}
# 6 "fdfilestuff.cpp" 2
# 1 "/usr/include/unistd.h" 1 3 4
# 71 "/usr/include/unistd.h" 3 4
# 1 "/usr/include/_types.h" 1 3 4
# 29 "/usr/include/_types.h" 3 4
typedef int __darwin_nl_item;
typedef int __darwin_wctrans_t;



typedef unsigned long __darwin_wctype_t;
# 72 "/usr/include/unistd.h" 2 3 4


# 1 "/usr/include/sys/unistd.h" 1 3 4
# 130 "/usr/include/sys/unistd.h" 3 4
struct accessx_descriptor {
 unsigned ad_name_offset;
 int ad_flags;
 int ad_pad[2];
};
# 75 "/usr/include/unistd.h" 2 3 4
# 121 "/usr/include/unistd.h" 3 4
typedef __darwin_uuid_t uuid_t;
# 388 "/usr/include/unistd.h" 3 4
extern "C" {

void _exit(int) __attribute__((__noreturn__));
int access(const char *, int);
unsigned int
  alarm(unsigned int);
int chdir(const char *);
int chown(const char *, uid_t, gid_t);
int close(int);
size_t confstr(int, char *, size_t);
char *crypt(const char *, const char *);
char *ctermid(char *);
int dup(int);
int dup2(int, int);
int execl(const char *, const char *, ...);
int execle(const char *, const char *, ...);
int execlp(const char *, const char *, ...);
int execv(const char *, char * const *);
int execve(const char *, char * const *, char * const *);
int execvp(const char *, char * const *);
int fchown(int, uid_t, gid_t);
int fchdir(int);
pid_t fork(void);
long fpathconf(int, int);
int ftruncate(int, off_t);
char *getcwd(char *, size_t);
gid_t getegid(void);
uid_t geteuid(void);
gid_t getgid(void);
int getgroups(int, gid_t []);
long gethostid(void);
int gethostname(char *, size_t);
char *getlogin(void);
int getlogin_r(char *, size_t);
int getopt(int, char * const [], const char *);
pid_t getpgid(pid_t);
pid_t getpgrp(void);
pid_t getpid(void);
pid_t getppid(void);
pid_t getsid(pid_t);
uid_t getuid(void);
char *getwd(char *);
int isatty(int);
int lchown(const char *, uid_t, gid_t) ;
int link(const char *, const char *);
int lockf(int, int, off_t);
off_t lseek(int, off_t, int);
int nice(int);
long pathconf(const char *, int);
int pause(void);
int pipe(int [2]);
ssize_t pread(int, void *, size_t, off_t);
ssize_t pwrite(int, const void *, size_t, off_t);
ssize_t read(int, void *, size_t);
ssize_t readlink(const char * , char * , size_t);
int rmdir(const char *);
int setegid(gid_t);
int seteuid(uid_t);
int setgid(gid_t);
int setpgid(pid_t, pid_t);



int setpgrp(pid_t pid, pid_t pgrp);

int setregid(gid_t, gid_t);
int setreuid(uid_t, uid_t);
pid_t setsid(void);
int setuid(uid_t);
unsigned int
  sleep(unsigned int);
void swab(const void * , void * , ssize_t);
int symlink(const char *, const char *);
void sync(void);
long sysconf(int);
pid_t tcgetpgrp(int);
int tcsetpgrp(int, pid_t);
int truncate(const char *, off_t);
char *ttyname(int);



char *ttyname_r(int, char *, size_t);

useconds_t
  ualarm(useconds_t, useconds_t);
int unlink(const char *);
int usleep(useconds_t);
pid_t vfork(void);
ssize_t write(int, const void *, size_t);

extern char *optarg;
extern int optind, opterr, optopt;


# 1 "/usr/include/sys/select.h" 1 3 4
# 82 "/usr/include/sys/select.h" 3 4
typedef __darwin_sigset_t sigset_t;
# 146 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/signal.h" 1 3 4
# 63 "/usr/include/signal.h" 3 4
# 1 "/usr/include/sys/signal.h" 1 3 4
# 73 "/usr/include/sys/signal.h" 3 4
# 1 "/usr/include/machine/signal.h" 1 3 4
# 29 "/usr/include/machine/signal.h" 3 4
# 1 "/usr/include/i386/signal.h" 1 3 4
# 31 "/usr/include/i386/signal.h" 3 4
typedef int sig_atomic_t;
# 46 "/usr/include/i386/signal.h" 3 4
struct sigcontext {
    int sc_onstack;
    int sc_mask;
    unsigned int sc_eax;
    unsigned int sc_ebx;
    unsigned int sc_ecx;
    unsigned int sc_edx;
    unsigned int sc_edi;
    unsigned int sc_esi;
    unsigned int sc_ebp;
    unsigned int sc_esp;
    unsigned int sc_ss;
    unsigned int sc_eflags;
    unsigned int sc_eip;
    unsigned int sc_cs;
    unsigned int sc_ds;
    unsigned int sc_es;
    unsigned int sc_fs;
    unsigned int sc_gs;
};
# 30 "/usr/include/machine/signal.h" 2 3 4
# 74 "/usr/include/sys/signal.h" 2 3 4
# 139 "/usr/include/sys/signal.h" 3 4
typedef __darwin_mcontext_t mcontext_t;





typedef __darwin_mcontext64_t mcontext64_t;
# 171 "/usr/include/sys/signal.h" 3 4
typedef __darwin_ucontext_t ucontext_t;





typedef __darwin_ucontext64_t ucontext64_t;
# 186 "/usr/include/sys/signal.h" 3 4
union sigval {

 int sival_int;
 void *sival_ptr;
};





struct sigevent {
 int sigev_notify;
 int sigev_signo;
 union sigval sigev_value;
 void (*sigev_notify_function)(union sigval);
 pthread_attr_t *sigev_notify_attributes;
};



typedef struct __siginfo {
 int si_signo;
 int si_errno;
 int si_code;
 pid_t si_pid;
 uid_t si_uid;
 int si_status;
 void *si_addr;
 union sigval si_value;
 long si_band;
 unsigned long pad[7];
} siginfo_t;
# 297 "/usr/include/sys/signal.h" 3 4
union __sigaction_u {
 void (*__sa_handler)(int);
 void (*__sa_sigaction)(int, struct __siginfo *,
         void *);
};


struct __sigaction {
 union __sigaction_u __sigaction_u;
 void (*sa_tramp)(void *, int, int, siginfo_t *, void *);
 sigset_t sa_mask;
 int sa_flags;
};




struct sigaction {
 union __sigaction_u __sigaction_u;
 sigset_t sa_mask;
 int sa_flags;
};
# 355 "/usr/include/sys/signal.h" 3 4
typedef void (*sig_t)(int);
# 364 "/usr/include/sys/signal.h" 3 4
typedef __darwin_stack_t stack_t;
# 377 "/usr/include/sys/signal.h" 3 4
struct sigvec {
 void (*sv_handler)(int);
 int sv_mask;
 int sv_flags;
};
# 396 "/usr/include/sys/signal.h" 3 4
struct sigstack {
 char *ss_sp;
 int ss_onstack;
};
# 418 "/usr/include/sys/signal.h" 3 4
extern "C" {
void (*signal(int, void (*)(int)))(int);
}
# 64 "/usr/include/signal.h" 2 3 4







extern const char *const sys_signame[32];
extern const char *const sys_siglist[32];


extern "C" {
int raise(int);
}


extern "C" {
void (*bsd_signal(int, void (*)(int)))(int);
int kill(pid_t, int);
int killpg(pid_t, int);
int pthread_kill(pthread_t, int);
int pthread_sigmask(int, const sigset_t *, sigset_t *);
int sigaction(int, const struct sigaction * ,
     struct sigaction * );
int sigaddset(sigset_t *, int);
int sigaltstack(const stack_t * , stack_t * );
int sigdelset(sigset_t *, int);
int sigemptyset(sigset_t *);
int sigfillset(sigset_t *);
int sighold(int);
int sigignore(int);
int siginterrupt(int, int);
int sigismember(const sigset_t *, int);
int sigpause(int);
int sigpending(sigset_t *);
int sigprocmask(int, const sigset_t * , sigset_t * );
int sigrelse(int);
void (*sigset(int, void (*)(int)))(int);
int sigsuspend(const sigset_t *);
int sigwait(const sigset_t * , int * );

void psignal(unsigned int, const char *);
int sigblock(int);
int sigreturn(struct sigcontext *);
int sigsetmask(int);
int sigvec(int, struct sigvec *, struct sigvec *);

}
# 147 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/sys/time.h" 1 3 4
# 81 "/usr/include/sys/time.h" 3 4
struct timeval {
 time_t tv_sec;
 suseconds_t tv_usec;
};






struct itimerval {
 struct timeval it_interval;
 struct timeval it_value;
};
# 178 "/usr/include/sys/time.h" 3 4
struct timezone {
 int tz_minuteswest;
 int tz_dsttime;
};
# 221 "/usr/include/sys/time.h" 3 4
struct clockinfo {
 int hz;
 int tick;
 int tickadj;
 int stathz;
 int profhz;
};




extern "C" {


# 1 "/usr/include/time.h" 1 3 4
# 95 "/usr/include/time.h" 3 4
struct tm {
 int tm_sec;
 int tm_min;
 int tm_hour;
 int tm_mday;
 int tm_mon;
 int tm_year;
 int tm_wday;
 int tm_yday;
 int tm_isdst;
 long tm_gmtoff;
 char *tm_zone;
};

# 1 "/usr/include/machine/_limits.h" 1 3 4
# 28 "/usr/include/machine/_limits.h" 3 4
# 1 "/usr/include/i386/_limits.h" 1 3 4
# 29 "/usr/include/machine/_limits.h" 2 3 4
# 110 "/usr/include/time.h" 2 3 4






extern char *tzname[];


extern int getdate_err;



extern int daylight;

extern "C" {
char *asctime(const struct tm *);
clock_t clock(void);
char *ctime(const time_t *);
double difftime(time_t, time_t);
struct tm *getdate(const char *);
struct tm *gmtime(const time_t *);
struct tm *localtime(const time_t *);
time_t mktime(struct tm *);
size_t strftime(char * , size_t, const char * , const struct tm * ) ;
char *strptime(const char * , const char * , struct tm * );
time_t time(time_t *);


void tzset(void);



char *asctime_r(const struct tm *, char *);
char *ctime_r(const time_t *, char *);
struct tm *gmtime_r(const time_t *, struct tm *);
struct tm *localtime_r(const time_t *, struct tm *);
time_t posix2time(time_t);

char *timezone(int, int);

void tzsetwall(void);
time_t time2posix(time_t);
time_t timelocal(struct tm * const);
time_t timegm(struct tm * const);



int nanosleep(const struct timespec *, struct timespec *) ;

}
# 236 "/usr/include/sys/time.h" 2 3 4

int adjtime(const struct timeval *, struct timeval *);
int futimes(int, const struct timeval *);
int settimeofday(const struct timeval *, const struct timezone *);


int getitimer(int, struct itimerval *);
int gettimeofday(struct timeval * , struct timezone * );
int select(int, fd_set * , fd_set * ,
  fd_set * , struct timeval * );
int setitimer(int, const struct itimerval * ,
  struct itimerval * );
int utimes(const char *, const struct timeval *);

}
# 149 "/usr/include/sys/select.h" 2 3 4


extern "C" {

int pselect(int, fd_set * , fd_set * ,
  fd_set * , const struct timespec * ,
  const sigset_t * );

int select(int, fd_set * , fd_set * ,
  fd_set * , struct timeval * );
}
# 484 "/usr/include/unistd.h" 2 3 4

void _Exit(int) __attribute__((__noreturn__));
int accessx_np(const struct accessx_descriptor *, size_t, int *, uid_t);
int acct(const char *);
int add_profil(char *, size_t, unsigned long, unsigned int);
int async_daemon(void);
void *brk(const void *);
int chroot(const char *);



int encrypt(char *, int);

void endusershell(void);
int execvP(const char *, const char *, char * const *);
char *fflagstostr(unsigned long);
int fsync(int);
int getdtablesize(void);
int getdomainname(char *, int);
int getgrouplist(const char *, int, int *, int *);
mode_t getmode(const void *, mode_t);
int getpagesize(void) __attribute__((__const__));
char *getpass(const char *);
int getpeereid(int, uid_t *, gid_t *);
int getpgid(pid_t _pid);
int getsgroups_np(int *, uuid_t);
int getsid(pid_t _pid);
char *getusershell(void);
int getwgroups_np(int *, uuid_t);
int initgroups(const char *, int);
int iruserok(unsigned long, int, const char *, const char *);
int issetugid(void);
char *mkdtemp(char *);
int mknod(const char *, mode_t, dev_t);
int mkstemp(char *);
int mkstemps(char *, int);
char *mktemp(char *);
int nfssvc(int, void *);
int profil(char *, size_t, unsigned long, unsigned int);
int pthread_setugid_np(uid_t, gid_t);
int pthread_getugid_np( uid_t *, gid_t *);
int rcmd(char **, int, const char *, const char *, const char *, int *);
int reboot(int);
int revoke(const char *);
int rresvport(int *);
int rresvport_af(int *, int);
int ruserok(const char *, int, const char *, const char *);
void *sbrk(int);
int setdomainname(const char *, int);
int setgroups(int, const gid_t *);
void sethostid(long);
int sethostname(const char *, int);



int setkey(const char *);

int setlogin(const char *);
void *setmode(const char *);
int setrgid(gid_t);
int setruid(uid_t);
int setsgroups_np(int, const uuid_t);
void setusershell(void);
int setwgroups_np(int, const uuid_t);
int strtofflags(char **, unsigned long *, unsigned long *);
int swapon(const char *);
int syscall(int, ...);
int ttyslot(void);
int undelete(const char *);
int unwhiteout(const char *);
void *valloc(size_t);

extern char *suboptarg;
int getsubopt(char **, char * const *, char **);
# 570 "/usr/include/unistd.h" 3 4
int getattrlist(const char*,void*,void*,size_t,unsigned long) ;
int setattrlist(const char*,void*,void*,size_t,unsigned long) ;
int exchangedata(const char*,const char*,unsigned long);
int checkuseraccess(const char*,uid_t,gid_t*,int,int,unsigned long);
int getdirentriesattr(int,void*,void*,size_t,unsigned long*,unsigned long*,unsigned long*,unsigned long);
int searchfs(const char*,void*,void*,unsigned long,unsigned long,void*);

int fsctl(const char *,unsigned long,void*,unsigned long);


extern int optreset;


}
# 7 "fdfilestuff.cpp" 2
# 1 "/usr/include/stdlib.h" 1 3 4
# 64 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/sys/wait.h" 1 3 4
# 73 "/usr/include/sys/wait.h" 3 4
typedef enum {
 P_ALL,
 P_PID,
 P_PGID
} idtype_t;
# 111 "/usr/include/sys/wait.h" 3 4
# 1 "/usr/include/sys/resource.h" 1 3 4
# 90 "/usr/include/sys/resource.h" 3 4
typedef __int64_t rlim_t;
# 135 "/usr/include/sys/resource.h" 3 4
struct rusage {
 struct timeval ru_utime;
 struct timeval ru_stime;
# 146 "/usr/include/sys/resource.h" 3 4
 long ru_maxrss;

 long ru_ixrss;
 long ru_idrss;
 long ru_isrss;
 long ru_minflt;
 long ru_majflt;
 long ru_nswap;
 long ru_inblock;
 long ru_oublock;
 long ru_msgsnd;
 long ru_msgrcv;
 long ru_nsignals;
 long ru_nvcsw;
 long ru_nivcsw;


};
# 207 "/usr/include/sys/resource.h" 3 4
struct rlimit {
 rlim_t rlim_cur;
 rlim_t rlim_max;
};



extern "C" {
int getpriority(int, id_t);
int getrlimit(int, struct rlimit *);
int getrusage(int, struct rusage *);
int setpriority(int, id_t, int);
int setrlimit(int, const struct rlimit *);
}
# 112 "/usr/include/sys/wait.h" 2 3 4
# 190 "/usr/include/sys/wait.h" 3 4
union wait {
 int w_status;



 struct {

  unsigned int w_Termsig:7,
    w_Coredump:1,
    w_Retcode:8,
    w_Filler:16;







 } w_T;





 struct {

  unsigned int w_Stopval:8,
    w_Stopsig:8,
    w_Filler:16;






 } w_S;
};
# 240 "/usr/include/sys/wait.h" 3 4
extern "C" {
pid_t wait(int *);
pid_t waitpid(pid_t, int *, int);

int waitid(idtype_t, id_t, siginfo_t *, int);


pid_t wait3(int *, int, struct rusage *);
pid_t wait4(pid_t, int *, int, struct rusage *);

}
# 65 "/usr/include/stdlib.h" 2 3 4

# 1 "/usr/include/alloca.h" 1 3 4
# 35 "/usr/include/alloca.h" 3 4
extern "C" {
void *alloca(size_t);
}
# 67 "/usr/include/stdlib.h" 2 3 4
# 80 "/usr/include/stdlib.h" 3 4
typedef __darwin_ct_rune_t ct_rune_t;




typedef __darwin_rune_t rune_t;
# 96 "/usr/include/stdlib.h" 3 4
typedef struct {
 int quot;
 int rem;
} div_t;

typedef struct {
 long quot;
 long rem;
} ldiv_t;


typedef struct {
 long long quot;
 long long rem;
} lldiv_t;
# 133 "/usr/include/stdlib.h" 3 4
extern int __mb_cur_max;
# 143 "/usr/include/stdlib.h" 3 4
extern "C" {
void abort(void) __attribute__((__noreturn__));
int abs(int) __attribute__((__const__));
int atexit(void (*)(void));
double atof(const char *);
int atoi(const char *);
long atol(const char *);

long long
  atoll(const char *);

void *bsearch(const void *, const void *, size_t,
     size_t, int (*)(const void *, const void *));
void *calloc(size_t, size_t);
div_t div(int, int) __attribute__((__const__));
void exit(int) __attribute__((__noreturn__));
void free(void *);
char *getenv(const char *);
long labs(long) __attribute__((__const__));
ldiv_t ldiv(long, long) __attribute__((__const__));

long long
  llabs(long long);
lldiv_t lldiv(long long, long long);

void *malloc(size_t);
int mblen(const char *, size_t);
size_t mbstowcs(wchar_t * , const char * , size_t);
int mbtowc(wchar_t * , const char * , size_t);
void qsort(void *, size_t, size_t,
     int (*)(const void *, const void *));
int rand(void);
void *realloc(void *, size_t);
void srand(unsigned);
double strtod(const char *, char **);
float strtof(const char *, char **);
long strtol(const char *, char **, int);
long double
  strtold(const char *, char **) ;

long long
  strtoll(const char *, char **, int);

unsigned long
  strtoul(const char *, char **, int);

unsigned long long
  strtoull(const char *, char **, int);

int system(const char *);
size_t wcstombs(char * , const wchar_t * , size_t);
int wctomb(char *, wchar_t);


void _Exit(int) __attribute__((__noreturn__));
long a64l(const char *);
double drand48(void);
char *ecvt(double, int, int *, int *);
double erand48(unsigned short[3]);
char *fcvt(double, int, int *, int *);
char *gcvt(double, int, char *);
int getsubopt(char **, char * const *, char **);
int grantpt(int);



char *initstate(unsigned long, char *, long);

long jrand48(unsigned short[3]);
char *l64a(long);
void lcong48(unsigned short[7]);
long lrand48(void);
char *mktemp(char *);
int mkstemp(char *);
long mrand48(void);
long nrand48(unsigned short[3]);
int posix_openpt(int);
char *ptsname(int);
int putenv(char *) ;
long random(void);
char *realpath(const char *, char *resolved_path);
unsigned short
 *seed48(unsigned short[3]);
int setenv(const char *, const char *, int) ;



int setkey(const char *);

char *setstate(const char *);
void srand48(long);



void srandom(unsigned long);

int unlockpt(int);



void unsetenv(const char *);
# 260 "/usr/include/stdlib.h" 3 4
u_int32_t
  arc4random(void);
void arc4random_addrandom(unsigned char *dat, int datlen);
void arc4random_stir(void);


char *cgetcap(char *, const char *, int);
int cgetclose(void);
int cgetent(char **, char **, const char *);
int cgetfirst(char **, char **);
int cgetmatch(const char *, const char *);
int cgetnext(char **, char **);
int cgetnum(char *, const char *, long *);
int cgetset(const char *);
int cgetstr(char *, const char *, char **);
int cgetustr(char *, const char *, char **);

int daemon(int, int);
char *devname(dev_t, mode_t);
char *devname_r(dev_t, mode_t, char *buf, int len);
char *getbsize(int *, long *);
int getloadavg(double [], int);
const char
 *getprogname(void);

int heapsort(void *, size_t, size_t,
     int (*)(const void *, const void *));
int mergesort(void *, size_t, size_t,
     int (*)(const void *, const void *));
void qsort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *));
int radixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void setprogname(const char *);
int sradixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void sranddev(void);
void srandomdev(void);
int rand_r(unsigned *);
void *reallocf(void *, size_t);

long long
  strtoq(const char *, char **, int);
unsigned long long
  strtouq(const char *, char **, int);

extern char *suboptarg;
void *valloc(size_t);

}
# 8 "fdfilestuff.cpp" 2
# 1 "/usr/include/string.h" 1 3 4
# 80 "/usr/include/string.h" 3 4
extern "C" {
void *memchr(const void *, int, size_t);
int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);

char *stpcpy(char *, const char *);
char *strcasestr(const char *, const char *);

char *strcat(char *, const char *);
char *strchr(const char *, int);
int strcmp(const char *, const char *);
int strcoll(const char *, const char *);
char *strcpy(char *, const char *);
size_t strcspn(const char *, const char *);
char *strerror(int);
int strerror_r(int, char *, size_t);
size_t strlen(const char *);
char *strncat(char *, const char *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);

char *strnstr(const char *, const char *, size_t);

char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
size_t strspn(const char *, const char *);
char *strstr(const char *, const char *);
char *strtok(char *, const char *);
size_t strxfrm(char *, const char *, size_t);



void *memccpy(void *, const void *, int, size_t);
char *strtok_r(char *, const char *, char **);
char *strdup(const char *);

int bcmp(const void *, const void *, size_t);
void bcopy(const void *, void *, size_t);
void bzero(void *, size_t);
int ffs(int);
char *index(const char *, int);
char *rindex(const char *, int);
int strcasecmp(const char *, const char *);
size_t strlcat(char *, const char *, size_t);
size_t strlcpy(char *, const char *, size_t);
void strmode(int, char *);
int strncasecmp(const char *, const char *, size_t);
char *strsep(char **, const char *);
char *strsignal(int sig);
void swab(const void * , void * , ssize_t);


}
# 9 "fdfilestuff.cpp" 2


extern "C"
off_t fd_get_file_size(int fd)
{
  struct stat statInfo;
  fstat(fd, &statInfo);
  return statInfo.st_size;
}
