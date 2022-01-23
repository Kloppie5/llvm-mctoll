void __assert_fail(const char *__assertion, const char *__file, unsigned int __line, const char *__function);
int __fxstat(int version, int fd, struct stat *buf);
int close(int fd);
void exit(int __status);
int fprintf(struct FILE *__restrict__ __stream, const char *__restrict__ __format, ...);
unsigned long fwrite(const void *__restrict__ __ptr, unsigned long __size, unsigned long __n, struct FILE *__restrict__ __s);
void *memset(void *__s, int __c, unsigned long __n);
void *mmap(void *addr, unsigned long length, int prot, int flags, int fd, unsigned long offset);
int munmap(void *__addr, unsigned long __len);
int open(const char *filename, int flags, ...);
void perror(const char *__s);
int printf(const char *__restrict__ __format, ...);
int pthread_attr_init(long int *__attr);
int pthread_attr_setscope(long int *__attr, int __scope);
int puts(const char *__s);
long sysconf(int __name);