void __assert_fail(const char *__assertion, const char *__file, unsigned int __line, const char *__function);
int __fxstat(int version, int fd, struct stat *buf);
int close(int fd);
void exit(int __status);
void *mmap(void *addr, unsigned long length, int prot, int flags, int fd, unsigned long offset);
int munmap(void *__addr, unsigned long __len);
int open(const char *filename, int flags, ...);
void perror(const char *__s);
int printf(const char *__restrict__ __format, ...);
int puts(const char *__s);
