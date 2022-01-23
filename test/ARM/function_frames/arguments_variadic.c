// RUN: clang -O3 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res

#include <stdarg.h>

int f (int count, ...) {
    int acc = 0;

    va_list l;
    va_start(l, count);
    for (int j = 0; j < count; ++j) {
      acc += va_arg(l, int);
    }
    va_end(l);

    return acc;
}

int main() {
  return f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) - 55;
}