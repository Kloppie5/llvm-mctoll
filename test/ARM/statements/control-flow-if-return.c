// RUN: clang -O0 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res 2>&1 | FileCheck %s

// CHECK: Result: 0

#include <stdio.h>

int f (int x) {
  if (x == 1)
    return 7;

  if (x < 0)
    return -1;

  return x + 5;
}

void main(int argc, char *argv[]) {
   int r = f(1) - f(2);
   printf("Result: %d\n", r);
}
