// RUN: clang -O0 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res 2>&1 | FileCheck %s

// CHECK: Result: 0

#include <stdio.h>

int f (int x) {
  int y = 2;

  if (x)
    y += 1;

  if (x > 1)
    y *= 2;
  else
    y += 2;

  return x + y;
}

void main(int argc, char *argv[]) {
   int r = f(1) - f(2);
   printf("Result: %d\n", r);
}
