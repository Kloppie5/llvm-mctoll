// RUN: clang -O0 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res 2>&1 | FileCheck %s

// CHECK: Result: 7

#include <stdio.h>

int f() {
  goto L1;
  {
    L2:;
    goto L4;
    L1:;
    goto L3;
  }
  L3:;
  goto L2;
  L4:;
  return 7;
}

void main(int argc, char *argv[]) {
   int r = f();
   printf("Result: %d\n", r);
}
