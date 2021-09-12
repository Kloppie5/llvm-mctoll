// RUN: clang -O3 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -O3 -o %t-res %t-dis.ll
// RUN: %t-res 2>&1 | FileCheck %s

// CHECK: Hello, World!

#include <stdio.h>

int main() {
  printf("Hello, World!\n");
  return 0;
}