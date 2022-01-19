// RUN: clang -O3 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res 2>&1 | FileCheck %s

// CHECK: Result: 0

#include <stdio.h>

int f(int x) {
  return x*3;
}
int g(int x, int y) {
  return x+y;
}

void main(int argc, char *argv[]) {
   switch(argc) {
    case 0:
    case 1:
    case 7:
      printf("Found %d\n", f(argc));
      break;
    case 9:
      printf("Error %d\n", g(f(argc), 4));
    default:
      break;
   }
   printf("Result: %d\n", argc);
}
