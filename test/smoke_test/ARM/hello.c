// RUN: clang -o %t %s --target=%arm_triple -fuse-ld=lld
// RUN: llvm-mctoll -I /usr/include/stdio.h %t
// RUN: clang -o %t1 %t-dis.ll
// RUN: %t1 2>&1 | FileCheck %s
// CHECK: Hello world!

#include <stdio.h>
int main(int argc, char **argv) {
  printf("Hello world!\n");
  return 0;
}
