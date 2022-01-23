// RUN: clang -O0 -target armv7-unknown-linux-gnueabisf -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h -I /usr/include/stdlib.h -I /usr/include/time.h
// RUN: clang -o %t-res %t-dis.ll
// UN: %t-res 2>&1 | FileCheck %s

// Currently only checks whether decompilation is successful.
// CHECK-NOT: ARMLinearRaiserPass encountered unhandled opcode

int get(x) {
  return x-1;
}

int main () {
  int x = 10;
  while (x = get(x));
  return x;
}
