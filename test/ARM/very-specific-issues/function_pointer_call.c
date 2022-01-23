// RUN: clang -O0 -target armv7-unknown-linux-gnueabisf -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h -I /usr/include/stdlib.h -I /usr/include/time.h
// RUN: clang -o %t-res %t-dis.ll
// UN: %t-res 2>&1 | FileCheck %s

// Currently only checks whether decompilation is successful.
// CHECK-NOT: ARMLinearRaiserPass encountered unhandled opcode

int A () {
  return 1;
}
int B () {
  return 0;
}

int main (int argc, char **argv) {
  int (*f)(void);

  if (argc == 1)
    f = A;
  else
    f = B;

  return f();
}
