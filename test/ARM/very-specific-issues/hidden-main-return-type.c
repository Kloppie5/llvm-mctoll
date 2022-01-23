// RUN: clang -O0 -target armv7-unknown-linux-gnueabisf -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o
// RUN: clang -o %t-res %t-dis.ll
// UN: %t-res 2>&1 | FileCheck %s

// Currently only checks whether decompilation is successful.
// CHECK-NOT: ARMLinearRaiserPass encountered unhandled opcode

int get() {
  return 1;
}

int main () {
  return get();
}
