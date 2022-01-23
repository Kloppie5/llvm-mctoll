// RUN: clang -O3 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res

int f ( int a, int b ) {
  return a + b;
}

int main() {
  return f(1, 2) - 3;
}