// RUN: clang -O3 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o -I /usr/include/stdio.h
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res

int f ( int a, int b, int c, int d, int e ) {
  return a + b + c + d + e;
}

int main() {
  return f(1, 2, 3, 4, 5) - 15;
}