// RUN: clang -O3 -S -emit-llvm -o %t-ref.ll %s
// RUN: clang -O3 -target arm-linux-gnueabi -mfloat-abi=soft -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res 1 2 3

int main(int argc, char *argv[]) {
  return argv[1] - 1;
}