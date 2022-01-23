// RUN: clang -O0 -target arm-linux-gnueabi -o %t.o %s
// RUN: llvm-mctoll -d -debug -o %t-dis.ll %t.o
// RUN: clang -o %t-res %t-dis.ll
// RUN: %t-res 1

int main(int argc, char *argv[]) {
   return 2/argc-1;
}
