# RUN: clang -O0 -o %t %s --target=arm-linux-gnueabi -march=armv8-a -nostdlib
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll -nostdlib
# RUN: %t-dis 2>&1

.section .text
.global _start
.code 32
_start:
  mov R7, #1   ;@ exit,           01 70 a0 e3
    mov R0, #0 ;@ error_code = 0; 00 00 a0 e3
  svc 0        ;@ syscall,        00 00 00 ef
