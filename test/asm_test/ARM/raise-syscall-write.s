# RUN: clang -o %t %s --target=%arm_triple
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1 | FileCheck %s
# CHECK: Hello World!

.section .text
  .align 4
  .code 32

  .global main
  .type main, %function
  main:
    mov R7, #4           ;@ write,              04 70 a0 e3
      mov R0, #1         ;@ fd = stdout,        01 00 a0 e3
      adr R1, helloworld ;@ *buff = helloworld, 10 10 8f e2
      mov R2, #13        ;@ count = 13,         0d 20 a0 e3
    svc 0                ;@ syscall,            00 00 00 ef

    mov R0, #0           ;@ return code = 0,    00 00 a0 e3
    bx lr
  .size main, .-main

.section .data
  helloworld:
    .ascii "Hello World!\n"
