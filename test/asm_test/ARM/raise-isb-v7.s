# RUN: clang -O0 -o %t %s --target=%arm_triple -march=armv7-a
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1

.section .text
  .align 4
  .code 32

  .global main
  .type main, %function
  main:
    isb        ;@ isb,             6 1111 F07FF5

    mov R0, #0 ;@ return code = 0, 00 00 a0 e3
    bx lr
  .size main, .-main
