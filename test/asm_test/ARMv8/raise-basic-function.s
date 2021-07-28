# RUN: clang -O0 -o %t %s --target=arm-linux-gnueabi -march=armv8-a
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1

.section .text
  .global main
  .type main, %function
  main:
    mov r0, #0
    bx lr
  .size main, .-main
