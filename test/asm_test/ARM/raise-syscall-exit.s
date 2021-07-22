# RUN: clang -o %t %s --target=%arm_triple -mfloat-abi=soft
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1

.section .text
  .align 4
  .code 32

  .global main
  .type main, %function
  main:
    mov R7, #1   ;@ exit,               01 70 a0 e3
      mov R0, #0 ;@ error_code = 0,     00 00 a0 e3
    svc 0        ;@ syscall,            00 00 00 ef

    bx lr
  .size main, .-main
