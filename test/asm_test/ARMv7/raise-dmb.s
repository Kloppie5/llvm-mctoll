# RUN: clang -O0 -o %t %s --target=arm-linux-gnueabi -march=armv7-a -marm
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1

.section .text
  .align 4
  .code 32

  .global main
  .type main, %function
  main:
    dmb oshst  ;@ dmb 2,           5 0010 F07FF5
    dmb osh    ;@ dmb 3,           5 0011 F07FF5
    dmb nshst  ;@ dmb 6,           5 0110 F07FF5
    dmb nsh    ;@ dmb 7,           5 0111 F07FF5
    dmb ishst  ;@ dmb A,           5 1010 F07FF5
    dmb ish    ;@ dmb B,           5 1011 F07FF5
    dmb st     ;@ dmb E,           5 1110 F07FF5
    dmb sy     ;@ dmb F,           5 1111 F07FF5

    mov R0, #0 ;@ return code = 0, 00 00 a0 e3
    bx lr
  .size main, .-main
