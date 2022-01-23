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
    dsb oshst  ;@ dsb 2,           4 0010 F07FF5
    dsb osh    ;@ dsb 3,           4 0011 F07FF5
    dsb nshst  ;@ dsb 6,           4 0110 F07FF5
    dsb nsh    ;@ dsb 7,           4 0111 F07FF5
    dsb ishst  ;@ dsb A,           4 1010 F07FF5
    dsb ish    ;@ dsb B,           4 1011 F07FF5
    dsb st     ;@ dsb E,           4 1110 F07FF5
    dsb sy     ;@ dsb F,           4 1111 F07FF5

    mov R0, #0 ;@ return code = 0, 00 00 a0 e3
    bx lr
  .size main, .-main
