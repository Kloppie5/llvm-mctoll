# RUN: clang -O0 -o %t %s --target=arm-linux-gnueabi -march=armv8-a
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1

.section .text
  .align 4

  .global main
  .type main, %function
  main:
    dmb oshld  ;@ dmb 1,           BF3 0001 03D5
    dmb oshst  ;@ dmb 2,           BF3 0010 03D5
    dmb osh    ;@ dmb 3,           BF3 0011 03D5
    dmb nshld  ;@ dmb 5,           BF3 0101 03D5
    dmb nshst  ;@ dmb 6,           BF3 0110 03D5
    dmb nsh    ;@ dmb 7,           BF3 0111 03D5
    dmb ishld  ;@ dmb 9,           BF3 1001 03D5
    dmb ishst  ;@ dmb A,           BF3 1010 03D5
    dmb ish    ;@ dmb B,           BF3 1011 03D5
    dmb ld     ;@ dmb D,           BF3 1101 03D5
    dmb st     ;@ dmb E,           BF3 1110 03D5
    dmb sy     ;@ dmb F,           BF3 1111 03D5

    mov R0, #0 ;@ return code = 0, 00 00 a0 e3
    bx lr
  .size main, .-main
