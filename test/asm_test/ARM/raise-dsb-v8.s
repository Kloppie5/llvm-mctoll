# RUN: clang -O0 -o %t %s --target=%arm_triple -march=armv8-a
# RUN: llvm-mctoll %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1

.section .text
  .align 4

  .global main
  .type main, %function
  main:
    dsb oshld  ;@ dsb 1,           9F3 0001 03D5
    dsb oshst  ;@ dsb 2,           9F3 0010 03D5
    dsb osh    ;@ dsb 3,           9F3 0011 03D5
    dsb nshld  ;@ dsb 5,           9F3 0101 03D5
    dsb nshst  ;@ dsb 6,           9F3 0110 03D5
    dsb nsh    ;@ dsb 7,           9F3 0111 03D5
    dsb ishld  ;@ dsb 9,           9F3 1001 03D5
    dsb ishst  ;@ dsb A,           9F3 1010 03D5
    dsb ish    ;@ dsb B,           9F3 1011 03D5
    dsb ld     ;@ dsb D,           9F3 1101 03D5
    dsb st     ;@ dsb E,           9F3 1110 03D5
    dsb sy     ;@ dsb F,           9F3 1111 03D5

    mov R0, #0 ;@ return code = 0, 00 00 a0 e3
    bx lr
  .size main, .-main
