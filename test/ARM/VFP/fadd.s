;# RUN: clang -O3 -c -target armv7-unknown-linux-gnueabihf -o %t.o %s
;# RUN: llvm-mctoll -d -debug  %t.o 2>&1 | FileCheck %s

;# Currently only checks whether decompilation is successful.
;# CHECK: ARMLinearRaiserPass end.

  .text
  .align 4
  .code 32
  .global func
  .type func, %function
func:
  fadds s0, s1, s2
  bx	lr
  .size func, .-func
