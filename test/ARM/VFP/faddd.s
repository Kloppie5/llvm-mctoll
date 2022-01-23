;# RUN: clang -O0 -c -target armv7-unknown-linux-gnueabihf -o %t.o %s
;# RUN: llvm-mctoll -d -debug  %t.o 2>&1 | FileCheck %s

;# Currently only checks whether decompilation is successful.
;# CHECK-NOT: ARMLinearRaiserPass encountered unhandled opcode

  .text
  .align 4
  .code 32
  .global func
  .type func, %function
func:
  vmov.f64 d0, #1.0
  vmov.f64 d1, #1.0
  vadd.f64 d0, d0, d1
  vmov.f64 d2, d0
  bx	lr
  bx	lr
  .size func, .-func
