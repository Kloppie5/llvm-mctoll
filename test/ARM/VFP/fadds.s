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
  vmov.f32 s0, #1.0
  vmov.f32 s1, #1.0
  vadd.f32 s0, s0, s1
  vmov.f32 r0, s0
  bx	lr
  .size func, .-func
