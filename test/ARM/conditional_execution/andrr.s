;# RUN: clang -O3 -c -target arm -mfloat-abi=soft -o %t.o %s
;# RUN: llvm-mctoll -d -debug  %t.o 2>&1 | FileCheck %s

;# Currently only checks whether decompilation is successful.
;# CHECK: ARMLinearRaiserPass end.

  .text
  .align 4
  .code 32
  .global func
  .type func, %function
func:
  cmp r0, #0
  andeq r0, r0, r1
  andne r0, r0, r1
  andcs r0, r0, r1
  andcc r0, r0, r1
  andmi r0, r0, r1
  andpl r0, r0, r1
  andvs r0, r0, r1
  andvc r0, r0, r1
  andhi r0, r0, r1
  andls r0, r0, r1
  andge r0, r0, r1
  andlt r0, r0, r1
  andgt r0, r0, r1
  andle r0, r0, r1
  andal r0, r0, r1
  bx	lr
  .size func, .-func
