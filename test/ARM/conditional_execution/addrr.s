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
  addeq r0, r0, r1
  addne r0, r0, r1
  addcs r0, r0, r1
  addcc r0, r0, r1
  addmi r0, r0, r1
  addpl r0, r0, r1
  addvs r0, r0, r1
  addvc r0, r0, r1
  addhi r0, r0, r1
  addls r0, r0, r1
  addge r0, r0, r1
  addlt r0, r0, r1
  addgt r0, r0, r1
  addle r0, r0, r1
  addal r0, r0, r1
  bx	lr
  .size func, .-func
