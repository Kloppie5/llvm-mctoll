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
  subeq r0, r0, r1
  subne r0, r0, r1
  subcs r0, r0, r1
  subcc r0, r0, r1
  submi r0, r0, r1
  subpl r0, r0, r1
  subvs r0, r0, r1
  subvc r0, r0, r1
  subhi r0, r0, r1
  subls r0, r0, r1
  subge r0, r0, r1
  sublt r0, r0, r1
  subgt r0, r0, r1
  suble r0, r0, r1
  subal r0, r0, r1
  bx	lr
  .size func, .-func
