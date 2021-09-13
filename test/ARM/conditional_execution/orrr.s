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
  oreq r0, r0, r1
  orne r0, r0, r1
  orcs r0, r0, r1
  orcc r0, r0, r1
  ormi r0, r0, r1
  orpl r0, r0, r1
  orvs r0, r0, r1
  orvc r0, r0, r1
  orhi r0, r0, r1
  orls r0, r0, r1
  orge r0, r0, r1
  orlt r0, r0, r1
  orgt r0, r0, r1
  orle r0, r0, r1
  oral r0, r0, r1
  bx	lr
  .size func, .-func
