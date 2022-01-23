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
  orreq r0, r0, r1
  orrne r0, r0, r1
  orrcs r0, r0, r1
  orrcc r0, r0, r1
  orrmi r0, r0, r1
  orrpl r0, r0, r1
  orrvs r0, r0, r1
  orrvc r0, r0, r1
  orrhi r0, r0, r1
  orrls r0, r0, r1
  orrge r0, r0, r1
  orrlt r0, r0, r1
  orrgt r0, r0, r1
  orrle r0, r0, r1
  orral r0, r0, r1
  bx	lr
  .size func, .-func
