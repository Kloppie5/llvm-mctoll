# RUN: clang -target arm -mfloat-abi=soft -c -o %t.o %s
# RUN: llvm-mctoll -d -debug  %t.o 2>&1 | FileCheck %s

  .text
  .align 4
  .code 32
  .global func
  .type func, %function
func:

  MOVEQ r0, r1

# CHECK: ARMConditionalExecutionPass {

# CHECK:        MOVr (874) { Reg:$R0 Reg:$R1 Imm:0 Reg:$CPSR Reg:$ }
# CHECK-NEXT: } => {
# CHECK-NEXT:   Bcc (720) { Imm:8 Imm:1 Reg:$CPSR }
# CHECK-NEXT:   MOVr (874) { Reg:$R0 Reg:$R1 Imm:14 Reg:$ Reg:$ }

# CHECK: } ARMConditionalExecutionPass

  # bb.0:
  # $r0 = MOVr $r1, 0, $cpsr, $noreg, <0x55a1bdc3ec88>
  # BX_RET 14, $noreg, <0x55a1bdc3f0d8>


#  bb.0:
#  successors: %bb.2, %bb.1
#  liveins: $cpsr
#  Bcc 4, 1, $cpsr, <0x5654185d1e98>

#bb.1:
#; predecessors: %bb.0
#  successors: %bb.2
#  liveins: $r1
#  $r0 = MOVr $r1, 14, $noreg, $noreg, <0x5654185d21e8>

#bb.2:
#; predecessors: %bb.0, %bb.1

  bx	lr
  .size func, .-func
