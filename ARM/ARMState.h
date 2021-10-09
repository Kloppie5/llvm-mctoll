#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSTATE_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSTATE_H

#include "Monitor.h"

using namespace llvm;

class ARMState {
  // General Purpose
  Value* R0_usr, * R1_usr, * R2_usr, * R3_usr,
       * R4_usr, * R5_usr, * R6_usr, * R7_usr,
       * R8_usr, * R9_usr, * R10_usr;
  // Semi General Purpose
  Value* FP_usr;
  bool R11_is_FP;
  int64_t FP_offset;
  Value* IP_usr;
  Value* SP_usr;
  int64_t SP_offset;
  Value* LR_usr;
  // Program Counter
  Value* PC;
  // CPSR Register
  Value* CPSR_N_flag,
       * CPSR_Z_flag,
       * CPSR_C_flag,
       * CPSR_V_flag;

  // VFP Register Banks
  Value* S0 , * S1 , * S2 , * S3 , * S4 , * S5 , * S6 , * S7 ,
       * S8 , * S9 , * S10, * S11, * S12, * S13, * S14, * S15,
       * S16, * S17, * S18, * S19, * S20, * S21, * S22, * S23,
       * S24, * S25, * S26, * S27, * S28, * S29, * S30, * S31;
  Value* D0 ,        * D1 ,        * D2 ,        * D3 ,
       * D4 ,        * D5 ,        * D6 ,        * D7 ,
       * D8 ,        * D9 ,        * D10,        * D11,
       * D12,        * D13,        * D14,        * D15;
  // FPSCR Register
  Value* FPSCR_N_flag,
       * FPSCR_Z_flag,
       * FPSCR_C_flag,
       * FPSCR_V_flag,
       * FPSCR_Stride,
       * FPSCR_Len;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSTATE_H
