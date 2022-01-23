#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSTATE_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSTATE_H

#include "Monitor.h"

#include "ARMSubtarget.h"

using namespace llvm;

/**
 * The ARMState class is used to represent the partially abstract state of an ARM
 * machine.
 */
class ARMState {

public:
  ARMState() {}
  ~ARMState() {}

  /*-+- Registers -+-*/
  std::map<Register, Value*> register_map;
  /**
    General Purpose
      R0_usr
      R1_usr
      R2_usr
      R3_usr
      R4_usr
      R5_usr
      R6_usr
      R7_usr
      R8_usr
      R9_usr
      R10_usr
    Semi General Purpose
      R11 / FP_usr
      R12 / IP_usr
      R13 / SP_usr
      R14 / LR_usr
    Program Counter
      R15 / PC

    VFP Register Banks
      S0,  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10, S11, S12, S13, S14, S15
      S16, S17, S18, S19, S20, S21, S22, S23, S24, S25, S26, S27, S28, S29, S30, S31
      D0,  D1,  D2,  D3,  D4,  D5,  D6,  D7,  D8,  D9,  D10, D11, D12, D13, D14, D15

    VFPv3-D32+ Register Banks
      D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26, D27, D28, D29, D30, D31
  */

  void setReg(Register reg, Value *V, bool clear_overlapping = true) {
    if ((reg >= ARM::R0 && reg <= ARM::R10) || reg == ARM::R12 || reg == ARM::LR) {
      register_map[reg] = V;
      return;
    }
    if (reg == ARM::R11) {
      register_map[reg] = V;
      R11_is_FP = false;
      return;
    }
    if (reg == ARM::SP) {
      register_map[reg] = V;
      R13_is_SP = false;
      return;
    }
    if (reg == ARM::PC) {
      assert(false && "Directly updating PC is not supported");
    }
    if (reg >= ARM::S0 && reg <= ARM::S31) {
      register_map[reg] = V;
      if (clear_overlapping) {
        Register bank = ARM::D0 + (reg - ARM::S0) / 2;
        register_map[bank] = nullptr;
      }
      return;
    }

    if (reg >= ARM::D0 && reg <= ARM::D31) {
      register_map[reg] = V;

      if (/* ARMv7 && */ clear_overlapping && reg <= ARM::D15 ) {
        Register bank0 = ARM::S0 + (reg - ARM::D0) * 2;
        Register bank1 = bank0 + 1;
        register_map[bank0] = nullptr;
        register_map[bank1] = nullptr;
      }
      return;
    }

    assert(false && "Unknown register");
  }
  Value *getReg(Register reg) {
    return register_map[reg];
  }

  /*-+- Status Flags -+-*/
  enum Status {
    INVALID = 0,
    CPSR_N,
    CPSR_Z,
    CPSR_C,
    CPSR_V,
    FPSCR_N,
    FPSCR_Z,
    FPSCR_C,
    FPSCR_V,
    FPSCR_Stride,
    FPSCR_Len
  };
  std::map<Status, Value*> status_map;

  void setStatus(Status status, Value *V) {
    status_map[status] = V;
  }
  Value* getStatus(Status status) {
    return status_map[status];
  }

  /*-+- Stack -+-*/
  bool R11_is_FP;
  int64_t FP_offset;
  bool R13_is_SP;
  int64_t SP_offset;
  std::map<int64_t, AllocaInst*> stack_map;

  void setStack(int64_t offset, AllocaInst *AI) {
    stack_map[offset] = AI;
  }
  AllocaInst* getStack(int64_t offset) {
    return stack_map[offset];
  }

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSTATE_H
