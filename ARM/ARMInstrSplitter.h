//===- ARMInstrSplitter.h --------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of ARMInstrSplitter class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMINSTRSPLITTER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMINSTRSPLITTER_H

#include "RaiserPass.h"

#include "ARMBaseInstrInfo.h"
#include "ARMSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"

using namespace llvm;

/// Some instructions which their patterns include more than one operations,
/// like 'add r0, r1, r0, asr r1' or 'ldr r0, [r1, #4]', are splitted into
/// multiple MIs at here.
class ARMInstrSplitter : public RaiserPass {
public:
  ARMInstrSplitter(ModuleRaiser &MR) : RaiserPass(MR) {};

  bool run (MachineFunction *MF, Function *F) override;
  
  bool splitMachineInstr(MachineBasicBlock *MBB, MachineInstr *MI);
  void splitLDRSTR(MachineBasicBlock *MBB, MachineInstr *MI, bool isLoad, bool isSO, bool isPre, bool isPost);
  void splitOperand2(MachineBasicBlock *MBB, MachineInstr *MI, unsigned idx, unsigned NewOpcode);

private:
  /// Check if the MI has shift pattern.
  unsigned getUnshiftedOpcode(unsigned Opcode);
  /// Get the shift opcode in MI.
  unsigned getShiftOpcode(ARM_AM::ShiftOpc SOpc, bool isReg = false);

  MachineRegisterInfo *MRI;
  const ARMBaseInstrInfo *TII;
  LLVMContext *CTX;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMINSTRSPLITTER_H
