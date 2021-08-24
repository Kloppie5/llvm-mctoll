//===- ARMCreateJumpTable.h - Binary raiser utility llvm-mctoll -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of ARMCreateJumpTable
// class for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMCREATEJUMPTABLE_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMCREATEJUMPTABLE_H

#include "RaiserPass.h"
#include "MCInstRaiser.h"
#include "MachineFunctionRaiser.h"

class ARMCreateJumpTable : public RaiserPass {
public:
  ARMCreateJumpTable(ModuleRaiser &MR, MCInstRaiser *MCIR) : RaiserPass(MR), MCIR(MCIR) {};
  bool run(MachineFunction *MF, Function *F) override;
  bool getJTlist(std::vector<JumpTableInfo> &List);
  
private:
  unsigned int getARMCPSR(unsigned int PhysReg);
  bool raiseMachineJumpTable(MachineFunction &MF);
  /// Get the MachineBasicBlock to add the jumptable instruction.
  MachineBasicBlock *checkJumptableBB(MachineFunction &MF);
  bool UpdatetheBranchInst(MachineBasicBlock &MBB);

  std::vector<JumpTableInfo> jtList;
  MCInstRaiser *MCIR;
};
#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMCREATEJUMPTABLE_H
