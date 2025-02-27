//===-- ARMRaiserBase.h -----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the ARMRaiserBase class. This class
// is a base class that provides some basic utilities to ARM raisers.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_RAISERPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_RAISERPASS_H

#include "ModuleRaiser.h"

using namespace llvm;

class RaiserPass {
protected:
  LLVMContext &Context;
  ModuleRaiser &MR;

  RaiserPass() = delete;
  RaiserPass(ModuleRaiser &MR) : Context(MR.getModule()->getContext()), MR(MR) {}

  virtual bool precondition (MachineFunction *MF, Function *F) { return true; }
  virtual bool run (MachineFunction *MF) { return false; }
  virtual bool run (MachineFunction *MF, Function *F) { return false; }
  virtual bool run (Function *F) { return false;}
  virtual bool postcondition (MachineFunction *MF, Function *F) { return true; }
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_RAISERPASS_H
