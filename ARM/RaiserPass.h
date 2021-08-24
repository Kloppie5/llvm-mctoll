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
  ModuleRaiser &MR;

  RaiserPass() = delete;
  RaiserPass(ModuleRaiser &MR) : MR(MR) {}

  virtual bool run (MachineFunction *MF, Function *F) = 0;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_RAISERPASS_H
