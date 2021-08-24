//===-- MachineFunctionRaiser.h ---------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MCTOLL_MACHINEFUNCTIONRAISER_H
#define LLVM_MCTOLL_MACHINEFUNCTIONRAISER_H


#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

// Structure holding all necessary information to raise control
// transfer (i.e., branch) instructions during a post-processing
// phase.
// TODO: Move
typedef struct ControlTransferInfo_t {
  BasicBlock *CandidateBlock;
  // This is the MachineInstr that needs to be raised
  const MachineInstr *CandidateMachineInstr;
  // A vector of values that could be of use while raising
  // CandidateMachineInstr. If it is a call instruction,
  // this vector has the Values corresponding to argument
  // registers (TODO : need to handles arguments passed on stack)
  // If this is a conditional branch instruction, it contains the
  // EFLAG bit values.
  std::vector<Value *> RegValues;
  // Flag to indicate that CandidateMachineInstr has been raised
  bool Raised;
} ControlTransferInfo;

class MachineFunctionRaiser {
  public:
    ModuleRaiser &MR;
    MachineFunction &MF;
    MCInstRaiser *MCIR;

    Function *F;
    std::vector<BasicBlock *> BasicBlocks;

    // A vector of information to be used for raising of control transfer
    // (i.e., Call and Terminator) instructions.
    std::vector<ControlTransferInfo *> CTInfo;

    MachineFunctionRaiser(ModuleRaiser &MR, MachineFunction &MF, MCInstRaiser *MCIR) : MR(MR), MF(MF), MCIR(MCIR) {}
    virtual FunctionType *getRaisedFunctionPrototype() = 0;

    bool raise();
  
};

#endif // LLVM_MCTOLL_MACHINEFUNCTIONRAISER_H
