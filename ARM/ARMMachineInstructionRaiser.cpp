//===-- ARMEliminatePrologEpilog.cpp ----------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of ARMMachineInstructionRaiser class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "ARMMachineInstructionRaiser.h"
#include "MachineFunctionRaiser.h"
#include "ARMModuleRaiser.h"
#include "ARMFunctionPrototype.h"

#include "ARMBenchmarker.h"
#include "ARMLinearRaiserPass.h"

using namespace llvm;

ARMMachineInstructionRaiser::ARMMachineInstructionRaiser(
    MachineFunction &machFunc, const ModuleRaiser *mr, MCInstRaiser *mcir)
    : MachineInstructionRaiser(machFunc, mr, mcir),
      machRegInfo(MF.getRegInfo()) {}

bool ARMMachineInstructionRaiser::raiseMachineFunction() {
  const ARMModuleRaiser *amr = dyn_cast<ARMModuleRaiser>(MR);
  assert(amr != nullptr && "The ARM module raiser is not initialized!");
  ARMModuleRaiser &rmr = const_cast<ARMModuleRaiser &>(*amr);

  ARMBenchmarker bm(rmr);
  bm.run(&MF, raisedFunction);

  ARMLinearRaiserPass sdis(rmr, mcInstRaiser);
  sdis.run(&MF, raisedFunction);

  return true;
}

bool ARMMachineInstructionRaiser::raise() {
  raiseMachineFunction();
  return true;
}

int ARMMachineInstructionRaiser::getArgumentNumber(unsigned PReg) {
  // NYI
  assert(false &&
         "Unimplemented ARMMachineInstructionRaiser::getArgumentNumber()");
  return -1;
}

bool ARMMachineInstructionRaiser::buildFuncArgTypeVector(
    const std::set<MCPhysReg> &PhysRegs, std::vector<Type *> &ArgTyVec) {
  // NYI
  assert(false &&
         "Unimplemented ARMMachineInstructionRaiser::buildFuncArgTypeVector()");
  return false;
}

Value *ARMMachineInstructionRaiser::getRegOrArgValue(unsigned PReg, int MBBNo) {
  assert(false &&
         "Unimplemented ARMMachineInstructionRaiser::getRegOrArgValue()");
  return nullptr;
}

FunctionType *ARMMachineInstructionRaiser::getRaisedFunctionPrototype() {
  ARMFunctionPrototype AFP;
  raisedFunction = AFP.discover(MF);

  Function *ori = const_cast<Function *>(&MF.getFunction());
  // Insert the map of raised function to tempFunctionPointer.
  const_cast<ModuleRaiser *>(MR)->insertPlaceholderRaisedFunctionMap(
      raisedFunction, ori);

  return raisedFunction->getFunctionType();
}

// Create a new MachineFunctionRaiser object and add it to the list of
// MachineFunction raiser objects of this module.
MachineFunctionRaiser *ARMModuleRaiser::CreateAndAddMachineFunctionRaiser(
    Function *f, const ModuleRaiser *mr, uint64_t start, uint64_t end) {
  MachineFunctionRaiser *mfRaiser = new MachineFunctionRaiser(
      *M, mr->getMachineModuleInfo()->getOrCreateMachineFunction(*f), mr, start,
      end);
  mfRaiser->setMachineInstrRaiser(new ARMMachineInstructionRaiser(
      mfRaiser->getMachineFunction(), mr, mfRaiser->getMCInstRaiser()));
  mfRaiserVector.push_back(mfRaiser);
  return mfRaiser;
}
