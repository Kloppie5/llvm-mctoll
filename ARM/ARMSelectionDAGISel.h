//===- ARMSelectionDAGISel.h ------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of ARMSelectionDAGISel class for
// use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISEL_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISEL_H

#include "RaiserPass.h"
#include "DAGBuilder.h"
#include "DAGRaisingInfo.h"
#include "FunctionRaisingInfo.h"
#include "IREmitter.h"
#include "InstSelector.h"
#include "ModuleRaiser.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"

/// This is responsible for constructing DAG, and does instruction selection on
/// the DAG, eventually emits SDNodes of the DAG to LLVM IRs.
class ARMSelectionDAGISel : public RaiserPass {
public:
  ARMSelectionDAGISel(ModuleRaiser &MR, std::vector<JumpTableInfo> &List) : RaiserPass(MR), jtList(List) {}
  bool run (MachineFunction *MF, Function *F) override;

private:
  void initEntryBasicBlock(Function *F);
  void selectBasicBlock();
  void doInstructionSelection();
  void emitDAG();

  std::unique_ptr<OptimizationRemarkEmitter> ORE;

  FunctionRaisingInfo *FuncInfo;
  DAGBuilder *SDB;
  InstSelector *SLT;

  SelectionDAG *CurDAG;
  DAGRaisingInfo *DAGInfo;
  MachineBasicBlock *MBB;
  BasicBlock *BB;
  std::vector<JumpTableInfo> jtList;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISEL_H
