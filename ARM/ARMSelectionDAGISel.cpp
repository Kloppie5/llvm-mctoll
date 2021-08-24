//===- ARMSelectionDAGISel.cpp - Binary raiser utility llvm-mctoll --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of ARMSelectionDAGISel class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "ARMSelectionDAGISel.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMSelectionDAGISel::run(MachineFunction *MF, Function *F) {
  ORE = make_unique<OptimizationRemarkEmitter>(F);
  FuncInfo = new FunctionRaisingInfo();
  CurDAG = new SelectionDAG(*MR.getTargetMachine(), CodeGenOpt::None);
  DAGInfo = new DAGRaisingInfo(*CurDAG);
  SDB = new DAGBuilder(*DAGInfo, *FuncInfo);
  SLT = new InstSelector(*DAGInfo, *FuncInfo);

  LLVM_DEBUG(dbgs() << "ARMSelectionDAGISel start.\n");

  CurDAG->init(*MF, *ORE.get(), nullptr, nullptr, nullptr, nullptr, nullptr);
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser&>(MR);
  FuncInfo->set(AMR, *F, *MF, CurDAG);

  initEntryBasicBlock(F);
  for (MachineBasicBlock &mbb : *MF) {
    MBB = &mbb;
    BB = FuncInfo->getOrCreateBasicBlock(MBB);
    selectBasicBlock();
  }

  // Add an additional exit BasicBlock, all of original return BasicBlocks
  // will branch to this exit BasicBlock. This will lead to the function has
  // one and only exit. If the function has return value, this help return
  // R0.
  BasicBlock *LBB = FuncInfo->getOrCreateBasicBlock();

  if (F->getReturnType()) {
    PHINode *LPHI = PHINode::Create(F->getReturnType(),
                                    FuncInfo->RetValMap.size(), "", LBB);
    for (auto Pair : FuncInfo->RetValMap)
      LPHI->addIncoming(Pair.second, Pair.first);

    ReturnInst::Create(F->getContext(), LPHI, LBB);
  } else
    ReturnInst::Create(F->getContext(), LBB);

  for (auto &FBB : F->getBasicBlockList())
    if (FBB.getTerminator() == nullptr)
      BranchInst::Create(LBB, &FBB);

  FuncInfo->clear();

  LLVM_DEBUG(dbgs() << "ARMSelectionDAGISel end.\n");

  return true;
}

void ARMSelectionDAGISel::selectBasicBlock() {

  for (MachineBasicBlock::const_iterator I = MBB->begin(), E = MBB->end();
       I != E; ++I) {
    SDB->visit(*I);
  }

  doInstructionSelection();
  emitDAG();

  // If the current function has return value, records relationship between
  // BasicBlock and each Value which is mapped with R0. In order to record
  // the return Value of each exit BasicBlock.
  Type *RTy = FuncInfo->Fn->getReturnType();
  if (RTy != nullptr && !RTy->isVoidTy() && MBB->succ_size() == 0) {
    Instruction *TInst = dyn_cast<Instruction>(
        DAGInfo->getRealValue(FuncInfo->RegValMap[ARM::R0]));
    assert(TInst && "A def R0 was pointed to a non-instruction!!!");
    BasicBlock *TBB = TInst->getParent();
    FuncInfo->RetValMap[TBB] = TInst;
  }

  // Free the SelectionDAG state, now that we're finished with it.
  DAGInfo->clear();
  CurDAG->clear();
}

void ARMSelectionDAGISel::doInstructionSelection() {

  SelectionDAG::allnodes_iterator ISelPosition = CurDAG->allnodes_begin();
  while (ISelPosition != CurDAG->allnodes_end()) {
    SDNode *Node = &*ISelPosition++;
    SLT->select(Node);
  }
}

void ARMSelectionDAGISel::emitDAG() {
  IREmitter imt(BB, DAGInfo, FuncInfo);
  imt.setjtList(jtList);
  SelectionDAG::allnodes_iterator ISelPosition = CurDAG->allnodes_begin();
  while (ISelPosition != CurDAG->allnodes_end()) {
    SDNode *Node = &*ISelPosition++;
    imt.emitNode(Node);
  }
}

void ARMSelectionDAGISel::initEntryBasicBlock(Function *F) {
  BasicBlock *bb = &F->getEntryBlock();
  for (unsigned i = 0; i < 4; i++) {
    Align MALG(32);
    AllocaInst *Alloc = new AllocaInst(Type::getInt1Ty(F->getContext()), 0,
                                       nullptr, MALG, "", bb);
    FuncInfo->AllocaMap[i] = Alloc;
    new StoreInst(ConstantInt::getFalse(F->getContext()), Alloc, bb);
  }
}

#undef DEBUG_TYPE
