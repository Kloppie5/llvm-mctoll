
#include "ARMSelectionDAGISelBypassPass.h"

#include "DAGBuilder.h"
#include "DAGRaisingInfo.h"
#include "FunctionRaisingInfo.h"
#include "IREmitter.h"
#include "InstSelector.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMSelectionDAGISelBypassPass::precondition(MachineFunction *MF, Function *F) {
  return true;
}

bool ARMSelectionDAGISelBypassPass::run(MachineFunction *MF, Function *F) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
  
  if (!precondition(MF, F)) {
    return false;
  }

  std::unique_ptr<OptimizationRemarkEmitter> ORE = make_unique<OptimizationRemarkEmitter>(F);

  FunctionRaisingInfo *FuncInfo;
  FuncInfo = new FunctionRaisingInfo();

  SelectionDAG *CurDAG;
  CurDAG = new SelectionDAG(*MR.getTargetMachine(), CodeGenOpt::None);
  CurDAG->init(*MF, *ORE.get(), nullptr, nullptr, nullptr, nullptr, nullptr);
  
  DAGRaisingInfo *DAGInfo;
  DAGInfo = new DAGRaisingInfo(*CurDAG);
  
  FuncInfo->set(AMR, *F, *MF, CurDAG);

  DAGBuilder *SDB;
  SDB = new DAGBuilder(*DAGInfo, *FuncInfo);
  
  InstSelector *SLT;
  SLT = new InstSelector(*DAGInfo, *FuncInfo);
  
  MachineBasicBlock *MBB;
  BasicBlock *BB;

  LLVM_DEBUG(dbgs() << "ARMSelectionDAGISelBypassPass start.\n");

  { // Create 4 argument store instructions
    BasicBlock *bb = &F->getEntryBlock();
    for (unsigned i = 0; i < 4; i++) {
      Align MALG(32);
      AllocaInst *Alloc = new AllocaInst(Type::getInt1Ty(F->getContext()), 0,
                                        nullptr, MALG, "", bb);
      FuncInfo->AllocaMap[i] = Alloc;
      new StoreInst(ConstantInt::getFalse(F->getContext()), Alloc, bb);
    }
  }

  for (MachineBasicBlock &mbb : *MF) {
    MBB = &mbb;
    BB = FuncInfo->getOrCreateBasicBlock(MBB);

    for (MachineBasicBlock::const_iterator I = MBB->begin(), E = MBB->end();
         I != E; ++I) {
      SDB->visit(*I);
    }

    { // Select
      dbgs() << "Selecting " << BB->getName() << "\n";
      dbgs() << "Consisting of " << CurDAG->allnodes_size() << " nodes.\n";
      SelectionDAG::allnodes_iterator ISelPosition = CurDAG->allnodes_begin();
      while (ISelPosition != CurDAG->allnodes_end()) {
        SDNode *Node = &*ISelPosition++;
        SLT->select(Node);
      }
    }

    { // Emit
      dbgs() << "Emitting " << BB->getName() << "\n";
      dbgs() << "Consisting of " << CurDAG->allnodes_size() << " nodes.\n";
      IREmitter imt(BB, DAGInfo, FuncInfo);
      imt.setjtList(jtList);
      SelectionDAG::allnodes_iterator ISelPosition = CurDAG->allnodes_begin();
      while (ISelPosition != CurDAG->allnodes_end()) {
        SDNode *Node = &*ISelPosition++;
        imt.emitNode(Node);
      }
    }

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

  LLVM_DEBUG(dbgs() << "ARMSelectionDAGISelBypassPass end.\n");

  return true;
}

#undef DEBUG_TYPE
