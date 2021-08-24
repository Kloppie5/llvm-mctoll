
#include "ARMSelectionDAGISelBypassPass.h"

#include "DAGBuilder.h"
#include "DAGRaisingInfo.h"
#include "FunctionRaisingInfo.h"
#include "IREmitter.h"
#include "InstSelector.h"
#include "Monitor.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMSelectionDAGISelBypassPass::precondition(MachineFunction *MF, Function *F) {
  Monitor::NOTE("ARMSelectionDAGISelBypassPass; precondition");
  auto OS = WithColor(errs(), HighlightColor::Warning);
  for (MachineBasicBlock &MBB : *MF) {
    OS << "In MBB: " << MBB.getFullName() << "\n";
    for (MachineInstr &MI : MBB) {
      
      switch (MI.getOpcode()) {
        default:
          OS << "ARMSelectionDAGISelBypassPass encountered unhandled opcode in instruction ";
          Monitor::printMachineInstr(&MI, true, OS);
          return false;
        
        case ARM::LDAEXB: // 797
        case ARM::LDREXB: // 837
        case ARM::STLEXB: // 1888
        case ARM::STREXB: // 1912
          OS << "LL-SC loop instruction does not have a single instruction decompilation: ";
          Monitor::printMachineInstr(&MI, true, OS);
          break;

        case ARM::ASRi: // 247
        case ARM::ASRr: // 248
        case ARM::LSLi: // 292
        case ARM::LSLr: // 293
        case ARM::LSRi: // 294
        case ARM::LSRr: // 295
        case ARM::RORi: // 325
        case ARM::RORr: // 326
        case ARM::RRX: // 327

        case ARM::ADCri: // 680
        case ARM::ADCrsi: // 682
        case ARM::ADDri: // 684
        case ARM::ADDrr: // 685
        case ARM::ADDrsi: // 686
        case ARM::ANDri: // 693
        case ARM::BICri: // 706
        case ARM::BL: // 711
        case ARM::BX_RET: // 718
        case ARM::Bcc: // 720
        case ARM::CMNri: // 755
        case ARM::CMPri: // 759
        case ARM::CMPrr: // 760
        case ARM::DMB: // 773
        case ARM::DSB: // 774
        case ARM::EORri: // 775
        case ARM::EORrr: // 776
        case ARM::ISB: // 793
        case ARM::LDMIA: // 821
        case ARM::LDMIA_UPD: // 822
        case ARM::LDRBi12: // 831
        case ARM::LDRBrs: // 832
        case ARM::LDRH: // 840
        case ARM::LDRSB: // 845
        case ARM::LDRSB_PRE: // 849
        case ARM::LDR_PRE_IMM: // 859
        case ARM::LDR_PRE_REG: // 860
        case ARM::LDRi12: // 862
        case ARM::LDRrs: // 863
        case ARM::MLA: // 868
        case ARM::MOVPCLR: // 870
        case ARM::MOVTi16: // 871
        case ARM::MOVi: // 872
        case ARM::MOVi16: // 873
        case ARM::MOVr: // 874
        case ARM::MOVsi: // 876
        case ARM::MUL: // 888
        case ARM::MVNi: // 1736
        case ARM::MVNr: // 1737
        case ARM::ORRri: // 1748
        case ARM::ORRrr:// 1749
        case ARM::SBCri: // 1794
        case ARM::SBCrr: // 1795
        case ARM::SBCrsi: // 1796
        case ARM::SMLAL: // 1824
        case ARM::STMDB_UPD: // 1895
        case ARM::STMIA: // 1896
        case ARM::STMIA_UPD: // 1897
        case ARM::STMIB: // 1898
        case ARM::STRBi12: // 1906
        case ARM::STRBrs: // 1907
        case ARM::STRH: // 1915
        case ARM::STRi12: // 1926
        case ARM::STRrs: // 1927
        case ARM::SUBri: // 1928
        case ARM::SUBrr: // 1929
        case ARM::SVC: // 1932
          OS << "Allowed instruction: ";
          Monitor::printMachineInstr(&MI, true, OS);
          break;
      }
    }
  }

  Monitor::NOTE("ARMSelectionDAGISelBypassPass; precondition passed");
  return true;
}

bool ARMSelectionDAGISelBypassPass::run(MachineFunction *MF, Function *F) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
  
  if (!precondition(MF, F))
    return false;

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
