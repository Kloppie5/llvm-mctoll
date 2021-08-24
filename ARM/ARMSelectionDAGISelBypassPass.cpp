
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

Value getRegValue(Register r) {
  return FuncInfo->RegValueMap[r];
}
void setRegValue(Register r, int MBBno, Value v) {
  FuncInfo->RegValueMap[r] = v;
}

bool raiseMachineInstr(BasicBlock &BB, MachineInstr &MI) {
  switch (MI.getOpcode()) {
    default:
      OS << "ARMSelectionDAGISelBypassPass encountered unhandled opcode in instruction ";
      Monitor::printMachineInstr(&MI, true, OS);
      return false;

    case ARM::ASRi: { // 247 | ASR{S}<c> <Rd>, <Rn>, #<imm> => Rd = AShr(<Rn>, imm)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::AShr, Rn, imm, "ASRi", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::ASRr: { // 248 | ASR{S}<c> <Rd>, <Rn>, <Rm> => Rd = AShr(<Rn>, <Rm>)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::AShr, Rn, Rm, "ASRr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::LSLi: { // 292 | LSL{S}<c> <Rd>, <Rn>, #<imm> => Rd = Shl(<Rn>, imm)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::Shl, Rn, imm, "LSLi", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::LSLr: { // 293 | LSL{S}<c> <Rd>, <Rn>, <Rm> => Rd = Shl(<Rn>, <Rm>)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::Shl, Rn, Rm, "LSLr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::LSRi: { // 294 | LSR{S}<c> <Rd>, <Rn>, #<imm> => Rd = LShr(<Rn>, imm)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::LShr, Rn, imm, "LSRi", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::LSRr: { // 295 | LSR{S}<c> <Rd>, <Rn>, <Rm> => Rd = LShr(<Rn>, <Rm>)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::LShr, Rn, Rm, "LSRr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::RORi: { // 325 | ROR{S}<c> <Rd>, <Rn>, #<imm> => Rd = OR(Shl(<Rn>, imm), LShr(<Rn>, Sub(32, imm)))
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Shl = BinaryOperator::Create(BinaryOps::Shl, Rn, imm, "RORiShl", BB);
      Instruction Sub = BinaryOperator::Create(BinaryOps::Sub, ConstantInt::get(Rn.getType(), 32), imm, "RORiSub", BB);
      Instruction LShr = BinaryOperator::Create(BinaryOps::LShr, Rn, Sub, "RORiLShr", BB);
      Instruction Instr = BinaryOperator::Create(BinaryOps::Or, Shl, LShr, "RORiOR", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::RORr: { // 326 | ROR{S}<c> <Rd>, <Rn>, <Rm> => Rd = OR(Shl(<Rn>, <Rm>), LShr(<Rn>, Sub(32, <Rm>)))
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Shl = BinaryOperator::Create(BinaryOps::Shl, Rn, Rm, "RORrShl", BB);
      Instruction Sub = BinaryOperator::Create(BinaryOps::Sub, ConstantInt::get(Rn.getType(), 32), Rm, "RORrSub", BB);
      Instruction LShr = BinaryOperator::Create(BinaryOps::LShr, Rn, Sub, "RORrLShr", BB);
      Instruction Instr = BinaryOperator::Create(BinaryOps::Or, Shl, LShr, "RORrOR", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::RRX: { // 327 | RRX{S}<c> <Rd>, <Rm> => ?
      assert(false && "RRX not yet implemented; requires Carry flag");
    } break;
    case ARM::ADCri: { // 680 | ADC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn + Imm
      assert(false && "RRX not yet implemented; requires Carry flag");
    } break;
    case ARM::ADCrsi: { // 682 | ADC{S}<c> <Rd>, <Rn>, <Rm>{,<shift>}
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::ADDri: { // 684 | ADD{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn + Imm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::Add, Rn, imm, "ADDri", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::ADDrr: { // 685 | ADD{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn + Rm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::Add, Rn, Rm, "ADDrr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::ADDrsi: { // 686 | ADD{S}<c> <Rd>, <Rn>, #<imm>
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::ANDri: { // 693 | AND{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & Imm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::And, Rn, imm, "ANDri", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::BICri: { // 706 | BIC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & ~Imm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());

      Instruction Instr = BinaryOperator::Create(BinaryOps::And, Rn, ConstantInt::get(Rn.getType(), ~imm), "BICri", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::BL: { // 711 | BL <label> => br label
      assert(false && "ARM::BL not yet implemented; requires PC-relative branch");
      // BasicBlock *Target = getBlockFromAddress(MI.getOperand(0).getImm());
      // Instruction Instr = BranchInst::Create(Target, BB);
      // Instr.setSuccessor(0, BB);
      // setValue(MI.getOperand(0).getReg(), BB->getNumber(), Instr);
    } break;
    case ARM::BX_RET: { // 718 | BX_RET <Rn> => br label
      assert(false && "ARM::BX_RET not yet implemented; requires PC-relative branch");
      // BasicBlock *Target = getBlockFromAddress(MI.getOperand(0).getImm());
      // Instruction Instr = BranchInst::Create(Target, BB);
      // Instr.setSuccessor(0, BB);
      // setValue(MI.getOperand(0).getReg(), BB->getNumber(), Instr);
    } break;
    case ARM::Bcc: { // 720 | Bcc <cond> <label> => br <cond> label
      assert(false && "ARM::Bcc not yet implemented; requires PC-relative branch");
      // BasicBlock *Target = getBlockFromAddress(MI.getOperand(1).getImm());
      // Instruction Instr = BranchInst::Create(Target, BB);
      // Instr.setSuccessor(0, BB);
      // setValue(MI.getOperand(0).getReg(), BB->getNumber(), Instr);
    } break;
    case ARM::CMNri: { // 755 | CMN{S}<c> <Rn>, #<imm> => Rn + Imm
      assert(false && "ARM::CMNri not yet implemented; requires NZCV flags");
    } break;
    case ARM::CMPri: { // 759 | CMP{S}<c> <Rn>, #<imm> => Rn - Imm
      assert(false && "ARM::CMPri not yet implemented; requires NZCV flags");
    } break;
    case ARM::CMPrr: { // 760 | CMP{S}<c> <Rn>, <Rm> => Rn - Rm
      assert(false && "ARM::CMPrr not yet implemented; requires NZCV flags");
    } break;
    case ARM::DMB: { // 773 | DMB <option> => UNDEF
      assert(false && "ARM::DMB not yet implemented; fence");
    } break;
    case ARM::DSB: { // 774 | DSB <option> => UNDEF
      assert(false && "ARM::DSB not yet implemented; fence");
    }
    case ARM::EORri: { // 775 | EOR{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn ^ Imm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());

      Instruction Instr = BinaryOperator::Create(BinaryOps::Xor, Rn, imm, "EORri", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::EORrr: { // 776 | EOR{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn ^ Rm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::Xor, Rn, Rm, "EORrr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::ISB: { // 793 | ISB <option> => UNDEF
      assert(false && "ARM::ISB not yet implemented; fence");
    } break;
    case ARM::LDMIA: { // 821 | LDMIA <Rn>{!}, <registers> => Rn = Rn + 4 * registers
      assert(false && "ARM::LDMIA not yet implemented");
    } break;
    case ARM::LDMIA_UPD: { // 822 | LDMIA_UPD <Rn>{!}, <registers> => Rn = Rn + 4 * registers
      assert(false && "ARM::LDMIA_UPD not yet implemented");
    } break;
    
    case ARM::LDRBi12: { // 831 | LDRB<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      Register Rt = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Value Ptr = BinaryOperator::CreateAdd(Rn, imm, "LDRBi12Ptr", BB);
      Instruction Instr = LoadInst::Create(Rt, Ptr, "LDRBi12", BB);
      setRegValue(Rt, MI.getParent()->getNumber(), Instr);
    } break;
      
    case ARM::LDRBrs: { // 832 | LDRB<c> <Rt>, [<Rn>, <Rm>] => Rt = *(Rn + Rm)
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::LDRH: { // 840 | LDRH<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      Register Rt = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());

      Value Ptr = BinaryOperator::CreateAdd(Rn, imm, "LDRHPtr", BB);
      Instruction Instr = LoadInst::Create(Rt, Ptr, "LDRH", BB);
      setRegValue(Rt, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::LDRSB: { // 845 | LDRSB<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      assert(false && "ARM::LDRSB not yet implemented");
    } break;
    case ARM::LDRSB_PRE: { // 849 | LDRSB_PRE<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      assert(false && "ARM::LDRSB_PRE not yet implemented");
    } break;
    case ARM::LDR_PRE_IMM: { // 859 | LDR_PRE_IMM<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      assert(false && "ARM::LDR_PRE_IMM not yet implemented");
    } break;
    case ARM::LDR_PRE_REG: { // 860 | LDR_PRE_REG<c> <Rt>, [<Rn>, <Rm>] => Rt = *(Rn + Rm)
      assert(false && "ARM::LDR_PRE_REG not yet implemented");
    } break;
    case ARM::LDRi12: { // 862 | LDR<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      Register Rt = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());

      Value Ptr = BinaryOperator::CreateAdd(Rn, imm, "LDRi12Ptr", BB);
      Instruction Instr = LoadInst::Create(Rt, Ptr, "LDRi12", BB);
      setRegValue(Rt, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::LDRrs: { // 863 | LDR<c> <Rt>, [<Rn>, <Rm>] => Rt = *(Rn + Rm)
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::MLA: { // 868 | MLA<c> <Rd>, <Rn>, <Rm>, <Ra> => Rd = Rn * Ra + Rm
      assert(false && "ARM::MLA not yet implemented");
    } break;
    case ARM::MOVPCLR: { // 870 | MOV<c> PC, LR => PC = LR
      assert(false && "ARM::MOVPCLR not yet implemented");
    } break;
    case ARM::MOVTi16: { // 871 | MOVT<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVTi16 not yet implemented");
    } break;
    case ARM::MOVi: { // 872 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVi not yet implemented");
    } break;
    case ARM::MOVi16: { // 873 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVi16 not yet implemented");
    } break;
    case ARM::MOVr: { // 874 | MOV<c> <Rd>, <Rm> => Rd = Rm
      assert(false && "ARM::MOVr not yet implemented");
    } break;
    case ARM::MOVsi: { // 876 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVsi not yet implemented");
    } break;
    case ARM::MUL: { // 888 | MUL<c> <Rd>, <Rn>, <Rm> => Rd = Rn * Rm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::CreateMul(Rn, Rm, "MUL", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::MVNi: { // 1736 | MVN<c> <Rd>, #<imm> => Rd = ~imm
      Register Rd = MI.getOperand(0).getReg();
      Value imm = ConstantInt::get(Rd.getType(), MI.getOperand(1).getImm());

      Instruction Instr = BinaryOperator::CreateNot(imm, "MVNi", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::MVNr: { // 1737 | MVN<c> <Rd>, <Rm> => Rd = ~Rm
      Register Rd = MI.getOperand(0).getReg();
      Value Rm = getRegValue(MI.getOperand(1).getReg());

      Instruction Instr = BinaryOperator::CreateNot(Rm, "MVNr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::ORRri: { // 1748 | ORR<c> <Rd>, <Rn>, #<imm> => Rd = Rn OR imm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());

      Instruction Instr = BinaryOperator::CreateOr(Rn, imm, "ORRri", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::ORRrr: { // 1749 | ORR<c> <Rd>, <Rn>, <Rm> => Rd = Rn OR Rm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::CreateOr(Rn, Rm, "ORRrr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::SBCri: { // 1794 | SBC<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm - C
      assert(false && "ARM::SBCri not yet implemented; requires Carry flag");
    } break;
    case ARM::SBCrr: { // 1795 | SBC<c> <Rd>, <Rn>, <Rm> => Rd = Rn - Rm - C
      assert(false && "ARM::SBCrr not yet implemented; requires Carry flag");
    } break;
    case ARM::SBCrsi: { // 1796 | SBC<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm - C
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::SMLAL: { // 1824 | SMLAL<c> <Rd>, <Rn>, <Rm>, <Ra> => Rd = Rn * Ra + Rm
      assert(false && "ARM::SMLAL not yet implemented");
    } break;
    case ARM::STMDB_UPD: { // 1895 | STMDB<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMDB_UPD not yet implemented");
    } break;
    case ARM::STMIA: { // 1896 | STMIA<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMIA not yet implemented");
    } break;
    case ARM::STMIA_UPD: { // 1897 | STMIA<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMIA_UPD not yet implemented");
    } break;
    case ARM::STMIB: { // 1898 | STMIB<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMIB not yet implemented");
    } break;
    case ARM::STRBi12: { // 1906 | STRB<c> <Rt>, [<Rn>, #-<imm>]!
      assert(false && "ARM::STRBi12 not yet implemented");
    } break;
    case ARM::STRBrs: { // 1907 | STRB<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRBrs not yet implemented");
    } break;
    case ARM::STRH: { // 1920 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRH not yet implemented");
    } break;
    case ARM::STRH: { // 1915 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRH not yet implemented");
    } break;
    case ARM::STR_PRE_IMM: { // 1920 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STR_PRE_IMM not yet implemented");
    } break;
    case ARM::STRi12: { // 1926 | STR<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRi12 not yet implemented");
    } break;
    case ARM::STRrs: { // 1927 | STR<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRrs not yet implemented");
    } break;
    case ARM::SUBri: { // 1928 | SUB<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());

      Instruction Instr = BinaryOperator::CreateSub(Rn, imm, "SUBri", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::SUBrr: { // 1929 | SUB<c> <Rd>, <Rn>, <Rm> => Rd = Rn - Rm
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::CreateSub(Rn, Rm, "SUBrr", BB);
      setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    } break;
    case ARM::SVC: { // 1932 | SVC<c> #<imm>
      assert(false && "ARM::SVC not yet implemented");
    } break;
  }
}

#undef DEBUG_TYPE
