
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
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::ASRr: { // 248 | ASR{S}<c> <Rd>, <Rn>, <Rm> => Rd = AShr(<Rn>, <Rm>)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::AShr, Rn, Rm, "ASRr", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::LSLi: { // 292 | LSL{S}<c> <Rd>, <Rn>, #<imm> => Rd = Shl(<Rn>, imm)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::Shl, Rn, imm, "LSLi", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::LSLr: { // 293 | LSL{S}<c> <Rd>, <Rn>, <Rm> => Rd = Shl(<Rn>, <Rm>)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::Shl, Rn, Rm, "LSLr", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::LSRi: { // 294 | LSR{S}<c> <Rd>, <Rn>, #<imm> => Rd = LShr(<Rn>, imm)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Instr = BinaryOperator::Create(BinaryOps::LShr, Rn, imm, "LSRi", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::LSRr: { // 295 | LSR{S}<c> <Rd>, <Rn>, <Rm> => Rd = LShr(<Rn>, <Rm>)
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Instr = BinaryOperator::Create(BinaryOps::LShr, Rn, Rm, "LSRr", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::RORi: { // 325 | ROR{S}<c> <Rd>, <Rn>, #<imm> => Rd = OR(Shl(<Rn>, imm), LShr(<Rn>, Sub(32, imm)))
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value imm = ConstantInt::get(Rn.getType(), MI.getOperand(2).getImm());
      
      Instruction Shl = BinaryOperator::Create(BinaryOps::Shl, Rn, imm, "RORiShl", BB);
      Instruction Sub = BinaryOperator::Create(BinaryOps::Sub, ConstantInt::get(Rn.getType(), 32), imm, "RORiSub", BB);
      Instruction LShr = BinaryOperator::Create(BinaryOps::LShr, Rn, Sub, "RORiLShr", BB);
      Instruction Instr = BinaryOperator::Create(BinaryOps::Or, Shl, LShr, "RORiOR", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::RORr: { // 326 | ROR{S}<c> <Rd>, <Rn>, <Rm> => Rd = OR(Shl(<Rn>, <Rm>), LShr(<Rn>, Sub(32, <Rm>)))
      Register Rd = MI.getOperand(0).getReg();
      Value Rn = getRegValue(MI.getOperand(1).getReg());
      Value Rm = getRegValue(MI.getOperand(2).getReg());

      Instruction Shl = BinaryOperator::Create(BinaryOps::Shl, Rn, Rm, "RORrShl", BB);
      Instruction Sub = BinaryOperator::Create(BinaryOps::Sub, ConstantInt::get(Rn.getType(), 32), Rm, "RORrSub", BB);
      Instruction LShr = BinaryOperator::Create(BinaryOps::LShr, Rn, Sub, "RORrLShr", BB);
      Instruction Instr = BinaryOperator::Create(BinaryOps::Or, Shl, LShr, "RORrOR", BB);
      Values->setRegValue(Rd, MI.getParent()->getNumber(), Instr);
    }
    case ARM::RRX: // 327 | RRX<c> <Rd>, <Rn> => Rd = ROR(Rn, 1)
      /*
      SDValue Rd = N->getOperand(0);
    SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
    SDNode *Node = nullptr;
    Node =
        CurDAG->getNode(ARMISD::RRX, dl, getDefaultEVT(), Rn, getMDOperand(N))
            .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
      */
    

    case ARM::ADCri: // 680
    case ARM::ADCrsi: // 682
      /*
      SDValue Rd = N->getOperand(0);
      SDValue Rn = N->getOperand(1);
      SDNode *Node = nullptr;
      if (isTwoAddressMode(Rd.getNode())) {
        // ADCS <Rdn>,<Rm>
        // ADC<c> <Rdn>,<Rm>
        if (RegisterSDNode::classof(N->getOperand(1).getNode()))
          Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

        SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
        Node =
            CurDAG
                ->getNode(ISD::ADDC, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
                .getNode();
      } else {
        // ADC{S}<c> <Rd>,<Rn>,#<const>
        SDValue op2 = N->getOperand(2);
        if (RegisterSDNode::classof(op2.getNode()))
          op2 = FuncInfo->getValFromRegMap(op2);
        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
        Node = CurDAG
                  ->getNode(ISD::ADDC, dl, getDefaultEVT(), Rn, op2,
                            getMDOperand(N))
                  .getNode();
      }

      recordDefinition(Rd.getNode(), Node);
      replaceNode(N, Node);
      */
    
    case ARM::ADDri: // 684
    case ARM::ADDrr: // 685
    case ARM::ADDrsi: // 686
      /*
      SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    // <opcode>   {<cond>}{s}<Rd>，<Rn>{，<OP2>}
    SDNode *Node = nullptr;
    if (FrameIndexSDNode::classof(N->getOperand(1).getNode())) {
      Node = CurDAG
                 ->getNode(EXT_ARMISD::LOAD, dl, getDefaultEVT(), Rn,
                           getMDOperand(N))
                 .getNode();
    } else {
      if (isTwoAddressMode(Rd.getNode())) {
        if (RegisterSDNode::classof(N->getOperand(1).getNode()))
          Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

        SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
        Node = CurDAG
                   ->getNode(ISD::ADD, dl, getDefaultEVT(), Rd, Rn,
                             getMDOperand(N))
                   .getNode();
      } else {
        SDValue op2 = N->getOperand(2);
        if (RegisterSDNode::classof(op2.getNode()))
          op2 = FuncInfo->getValFromRegMap(op2);

        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
        Node = CurDAG
                   ->getNode(ISD::ADD, dl, getDefaultEVT(), Rn, op2,
                             getMDOperand(N))
                   .getNode();
      }
    }

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
      */

    case ARM::ANDri: // 693
    /*
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;

    if (isTwoAddressMode(Rd.getNode())) {
      // AND<c> <Rdn>,<Rm>
      // ANDS <Rdn>,<Rm>
      if (RegisterSDNode::classof(N->getOperand(1).getNode()))
        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
      SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
      Node =
          CurDAG
              ->getNode(ISD::AND, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
              .getNode();
    } else {
      // AND{S}<c> <Rd>,<Rn>,#<const>
      SDValue op2 = N->getOperand(2);
      if (RegisterSDNode::classof(op2.getNode()))
        op2 = FuncInfo->getValFromRegMap(op2);

      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
      Node =
          CurDAG
              ->getNode(ISD::AND, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
              .getNode();
    }

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
    // TODO:
    // AND{S}<c>.W <Rd>,<Rn>,<Rm>{,<shift>}
    // AND{S}<c> <Rd>,<Rn>,<Rm>{,<shift>}
    // AND{S}<c> <Rd>,<Rn>,<Rm>,<type> <Rs>
    */
    case ARM::BICri: // 706
/*
SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    if (isTwoAddressMode(Rd.getNode())) {
      if (RegisterSDNode::classof(N->getOperand(1).getNode()))
        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

      SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
      Node = CurDAG
                 ->getNode(EXT_ARMISD::BIC, dl, getDefaultEVT(), Rd, Rn,
                           getMDOperand(N))
                 .getNode();
    } else {
      SDValue op2 = N->getOperand(2);
      if (RegisterSDNode::classof(op2.getNode()))
        op2 = FuncInfo->getValFromRegMap(op2);

      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
      Node = CurDAG
                 ->getNode(EXT_ARMISD::BIC, dl, getDefaultEVT(), Rn, op2,
                           getMDOperand(N))
                 .getNode();
    }

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
*/
    case ARM::BL: // 711
    /*
    SDValue Func = N->getOperand(0);
    SDNode *Node = nullptr;
    if (RegisterSDNode::classof(Func.getNode())) {
      Func = FuncInfo->getValFromRegMap(Func);
      Node =
          CurDAG
              ->getNode(ISD::BRIND, dl, getDefaultEVT(), Func, getMDOperand(N))
              .getNode();
    } else {
      Node = CurDAG
                 ->getNode(EXT_ARMISD::BRD, dl, getDefaultEVT(), Func,
                           getMDOperand(N))
                 .getNode();
    }

    FuncInfo->setValueByRegister(ARM::R0, SDValue(Node, 0));
    FuncInfo->NodeRegMap[Node] = ARM::R0;
    replaceNode(N, Node);
    */
    case ARM::BX_RET: // 718
    // assert(0 && "Branch instructions are removed in previous stage. should
    // not get here!");
    case ARM::Bcc: // 720
/*
SDValue Iftrue = N->getOperand(0);
    SDValue Cond = N->getOperand(1);
    SDNode *Node = nullptr;

    if (DAGInfo->NPMap[N]->HasCPSR)
      Node = CurDAG
                 ->getNode(ISD::BRCOND, dl, getDefaultEVT(), Iftrue, Cond,
                           getMDOperand(N))
                 .getNode();
    else
      Node =
          CurDAG->getNode(ISD::BR, dl, getDefaultEVT(), Iftrue, getMDOperand(N))
              .getNode();

    const MachineBasicBlock *LMBB = DAGInfo->NPMap[N]->MI->getParent();
    if (LMBB->succ_size() == 0) {
      FuncInfo->setValueByRegister(ARM::R0, SDValue(Node, 0));
      FuncInfo->NodeRegMap[Node] = ARM::R0;
    }
    replaceNode(N, Node);
*/
    case ARM::CMNri: // 755
/*
 SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;

    if (RegisterSDNode::classof(N->getOperand(1).getNode()))
      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
    Rd = FuncInfo->getValFromRegMap(Rd);
    Node =
        CurDAG
            ->getNode(ARMISD::CMN, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
            .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
*/
    case ARM::CMPri: // 759
    case ARM::CMPrr: // 760
    /*
    SDValue cmpl = N->getOperand(0);
    SDValue cmph = N->getOperand(1);
    if (RegisterSDNode::classof(cmph.getNode()))
      cmph = FuncInfo->getValFromRegMap(N->getOperand(1));
    cmpl = FuncInfo->getValFromRegMap(cmpl);

    // Create condition SDValuleR
    // TODO: It should be verified why this type node can not be added Metadata
    // Operand.
    SDNode *Node = CurDAG
                       ->getNode(ISD::SETCC, dl, getDefaultEVT(), cmpl, cmph
                                 /* , getMDOperand(N) )
                       .getNode();

    replaceNode(N, Node);
    */
    case ARM::DMB: // 773
    case ARM::DSB: // 774

    case ARM::EORri: // 775
    case ARM::EORrr: // 776
      /*
      SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    if (isTwoAddressMode(Rd.getNode())) {
      // EORS <Rdn>,<Rm>
      // EOR<c> <Rdn>,<Rm>
      if (RegisterSDNode::classof(N->getOperand(1).getNode()))
        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

      SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
      Node =
          CurDAG
              ->getNode(ISD::XOR, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
              .getNode();
    } else {
      // EOR{S}<c> <Rd>,<Rn>,#<const>
      SDValue op2 = N->getOperand(2);
      if (RegisterSDNode::classof(op2.getNode()))
        op2 = FuncInfo->getValFromRegMap(op2);
      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
      Node =
          CurDAG
              ->getNode(ISD::XOR, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
              .getNode();
    }
    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
    // TODO:
    // EOR{S}<c>.W <Rd>,<Rn>,<Rm>{,<shift>}
    // EOR{S}<c> <Rd>,<Rn>,<Rm>{,<shift>}
    // EOR{S}<c> <Rd>,<Rn>,<Rm>,<type> <Rs>
      */
    case ARM::ISB: // 793
    case ARM::LDMIA: // 821
    case ARM::LDMIA_UPD: // 822
    
    case ARM::LDRBi12: // 831
    case ARM::LDRBrs: // 832
    /*
    EVT InstTy = EVT::getEVT(Type::getInt8Ty(*CurDAG->getContext()));
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;

    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);
    Node = CurDAG->getNode(EXT_ARMISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
    */
    case ARM::LDRH: // 840
      /*
      EVT InstTy = EVT::getEVT(Type::getInt16Ty(*CurDAG->getContext()));
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;

    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);
    Node = CurDAG->getNode(EXT_ARMISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
      */
    case ARM::LDRSB: // 845
    case ARM::LDRSB_PRE: // 849
/*
EVT InstTy = EVT::getEVT(Type::getInt8Ty(*CurDAG->getContext()));
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;

    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);
    Node = CurDAG->getNode(EXT_ARMISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
*/
    case ARM::LDR_PRE_IMM: // 859
    case ARM::LDR_PRE_REG: // 860
    case ARM::LDRi12: // 862
    case ARM::LDRrs: // 863
      /*
      EVT InstTy = EVT::getEVT(Type::getInt32Ty(*CurDAG->getContext()));
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);

    Node = CurDAG->getNode(EXT_ARMISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
      */
    case ARM::MLA: // 868
    /*
    SDValue Rd = N->getOperand(0);
    SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
    SDValue Rm = FuncInfo->getValFromRegMap(N->getOperand(2));
    SDValue Ra = FuncInfo->getValFromRegMap(N->getOperand(3));
    SDNode *Node = nullptr;
    Node = CurDAG
               ->getNode(EXT_ARMISD::MLA, dl, getDefaultEVT(), Rn, Rm, Ra,
                         getMDOperand(N))
               .getNode();
    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
    */
    case ARM::MOVPCLR: // 870

    case ARM::MOVTi16: // 871
    case ARM::MOVi: // 872
    case ARM::MOVi16: // 873
    case ARM::MOVr: // 874
    case ARM::MOVsi: // 876
      /*
      // Dispalcement operation need do.
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);

    SDNode *Node = CurDAG
                       ->getNode(ARMISD::CMOV, dl, getDefaultEVT(), Rn,
                                 CurDAG->getConstant(0, dl, getDefaultEVT()))
                       .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
      */
    
    case ARM::MUL: // 888
    /*
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    SDValue op2 = N->getOperand(2);
    op2 = FuncInfo->getValFromRegMap(op2);
    Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
    Node =
        CurDAG->getNode(ISD::MUL, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
            .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
    */
    case ARM::MVNi: // 1736
    case ARM::MVNr: // 1737
    /*
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    if (RegisterSDNode::classof(N->getOperand(1).getNode()))
      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

    Node = CurDAG
               ->getNode(ISD::XOR, dl, getDefaultEVT(), Rn,
                         CurDAG->getConstant(-1, dl, getDefaultEVT()))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
    */
    case ARM::ORRri: // 1748
    case ARM::ORRrr:// 1749
    /*
        SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    // <opcode>   {<cond>}{s}<Rd>，<Rn>{，<OP2>}
    SDNode *Node = nullptr;
    if (isTwoAddressMode(Rd.getNode())) {
      if (RegisterSDNode::classof(N->getOperand(1).getNode()))
        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

      SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
      Node =
          CurDAG->getNode(ISD::OR, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
              .getNode();
    } else {
      SDValue op2 = N->getOperand(2);
      if (RegisterSDNode::classof(op2.getNode()))
        op2 = FuncInfo->getValFromRegMap(op2);

      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
      Node =
          CurDAG
              ->getNode(ISD::OR, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
              .getNode();
    }
    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
        */
    case ARM::SBCri: // 1794
    case ARM::SBCrr: // 1795
    /*
    SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
    SDValue Operand2 = FuncInfo->getValFromRegMap(N->getOperand(2));
    SDNode *Node = CurDAG
                       ->getNode(EXT_ARMISD::SBC, dl, getDefaultEVT(), Rn,
                                 Operand2, getMDOperand(N))
                       .getNode();

    recordDefinition(Rn.getNode(), Node);
    replaceNode(N, Node);
    */
    case ARM::SBCrsi: // 1796
    // missing
    case ARM::SMLAL: // 1824
    case ARM::STMDB_UPD: // 1895
    case ARM::STMIA: // 1896
    case ARM::STMIA_UPD: // 1897
    case ARM::STMIB: // 1898
    // missing
    case ARM::STRBi12: // 1906
    case ARM::STRBrs: // 1907
    // missing
    case ARM::STRH: // 1920
      /*
      EVT InstTy = EVT::getEVT(Type::getInt8Ty(*CurDAG->getContext()));
    SDValue Val = N->getOperand(0);
    SDValue Op1 = N->getOperand(1);
    SDNode *Node = nullptr;

    if (RegisterSDNode::classof(Val.getNode()))
      Val = FuncInfo->getValFromRegMap(Val);

    if (RegisterSDNode::classof(Op1.getNode()))
      Op1 = FuncInfo->getValFromRegMap(Op1);

    if (N->getNumOperands() < 5)
      Node = CurDAG
                 ->getNode(EXT_ARMISD::STORE, dl, InstTy, Val, Op1,
                           getMDOperand(N))
                 .getNode();
    else {
      SDValue Op2 = N->getOperand(2);
      Op2 = FuncInfo->getValFromRegMap(Op2);
      Node = CurDAG
                 ->getNode(EXT_ARMISD::STORE, dl, InstTy, Val, Op1, Op2,
                           getMDOperand(N))
                 .getNode();
    }

    replaceNode(N, Node);
      */

    case ARM::STRH: // 1915
    case ARM::STR_PRE_IMM: // 1920
      /*
      EVT InstTy = EVT::getEVT(Type::getInt16Ty(*CurDAG->getContext()));
    SDValue Val = N->getOperand(0);
    SDValue Op1 = N->getOperand(1);
    SDNode *Node = nullptr;

    if (RegisterSDNode::classof(Val.getNode()))
      Val = FuncInfo->getValFromRegMap(Val);

    if (RegisterSDNode::classof(Op1.getNode()))
      Op1 = FuncInfo->getValFromRegMap(Op1);

    if (N->getNumOperands() < 5)
      Node = CurDAG
                 ->getNode(EXT_ARMISD::STORE, dl, InstTy, Val, Op1,
                           getMDOperand(N))
                 .getNode();
    else {
      SDValue Op2 = N->getOperand(2);
      Op2 = FuncInfo->getValFromRegMap(Op2);
      Node = CurDAG
                 ->getNode(EXT_ARMISD::STORE, dl, InstTy, Val, Op1, Op2,
                           getMDOperand(N))
                 .getNode();
    }

    replaceNode(N, Node);
      */


    case ARM::STRi12: // 1926
    case ARM::STRrs: // 1927
      /*
 SDValue Val = N->getOperand(0);
    SDValue Ptr = N->getOperand(1); // This is a pointer.

    if (RegisterSDNode::classof(Val.getNode()))
      Val = FuncInfo->getValFromRegMap(Val);

    if (RegisterSDNode::classof(Ptr.getNode()))
      Ptr = FuncInfo->getValFromRegMap(Ptr);

    SDNode *Node = CurDAG
                       ->getNode(EXT_ARMISD::STORE, dl, getDefaultEVT(), Val,
                                 Ptr, getMDOperand(N))
                       .getNode();
    replaceNode(N, Node);
      */
    case ARM::SUBri: // 1928
    case ARM::SUBrr: // 1929
      /*
      SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    if (isTwoAddressMode(Rd.getNode())) {
      if (RegisterSDNode::classof(N->getOperand(1).getNode()))
        Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

      SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
      Node =
          CurDAG
              ->getNode(ISD::SUB, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
              .getNode();
    } else {
      SDValue op2 = N->getOperand(2);
      if (RegisterSDNode::classof(op2.getNode()))
        op2 = FuncInfo->getValFromRegMap(op2);

      Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
      Node =
          CurDAG
              ->getNode(ISD::SUB, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
              .getNode();
    }
    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
      */


    case ARM::SVC: // 1932
      OS << "Allowed instruction: ";
      Monitor::printMachineInstr(&MI, true, OS);
      break;
  }
}

#undef DEBUG_TYPE
