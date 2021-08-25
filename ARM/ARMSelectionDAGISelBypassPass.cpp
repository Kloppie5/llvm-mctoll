
#include "ARMSelectionDAGISelBypassPass.h"

#include "FunctionRaisingInfo.h"
#include "Monitor.h"

#include "ARMSubtarget.h"
#include "llvm/IR/Instruction.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMSelectionDAGISelBypassPass::precondition(MachineFunction *MF, Function *F) {
  // Monitor::NOTE("ARMSelectionDAGISelBypassPass; precondition");
  auto OS = WithColor(errs(), HighlightColor::Warning);
  for (MachineBasicBlock &MBB : *MF) {
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
          return false;

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
        case ARM::ORRrr: // 1749
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
          // OS << "Allowed instruction: ";
          // Monitor::printMachineInstr(&MI, true, OS);
          break;
      }
    }
  }

  // Monitor::NOTE("ARMSelectionDAGISelBypassPass; precondition passed");
  return true;
}

bool ARMSelectionDAGISelBypassPass::run(MachineFunction *MF, Function *F) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
  
  if (!precondition(MF, F))
    return false;

  FuncInfo = new FunctionRaisingInfo();

  FuncInfo->set(AMR, *F, *MF, nullptr);

  LLVM_DEBUG(dbgs() << "ARMSelectionDAGISelBypassPass start.\n");

  // Add arguments to FuncInfo->RegValueMap
  
  if (F->arg_size() > 0) FuncInfo->RegValueMap[ARM::R0] = F->getArg(0);
  if (F->arg_size() > 1) FuncInfo->RegValueMap[ARM::R1] = F->getArg(1);
  if (F->arg_size() > 2) FuncInfo->RegValueMap[ARM::R2] = F->getArg(2);
  if (F->arg_size() > 3) FuncInfo->RegValueMap[ARM::R3] = F->getArg(3);
  if (F->arg_size() > 4) FuncInfo->FrameIndexValueMap[0] = F->getArg(4);
  if (F->arg_size() > 5) FuncInfo->FrameIndexValueMap[1] = F->getArg(5);
  if (F->arg_size() > 6) FuncInfo->FrameIndexValueMap[2] = F->getArg(6);
  if (F->arg_size() > 7) FuncInfo->FrameIndexValueMap[3] = F->getArg(7);
  if (F->arg_size() > 8) assert(false && "Fix arguments");

  { // Create NZCV store instructions
    BasicBlock *bb = &F->getEntryBlock();
    for (unsigned i = 0; i < 4; i++) {
      Align MALG(32);
      AllocaInst *Alloc = new AllocaInst(Type::getInt1Ty(F->getContext()), 0,
                                        nullptr, MALG, "", bb);
      FuncInfo->AllocaMap[i] = Alloc;
      new StoreInst(ConstantInt::getFalse(F->getContext()), Alloc, bb);
    }
  }

  for (MachineBasicBlock &MBB : *MF) {
    BasicBlock *BB = FuncInfo->getOrCreateBasicBlock(&MBB);
    for (MachineInstr &MI : MBB) {
      raiseMachineInstr(BB, &MI);
    }
    
    // If the current function has return value, records relationship between
    // BasicBlock and each Value *which is mapped with R0. In order to record
    // the return Value *of each exit BasicBlock.
    Type *RTy = FuncInfo->Fn->getReturnType();
    if (RTy != nullptr && !RTy->isVoidTy() && MBB.succ_size() == 0) {
      Instruction *TInst = dyn_cast<Instruction>(FuncInfo->RegValueMap[ARM::R0]);
      assert(TInst && "A def R0 was pointed to a non-instruction!!!");
      BasicBlock *TBB = TInst->getParent();
      FuncInfo->RetValMap[TBB] = TInst;
    }
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

Value *ARMSelectionDAGISelBypassPass::getOperandValue(MachineInstr *MI, int OpIdx, Type *Ty /* = nullptr */) {
  switch(MI->getOperand(OpIdx).getType()) {
    case MachineOperand::MO_Register:
      dbgs() << "Get register operand: " << MI->getOperand(OpIdx).getReg() << " | " << FuncInfo->RegValueMap[MI->getOperand(OpIdx).getReg()] << "\n";
      return FuncInfo->RegValueMap[MI->getOperand(OpIdx).getReg()];
    case MachineOperand::MO_FrameIndex:
      dbgs() << "Get frame index operand: " << MI->getOperand(OpIdx).getIndex() << " | " << FuncInfo->FrameIndexValueMap[MI->getOperand(OpIdx).getIndex()] << "\n";
      return FuncInfo->FrameIndexValueMap[MI->getOperand(OpIdx).getIndex()];
    case MachineOperand::MO_Immediate:
      dbgs() << "Get immediate operand: " << MI->getOperand(OpIdx).getImm() << "\n";
      return ConstantInt::get(Ty, MI->getOperand(OpIdx).getImm());
    default:
      llvm_unreachable("Unsupported operand type!");
  }
}
void ARMSelectionDAGISelBypassPass::setOperandValue(MachineInstr *MI, int OpIdx, Value *v) {
  switch(MI->getOperand(OpIdx).getType()) {
    case MachineOperand::MO_Register:
      FuncInfo->RegValueMap[MI->getOperand(OpIdx).getReg()] = v;
      dbgs() << "Set register value: " << MI->getOperand(OpIdx).getReg() << " to " << v << "\n";
      break;
    case MachineOperand::MO_FrameIndex:
      FuncInfo->FrameIndexValueMap[MI->getOperand(OpIdx).getIndex()] = v;
      dbgs() << "Set frame index value: " << MI->getOperand(OpIdx).getIndex() << " to " << v << "\n";
      break;
    default:
      llvm_unreachable("Unsupported operand type!");
  }
}

bool ARMSelectionDAGISelBypassPass::raiseMachineInstr(BasicBlock *BB, MachineInstr *MI) {
  Monitor::printMachineInstr(MI, true, dbgs());
  Instruction *Instr = nullptr;
  switch (MI->getOpcode()) {
    default: {
      auto OS = WithColor(errs(), HighlightColor::Warning);
      OS << "ARMSelectionDAGISelBypassPass encountered unhandled opcode in instruction ";
      Monitor::printMachineInstr(MI, true, OS);
      return false;
    } break;

    case ARM::ASRi: { // 247 | ASR{S}<c> <Rd>, <Rn>, #<imm> => Rd = AShr(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instr = BinaryOperator::Create(Instruction::AShr, Rn, imm, "ASRi", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ASRr: { // 248 | ASR{S}<c> <Rd>, <Rn>, <Rm> => Rd = AShr(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::AShr, Rn, Rm, "ASRr", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSLi: { // 292 | LSL{S}<c> <Rd>, <Rn>, #<imm> => Rd = Shl(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instr = BinaryOperator::Create(Instruction::Shl, Rn, imm, "LSLi", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSLr: { // 293 | LSL{S}<c> <Rd>, <Rn>, <Rm> => Rd = Shl(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::Shl, Rn, Rm, "LSLr", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSRi: { // 294 | LSR{S}<c> <Rd>, <Rn>, #<imm> => Rd = LShr(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instr = BinaryOperator::Create(Instruction::LShr, Rn, imm, "LSRi", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSRr: { // 295 | LSR{S}<c> <Rd>, <Rn>, <Rm> => Rd = LShr(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::LShr, Rn, Rm, "LSRr", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RORi: { // 325 | ROR{S}<c> <Rd>, <Rn>, #<imm> => Rd = OR(Shl(<Rn>, imm), LShr(<Rn>, Sub(32, imm)))
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Shl = BinaryOperator::Create(Instruction::Shl, Rn, imm, "RORiShl", BB);
      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Rn->getType(), 32), imm, "RORiSub", BB);
      Instruction *LShr = BinaryOperator::Create(Instruction::LShr, Rn, Sub, "RORiLShr", BB);
      Instr = BinaryOperator::Create(Instruction::Or, Shl, LShr, "RORiOR", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RORr: { // 326 | ROR{S}<c> <Rd>, <Rn>, <Rm> => Rd = OR(Shl(<Rn>, <Rm>), LShr(<Rn>, Sub(32, <Rm>)))
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Shl = BinaryOperator::Create(Instruction::Shl, Rn, Rm, "RORrShl", BB);
      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Rn->getType(), 32), Rm, "RORrSub", BB);
      Instruction *LShr = BinaryOperator::Create(Instruction::LShr, Rn, Sub, "RORrLShr", BB);
      Instr = BinaryOperator::Create(Instruction::Or, Shl, LShr, "RORrOR", BB);
      setOperandValue(MI, 0, Instr);
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
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instr = BinaryOperator::Create(Instruction::Add, Rn, imm, "ADDri", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ADDrr: { // 685 | ADD{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn + Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::Add, Rn, Rm, "ADDrr", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ADDrsi: { // 686 | ADD{S}<c> <Rd>, <Rn>, #<imm>
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::ANDri: { // 693 | AND{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & Imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instr = BinaryOperator::Create(Instruction::And, Rn, imm, "ANDri", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::BICri: { // 706 | BIC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & (0 - Imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      Value *zero = ConstantInt::get(Rn->getType(), 0);

      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, zero, imm, "BICriSub", BB);
      Instr = BinaryOperator::Create(Instruction::And, Rn, Sub, "BICriAnd", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::BL: { // 711 | BL #<imm> => call PC + Imm
      // Earlier pass changed this to BL #<imm> => call imm
      int64_t imm = MI->getOperand(0).getImm();

      Function *Func = MR.getRaisedFunctionAt(imm);

      dbgs() << "BL: " << Func->getName() << "\n";
        
      std::vector<Value *> ArgVals;
      for (unsigned i = 0; i < Func->arg_size(); ++i) {
        if (i == 0) ArgVals.push_back(FuncInfo->RegValueMap[ARM::R0]);
        else if (i == 1) ArgVals.push_back(FuncInfo->RegValueMap[ARM::R1]);
        else if (i == 2) ArgVals.push_back(FuncInfo->RegValueMap[ARM::R2]);
        else if (i == 3) ArgVals.push_back(FuncInfo->RegValueMap[ARM::R3]);
        else assert(false && "Too many arguments for register only function call; fixing later.");
      }

      Instr = CallInst::Create(Func, ArgVals, "", BB);
    } break;
    case ARM::BX_RET: { // 718 | BX_RET = BX LR = Return
      Instr = ReturnInst::Create(BB->getContext(), BB);
    } break;
    case ARM::Bcc: { // 720 | Bcc <cond> <label> => br <cond> label
      assert(false && "ARM::Bcc not yet implemented; requires PC-relative branch");
      // BasicBlock *Target = getBlockFromAddress(MI->getOperand(1).getImm());
      // Instr = BranchInst::Create(Target, BB);
      // Instr.setSuccessor(0, BB);
      // setValue(MI->getOperand(0).getReg(), BB->getNumber(), Instr);
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
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());

      Instr = BinaryOperator::Create(Instruction::Xor, Rn, imm, "EORri", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::EORrr: { // 776 | EOR{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn ^ Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::Xor, Rn, Rm, "EORrr", BB);
      setOperandValue(MI, 0, Instr);
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
      Type *Ty = Type::getInt8Ty(BB->getContext());
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Rn->getType());

      Value *Ptr = BinaryOperator::Create(Instruction::Add, Rn, imm, "LDRBi12Ptr", BB);
      Instr = new LoadInst(Ty, Ptr, "LDRBi12", BB);
      setOperandValue(MI, 0, Instr);
    } break;
      
    case ARM::LDRBrs: { // 832 | LDRB<c> <Rt>, [<Rn>, <Rm>] => Rt = *(Rn + Rm)
      assert(false && "Shifted instructions should have been removed in an earlier pass.");
    } break;
    case ARM::LDRH: { // 840 | LDRH<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      Type *Ty = Type::getInt16Ty(BB->getContext());
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Ty);

      Value *Ptr = BinaryOperator::Create(Instruction::Add, Rn, imm, "LDRHPtr", BB);
      Instr = new LoadInst(Ty, Ptr, "LDRH", BB);
      setOperandValue(MI, 0, Instr);
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
    case ARM::LDRi12: { // 862 | LDR<c> <Rt>, [#<imm>] => Rt = *(imm)
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Value *Ptr = getOperandValue(MI, 1, Ty);

      // Instr = new LoadInst(Ty, Ptr, "LDRi12", BB);
      setOperandValue(MI, 0, Ptr);
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
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Value *imm = getOperandValue(MI, 1, Ty);
      setOperandValue(MI, 0, imm);
    } break;
    case ARM::MOVi16: { // 873 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVi16 not yet implemented");
    } break;
    case ARM::MOVr: { // 874 | MOV<c> <Rd>, Operand2 => Rd = Operand2
      Value *Op2 = getOperandValue(MI, 1);
      
      setOperandValue(MI, 0, Op2);
    } break;
    case ARM::MOVsi: { // 876 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVsi not yet implemented");
    } break;
    case ARM::MUL: { // 888 | MUL<c> <Rd>, <Rn>, <Rm> => Rd = Rn * Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::Mul, Rn, Rm, "MUL", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::MVNi: { // 1736 | MVN<c> <Rd>, #<imm> => Rd = 0 - imm
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Value *imm = getOperandValue(MI, 1, Ty);
      Value *zero = ConstantInt::get(Ty, 0);

      Instr = BinaryOperator::Create(Instruction::Sub, zero, imm, "MVNi", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::MVNr: { // 1737 | MVN<c> <Rd>, <Rm> => Rd = 0 - Rm
      Value *Rm = getOperandValue(MI, 1);
      Value *zero = ConstantInt::get(Rm->getType(), 0);

      Instr = BinaryOperator::Create(Instruction::Sub, zero, Rm, "MVNr", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ORRri: { // 1748 | ORR<c> <Rd>, <Rn>, #<imm> => Rd = Rn OR imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Rn->getType());

      Instr = BinaryOperator::Create(Instruction::Or, Rn, imm, "ORRri", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ORRrr: { // 1749 | ORR<c> <Rd>, <Rn>, <Rm> => Rd = Rn OR Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::Or, Rn, Rm, "ORRrr", BB);
      setOperandValue(MI, 0, Instr);
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
    case ARM::STRH: { // 1915 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRH not yet implemented");
    } break;
    case ARM::STR_PRE_IMM: { // 1920 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STR_PRE_IMM not yet implemented");
    } break;
    case ARM::STRi12: { // 1926 | STR<c> <Rt>, [<Rn>, #<imm>]!
      Value *Rt = getOperandValue(MI, 0);
      // Value *Ptr = getOperandValue(MI, 1);

      // Instr = new StoreInst(Rt, Ptr, false, BB);
      setOperandValue(MI, 1, Rt);
    } break;
    case ARM::STRrs: { // 1927 | STR<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRrs not yet implemented");
    } break;
    case ARM::SUBri: { // 1928 | SUB<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Rn->getType());

      Instr = BinaryOperator::Create(Instruction::Sub, Rn, imm, "SUBri", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::SUBrr: { // 1929 | SUB<c> <Rd>, <Rn>, <Rm> => Rd = Rn - Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instr = BinaryOperator::Create(Instruction::Sub, Rn, Rm, "SUBrr", BB);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::SVC: { // 1932 | SVC<c> #<imm>
      assert(false && "ARM::SVC not yet implemented");
    } break;
  }
  Monitor::event_RaisedInstruction(MI, Instr);
  return true;
}

#undef DEBUG_TYPE
