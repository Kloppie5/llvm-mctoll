
#include "ARMSelectionDAGISelBypassPass.h"

#include "FunctionRaisingInfo.h"
#include "Monitor.h"

#include "ARMModuleRaiser.h"
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
        case ARM::B: // 249
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
  Monitor::event_start("ARMSelectionDAGISelBypassPass");
  LLVM_DEBUG(dbgs() << "ARMSelectionDAGISelByPassPass start.\n");

  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
  
  if (!precondition(MF, F))
    return false;

  FuncInfo = new FunctionRaisingInfo();

  FuncInfo->set(AMR, *F, *MF, nullptr);

  // Add arguments to FuncInfo->RegValueMap
  if (F->arg_size() > 0) FuncInfo->RegValueMap[ARM::R0] = F->getArg(0);
  if (F->arg_size() > 1) FuncInfo->RegValueMap[ARM::R1] = F->getArg(1);
  if (F->arg_size() > 2) FuncInfo->RegValueMap[ARM::R2] = F->getArg(2);
  if (F->arg_size() > 3) FuncInfo->RegValueMap[ARM::R3] = F->getArg(3);

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
      raiseMachineInstr(&MBB, &MI, BB);
    }
  }

  FuncInfo->clear();

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMSelectionDAGIselBypassPass end.\n");
  Monitor::event_end("ARMSelectionDAGISelBypassPass");
  return true;
}

// TODO: make const
Value *ARMSelectionDAGISelBypassPass::getOperandValue(MachineInstr *MI, int OpIdx, Type *Ty /* = nullptr */) {
  switch(MI->getOperand(OpIdx).getType()) {
    case MachineOperand::MO_Register: {
      Value *v = FuncInfo->RegValueMap[MI->getOperand(OpIdx).getReg()];
      if (!v)
        Monitor::ERROR("Operand register has no assigned value.");
      return v;
    }
    case MachineOperand::MO_FrameIndex: {
      int frameindex = MI->getOperand(OpIdx).getIndex();
      Value *v = FuncInfo->FrameIndexValueMap[frameindex];
      if (v)
        return v;
      
      if (frameindex < (int) FuncInfo->Fn->arg_size()-4) { // ArgIndex
        v = FuncInfo->Fn->getArg(frameindex);
        FuncInfo->FrameIndexValueMap[frameindex] = v;
        return v;
      } else { // StackIndex
        const MachineFrameInfo &MFI = MI->getMF()->getFrameInfo();
        v = const_cast<AllocaInst *>(MFI.getObjectAllocation(frameindex)); // TODO: propagate const qualifier instead of const_cast
        FuncInfo->FrameIndexValueMap[frameindex] = v;
        return v;
      }
    }
    case MachineOperand::MO_Immediate: {
      Value *v = ConstantInt::get(Ty, MI->getOperand(OpIdx).getImm());
      return v;
    }
    case MachineOperand::MO_GlobalAddress: {
      const GlobalValue *GV = MI->getOperand(OpIdx).getGlobal();
      /* Something akin to:
      if (GV->hasLocalLinkage()) {
        Value *v = FuncInfo->getOrCreateLocalValue(GV);
        return v;
      } else {
        Value *v = FuncInfo->getOrCreateGlobalValue(GV);
        return v;
      }
      */
     return const_cast<GlobalValue *>(GV); // NOTE: yes, const_cast const removal is unsafe and not okay
    } break;
    default:
      llvm_unreachable("Unsupported operand type!");
  }
}
void ARMSelectionDAGISelBypassPass::setOperandValue(MachineInstr *MI, int OpIdx, Value *v) {
  switch(MI->getOperand(OpIdx).getType()) {
    case MachineOperand::MO_Register:
      FuncInfo->RegValueMap[MI->getOperand(OpIdx).getReg()] = v;
      break;
    case MachineOperand::MO_FrameIndex:
      FuncInfo->FrameIndexValueMap[MI->getOperand(OpIdx).getIndex()] = v;
      break;
    default:
      llvm_unreachable("Unsupported operand type!");
  }
}

// TODO: Move CC to separate pass.
Value *ARMSelectionDAGISelBypassPass::ARMCCToValue(int Cond, BasicBlock *BB) {
  LLVMContext &Ctx = BB->getContext();
  // Why do ICmpInst constructors put &InsertBefore/&InsertAtEnd as the first
  // operand instead of *InsertBefore/*InsertAtEnd as the last one? Who knows.
  switch (Cond) {
    default:
      llvm_unreachable("Unknown condition code!");
    case ARMCC::EQ: { // Z = 1
      Instruction *Z = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Ctx), "ZS"); Monitor::event_Instruction(ZS);
      return ZS;
    } break;
    case ARMCC::NE: { // Z = 0
      Instruction *Z = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Ctx), "ZC"); Monitor::event_Instruction(ZC);
      return ZC;
    } break;
    case ARMCC::HS: { // C = 1
      Instruction *C = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Ctx), "CS"); Monitor::event_Instruction(CS);
      return CS;
    } break;
    case ARMCC::LO: { // C = 0
      Instruction *C = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Ctx), "CC"); Monitor::event_Instruction(CC);
      return CC;
    } break;
    case ARMCC::MI: { // N = 1
      Instruction *N = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *NS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getTrue(Ctx), "NS"); Monitor::event_Instruction(NS);
      return NS;
    } break;
    case ARMCC::PL: { // N = 0
      Instruction *N = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *NC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getFalse(Ctx), "NC"); Monitor::event_Instruction(NC);
      return NC;
    } break;
    case ARMCC::VS: { // V = 1
      Instruction *V = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *VS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getTrue(Ctx), "VS"); Monitor::event_Instruction(VS);
      return VS;
    } break;
    case ARMCC::VC: { // V = 0
      Instruction *V = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *VC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getFalse(Ctx), "VC"); Monitor::event_Instruction(VC);
      return VC;
    } break;
    case ARMCC::HI: { // C = 1 && Z = 0
      Instruction *C = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Ctx), "CS"); Monitor::event_Instruction(CS);
      Instruction *Z = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Ctx), "ZC"); Monitor::event_Instruction(ZC);
      Instruction *HI = BinaryOperator::Create(Instruction::Add, CS, ZC, "HI", BB); Monitor::event_Instruction(HI);
      return HI;
    } break;
    case ARMCC::LS: { // C = 0 || Z = 1
      Instruction *C = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Ctx), "CC"); Monitor::event_Instruction(CC);
      Instruction *Z = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Ctx), "ZS"); Monitor::event_Instruction(ZS);
      Instruction *LS = BinaryOperator::Create(Instruction::Or, CC, ZS, "LS", BB); Monitor::event_Instruction(LS);
      return LS;
    } break;
    case ARMCC::GE: { // N = V
      Instruction *N = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *GE = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "GE"); Monitor::event_Instruction(GE);
      return GE;
    } break;
    case ARMCC::LT: { // N != V
      Instruction *N = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *LT = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "LT"); Monitor::event_Instruction(LT);
      return LT;
    } break;
    case ARMCC::GT: { // Z = 0 && N = V
      Instruction *Z = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Ctx), "ZC"); Monitor::event_Instruction(ZC);
      Instruction *N = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *Sign = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "S"); Monitor::event_Instruction(Sign);
      Instruction *GT = BinaryOperator::Create(Instruction::And, ZC, Sign, "GT", BB); Monitor::event_Instruction(GT);
      return GT;
    } break;
    case ARMCC::LE: { // Z = 1 || N != V
      Instruction *Z = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Ctx), "ZS"); Monitor::event_Instruction(ZS);
      Instruction *N = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Ctx), FuncInfo->AllocaMap[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *Sign = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "S"); Monitor::event_Instruction(Sign);
      Instruction *LE = BinaryOperator::Create(Instruction::Or, ZS, Sign, "LE", BB); Monitor::event_Instruction(LE);
      return LE;
    } break;
    case ARMCC::AL: { // always
      return ConstantInt::getTrue(Ctx);
    } break;
  }
}

bool ARMSelectionDAGISelBypassPass::raiseMachineInstr(MachineBasicBlock *MBB, MachineInstr *MI, BasicBlock *BB) {
  Monitor::event_start("ARMSelectionDAGISelBypassPass::RaiseMachineInstr");
  Monitor::event_MachineInstr(MI);
  Monitor::event_stateswitch();

  // TODO: Fix and move to instruction splitting
  int idx = MI->findFirstPredOperandIdx();
  if (idx != -1) {
    ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(idx).getImm();
    // Register CPSR = MI->getOperand(idx+1).getReg();
    if (CC != ARMCC::AL) {
      if (MI->getOpcode() != ARM::Bcc && MI->getOpcode() != ARM::MOVi) {
      Monitor::ERROR("Condition code not supported");
      //Instruction *Cond = getCond(CC, CPSR, BB, Instrs);
      }
    }
  }

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
      
      Instruction *Instr = BinaryOperator::Create(Instruction::AShr, Rn, imm, "ASRi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ASRr: { // 248 | ASR{S}<c> <Rd>, <Rn>, <Rm> => Rd = AShr(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::AShr, Rn, Rm, "ASRr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::B: { // 249 | B <MBB> => Branch(MBB)
      MachineBasicBlock *TargetMBB = MI->getOperand(0).getMBB();
      BasicBlock *TargetBB = FuncInfo->getOrCreateBasicBlock(TargetMBB);
      Instruction *Br = BranchInst::Create(TargetBB, BB); Monitor::event_Instruction(Br);
    } break;
    case ARM::LSLi: { // 292 | LSL{S}<c> <Rd>, <Rn>, #<imm> => Rd = Shl(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::Shl, Rn, imm, "LSLi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSLr: { // 293 | LSL{S}<c> <Rd>, <Rn>, <Rm> => Rd = Shl(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Shl, Rn, Rm, "LSLr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSRi: { // 294 | LSR{S}<c> <Rd>, <Rn>, #<imm> => Rd = LShr(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::LShr, Rn, imm, "LSRi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSRr: { // 295 | LSR{S}<c> <Rd>, <Rn>, <Rm> => Rd = LShr(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::LShr, Rn, Rm, "LSRr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RORi: { // 325 | ROR{S}<c> <Rd>, <Rn>, #<imm> => Rd = OR(Shl(<Rn>, imm), LShr(<Rn>, Sub(32, imm)))
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Shl = BinaryOperator::Create(Instruction::Shl, Rn, imm, "RORiShl", BB); Monitor::event_Instruction(Shl);
      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Rn->getType(), 32), imm, "RORiSub", BB); Monitor::event_Instruction(Sub);
      Instruction *LShr = BinaryOperator::Create(Instruction::LShr, Rn, Sub, "RORiLShr", BB); Monitor::event_Instruction(LShr);
      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Shl, LShr, "RORiOR", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RORr: { // 326 | ROR{S}<c> <Rd>, <Rn>, <Rm> => Rd = OR(Shl(<Rn>, <Rm>), LShr(<Rn>, Sub(32, <Rm>)))
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Shl = BinaryOperator::Create(Instruction::Shl, Rn, Rm, "RORrShl", BB); Monitor::event_Instruction(Shl);
      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Rn->getType(), 32), Rm, "RORrSub", BB); Monitor::event_Instruction(Sub);
      Instruction *LShr = BinaryOperator::Create(Instruction::LShr, Rn, Sub, "RORrLShr", BB); Monitor::event_Instruction(LShr);
      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Shl, LShr, "RORrOR", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RRX: { // 327 | RRX{S}<c> <Rd>, <Rm> => ?
      // Rotate Right with Extend
      Value *Rm = getOperandValue(MI, 1);
      Instruction *Shr = BinaryOperator::Create(Instruction::LShr, Rm, ConstantInt::get(Rm->getType(), 1), "RRXShr", BB); Monitor::event_Instruction(Shr);
      Instruction *C = new LoadInst(Type::getInt1Ty(BB->getContext()), FuncInfo->AllocaMap[1], "Carry", BB); Monitor::event_Instruction(C);
      Instruction *ShlC = BinaryOperator::Create(Instruction::Shl, Shr, ConstantInt::get(Shr->getType(), 31), "RRXCShl", BB); Monitor::event_Instruction(ShlC);
      Instruction *Or = BinaryOperator::Create(Instruction::Or, Shr, ShlC, "RRXOr", BB); Monitor::event_Instruction(Or);
      setOperandValue(MI, 0, Or);
    } break;
    case ARM::ADCri: { // 680 | ADC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn + Imm
      assert(false && "ADCri not yet implemented; requires Carry flag");
    } break;
    case ARM::ADDri: { // 684 | ADD{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn + Imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::Add, Rn, imm, "ADDri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ADDrr: { // 685 | ADD{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn + Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Add, Rn, Rm, "ADDrr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ANDri: { // 693 | AND{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & Imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::And, Rn, imm, "ANDri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::BICri: { // 706 | BIC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & (0 - Imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      Value *zero = ConstantInt::get(Rn->getType(), 0);

      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, zero, imm, "BICriSub", BB); Monitor::event_Instruction(Sub);
      Instruction *Instr = BinaryOperator::Create(Instruction::And, Rn, Sub, "BICriAnd", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::BL: { // 711 | BL #<imm> => call PC + Imm
      // Earlier pass changed this to BL #<imm> => call imm
      ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);

      Monitor::event_raw() << "Creating call\n";
      uint64_t imm = (uint64_t) MI->getOperand(0).getImm();
      Function *CallFunc = MR.getRaisedFunctionAt(imm);
      if (CallFunc == nullptr)
        CallFunc = AMR.getSyscallFunc(imm);
      assert(CallFunc && "No function found for call");

      Monitor::event_raw() << "Call Function: " << CallFunc->getName() << "/" << CallFunc->arg_size() << "(\n";

      std::vector<Value *> ArgVals;
      for (unsigned i = 0; i < CallFunc->arg_size(); ++i) {
        Value *ArgVal = nullptr;
        Type *Ty = CallFunc->getFunctionType()->getParamType(i);
        if (i == 0) ArgVal = FuncInfo->RegValueMap[ARM::R0];
        else if (i == 1) ArgVal = FuncInfo->RegValueMap[ARM::R1];
        else if (i == 2) ArgVal = FuncInfo->RegValueMap[ARM::R2];
        else if (i == 3) ArgVal = FuncInfo->RegValueMap[ARM::R3];
        else assert(false && "Too many arguments for register only function call; fixing later.");
        if (ArgVal->getType() != Ty) {
          raw_ostream &OS = Monitor::event_raw();
          OS << "Arg " << i << ": "; ArgVal->getType()->print(OS); OS << " => "; Ty->print(OS); OS << "\n";
          // Handle special string value to pointer case, should obviously be moved
          Instruction *Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB); Monitor::event_Instruction(Cast);
          ArgVal = Cast;
        }
        ArgVals.push_back(ArgVal);
      }

      Instruction *Instr = CallInst::Create(CallFunc, ArgVals, "", BB); Monitor::event_Instruction(Instr);
      if (CallFunc->getReturnType()->isVoidTy()) {
        // FuncInfo->RegValueMap[ARM::R0] = ConstantInt::get(Type::getInt32Ty(BB->getContext()), 0);
      } else {
        FuncInfo->RegValueMap[ARM::R0] = Instr;
      }
    } break;
    case ARM::BX_RET: { // 718 | BX_RET = BX LR = Return
      Type *RetTy = FuncInfo->Fn->getReturnType();

      if (RetTy->isVoidTy()) {
        Instruction *Instr = ReturnInst::Create(BB->getContext(), BB); Monitor::event_Instruction(Instr);
      } else if (RetTy->isIntegerTy()) {
        Instruction *Instr = ReturnInst::Create(BB->getContext(), FuncInfo->RegValueMap[ARM::R0], BB); Monitor::event_Instruction(Instr);
      } else
        assert(false && "Unsupported return type");
    } break;
    case ARM::Bcc: { // 720 | Bcc <label> <cond?> => Branch
      ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);

      if (MI->getOperand(0).isMBB()) {
        MachineBasicBlock *TargetMBB = MI->getOperand(0).getMBB();
        MachineInstr *NextMI = &*(++MI->getIterator());
        if (NextMI->getOpcode() == ARM::B) {
          MachineBasicBlock *NextTargetMBB = NextMI->getOperand(0).getMBB();
          BasicBlock *NextTargetBB = FuncInfo->getOrCreateBasicBlock(NextTargetMBB);
          BasicBlock *TargetBB = FuncInfo->getOrCreateBasicBlock(TargetMBB);
          Value *Cond = ARMCCToValue(MI->getOperand(1).getImm(), BB);
          Instruction *Branch = BranchInst::Create(TargetBB, NextTargetBB, Cond, BB); Monitor::event_Instruction(Branch);
          break;
        }
        assert(false && "MBB based Bcc only emitted before MBB based B;\nEarlier passes should handle all Imm based branches before fixing this");
      }
        


      // Imm based
      MachineBasicBlock::succ_iterator BranchMBBitt = MBB->succ_begin();

      if (BranchMBBitt != MBB->succ_end()) {
        Monitor::event_raw() << "Using MBB (" << MBB->succ_size() << ") successors\n";
        BasicBlock *BranchBB = FuncInfo->getOrCreateBasicBlock(*BranchMBBitt++);
        // The offset is ignored since it was used in an earlier part of the raising process
        // to determine the successor basic blocks.
        Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";

        if (MI->getOperand(2).getReg() == 0) {
          Monitor::event_raw() << "Unconditional\n";
          // Unconditional branch
          Instruction *Branch = BranchInst::Create(BranchBB, BB); Monitor::event_Instruction(Branch);
        } else {
          Monitor::event_raw() << "Conditional\n";
          // Conditional branch
          BasicBlock *NextBB = FuncInfo->getOrCreateBasicBlock(*BranchMBBitt++);
          Monitor::event_raw() << "Next BB: " << BranchBB->getName() << "\n";
          Value *Cond = ARMCCToValue(MI->getOperand(1).getImm(), BB);
          Instruction *Branch = BranchInst::Create(BranchBB, NextBB, Cond, BB); Monitor::event_Instruction(Branch);
        }
        break;
      }

      Monitor::event_raw() << "Creating call\n";
      uint64_t imm = (uint64_t) MI->getOperand(0).getImm();
      Function *CallFunc = MR.getRaisedFunctionAt(imm);
      if (CallFunc == nullptr)
        CallFunc = AMR.getSyscallFunc(imm);
      assert(CallFunc && "No function found for call");

      Monitor::event_raw() << "Call Function: " << CallFunc->getName() << "/" << CallFunc->arg_size() << "(\n";

      std::vector<Value *> ArgVals;
      for (unsigned i = 0; i < CallFunc->arg_size(); ++i) {
        Value *ArgVal = nullptr;
        Type *Ty = CallFunc->getFunctionType()->getParamType(i);
        if (i == 0) ArgVal = FuncInfo->RegValueMap[ARM::R0];
        else if (i == 1) ArgVal = FuncInfo->RegValueMap[ARM::R1];
        else if (i == 2) ArgVal = FuncInfo->RegValueMap[ARM::R2];
        else if (i == 3) ArgVal = FuncInfo->RegValueMap[ARM::R3];
        else assert(false && "Too many arguments for register only function call; fixing later.");
        if (ArgVal->getType() != Ty) {
          raw_ostream &OS = Monitor::event_raw();
          OS << "Arg " << i << ": "; ArgVal->getType()->print(OS); OS << " => "; Ty->print(OS); OS << "\n";
          // Handle special string value to pointer case, should obviously be moved
          Instruction *Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB); Monitor::event_Instruction(Cast);
          ArgVal = Cast;
        }
        ArgVals.push_back(ArgVal);
      }

      Instruction *Instr = CallInst::Create(CallFunc, ArgVals, "", BB); Monitor::event_Instruction(Instr);
    } break;
    case ARM::CMNri: { // 755 | CMN{S}<c> <Rn>, #<imm> => Rn + Imm
      assert(false && "ARM::CMNri not yet implemented; requires NZCV flags");
    } break;
    case ARM::CMPri: { // 759 | CMP{S}<c> <Rn>, #<imm> => Rn - Imm
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Value *Rn = getOperandValue(MI, 0);
      Value *imm = getOperandValue(MI, 1, Ty);

      // Negative flag
      Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Rn, imm, "CMPriNeg", BB); Monitor::event_Instruction(CmpNeg);
      Instruction *StoreNeg = new StoreInst(CmpNeg, FuncInfo->AllocaMap[0], BB); Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Rn, imm, "CMPriZero", BB); Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, FuncInfo->AllocaMap[1], BB); Monitor::event_Instruction(StoreZero);

      // Carry flag
      //Instruction *CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, imm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
      Instruction *CallUAdd = CallInst::Create(Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Ty), {Rn, imm}, "UAddInstrinsic", BB); Monitor::event_Instruction(CallUAdd);
      Instruction *C_Flag = ExtractValueInst::Create(CallUAdd, 1, "CMPCFlag", BB); Monitor::event_Instruction(C_Flag);
      Instruction *StoreCarry = new StoreInst(C_Flag, FuncInfo->AllocaMap[2], BB); Monitor::event_Instruction(StoreCarry);

      // Overflow flag
      //Instruction *CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, imm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
      Instruction *CallSAdd = CallInst::Create(Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Ty), {Rn, imm}, "SAddIntrinsic", BB); Monitor::event_Instruction(CallSAdd);
      Instruction *V_Flag = ExtractValueInst::Create(CallSAdd, 1, "CMPVFlag", BB); Monitor::event_Instruction(V_Flag);
      Instruction *StoreOverflow = new StoreInst(V_Flag, FuncInfo->AllocaMap[3], BB); Monitor::event_Instruction(StoreOverflow);
    } break;
    case ARM::CMPrr: { // 760 | CMP{S}<c> <Rn>, <Rm> => Rn - Rm
      Value *Rn = getOperandValue(MI, 0);
      Value *Rm = getOperandValue(MI, 1);
      
      // Negative flag
      Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Rn, Rm, "CMPriNeg", BB); Monitor::event_Instruction(CmpNeg);
      Instruction *StoreNeg = new StoreInst(CmpNeg, FuncInfo->AllocaMap[0], BB); Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Rn, Rm, "CMPriZero", BB); Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, FuncInfo->AllocaMap[1], BB); Monitor::event_Instruction(StoreZero);

      // Carry flag
      Instruction *CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, Rm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
      Instruction *StoreCarry = new StoreInst(CmpCarry, FuncInfo->AllocaMap[2], BB); Monitor::event_Instruction(StoreCarry);
      
      // Overflow flag
      Instruction *CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, Rm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
      Instruction *StoreOverflow = new StoreInst(CmpOverflow, FuncInfo->AllocaMap[3], BB); Monitor::event_Instruction(StoreOverflow);
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

      Instruction *Instr = BinaryOperator::Create(Instruction::Xor, Rn, imm, "EORri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::EORrr: { // 776 | EOR{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn ^ Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Xor, Rn, Rm, "EORrr", BB); Monitor::event_Instruction(Instr);
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

      Instruction *Ptr = BinaryOperator::Create(Instruction::Add, Rn, imm, "LDRBi12Ptr", BB); Monitor::event_Instruction(Ptr);
      Instruction *Instr = new LoadInst(Ty, Ptr, "LDRBi12", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LDRH: { // 840 | LDRH<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      Type *Ty = Type::getInt16Ty(BB->getContext());
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Ty);

      Instruction *Ptr = BinaryOperator::Create(Instruction::Add, Rn, imm, "LDRHPtr", BB); Monitor::event_Instruction(Ptr);
      Instruction *Instr = new LoadInst(Ty, Ptr, "LDRH", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LDRSB: { // 845 | LDRSB<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      assert(false && "ARM::LDRSB not yet implemented");
    } break;
    case ARM::LDRi12: { // 862 | LDR<c> Rt, [Rn, imm] => Rt = *(Rn + imm)
      Type *Ty = Type::getInt32Ty(BB->getContext()); // TODO: Use the type of the operand
      Type *PtrTy = Ty->getPointerTo();
      Value *Rn = getOperandValue(MI, 1);
      Value *Ptr = nullptr;

      if (MI->getOperand(1).isGlobal()) {
        Monitor::event_raw() << "LDRi12: Global\n";
        const GlobalValue *cGV = MI->getOperand(1).getGlobal();
        GlobalValue *GV = const_cast<GlobalValue *>(cGV);
        setOperandValue(MI, 0, GV);
        break;
      }
      
      if (Rn->getType()->isPointerTy()
       && MI->getOperand(2).isImm()
       && MI->getOperand(2).getImm() == 0) {
        Ptr = Rn;
      } else {
        Value *imm = getOperandValue(MI, 2, Ty);
        Instruction *Add = BinaryOperator::Create(Instruction::Add, Rn, imm, "STRi12", BB); Monitor::event_Instruction(Add);
        Ptr = Add;
      }

      if (!Ptr->getType()->isPointerTy()) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy, "", BB); Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }

      Instruction *Instr = new LoadInst(Ty, Ptr, "LDRi12", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
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
    case ARM::MOVi: { // 872 | MOV<c> <Rd>, #<imm> => Rd = imm + 0
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Value *imm = getOperandValue(MI, 1, Ty);
      
      Value *zero = ConstantInt::get(Ty, 0);
      Instruction *Instr = BinaryOperator::Create(Instruction::Add, imm, zero, "MOVi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::MOVi16: { // 873 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVi16 not yet implemented");
    } break;
    case ARM::MOVr: { // 874 | MOV<c> <Rd>, <Rn> => Rd = Rn
      Monitor::event_raw() << "set Rd to Rn\n";
      setOperandValue(MI, 0, getOperandValue(MI, 1));
    } break;
    case ARM::MUL: { // 888 | MUL<c> <Rd>, <Rn>, <Rm> => Rd = Rn * Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Mul, Rn, Rm, "MUL", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::MVNi: { // 1736 | MVN<c> <Rd>, #<imm> => Rd = 0 - imm
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Value *imm = getOperandValue(MI, 1, Ty);
      Value *zero = ConstantInt::get(Ty, 0);

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, zero, imm, "MVNi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::MVNr: { // 1737 | MVN<c> <Rd>, <Rm> => Rd = 0 - Rm
      Value *Rm = getOperandValue(MI, 1);
      Value *zero = ConstantInt::get(Rm->getType(), 0);

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, zero, Rm, "MVNr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ORRri: { // 1748 | ORR<c> <Rd>, <Rn>, #<imm> => Rd = Rn OR imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Rn->getType());

      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Rn, imm, "ORRri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ORRrr: { // 1749 | ORR<c> <Rd>, <Rn>, <Rm> => Rd = Rn OR Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Rn, Rm, "ORRrr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::SBCri: { // 1794 | SBC<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm - C
      assert(false && "ARM::SBCri not yet implemented; requires Carry flag");
    } break;
    case ARM::SBCrr: { // 1795 | SBC<c> <Rd>, <Rn>, <Rm> => Rd = Rn - Rm - C
      assert(false && "ARM::SBCrr not yet implemented; requires Carry flag");
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
    case ARM::STRH: { // 1915 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRH not yet implemented");
    } break;
    case ARM::STRi12: { // 1926 | STR<c> Rt, [Rn, imm] => &*(Rn+imm) = Rt
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Type *PtrTy = PointerType::get(Ty, 0);
      Value *Rt = getOperandValue(MI, 0);
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Ty);
      Value *Ptr = nullptr;

      if (Rn->getType()->isPointerTy() && MI->getOperand(2).getImm() == 0) {
        Ptr = Rn;
      } else {
        Instruction *Add = BinaryOperator::Create(Instruction::Add, Rn, imm, "STRi12", BB); Monitor::event_Instruction(Add);
        Ptr = Add;
      }

      if (!Ptr->getType()->isPointerTy()) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy, "", BB); Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }

      Instruction *Instr = new StoreInst(Rt, Ptr, false, BB); Monitor::event_Instruction(Instr);
    } break;
    case ARM::SUBri: { // 1928 | SUB<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Rn->getType());

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, Rn, imm, "SUBri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::SUBrr: { // 1929 | SUB<c> <Rd>, <Rn>, <Rm> => Rd = Rn - Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, Rn, Rm, "SUBrr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::SVC: { // 1932 | SVC<c> #<imm>
      assert(false && "ARM::SVC not yet implemented");
    } break;
  }
  Monitor::event_end("ARMSelectionDAGISelBypassPass::RaiseMachineInstr");
  return true;
}

#undef DEBUG_TYPE
