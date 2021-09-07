
#include "ARMLinearRaiserPass.h"

#include "Monitor.h"

#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "llvm/IR/Instruction.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMLinearRaiserPass::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMLinearRaiserPass");
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass start.\n");

  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);

  // Set up entry block.
  BasicBlock *BB = &F->getEntryBlock();
  MachineBasicBlock *MBB = &MF->front();
  MBBBBMap[MBB] = BB;
  BB->setName("entry");

  // Allocate NZCV flags.
  Type *Ty = Type::getInt1Ty(Context);
  for (unsigned i = 0; i < 4; i++) {
    Monitor::event_raw() << "Allocate NZCV flag " << i << " for " << F->getName() << "\n";
    Align MALG(32);
    AllocaInst *Alloc = new AllocaInst(Ty, 0, nullptr, MALG, "", BB);
    Flags.push_back(Alloc);
  }

  // Add arguments to RegValueMap
  if (F->arg_size() > 0) RegValueMap[ARM::R0] = F->getArg(0);
  if (F->arg_size() > 1) RegValueMap[ARM::R1] = F->getArg(1);
  if (F->arg_size() > 2) RegValueMap[ARM::R2] = F->getArg(2);
  if (F->arg_size() > 3) RegValueMap[ARM::R3] = F->getArg(3);
  // TODO variadic arguments

  std::vector<MachineInstr *> Worklist;
  for (MachineBasicBlock &MBB : *MF)
    for (MachineInstr &MI : MBB)
      Worklist.push_back(&MI);

  Monitor::event_raw() << "LinearRaiser: " << Worklist.size() << " instructions to process.\n";
  for (MachineInstr *MI : Worklist)
    raiseMachineInstr(MI);

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass end.\n");
  Monitor::event_end("ARMLinearRaiserPass");
  return true;
}

// TODO: Move CC to separate pass.
Value *ARMLinearRaiserPass::ARMCCToValue(int Cond, BasicBlock *BB) {
  // Why do ICmpInst constructors put &InsertBefore/&InsertAtEnd as the first
  // operand instead of *InsertBefore/*InsertAtEnd as the last one? Who knows.
  switch (Cond) {
    default:
      llvm_unreachable("Unknown condition code!");
    case ARMCC::EQ: { // Z = 1
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS"); Monitor::event_Instruction(ZS);
      return ZS;
    } break;
    case ARMCC::NE: { // Z = 0
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC"); Monitor::event_Instruction(ZC);
      return ZC;
    } break;
    case ARMCC::HS: { // C = 1
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS"); Monitor::event_Instruction(CS);
      return CS;
    } break;
    case ARMCC::LO: { // C = 0
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC"); Monitor::event_Instruction(CC);
      return CC;
    } break;
    case ARMCC::MI: { // N = 1
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *NS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getTrue(Context), "NS"); Monitor::event_Instruction(NS);
      return NS;
    } break;
    case ARMCC::PL: { // N = 0
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *NC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getFalse(Context), "NC"); Monitor::event_Instruction(NC);
      return NC;
    } break;
    case ARMCC::VS: { // V = 1
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *VS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getTrue(Context), "VS"); Monitor::event_Instruction(VS);
      return VS;
    } break;
    case ARMCC::VC: { // V = 0
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *VC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getFalse(Context), "VC"); Monitor::event_Instruction(VC);
      return VC;
    } break;
    case ARMCC::HI: { // C = 1 && Z = 0
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS"); Monitor::event_Instruction(CS);
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC"); Monitor::event_Instruction(ZC);
      Instruction *HI = BinaryOperator::Create(Instruction::Add, CS, ZC, "HI", BB); Monitor::event_Instruction(HI);
      return HI;
    } break;
    case ARMCC::LS: { // C = 0 || Z = 1
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC"); Monitor::event_Instruction(CC);
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS"); Monitor::event_Instruction(ZS);
      Instruction *LS = BinaryOperator::Create(Instruction::Or, CC, ZS, "LS", BB); Monitor::event_Instruction(LS);
      return LS;
    } break;
    case ARMCC::GE: { // N = V
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *GE = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "GE"); Monitor::event_Instruction(GE);
      return GE;
    } break;
    case ARMCC::LT: { // N != V
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *LT = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "LT"); Monitor::event_Instruction(LT);
      return LT;
    } break;
    case ARMCC::GT: { // Z = 0 && N = V
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC"); Monitor::event_Instruction(ZC);
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *Sign = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "S"); Monitor::event_Instruction(Sign);
      Instruction *GT = BinaryOperator::Create(Instruction::And, ZC, Sign, "GT", BB); Monitor::event_Instruction(GT);
      return GT;
    } break;
    case ARMCC::LE: { // Z = 1 || N != V
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS"); Monitor::event_Instruction(ZS);
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *Sign = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "S"); Monitor::event_Instruction(Sign);
      Instruction *LE = BinaryOperator::Create(Instruction::Or, ZS, Sign, "LE", BB); Monitor::event_Instruction(LE);
      return LE;
    } break;
    case ARMCC::AL: { // always
      return ConstantInt::getTrue(Context);
    } break;
  }
}

bool ARMLinearRaiserPass::raiseMachineInstr(MachineInstr *MI) {
  Monitor::event_start("ARMLinearRaiserPass::RaiseMachineInstr");
  Monitor::event_MachineInstr(MI);
  Monitor::event_stateswitch();

  MachineBasicBlock *MBB = MI->getParent();
  BasicBlock *BB;
  DenseMapIterator<MachineBasicBlock*, BasicBlock *> I = MBBBBMap.find(MBB);
  if (I != MBBBBMap.end()) {
    BB = I->second;
  } else {
    BB = BasicBlock::Create(Context, "", F);
    MBBBBMap[MBB] = BB;
  }
  
  // TODO: Fix and move to instruction splitting
  int idx = MI->findFirstPredOperandIdx();
  if (idx != -1) {
    ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(idx).getImm();
    // Register CPSR = MI->getOperand(idx+1).getReg();
    if (CC != ARMCC::AL) {
      if (MI->getOpcode() != ARM::Bcc) {
      Monitor::ERROR("Condition code not supported");
      //Instruction *Cond = getCond(CC, CPSR, BB, Instrs);
      }
    }
  }

  switch (MI->getOpcode()) {
    default: {
      auto OS = WithColor(errs(), HighlightColor::Warning);
      OS << "ARMLinearRaiserPass encountered unhandled opcode in instruction ";
      Monitor::printMachineInstr(MI, true, OS);
      assert(false && "Unhandled opcode");
      return false;
    } break;
    /*
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
      Instruction *C = new LoadInst(Type::getInt1Ty(BB->getContext()), Flags[1], "Carry", BB); Monitor::event_Instruction(C);
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
      ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);

      Monitor::event_raw() << "Creating call\n";
      uint64_t imm = (uint64_t) MI->getOperand(0).getImm();
      Function *CallFunc = MR.getRaisedFunctionAt(imm);
       
      if (CallFunc == nullptr)
        CallFunc = AMR.getSyscallFunc(imm);
      
      assert(CallFunc && "No function found for call");
      unsigned ArgCount = AMR.getFunctionArgNum(imm);
      if (ArgCount < CallFunc->arg_size()) ArgCount = CallFunc->arg_size();
      Monitor::event_raw() << "Call Function: " << CallFunc->getName() << "/" << ArgCount << "(\n";

      std::vector<Value *> ArgVals;
      const MachineFrameInfo &MFI = FuncInfo->MF->getFrameInfo();
      for (unsigned i = 0; i < ArgCount; ++i) {
        Value *ArgVal = nullptr;
        Type *Ty = CallFunc->getFunctionType()->getParamType(i);
        if (i < 4) ArgVal = RegValueMap[ARM::R0 + i];
        else assert(false && "Fixing later");

        if (ArgVal->getType() != Ty && i < CallFunc->arg_size()) { // Skip variadic args
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
        // RegValueMap[ARM::R0] = ConstantInt::get(Type::getInt32Ty(BB->getContext()), 0);
      } else {
        RegValueMap[ARM::R0] = Instr;
      }
    } break;
    case ARM::BX_RET: { // 718 | BX_RET = BX LR = Return
      Type *RetTy = FuncInfo->Fn->getReturnType();

      if (RetTy->isVoidTy()) {
        Instruction *Instr = ReturnInst::Create(BB->getContext(), BB); Monitor::event_Instruction(Instr);
      } else if (RetTy->isIntegerTy()) {
        Instruction *Instr = ReturnInst::Create(BB->getContext(), RegValueMap[ARM::R0], BB); Monitor::event_Instruction(Instr);
      } else
        assert(false && "Unsupported return type");
    } break;
    case ARM::Bcc: { // 720 | Bcc <label> <cond?> => Branch

      // B | A MachineBlock ending with an unconditional branch should have one successor
      if (MI->getOperand(1).getImm() == ARMCC::AL && MBB->succ_size() == 1) {
        BasicBlock *BranchBB = FuncInfo->getOrCreateBasicBlock(*MBB->succ_begin());
        Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
        Instruction *Branch = BranchInst::Create(BranchBB, BB); Monitor::event_Instruction(Branch);
        break;
      }

      // Call | If no successor was added during CFG construction, the branch is a shorted call
      if (MI->getOperand(1).getImm() == ARMCC::AL && MBB->succ_size() == 0) {
        ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
        Monitor::event_raw() << "Replacing unconditional ending branch with call and return\n";
        uint64_t imm = (uint64_t) MI->getOperand(0).getImm();
        Function *CallFunc = MR.getRaisedFunctionAt(imm);
        
        if (CallFunc == nullptr)
          CallFunc = AMR.getSyscallFunc(imm);
        
        assert(CallFunc && "No function found for call");
        unsigned ArgCount = AMR.getFunctionArgNum(imm);
        if (ArgCount < CallFunc->arg_size()) ArgCount = CallFunc->arg_size();
        Monitor::event_raw() << "Call Function: " << CallFunc->getName() << "/" << ArgCount << "(\n";

        std::vector<Value *> ArgVals;
        const MachineFrameInfo &MFI = FuncInfo->MF->getFrameInfo();
        for (unsigned i = 0; i < ArgCount; ++i) {
          Value *ArgVal = nullptr;
          Type *Ty = CallFunc->getFunctionType()->getParamType(i);
          if (i < 4) ArgVal = RegValueMap[ARM::R0 + i];
          else assert(false && "Fixing later");

          if (ArgVal->getType() != Ty && i < CallFunc->arg_size()) {
            raw_ostream &OS = Monitor::event_raw();
            OS << "Arg " << i << ": "; ArgVal->getType()->print(OS); OS << " => "; Ty->print(OS); OS << "\n";
            Instruction *Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB); Monitor::event_Instruction(Cast);
            ArgVal = Cast;
          }
          ArgVals.push_back(ArgVal);
        }
        Instruction *Instr = CallInst::Create(CallFunc, ArgVals, "", BB); Monitor::event_Instruction(Instr);
        if (CallFunc->getReturnType()->isVoidTy()) {
          // RegValueMap[ARM::R0] = ConstantInt::get(Type::getInt32Ty(BB->getContext()), 0);
          Instruction *Return = ReturnInst::Create(BB->getContext(), BB); Monitor::event_Instruction(Return);
        } else {
          RegValueMap[ARM::R0] = Instr;
          Instruction *Return = ReturnInst::Create(BB->getContext(), RegValueMap[ARM::R0], BB); Monitor::event_Instruction(Return);
        }
        break;
      }

      // B<cc> | A conditional branch either branches to the target, or falls through to the next block
      if (MI->getOperand(1).getImm() != ARMCC::AL && MBB->succ_size() == 2) {
        Value *Cond = ARMCCToValue(MI->getOperand(1).getImm(), BB);
        BasicBlock *BranchBB = FuncInfo->getOrCreateBasicBlock(*MBB->succ_begin());
        Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
        BasicBlock *NextBB = FuncInfo->getOrCreateBasicBlock(&*std::next(MBB->getIterator()));
        Monitor::event_raw() << "Next BB: " << BranchBB->getName() << "\n";          
        Instruction *Branch = BranchInst::Create(BranchBB, NextBB, Cond, BB); Monitor::event_Instruction(Branch);
        break;
      }

      assert(false && "Encountered a Bcc instruction with unexpected operands");
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
      Instruction *StoreNeg = new StoreInst(CmpNeg, Flags[0], BB); Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Rn, imm, "CMPriZero", BB); Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, Flags[1], BB); Monitor::event_Instruction(StoreZero);

      // Carry flag
      //Instruction *CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, imm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
      Instruction *CallUAdd = CallInst::Create(Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Ty), {Rn, imm}, "UAddInstrinsic", BB); Monitor::event_Instruction(CallUAdd);
      Instruction *C_Flag = ExtractValueInst::Create(CallUAdd, 1, "CMPCFlag", BB); Monitor::event_Instruction(C_Flag);
      Instruction *StoreCarry = new StoreInst(C_Flag, Flags[2], BB); Monitor::event_Instruction(StoreCarry);

      // Overflow flag
      //Instruction *CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, imm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
      Instruction *CallSAdd = CallInst::Create(Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Ty), {Rn, imm}, "SAddIntrinsic", BB); Monitor::event_Instruction(CallSAdd);
      Instruction *V_Flag = ExtractValueInst::Create(CallSAdd, 1, "CMPVFlag", BB); Monitor::event_Instruction(V_Flag);
      Instruction *StoreOverflow = new StoreInst(V_Flag, Flags[3], BB); Monitor::event_Instruction(StoreOverflow);
    } break;
    case ARM::CMPrr: { // 760 | CMP{S}<c> <Rn>, <Rm> => Rn - Rm
      Value *Rn = getOperandValue(MI, 0);
      Value *Rm = getOperandValue(MI, 1);
      
      // Negative flag
      Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Rn, Rm, "CMPriNeg", BB); Monitor::event_Instruction(CmpNeg);
      Instruction *StoreNeg = new StoreInst(CmpNeg, Flags[0], BB); Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Rn, Rm, "CMPriZero", BB); Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, Flags[1], BB); Monitor::event_Instruction(StoreZero);

      // Carry flag
      Instruction *CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, Rm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
      Instruction *StoreCarry = new StoreInst(CmpCarry, Flags[2], BB); Monitor::event_Instruction(StoreCarry);
      
      // Overflow flag
      Instruction *CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, Rm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
      Instruction *StoreOverflow = new StoreInst(CmpOverflow, Flags[3], BB); Monitor::event_Instruction(StoreOverflow);
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
      assert(false && "ARM::LDRBi12 not yet implemented");
    } break;
    case ARM::LDREX: { // 836 | <c> Rt, [Rn {, offset}]
      assert(false && "ARM::LDREX not yet implemented");
      // Instruction *CallUAdd = CallInst::Create(
      //   Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_ldrex),
      //   {Rn, imm}, "LDREX", BB); Monitor::event_Instruction(CallUAdd);
    } break;
    case ARM::LDRH: { // 840 | LDRH<c> <Rt>, [<Rn>, #<imm>] => Rt = *(Rn + imm)
      assert(false && "ARM::LDRH not yet implemented");
    } break;
    case ARM::LDRSB: { // 845 | LDR{type}{cond} Rt, [Rn {, #offset}]
      // SB | Load Signed Byte; Sign extend to 32 bits
      Type *Ty8 = Type::getInt8Ty(BB->getContext());
      Type *Ty32 = Type::getInt32Ty(BB->getContext());
      Value *Rn = getOperandValue(MI, 1);
      assert(MI->getOperand(2).getReg() == 0 && MI->getOperand(3).getImm() == 0
        && "ARM::LDRSB: assuming zero offsets for now");

      Instruction *Cast = new IntToPtrInst(Rn, Ty32->getPointerTo(), "LDRSBPtrCast", BB); Monitor::event_Instruction(Cast);
      Instruction *Instr = new LoadInst(Ty32, Cast, "LDRSBLoad", BB); Monitor::event_Instruction(Instr);
      Instruction *Trunc = new TruncInst(Instr, Ty8, "LDRSBTrunc", BB); Monitor::event_Instruction(Trunc);
      Instruction *SExt = new SExtInst(Trunc, Ty32, "LDRSBSExt", BB); Monitor::event_Instruction(SExt);
      setOperandValue(MI, 0, SExt);
    } break;
    */
    case ARM::LDRi12: { // 862 | LDRi12 Rt, Rn, Imm, CC, CPSR { Reg:$R0 Reg:$PC Imm:52 Imm:14 Reg:$ }
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *PtrTy32 = Ty32->getPointerTo();

      assert(
        MI->getOperand(0).isReg() && 
        MI->getOperand(1).isReg() && 
        MI->getOperand(2).isImm() && 
        MI->getOperand(3).isImm() && 
        MI->getOperand(4).isReg() &&
        "ARM::LDRi12: unexpecting operands"
      );

      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int32_t Imm = MI->getOperand(2).getImm();
      int32_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::LDRi12: assuming no flags for now"
      );

      Value *Ptr;

      if (Rn == ARM::PC) {
        uint64_t offset = MCIR->getMCInstIndex(*MI);
        Monitor::event_raw() << "address = " << offset+Imm+8 << "\n";
        Constant *address = ConstantInt::get(Ty32, offset+Imm+8);
        Ptr = ConstantExpr::getIntToPtr(address, PtrTy32);
      } else {
        Ptr = RegValueMap[Rn];
        assert(Ptr->getType() == Ty32 && "ARM::LDRi12: expecting i32 type");
        if (Imm != 0) {
          Instruction *Add = BinaryOperator::Create(Instruction::Add, Ptr, ConstantInt::get(Ty32, Imm), "LDRi12Add", BB);
          Monitor::event_Instruction(Add);
          Ptr = Add;
        }
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy32, "LDRi12Cast", BB);
        Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }

      Instruction *Instr = new LoadInst(Ty32, Ptr, "LDRi12", BB);
      Monitor::event_Instruction(Instr);
      
      RegValueMap[Rt] = Instr;
    } break;
    /*
    case ARM::MLA: { // 868 | MLA<c> <Rd>, <Rn>, <Rm>, <Ra> => Rd = Rn * Ra + Rm
      assert(false && "ARM::MLA not yet implemented");
    } break;
    case ARM::MOVPCLR: { // 870 | MOV<c> PC, LR => PC = LR
      Type *RetTy = FuncInfo->Fn->getReturnType();

      if (RetTy->isVoidTy()) {
        Instruction *Instr = ReturnInst::Create(BB->getContext(), BB); Monitor::event_Instruction(Instr);
      } else if (RetTy->isIntegerTy()) {
        Instruction *Instr = ReturnInst::Create(BB->getContext(), RegValueMap[ARM::R0], BB); Monitor::event_Instruction(Instr);
      } else
        assert(false && "Unsupported return type");
    } break;
    case ARM::MOVTi16: { // 871 | MOVT<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVTi16 not yet implemented");
    } break;
    */
    case ARM::MOVi: { // 872 | MOV<c> <Rd>, #<imm> => Rd = imm + 0
      // MOVi (872) { Reg:$R1 Imm:1 Imm:14 Reg:$ Reg:$ }
      Type *Ty32 = Type::getInt32Ty(Context);

      assert(
        MI->getOperand(0).isReg() &&
        MI->getOperand(1).isImm() &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(3).isReg() &&
        MI->getOperand(4).isReg() &&
        "ARM::MOVi: unexpecting operands"
      );

      Register Rt = MI->getOperand(0).getReg();
      int32_t Imm = MI->getOperand(1).getImm();
      int32_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();
      Register S = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::LDRi12: assuming no flags for now"
      );

      RegValueMap[Rt] = ConstantInt::get(Ty32, Imm);
      Monitor::event_raw() << "Reg " << Rt << " <= " << Imm << "\n";
    } break;
    /*
    case ARM::MOVi16: { // 873 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVi16 not yet implemented");
    } break;
    case ARM::MOVr: { // 874 | MOV<c> <Rd>, <Rn> => Rd = Rn
      Monitor::event_raw() << "set Rd to Rn\n";
      setOperandValue(MI, 0, getOperandValue(MI, 1));
    } break;
    case ARM::MOVsi: { // 876 | MOV<c> <Rd>, #<imm> => Rd = imm
      assert(false && "ARM::MOVsi not yet implemented");
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
    */
    case ARM::STRi12: { // 1926 | STR<c> Rt, [Rn, imm] => &*(Rn+imm) = Rt
      // STRi12 (1926) { Reg:$R0 Reg:$SP Imm:8 Imm:14 Reg:$ }
      Type *Ty = Type::getInt32Ty(BB->getContext());
      Type *PtrTy = Ty->getPointerTo();

      assert(
        MI->getOperand(0).isReg() &&
        MI->getOperand(1).isReg() &&
        MI->getOperand(2).isImm() &&
        MI->getOperand(3).isImm() &&
        MI->getOperand(4).isReg() &&
        "Invalid operand for ARM::STRi12"
      );

      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int32_t imm = MI->getOperand(2).getImm();
      int32_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      

    } break;
    /*
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
    */
  }
  Monitor::event_end("ARMLinearRaiserPass::RaiseMachineInstr");
  return true;
}

#undef DEBUG_TYPE
