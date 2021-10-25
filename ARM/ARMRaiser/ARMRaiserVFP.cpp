// Static ARMRaiser implementations for the
// ARM VFP instruction set.

#include "ARMRaiser.h"

/* static */ bool ARMRaiser::raiseFMSTAT(ARMRaiserState* State, MachineInstr* MI) { //  786 | FMRX CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  // FMRX r15, FPSCR Alias; which doesnt move to PC, but instead targets CPSR
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(0).getImm();
  Register CPSR = MI->getOperand(1).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Monitor::event_raw() << "Moving FPSCR flags to CPSR\n";
  State->setStatus(ARMState::CPSR_N, State->getStatus(ARMState::FPSCR_N, BB), BB);
  State->setStatus(ARMState::CPSR_Z, State->getStatus(ARMState::FPSCR_Z, BB), BB);
  State->setStatus(ARMState::CPSR_C, State->getStatus(ARMState::FPSCR_C, BB), BB);
  State->setStatus(ARMState::CPSR_V, State->getStatus(ARMState::FPSCR_V, BB), BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseFCONSTD(ARMRaiserState* State, MachineInstr* MI) { // 780 | VMOV.F64 Dd Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  uint8_t Sign = (Imm >> 7) & 0x1;
  uint8_t Exp = (Imm >> 4) & 0x7;
  uint8_t Mantissa = Imm & 0xf;
  float FImm = bit_cast<float>(
      (Sign << 31)
    | (((Exp & 0x4) != 0 ? 0 : 1) << 30)
    | (((Exp & 0x4) != 0 ? 0x1f : 0) << 25)
    | ((Exp & 0x3) << 23)
    | (Mantissa << 19)
  );
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* ImmVal = ConstantFP::get(Type::getDoubleTy(State->Context), FImm);
  State->setReg(Dd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseFCONSTS(ARMRaiserState* State, MachineInstr* MI) { // 782 | VMOV.F32 Sd Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Sd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  uint8_t Sign = (Imm >> 7) & 0x1;
  uint8_t Exp = (Imm >> 4) & 0x7;
  uint8_t Mantissa = Imm & 0xf;
  float F = bit_cast<float>(
      (Sign << 31)
    | (((Exp & 0x4) != 0 ? 0 : 1) << 30)
    | (((Exp & 0x4) != 0 ? 0x1f : 0) << 25)
    | ((Exp & 0x3) << 23)
    | (Mantissa << 19)
  );
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* ImmVal = ConstantFP::get(Type::getFloatTy(State->Context), F);
  Monitor::event_raw() << "Reg: " << Sd << " <= " << F << "\n";
  State->setReg(Sd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVABSD(ARMRaiserState* State, MachineInstr* MI) { // 2026 | VABS.F64 Dd Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Cmp = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OLT, DmVal, ConstantFP::get(State->Context, APFloat(0.0)), "VABSDCmp", BB);
  Monitor::event_Instruction(Cmp);
  Instruction* Neg = BinaryOperator::Create(Instruction::FSub, ConstantFP::get(State->Context, APFloat(0.0)), DmVal, "VABSDNeg", BB);
  Monitor::event_Instruction(Neg);

  Instruction* Sel = SelectInst::Create(Cmp, Neg, DmVal, "VABSDSelect", BB);
  Monitor::event_Instruction(Sel);

  State->setReg(Dd, Sel, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVADDD(ARMRaiserState* State, MachineInstr* MI) { // 2047 | VADD.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FAdd, DnVal, DmVal, "VADDD", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVADDS(ARMRaiserState* State, MachineInstr* MI) { // 2058 | VADD.F32 Sd Sn Sm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Sd = MI->getOperand(0).getReg();
  Register Sn = MI->getOperand(1).getReg();
  Register Sm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* SnVal = State->getReg(Sn, Type::getFloatTy(State->Context), BB);
  Value* SmVal = State->getReg(Sm, Type::getFloatTy(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FAdd, SnVal, SmVal, "VADDS", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Sd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVCMPD(ARMRaiserState* State, MachineInstr* MI) { // 2213 | VCMP.F64 Dd Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = State->getReg(Dd, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  // TODO: Handle NaN

  Instruction* CmpLT = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OLT, DdVal, DmVal, "VCMPDCmp", BB);
  Monitor::event_Instruction(CmpLT);
  State->setStatus(ARMState::FPSCR_N, CmpLT, BB);

  Instruction* CmpEQ = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OEQ, DdVal, DmVal, "VCMPDCmp", BB);
  Monitor::event_Instruction(CmpEQ);
  State->setStatus(ARMState::FPSCR_Z, CmpEQ, BB);

  Instruction* CmpGE = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OGE, DdVal, DmVal, "VCMPDCmp", BB);
  Monitor::event_Instruction(CmpGE);
  State->setStatus(ARMState::FPSCR_C, CmpGE, BB);

  State->setStatus(ARMState::FPSCR_V, ConstantInt::get(Type::getInt1Ty(State->Context), 0), BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVCMPZD(ARMRaiserState* State, MachineInstr* MI) { // 2222 | VCMP.F64 Dd [0] CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = State->getReg(Dd, Type::getDoubleTy(State->Context), BB);

  // TODO: Handle NaN

  Instruction* CmpLT = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OLT, DdVal, ConstantFP::get(State->Context, APFloat(0.0)), "VCMPZDCmp", BB);
  Monitor::event_Instruction(CmpLT);
  State->setStatus(ARMState::FPSCR_N, CmpLT, BB);

  Instruction* CmpEQ = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OEQ, DdVal, ConstantFP::get(State->Context, APFloat(0.0)), "VCMPZDCmp", BB);
  Monitor::event_Instruction(CmpEQ);
  State->setStatus(ARMState::FPSCR_Z, CmpEQ, BB);

  Instruction* CmpGE = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OGE, DdVal, ConstantFP::get(State->Context, APFloat(0.0)), "VCMPZDCmp", BB);
  Monitor::event_Instruction(CmpGE);
  State->setStatus(ARMState::FPSCR_C, CmpGE, BB);

  State->setStatus(ARMState::FPSCR_V, ConstantInt::get(Type::getInt1Ty(State->Context), 0), BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVDIVD(ARMRaiserState* State, MachineInstr* MI) { // 2327 | VDIV.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FDiv, DnVal, DmVal, "VDIVD", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVLDMDIA_UPD(ARMRaiserState* State, MachineInstr* MI) { // 2778 | VLDM.F64 Rt! {Rwb} CC CPSR Dn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Dn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(State, Rt, 0, Type::getDoubleTy(State->Context), BB);
    Monitor::event_raw() << "Reg " << Dn << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getDoubleTy(State->Context), Ptr, "VLDMDIA_UPD", BB);
    Monitor::event_Instruction(Load);

    State->setReg(Dn, Load, BB);

    Monitor::event_raw() << "incrementing SP by 8\n";
    State->BBStateMap[BB]->QState->SP_offset += 8;
    return true;
  }

  assert(Rt != ARM::PC && "VLDMDIA_UPD: Rt == PC");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getDoublePtrTy(State->Context), "VLDMDIA_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getDoubleTy(State->Context), Ptr, "VLDMDIA_UPD", BB);
  Monitor::event_Instruction(Load);

  State->setReg(Dn, Load, BB);

  Monitor::event_raw() << "VLDMDIA_UPD: incrementing Rt by 8\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Add, RtVal, ConstantInt::get(Type::getInt32Ty(State->Context), 8), "VLDMDIA_UPD", BB);
  Monitor::event_Instruction(Add);

  State->setReg(Rt, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVLDRD(ARMRaiserState* State, MachineInstr* MI) { // 2783 | VLDR.F64 Dd Rn Imm/4 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rn == ARM::PC) { // Load PC-relative GlobalValue
    // A direct offset has to be a multiple of 4, and a PC-relative offset
    // created by an assembler based on a label or created by adding to the
    // double literal pool has to be word boundary aligned, which is also a
    // multiple of 4. Because of this, LLVM thought it was a great idea to
    // save a whopping 2 bits by using the offset/4 in the instruction.
    Value* GV = getGlobalValueByOffset(State, State->MCIR->getMCInstIndex(*MI), Imm * 4 + 8, Type::getDoubleTy(State->Context));
    Monitor::event_raw() << "Global Value: " << GV->getName() << "\n";
    Instruction* Load = new LoadInst(Type::getDoubleTy(State->Context), GV, "VLDRD", BB);
    Monitor::event_Instruction(Load);
    State->setReg(Dd, Load, BB);
    return true;
  }

  if (Rn == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(State, Rn, Imm*4, Type::getDoubleTy(State->Context), BB);
    Monitor::event_raw() << "Reg " << Dd << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getDoubleTy(State->Context), Ptr, "VLDRS", BB);
    Monitor::event_Instruction(Load);

    State->setReg(Dd, Load, BB);
    return true;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);

  if (Imm != 0) {
    Instruction* Add = BinaryOperator::Create(Instruction::Add, RnVal, ConstantInt::get(Type::getInt32Ty(State->Context), Imm * 4), "VLDRD", BB);
    Monitor::event_Instruction(Add);
    RnVal = Add;
  }

  Instruction* Ptr = new IntToPtrInst(RnVal, Type::getDoublePtrTy(State->Context), "VLDRD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getDoubleTy(State->Context), Ptr, "VLDRD", BB);
  Monitor::event_Instruction(Load);

  State->setReg(Dd, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return false;
}
/* static */ bool ARMRaiser::raiseVLDRS(ARMRaiserState* State, MachineInstr* MI) { // 2785 | VLDR.F32 Sd Rn Imm/4 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Sd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rn == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(State, Rn, Imm*4, Type::getFloatTy(State->Context), BB);
    Monitor::event_raw() << "Reg " << Sd << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getFloatTy(State->Context), Ptr, "VLDRS", BB);
    Monitor::event_Instruction(Load);

    State->setReg(Sd, Load, BB);
    return true;
  }

  assert(Rn == ARM::SP && "VLDRS not yet implemented for non-SP registers");

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return false;
}
/* static */ bool ARMRaiser::raiseVMLAD(ARMRaiserState* State, MachineInstr* MI) { // 2838 | VMLA.F64 Dd {Dwb} Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(2).getReg();
  Register Dm = MI->getOperand(3).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = State->getReg(Dd, Type::getDoubleTy(State->Context), BB);
  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Mul = BinaryOperator::Create(Instruction::FMul, DnVal, DmVal, "VMLA.F64", BB);
  Monitor::event_Instruction(Mul);
  Instruction* Add = BinaryOperator::Create(Instruction::FAdd, DdVal, Mul, "VMLA.F64", BB);
  Monitor::event_Instruction(Add);
  State->setReg(Dd, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMOVD(ARMRaiserState* State, MachineInstr* MI) { // 2901 | VMOV.F64 Dd Dn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Monitor::event_raw() << "Reg: " << Dn << " <= " << DnVal << "\n";
  State->setReg(Dd, DnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMOVDRR(ARMRaiserState* State, MachineInstr* MI) { // 2902 | VMOV Dm Rd Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dm = MI->getOperand(0).getReg();
  Register S0 = ARM::S0 + (Dm - ARM::D0) / 2;
  Register S1 = S0 + 1;
  Register Rd = MI->getOperand(1).getReg();
  Register Rn = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RdVal = State->getReg(Rd, Type::getInt32Ty(State->Context), BB);
  State->setReg(S0, RdVal, BB);
  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  State->setReg(S1, RnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMOVRRD(ARMRaiserState* State, MachineInstr* MI) { // 2915 | VMOV Rd Rn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  Register S0 = ARM::S0 + (Dm - ARM::D0) / 2;
  Register S1 = S0 + 1;
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* S0Val = State->getReg(S0, Type::getInt32Ty(State->Context), BB);
  State->setReg(Rd, S0Val, BB);
  Value* S1Val = State->getReg(S1, Type::getInt32Ty(State->Context), BB);
  State->setReg(Rn, S1Val, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMOVRS(ARMRaiserState* State, MachineInstr* MI) { // 2917 | VMOV.F32 Rd Sn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Sn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* SnVal = State->getReg(Sn, Type::getFloatTy(State->Context), BB);
  Monitor::event_raw() << "Reg: " << Rd << " <= " << SnVal << "\n";
  State->setReg(Rd, SnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMOVSR(ARMRaiserState* State, MachineInstr* MI) { // 2919 VMOV.F32 St Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register St = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn,  Type::getFloatTy(State->Context), BB);
  Monitor::event_raw() << "Reg: " << St << " <= " << RnVal << "\n";
  State->setReg(St, RnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMOVv2i32(ARMRaiserState* State, MachineInstr* MI) { // 2924 | VMOV.I32 Dd Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  double_t DImm; memcpy(&DImm, &Imm, sizeof(DImm));
  Value* ImmVal = ConstantFP::get(Type::getDoubleTy(State->Context), DImm);
  Monitor::event_raw() << "Reg: " << Dd << " <= " << ImmVal << "\n";
  State->setReg(Dd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVMULD(ARMRaiserState* State, MachineInstr* MI) { // 2954 | VMUL.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FMul, DnVal, DmVal, "VMULD", BB);
  Monitor::event_Instruction(Result);
  State->setReg(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVNEGD(ARMRaiserState* State, MachineInstr* MI) { // 2995 | VNEG.F64 Dd Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FSub, ConstantFP::get(Type::getDoubleTy(State->Context), 0), DmVal, "VNEGD", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVORRd(ARMRaiserState* State, MachineInstr* MI) { // 3019 | VORR Dd Dn Dm {CC CPSR}
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  assert(!conditional_execution && "ARM VORR must be unconditional");

  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* DnCast = CastInst::Create(Instruction::BitCast, DnVal, Type::getInt64Ty(State->Context), "DnCast", BB);
  Monitor::event_Instruction(DnCast);
  Instruction* DmCast = CastInst::Create(Instruction::BitCast, DmVal, Type::getInt64Ty(State->Context), "DmCast", BB);
  Monitor::event_Instruction(DmCast);
  Instruction* Result = BinaryOperator::Create(Instruction::Or, DnCast, DmCast, "VORR", BB);
  Monitor::event_Instruction(Result);
  Instruction* ResultCast = CastInst::Create(Instruction::BitCast, Result, Type::getDoubleTy(State->Context), "ResultCast", BB);
  Monitor::event_Instruction(ResultCast);
  State->setReg(Dd, ResultCast, BB);

  return true;
}
/* static */ bool ARMRaiser::raiseVSITOD(ARMRaiserState* State, MachineInstr* MI) { // 3463 | VCVT.F64.S32 Dd Sm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Sm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* SmVal = State->getReg(Sm, Type::getInt32Ty(State->Context), BB);

  Instruction* Cast = CastInst::Create(Instruction::SIToFP, SmVal, Type::getDoubleTy(State->Context), "VSITOD", BB);
  Monitor::event_Instruction(Cast);
  State->setReg(Dd, Cast, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVSTMDDB_UPD(ARMRaiserState* State, MachineInstr* MI) { // 3763 VSTM.F64 Rt! {Rwb} CC CPSR Dn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Dn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(Rt == ARM::SP && "VSTMDDB_UPD not yet implemented for non SP registers");

  // Value* RegVal = State->getReg(Reg, Type::getDoubleTy(State->Context), BB);

  Monitor::event_raw() << "VSTMDDB_UPD: decrementing SP by 8\n";
  State->BBStateMap[BB]->QState->SP_offset -= 8;

  Monitor::event_raw() << "stack[" << State->BBStateMap[BB]->QState->SP_offset << "] ~ Reg " << Dn << ", ignored under assumption of function prologue\n";

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVSTMDIA_UPD(ARMRaiserState* State, MachineInstr* MI) { // 3765 | VSTM.F64 Rt! {Rwb} CC CPSR Dn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Dn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(Rt != ARM::SP && "VSTMDIA_UPD not yet implemented for SP registers");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getDoublePtrTy(State->Context), "VSTMDIA_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(DnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  Monitor::event_raw() << "VSTMDIA_UPD: incrementing Rt by 8\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Add, RtVal, ConstantInt::get(Type::getInt32Ty(State->Context), 8), "VSTMDIA_UPD", BB);
  Monitor::event_Instruction(Add);

  State->setReg(Rt, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVSTRD(ARMRaiserState* State, MachineInstr* MI) { // 3770 | VSTR.F64 Dd Rn Imm/4 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  uint64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = State->getReg(Dd, Type::getDoubleTy(State->Context), BB);
  Value* Ptr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) {
    Ptr = getOrCreateStackAlloca(State, Rn, Imm*4, Type::getDoubleTy(State->Context), BB);
  } else {
    Ptr = State->getReg(Rn, Type::getDoublePtrTy(State->Context), BB);
    if (Imm != 0) {
      Instruction* GEP = GetElementPtrInst::Create(Type::getDoubleTy(State->Context), Ptr, ConstantInt::get(Type::getInt32Ty(State->Context), Imm*4, false), "VSTRDGEP", BB);
      Monitor::event_Instruction(GEP);
      Ptr = GEP;
    }
  }

  Instruction* Store = new StoreInst(DdVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseVSUBD(ARMRaiserState* State, MachineInstr* MI) { // 3791 | VSUB.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = State->getReg(Dn, Type::getDoubleTy(State->Context), BB);
  Value* DmVal = State->getReg(Dm, Type::getDoubleTy(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FSub, DnVal, DmVal, "VSUBD", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
