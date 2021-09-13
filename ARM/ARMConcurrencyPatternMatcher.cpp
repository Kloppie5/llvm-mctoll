
#include "ARMConcurrencyPatternMatcher.h"
#include "Monitor.h"

#include "ARMBaseInstrInfo.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

bool ARMConcurrencyPatternMatcher::run(Function *F) {
  Monitor::event_start("ARMConcurrencyPatternMatcher");

  std::vector<Instruction *> Worklist;

  for (BasicBlock &BB : *F)
    for (Instruction &I : BB)
      if (CallInst *CI = dyn_cast<CallInst>(&I))
        if (CI->getCalledFunction()->getName().equals("llvm.arm.ldrex.p0i32"))
          Worklist.push_back(CI);

  Monitor::event_raw() << "Worklist size: " << Worklist.size() << "\n";
  
  for (Instruction *I : Worklist) {
    Monitor::event_start("matchLDREXSTREXLoop");
    matchLDREXSTREXLoop(I);
    Monitor::event_end("matchLDREXSTREXLoop");
  }

  Monitor::event_end("ARMConcurrencyPatternMatcher");
  return true;
}



bool ARMConcurrencyPatternMatcher::matchLDREXSTREXLoop(Instruction *Instr) {
  BasicBlock *BBLoop = Instr->getParent();
  auto I = BBLoop->begin();
  // [[bb.loop]]:
  //   [[loadpointer]] = inttoptr i32 [[address]] to i32*
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  IntToPtrInst *loadpointer = dyn_cast<IntToPtrInst>(I++);
  if (!loadpointer) return false;
  // > [[ldrexcall]] = call i32 @llvm.arm.ldrex.p0i32(i32* [[loadpointer]])
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CallInst *ldrexcall = dyn_cast<CallInst>(I++);
  if (!ldrexcall) return false;
  //   [[opresult]] = [[op]] i32 [[ldrexcall]], [[operand]]
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  BinaryOperator *opresult = dyn_cast<BinaryOperator>(I++);
  if (!opresult) return false;
  Instruction::BinaryOps op = opresult->getOpcode();
  Value *operand = opresult->getOperand(1);
  
  //   [[storepointer]] = inttoptr i32 [[opresult]] to i32*
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  IntToPtrInst *storepointer = dyn_cast<IntToPtrInst>(I++);
  if (!storepointer) return false;
  //   [[strexcall]] = call i32 @llvm.arm.strex.p0i32(i32 [[address]], i32* [[storepointer]], i32 [[ldrexcall]])
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CallInst *strexcall = dyn_cast<CallInst>(I++);
  if (!strexcall) return false;

  //   [[cmpN]] = icmp slt i32 [[strexcall]], 0
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CmpInst *cmpN = dyn_cast<CmpInst>(I++);
  if (!cmpN) return false;
  //   store i1 [[cmpN]], i1* %N_flag, align 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  StoreInst *storeN = dyn_cast<StoreInst>(I++);
  if (!storeN) return false;
  //   [[cmpZ]] = icmp eq i32 [[strexcall]], 0
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CmpInst *cmpZ = dyn_cast<CmpInst>(I++);
  if (!cmpZ) return false;
  //   store i1 [[cmpZ]], i1* %Z_flag, align 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  StoreInst *storeZ = dyn_cast<StoreInst>(I++);
  if (!storeZ) return false;
  //   [[usub]] = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 [[strexcall]], i32 0)
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CallInst *usub = dyn_cast<CallInst>(I++);
  if (!usub) return false;
  //   [[cmpZ]] = extractvalue { i32, i1 } [[usub]], 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  ExtractValueInst *cmpC = dyn_cast<ExtractValueInst>(I++);
  if (!cmpC) return false;
  //   store i1 [[cmpC]], i1* %C_flag, align 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  StoreInst *storeC = dyn_cast<StoreInst>(I++);
  if (!storeC) return false;
  //   [[ssub]] = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 [[strexcall]], i32 0)
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CallInst *ssub = dyn_cast<CallInst>(I++);
  if (!ssub) return false;
  //   [[cmpV]] = extractvalue { i32, i1 } [[ssub]], 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  ExtractValueInst *cmpV = dyn_cast<ExtractValueInst>(I++);
  if (!cmpV) return false;
  //   store i1 [[cmpV]], i1* %V_flag, align 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  StoreInst *storeV = dyn_cast<StoreInst>(I++);
  if (!storeV) return false;

  //   [[Z]] = load i1, i1* %Z_flag, align 1
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  LoadInst *Z = dyn_cast<LoadInst>(I++);
  if (!Z) return false;
  //   [[ZC]] = icmp eq i1 [[Z]], false
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  CmpInst *ZC = dyn_cast<CmpInst>(I++);
  if (!ZC) return false;
  //   br i1 [[ZC]], label [[bb.loop]], label [[bb.continue]]
  Monitor::event_Instruction(dyn_cast<Instruction>(I));
  BranchInst *brZC = dyn_cast<BranchInst>(I++);
  if (!brZC) return false;
  BasicBlock *BBLoopContinue = brZC->getSuccessor(1);

  if (I != BBLoop->end()) return false;

  Monitor::event_stateswitch();

  // [[bb.continue]]:
  //   <use> [[ldrexcall]]

  // =>

  // [[ldrexcall]] = atomicrmw [[op]] i32* [[loadpointer]], i32 [[operand]] seq_cst
  AtomicRMWInst::BinOp atomicop;
  switch (op) {
    case Instruction::Add: atomicop = AtomicRMWInst::Add; break;
    case Instruction::Sub: atomicop = AtomicRMWInst::Sub; break;
    case Instruction::And: atomicop = AtomicRMWInst::And; break;
    case Instruction::Or:  atomicop = AtomicRMWInst::Or;  break;
    case Instruction::Xor: atomicop = AtomicRMWInst::Xor; break;
    default: assert(false && "Unsupported opcode");
  }

  AtomicRMWInst *atomicrmw = new AtomicRMWInst(
    atomicop, loadpointer, operand,
    Align(32), AtomicOrdering::SequentiallyConsistent, SyncScope::System,
    BBLoop);
  Monitor::event_Instruction(atomicrmw);
  BranchInst *br = BranchInst::Create(BBLoopContinue, BBLoop);
  Monitor::event_Instruction(br);
  
  ldrexcall->replaceAllUsesWith(atomicrmw);

  // Delete all* old instructions
  brZC->eraseFromParent();
  ZC->eraseFromParent();
  Z->eraseFromParent();
  storeV->eraseFromParent();
  cmpV->eraseFromParent();
  ssub->eraseFromParent();
  storeC->eraseFromParent();
  cmpC->eraseFromParent();
  usub->eraseFromParent();
  storeZ->eraseFromParent();
  cmpZ->eraseFromParent();
  storeN->eraseFromParent();
  cmpN->eraseFromParent();
  strexcall->eraseFromParent();
  storepointer->eraseFromParent();
  opresult->eraseFromParent();
  ldrexcall->eraseFromParent();

  return true;
}

#undef DEBUG_TYPE
