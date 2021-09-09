
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H

#include "RaiserPass.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

/// This is responsible for constructing DAG, and does instruction selection on
/// the DAG, eventually emits SDNodes of the DAG to LLVM IRs.
class ARMLinearRaiserPass : public RaiserPass {
public:
  MachineFunction *MF;
  Function *F;

  DenseMap<MachineBasicBlock *, BasicBlock *> MBBBBMap;
  
  DenseMap<Register, Value*> RegValueMap;

  int stackOffset = 0;
  DenseMap<int, Value*> StackValueMap;
  
  // NZCV
  SmallVector<Value*, 4> Flags;

  ARMLinearRaiserPass(ModuleRaiser &MR, std::vector<JumpTableInfo> &List, MCInstRaiser *MCIR)
   : RaiserPass(MR), jtList(List), MCIR(MCIR) {}

  bool run (MachineFunction *MF, Function *F) override;
  
  Value *ARMCCToValue(int Cond, BasicBlock *BB);
  BasicBlock *getBasicBlock(MachineBasicBlock *MBB);
  bool raiseMachineInstr(MachineInstr *MI);

private:
  std::vector<JumpTableInfo> jtList;
  MCInstRaiser *MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
