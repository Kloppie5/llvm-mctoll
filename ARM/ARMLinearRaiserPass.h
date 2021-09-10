
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H

#include "RaiserPass.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

class ARMLinearRaiserPass : public RaiserPass {
public:
  MachineFunction *MF;
  Function *F;

  DenseMap<MachineBasicBlock *, BasicBlock *> MBBBBMap;
  
  DenseMap<Register, Value*> RegValueMap;
  
  // NZCV
  SmallVector<Value*, 4> Flags;

  ARMLinearRaiserPass(ModuleRaiser &MR, MCInstRaiser *MCIR)
   : RaiserPass(MR), MCIR(MCIR) {}

  bool run (MachineFunction *MF, Function *F) override;
  
  GlobalValue *getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset);
  Value *ARMCCToValue(int Cond, BasicBlock *BB);
  BasicBlock *getBasicBlock(MachineBasicBlock *MBB);
  bool raiseMachineInstr(MachineInstr *MI);

private:
  MCInstRaiser *MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
