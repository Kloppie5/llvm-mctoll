
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

  DenseMap<MachineBasicBlock *, std::vector<BasicBlock *>> MBBBBMap;
  
  DenseMap<Register, Value*> RegValueMap;

  int64_t stack_offset;
  std::map<int64_t, Value*> stack_map;
  
  // NZCV
  SmallVector<Value*, 4> Flags;

  ARMLinearRaiserPass(ModuleRaiser &MR, MCInstRaiser *MCIR)
   : RaiserPass(MR), MCIR(MCIR) {}

  bool run (MachineFunction *MF, Function *F) override;
  
  GlobalValue *getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset);
  Value *ARMCCToValue(int Cond, BasicBlock *BB);
  std::vector<BasicBlock *> getBasicBlocks(MachineBasicBlock *MBB);
  bool raiseMachineInstr(MachineInstr *MI);

private:
  MCInstRaiser *MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
