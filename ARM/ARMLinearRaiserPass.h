
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H

#include "RaiserPass.h"
#include "ARMBasicBlockState.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

class ARMLinearRaiserPass : public RaiserPass {
public:
  MachineFunction* MF;
  Function* F;

  std::map<MachineBasicBlock* , std::vector<BasicBlock* >> MBBBBMap;

  ARMLinearRaiserPass(ModuleRaiser &MR, MCInstRaiser* MCIR)
   : RaiserPass(MR), MCIR(MCIR) {}

  bool run (MachineFunction* MF, Function* F) override;

  std::map<BasicBlock* , ARMBasicBlockState* > BBStateMap;

  Instruction* stack_insertion_point;
  std::map<int64_t, Value*> stack_map;
  Value* getStackValue(Register Reg, int64_t offset, Type* Ty, BasicBlock* BB);
  Value* getStackValue(int64_t offset, Type* Ty, BasicBlock* BB);
  Value* getOrCreateStackValue(Register Reg, int64_t offset, Type* Ty, BasicBlock* BB);
  Value* getOrCreateStackValue(int64_t offset, Type* Ty, BasicBlock* BB);
  void setStackValue(Register Reg, int64_t offset, Value* V, BasicBlock* BB);
  void setStackValue(int64_t offset, Value* V, BasicBlock* BB);

  Value* getRegValue(Register Reg, Type* Ty, BasicBlock* BB);
  void setRegValue(Register Reg, Value* V, BasicBlock* BB);

  Value* resolveAM2Shift(Register Rn, Register Rs, Register Rm, int64_t AM2Shift, BasicBlock* BB);

  GlobalValue* getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset);
  Value* ARMCCToValue(int Cond, BasicBlock* BB);

  BasicBlock* createBasicBlock(MachineBasicBlock* MBB);
  std::vector<BasicBlock* > getBasicBlocks(MachineBasicBlock* MBB);

  bool raiseMachineInstr(MachineInstr* MI);

private:
  MCInstRaiser* MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
