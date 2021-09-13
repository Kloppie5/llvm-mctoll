
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

  // NZCV
  SmallVector<Value*, 4> Flags;

  ARMLinearRaiserPass(ModuleRaiser &MR, MCInstRaiser *MCIR)
   : RaiserPass(MR), MCIR(MCIR) {}

  bool run (MachineFunction *MF, Function *F) override;
  
  DenseMap<Register, Value*> RegValueMap;
  Value *getRegValue(Register Reg, BasicBlock *BB);
  void setRegValue(Register Reg, Value *V, BasicBlock *BB);

  int64_t stack_offset;
  std::map<int64_t, Value*> stack_map;
  Value *getStackValue(Register Reg, int64_t offset, BasicBlock *BB);
  Value *getStackValue(int64_t offset, BasicBlock *BB);
  void setStackValue(Register Reg, int64_t offset, Value *V, BasicBlock *BB);
  void setStackValue(int64_t offset, Value *V, BasicBlock *BB);
  
  Value *toPtr(Value *Addr, Type *Ty, BasicBlock *BB);
  Value *resolveAM2Shift(Register Rn, Register Rm, int64_t AM2Shift, BasicBlock *BB);

  GlobalValue *getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset);
  Value *ARMCCToValue(int Cond, BasicBlock *BB);
  std::vector<BasicBlock *> getBasicBlocks(MachineBasicBlock *MBB);
  bool raiseMachineInstr(MachineInstr *MI);

  void raiseBINOPrr(MachineInstr *MI, Instruction::BinaryOps Opcode);

private:
  MCInstRaiser *MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
