
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISELBYPASSPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISELBYPASSPASS_H

#include "RaiserPass.h"
#include "FunctionRaisingInfo.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

/// This is responsible for constructing DAG, and does instruction selection on
/// the DAG, eventually emits SDNodes of the DAG to LLVM IRs.
class ARMSelectionDAGISelBypassPass : public RaiserPass {
public:
  ARMSelectionDAGISelBypassPass(ModuleRaiser &MR, std::vector<JumpTableInfo> &List, MCInstRaiser *MCIR) : RaiserPass(MR), jtList(List), MCIR(MCIR) {}

  bool precondition(MachineFunction *MF, Function *F) override;
  bool run (MachineFunction *MF, Function *F) override;

  Value *getOperandValue(MachineInstr *MI, int OpIdx, Type *Ty = nullptr);
  void setOperandValue(MachineInstr *MI, int OpIdx, Value *v);
  bool raiseMachineInstr(BasicBlock *BB, MachineInstr *MI);

private:
  FunctionRaisingInfo *FuncInfo;
  std::vector<JumpTableInfo> jtList;
  MCInstRaiser *MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISELBYPASSPASS_H
