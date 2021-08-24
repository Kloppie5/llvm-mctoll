
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISELBYPASSPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISELBYPASSPASS_H

#include "RaiserPass.h"
#include "ModuleRaiser.h"

/// This is responsible for constructing DAG, and does instruction selection on
/// the DAG, eventually emits SDNodes of the DAG to LLVM IRs.
class ARMSelectionDAGISelBypassPass : public RaiserPass {
public:
  ARMSelectionDAGISelBypassPass(ModuleRaiser &MR, std::vector<JumpTableInfo> &List) : RaiserPass(MR), jtList(List) {}

  bool precondition(MachineFunction *MF, Function *F) override;
  bool run (MachineFunction *MF, Function *F) override;

private:
  std::vector<JumpTableInfo> jtList;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMSELECTIONDAGISELBYPASSPASS_H
