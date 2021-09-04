
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_FOCUSBRANCHESPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_FOCUSBRANCHESPASS_H

#include "RaiserPass.h"
#include "FunctionRaisingInfo.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

/// This is responsible for constructing DAG, and does instruction selection on
/// the DAG, eventually emits SDNodes of the DAG to LLVM IRs.
class ARMFocusBranchesPass : public RaiserPass {
public:
 ARMFocusBranchesPass(ModuleRaiser &MR) : RaiserPass(MR) {}

  bool precondition(MachineFunction *MF, Function *F) override;
  bool run (MachineFunction *MF, Function *F) override;
  bool postcondition(MachineFunction *MF, Function *F) override;

  bool focusBranch(MachineInstr *MI);

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_FOCUSBRANCHESPASS_H
