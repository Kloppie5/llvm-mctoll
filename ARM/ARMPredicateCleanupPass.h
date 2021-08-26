
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMPREDICATECLEANUPPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMPREDICATECLEANUPPASS_H

#include "RaiserPass.h"

using namespace llvm;

class ARMPredicateCleanupPass : public RaiserPass {
public:
  ARMPredicateCleanupPass(ModuleRaiser &MR) : RaiserPass(MR) {};
  
  bool run (MachineFunction *MF, Function *F) override;
  bool postcondition(MachineFunction *MF, Function *F) override;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMPREDICATECLEANUPPASS_H
