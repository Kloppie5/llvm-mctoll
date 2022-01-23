
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBENCHMARKER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBENCHMARKER_H

#include "RaiserPass.h"

using namespace llvm;

class ARMBenchmarker : public RaiserPass {
public:
  ARMBenchmarker(ModuleRaiser &MR) : RaiserPass(MR) {};

  bool run (MachineFunction *MF, Function *F) override;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBENCHMARKER_H
