
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBENCHMARKER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBENCHMARKER_H

#include "RaiserPass.h"

using namespace llvm;

/// Some instructions which their patterns include more than one operations,
/// like 'add r0, r1, r0, asr r1' or 'ldr r0, [r1, #4]', are splitted into
/// multiple MIs at here.
class ARMBenchmarker : public RaiserPass {
public:
  ARMBenchmarker(ModuleRaiser &MR) : RaiserPass(MR) {};

  bool run (MachineFunction *MF, Function *F) override;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBENCHMARKER_H
