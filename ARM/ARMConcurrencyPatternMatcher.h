
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMCONCURRENCYPATTERNMATCHER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMCONCURRENCYPATTERNMATCHER_H

#include "RaiserPass.h"

using namespace llvm;

class ARMConcurrencyPatternMatcher : public RaiserPass {
public:
  ARMConcurrencyPatternMatcher(ModuleRaiser &MR) : RaiserPass(MR) {};

  bool run (Function *F) override;

  bool matchLDREXSTREXLoop(Instruction *Instr);
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMCONCURRENCYPATTERNMATCHER_H
