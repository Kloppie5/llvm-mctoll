#ifndef LLVM_MCTOLL_RAISERPASS_H_
#define LLVM_MCTOLL_RAISERPASS_H_

#include "ModuleRaiser.h"

class RaiserPass {
public:
  ModuleRaiser &MR;
  std::string Name;

  RaiserPass(ModuleRaiser &MR, std::string Name) : MR(MR), Name(Name) {}

  bool precondition() { return true; }
  bool postcondition() { return true; }
  bool run() { return true; }
  bool runOnSingleFunction(MachineFunction *MF, Function *F) { return true; }
};

#endif // LLVM_MCTOLL_MONITOR_H_