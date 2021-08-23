#ifndef LLVM_MCTOLL_RAISERPASS_H_
#define LLVM_MCTOLL_RAISERPASS_H_

#include "ModuleRaiser.h"

class RaiserPass {
  public:
    std::string Name;
    RaiserPass(ModuleRaiser &MR, std::string Name) {}
    bool precondition () { return true; }
    bool postcondition () { return true; }
    bool run() { return true; }
};

#endif // LLVM_MCTOLL_MONITOR_H_