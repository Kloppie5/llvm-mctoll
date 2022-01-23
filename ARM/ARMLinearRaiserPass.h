
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H

#include "RaiserPass.h"
#include "ARMBasicBlockState.h"
#include "ARMRaiserState.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

class ARMLinearRaiserPass : public RaiserPass {
public:
  ARMRaiserState* State;

  ARMLinearRaiserPass(ModuleRaiser &MR, MCInstRaiser* MCIR)
   : RaiserPass(MR), MCIR(MCIR) {
    State = new ARMRaiserState(MR, MCIR);
   }

  bool run (MachineFunction* MF, Function* F) override;

  bool raiseMachineInstr(MachineInstr* MI);

private:
  MCInstRaiser* MCIR;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
