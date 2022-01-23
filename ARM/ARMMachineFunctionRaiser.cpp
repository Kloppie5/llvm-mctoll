#include "MachineFunctionRaiser.h"

#include "ARMArgumentRaiser.h"
#include "ARMCreateJumpTable.h"
#include "ARMEliminatePrologEpilog.h"
#include "ARMFrameBuilder.h"
#include "ARMFunctionPrototype.h"
#include "ARMInstructionSplitting.h"
#include "ARMMIRevising.h"
#include "ARMModuleRaiser.h"
#include "ARMPatternMatchConcurrencyPass.h"
#include "ARMSelectionDAGISel.h"
#include "Monitor.h"

using namespace llvm;

class ARMMachineFunctionRaiser : MachineFunctionRaiser {
public:
  ARMMachineFunctionRaiser(ModuleRaiser &MR, MachineFunction &MF, MCInstRaiser *MCIR)
  : MachineFunctionRaiser(MR, MF, MCIR) {}

  bool raise( ) {
    Monitor::NOTE("ARMMachineFunctionRaiser::run");
    /**
    ARMPatternMatchConcurrencyPass::run(MF);

    ARMMIRevising mir(rmr);
    mir.init(&MF, raisedFunction);
    mir.setMCInstRaiser(mcInstRaiser);
    mir.revise();

    // Erasure of instructions
    ARMEliminatePrologEpilog epe(rmr);
    epe.init(&MF, raisedFunction);
    epe.eliminate();

    ARMCreateJumpTable cjt(rmr);
    cjt.init(&MF, raisedFunction);
    cjt.setMCInstRaiser(mcInstRaiser);
    cjt.create();
    std::vector<JumpTableInfo> jtList;
    cjt.getJTlist(jtList);

    ARMArgumentRaiser ar(rmr);
    ar.init(&MF, raisedFunction);
    ar.raiseArgs();

    ARMFrameBuilder fb(rmr);
    fb.init(&MF, raisedFunction);
    fb.build();

    // Splits of Operand2
    ARMInstructionSplitting ispl(rmr);
    ispl.init(&MF, raisedFunction);
    ispl.split();

    ARMSelectionDAGISel sdis(rmr);
    sdis.init(&MF, raisedFunction);
    sdis.setjtList(jtList);
    sdis.doSelection();
    */
    return true;
  }

  FunctionType *getRaisedFunctionPrototype() {
    ARMFunctionPrototype AFP;
    raisedFunction = AFP.discover(MF);

    Function *ori = const_cast<Function *>(&MF.getFunction());
    // Insert the map of raised function to tempFunctionPointer.
    const_cast<ModuleRaiser *>(MR)->insertPlaceholderRaisedFunctionMap(
        raisedFunction, ori);

    return raisedFunction->getFunctionType();
  }
};
