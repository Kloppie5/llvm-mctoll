
#include "ARMPredicateCleanupPass.h"
#include "Monitor.h"

#include "ARMSubtarget.h"

bool ARMPredicateCleanupPass::run(MachineFunction *MF, Function *F) {
  Monitor::NOTE("ARMPredicateCleanupPass; started\n");
  bool changed = false;
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      const MCInstrDesc &MCID = MI.getDesc();
      if (!MCID.isPredicable()) continue;
      
      int idx = MI.findFirstPredOperandIdx();
      Register PredReg = MI.getOperand(idx+1).getReg();
      int Cond = MI.getOperand(idx).getImm();
      if (!(Cond == ARMCC::AL && PredReg == 0)) continue;

      changed = true;
      const ARMBaseInstrInfo *TII = MF->getSubtarget<ARMSubtarget>().getInstrInfo();
      MachineInstr &OldMI = TII->duplicate(MBB, MI, MI);
      MI.RemoveOperand(idx+1);
      MI.RemoveOperand(idx);
      Monitor::event_MachineInstrsToMachineInstrs("Predicate Cleanup", {&OldMI}, {&MI});
      OldMI.eraseFromParent();
    }
  }
  
  if (!postcondition(MF, F)) {
    Monitor::ERROR("ARMPredicateCleanupPass; postcondition failed\n");
    return false;
  }
  if (changed) {
    Monitor::NOTE("ARMPredicateCleanupPass; changed\n");
    return true;
  }
  Monitor::NOTE("ARMPredicateCleanupPass; no change\n");
  return false;
}
bool ARMPredicateCleanupPass::postcondition(MachineFunction *MF, Function *F) {
  return true;
}



