
#include "ARMConditionalExecutionPass.h"

#include "FunctionRaisingInfo.h"
#include "Monitor.h"

#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "llvm/IR/Instruction.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMConditionalExecutionPass::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMConditionalExecutionPass");
  LLVM_DEBUG(dbgs() << "ARMConditionalExecutionPass start.\n");
  
  
  LLVM_DEBUG(MF->dump());

  std::vector<MachineInstr *> worklist;
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      if (MI.getOpcode() == ARM::Bcc)
        continue;
      
      if (hasConditionalExecution(&MI))
        worklist.push_back(&MI);
    }
  }

  // splitConditionalExecution aggressively alters the CFG, so we need to
  // use a worklist to avoid iterator invalidation.
  // Using a worklist also simplifies the implementation of the function.
  for (MachineInstr *MI : worklist)
    splitConditionalExecution(MI);

  if (!postcondition(MF, F))
    return false;

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMConditionalExecutionPass end.\n");
  Monitor::event_end("ARMConditionalExecutionPass");
  return true;
}
bool ARMConditionalExecutionPass::postcondition(MachineFunction *MF, Function *F) {
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      if (MI.getOpcode() == ARM::Bcc)
        continue;
    
      if (!hasConditionalExecution(&MI))
        continue;
      
      Monitor::ERROR("Unexpected conditional instruction remaining\n");
      Monitor::event_MachineInstr(&MI);
      assert(false);
      return false;
    }
  }

  return true;
}

bool ARMConditionalExecutionPass::hasConditionalExecution(MachineInstr *MI) {
  int idx = MI->findFirstPredOperandIdx();
  if (idx == -1)
    return false;
  
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(idx).getImm();
  if (CC == ARMCC::AL)
    return false;
  
  return true;
}

void ARMConditionalExecutionPass::splitConditionalExecution(MachineInstr *MI) {
  Monitor::event_start("ARMConditionalExecutionPass::splitConditionalExecution");
  Monitor::event_MachineInstr(MI);
  Monitor::event_stateswitch();

  MachineBasicBlock *MBB = MI->getParent();

  int idx = MI->findFirstPredOperandIdx();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(idx).getImm();    
  ARMCC::CondCodes oCC = ARMCC::getOppositeCondition(CC);

  const ARMSubtarget &STI = MI->getParent()->getParent()->getSubtarget<ARMSubtarget>();
  const ARMBaseInstrInfo *TII = STI.getInstrInfo();

  // Create a conditional branch that skips the instruction
  MachineInstr *Branch = BuildMI(*MBB, MI, MI->getDebugLoc(),TII->get(ARM::Bcc))
    .addImm(/*0*/ 8)
    .addImm(oCC).addReg(ARM::CPSR)
    .getInstr();
    
  // Update the conditional execution
  MI->getOperand(idx).setImm(ARMCC::AL);
  MI->getOperand(idx+1).setReg(0);
  
  Monitor::event_MachineInstr(Branch);
  Monitor::event_MachineInstr(MI);
  Monitor::event_end("ARMConditionalExecutionPass::splitConditionalExecution");
}

#undef DEBUG_TYPE
