
#include "ARMFocusBranchesPass.h"
#include "Monitor.h"

#include "FunctionRaisingInfo.h"

#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "llvm/IR/Instruction.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMFocusBranchesPass::precondition(MachineFunction *MF, Function *F) {
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      switch(MI.getOpcode()) {
        default:
          OS << "ARMFocusBranchesPass encountered unhandled opcode in instruction ";
          Monitor::printMachineInstr(&MI, true, OS);
          assert(false);
          return false;
        
        case ARM::ASRi: // 247
        case ARM::ASRr: // 248
        case ARM::B: // 249
        case ARM::LSLi: // 292
        case ARM::LSLr: // 293
        case ARM::LSRi: // 294
        case ARM::LSRr: // 295
        case ARM::RORi: // 325
        case ARM::RORr: // 326
        case ARM::RRX: // 327
        case ARM::ADCri: // 680
        case ARM::ADCrsi: // 682
        case ARM::ADDri: // 684
        case ARM::ADDrr: // 685
        case ARM::ADDrsi: // 686
        case ARM::ANDri: // 693
        case ARM::BICri: // 706
        case ARM::BL: // 711
        case ARM::BX_RET: // 718
        case ARM::Bcc: // 720
        case ARM::CMNri: // 755
        case ARM::CMPri: // 759
        case ARM::CMPrr: // 760
        case ARM::DMB: // 773
        case ARM::DSB: // 774
        case ARM::EORri: // 775
        case ARM::EORrr: // 776
        case ARM::ISB: // 793
        case ARM::LDAEXB: // 797
        case ARM::LDMIA: // 821
        case ARM::LDMIA_UPD: // 822
        case ARM::LDRBi12: // 831
        case ARM::LDRBrs: // 832
        case ARM::LDREXB: // 837
        case ARM::LDRH: // 840
        case ARM::LDRSB: // 845
        case ARM::LDRSB_PRE: // 849
        case ARM::LDR_PRE_IMM: // 859
        case ARM::LDR_PRE_REG: // 860
        case ARM::LDRi12: // 862
        case ARM::LDRrs: // 863
        case ARM::MLA: // 868
        case ARM::MOVPCLR: // 870
        case ARM::MOVTi16: // 871
        case ARM::MOVi: // 872
        case ARM::MOVi16: // 873
        case ARM::MOVr: // 874
        case ARM::MOVsi: // 876
        case ARM::MUL: // 888
        case ARM::MVNi: // 1736
        case ARM::MVNr: // 1737
        case ARM::ORRri: // 1748
        case ARM::ORRrr: // 1749
        case ARM::SBCri: // 1794
        case ARM::SBCrr: // 1795
        case ARM::SBCrsi: // 1796
        case ARM::SMLAL: // 1824
        case ARM::STLEXB: // 1888
        case ARM::STMDB_UPD: // 1895
        case ARM::STMIA: // 1896
        case ARM::STMIA_UPD: // 1897
        case ARM::STMIB: // 1898
        case ARM::STRBi12: // 1906
        case ARM::STRBrs: // 1907
        case ARM::STREXB: // 1912
        case ARM::STRH: // 1915
        case ARM::STRi12: // 1926
        case ARM::SUBri: // 1928
        case ARM::SUBrr: // 1929
        case ARM::SVC: // 1932
          break;
      }
    }
  }

  return true;
}

/**
 * The ARMFocusBranchesPass is a pass that makes all branch instructions
 * target MachineBasicBlocks instead of using pc-relative addresses.
 **/
bool ARMFocusBranchesPass::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMFocusBranchesPass");
  LLVM_DEBUG(dbgs() << "ARMFocusBranchesPass start.\n");
  
  if (!precondition(MF, F))
    return false;

  std::vector<MachineInstr *> worklist;
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      switch(MI.getOpcode()) {
        case ARM::Bcc:
            worklist.push_back(&MI);
            break;
        default:
            break;
      }
    }
  }

  for (MachineInstr *MI : worklist)
    focusBranch(MI);

  if (!postcondition(MF, F))
    return false;

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMFocusBranchesPass end.\n");
  Monitor::event_end("ARMFocusBranchesPass");
  return true;
}
bool ARMFocusBranchesPass::postcondition(MachineFunction *MF, Function *F) {
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      switch(MI.getOpcode()) {
        case ARM::Bcc:
        if (MI.getOperand(0).isReg()) {

    }
  }

  return true;
}
bool ARMFocusBranchesPass::focusBranch(MachineInstr *MI) {
  Monitor::event_start("ARMFocusBranchesPass::focusBranch");
  Monitor::event_MachineInstr(MI);
  Monitor::event_stateswitch();

  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser&>(MR);

  MachineBasicBlock *MBB = MI->getParent();
  MachineFunction *MF = MBB->getParent();
  Function *F = MF->getFunction();
  ARMSubtarget *Subtarget = MF->getSubtarget<ARMSubtarget>();
  const ARMBaseInstrInfo *TII = Subtarget->getInstrInfo();
  const TargetRegisterInfo *TRI = Subtarget->getRegisterInfo();
  const TargetInstrInfo *TII = Subtarget->getInstrInfo();
  
  if (!MO.isImm())
    assert(false && "Branch target is not an immediate!");

  int64_t relativeAddress = MI->getOperand(0).getImm();
  int64_t absoluteAddress =
    getMCInstIndex(MInst)
    + AMR.getTextSectionAddress()
    + relativeAddress
    + 8;

  Function *CalledFunc = AMR.getRaisedFunctionAt(absoluteAddress);
  Monitor::event_raw() << "Direct call target: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  if (!CalledFunc) {
    CalledFunc = AMR.getCalledFunctionUsingTextReloc(getMCInstIndex(MInst), 4);
    Monitor::event_raw() << "Call target using text reloc: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  }
  if (!CalledFunc) {
    CalledFunc = getCalledFunctionAtPLTOffset(absoluteAddress);
    Monitor::event_raw() << "Call target using PLT: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  }
  AMR
  MInst.getOperand(0).setImm(absoluteAddress);

  } else {
    uint64_t Offset = getMCInstIndex(MInst);
    Monitor::event_raw() << "Offset: " << Offset << "\n";
    const RelocationRef *reloc = AMR.getTextRelocAtOffset(Offset, 4);
    auto ImmValOrErr = (*reloc->getSymbol()).getValue();
    assert(ImmValOrErr && "Failed to get immediate value");
    Monitor::event_raw() << "Relocated: " << *ImmValOrErr << "\n";
    MInst.getOperand(0).setImm(*ImmValOrErr);
  }

  Monitor::event_MachineInstr(&MInst);
  Monitor::event_end("ARMFocusBranchesPass::focusBranch");

}

#undef DEBUG_TYPE
