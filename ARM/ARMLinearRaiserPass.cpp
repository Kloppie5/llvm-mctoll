
#include "ARMLinearRaiserPass.h"

#include "Monitor.h"

#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "IncludedFileInfo.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"

#include "ARMRaiser/ARMRaiser.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMLinearRaiserPass::run(MachineFunction* MF, Function* F) {
  Monitor::event_start("ARMLinearRaiserPass");
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass start.\n");

  State->setup(MF, F);

  for (MachineBasicBlock &MBB : *MF)
    for (MachineInstr &MI : MBB)
      raiseMachineInstr(&MI);

  for (MachineBasicBlock &MBB : *MF) {
    if (MBB.succ_size() == 1 && MBB.terminators().empty()) { // For fallthrough blocks
      BasicBlock* BB = State->MBBBBMap[&MBB].back();
      Monitor::event_raw() << "LinearRaiser: add terminator to " << BB->getName() << "\n";
      auto succ_itt = MBB.succ_begin();
      BasicBlock* BranchBB = State->getBasicBlocks(*succ_itt).front();
      Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
      Instruction* Instr = BranchInst::Create(BranchBB, BB);
      Monitor::event_Instruction(Instr);
    }
  }

  // Remove instructions after terminating call unreachables
  for (BasicBlock &BB : *F) {
    bool del = false;
    std::vector<Instruction*> rem;
    for (auto it = BB.begin(); it != BB.end(); ++it) {
      if (del)
        rem.push_back(&*it);
      if ((&*it)->getOpcode() == Instruction::Unreachable)
        del = true; // del updated AFTER to not remove the terminator
    }

    if (del) {
      Monitor::event_raw() << "LinearRaiser: delete " << rem.size() << " instructions from BB " << BB.getName() << "\n";
      for (Instruction* I : rem) {
        Monitor::event_Instruction(I);
        I->eraseFromParent();
      }
    }
  }

  // Propagate PHINodes
  std::vector<BasicBlock*> PHIWorklist;
  for (BasicBlock &BB : *F)
    PHIWorklist.push_back(&BB);

  while (!PHIWorklist.empty()) {
    BasicBlock* BB = PHIWorklist.back();
    PHIWorklist.pop_back();
    for (BasicBlock* PBB : predecessors(BB)) {
      bool changed = State->BBStateMap[BB]->updatePHINodes(PBB, State->BBStateMap[PBB]);
      if (changed) {
        PHIWorklist.push_back(PBB);
      }
    }
  }

  // Add returns to non terminated blocks | TODO: fix main function return type
  for ( BasicBlock &BB : *F ) {
    if (BB.getTerminator() == nullptr) {
      Monitor::event_raw() << "LinearRaiser: add return to " << BB.getName() << "\n";
      if (F->getReturnType() == Type::getVoidTy(Context))
        ReturnInst::Create(Context, &BB);
      else
        ReturnInst::Create(Context, State->getReg(ARM::R0, F->getReturnType(), &BB), &BB);
    }
  }

  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass end.\n");
  Monitor::event_end("ARMLinearRaiserPass");
  return true;
}

bool ARMLinearRaiserPass::raiseMachineInstr(MachineInstr* MI) {
  Monitor::event_start("ARMLinearRaiserPass::RaiseMachineInstr");
  Monitor::event_MachineInstr(MI);
  Monitor::event_stateswitch();

  switch (MI->getOpcode()) {
    default: {
      auto OS = WithColor(errs(), HighlightColor::Warning);
      OS << "ARMLinearRaiserPass encountered unhandled opcode in instruction ";
      Monitor::printMachineInstr(MI, true, OS);
      assert(false && "Unhandled opcode");
      return false;
    } break;
    case ARM::ADCri:         ARMRaiser::raiseADCri        (State, MI); break; //  680 | ADC Rd Rn Op2 CC CPSR S
    case ARM::ADDri:         ARMRaiser::raiseADDri        (State, MI); break; //  684 | ADD Rd Rn Op2 CC CPSR S
    case ARM::ADDrr:         ARMRaiser::raiseADDrr        (State, MI); break; //  685 | ADD Rd Rn Rm CC CPSR S
    case ARM::ADDrsi:        ARMRaiser::raiseADDrsi       (State, MI); break; //  686 | ADD Rd Rn Rm Shift CC CPSR S
    case ARM::ANDri:         ARMRaiser::raiseANDri        (State, MI); break; //  693 | AND Rd Rn Op2 CC CPSR S
    case ARM::ANDrsi:        ARMRaiser::raiseANDrsi       (State, MI); break; //  695 | AND Rd Rn Rm Shift CC CPSR S
    case ARM::ANDrr:         ARMRaiser::raiseANDrr        (State, MI); break; //  694 | AND Rd Rn Rm CC CPSR S
    case ARM::BFC:           ARMRaiser::raiseBFC          (State, MI); break; //  704 | BFC Rd {Rwb} Imm CC CPSR
    case ARM::BICri:         ARMRaiser::raiseBICri        (State, MI); break; //  706 | BIC Rd Rn Op2 CC CPSR S
    case ARM::BL:            ARMRaiser::raiseBL           (State, MI); break; //  711 | BL Imm
    case ARM::BL_pred:       ARMRaiser::raiseBL_pred      (State, MI); break; //  715 | BL Imm CC CPSR
    case ARM::BX_RET:        ARMRaiser::raiseBX_RET       (State, MI); break; //  718 | BX_RET CC CPSR
    case ARM::Bcc:           ARMRaiser::raiseBcc          (State, MI); break; //  720 | Bcc offset CC CPSR
    case ARM::CLZ:           ARMRaiser::raiseCLZ          (State, MI); break; //  754 | CLZ Rd Rm CC CPSR
    case ARM::CMNri:         ARMRaiser::raiseCMNri        (State, MI); break; //  755 | CMN Rn Op2 CC CPSR
    case ARM::CMPri:         ARMRaiser::raiseCMPri        (State, MI); break; //  759 | CMP Rn Op2 CC CPSR
    case ARM::CMPrr:         ARMRaiser::raiseCMPrr        (State, MI); break; //  760 | CMP Rn Rm CC CPSR
    case ARM::DMB:           ARMRaiser::raiseDMB          (State, MI); break; //  773 | DMB Imm
    case ARM::EORrr:         ARMRaiser::raiseEORrr        (State, MI); break; //  776 | EOR Rd Rn Rm CC CPSR S
    case ARM::FMSTAT:        ARMRaiser::raiseFMSTAT       (State, MI); break; //  786 | FMRX CC CPSR
    case ARM::FCONSTD:       ARMRaiser::raiseFCONSTD      (State, MI); break; //  780 | VMOV.F64 Dd Imm CC CPSR
    case ARM::FCONSTS:       ARMRaiser::raiseFCONSTS      (State, MI); break; //  782 | VMOV.F32 Sd Imm CC CPSR
    case ARM::HINT:          ARMRaiser::raiseHINT         (State, MI); break; //  790 | HINT Imm CC CPSR
    case ARM::LDMIA:         ARMRaiser::raiseLDMIA        (State, MI); break; //  821 | LDMIA Rt CC CPSR Rn
    case ARM::LDMIA_UPD:     ARMRaiser::raiseLDMIA_UPD    (State, MI); break; //  822 | LDMIA Rt! {Rwb} CC CPSR Rn
    case ARM::LDRB_PRE_REG:  ARMRaiser::raiseLDRB_PRE_REG (State, MI); break; //  830 | LDRB_PRE_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
    case ARM::LDRBi12:       ARMRaiser::raiseLDRBi12      (State, MI); break; //  831 | LDRB Rt Rn Imm12 CC CPSR
    case ARM::LDREX:         ARMRaiser::raiseLDREX        (State, MI); break; //  836 | LDREX Rt Rn CC CPSR
    case ARM::LDRH:          ARMRaiser::raiseLDRH         (State, MI); break; //  840 | LDRH Rt Rn AM3Reg AM3Imm CC CPSR
    case ARM::LDR_POST_IMM:  ARMRaiser::raiseLDR_POST_IMM (State, MI); break; //  857 | LDR_POST_IMM Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
    case ARM::LDR_POST_REG:  ARMRaiser::raiseLDR_POST_REG (State, MI); break; //  858 | LDR_POST_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
    case ARM::LDRi12:        ARMRaiser::raiseLDRi12       (State, MI); break; //  862 | LDR Rt Rn Imm12 CC CPSR
    case ARM::LDRrs:         ARMRaiser::raiseLDRrs        (State, MI); break; //  863 | LDR Rt Rn Rm AM2Shift CC CPSR
    case ARM::MLA:           ARMRaiser::raiseMLA          (State, MI); break; //  868 | MLA Rd Rn Rm Ra CC CPSR S
    case ARM::MOVTi16:       ARMRaiser::raiseMOVTi16      (State, MI); break; //  871 | MOV.t Rd Rd Imm16 CC CPSR
    case ARM::MOVi:          ARMRaiser::raiseMOVi         (State, MI); break; //  872 | MOV Rt Op2 CC CPSR S
    case ARM::MOVi16:        ARMRaiser::raiseMOVi16       (State, MI); break; //  873 | MOV Rd Imm16 CC CPSR
    case ARM::MOVr:          ARMRaiser::raiseMOVr         (State, MI); break; //  874 | MOV Rd Rn CC CPSR
    case ARM::MOVsi:         ARMRaiser::raiseMOVsi        (State, MI); break; //  876 | MOV Rd Rm Shift CC CPSR S
    case ARM::MUL:           ARMRaiser::raiseMUL          (State, MI); break; //  888 | MUL Rd Rn Rm CC CPSR S
    case ARM::MVNi:          ARMRaiser::raiseMVNi         (State, MI); break; // 1736 | MVN Rd Imm CC CPSR S
    case ARM::MVNr:          ARMRaiser::raiseMVNr         (State, MI); break; // 1737 | MVN Rd Rn CC CPSR S
    case ARM::ORRri:         ARMRaiser::raiseORRri        (State, MI); break; // 1748 | ORR Rd Rn Op2 CC CPSR S
    case ARM::ORRrr:         ARMRaiser::raiseORRrr        (State, MI); break; // 1749 | ORR Rd Rn Rm CC CPSR S
    case ARM::RSBri:         ARMRaiser::raiseRSBri        (State, MI); break; // 1782 | RSB Rd Rn Op2 CC CPSR S
    case ARM::SBCrsi:        ARMRaiser::raiseSBCrsi       (State, MI); break; // 1796 | SBC Rd Rn Rm Shift CC CPSR S
    case ARM::SMMUL:         ARMRaiser::raiseSMMUL        (State, MI); break; // 1843 | SMMUL Rd Rn Rm CC CPSR
    case ARM::SMULL:         ARMRaiser::raiseSMULL        (State, MI); break; // 1849 | SMULL RdLo RdHi Rn Rm CC CPSR S
    case ARM::STMDB_UPD:     ARMRaiser::raiseSTMDB_UPD    (State, MI); break; // 1895 | STMDB Rt! {Rwb} CC CPSR Rn
    case ARM::STMIA:         ARMRaiser::raiseSTMIA        (State, MI); break; // 1896 | STMIA Rt CC CPSR Rn
    case ARM::STMIB:         ARMRaiser::raiseSTMIB        (State, MI); break; // 1898 | STMIB Rt CC CPSR Rn
    case ARM::STRB_POST_IMM: ARMRaiser::raiseSTRB_POST_IMM(State, MI); break; // 1902 | STRB Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
    case ARM::STRBi12:       ARMRaiser::raiseSTRBi12      (State, MI); break; // 1906 | STRB Rt Rn Imm12 CC CPSR
    case ARM::STREX:         ARMRaiser::raiseSTREX        (State, MI); break; // 1911 | STREX Rd Rt Rn CC CPSR
    case ARM::STRH:          ARMRaiser::raiseSTRH         (State, MI); break; // 1915 | STRH Rt Rn AM3Reg AM3Imm CC CPSR
    case ARM::STRi12:        ARMRaiser::raiseSTRi12       (State, MI); break; // 1926 | STR Rt Rn Imm12 CC CPSR
    case ARM::STRrs:         ARMRaiser::raiseSTRrs        (State, MI); break; // 1927 | STR Rt Rn Rm AM2Shift CC CPSR
    case ARM::SUBri:         ARMRaiser::raiseSUBri        (State, MI); break; // 1928 | SUB Rd Rn Op2 CC CPSR S
    case ARM::SUBrr:         ARMRaiser::raiseSUBrr        (State, MI); break; // 1929 | SUB Rd Rn Rm CC CPSR S
    case ARM::SUBrsi:        ARMRaiser::raiseSUBrsi       (State, MI); break; // 1930 | SUB Rd Rn Rm Shift CC CPSR S
    case ARM::TEQrr:         ARMRaiser::raiseTEQrr        (State, MI); break; // 1942 | TEQ Rn Rm CC CPSR
    case ARM::TSTri:         ARMRaiser::raiseTSTri        (State, MI); break; // 1948 | TST Rn Imm CC CPSR
    case ARM::TSTrr:         ARMRaiser::raiseTSTrr        (State, MI); break; // 1949 | TST Rn Rm CC CPSR
    case ARM::VABSD:         ARMRaiser::raiseVABSD        (State, MI); break; // 2026 | VABS.F64 Dd Dm CC CPSR
    case ARM::VADDD:         ARMRaiser::raiseVADDD        (State, MI); break; // 2047 | VADD.F64 Dd Dn Dm CC CPSR
    case ARM::VADDS:         ARMRaiser::raiseVADDS        (State, MI); break; // 2058 | VADD.F32 Sd Sn Sm CC CPSR
    case ARM::VCMPD:         ARMRaiser::raiseVCMPD        (State, MI); break; // 2213 | VCMP.F64 Dd Dm CC CPSR
    case ARM::VCMPZD:        ARMRaiser::raiseVCMPZD       (State, MI); break; // 2222 | VCMP.F64 Dd [0] CC CPSR
    case ARM::VDIVD:         ARMRaiser::raiseVDIVD        (State, MI); break; // 2327 | VDIV.F64 Dd Dn Dm CC CPSR
    case ARM::VLDMDIA_UPD:   ARMRaiser::raiseVLDMDIA_UPD  (State, MI); break; // 2778 | VLDM.F64 Rt! {Rwb} CC CPSR Dn
    case ARM::VLDRD:         ARMRaiser::raiseVLDRD        (State, MI); break; // 2783 | VLDR.F64 Dd Rn Imm/4 CC CPSR
    case ARM::VLDRS:         ARMRaiser::raiseVLDRS        (State, MI); break; // 2785 | VLDR.F32 Sd Rn Imm/4 CC CPSR
    case ARM::VMLAD:         ARMRaiser::raiseVMLAD        (State, MI); break; // 2838 | VMLA.F64 Dd {Dwb} Dn Dm CC CPSR
    case ARM::VMOVD:         ARMRaiser::raiseVMOVD        (State, MI); break; // 2901 | VMOV.F64 Dd Dn CC CPSR
    case ARM::VMOVDRR:       ARMRaiser::raiseVMOVDRR      (State, MI); break; // 2902 | VMOV Dm Rd Rn CC CPSR
    case ARM::VMOVRRD:       ARMRaiser::raiseVMOVRRD      (State, MI); break; // 2915 | VMOV Rd Rn Dm CC CPSR
    case ARM::VMOVRS:        ARMRaiser::raiseVMOVRS       (State, MI); break; // 2917 | VMOV.F32 Rd Sn CC CPSR
    case ARM::VMOVSR:        ARMRaiser::raiseVMOVSR       (State, MI); break; // 2919 | VMOV.F32 St Rn CC CPSR
    case ARM::VMOVv2i32:     ARMRaiser::raiseVMOVv2i32    (State, MI); break; // 2924 | VMOV.I32 Dd Imm CC CPSR
    case ARM::VMULD:         ARMRaiser::raiseVMULD        (State, MI); break; // 2954 | VMUL.F64 Dd Dn Dm CC CPSR
    case ARM::VNEGD:         ARMRaiser::raiseVNEGD        (State, MI); break; // 2995 | VNEG.F64 Dd Dm CC CPSR
    case ARM::VORRd:         ARMRaiser::raiseVORRd        (State, MI); break; // 3019 | VORR Dd Dn Dm {CC CPSR}
    case ARM::VSITOD:        ARMRaiser::raiseVSITOD       (State, MI); break; // 3463 | VCVT.F64.S32 Dd Sm CC CPSR
    case ARM::VSTMDDB_UPD:   ARMRaiser::raiseVSTMDDB_UPD  (State, MI); break; // 3763 | VSTM.F64 Rt! {Rwb} CC CPSR Dn
    case ARM::VSTMDIA_UPD:   ARMRaiser::raiseVSTMDIA_UPD  (State, MI); break; // 3765 | VSTM.F64 Rt! {Rwb} CC CPSR Dn
    case ARM::VSTRD:         ARMRaiser::raiseVSTRD        (State, MI); break; // 3770 | VSTR.F64 Dd Rn Imm/4 CC CPSR
    case ARM::VSUBD:         ARMRaiser::raiseVSUBD        (State, MI); break; // 3791 | VSUB.F64 Dd Dn Dm CC CPSR
  }
  Monitor::event_end("ARMLinearRaiserPass::RaiseMachineInstr");
  return true;
}

#undef DEBUG_TYPE
