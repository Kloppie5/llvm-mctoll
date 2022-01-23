//===- ARMPatternMatchConcurrencyPass.h -------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MCTOLL_ARM_ARMPATTERNMATCHCONCURRENCYPASS_H
#define LLVM_MCTOLL_ARM_ARMPATTERNMATCHCONCURRENCYPASS_H

#include "Monitor.h"

using namespace llvm;

// ARMPatternMatchConcurrencyPass - This pass is responsible for
// identifying instruction patterns that correspond to commonly
// used concurrency instructions.
// Intentionally doesn't inherit from ARMRaiserBase.
class ARMPatternMatchConcurrencyPass {

public:
  bool precondition (const MachineFunction& MF) {
    auto OS = WithColor(errs(), HighlightColor::Warning);
    for (const auto &MBB : MF) {
      for (const auto &MI : MBB) {
        switch (MI.getOpcode()) {
          default:
            OS << "Unhandled opcode in instruction ";
            Monitor::printMachineInstr(&MI, true, OS);
            return false;
            break;
          
          // Instructions to be handled:
          case ARM::DMB: // 773
          case ARM::DSB: // 774
          case ARM::ISB: // 793
          case ARM::LDAEXB: // 797
          case ARM::LDREXB: // 837
          case ARM::STLEXB: // 1888
          case ARM::STREXB: // 1912
            break;
          
          // Fallthroughs:
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
          case ARM::EORri: // 775
          case ARM::EORrr: // 776
          case ARM::LDMIA: // 821
          case ARM::LDMIA_UPD: // 822
          case ARM::LDRBrs: // 832
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
          case ARM::ORRrr:// 1749
          case ARM::SBCri: // 1794
          case ARM::SBCrr: // 1795
          case ARM::SBCrsi: // 1796
          case ARM::SMLAL: // 1824
          case ARM::STMDB_UPD: // 1895
          case ARM::STMIA: // 1896
          case ARM::STMIA_UPD: // 1897
          case ARM::STMIB: // 1898
          case ARM::STRBrs: // 1907
          case ARM::STRi12: // 1926
          case ARM::STRrs: // 1927
          case ARM::SUBri: // 1928
          case ARM::SUBrr: // 1929
          case ARM::SVC: // 1932
            break;
        }
      }
    }
    return true;
  }
  static bool postcondition (const MachineFunction& MF) {
    auto OS = WithColor(errs(), HighlightColor::Warning);
    for (const auto &MBB : MF) {
      for (const auto &MI : MBB) {
        switch (MI.getOpcode()) {
          default:
            OS << "Unhandled opcode in instruction ";
            Monitor::printMachineInstr(&MI, true, OS);
            return false;
            break;
          
          // Instructions that should have been handled:
          case ARM::DMB: // 773
          case ARM::DSB: // 774
          case ARM::ISB: // 793
          case ARM::LDAEXB: // 797
          case ARM::LDREXB: // 837
          case ARM::STLEXB: // 1888
          case ARM::STREXB: // 1912
            OS << "Failed to raise instruction: ";
            Monitor::printMachineInstr(&MI, true, OS);
            return false;
            break;
          
          // Fallthroughs:
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
          case ARM::EORri: // 775
          case ARM::EORrr: // 776
          case ARM::LDMIA: // 821
          case ARM::LDMIA_UPD: // 822
          case ARM::LDRBrs: // 832
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
          case ARM::ORRrr:// 1749
          case ARM::SBCri: // 1794
          case ARM::SBCrr: // 1795
          case ARM::SBCrsi: // 1796
          case ARM::SMLAL: // 1824
          case ARM::STMDB_UPD: // 1895
          case ARM::STMIA: // 1896
          case ARM::STMIA_UPD: // 1897
          case ARM::STMIB: // 1898
          case ARM::STRBrs: // 1907
          case ARM::STRi12: // 1926
          case ARM::STRrs: // 1927
          case ARM::SUBri: // 1928
          case ARM::SUBrr: // 1929
          case ARM::SVC: // 1932
            break;
        }
      }
    }
    return true;
  }

  static bool raiseLDREXBSTREXBLoops(MachineFunction &MF) {
    auto OS = WithColor(errs(), HighlightColor::Warning);
    // [[LOOP_BB:\.?bb.*]]:
    //   ldrexb [[OLDVAL:r[0-9]+]], [[ADDR]]
    //   add [[NEWVAL:r[0-9]+]], [[OLDVAL]], [[ARG0:r0]]
    //   strexb [[STATUS:r[0-9]+]], [[NEWVAL]], [[ADDR]]
    //   cmp [[STATUS]], #0
    //   b{c} [[LOOP_BB:-24]] {ne:1}
    // }
    for (MachineBasicBlock &MBB : MF) {
      // Get instruction iterator
      auto MIitt = MBB.begin();
      
      MachineInstr *MIC_LDREXB = &*MIitt++;
      if (MIC_LDREXB->getOpcode() != ARM::LDREXB) continue;
      MachineOperand LDREXB_Rd = MIC_LDREXB->getOperand(0);
      MachineOperand LDREXB_Rn = MIC_LDREXB->getOperand(1);

      MachineInstr *MIC_BinOp = &*MIitt++;
      MachineOperand BinOp_Rd = MIC_BinOp->getOperand(0);
      MachineOperand BinOp_Rn = MIC_BinOp->getOperand(1);
      MachineOperand BinOp_Operand2 = MIC_BinOp->getOperand(2);
      if (BinOp_Rn.getReg() != LDREXB_Rd.getReg()) continue; // OLDVAL

      MachineInstr *MIC_STREXB = &*MIitt++;
      if (MIC_STREXB->getOpcode() != ARM::STREXB) continue;
      MachineOperand STREXB_Rd = MIC_STREXB->getOperand(0);
      MachineOperand STREXB_Rt = MIC_STREXB->getOperand(1);
      MachineOperand STREXB_Rn = MIC_STREXB->getOperand(2);
      if (STREXB_Rt.getReg() != BinOp_Rd.getReg()) continue; // NEWVAL
      if (STREXB_Rn.getReg() != LDREXB_Rn.getReg()) continue; // ADDR

      MachineInstr *MIC_CMPri = &*MIitt++;
      if (MIC_CMPri->getOpcode() != ARM::CMPri) continue;
      MachineOperand CMPri_Rn = MIC_CMPri->getOperand(0);
      MachineOperand CMPri_Operand2 = MIC_CMPri->getOperand(1);
      if (CMPri_Rn.getReg() != STREXB_Rd.getReg()) continue; // STATUS
      if (CMPri_Operand2.getImm() != 0) continue; // CMP 0

      MachineInstr *MIC_Bcc = &*MIitt++;
      if (MIC_Bcc->getOpcode() != ARM::Bcc) continue;
      MachineOperand Bcc_Imm = MIC_Bcc->getOperand(0);
      MachineOperand Bcc_CC = MIC_Bcc->getOperand(1);
      if (Bcc_Imm.getImm() != -24) continue; // LOOP_BB
      if (Bcc_CC.getImm() != ARMCC::NE) continue; // B{c}

      Monitor::NOTE("Found valid LDREXB STREXB loop");
      switch(MIC_BinOp->getOpcode()) {
        default:
          Monitor::printMachineInstr(MIC_BinOp);
          Monitor::ERROR("Unhandled Atomic Operation");
          break;
        case ARM::ADDri:
          Monitor::NOTE("Replacing ADDri with ATOMIC_LOAD_ADD");

          const TargetMachine &TM = MF.getTarget();
          const MCInstrInfo *TII = TM.getMCInstrInfo();
          const MCInstrDesc MCID = TII->get(TargetOpcode::G_ATOMICRMW_ADD);
          DebugLoc DL;

          MachineInstr *newMI = MF.CreateMachineInstr(MCID, DL);
          newMI->addOperand(MF, MachineOperand::CreateReg(LDREXB_Rd.getReg(), true));
          newMI->addOperand(MF, MachineOperand::CreateReg(STREXB_Rt.getReg(), false));
          newMI->addOperand(MF, MachineOperand::CreateImm(BinOp_Operand2.getImm()));

          newMI->dump();

          MBB.insert(MIitt, newMI);
          MBB.erase_instr(MIC_LDREXB);
          MBB.erase_instr(MIC_BinOp);
          MBB.erase_instr(MIC_STREXB);
          MBB.erase_instr(MIC_CMPri);
          MBB.erase_instr(MIC_Bcc);
          break;
      }
    }

      // Replace instructions with 
      /**

            SDValue Op2 = GetPromotedInteger(N->getOperand(2));
  SDValue Res = DAG.getAtomic(N->getOpcode(), SDLoc(N),
                              N->getMemoryVT(),
                              N->getChain(), N->getBasePtr(),
                              Op2, N->getMemOperand());
  // Legalize the chain result - switch anything that used the old chain to
  // use the new one.
  ReplaceValueWith(SDValue(N, 1), Res.getValue(1));

  const AtomicSDNode *AT = cast<AtomicSDNode>(N);
    ID.AddInteger(AT->getMemoryVT().getRawBits());
    ID.AddInteger(AT->getRawSubclassData());
    ID.AddInteger(AT->getPointerInfo().getAddrSpace());

case AtomicRMWInst::Add:  NT = ISD::ATOMIC_LOAD_ADD; break;
  AtomicOrdering Ordering = I.getOrdering();
  SyncScope::ID SSID = I.getSyncScopeID();

  SDValue InChain = getRoot();

  auto MemVT = getValue(I.getValOperand()).getSimpleValueType();
  const TargetLowering &TLI = DAG.getTargetLoweringInfo();
  auto Flags = TLI.getAtomicMemOperandFlags(I, DAG.getDataLayout());

  MachineFunction &MF = DAG.getMachineFunction();
  MachineMemOperand *MMO = MF.getMachineMemOperand(
      MachinePointerInfo(I.getPointerOperand()), Flags, MemVT.getStoreSize(),
      DAG.getEVTAlign(MemVT), AAMDNodes(), nullptr, SSID, Ordering);

  SDValue L =
    DAG.getAtomic(NT, dl, MemVT, InChain,
                  getValue(I.getPointerOperand()), getValue(I.getValOperand()),
                  MMO);

  SDValue OutChain = L.getValue(1);

  setValue(&I, L);
  DAG.setRoot(OutChain);
  */
    return true;
  }

  static bool run(MachineFunction &MF) {
    Monitor::NOTE("ARMPatternMatchConcurrencyPass; started");
    if (!precondition(MF)) {
      Monitor::ERROR("ARMPatternMatchConcurrencyPass; precondition failed");
      return false;
    }

    raiseLDREXBSTREXBLoops(MF);

    if (!postcondition(MF)) {
      Monitor::ERROR("ARMPatternMatchConcurrencyPass; postcondition failed");
      return false;
    }
    Monitor::NOTE("ARMPatternMatchConcurrencyPass; finished");
    return true;
  }
};

#endif // LLVM_MCTOLL_ARM_ARMPATTERNMATCHCONCURRENCYPASS_H
