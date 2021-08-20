//===- ARMPatternMatchConcurrencyPass.h -------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MCTOLL_ARM_ARMPATTERNMATCHCONCURRENCYPASS_H
#define LLVM_MCTOLL_ARM_ARMPATTERNMATCHCONCURRENCYPASS_H

#include "ARMRaiserBase.h"
#include "Monitor.h"

using namespace llvm;

// ARMPatternMatchConcurrencyPass - This pass is responsible for
// identifying instruction patterns that correspond to commonly
// used concurrency patterns.
// Intentionally doesn't inherit from ARMRaiserBase.
class ARMPatternMatchConcurrencyPass {

public:
  ARMPatternMatchConcurrencyPass() = delete;
  ARMPatternMatchConcurrencyPass(const ARMPatternMatchConcurrencyPass &) =
      delete;
  ARMPatternMatchConcurrencyPass &
  operator=(const ARMPatternMatchConcurrencyPass &) = delete;

  static bool run(MachineFunction &MF, Function &F) {
    Monitor::NOTE("ARMPatternMatchConcurrencyPass; started");
    auto OS = WithColor(errs(), HighlightColor::Warning);
    for (MachineBasicBlock &MBB : MF) {
      OS << "In MBB: " << MBB.getFullName() << "\n";
      for (MachineInstr &MI : MBB) {
        
        switch (MI.getOpcode()) {
          default:
            OS << "Unhandled opcode in instruction ";
            Monitor::printMachineInstr(&MI, true, OS);
            assert(false && "Unhandled opcode in ARMPatternMatchConcurrencyPass");
            break;

         
         
         
         

          case ARM::DMB: // 773
          case ARM::DSB: // 774
          case ARM::ISB: // 793
          case ARM::LDAEXB: // 797
          case ARM::LDREXB: // 837
          case ARM::STLEXB: // 1888
          case ARM::STREXB: // 1912
            OS << "Illegal instruction: ";
            Monitor::printMachineInstr(&MI, true, OS);
            break;
        
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
          case ARM::EORri: 775
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
            // OS << "Allowed instruction: ";
            // Monitor::printMachineInstr(&MI, true, OS);
            break;
        }
      }
    }

    Monitor::NOTE("ARMPatternMatchConcurrencyPass; finished");
    return false;
  }
};

#endif // LLVM_MCTOLL_ARM_ARMPATTERNMATCHCONCURRENCYPASS_H
