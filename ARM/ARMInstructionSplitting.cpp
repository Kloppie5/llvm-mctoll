//===- ARMInstructionSplitting.cpp - Binary raiser utility llvm-mctoll ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of ARMInstructionSplitting class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "ARMInstructionSplitting.h"
#include "Monitor.h"
#include "ARMBaseInstrInfo.h"
#include "ARMSubtarget.h"
#include "MCTargetDesc/ARMAddressingModes.h"
#include "llvm/CodeGen/MachineOperand.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

// TODO: Overhaul
bool ARMInstructionSplitting::run(MachineFunction *MF, Function *F) {
  TII = MF->getSubtarget<ARMSubtarget>().getInstrInfo();
  MRI = &MF->getRegInfo();
  CTX = &MR.getModule()->getContext();

  LLVM_DEBUG(dbgs() << "ARMInstructionSplitting start.\n");

  std::vector<MachineInstr *> removelist;
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E;
         ++I) {
      MachineInstr &MI = *I;
      MachineInstr *removeMI = nullptr;

      unsigned Opcode, newOpc;
      Opcode = MI.getOpcode();
      newOpc = checkisShifter(Opcode);

      // Need to split
      if (getLoadStoreOpcode(Opcode)) {
        // Split the MI about Load and Store.

        // TODO: LDRSH/LDRSB/LDRH/LDRD split.
        if (isLDRSTRPre(Opcode)) {
          if (MI.getOperand(3).isReg())
            removeMI = splitLDRSTRPre(MBB, MI);
          else if (MI.getOperand(3).isImm() && MI.getOperand(3).getImm() != 0)
            removeMI = splitLDRSTRPreImm(MBB, MI);
          if (removeMI)
            removelist.push_back(removeMI);
        } else if (MI.getOperand(1).isReg() &&
                   MI.getOperand(1).getReg() != ARM::SP &&
                   MI.getOperand(1).getReg() != ARM::PC) {
          if (MI.getOperand(2).isReg())
            removeMI = splitLDRSTR(MBB, MI);
          else if (MI.getOperand(2).isImm() && MI.getOperand(2).getImm() != 0)
            removeMI = splitLDRSTRImm(MBB, MI);
          if (removeMI)
            removelist.push_back(removeMI);
        }
      } else if (newOpc) {
        // Split the MI except Load and Store.
        int idx = MI.findFirstPredOperandIdx();

        removeMI = splitCS(MBB, MI, newOpc, idx);

        if (removeMI)
          removelist.push_back(removeMI);
      }
    }
  }

  // Remove old MI.
  for (MachineInstr *mi : removelist)
    mi->removeFromParent();

  // For debugging.
  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMInstructionSplitting end.\n");

  return true;
}

/// Check if the MI has shift pattern.
unsigned ARMInstructionSplitting::checkisShifter(unsigned Opcode) {
  switch (Opcode) {
  case ARM::MOVsr:
  case ARM::MOVsi:
    return ARM::MOVr;
  case ARM::ADCrsi:
  case ARM::ADCrsr:
    return ARM::ADCrr;
  case ARM::ADDrsi:
  case ARM::ADDrsr:
    return ARM::ADDrr;
  case ARM::ANDrsi:
  case ARM::ANDrsr:
    return ARM::ANDrr;
  case ARM::BICrsr:
  case ARM::BICrsi:
    return ARM::BICrr;
  case ARM::CMNzrsi:
  case ARM::CMNzrsr:
    return ARM::CMNzrr;
  case ARM::CMPrsi:
  case ARM::CMPrsr:
    return ARM::CMPrr;
  case ARM::EORrsr:
  case ARM::EORrsi:
    return ARM::EORrr;
  case ARM::MVNsr:
  case ARM::MVNsi:
    return ARM::MVNr;
  case ARM::ORRrsi:
  case ARM::ORRrsr:
    return ARM::ORRrr;
  case ARM::RSBrsi:
  case ARM::RSBrsr:
    return ARM::RSBrr;
  case ARM::SUBrsi:
  case ARM::SUBrsr:
    return ARM::SUBrr;
  case ARM::TEQrsr:
  case ARM::TEQrsi:
    return ARM::TEQrr;
  case ARM::TSTrsr:
  case ARM::TSTrsi:
    return ARM::TSTrr;
  default:
    return 0;
  }
}

/// If the MI is load/store which needs wback, it will return true.
bool ARMInstructionSplitting::isLDRSTRPre(unsigned Opcode) {
  switch (Opcode) {
  case ARM::LDR_PRE_REG:
  case ARM::LDR_PRE_IMM:
  case ARM::LDRB_PRE_REG:
  case ARM::LDRB_PRE_IMM:
  case ARM::STR_PRE_REG:
  case ARM::STR_PRE_IMM:
  case ARM::STRB_PRE_REG:
  case ARM::STRB_PRE_IMM:
    return true;
  default:
    return false;
  }
}

/// No matter what pattern of Load/Store is, change the Opcode to xxxi12.
unsigned ARMInstructionSplitting::getLoadStoreOpcode(unsigned Opcode) {
  switch (Opcode) {
  case ARM::LDRrs:
  case ARM::LDRi12:
  case ARM::LDR_PRE_REG:
  case ARM::LDR_PRE_IMM:
    return ARM::LDRi12;
  case ARM::LDRBrs:
  case ARM::LDRBi12:
  case ARM::LDRB_PRE_REG:
  case ARM::LDRB_PRE_IMM:
    return ARM::LDRBi12;
  case ARM::STRrs:
  case ARM::STRi12:
  case ARM::STR_PRE_REG:
  case ARM::STR_PRE_IMM:
    return ARM::STRi12;
  case ARM::STRBrs:
  case ARM::STRBi12:
  case ARM::STRB_PRE_REG:
  case ARM::STRB_PRE_IMM:
    return ARM::STRBi12;
  default:
    return 0;
  }
}

/// True if the ARM instruction performs Shift_C().
bool ARMInstructionSplitting::isShift_C(unsigned Opcode) {
  switch (Opcode) {
  case ARM::ANDrsr:
  case ARM::ANDrsi:
  case ARM::BICrsr:
  case ARM::BICrsi:
  case ARM::EORrsr:
  case ARM::EORrsi:
  case ARM::MVNsr:
  case ARM::MVNsi:
  case ARM::ORRrsr:
  case ARM::ORRrsi:
  case ARM::TEQrsr:
  case ARM::TEQrsi:
  case ARM::TSTrsr:
  case ARM::TSTrsi:
    return true;
  default:
    return false;
  }
}

/// Get the shift opcode in MI.
unsigned ARMInstructionSplitting::getShiftOpcode(ARM_AM::ShiftOpc SOpc,
                                                 unsigned OffSet) {
  switch (SOpc) {
  case ARM_AM::asr: {
    if (OffSet != 0)
      return ARM::ASRi;
    else
      return ARM::ASRr;
  }
  case ARM_AM::lsl: {
    if (OffSet != 0)
      return ARM::LSLi;
    else
      return ARM::LSLr;
  }
  case ARM_AM::lsr: {
    if (OffSet != 0)
      return ARM::LSRi;
    else
      return ARM::LSRr;
  }
  case ARM_AM::ror: {
    if (OffSet != 0)
      return ARM::RORi;
    else
      return ARM::RORr;
  }
  case ARM_AM::rrx:
    return ARM::RRX;
  case ARM_AM::no_shift:
  default:
    return 0;
  }
}

MachineInstrBuilder &
ARMInstructionSplitting::addOperand(MachineInstrBuilder &mib,
                                    MachineOperand &mo, bool isDef) {
  switch (mo.getType()) {
  default:
    assert(false && "Unsupported MachineOperand type!");
    break;
  case MachineOperand::MO_Register: {
    if (isDef)
      mib.addDef(mo.getReg());
    else
      mib.addUse(mo.getReg());
  } break;
  case MachineOperand::MO_FrameIndex: {
    mib.addFrameIndex(mo.getIndex());
  } break;
  }

  return mib;
}

/// Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, #+/-<imm>]! to:
/// ADD Rn, Rn, #imm
/// LDRxxx/STRxxx Rt, [Rn]
MachineInstr *ARMInstructionSplitting::splitLDRSTRPreImm(MachineBasicBlock &MBB,
                                                         MachineInstr &MI) {
  MachineOperand &Rd = MI.getOperand(0);
  MachineOperand &Rn = MI.getOperand(1);
  MachineOperand &Rm = MI.getOperand(2);
  MachineOperand &imm = MI.getOperand(3);

  // MI is splitted into 2 instructions.
  // So get Metadata for the first instruction.
  ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
  MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

  // Get Metadata for the second instruction.
  ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
  MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

  unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
  // Add Rm,[Rm, #imm]!
  MachineInstrBuilder fst =
      BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDri));
  addOperand(fst, Rm, true);
  addOperand(fst, Rm);
  fst.addImm(imm.getImm());

  MachineInstrBuilder sec =
      BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
  if (MI.mayStore())
    // STRxxx Rn, [Rm]
    addOperand(sec, Rn);
  else if (MI.mayLoad())
    // LDRxxx Rd, [Rm]
    addOperand(sec, Rd, true);
  addOperand(sec, Rm);

  int idx = MI.findFirstPredOperandIdx();
  // Add predicate
  fst.addImm(MI.getOperand(idx).getImm());
  addOperand(fst, MI.getOperand(idx + 1));
  sec.addImm(MI.getOperand(idx).getImm());
  addOperand(sec, MI.getOperand(idx + 1));
 
  fst.addMetadata(N_fst);
  sec.addMetadata(N_sec);

  Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});

  return &MI;
}

/// Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>{, <shift>}]! to:
/// Rm shift #imm, but write result to VReg.
/// Add Rn, Rm
/// LDRxxx/STRxxx Rt, [Rn]
MachineInstr *ARMInstructionSplitting::splitLDRSTRPre(MachineBasicBlock &MBB,
                                                      MachineInstr &MI) {
  unsigned Simm = MI.getOperand(4).getImm();
  unsigned SOffSet = ARM_AM::getAM2Offset(Simm);
  ARM_AM::ShiftOpc SOpc = ARM_AM::getAM2ShiftOpc(Simm);
  unsigned SVReg = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);

  MachineOperand &Rd = MI.getOperand(0);
  MachineOperand &Rn = MI.getOperand(1);
  MachineOperand &Rm = MI.getOperand(2);
  MachineOperand &Rs = MI.getOperand(3);
  unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);

  // Get Metadata for the first instruction.
  ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
  MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

  // Get Metadata for the second instruction.
  ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
  MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

  // Get Metadata for the third instruction.
  ConstantAsMetadata *CMD_thd = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 2, false)));
  MDNode *N_thd = MDNode::get(*CTX, CMD_thd);

  unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
  int idx = MI.findFirstPredOperandIdx();
  
  if (SOffSet > 0) {
    // LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>{, <shift>}]!

    // Rs shift #imm and write result to VReg.
    MachineInstrBuilder fst =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
    addOperand(fst, Rs);
    fst.addImm(SOffSet);

    // Add Rm, VReg
    MachineInstrBuilder sec =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDrr));
    addOperand(sec, Rm, true);
    addOperand(sec, Rm);
    sec.addReg(SVReg);

    MachineInstrBuilder thd =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
    if (MI.mayStore())
      // STRxxx Rn, [Rm]
      addOperand(thd, Rn);
    else if (MI.mayLoad())
      // LDRxxx Rd, [Rm]
      addOperand(thd, Rd, true);
    addOperand(thd, Rm);

    // Add predicate
    fst.addImm(MI.getOperand(idx).getImm());
    addOperand(fst, MI.getOperand(idx + 1));
    sec.addImm(MI.getOperand(idx).getImm());
    addOperand(sec, MI.getOperand(idx + 1));
    thd.addImm(MI.getOperand(idx).getImm());
    addOperand(thd, MI.getOperand(idx + 1));
    
    fst.addMetadata(N_fst);
    sec.addMetadata(N_sec);
    thd.addMetadata(N_thd);

    Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr(), thd.getInstr()});
  } else if (ShiftOpc == ARM::RRX) {
    // Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>, RRX]!
    MachineInstrBuilder fst =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
    addOperand(fst, Rs);

    MachineInstrBuilder sec =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDrr));
    addOperand(sec, Rm, true);
    addOperand(sec, Rm);
    sec.addReg(SVReg);

    MachineInstrBuilder thd =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
    if (MI.mayStore())
      addOperand(thd, Rn);
    else if (MI.mayLoad())
      addOperand(thd, Rd, true);
    addOperand(thd, Rm);

    // Add predicate
    sec.addImm(MI.getOperand(idx).getImm());
    addOperand(sec, MI.getOperand(idx + 1));
    thd.addImm(MI.getOperand(idx).getImm());
    addOperand(thd, MI.getOperand(idx + 1));
    
    fst.addMetadata(N_fst);
    sec.addMetadata(N_sec);
    thd.addMetadata(N_thd);

    Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr(), thd.getInstr()});
  } else {
    // Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>]!
    MachineInstrBuilder fst =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDrr));
    addOperand(fst, Rm, true);
    addOperand(fst, Rm);
    addOperand(fst, Rs);

    MachineInstrBuilder sec =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
    if (MI.mayStore())
      addOperand(sec, Rn);
    else if (MI.mayLoad())
      addOperand(sec, Rd, true);
    addOperand(sec, Rm);

    // Add predicate
    fst.addImm(MI.getOperand(idx).getImm());
    addOperand(fst, MI.getOperand(idx + 1));
    sec.addImm(MI.getOperand(idx).getImm());
    addOperand(sec, MI.getOperand(idx + 1));
    
    fst.addMetadata(N_fst);
    sec.addMetadata(N_sec);

    Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
  }
  return &MI;
}

/// Split LDRxxx/STRxxx<c><q> <Rd>, [<Rn>, +/-<#imm>] to:
/// Add VReg, Rn, #imm
/// LDRxxx/STRxxx Rd, [VReg]
MachineInstr *ARMInstructionSplitting::splitLDRSTRImm(MachineBasicBlock &MBB,
                                                      MachineInstr &MI) {
  unsigned VReg = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);
  MachineOperand &Rd = MI.getOperand(0);
  MachineOperand &Rn = MI.getOperand(1);
  MachineOperand &imm = MI.getOperand(2);

  // The MI is splitted into 2 instructions.
  // Get Metadata for the first instruction.
  ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
  MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

  // Get Metadata for the first instruction.
  ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
  MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

  unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
  // Add VReg, Rn, #imm
  MachineInstrBuilder fst =
      BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDri), VReg);
  addOperand(fst, Rn);
  fst.addImm(imm.getImm());

  // LDRxxx/STRxxx Rd, [VReg]
  MachineInstrBuilder sec =
      BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
  if (MI.mayStore())
    addOperand(sec, Rd);
  else
    addOperand(sec, Rd, true);
  sec.addReg(VReg);

  int idx = MI.findFirstPredOperandIdx();
  // Add predicate
  fst.addImm(MI.getOperand(idx).getImm());
  addOperand(fst, MI.getOperand(idx + 1));
  sec.addImm(MI.getOperand(idx).getImm());
  addOperand(sec, MI.getOperand(idx + 1));
  
  fst.addMetadata(N_fst);
  sec.addMetadata(N_sec);

  Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});

  return &MI;
}

/// Split LDRxxx/STRxxx<c><q> <Rd>, [<Rn>, +/-<Rm>{, <shift>}] to:
/// Rm shift #imm, but write result to VReg.
/// Add VReg, Rn, Rm
/// LDRxxx/STRxxx Rd, [VReg]
MachineInstr *ARMInstructionSplitting::splitLDRSTR(MachineBasicBlock &MBB,
                                                   MachineInstr &MI) {
  unsigned Simm = MI.getOperand(3).getImm();
  unsigned SOffSet = ARM_AM::getAM2Offset(Simm);
  ARM_AM::ShiftOpc SOpc = ARM_AM::getAM2ShiftOpc(Simm);
  unsigned SVReg = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);
  unsigned AVReg = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);

  MachineOperand &Rd = MI.getOperand(0);
  MachineOperand &Rn = MI.getOperand(1);
  MachineOperand &Rm = MI.getOperand(2);
  unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);

  // Get Metadata for the fisrt insturction.
  ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
  MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

  // Get Metadata for the second insturction.
  ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
  MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

  // Get Metadata for the third insturction.
  ConstantAsMetadata *CMD_thd = ConstantAsMetadata::get(
      ConstantInt::get(*CTX, llvm::APInt(64, 2, false)));
  MDNode *N_thd = MDNode::get(*CTX, CMD_thd);

  unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
  int idx = MI.findFirstPredOperandIdx();
  
  if (SOffSet > 0) {
    // Split LDRxxx/STRxxx Rd, [Rn, Rm, shift]
    MachineInstrBuilder fst =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
    addOperand(fst, Rm);
    fst.addImm(SOffSet);

    MachineInstrBuilder sec =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDrr), AVReg);
    addOperand(sec, Rn);
    sec.addReg(SVReg);

    MachineInstrBuilder thd =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
    if (MI.mayStore())
      addOperand(thd, Rd);
    else
      addOperand(thd, Rd, true);
    thd.addReg(AVReg);
    // Add predicate
    fst.addImm(MI.getOperand(idx).getImm());
    addOperand(fst, MI.getOperand(idx + 1));
    sec.addImm(MI.getOperand(idx).getImm());
    addOperand(sec, MI.getOperand(idx + 1));
    thd.addImm(MI.getOperand(idx).getImm());
    addOperand(thd, MI.getOperand(idx + 1));
    
    fst.addMetadata(N_fst);
    sec.addMetadata(N_sec);
    thd.addMetadata(N_thd);

    Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr(), thd.getInstr()});
  } else if (ShiftOpc == ARM::RRX) {
    // Split LDRxxx/STRxxx Rd, [Rn, Rm, rrx]
    MachineInstrBuilder fst =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
    addOperand(fst, Rm);

    MachineInstrBuilder sec =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDrr), AVReg);
    addOperand(sec, Rn);
    sec.addReg(SVReg);

    MachineInstrBuilder thd =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
    if (MI.mayStore())
      addOperand(thd, Rd);
    else
      addOperand(thd, Rd, true);
    thd.addReg(AVReg);
    // Add predicate
    sec.addImm(MI.getOperand(idx).getImm());
    addOperand(sec, MI.getOperand(idx + 1));
    thd.addImm(MI.getOperand(idx).getImm());
    addOperand(thd, MI.getOperand(idx + 1));
    
    fst.addMetadata(N_fst);
    sec.addMetadata(N_sec);
    thd.addMetadata(N_thd);

    Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr(), thd.getInstr()});
  } else {
    // Split LDRxxx/STRxxx Rd, [Rn, Rm]
    MachineInstrBuilder fst =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ARM::ADDrr), AVReg);
    addOperand(fst, Rn);
    addOperand(fst, Rm);

    MachineInstrBuilder sec =
        BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
    if (MI.mayStore())
      addOperand(sec, Rd);
    else
      addOperand(sec, Rd, true);
    sec.addReg(AVReg);
    // Add [predicate
    fst.addImm(MI.getOperand(idx).getImm());
    addOperand(fst, MI.getOperand(idx + 1));
    sec.addImm(MI.getOperand(idx).getImm());
    addOperand(sec, MI.getOperand(idx + 1));
    
    fst.addMetadata(N_fst);
    sec.addMetadata(N_sec);

    Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
  }
  return &MI;
}

/// Split 'opcode<s><c> Rd, Rn, Rm, shift' except LDRxxx/STRxxx.
MachineInstr *ARMInstructionSplitting::splitCS(MachineBasicBlock &MBB,
                                               MachineInstr &MI,
                                               unsigned newOpc, int idx) {
  MachineInstr *mi = nullptr;
  for (unsigned i = 0; i < MI.getNumOperands(); i++) {
    if (MI.getOperand(i).isImm()) {
      unsigned Simm = MI.getOperand(i).getImm();
      unsigned SOffSet = ARM_AM::getSORegOffset(Simm);
      ARM_AM::ShiftOpc SOpc = ARM_AM::getSORegShOp(Simm);
      unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);
      assert (ShiftOpc && "Invalid shift opcode!");
      unsigned VReg = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);

      MachineOperand &Rd = MI.getOperand(0);
      MachineOperand &Rn = MI.getOperand(i - 2);
      MachineOperand &Rm = MI.getOperand(i - 1);

      ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
          ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
      MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

      ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
          ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
      MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

      // C flag is affected by Shift_c() if isShift_C is true.
      if (isShift_C(MI.getOpcode())) {
        if (SOffSet) {
          // Split opcode<s><c> Rd, Rn, Rm, shift #imm

          // The new MI both updates CPSR and checks CondCode.
          MachineInstrBuilder fst =
              BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc))
              .addDef(VReg)                            // Rn =
              .addReg(Rm.getReg())                     // Rn
              .addImm(SOffSet)                         // imm
              .addImm(MI.getOperand(idx).getImm())     // CC
              .addReg(MI.getOperand(idx + 1).getReg()) // CPSR
              .addReg(MI.getOperand(idx + 2).getReg()) // Implicit CPSR
              .addMetadata(N_fst);

          MachineInstrBuilder sec =
              BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
          addOperand(sec, Rd, true);
          for (unsigned n = 1; n < (i - 1); n++) {
            addOperand(sec, MI.getOperand(n));
          }
          sec.addReg(VReg);
          sec.addImm(MI.getOperand(idx).getImm());
          addOperand(sec, MI.getOperand(idx + 1));
          addOperand(sec, MI.getOperand(idx + 2));
          sec.addMetadata(N_sec);

          Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
        } else {
          if (ShiftOpc == ARM::RRX) {
            // Split opcode<s><c> Rd, Rn, Rm, RRX
            MachineInstrBuilder fst =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
            addOperand(fst, Rm);
            fst.addMetadata(N_fst);
            // RRX implicit CPSR, how to add cpsr?

            MachineInstrBuilder sec =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
            addOperand(sec, Rd, true);

            for (unsigned n = 1; n < (i - 1); n++) {
              addOperand(sec, MI.getOperand(n));
            }
            sec.addReg(VReg);
            sec.addImm(MI.getOperand(idx).getImm());
            addOperand(sec, MI.getOperand(idx + 1));
            addOperand(sec, MI.getOperand(idx + 2));
            sec.addMetadata(N_sec);

            Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
          } else {
            // Split opcode<s><c> Rd, Rn, Rm, shift Rs

            // The new MI both updates CPSR and checks CondCode.
            MachineInstrBuilder fst =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
            addOperand(fst, Rn);
            addOperand(fst, Rm);
            fst.addImm(MI.getOperand(idx).getImm());
            addOperand(fst, MI.getOperand(idx + 1));
            addOperand(fst, MI.getOperand(idx + 2));
            fst.addMetadata(N_fst);

            MachineInstrBuilder sec =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
            addOperand(sec, Rd, true);

            for (unsigned n = 1; n < (i - 2); n++) {
              addOperand(sec, MI.getOperand(n));
            }
            sec.addReg(VReg);
            sec.addImm(MI.getOperand(idx).getImm());
            addOperand(sec, MI.getOperand(idx + 1));
            addOperand(sec, MI.getOperand(idx + 2));
            sec.addMetadata(N_sec);

            Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
          }
        }
      } else {
        // Shifter doesn't update cpsr
        if (SOffSet) {
          // Split 'opcode<s><c> Rd, Rn, Rm, shift #imm'

          // Rm shifts #imm
          // The new MI checks CondCode, doesn't update CPSR.
          MachineInstrBuilder fst =
              BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
          addOperand(fst, Rm);
          fst.addImm(SOffSet);
          fst.addImm(MI.getOperand(idx).getImm());
          addOperand(fst, MI.getOperand(idx + 1));
          fst.addMetadata(N_fst);

          // Build 'newOpc<s><c> Rd, Rn, VReg'
          // The new MI both updates CPSR and checks CondCode.
          MachineInstrBuilder sec =
              BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
          addOperand(sec, Rd, true);
          for (unsigned n = 1; n < (i - 1); n++) {
            addOperand(sec, MI.getOperand(n));
          }
          sec.addReg(VReg);
          sec.addImm(MI.getOperand(idx).getImm());
          addOperand(sec, MI.getOperand(idx + 1));
          addOperand(sec, MI.getOperand(idx + 2));
          sec.addMetadata(N_sec);

          Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
        } else {
          if (ShiftOpc == ARM::RRX) {
            // Split opcode<s><c> Rd, Rn, Rm, RRX
            MachineInstrBuilder fst =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
            addOperand(fst, Rm);
            fst.addMetadata(N_fst);
            // RRX implicit CPSR, how to add cpsr?

            MachineInstrBuilder sec =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
            addOperand(sec, Rd, true);

            for (unsigned n = 1; n < (i - 1); n++) {
              addOperand(sec, MI.getOperand(n));
            }
            sec.addReg(VReg);
            sec.addImm(MI.getOperand(idx).getImm());
            addOperand(sec, MI.getOperand(idx + 1));
            addOperand(sec, MI.getOperand(idx + 2));
            sec.addMetadata(N_sec);

            Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
          } else {
            // Split opcode<s><c> Rd, Rn, Rm, shift Rs

            // Rm shift #imm.
            // The new MI checks CondCode, doesn't update CPSR.
            MachineInstrBuilder fst =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
            addOperand(fst, Rn);
            addOperand(fst, Rm);
            fst.addImm(MI.getOperand(idx).getImm());
            addOperand(fst, MI.getOperand(idx + 1));
            fst.addMetadata(N_fst);

            // Build 'newOpc<s><c> Rd, Rn, VReg'
            // The new MI both updates CPSR and checks CondCode.
            MachineInstrBuilder sec =
                BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
            addOperand(sec, Rd, true);

            for (unsigned n = 1; n < (i - 2); n++) {
              addOperand(sec, MI.getOperand(n));
            }
            sec.addReg(VReg);
            sec.addImm(MI.getOperand(idx).getImm());
            addOperand(sec, MI.getOperand(idx + 1));
            addOperand(sec, MI.getOperand(idx + 2));
            sec.addMetadata(N_sec);

            Monitor::event_MachineInstrsToMachineInstrs("Split", {&MI}, {fst.getInstr(), sec.getInstr()});
          }
        }
      }
      mi = &MI;
      break;
    }
  }

  return mi;
}

#undef DEBUG_TYPE
