//===- ARMInstrSplitter.cpp - Binary raiser utility llvm-mctoll ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of ARMInstrSplitter class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "ARMInstrSplitter.h"
#include "Monitor.h"

#include "ARMBaseInstrInfo.h"
#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "MCTargetDesc/ARMAddressingModes.h"
#include "llvm/CodeGen/MachineOperand.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

bool ARMInstrSplitter::precondition(MachineFunction *MF, Function *F) {
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      switch(MI.getOpcode()) {
        default: {
          auto OS = WithColor(errs(), HighlightColor::Warning);
          OS << "ARMInstrSplitter encountered unhandled opcode in instruction ";
          Monitor::printMachineInstr(&MI, true, OS);
          assert(false && "ARMInstrSplitter encountered unhandled opcode.");
          return false;
        } break;

        // Handled instructions;
        case ARM::ADDrsi: // 686
        case ARM::ADDrsr: // 687
        case ARM::EORrsi: // 777
        case ARM::LDR_PRE_IMM: // 859
        case ARM::LDR_PRE_REG: // 860
        case ARM::LDRrs: // 863
        case ARM::MOVsi: // 876
        case ARM::MOVsr: // 877
        case ARM::STR_PRE_IMM: // 1924
        case ARM::STR_PRE_REG: // 1925
        case ARM::STRrs: // 1927
          break;

        // Ignored instructions;
        case ARM::ADDri: // 684
        case ARM::ADDrr: // 685
        case ARM::ANDri: // 693
        case ARM::BL: // 711
        case ARM::BX_RET: // 718
        case ARM::Bcc: // 720
        case ARM::CMPri: // 759
        case ARM::CMPrr: // 760
        case ARM::LDRBi12: // 831
        case ARM::LDRi12: // 862
        case ARM::MOVi: // 872
        case ARM::MOVr: // 874
        case ARM::MUL: // 888
        case ARM::STRBi12: // 1906
        case ARM::STRi12: // 1926
        case ARM::SUBri: // 1928
        case ARM::SUBrr: // 1929
          break;
      }
    }
  }
  return true;
}
bool ARMInstrSplitter::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMInstrSplitter");
  LLVM_DEBUG(dbgs() << "ARMInstrSplitter start.\n");

  TII = MF->getSubtarget<ARMSubtarget>().getInstrInfo();
  MRI = &MF->getRegInfo();
  CTX = &MR.getModule()->getContext();

  if (!precondition(MF, F))
    return false;

  // Because instructions are added to the blocks,
  // we can run into problems with the iterator.
  // To avoid this, we should copy the blocks into a vector
  // and then iterate over the vector, but for now,
  // we iterate over the blocks with a rather ugly hack.
  // This should be fixed when moving to a more stable
  // MachineInstr to Instruction mapping.
  
  ONCE_MORE:
  for (auto &MBB : *MF)
    for (auto &MI : MBB)
      if(splitMachineInstr(&MBB, &MI))
        goto ONCE_MORE;

  if (!postcondition(MF, F))
    return false;

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMInstrSplitter end.\n");
  Monitor::event_end("ARMInstrSplitter");
  return true;
}
bool ARMInstrSplitter::postcondition(MachineFunction *MF, Function *F) {
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      switch(MI.getOpcode()) {
        default: {
          auto OS = WithColor(errs(), HighlightColor::Warning);
          OS << "ARMInstrSplitter emitted unexpected opcode in instruction ";
          Monitor::printMachineInstr(&MI, true, OS);
          assert(false && "ARMInstrSplitter emitted unexpected opcode.");
          return false;
        } break;

        // Handled instructions;
        case ARM::ADDrsi: // 686
        case ARM::ADDrsr: // 687
        case ARM::EORrsi: // 777
        case ARM::LDR_PRE_IMM: // 859
        case ARM::LDR_PRE_REG: // 860
        case ARM::LDRrs: // 863
        case ARM::MOVsi: // 876
        case ARM::MOVsr: // 877
        case ARM::STR_PRE_IMM: // 1924
        case ARM::STR_PRE_REG: // 1925 
        case ARM::STRrs: // 1927
        { auto OS = WithColor(errs(), HighlightColor::Warning);
          OS << "ARMInstrSplitter failed to handle instruction ";
          Monitor::printMachineInstr(&MI, true, OS);
          return false;
        } break;

        // Output instructions;
        case ARM::ASRi: // 247
        case ARM::ASRr: // 248
        case ARM::LSLi: // 292
        case ARM::LSLr: // 293
        case ARM::LSRi: // 294
        case ARM::LSRr: // 295
        case ARM::RORi: // 325
        case ARM::RORr: // 326
        case ARM::RRX: // 327
        case ARM::ADDri: // 684
        case ARM::ADDrr: // 685
        case ARM::ANDri: // 693
        case ARM::BL: // 711
        case ARM::BX_RET: // 718
        case ARM::Bcc: // 720
        case ARM::CMPri: // 759
        case ARM::CMPrr: // 760
        case ARM::EORrr: // 776
        case ARM::LDRBi12: // 831
        case ARM::LDRi12: // 862
        case ARM::MOVi: // 872
        case ARM::MOVr: // 874
        case ARM::MUL: // 888
        case ARM::STRBi12: // 1906
        case ARM::STRi12: // 1926
        case ARM::SUBri: // 1928
        case ARM::SUBrr: // 1929
          break;
      }
    }
  }
  return true;
}
bool ARMInstrSplitter::splitMachineInstr(MachineBasicBlock *MBB, MachineInstr *MI) {

  switch (MI->getOpcode()) {
    default:
      return false;
      break;
  
    case ARM::ADDrsi: // 686
    case ARM::ADDrsr: // 687
      splitOperand2(MBB, MI, 2, ARM::ADDrr); break;
    case ARM::EORrsi: // 777
      splitOperand2(MBB, MI, 2, ARM::EORrr); break;
    case ARM::MOVsi: // 876
    case ARM::MOVsr: // 877
      splitOperand2(MBB, MI, 1, ARM::MOVr); break;
   
    case ARM::LDR_PRE_IMM: // 859 | LDR Rt, [Rn, #offset]! = { Rt Rn Rs Imm CC CPSR }
      splitLDRSTR(MBB, MI, 1, 1, 1, 0); break;
    case ARM::LDR_PRE_REG: // 860 | LDR Rt, [Rn, ±Rm {, shift}]! = { Rt Rn Rs Rm ModImm CC CPSR }
      splitLDRSTR(MBB, MI, 1, 0, 1, 0); break;
    case ARM::LDRrs: // 863 | LDR Rt, [Rn, ±Rm {, shift}] { Rt Rn Rm ModImm CC CPSR }
      splitLDRSTR(MBB, MI, 1, 0, 0, 0); break;
    case ARM::STR_PRE_IMM: // 1924 | STR Rt, [Rn, #offset]! = { Rn Rt Rs Imm CC CPSR }
      splitLDRSTR(MBB, MI, 0, 1, 1, 0); break;
    case ARM::STR_PRE_REG: // 1925 | STR Rt, [Rn, ±Rm {, shift}]! = { Rn Rt Rs Rm ModImm CC CPSR }
      splitLDRSTR(MBB, MI, 0, 0, 1, 0); break;
    case ARM::STRrs: // 1927 | STR Rt, [Rn, ±Rm {, shift}] { Rn Rt Rm ModImm CC CPSR }
      splitLDRSTR(MBB, MI, 0, 0, 0, 0); break;
  }

  return true;
}

void ARMInstrSplitter::splitLDRSTR(MachineBasicBlock *MBB, MachineInstr *MI, bool isLoad, bool isSO, bool isPre, bool isPost) {
  std::vector<MachineInstr *> Instrs = {};

  Register Rt = MI->getOperand(!isLoad).getReg();
  Register Rn = MI->getOperand(isLoad).getReg();

  int CCidx = MI->findFirstPredOperandIdx();
  int64_t Imm = MI->getOperand(CCidx - 1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(CCidx).getImm();
  Register CPSR = MI->getOperand(CCidx + 1).getReg();

  unsigned Imm12 = Imm & 0xFFF;
  ARM_AM::ShiftOpc ShiftOpc = (ARM_AM::ShiftOpc) ((Imm >> 13) & 0x7);
  bool isSub = (Imm >> 12) & 0x1;

  Register Rm = MI->getOperand(CCidx-2).getReg();

  // Calculate shifted offset
  // SHIFTi { Roff Rm Imm12 CC CPSR noreg }
  Register Roff = Rm;
  if (!isSO) {
    if (ShiftOpc == ARM_AM::rrx) {
      Roff = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);
      MachineInstrBuilder Shift = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(ARM::RRX));
      Shift.addReg(Roff, RegState::Define); // Roff =
      Shift.addReg(Rm);                     // Rm
                                            // RRX 1
      Instrs.push_back(Shift.getInstr());
    } else if (Imm12 != 0) {
      unsigned ShiftOpcode = getShiftOpcode(ShiftOpc);
      Roff = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);
      MachineInstrBuilder Shift = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(ShiftOpcode));
      Shift.addReg(Roff, RegState::Define); // Roff =
      Shift.addReg(Rm);                     // Rm =
      Shift.addImm(Imm12);                  // SHIFTOP Imm
      Shift.addImm(CC);                     // ? Cond
      Shift.addReg(CPSR);                   // @ CPSR
      Shift.addReg(0);                      // # Sreg
      Instrs.push_back(Shift.getInstr());
    }
  }

  // Pre-increment Rn
  // ALUrr { Rn Rs Roff CC CPSR noreg } 
  Register Radd = isPre ? Rn : MRI->createVirtualRegister(&ARM::GPRnopcRegClass);
  unsigned PreOpcode = isSO
    ? (isSub ? ARM::SUBri : ARM::ADDri)
    : (isSub ? ARM::SUBrr : ARM::ADDrr);
  MachineInstrBuilder PreInc = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(PreOpcode));
  PreInc.addReg(Radd, RegState::Define);  // Radd =
  PreInc.addReg(Rn);                      // Rn
  
  if (isSO) PreInc.addImm(Imm);           // +- Imm
  else PreInc.addReg(Roff);               // +- Roff
  
  PreInc.addImm(CC);                      // ? Cond
  PreInc.addReg(CPSR);                    // @ CPSR
  PreInc.addReg(0);                       // # Sreg
  Instrs.push_back(PreInc.getInstr());

  // Load/Store
  // LDRSTRi12 { Rt Rn 0 CC CPSR }
  unsigned Opcode = isLoad ? ARM::LDRi12 : ARM::STRi12;
  MachineInstrBuilder LDRSTR = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(Opcode));
  LDRSTR.addReg(Rt, isLoad*RegState::Define); // Rt <=>
  LDRSTR.addReg(Radd);                        // Rn
  LDRSTR.addImm(0);                           // +0
  LDRSTR.addImm(CC);                          // ? Cond
  LDRSTR.addReg(CPSR);                        // @ CPSR
  Instrs.push_back(LDRSTR.getInstr());

  // Post-increment Rn
  // ALUrr { Rn Rs Roff CC CPSR noreg }
  if (isPost) {
    unsigned PostOpcode = isSO
      ? (isSub ? ARM::SUBri : ARM::ADDri)
      : (isSub ? ARM::SUBrr : ARM::ADDrr);
    MachineInstrBuilder PostInc = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(PostOpcode));
    PostInc.addReg(Rn, RegState::Define);  // Rn =
    PostInc.addReg(Rn);                    // Rn
    
    if (isSO) PostInc.addImm(Imm);         // +- Imm
    else PostInc.addReg(Roff);             // +- Roff

    PostInc.addImm(CC);                    // ? Cond
    PostInc.addReg(CPSR);                  // @ CPSR
    PostInc.addReg(0);                     // # Sreg
    Instrs.push_back(PostInc.getInstr());
  }

  Monitor::event_MachineInstrsToMachineInstrs("Split", {MI}, Instrs);
  MI->eraseFromParent();
}

void ARMInstrSplitter::splitOperand2(MachineBasicBlock *MBB, MachineInstr *MI, unsigned idx, unsigned NewOpcode) {
  std::vector<MachineInstr *> Instrs = {};

  int CCidx = MI->findFirstPredOperandIdx();

  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(CCidx).getImm();
  Register CPSR = MI->getOperand(CCidx + 1).getReg();

  unsigned VReg = MRI->createVirtualRegister(&ARM::GPRnopcRegClass);

  if (CCidx - idx == 3) { // Rd Rn (Rm Rs T) CC CPSR Sreg
    Register Rm = MI->getOperand(idx).getReg();
    Register Rs = MI->getOperand(idx+1).getReg();
    ARM_AM::ShiftOpc ShiftOpc = (ARM_AM::ShiftOpc) MI->getOperand(idx + 2).getImm();
    unsigned ShiftOpcode = getShiftOpcode(ShiftOpc, true);

    MachineInstrBuilder Shift = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(ShiftOpcode));
    Shift.addReg(VReg, RegState::Define);
    Shift.addReg(Rm);
    Shift.addReg(Rs);
    Shift.addImm(CC);
    Shift.addReg(CPSR);
    Shift.addReg(0);
    Instrs.push_back(Shift);
  }

  if (CCidx - idx == 2) { // Rd Rn (Rs Imm) CC CPSR Sreg
    Register Rs = MI->getOperand(idx).getReg();
    int64_t Imm = MI->getOperand(idx+1).getImm();
    int64_t Offset = Imm >> 3;
    ARM_AM::ShiftOpc ShiftOpc = (ARM_AM::ShiftOpc) (Imm & 0x7);
    unsigned ShiftOpcode = getShiftOpcode(ShiftOpc, false);

    MachineInstrBuilder Shift = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(ShiftOpcode));
    Shift.addReg(VReg, RegState::Define);
    Shift.addReg(Rs);
    if (ShiftOpc != ARM_AM::rrx) {
      Shift.addImm(Offset);
      Shift.addImm(CC);
      Shift.addReg(CPSR);
      Shift.addReg(0);
    }
    Instrs.push_back(Shift);
  }  

  MachineInstrBuilder NewOp = BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(NewOpcode));
  for (unsigned i = 0; i < idx; ++i)
    NewOp.add(MI->getOperand(i));
  NewOp.addReg(VReg);
  NewOp.addImm(CC);
  NewOp.addReg(CPSR);
  NewOp.add(MI->getOperand(CCidx+2));
  Instrs.push_back(NewOp);

  Monitor::event_MachineInstrsToMachineInstrs("Split", {MI}, Instrs);
  MI->eraseFromParent();
}

/// Get the shift opcode in MI
unsigned ARMInstrSplitter::getShiftOpcode(ARM_AM::ShiftOpc SOpc, bool isReg) {
  switch (SOpc) {
    case ARM_AM::no_shift: // 0
      return isReg ? ARM::ADDrr : ARM::ADDri;
    case ARM_AM::asr: // 1
      return isReg ? ARM::ASRr : ARM::ASRi;
    case ARM_AM::lsl: // 2
      return isReg ? ARM::LSLr : ARM::LSLi;
    case ARM_AM::lsr: // 3
      return isReg ? ARM::LSRr : ARM::LSRi;
    case ARM_AM::ror: // 4
      return isReg ? ARM::RORr : ARM::RORi;
    case ARM_AM::rrx: // 5
      return ARM::RRX;
    case ARM_AM::uxtw: // 6
      assert(false && "UXTW is not supported yet!");
    default:
      return -1;
  }
}

#undef DEBUG_TYPE
