//===- ARMFunctionPrototype.cpp - Binary raiser utility llvm-mctoll -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of ARMFunctionPrototype class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "Monitor.h"
#include "ARMFunctionPrototype.h"
#include "ARMSubtarget.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

char ARMFunctionPrototype::ID = 0;

ARMFunctionPrototype::ARMFunctionPrototype() : MachineFunctionPass(ID) {}

ARMFunctionPrototype::~ARMFunctionPrototype() {}

/// Check the first reference of the reg is USE.
bool ARMFunctionPrototype::isUsedRegister(unsigned reg, MachineFunction *MF) {
  for (MachineBasicBlock &MBB : *MF)
    for (MachineInstr &MI : MBB)
      for (MachineOperand &MO : MI.operands())
        if (MO.isReg() && MO.getReg() == reg) // Find first reference of reg
          return MO.isUse(); // Return true if it is a use
  return false;
}

/// Check the first reference of the reg is DEF.
void ARMFunctionPrototype::genParameterTypes(std::vector<Type *> &paramTypes) {
  assert(!MF->empty() && "The function body is empty!!!");
  DenseMap<int, Type *> tarr; // assuming the number of parameters tends to be around 50?
  int maxidx = -1; // When the maxidx is -1, means there is no argument.
  // Check register liveness at the beginning of the function.
  for (unsigned IReg = ARM::R0; IReg < ARM::R4; IReg++) {
    if (!isUsedRegister(IReg, MF))
      continue;

    int idx = IReg - ARM::R0;
    if (idx > maxidx)
      maxidx = idx;

    tarr[idx] = getDefaultType();
    Monitor::event_raw() << "Found reg " << idx << "; maxidx = " << maxidx << "\n";
  }

  // Because stack accesses are non-trivial, the only way to determine
  // stack arguments beforehand is via symbolic execution.
  // A better approach would be to adapt the function prototype on
  // the fly, but this is not feasible for now.

  // A somewhat stable way is to symbolically execute the stack access
  // related instructions and hope that no stack accesses are missed.
  int64_t stack_offset = 0;
  std::map<int64_t, Type*> stack_offsets;
  for (MachineBasicBlock &MBB : *MF) {
    for (MachineInstr &MI : MBB) {
      switch(MI.getOpcode()) {
        default:
          for (MachineOperand &MO : MI.operands()) {
            if (MO.isReg() && MO.isUse() && !MO.isImplicit() && MO.getReg() == ARM::SP) {
              Monitor::event_MachineInstr(&MI);
              assert(false && "SP used in unchecked operation during function prototype discovery");
            }
          } break;
        case ARM::ADDri: // 684
          if (MI.getOperand(0).isReg() && MI.getOperand(0).getReg() == ARM::SP
           && MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP) {
            int64_t Op2 = MI.getOperand(2).getImm();
            unsigned Bits = Op2 & 0xFF;
            unsigned Rot = (Op2 & 0xF00) >> 7;
            int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
            Monitor::event_raw() << "Incrementing stack offset: " << Imm << " to " << (stack_offset+Imm) << "\n";
            stack_offset += Imm;
          } break;
        case ARM::LDMIA_UPD: // 822
          Monitor::event_raw() << "Incrementing stack offset: 4 to " << (stack_offset+4) << "\n";
          stack_offsets[stack_offset] = Type::getInt32Ty(MF->getFunction().getContext());
          stack_offset += 4;
          break;
        case ARM::LDRBi12: // 831
          if(MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(3).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(3).getImm()] = Type::getInt8Ty(MF->getFunction().getContext());
          } break;
        case ARM::LDRH: // 840
          if(MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(3).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(3).getImm()] = Type::getInt16Ty(MF->getFunction().getContext());
          } break;
        case ARM::LDR_POST_IMM: // 857 { Reg:$R0 Reg:$SP Reg:$SP Reg:$ Imm:147460 Imm:14 Reg:$ }
          if (MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP
           && MI.getOperand(2).isReg() && MI.getOperand(2).getReg() == ARM::SP) {
            int64_t AM2Shift = MI.getOperand(4).getImm();
            unsigned Imm12 = AM2Shift & 0xFFF;
            bool isSub = (AM2Shift >> 12) & 0x1;
            Monitor::event_raw() << "Found stack access: " << stack_offset << "\n";
            stack_offsets[stack_offset + MI.getOperand(3).getImm()] = Type::getInt32Ty(MF->getFunction().getContext());
            if (isSub)
              stack_offset -= Imm12;
            else
              stack_offset += Imm12;
            Monitor::event_raw() << "Changing stack offset: " << Imm12 << " to " << stack_offset << "\n";
          } break;
        case ARM::LDRi12: // 862
          if (MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(2).getImm()] = Type::getInt32Ty(MF->getFunction().getContext());
          } break;
        case ARM::MOVr: // 874
          if (MI.getOperand(0).getReg() == ARM::R11
           && MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Ignoring R11 = SP, assumed to be in stack setup\n";
           } break;
        case ARM::STMDB_UPD: // 1895
          Monitor::event_raw() << "Decrementing stack offset: 4 to " << (stack_offset-4) << "\n";
          stack_offset -= 4;
          stack_offsets[stack_offset] = Type::getInt32Ty(MF->getFunction().getContext());
          break;
        case ARM::STMIA: // 1896
          // Why does this exist?
          stack_offsets[stack_offset] = Type::getInt32Ty(MF->getFunction().getContext());
          Monitor::event_raw() << "Found stack access: " << stack_offset << "\n";
          break;
        case ARM::STRBi12: // 1906
          if (MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(2).getImm()] = Type::getInt8Ty(MF->getFunction().getContext());
          } break;
        case ARM::STRH: // 1915
          if(MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(3).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(3).getImm()] = Type::getInt16Ty(MF->getFunction().getContext());
          }
          break;
        case ARM::STR_PRE_IMM: // 1924
          if (MI.getOperand(2).isReg() && MI.getOperand(2).getReg() == ARM::SP) {
            stack_offset += MI.getOperand(3).getImm();
            stack_offsets[stack_offset] = Type::getInt32Ty(MF->getFunction().getContext());
            Monitor::event_raw() << "Found stack access: " << stack_offset << "\n";
          } break;
        case ARM::STRi12: // 1926
          if (MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(2).getImm()] = Type::getInt32Ty(MF->getFunction().getContext());
          } break;
        case ARM::SUBri: // 1928
          if (MI.getOperand(0).isReg() && MI.getOperand(0).getReg() == ARM::SP
           && MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP) {
            int64_t Op2 = MI.getOperand(2).getImm();
            unsigned Bits = Op2 & 0xFF;
            unsigned Rot = (Op2 & 0xF00) >> 7;
            int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
            Monitor::event_raw() << "Decrementing stack offset: " << Imm << " to " << (stack_offset-Imm) << "\n";
            stack_offset -= Imm;
          } break;
        case ARM::VLDMDIA_UPD: // 2778
          Monitor::event_raw() << "Incrementing stack offset: 8 to " << (stack_offset+8) << "\n";
          stack_offsets[stack_offset] = Type::getDoubleTy(MF->getFunction().getContext());
          stack_offset += 8;
          break;
        case ARM::VLDRD: // 2783
          Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
          stack_offsets[stack_offset + MI.getOperand(2).getImm()] = Type::getDoubleTy(MF->getFunction().getContext());
          break;
        case ARM::VSTMDDB_UPD: // 3763
          Monitor::event_raw() << "Decrementing stack offset: 4 to " << (stack_offset-8) << "\n";
          stack_offset -= 8;
          stack_offsets[stack_offset] = Type::getDoubleTy(MF->getFunction().getContext());
          break;
        case ARM::VSTRD: // 3770
          if (MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
            stack_offsets[stack_offset + MI.getOperand(2).getImm()] = Type::getDoubleTy(MF->getFunction().getContext());
          } break;
      }
    }
  }

  // Add parameters on the stack to the function prototype
  for (auto &stack_offset : stack_offsets) {
    int64_t offset = stack_offset.first;
    Type *type = stack_offset.second;

    if (offset < 0)
      continue;
    int idx = offset / 4 + 4;

    if (idx > 127) {
      Monitor::ERROR("Stack index exceeds 127, assumed to be accessing pass by value struct");
      continue;
    }

    if (idx > maxidx)
      maxidx = idx;
    tarr[idx] = type;
  }

  Monitor::event_raw() << "Found " << maxidx+1 << " parameters.\n";
  for (int i = 0; i <= maxidx; ++i) {
    if (tarr[i] == nullptr)
      paramTypes.push_back(getDefaultType());
    else
      paramTypes.push_back(tarr[i]);
  }
}

/// Get all arguments types of current MachineFunction.
bool ARMFunctionPrototype::isDefinedRegiser(unsigned reg,
                                            const MachineBasicBlock &mbb) {
  for (MachineBasicBlock::const_reverse_iterator ii = mbb.rbegin(),
                                                 ie = mbb.rend();
       ii != ie; ++ii) {
    const MachineInstr &mi = *ii;
    for (MachineInstr::const_mop_iterator oi = mi.operands_begin(),
                                          oe = mi.operands_end();
         oi != oe; oi++) {
      const MachineOperand &mo = *oi;
      if (mo.isReg() && (mo.getReg() == reg)) {
        // The return register must not be tied to another register.
        // If it was, it should not be return register.
        if (mo.isTied())
          return false;

        return mo.isDef();
      }
    }
  }

  return false;
}

/// Get return type of current MachineFunction.
Type *ARMFunctionPrototype::genReturnType() {
  // TODO: Need to track register liveness on CFG.
  Type *retTy;
  retTy = Type::getVoidTy(*CTX);
  for (const MachineBasicBlock &mbb : *MF) {
    if (mbb.succ_empty()) {
      if (isDefinedRegiser(ARM::R0, mbb)) {
        // TODO: Need to identify data type, int, long, float or double.
        retTy = getDefaultType();
        break;
      }
    }
  }

  return retTy;
}

Function *ARMFunctionPrototype::discover(MachineFunction &mf) {
  LLVM_DEBUG(dbgs() << "ARMFunctionPrototype start.\n");

  MF = &mf;
  Function &fn = const_cast<Function &>(mf.getFunction());
  CTX = &fn.getContext();

  std::vector<Type *> paramTys;
  genParameterTypes(paramTys);
  Type *retTy = genReturnType();
  FunctionType *fnTy = FunctionType::get(retTy, paramTys, false);

  MachineModuleInfo &mmi = mf.getMMI();
  Module *mdl = const_cast<Module *>(mmi.getModule());
  mdl->getFunctionList().remove(&fn);
  Function *pnfn =
      Function::Create(fnTy, GlobalValue::ExternalLinkage, fn.getName(), mdl);
  // When run as FunctionPass, the Function must not be empty, so add
  // EntryBlock at here.
  BasicBlock::Create(pnfn->getContext(), "EntryBlock", pnfn);

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(pnfn->dump());
  LLVM_DEBUG(dbgs() << "ARMFunctionPrototype end.\n");

  return pnfn;
}

bool ARMFunctionPrototype::runOnMachineFunction(MachineFunction &mf) {
  discover(mf);
  return true;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

MachineFunctionPass *InitializeARMFunctionPrototype() {
  return new ARMFunctionPrototype();
}

#ifdef __cplusplus
}
#endif
