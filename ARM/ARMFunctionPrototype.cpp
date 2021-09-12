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
bool ARMFunctionPrototype::isUsedRegiser(unsigned reg,
                                         const MachineBasicBlock &mbb) {
  for (MachineBasicBlock::const_iterator ii = mbb.begin(), ie = mbb.end();
       ii != ie; ++ii) {
    const MachineInstr &mi = *ii;
    for (MachineInstr::const_mop_iterator oi = mi.operands_begin(),
                                          oe = mi.operands_end();
         oi != oe; oi++) {
      const MachineOperand &mo = *oi;
      if (mo.isReg() && (mo.getReg() == reg))
        return mo.isUse();
    }
  }

  return false;
}

/// Check the first reference of the reg is DEF.
void ARMFunctionPrototype::genParameterTypes(std::vector<Type *> &paramTypes) {
  assert(!MF->empty() && "The function body is empty!!!");
  MF->getRegInfo().freezeReservedRegs(*MF);
  LivePhysRegs liveInPhysRegs;
  for (MachineBasicBlock &EMBB : *MF)
    computeAndAddLiveIns(liveInPhysRegs, EMBB);
  // Walk the CFG DFS to discover first register usage
  df_iterator_default_set<const MachineBasicBlock *, 16> Visited;
  DenseMap<unsigned, bool> ArgObtain;
  ArgObtain[ARM::R0] = false;
  ArgObtain[ARM::R1] = false;
  ArgObtain[ARM::R2] = false;
  ArgObtain[ARM::R3] = false;
  const MachineBasicBlock &fmbb = MF->front();
  DenseMap<int, Type *> tarr;
  int maxidx = -1; // When the maxidx is -1, means there is no argument.
  // Track register liveness on CFG.
  for (const MachineBasicBlock *Mbb : depth_first_ext(&fmbb, Visited)) {
    for (unsigned IReg = ARM::R0; IReg < ARM::R4; IReg++) {
      if (!ArgObtain[IReg] && Mbb->isLiveIn(IReg)) {
        for (MachineBasicBlock::const_iterator ii = Mbb->begin(),
                                               ie = Mbb->end();
             ii != ie; ++ii) {
          const MachineInstr &LMI = *ii;
          auto RUses = LMI.uses();
          auto ResIter =
              std::find_if(RUses.begin(), RUses.end(),
                           [IReg](const MachineOperand &OP) -> bool {
                             return OP.isReg() && (OP.getReg() == IReg);
                           });
          if (ResIter != RUses.end()) {
            int idx = IReg - ARM::R0;
            tarr[idx] = getDefaultType();
            if (maxidx < idx)
              maxidx = idx;
            Monitor::event_raw() << "Found reg " << idx << "; maxidx = " << maxidx << "\n";
            break;
          }
        }
        ArgObtain[IReg] = true;
      }
    }
  }

  // Because stack accesses are non-trivial, the only way to determine
  // stack arguments beforehand is via symbolic execution.
  // A better approach would be to adapt the function prototype on
  // the fly, but this is not feasible for now.
  
  // A somewhat stable way is to symbolically execute the stack access
  // related instructions and hope that no stack accesses are missed.
  int64_t stack_offset = 0;
  std::set<int64_t> stack_offsets;
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
          stack_offset += 4;
          break;
        case ARM::LDRi12: // 862
          if (MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
            stack_offsets.insert(stack_offset + MI.getOperand(2).getImm());
          } break;
        case ARM::STMDB_UPD: // 1895
          Monitor::event_raw() << "Decrementing stack offset: 4 to " << (stack_offset-4) << "\n";
          stack_offset -= 4;
          break;
        case ARM::STRi12: // 1926
          if (MI.getOperand(1).isReg() && MI.getOperand(1).getReg() == ARM::SP) {
            Monitor::event_raw() << "Found stack access: " << (stack_offset + MI.getOperand(2).getImm()) << "\n";
            stack_offsets.insert(stack_offset + MI.getOperand(2).getImm());
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
      }
    }
  }

  // Add parameters on the stack to the function prototype
  for (int64_t offset : stack_offsets) {
    if (offset < 0)
      continue;
    int idx = offset / 4 + 4;

    if (idx > 127) {
      Monitor::ERROR("Stack index exceeds 127, assumed to be accessing pass by value struct");
      continue;
    }

    if (idx > maxidx)
      maxidx = idx;
    tarr[idx] = getDefaultType();
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
