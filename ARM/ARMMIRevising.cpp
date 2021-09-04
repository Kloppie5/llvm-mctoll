//===- ARMMIRevising.cpp - Binary raiser utility llvm-mctoll --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of ARMMIRevising class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "ARMMIRevising.h"
#include "Monitor.h"
#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "ExternalFunctions.h"
#include "MCInstRaiser.h"
#include "MachineFunctionRaiser.h"
#include "llvm-mctoll.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;
using namespace llvm::object;

bool ARMMIRevising::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMMIRevising");
  LLVM_DEBUG(dbgs() << "ARMMIRevising start.\n");

  bool rtn = false;

  vector<MachineInstr *> RMVec;
  for (MachineFunction::iterator mbbi = MF->begin(), mbbe = MF->end();
       mbbi != mbbe; ++mbbi) {
    for (MachineBasicBlock::iterator mii = mbbi->begin(), mie = mbbi->end();
         mii != mie; ++mii) {
      if (removeNeedlessInst(&*mii)) {
        RMVec.push_back(&*mii);
        rtn = true;
      } else
        rtn = reviseMI(*mii);
    }
  }

  for (MachineInstr *PMI : RMVec)
    PMI->eraseFromParent();

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMMIRevising end.\n");
  Monitor::event_end("ARMMIRevising");
  return rtn;
}

// Extract the offset of MachineInstr MI from the Metadata operand.
static uint64_t getMCInstIndex(const MachineInstr &MI) {
  unsigned NumExpOps = MI.getNumExplicitOperands();
  const MachineOperand &MO = MI.getOperand(NumExpOps);
  assert(MO.isMetadata() &&
         "Unexpected non-metadata operand in branch instruction!");
  const MDNode *MDN = MO.getMetadata();
  // Unwrap metadata of the instruction to get the MCInstIndex of
  // the MCInst corresponding to this MachineInstr.
  ConstantAsMetadata *CAM = dyn_cast<ConstantAsMetadata>(MDN->getOperand(0));
  assert(CAM != nullptr && "Unexpected metadata type!");
  Constant *CV = CAM->getValue();
  ConstantInt *CI = dyn_cast<ConstantInt>(CV);
  assert(CI != nullptr && "Unexpected metadata constant type!");
  APInt ArbPrecInt = CI->getValue();
  return ArbPrecInt.getSExtValue();
}

template <class ELFT>
uint64_t getLoadAlignProgramHeader(const ELFFile<ELFT> *Obj) {
  typedef ELFFile<ELFT> ELFO;
  auto ProgramHeaderOrError = Obj->program_headers();

  if (!ProgramHeaderOrError)
    report_fatal_error(
        errorToErrorCode(ProgramHeaderOrError.takeError()).message());

  for (const typename ELFO::Elf_Phdr &Phdr : *ProgramHeaderOrError) {
    if (Phdr.p_type == ELF::PT_LOAD)
      return (uint64_t)Phdr.p_align;
  }

  assert(false && "Failed to get Phdr p_align!");
  return 0;
}

/// Find global value by PC offset.
const GlobalValue *ARMMIRevising::getGlobalValueByOffset(int64_t MCInstOffset,
                                                   uint64_t PCOffset) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser&>(MR);
  const GlobalValue *GlobVal = nullptr;
  const ELF32LEObjectFile *ObjFile =
      dyn_cast<ELF32LEObjectFile>(AMR.getObjectFile());
  assert(ObjFile != nullptr &&
         "Only 32-bit ELF binaries supported at present.");

  // Get the text section address
  int64_t TextSecAddr = AMR.getTextSectionAddress();
  assert(TextSecAddr >= 0 && "Failed to find text section address");

  uint64_t InstAddr = TextSecAddr + MCInstOffset;
  uint64_t Offset = InstAddr + PCOffset;

  // Start to search the corresponding symbol.
  const SymbolRef *Symbol = nullptr;
  const RelocationRef *DynReloc = AMR.getDynRelocAtOffset(Offset);
  if (DynReloc && (DynReloc->getType() == ELF::R_ARM_ABS32 ||
                   DynReloc->getType() == ELF::R_ARM_GLOB_DAT))
    Symbol = &*DynReloc->getSymbol();

  assert(MCIR != nullptr && "MCInstRaiser was not initialized!");
  if (Symbol == nullptr) {
    auto Iter = MCIR->getMCInstAt(Offset - TextSecAddr);
    uint64_t OffVal = static_cast<uint64_t>((*Iter).second.getData());

    for (auto &Sym : ObjFile->symbols()) {
      if (Sym.getELFType() == ELF::STT_OBJECT) {
        auto SymAddr = Sym.getAddress();
        assert(SymAddr && "Failed to lookup symbol for global address!");

        if (OffVal >= SymAddr.get() &&
            OffVal < (SymAddr.get() + Sym.getSize())) {
          Symbol = &Sym;
          break;
        }
      }
    }
  }

  LLVMContext &LCTX = MR.getModule()->getContext();
  if (Symbol != nullptr) {
    // If the symbol is found.
    Expected<StringRef> SymNameVal = Symbol->getName();
    assert(SymNameVal &&
           "Failed to find symbol associated with dynamic relocation.");
    auto SymName = SymNameVal.get();
    GlobVal = MR.getModule()->getGlobalVariable(SymName);
    if (GlobVal == nullptr) {
      DataRefImpl SymImpl = Symbol->getRawDataRefImpl();
      auto SymbOrErr = ObjFile->getSymbol(SymImpl);
      if (!SymbOrErr)
        consumeError(SymbOrErr.takeError());
      else {
        auto Symb = SymbOrErr.get();
        assert((Symb->getType() == ELF::STT_OBJECT) &&
               "Object symbol type is expected. But not found!");
        GlobalValue::LinkageTypes Linkage;
        switch (Symb->getBinding()) {
        case ELF::STB_GLOBAL:
          Linkage = GlobalValue::ExternalLinkage;
          break;
        default:
          assert(false && "Unhandled dynamic symbol");
        }
        uint64_t SymSz = Symb->st_size;
        Type *GlobValTy = nullptr;
        switch (SymSz) {
        case 4:
          GlobValTy = Type::getInt32Ty(LCTX);
          break;
        case 2:
          GlobValTy = Type::getInt16Ty(LCTX);
          break;
        case 1:
          GlobValTy = Type::getInt8Ty(LCTX);
          break;
        default:
          GlobValTy = ArrayType::get(Type::getInt8Ty(LCTX), SymSz);
          break;
        }

        auto SymOrErr = Symbol->getValue();
        if (!SymOrErr)
          report_error(SymOrErr.takeError(), "Can not find the symbol!");

        uint64_t SymVirtAddr = *SymOrErr;
        auto SecOrErr = Symbol->getSection();
        if (!SecOrErr)
          report_error(SecOrErr.takeError(),
                       "Can not find the section which is the symbol in!");

        section_iterator SecIter = *SecOrErr;
        Constant *GlobInit = nullptr;
        if (SecIter->isBSS()) {
          Linkage = GlobalValue::CommonLinkage;
          if (ArrayType::classof(GlobValTy))
            GlobInit = ConstantAggregateZero::get(GlobValTy);
          else
            GlobInit = ConstantInt::get(GlobValTy, 0);
        } else {
          auto StrOrErr = SecIter->getContents();
          if (!StrOrErr)
            report_error(StrOrErr.takeError(),
                         "Failed to get the content of section!");
          StringRef SecData = *StrOrErr;
          // Currently, Symbol->getValue() is virtual address.
          unsigned Index = SymVirtAddr - SecIter->getAddress();
          const unsigned char *Beg = SecData.bytes_begin() + Index;
          char Shift = 0;
          uint64_t InitVal = 0;
          while (SymSz-- > 0) {
            // We know this is little-endian
            InitVal = ((*Beg++) << Shift) | InitVal;
            Shift += 8;
          }
          GlobInit = ConstantInt::get(GlobValTy, InitVal);
        }

        GlobVal = new GlobalVariable(*MR.getModule(), GlobValTy, false /* isConstant */,
                                          Linkage, GlobInit, SymName);
      }
    }
  } else {
    // If can not find the corresponding symbol.
    const Value *ROVal = AMR.getRODataValueAt(Offset);
    if (ROVal != nullptr) {
      GlobVal = dyn_cast<GlobalVariable>(ROVal);
      assert(GlobVal && "Failed to cast the value to global variable!");
    } else {
      uint64_t Index = Offset - TextSecAddr;
      if (MCIR->getMCInstAt(Index) != MCIR->const_mcinstr_end()) {
        std::string LocalName("ROConst");
        LocalName.append(std::to_string(Index));
        // Find if a global value associated with symbol name is already
        // created
        StringRef LocalNameRef(LocalName);
        GlobVal = MR.getModule()->getGlobalVariable(LocalNameRef);
        if (GlobVal == nullptr) {
          MCInstOrData MD = MCIR->getMCInstAt(Index)->second;
          uint32_t Data = MD.getData();
          uint64_t DataAddr = (uint64_t)Data;
          // Check if this is an address in .rodata
          for (section_iterator SecIter : ObjFile->sections()) {
            uint64_t SecStart = SecIter->getAddress();
            uint64_t SecEnd = SecStart + SecIter->getSize();

            if ((SecStart <= DataAddr) && (SecEnd >= DataAddr)) {
              if (SecIter->isData()) {
                auto StrOrErr = SecIter->getContents();
                assert(StrOrErr && "Failed to get the content of section!");
                StringRef SecData = *StrOrErr;
                uint64_t DataOffset = DataAddr - SecStart;
                const unsigned char *RODataBegin =
                    SecData.bytes_begin() + DataOffset;

                unsigned char c;
                uint64_t argNum = 0;
                const unsigned char *str = RODataBegin;
                do {
                  c = (unsigned char)*str++;
                  if (c == '%') {
                    argNum++;
                  }
                } while (c != '\0');
                if (argNum != 0) {
                  AMR.collectRodataInstAddr(InstAddr);
                  AMR.fillInstArgMap(InstAddr, argNum + 1);
                }
                StringRef ROStringRef(
                    reinterpret_cast<const char *>(RODataBegin));
                Constant *StrConstant =
                    ConstantDataArray::getString(LCTX, ROStringRef);
                GlobalValue *GlobalStrConstVal = new GlobalVariable(
                    *MR.getModule(), StrConstant->getType(), /* isConstant */ true,
                    GlobalValue::PrivateLinkage, StrConstant, "RO-String");
                // Record the mapping between offset and global value
                AMR.addRODataValueAt(GlobalStrConstVal, Offset);
                GlobVal = GlobalStrConstVal;
                break;
              }
            }
          }

          if (GlobVal == nullptr) {
            Type *ty = Type::getInt32Ty(LCTX);
            Constant *GlobInit = ConstantInt::get(ty, Data);
            GlobVal = new GlobalVariable(*MR.getModule(), ty, /* isConstant */ true,
                                              GlobalValue::PrivateLinkage,
                                              GlobInit, LocalNameRef);
          }
        }
      }
    }
  }

  return GlobVal;
}

/// Address PC relative data in function, and create corresponding global value.
void ARMMIRevising::addressPCRelativeData(MachineInstr &MInst) {
  const GlobalValue *GlobVal = nullptr;
  int64_t Imm = 0;
  // To match the pattern: OPCODE Rx, [PC, #IMM]
  if (MInst.getNumOperands() > 2) {
    assert(MInst.getOperand(2).isImm() &&
           "The third operand must be immediate data!");
    Imm = MInst.getOperand(2).getImm();
  }
  // Get MCInst offset - the offset of machine instruction in the binary
  // and instruction size
  int64_t MCInstOffset = getMCInstIndex(MInst);
  GlobVal = getGlobalValueByOffset(MCInstOffset, static_cast<uint64_t>(Imm) + 8);

  assert(GlobVal && "A not addressed pc-relative data!");

  MachineInstr *OldMI = MInst.getParent()->getParent()->CloneMachineInstr(&MInst);
  MInst.getParent()->insertAfter(MInst.getIterator(), OldMI);
  // Replace PC relative operands to symbol operand.
  // The pattern will be generated.
  // ldr r3, [pc, #20] => ldr r3, @globalvalue
  MInst.getOperand(1).ChangeToGA(GlobVal, 0);
  MInst.getOperand(2).setImm(0);
  Monitor::event_MachineInstrsToMachineInstrs("Revised", {OldMI}, {&MInst});
  OldMI->removeFromParent();
}

/// Decode modified immediate constants in some instructions with immediate
/// operand.
void ARMMIRevising::decodeModImmOperand(MachineInstr &MInst) {
  switch (MInst.getOpcode()) {
    default:
      break;
    case ARM::ORRri: {
      MachineOperand &mo = MInst.getOperand(2);
      unsigned Bits = mo.getImm() & 0xFF;
      unsigned Rot = (mo.getImm() & 0xF00) >> 7;
      int64_t Rotated = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      mo.setImm(Rotated);
    } break;
  }
}

/// Remove some useless operations of instructions. Some instructions like
/// NOP (mov r0, r0).
bool ARMMIRevising::removeNeedlessInst(MachineInstr *MInst) {
  if (MInst->getOpcode() == ARM::MOVr && MInst->getNumOperands() >= 2 &&
      MInst->getOperand(0).isReg() && MInst->getOperand(1).isReg() &&
      MInst->getOperand(0).getReg() == MInst->getOperand(1).getReg()) {
    return true;
  }

  return false;
}

/// The entry function of this class.
bool ARMMIRevising::reviseMI(MachineInstr &MInst) {
  decodeModImmOperand(MInst);
  
  if (MInst.getOpcode() == ARM::LDRi12 || MInst.getOpcode() == ARM::STRi12) {
    if (MInst.getNumOperands() >= 2 && MInst.getOperand(1).isReg() &&
        MInst.getOperand(1).getReg() == ARM::PC) {
      addressPCRelativeData(MInst);
    }
  }

  return true;
}

#undef DEBUG_TYPE
