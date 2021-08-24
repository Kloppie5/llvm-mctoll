//===-- ModuleRaiser.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_MODULERAISER_H
#define LLVM_TOOLS_LLVM_MCTOLL_MODULERAISER_H

#include "FunctionFilter.h"
#include "MCInstRaiser.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Target/TargetMachine.h"
#include <vector>

using namespace llvm;
using namespace object;

class MachineFunctionRaiser;
class MachineInstructionRaiser;

using JumpTableBlock = std::pair<ConstantInt *, MachineBasicBlock *>;

struct JumpTableInfo {
  /// The index of jump table in the function.
  unsigned jtIdx;

  /// The MachineBasicBlock which includes the jump table condition value.
  MachineBasicBlock *conditionMBB;

  /// The MachineBasicBlock which includes the default destination.
  MachineBasicBlock *df_MBB;
};

// The ModuleRaiser class encapsulates information needed to raise a given
// module.
class ModuleRaiser {
public:
  Module *M;
  FunctionFilter *FFT;

  ModuleRaiser()
      : M(nullptr), TM(nullptr), MMI(nullptr), MIA(nullptr), MII(nullptr),
        Obj(nullptr), DisAsm(nullptr), TextSectionIndex(-1),
        Arch(Triple::ArchType::UnknownArch), FFT(nullptr), InfoSet(false) {}

  static void InitializeAllModuleRaisers();

  void setModuleRaiserInfo(Module *M, const TargetMachine *TM,
                           MachineModuleInfo *MMI, const MCInstrAnalysis *MIA,
                           const MCInstrInfo *MII, const ObjectFile *Obj,
                           MCDisassembler *DisAsm) {
    assert((InfoSet == false) &&
           "Module Raiser information can be set only once");
    this->M = M;
    this->TM = TM;
    this->MMI = MMI;
    this->MIA = MIA;
    this->MII = MII;
    this->Obj = Obj;
    this->DisAsm = DisAsm;
    this->FFT = new FunctionFilter(*M);
    InfoSet = true;
  }

  // Function to create a MachineFunctionRaiser corresponding to Function f.
  // As noted elsewhere (llvm-mctoll.cpp), f is a place holder to allow for
  // creation of MachineFunction. The Function object representing raising
  // of MachineFunction is accessible by calling getRaisedFunction()
  // on the MachineFunctionRaiser object.
  virtual MachineFunctionRaiser* NewMachineFunctionRaiser(StringRef FunctionName, MCInstRaiser *MCIR) = 0;
  virtual FunctionType* getRaisedFunctionPrototype() = 0;

  MachineFunctionRaiser* getCurrentMachineFunctionRaiser() {
    if (mfRaiserVector.size() > 0)
      return mfRaiserVector.back();
    return nullptr;
  }

  bool collectTextSectionRelocs(const SectionRef &);
  virtual bool collectDynamicRelocations() = 0;

  /// Get the data type corresponding to type string.
  Type *getPrimitiveDataType(const StringRef &TypeStr);

  const TargetMachine *getTargetMachine() const { return TM; }
  MachineModuleInfo *getMachineModuleInfo() const { return MMI; }
  const MCInstrAnalysis *getMCInstrAnalysis() const { return MIA; }
  const MCInstrInfo *getMCInstrInfo() const { return MII; }
  const ObjectFile *getObjectFile() const { return Obj; }
  const MCDisassembler *getMCDisassembler() const { return DisAsm; }
  Triple::ArchType getArchType() { return Arch; }
  FunctionFilter *getFunctionFilter() const { return FFT; }


  bool runMachineFunctionPasses();

  // Return the Function * corresponding to input binary function with
  // start offset equal to that specified as argument. This returns the pointer
  // to raised function, if one was constructed; else returns nullptr.
  Function *getRaisedFunctionAt(uint64_t) const;

  // Return the Function * corresponding to input binary function from
  // text relocation record with off set in the range [Loc, Loc+Size].
  Function *getCalledFunctionUsingTextReloc(uint64_t Loc, uint64_t Size) const;

  // Get dynamic relocation with offset 'O'
  const RelocationRef *getDynRelocAtOffset(uint64_t O) const;

  // Return text relocation of instruction at index 'I'. 'S' is the size of the
  // instruction at index 'I'.
  const RelocationRef *getTextRelocAtOffset(uint64_t I, uint64_t S) const;

  int64_t getTextSectionAddress() const;

  bool changeRaisedFunctionReturnType(Function *, Type *);

protected:
  // A sequential list of MachineFunctionRaiser objects created
  // as the instructions of the input binary are parsed. Each of
  // these correspond to a "machine function". A machine function
  // corresponds to a sequence of instructions (possibly interspersed
  // by data bytes) whose start is denoted by a function symbol in
  // the binary.
  std::vector<MachineFunctionRaiser *> mfRaiserVector;
  // Sorted vector of text relocations
  std::vector<RelocationRef> TextRelocs;
  // Vector of dynamic relocation records
  std::vector<RelocationRef> DynRelocs;

  // Commonly used data structures
  const TargetMachine *TM;
  MachineModuleInfo *MMI;
  const MCInstrAnalysis *MIA;
  const MCInstrInfo *MII;
  const ObjectFile *Obj;
  MCDisassembler *DisAsm;
  // Index of text section whose instructions are raised
  int64_t TextSectionIndex;
  Triple::ArchType Arch;
  // Flag to indicate that fields are set. Resetting is not allowed/expected.
  bool InfoSet;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_MODULERAISER_H
