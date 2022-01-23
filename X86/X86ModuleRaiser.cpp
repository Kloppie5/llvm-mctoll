//===-- X86ModuleRaiser.cpp -------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of X86ModuleRaiser class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "X86ModuleRaiser.h"
#include "llvm/Object/ELFObjectFile.h"

using namespace llvm;

namespace RaiserContext {
extern SmallVector<ModuleRaiser *, 4> ModuleRaiserRegistry;
}

bool X86ModuleRaiser::collectDynamicRelocations() {
  if (!Obj->isELF())
    return false;

  const ELF64LEObjectFile *Elf64LEObjFile = dyn_cast<ELF64LEObjectFile>(Obj);
  if (!Elf64LEObjFile)
    return false;

  // Collect all relocation records from various relocation sections
  std::vector<SectionRef> DynRelSec = Obj->dynamic_relocation_sections();
  for (const SectionRef &Section : DynRelSec)
    for (const RelocationRef &Reloc : Section.relocations())
      DynRelocs.push_back(Reloc);

  return true;
}

// Create a new MachineFunctionRaiser object and add it to the list of
// MachineFunction raiser objects of this module.
MachineFunctionRaiser* X86ModuleRaiser::NewMachineFunctionRaiser(StringRef FunctionName, MCInstRaiser *MCIR) {
  FunctionType *FTy = FunctionType::get(Type::getVoidTy(M->getContext()), false);
  Function *Func = Function::Create(FTy, GlobalValue::ExternalLinkage, FunctionName, M);
  MachineFunction &MF = getMachineModuleInfo()->getOrCreateMachineFunction(*Func);
  MachineFunctionRaiser* mfRaiser = new X86MachineFunctionRaiser(MF, MCIR);
  mfRaiserVector.push_back(mfRaiser);
  return mfRaiser;
}

#ifdef __cplusplus
extern "C" {
#endif

void InitializeX86ModuleRaiser() {
  ModuleRaiser *m = new X86ModuleRaiser();
  RaiserContext::ModuleRaiserRegistry.push_back(m);
}

#ifdef __cplusplus
}
#endif
