
#include "ARMLinearRaiserPass.h"

#include "Monitor.h"

#include "ARMModuleRaiser.h"
#include "ARMSubtarget.h"
#include "ExternalFunctions.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"

using namespace llvm;

#define DEBUG_TYPE "mctoll"

bool ARMLinearRaiserPass::run(MachineFunction* MF, Function* F) {
  Monitor::event_start("ARMLinearRaiserPass");
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass start.\n");

  this->MF = MF;
  this->F = F;

  // Set up entry block.
  BasicBlock* EBB = &F->getEntryBlock();
  MachineBasicBlock* MBB = &MF->front();
  MBBBBMap[MBB].push_back(EBB);
  BBStateMap[EBB] = new ARMBasicBlockState(EBB);
  EBB->setName("entry");
  BBStateMap[EBB]->FP_offset = 0;
  BBStateMap[EBB]->setRegValue(ARM::R11, ConstantInt::get(Type::getInt32Ty(F->getContext()), -1));
  BBStateMap[EBB]->SP_offset = 0;
  BBStateMap[EBB]->setRegValue(ARM::SP, ConstantInt::get(Type::getInt32Ty(F->getContext()), -1));

  // Add arguments to RegValueMap
  for (unsigned i = 0; i < 4 && i < F->arg_size(); ++i) {
    Register reg = ARM::R0 + i;
    Argument* arg = F->arg_begin() + i;
    Monitor::event_raw() << "Add argument " << arg->getName() << " to RegValueMap reg " << reg << "\n";
    setRegValue(reg, arg, EBB);
  }

  // Add excess arguments to StackArgMap
  for (unsigned i = 4; i < F->arg_size(); ++i) {
    Argument* arg = F->arg_begin() + i;
    Monitor::event_raw() << "Add argument " << arg->getName() << " to StackArgMap at offset " << ((i-4)*4) << "\n";
    AllocaInst* alloca = new AllocaInst(arg->getType(), 0, "Argument_" + Twine((i-4)*4), EBB);
    Monitor::event_Instruction(alloca);
    Instruction* store = new StoreInst(arg, alloca, false, EBB);
    Monitor::event_Instruction(store);
    stack_map[(i-4)*4] = alloca;
  }

  std::vector<MachineInstr* > Worklist;
  for (MachineBasicBlock &MBB : *MF)
    for (MachineInstr &MI : MBB)
      raiseMachineInstr(&MI);

  for (MachineBasicBlock &MBB : *MF) {
    if (MBB.succ_size() == 1 && MBB.terminators().empty()) { // For fallthrough blocks
      BasicBlock* BB = MBBBBMap[&MBB].back();
      Monitor::event_raw() << "LinearRaiser: add terminator to " << BB->getName() << "\n";
      auto succ_itt = MBB.succ_begin();
      BasicBlock* BranchBB = getBasicBlocks(*succ_itt).front();
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
      bool changed = BBStateMap[BB]->updatePHINodes(PBB, BBStateMap[PBB]);
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
        ReturnInst::Create(Context, getRegValue(ARM::R0, F->getReturnType(), &BB), &BB);
    }
  }

  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass end.\n");
  Monitor::event_end("ARMLinearRaiserPass");
  return true;
}

Value* ARMLinearRaiserPass::getRegValue(Register Reg, Type* Ty, BasicBlock* BB) {
  assert(BBStateMap.count(BB) && "BBStateMap does not contain BB");
  assert(Reg != ARM::PC && "getRegValue: PC is not allowed");
  assert((Reg < ARM::D0 || Reg > ARM::D31) && "getRegValue: D register is not allowed");
  Value* V = BBStateMap[BB]->getRegValue(Reg, Ty->isPointerTy() ? Type::getInt32Ty(Context) : Ty);
  {auto &OS=Monitor::event_raw(); OS << "getRegValue: " << Reg << ": "; if(Ty) {Ty->print(OS); OS << " <= ";} V->getType()->print(OS); OS << "\n";}
  if (!Ty || V->getType() == Ty)
    return V;
  if (!V->getType()->isPointerTy() && Ty->isPointerTy()) {
    Instruction* Cast = new IntToPtrInst(V, Ty, "Cast", BB);
    Monitor::event_Instruction(Cast);
    return Cast;
  }

  Instruction* Cast = new BitCastInst(V, Ty, "Cast", BB);
  Monitor::event_Instruction(Cast);
  return Cast;
}
void ARMLinearRaiserPass::setRegValue(Register Reg, Value* V, BasicBlock* BB) {
  {auto &OS=Monitor::event_raw(); OS << "setRegValue: " << Reg << ": "; V->getType()->print(OS); OS << "\n";}
  assert(Reg != ARM::SP && "getRegValue: SP is not allowed");
  assert(Reg != ARM::PC && "getRegValue: PC is not allowed");
  assert((Reg < ARM::D0 || Reg > ARM::D31) && "getRegValue: D register is not allowed");
  assert((V->getType() == Type::getInt32Ty(Context) || V->getType() == Type::getFloatTy(Context) || V->getType() == Type::getDoubleTy(Context))
         && "Value must be of type i32 or float");
  if (Reg == ARM::R11) BBStateMap[BB]->R11_is_FP = false;
  BBStateMap[BB]->setRegValue(Reg, V);
}
Value* ARMLinearRaiserPass::getDRegValueF64(Register DReg, BasicBlock* BB) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "DReg must be a D register");
  std::pair<Register, Register> SRegs = splitARMv7DRegister(DReg);

  Value* V1 = getRegValue(SRegs.first, Type::getInt32Ty(Context), BB);
  Value* V2 = getRegValue(SRegs.second, Type::getInt32Ty(Context), BB);

  Instruction* ZExt1 = new ZExtInst(V1, Type::getInt64Ty(Context), "", BB);
  Monitor::event_Instruction(ZExt1);
  Instruction* ZExt2 = new ZExtInst(V2, Type::getInt64Ty(Context), "", BB);
  Monitor::event_Instruction(ZExt2);
  Instruction* Shl = BinaryOperator::Create(Instruction::Shl, ZExt1, ConstantInt::get(Type::getInt64Ty(Context), 32), "", BB);
  Monitor::event_Instruction(Shl);
  Instruction* Add = BinaryOperator::Create(Instruction::Add, Shl, ZExt2, "", BB);
  Monitor::event_Instruction(Add);

  Instruction* Cast = new BitCastInst(Add, Type::getDoubleTy(Context), "", BB);
  Monitor::event_Instruction(Cast);

  return Cast;
}
std::pair<Value*, Value*> ARMLinearRaiserPass::getDRegValueV2F32(Register DReg, BasicBlock* BB) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "DReg must be a D register");
  std::pair<Register, Register> SRegs = splitARMv7DRegister(DReg);

  Value* V1 = getRegValue(SRegs.first, Type::getFloatTy(Context), BB);
  Value* V2 = getRegValue(SRegs.second, Type::getFloatTy(Context), BB);

  return std::make_pair(V1, V2);
}
std::pair<Value*, Value*> ARMLinearRaiserPass::getDRegValueV2I32(Register DReg, BasicBlock* BB) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "DReg must be a D register");
  std::pair<Register, Register> SRegs = splitARMv7DRegister(DReg);

  Value* V1 = getRegValue(SRegs.first, Type::getInt32Ty(Context), BB);
  Value* V2 = getRegValue(SRegs.second, Type::getInt32Ty(Context), BB);

  return std::make_pair(V1, V2);
}
void ARMLinearRaiserPass::setDRegValueF64(Register DReg, Value* V, BasicBlock* BB) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "setDRegValue: DReg must be a D register");
  std::pair<Register, Register> SRegs = splitARMv7DRegister(DReg);

  Instruction* Cast = new BitCastInst(V, Type::getInt64Ty(Context), "", BB);
  Monitor::event_Instruction(Cast);

  Instruction* Split = BinaryOperator::Create(Instruction::LShr, Cast, ConstantInt::get(Type::getInt64Ty(Context), 32), "", BB);
  Monitor::event_Instruction(Split);
  Instruction* Split1 = new TruncInst(Cast, Type::getInt32Ty(Context), "", BB);
  Monitor::event_Instruction(Split1);
  Instruction* Split2 = new TruncInst(Split, Type::getInt32Ty(Context), "", BB);
  Monitor::event_Instruction(Split2);

  setRegValue(SRegs.first, Split1, BB);
  Monitor::event_raw() << "Setting " << SRegs.first << " to " << Split1 << "\n";

  setRegValue(SRegs.second, Split2, BB);
  Monitor::event_raw() << "Setting " << SRegs.second << " to " << Split2 << "\n";
}
void ARMLinearRaiserPass::setDRegValueV2F32(Register DReg, Value* V1, Value* V2, BasicBlock* BB) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "setDRegValue: DReg must be a D register");
  std::pair<Register, Register> SRegs = splitARMv7DRegister(DReg);

  Instruction* Cast1 = new BitCastInst(V1, Type::getInt32Ty(Context), "", BB);
  Monitor::event_Instruction(Cast1);
  Instruction* Cast2 = new BitCastInst(V2, Type::getInt32Ty(Context), "", BB);
  Monitor::event_Instruction(Cast2);

  setRegValue(SRegs.first, Cast1, BB);
  Monitor::event_raw() << "Setting " << SRegs.first << " to " << Cast1 << "\n";

  setRegValue(SRegs.second, Cast2, BB);
  Monitor::event_raw() << "Setting " << SRegs.second << " to " << Cast2 << "\n";
}
void ARMLinearRaiserPass::setDRegValueV2I32(Register DReg, Value* V1, Value* V2, BasicBlock* BB) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "setDRegValue: DReg must be a D register");
  std::pair<Register, Register> SRegs = splitARMv7DRegister(DReg);

  setRegValue(SRegs.first, V1, BB);
  Monitor::event_raw() << "Setting " << SRegs.first << " to " << V1 << "\n";

  setRegValue(SRegs.second, V2, BB);
  Monitor::event_raw() << "Setting " << SRegs.second << " to " << V2 << "\n";
}

std::pair<Register, Register> ARMLinearRaiserPass::splitARMv7DRegister(Register DReg) {
  assert(DReg >= ARM::D0 && DReg <= ARM::D31 && "DReg must be a D register");
  Register S1 = Register(ARM::S0 + (DReg - ARM::D0) / 2);
  Register S2 = Register(ARM::S0 + (DReg - ARM::D0) / 2 + 1);
  Monitor::event_raw() << "splitARMv7DRegister: " << DReg << ": " << S1 << " " << S2 << "\n";
  return std::make_pair(S1, S2);
}

AllocaInst* ARMLinearRaiserPass::getOrCreateStackAlloca(Register Reg, int64_t Offset, Type* Ty, BasicBlock* BB) {
  if (Reg == ARM::SP)
    return getOrCreateStackAlloca(BBStateMap[BB]->SP_offset + Offset, Ty, BB);
  if (Reg == ARM::R11 && BBStateMap[BB]->R11_is_FP)
    return getOrCreateStackAlloca(BBStateMap[BB]->FP_offset + Offset, Ty, BB);
  if (Reg == ARM::R11 && !BBStateMap[BB]->R11_is_FP)
    assert(false && "getOrCreateStackAlloca: R11 was used after being invalidated");

  assert(false && "getOrCreateStackAlloca: only stack registers are allowed");
}
AllocaInst* ARMLinearRaiserPass::getOrCreateStackAlloca(int64_t offset, Type* Ty, BasicBlock* BB) {
  {auto &OS=Monitor::event_raw(); OS << "getOrCreateStackAlloca: offset " << offset << ", "; Ty->print(OS); OS << "\n";}
  auto I = stack_map.find(offset);
  if (I != stack_map.end()) {
    AllocaInst* alloca = I->second;
    if (alloca->getType()->getPointerElementType() != Ty) {
      {auto &OS=Monitor::event_raw(); OS << "getOrCreateStackAlloca: type mismatch, found type "; alloca->getType()->print(OS); OS << ", creating new alloca of type "; Ty->print(OS); OS << "\n";}
      AllocaInst* newalloca = new AllocaInst(Ty, 0, "StackAlloca_" + Twine(offset) + "|", &*F->getEntryBlock().getFirstInsertionPt());
      Monitor::event_Instruction(newalloca);
      stack_map[offset] = newalloca;
      return newalloca;
    }
    return alloca;
  }

  {auto &OS=Monitor::event_raw(); OS << "getOrCreateStackAlloca: creating new StackValue of type "; Ty->print(OS); OS << "\n";}
  if (F->getEntryBlock().getFirstInsertionPt() == F->getEntryBlock().end()) {
    Monitor::event_raw() << "getOrCreateStackAlloca: no insertion point found\n";
    AllocaInst* alloca = new AllocaInst(Ty, 0, "StackAlloca_" + Twine(offset) + "|", &F->getEntryBlock());
    Monitor::event_Instruction(alloca);
    stack_map[offset] = alloca;
    return alloca;
  }
  AllocaInst* alloca = new AllocaInst(Ty, 0, "StackAlloca_" + Twine(offset) + "|", &*F->getEntryBlock().getFirstInsertionPt());
  Monitor::event_Instruction(alloca);
  stack_map[offset] = alloca;
  return alloca;
}

Value* ARMLinearRaiserPass::resolveAM2Shift(Register Rn, Register Rs, Register Rm, int64_t AM2Shift, BasicBlock* BB) {
  unsigned Imm12 = AM2Shift & 0xFFF;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) ((AM2Shift >> 13) & 0x7);
  bool isSub = (AM2Shift >> 12) & 0x1;
  ARMII::IndexMode IdxMode = (ARMII::IndexMode) (AM2Shift >> 16);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "assuming default shift for now");
  }

  Value* Val = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* Offset;

  if (Rm != 0) {
    Offset = getRegValue(Rm, Type::getInt32Ty(Context), BB);
    if (!(ShiftOpcode == ARM_AM::lsr && Imm12 == 0)) {
      Instruction* Shift = BinaryOperator::Create(ShiftOp, Offset, ConstantInt::get(Type::getInt32Ty(Context), Imm12), "AM2Shift", BB);
      Monitor::event_Instruction(Shift);
      Offset = Shift;
    }
  } else {
    Offset = ConstantInt::get(Type::getInt32Ty(Context), Imm12);
  }

  Instruction* Add = BinaryOperator::Create(isSub ? Instruction::Sub : Instruction::Add, Val, Offset, "AM2Shift", BB);
  Monitor::event_Instruction(Add);

  if (Rs != 0)
    setRegValue(Rs, Add, BB);

  if (IdxMode == ARMII::IndexModeNone || IdxMode == ARMII::IndexModePre)
    return Add;
  if (IdxMode == ARMII::IndexModePost)
    return Val;

  assert(false && "AM2Shift: invalid index mode");
}

// TODO: merge with X86MachineInstructionRaiser::createPCRelativeAccesssValue
GlobalValue* ARMLinearRaiserPass::getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset, Type* Ty) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser&>(MR);
  GlobalValue* GlobVal = nullptr;
  const ELF32LEObjectFile* ObjFile =
      dyn_cast<ELF32LEObjectFile>(AMR.getObjectFile());
  assert(ObjFile != nullptr &&
         "Only 32-bit ELF binaries supported at present.");

  // Get the text section address
  int64_t TextSecAddr = AMR.getTextSectionAddress();
  assert(TextSecAddr >= 0 && "Failed to find text section address");

  uint64_t InstAddr = TextSecAddr + MCInstOffset;
  uint64_t Offset = InstAddr + PCOffset;
  uint64_t Target = MCInstOffset + PCOffset;

  // Start to search the corresponding symbol.
  const SymbolRef* Symbol = nullptr;
  const RelocationRef* DynReloc = AMR.getDynRelocAtOffset(Offset);
  if (DynReloc && (DynReloc->getType() == ELF::R_ARM_ABS32 ||
                   DynReloc->getType() == ELF::R_ARM_GLOB_DAT)) {
    Symbol = &*DynReloc->getSymbol();
    Monitor::event_raw() << "Found DynReloc offset " << DynReloc->getOffset() << "\n";
  }

  assert(MCIR != nullptr && "MCInstRaiser was not initialized!");
  if (Symbol == nullptr) {
    auto Iter = MCIR->getMCInstAt(Target);
    uint64_t OffVal = static_cast<uint64_t>((*Iter).second.getData());

    for (auto &Sym : ObjFile->symbols()) {
      if (Sym.getELFType() == ELF::STT_OBJECT) {
        auto SymAddr = Sym.getAddress();
        assert(SymAddr && "Failed to lookup symbol for global address!");

        if (OffVal >= SymAddr.get() &&
            OffVal < (SymAddr.get() + Sym.getSize())) {
          Monitor::event_raw() << "Found symbol at " << SymAddr.get() << "-" << (SymAddr.get() + Sym.getSize()) << ": " << OffVal << "\n";
          Symbol = &Sym;
          break;
        }
      }
    }
  }

  if (Symbol != nullptr) {
    // If the symbol is found.
    Expected<StringRef> SymNameVal = Symbol->getName();
    assert(SymNameVal &&
           "Failed to find symbol associated with dynamic relocation.");
    auto SymName = SymNameVal.get();
    Monitor::event_raw() << "Found symbol " << SymName << "\n";
    GlobVal = MR.getModule()->getGlobalVariable(SymName);
    if (GlobVal != nullptr)
      return GlobVal;

    Monitor::event_raw() << "Symbol is unregistered; creating GlobalValue\n";
    DataRefImpl SymImpl = Symbol->getRawDataRefImpl();
    auto SymbOrErr = ObjFile->getSymbol(SymImpl);
    assert(SymbOrErr && "Failed to find symbol!");
    auto Symb = SymbOrErr.get();
    assert((Symb->getType() == ELF::STT_OBJECT) &&
            "Object symbol type is expected. But not found!");
    GlobalValue::LinkageTypes Linkage;
    switch (Symb->getBinding()) {
      case ELF::STB_LOCAL:
        Linkage = GlobalValue::InternalLinkage;
        break;
      case ELF::STB_GLOBAL:
        Linkage = GlobalValue::ExternalLinkage;
        break;
      default:
        Monitor::event_raw() << "Unsupported symbol binding: " << ((int) Symb->st_info) << "\n";
        assert(false && "Unhandled dynamic symbol");
    }
    uint64_t SymSz = Symb->st_size;
    Type* GlobValTy = nullptr;
    switch (SymSz) {
      case 4:
        GlobValTy = Type::getInt32Ty(Context);
        break;
      case 2:
        GlobValTy = Type::getInt16Ty(Context);
        break;
      case 1:
        GlobValTy = Type::getInt8Ty(Context);
        break;
      default:
        GlobValTy = ArrayType::get(Type::getInt8Ty(Context), SymSz);
        break;
    }

    auto SymOrErr = Symbol->getValue();
    assert (SymOrErr && "Can not find the symbol!");

    uint64_t SymVirtAddr =* SymOrErr;
    auto SecOrErr = Symbol->getSection();
    assert(SecOrErr && "Can not find the section which is the symbol in!");

    section_iterator SecIter =* SecOrErr;
    Constant* GlobInit = nullptr;
    if (SecIter->isBSS()) {
      Monitor::event_raw() << "Symbol in BSS section\n";
      Linkage = GlobalValue::CommonLinkage;
      if (ArrayType::classof(GlobValTy))
        GlobInit = ConstantAggregateZero::get(GlobValTy);
      else
        GlobInit = ConstantInt::get(GlobValTy, 0);
    } else {
      auto StrOrErr = SecIter->getContents();
      assert (StrOrErr && "Failed to get the content of section!");
      StringRef SecData =* StrOrErr;
      // Currently, Symbol->getValue() is virtual address.
      unsigned Index = SymVirtAddr - SecIter->getAddress();
      const unsigned char* Beg = SecData.bytes_begin() + Index;
      char Shift = 0;
      uint64_t InitVal = 0;
      while (SymSz-- > 0) {
        // We know this is little-endian
        InitVal = ((*Beg++) << Shift) | InitVal;
        Shift += 8;
      }
      Monitor::event_raw() << "Symbol with default value " << InitVal << "\n";
      GlobInit = ConstantInt::get(GlobValTy, InitVal);
    }

    return new GlobalVariable(*MR.getModule(), GlobValTy, false /* isConstant */,
                                      Linkage, GlobInit, SymName);
  } else {
    Monitor::event_raw() << "Failed to find symbol associated with dynamic relocation, reading as ROData.\n";
    // If can not find the corresponding symbol.
    const Value* ROVal = AMR.getRODataValueAt(Offset);
    if (ROVal != nullptr) {
      GlobVal = const_cast<GlobalVariable*>(dyn_cast<GlobalVariable>(ROVal));
      Monitor::event_raw() << "Found ROVal " << GlobVal->getName() << "\n";
      assert(GlobVal && "Failed to cast the value to global variable!");
      return GlobVal;
    }

    if (MCIR->getMCInstAt(Target) == MCIR->const_mcinstr_end()) {
      Monitor::event_raw() << "Failed to read data at " << Target << "\n";
      Monitor::event_raw() << "MCInstOffset: " << MCInstOffset << "\n";
      Monitor::event_raw() << "PCOffset: " << PCOffset << "\n";
      assert(false && "Target out of bounds");
    }
    std::string LocalName("ROConst");
    LocalName.append(std::to_string(Target));
    // Find if a global value associated with symbol name is already
    // created
    StringRef LocalNameRef(LocalName);
    Monitor::event_raw() << "Looking for " << LocalName << "\n";
    GlobVal = MR.getModule()->getGlobalVariable(LocalNameRef);
    if (GlobVal) {
      Monitor::event_raw() << "Found " << LocalName << "\n";
      return GlobVal;
    }

    Monitor::event_raw() << "Creating GlobalValue " << LocalName << "\n";

    if (Ty == Type::getInt32Ty(Context)) {
      uint32_t Data = MCIR->getMCInstAt(Target)->second.getData();
      return new GlobalVariable(*MR.getModule(), Ty, false /* isConstant */,
                                GlobalValue::InternalLinkage,
                                ConstantInt::get(Ty, Data), LocalName);
    }
    if (Ty == Type::getDoubleTy(Context)) {
      uint32_t firsthalf = MCIR->getMCInstAt(Target)->second.getData();
      uint32_t secondhalf = MCIR->getMCInstAt(Target + 4)->second.getData();
      uint64_t InitVal = ((uint64_t) firsthalf << 32) | secondhalf;
      // double_t InitValD = *reinterpret_cast<double_t*>(&InitVal); Somewhat defined undefined behaviour because of type pruning
      double_t InitValD; memcpy(&InitValD, &InitVal, sizeof(InitValD));
      return new GlobalVariable(*MR.getModule(), Ty, false /* isConstant */,
                                GlobalValue::InternalLinkage,
                                ConstantFP::get(Context, APFloat(InitValD)), LocalName);
    }

    MCInstOrData MD = MCIR->getMCInstAt(Target)->second;
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
          StringRef SecData =* StrOrErr;
          uint64_t DataOffset = DataAddr - SecStart;
          const unsigned char* RODataBegin =
              SecData.bytes_begin() + DataOffset;

          unsigned char c;
          uint64_t argNum = 0;
          const unsigned char* str = RODataBegin;
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
              reinterpret_cast<const char* >(RODataBegin));
          Constant* StrConstant =
              ConstantDataArray::getString(Context, ROStringRef);
          GlobalValue* GlobalStrConstVal = new GlobalVariable(
              * MR.getModule(), StrConstant->getType(), /* isConstant */ true,
              GlobalValue::PrivateLinkage, StrConstant, "RO-String");
          // Record the mapping between offset and global value
          AMR.addRODataValueAt(GlobalStrConstVal, Offset);
          return GlobalStrConstVal;
        }
      }
    }
  }

  assert(false && "Could not get or create a global value");
  return nullptr;
}

Value* ARMLinearRaiserPass::ARMCCToValue(int Cond, BasicBlock* BB) {
  // Why do ICmpInst constructors put &InsertBefore/&InsertAtEnd as the first
  // operand instead of* InsertBefore/*InsertAtEnd as the last one? Who knows.
  switch (Cond) {
    default:
      llvm_unreachable("Unknown condition code!");
    case ARMCC::EQ: { // Z = 1
      Value* Z = BBStateMap[BB]->getCPSRZFlag();
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS");
      Monitor::event_Instruction(ZS);
      return ZS;
    } break;
    case ARMCC::NE: { // Z = 0
      Value* Z = BBStateMap[BB]->getCPSRZFlag();
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC");
      Monitor::event_Instruction(ZC);
      return ZC;
    } break;
    case ARMCC::HS: { // C = 1
      Value* C = BBStateMap[BB]->getCPSRCFlag();
      Instruction* CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS");
      Monitor::event_Instruction(CS);
      return CS;
    } break;
    case ARMCC::LO: { // C = 0
      Value* C = BBStateMap[BB]->getCPSRCFlag();
      Instruction* CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC");
      Monitor::event_Instruction(CC);
      return CC;
    } break;
    case ARMCC::MI: { // N = 1
      Value* N = BBStateMap[BB]->getCPSRNFlag();
      Instruction* NS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getTrue(Context), "NS");
      Monitor::event_Instruction(NS);
      return NS;
    } break;
    case ARMCC::PL: { // N = 0
      Value* N = BBStateMap[BB]->getCPSRNFlag();
      Instruction* NC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getFalse(Context), "NC");
      Monitor::event_Instruction(NC);
      return NC;
    } break;
    case ARMCC::VS: { // V = 1
      Value* V = BBStateMap[BB]->getCPSRVFlag();
      Instruction* VS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getTrue(Context), "VS");
      Monitor::event_Instruction(VS);
      return VS;
    } break;
    case ARMCC::VC: { // V = 0
      Value* V = BBStateMap[BB]->getCPSRVFlag();
      Instruction* VC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getFalse(Context), "VC");
      Monitor::event_Instruction(VC);
      return VC;
    } break;
    case ARMCC::HI: { // C = 1 && Z = 0
      Value* C = BBStateMap[BB]->getCPSRCFlag();
      Instruction* CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS");
      Monitor::event_Instruction(CS);
      Value* Z = BBStateMap[BB]->getCPSRZFlag();
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC");
      Monitor::event_Instruction(ZC);
      Instruction* HI = BinaryOperator::Create(Instruction::Add, CS, ZC, "HI", BB);
      Monitor::event_Instruction(HI);
      return HI;
    } break;
    case ARMCC::LS: { // C = 0 || Z = 1
      Value* C = BBStateMap[BB]->getCPSRCFlag();
      Instruction* CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC");
      Monitor::event_Instruction(CC);
      Value* Z = BBStateMap[BB]->getCPSRZFlag();
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS");
      Monitor::event_Instruction(ZS);
      Instruction* LS = BinaryOperator::Create(Instruction::Or, CC, ZS, "LS", BB);
      Monitor::event_Instruction(LS);
      return LS;
    } break;
    case ARMCC::GE: { // N = V
      Value* N = BBStateMap[BB]->getCPSRNFlag();
      Value* V = BBStateMap[BB]->getCPSRVFlag();
      Instruction* GE = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "GE");
      Monitor::event_Instruction(GE);
      return GE;
    } break;
    case ARMCC::LT: { // N != V
      Value* N = BBStateMap[BB]->getCPSRNFlag();
      Value* V = BBStateMap[BB]->getCPSRVFlag();
      Instruction* LT = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "LT");
      Monitor::event_Instruction(LT);
      return LT;
    } break;
    case ARMCC::GT: { // Z = 0 && N = V
      Value* Z = BBStateMap[BB]->getCPSRZFlag();
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC");
      Monitor::event_Instruction(ZC);
      Value* N = BBStateMap[BB]->getCPSRNFlag();
      Value* V = BBStateMap[BB]->getCPSRVFlag();
      Instruction* Sign = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "S");
      Monitor::event_Instruction(Sign);
      Instruction* GT = BinaryOperator::Create(Instruction::And, ZC, Sign, "GT", BB);
      Monitor::event_Instruction(GT);
      return GT;
    } break;
    case ARMCC::LE: { // Z = 1 || N != V
      Value* Z = BBStateMap[BB]->getCPSRZFlag();
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS");
      Monitor::event_Instruction(ZS);
      Value* N = BBStateMap[BB]->getCPSRNFlag();
      Value* V = BBStateMap[BB]->getCPSRVFlag();
      Instruction* Sign = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "S");
      Monitor::event_Instruction(Sign);
      Instruction* LE = BinaryOperator::Create(Instruction::Or, ZS, Sign, "LE", BB);
      Monitor::event_Instruction(LE);
      return LE;
    } break;
    case ARMCC::AL: { // always
      return ConstantInt::getTrue(Context);
    } break;
  }
}

BasicBlock* ARMLinearRaiserPass::createBasicBlock(MachineBasicBlock* MBB) {
  BasicBlock* BB = BasicBlock::Create(Context, "bb." + Twine(MBB->getNumber()), F);
  BBStateMap[BB] = new ARMBasicBlockState(BB);
  MBBBBMap[MBB].push_back(BB);
  return BB;
}

std::vector<BasicBlock* > ARMLinearRaiserPass::getBasicBlocks(MachineBasicBlock* MBB) {
  if (MBBBBMap.find(MBB) == MBBBBMap.end())
    return {createBasicBlock(MBB)};
  return MBBBBMap[MBB];
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
    case ARM::ADCri:         raiseADCri(MI);         break; //  680 | ADC Rd Rn Op2 CC CPSR S
    case ARM::ADDri:         raiseADDri(MI);         break; //  684 | ADD Rd Rn Op2 CC CPSR S
    case ARM::ADDrr:         raiseADDrr(MI);         break; //  685 | ADD Rd Rn Rm CC CPSR S
    case ARM::ADDrsi:        raiseADDrsi(MI);        break; //  686 | ADD Rd Rn Rm Shift CC CPSR S
    case ARM::ANDri:         raiseANDri(MI);         break; //  693 | AND Rd Rn Op2 CC CPSR S
    case ARM::ANDrsi:        raiseANDrsi(MI);        break; //  695 | AND Rd Rn Rm Shift CC CPSR S
    case ARM::ANDrr:         raiseANDrr(MI);         break; //  694 | AND Rd Rn Rm CC CPSR S
    case ARM::BFC:           raiseBFC(MI);           break; //  704 | BFC Rd {Rwb} Imm CC CPSR
    case ARM::BICri:         raiseBICri(MI);         break; //  706 | BIC Rd Rn Op2 CC CPSR S
    case ARM::BL:            raiseBL(MI);            break; //  711 | BL Imm
    case ARM::BL_pred:       raiseBL_pred(MI);       break; //  715 | BL Imm CC CPSR
    case ARM::BX_RET:        raiseBX_RET(MI);        break; //  718 | BX_RET CC CPSR
    case ARM::Bcc:           raiseBcc(MI);           break; //  720 | Bcc offset CC CPSR
    case ARM::CLZ:           raiseCLZ(MI);           break; //  754 | CLZ Rd Rm CC CPSR
    case ARM::CMNri:         raiseCMNri(MI);         break; //  755 | CMN Rn Op2 CC CPSR
    case ARM::CMPri:         raiseCMPri(MI);         break; //  759 | CMP Rn Op2 CC CPSR
    case ARM::CMPrr:         raiseCMPrr(MI);         break; //  760 | CMP Rn Rm CC CPSR
    case ARM::DMB:           raiseDMB(MI);           break; //  773 | DMB Imm
    case ARM::EORrr:         raiseEORrr(MI);         break; //  776 | EOR Rd Rn Rm CC CPSR S
    case ARM::FMSTAT:        raiseFMSTAT(MI);        break; //  786 | FMRX CC CPSR
    case ARM::FCONSTD:       raiseFCONSTD(MI);       break; //  780 | VMOV.F64 Dd Imm CC CPSR
    case ARM::FCONSTS:       raiseFCONSTS(MI);       break; //  782 | VMOV.F32 Sd Imm CC CPSR
    case ARM::HINT:          raiseHINT(MI);          break; //  790 | HINT Imm CC CPSR
    case ARM::LDMIA:         raiseLDMIA(MI);         break; //  821 | LDMIA Rt CC CPSR Rn
    case ARM::LDMIA_UPD:     raiseLDMIA_UPD(MI);     break; //  822 | LDMIA Rt! {Rwb} CC CPSR Rn
    case ARM::LDRB_PRE_REG:  raiseLDRB_PRE_REG(MI);  break; //  830 | LDRB_PRE_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
    case ARM::LDRBi12:       raiseLDRBi12(MI);       break; //  831 | LDRB Rt Rn Imm12 CC CPSR
    case ARM::LDREX:         raiseLDREX(MI);         break; //  836 | LDREX Rt Rn CC CPSR
    case ARM::LDRH:          raiseLDRH(MI);          break; //  840 | LDRH Rt Rn AM3Reg AM3Imm CC CPSR
    case ARM::LDR_POST_IMM:  raiseLDR_POST_IMM(MI);  break; //  857 | LDR_POST_IMM Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
    case ARM::LDR_POST_REG:  raiseLDR_POST_REG(MI);  break; //  858 | LDR_POST_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
    case ARM::LDRi12:        raiseLDRi12(MI);        break; //  862 | LDR Rt Rn Imm12 CC CPSR
    case ARM::LDRrs:         raiseLDRrs(MI);         break; //  863 | LDR Rt Rn Rm AM2Shift CC CPSR
    case ARM::MLA:           raiseMLA(MI);           break; //  868 | MLA Rd Rn Rm Ra CC CPSR S
    case ARM::MOVTi16:       raiseMOVTi16(MI);       break; //  871 | MOV.t Rd Rd Imm16 CC CPSR
    case ARM::MOVi:          raiseMOVi(MI);          break; //  872 | MOV Rt Op2 CC CPSR S
    case ARM::MOVi16:        raiseMOVi16(MI);        break; //  873 | MOV Rd Imm16 CC CPSR
    case ARM::MOVr:          raiseMOVr(MI);          break; //  874 | MOV Rd Rn CC CPSR
    case ARM::MOVsi:         raiseMOVsi(MI);         break; //  876 | MOV Rd Rm Shift CC CPSR S
    case ARM::MUL:           raiseMUL(MI);           break; //  888 | MUL Rd Rn Rm CC CPSR S
    case ARM::MVNi:          raiseMVNi(MI);          break; // 1736 | MVN Rd Imm CC CPSR S
    case ARM::MVNr:          raiseMVNr(MI);          break; // 1737 | MVN Rd Rn CC CPSR S
    case ARM::ORRri:         raiseORRri(MI);         break; // 1748 | ORR Rd Rn Op2 CC CPSR S
    case ARM::ORRrr:         raiseORRrr(MI);         break; // 1749 | ORR Rd Rn Rm CC CPSR S
    case ARM::RSBri:         raiseRSBri(MI);         break; // 1782 | RSB Rd Rn Op2 CC CPSR S
    case ARM::SBCrsi:        raiseSBCrsi(MI);        break; // 1796 | SBC Rd Rn Rm Shift CC CPSR S
    case ARM::SMMUL:         raiseSMMUL(MI);         break; // 1843 | SMMUL Rd Rn Rm CC CPSR
    case ARM::SMULL:         raiseSMULL(MI);         break; // 1849 | SMULL RdLo RdHi Rn Rm CC CPSR S
    case ARM::STMDB_UPD:     raiseSTMDB_UPD(MI);     break; // 1895 | STMDB Rt! {Rwb} CC CPSR Rn
    case ARM::STMIA:         raiseSTMIA(MI);         break; // 1896 | STMIA Rt CC CPSR Rn
    case ARM::STMIB:         raiseSTMIB(MI);         break; // 1898 | STMIB Rt CC CPSR Rn
    case ARM::STRB_POST_IMM: raiseSTRB_POST_IMM(MI); break; // 1902 | STRB Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
    case ARM::STRBi12:       raiseSTRBi12(MI);       break; // 1906 | STRB Rt Rn Imm12 CC CPSR
    case ARM::STREX:         raiseSTREX(MI);         break; // 1911 | STREX Rd Rt Rn CC CPSR
    case ARM::STRH:          raiseSTRH(MI);          break; // 1915 | STRH Rt Rn AM3Reg AM3Imm CC CPSR
    case ARM::STRi12:        raiseSTRi12(MI);        break; // 1926 | STR Rt Rn Imm12 CC CPSR
    case ARM::STRrs:         raiseSTRrs(MI);         break; // 1927 | STR Rt Rn Rm AM2Shift CC CPSR
    case ARM::SUBri:         raiseSUBri(MI);         break; // 1928 | SUB Rd Rn Op2 CC CPSR S
    case ARM::SUBrr:         raiseSUBrr(MI);         break; // 1929 | SUB Rd Rn Rm CC CPSR S
    case ARM::SUBrsi:        raiseSUBrsi(MI);        break; // 1930 | SUB Rd Rn Rm Shift CC CPSR S
    case ARM::TEQrr:         raiseTEQrr(MI);         break; // 1942 | TEQ Rn Rm CC CPSR
    case ARM::TSTri:         raiseTSTri(MI);         break; // 1948 | TST Rn Imm CC CPSR
    case ARM::TSTrr:         raiseTSTrr(MI);         break; // 1949 | TST Rn Rm CC CPSR
    case ARM::VABSD:         raiseVABSD(MI);         break; // 2026 | VABS.F64 Dd Dm CC CPSR
    case ARM::VADDD:         raiseVADDD(MI);         break; // 2047 | VADD.F64 Dd Dn Dm CC CPSR
    case ARM::VADDS:         raiseVADDS(MI);         break; // 2058 | VADD.F32 Sd Sn Sm CC CPSR
    case ARM::VCMPD:         raiseVCMPD(MI);         break; // 2213 | VCMP.F64 Dd Dm CC CPSR
    case ARM::VCMPZD:        raiseVCMPZD(MI);        break; // 2222 | VCMP.F64 Dd [0] CC CPSR
    case ARM::VDIVD:         raiseVDIVD(MI);         break; // 2327 | VDIV.F64 Dd Dn Dm CC CPSR
    case ARM::VLDMDIA_UPD:   raiseVLDMDIA_UPD(MI);   break; // 2778 | VLDM.F64 Rt! {Rwb} CC CPSR Dn
    case ARM::VLDRD:         raiseVLDRD(MI);         break; // 2783 | VLDR.F64 Dd Rn Imm/4 CC CPSR
    case ARM::VLDRS:         raiseVLDRS(MI);         break; // 2785 | VLDR.F32 Sd Rn Imm/4 CC CPSR
    case ARM::VMLAD:         raiseVMLAD(MI);         break; // 2838 | VMLA.F64 Dd {Dwb} Dn Dm CC CPSR
    case ARM::VMOVD:         raiseVMOVD(MI);         break; // 2901 | VMOV.F64 Dd Dn CC CPSR
    case ARM::VMOVDRR:       raiseVMOVDRR(MI);       break; // 2902 | VMOV Dm Rd Rn CC CPSR
    case ARM::VMOVRRD:       raiseVMOVRRD(MI);       break; // 2915 | VMOV Rd Rn Dm CC CPSR
    case ARM::VMOVRS:        raiseVMOVRS(MI);        break; // 2917 | VMOV.F32 Rd Sn CC CPSR
    case ARM::VMOVSR:        raiseVMOVSR(MI);        break; // 2919 | VMOV.F32 St Rn CC CPSR
    case ARM::VMOVv2i32:     raiseVMOVv2i32(MI);     break; // 2924 | VMOV.I32 Dd Imm CC CPSR
    case ARM::VMULD:         raiseVMULD(MI);         break; // 2954 | VMUL.F64 Dd Dn Dm CC CPSR
    case ARM::VNEGD:         raiseVNEGD(MI);         break; // 2995 | VNEG.F64 Dd Dm CC CPSR
    case ARM::VORRd:         raiseVORRd(MI);         break; // 3019 | VORR Dd Dn Dm {CC CPSR}
    case ARM::VSITOD:        raiseVSITOD(MI);        break; // 3463 | VCVT.F64.S32 Dd Sm CC CPSR
    case ARM::VSTMDDB_UPD:   raiseVSTMDDB_UPD(MI);   break; // 3763 | VSTM.F64 Rt! {Rwb} CC CPSR Dn
    case ARM::VSTMDIA_UPD:   raiseVSTMDIA_UPD(MI);   break; // 3765 | VSTM.F64 Rt! {Rwb} CC CPSR Dn
    case ARM::VSTRD:         raiseVSTRD(MI);         break; // 3770 | VSTR.F64 Dd Rn Imm/4 CC CPSR
    case ARM::VSUBD:         raiseVSUBD(MI);         break; // 3791 | VSUB.F64 Dd Dn Dm CC CPSR
  }
  Monitor::event_end("ARMLinearRaiserPass::RaiseMachineInstr");
  return true;
}

bool ARMLinearRaiserPass::raiseADCri(MachineInstr* MI) { // 680 | ADC Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Add = BinaryOperator::Create(Instruction::Add, RnVal, ImmVal, "ADCri", BB);
  Monitor::event_Instruction(Add);

  Value* Carry = BBStateMap[BB]->getCPSRCFlag();
  Instruction* ZExt = new ZExtInst(Carry, Type::getInt32Ty(Context), "ZExt", BB);
  Monitor::event_Instruction(ZExt);
  Instruction* Result = BinaryOperator::Create(Instruction::Add, Add, ZExt, "ADCri", BB);
  Monitor::event_Instruction(Result);

  assert(!update_flags && "Unhandled instruction flags");

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseADDri(MachineInstr* MI) { // 684 | ADD Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  if (Rd == ARM::SP && Rn == ARM::SP) { // Move SP to allocate space on the stack
    assert(!conditional_execution && !update_flags && "Unhandled instruction flags");
    BBStateMap[BB]->SP_offset += Imm;
    Monitor::event_raw() << "Incrementing stack by " << Imm << "\n";
    return true;
  }

  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) { // Load StackValue
    assert(!conditional_execution && !update_flags && "Unhandled instruction flags");
    AllocaInst* alloca = getOrCreateStackAlloca(Rn, Imm, Type::getInt32Ty(Context), BB);
    Instruction* Cast = new PtrToIntInst(alloca, Type::getInt32Ty(Context), "StackAllocaDowncast", BB);
    Monitor::event_Instruction(Cast);
    setRegValue(Rd, Cast, BB);
    return true;
  }

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::Add, RnVal, ImmVal, "ADDri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ADDSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ADDSZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag
    Function* UAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, ImmVal}, "UAddInstrinsic", BB);
    Monitor::event_Instruction(CallUAdd);
    Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "ADDSCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    BBStateMap[BB]->setCPSRCFlag(C_Flag);

    // Overflow flag
    Function* SAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, ImmVal}, "SAddIntrinsic", BB);
    Monitor::event_Instruction(CallSAdd);
    Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "ADDSVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    BBStateMap[BB]->setCPSRVFlag(V_Flag);
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseADDrr(MachineInstr* MI) { // 685 | ADD Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Add, RnVal, RmVal, "ADDrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ADDSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ADDSZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag
    Function* UAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, RmVal}, "UAddInstrinsic", BB);
    Monitor::event_Instruction(CallUAdd);
    Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "ADDSCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    BBStateMap[BB]->setCPSRCFlag(C_Flag);

    // Overflow flag
    Function* SAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, RmVal}, "SAddIntrinsic", BB);
    Monitor::event_Instruction(CallSAdd);
    Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "ADDSVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    BBStateMap[BB]->setCPSRVFlag(V_Flag);
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseADDrsi(MachineInstr* MI) { // 686 | ADD Rd Rn Rm Shift CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(6).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(
    S == 0 &&
    "ADDrsi: assuming no S flag for now"
  );

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "ADDrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(Context), ShiftAmount), "ADDrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);
  Instruction* Instr = BinaryOperator::Create(Instruction::Add, RnVal, ShiftInstr, "ADDrsi", BB);
  Monitor::event_Instruction(Instr);
  setRegValue(Rd, Instr, BB);

  assert(!update_flags && "ADDrsi: assuming no S flag for now");

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseANDri(MachineInstr* MI) { // 693 | AND Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "ANDri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ANDSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ANDSZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag update cant occur for constant Op2
    // Overflow flag is not updated
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseANDrsi(MachineInstr* MI) { // 695 | AND Rd Rn Rm Shift CC CPSR S
    MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(6).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "ANDrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(Context), ShiftAmount), "ANDrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, ShiftInstr, "ANDrsi", BB);

  setRegValue(Rd, Result, BB);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "CmpNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "CMPriZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag can be set by Operand2 calculation; TODO
    // Overflow flag is not updated
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseANDrr(MachineInstr* MI) { // 694 | AND Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, RmVal, "ANDrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    assert(false && "ANDrr: update flags not implemented");
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseBFC(MachineInstr* MI) { //  704 | BFC Rd {Rwb} Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RdVal = getRegValue(Rd, Type::getInt32Ty(Context), BB);
  // Instead of the lsb and width, llvm saves the expanded form
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RdVal, ImmVal, "BFC", BB);
  Monitor::event_Instruction(Result);

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseBICri(MachineInstr* MI) { // 706 | BIC Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), -Imm);
  Instruction* Instr = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "BICri", BB);
  Monitor::event_Instruction(Instr);
  setRegValue(Rd, Instr, BB);

  assert(!update_flags && "BICri: update flags not implemented");

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseBL(MachineInstr* MI) { // 711 | BL Imm
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  int64_t Imm = MI->getOperand(0).getImm();
  int64_t offset = MCIR->getMCInstIndex(*MI);
  uint64_t target = AMR.getTextSectionAddress() + offset + Imm + 8;
  Monitor::event_raw() << "address = " << target << "\n";

  Function* CalledFunc = AMR.getRaisedFunctionAt(target);
  Monitor::event_raw() << "Direct call target: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";

  if (!CalledFunc) {
    CalledFunc = AMR.getCalledFunctionUsingTextReloc(offset, 4);
    Monitor::event_raw() << "Call target using text reloc: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  }

  if (!CalledFunc) {
    // Get CalledFunc using PLT
    const ELF32LEObjectFile* Elf32LEObjFile = dyn_cast<ELF32LEObjectFile>(AMR.getObjectFile());
    assert(Elf32LEObjFile != nullptr && "Only 32-bit ELF binaries supported at present!");
    unsigned char ExecType = Elf32LEObjFile->getELFFile().getHeader().e_type;
    assert((ExecType == ELF::ET_DYN) || (ExecType == ELF::ET_EXEC));

    for (section_iterator SecIter : Elf32LEObjFile->sections()) {
      uint64_t SecStart = SecIter->getAddress();
      uint64_t SecEnd = SecStart + SecIter->getSize();
      if (SecStart > target || SecEnd < target)
        continue;

      auto NameOrErr = SecIter->getName();
      assert(NameOrErr && "Failed to get section name");
      StringRef SecName =* NameOrErr;
      if (SecName.compare(".plt") != 0)
        assert(false && "Unexpected section name of PLT offset");

      auto StrOrErr = SecIter->getContents();
      assert(StrOrErr && "Failed to get the content of section!");
      auto SecData =* StrOrErr;
      ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t* >(SecData.data()), SecData.size());

      Monitor::event_raw() << "PLT section size: " << Bytes.size() << "\n";

      uint64_t ignored;

      MCInst Inst1;
      bool Success = AMR.getMCDisassembler()->getInstruction(Inst1, ignored, Bytes.slice(target - SecStart), target, nulls());
      assert(Success && "Failed to disassemble instruction in PLT");

      MCInst Inst2;
      Success = AMR.getMCDisassembler()->getInstruction(Inst2, ignored, Bytes.slice(target + 4 - SecStart), target + 4, nulls());
      assert(Success && "Failed to disassemble instruction in PLT");

      MCInst Inst3;
      Success = AMR.getMCDisassembler()->getInstruction(Inst3, ignored, Bytes.slice(target + 8 - SecStart), target + 8, nulls());
      assert(Success && "Failed to disassemble instruction in PLT");

      uint64_t GotPltRelocOffset = target;

      if (
        Inst1.getOpcode() == ARM::ADDri &&
        Inst2.getOpcode() == ARM::ADDri &&
        Inst3.getOpcode() == ARM::LDR_PRE_IMM
      ) {
        unsigned Bits1 = Inst1.getOperand(2).getImm() & 0xFF;
        unsigned Rot1 = (Inst1.getOperand(2).getImm() & 0xF00) >> 7;
        unsigned Bits2 = Inst2.getOperand(2).getImm() & 0xFF;
        unsigned Rot2 = (Inst2.getOperand(2).getImm() & 0xF00) >> 7;

        GotPltRelocOffset +=
          static_cast<int64_t>(ARM_AM::rotr32(Bits1, Rot1)) +
          static_cast<int64_t>(ARM_AM::rotr32(Bits2, Rot2)) +
          Inst3.getOperand(3).getImm() +
          8;
      } else {
        assert(false && "Unexpected instruction sequence in PLT");
      }

      const RelocationRef* GotPltReloc =
          AMR.getDynRelocAtOffset(GotPltRelocOffset);
      assert(GotPltReloc != nullptr && "Failed to get dynamic relocation for jmp target of PLT entry");

      assert(GotPltReloc->getType() == ELF::R_ARM_JUMP_SLOT && "Unexpected relocation type for PLT jmp instruction");
      symbol_iterator CalledFuncSym = GotPltReloc->getSymbol();
      assert(CalledFuncSym != Elf32LEObjFile->symbol_end() && "Failed to find relocation symbol for PLT entry");
      Expected<StringRef> CalledFuncSymName = CalledFuncSym->getName();
      assert(CalledFuncSymName && "Failed to find symbol associated with dynamic relocation of PLT jmp target.");

      Monitor::event_raw() << "Found called function " << CalledFuncSymName.get() << "\n";

      CalledFunc = ExternalFunctions::Create(*CalledFuncSymName, AMR);
      assert(CalledFunc && "Failed to create external function");

      AMR.setSyscallMapping(target, CalledFunc);
      Monitor::event_raw() << "Set Syscall " << CalledFunc->getName() << " for " << target << "\n";

      break;
    }
    Monitor::event_raw() << "Call target using PLT: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  }

  assert(CalledFunc && "No function found for call");
  unsigned ArgCount = AMR.getFunctionArgNum(target);
  Monitor::event_raw() << "ArgCount = " << ArgCount << ", CalledFunc->arg_size() = " << CalledFunc->arg_size() << "\n";
  if (ArgCount < CalledFunc->arg_size()) ArgCount = CalledFunc->arg_size();
  {auto &OS = Monitor::event_raw();
    OS << "Call Function: "; CalledFunc->getReturnType()->print(OS); OS << " " << CalledFunc->getName() << "/" << ArgCount << "(";
    for (unsigned i = 0; i < ArgCount; i++) {
      Value* Arg = CalledFunc->arg_begin() + i;
      if (i > 0) OS << ", ";
      OS << Arg->getName() << " ";
      if (i < CalledFunc->arg_size())
        Arg->getType()->print(OS);
      else
        OS << "...";
    }
    OS << ")\n";
  }

  std::vector<Value* > ArgVals;
  for (unsigned i = 0; i < ArgCount; ++i) {
    Value* ArgVal = nullptr;
    Type* Ty = CalledFunc->getFunctionType()->getParamType(i);
    if (i < 4) ArgVal = getRegValue(ARM::R0 + i, Type::getInt32Ty(Context), BB);
    else {
      auto I = stack_map.find((i - 4)*  4);
      assert(I != stack_map.end() && "Failed to find stack value");
      ArgVal = I->second;
    };

    if (ArgVal->getType() != Ty && i < CalledFunc->arg_size()) { // Skip variadic args
      // Handle special string value to pointer case, should obviously be moved
      Instruction* Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB);
      Monitor::event_Instruction(Cast);
      ArgVal = Cast;
    }
    ArgVals.push_back(ArgVal);
  }

  Instruction* Call = CallInst::Create(CalledFunc, ArgVals, "", BB);
  Monitor::event_Instruction(Call);

  if (CalledFunc->getReturnType()->isVoidTy()) {

  } else {
    if (CalledFunc->getReturnType()->isPointerTy()) {
      Instruction* Cast = new PtrToIntInst(Call, Type::getInt32Ty(Context), "", BB);
      Monitor::event_Instruction(Cast);
      setRegValue(ARM::R0, Cast, BB);
    } else
      setRegValue(ARM::R0, Call, BB);
  }

  if (CalledFunc->getName() == "exit" || CalledFunc->getName() == "__assert_fail") {
    Instruction* Unreachable = new UnreachableInst(BB->getContext(), BB);
    Monitor::event_Instruction(Unreachable);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseBL_pred(MachineInstr* MI) { // 715 | BL Imm CC CPSR
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  int64_t Imm = MI->getOperand(0).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t offset = MCIR->getMCInstIndex(*MI);
  uint64_t target = AMR.getTextSectionAddress() + offset + Imm + 8;
  Monitor::event_raw() << "address = " << target << "\n";

  Function* CalledFunc = AMR.getRaisedFunctionAt(target);
  Monitor::event_raw() << "Direct call target: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";

  if (!CalledFunc) {
    CalledFunc = AMR.getCalledFunctionUsingTextReloc(offset, 4);
    Monitor::event_raw() << "Call target using text reloc: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  }

  if (!CalledFunc) {
    // Get CalledFunc using PLT
    const ELF32LEObjectFile* Elf32LEObjFile = dyn_cast<ELF32LEObjectFile>(AMR.getObjectFile());
    assert(Elf32LEObjFile != nullptr && "Only 32-bit ELF binaries supported at present!");
    unsigned char ExecType = Elf32LEObjFile->getELFFile().getHeader().e_type;
    assert((ExecType == ELF::ET_DYN) || (ExecType == ELF::ET_EXEC));

    for (section_iterator SecIter : Elf32LEObjFile->sections()) {
      uint64_t SecStart = SecIter->getAddress();
      uint64_t SecEnd = SecStart + SecIter->getSize();
      if (SecStart > target || SecEnd < target)
        continue;

      auto NameOrErr = SecIter->getName();
      assert(NameOrErr && "Failed to get section name");
      StringRef SecName =* NameOrErr;
      if (SecName.compare(".plt") != 0)
        assert(false && "Unexpected section name of PLT offset");

      auto StrOrErr = SecIter->getContents();
      assert(StrOrErr && "Failed to get the content of section!");
      auto SecData =* StrOrErr;
      ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t* >(SecData.data()), SecData.size());

      Monitor::event_raw() << "PLT section size: " << Bytes.size() << "\n";

      uint64_t ignored;

      MCInst Inst1;
      bool Success = AMR.getMCDisassembler()->getInstruction(Inst1, ignored, Bytes.slice(target - SecStart), target, nulls());
      assert(Success && "Failed to disassemble instruction in PLT");

      MCInst Inst2;
      Success = AMR.getMCDisassembler()->getInstruction(Inst2, ignored, Bytes.slice(target + 4 - SecStart), target + 4, nulls());
      assert(Success && "Failed to disassemble instruction in PLT");

      MCInst Inst3;
      Success = AMR.getMCDisassembler()->getInstruction(Inst3, ignored, Bytes.slice(target + 8 - SecStart), target + 8, nulls());
      assert(Success && "Failed to disassemble instruction in PLT");

      uint64_t GotPltRelocOffset = target;

      if (
        Inst1.getOpcode() == ARM::ADDri &&
        Inst2.getOpcode() == ARM::ADDri &&
        Inst3.getOpcode() == ARM::LDR_PRE_IMM
      ) {
        unsigned Bits1 = Inst1.getOperand(2).getImm() & 0xFF;
        unsigned Rot1 = (Inst1.getOperand(2).getImm() & 0xF00) >> 7;
        unsigned Bits2 = Inst2.getOperand(2).getImm() & 0xFF;
        unsigned Rot2 = (Inst2.getOperand(2).getImm() & 0xF00) >> 7;

        GotPltRelocOffset +=
          static_cast<int64_t>(ARM_AM::rotr32(Bits1, Rot1)) +
          static_cast<int64_t>(ARM_AM::rotr32(Bits2, Rot2)) +
          Inst3.getOperand(3).getImm() +
          8;
      } else {
        assert(false && "Unexpected instruction sequence in PLT");
      }

      const RelocationRef* GotPltReloc =
          AMR.getDynRelocAtOffset(GotPltRelocOffset);
      assert(GotPltReloc != nullptr && "Failed to get dynamic relocation for jmp target of PLT entry");

      assert(GotPltReloc->getType() == ELF::R_ARM_JUMP_SLOT && "Unexpected relocation type for PLT jmp instruction");
      symbol_iterator CalledFuncSym = GotPltReloc->getSymbol();
      assert(CalledFuncSym != Elf32LEObjFile->symbol_end() && "Failed to find relocation symbol for PLT entry");
      Expected<StringRef> CalledFuncSymName = CalledFuncSym->getName();
      assert(CalledFuncSymName && "Failed to find symbol associated with dynamic relocation of PLT jmp target.");

      Monitor::event_raw() << "Found called function " << CalledFuncSymName.get() << "\n";

      CalledFunc = ExternalFunctions::Create(*CalledFuncSymName, AMR);
      assert(CalledFunc && "Failed to create external function");

      AMR.setSyscallMapping(target, CalledFunc);
      Monitor::event_raw() << "Set Syscall " << CalledFunc->getName() << " for " << target << "\n";

      break;
    }
    Monitor::event_raw() << "Call target using PLT: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
  }

  assert(CalledFunc && "No function found for call");
  unsigned ArgCount = AMR.getFunctionArgNum(target);
  Monitor::event_raw() << "ArgCount = " << ArgCount << ", CalledFunc->arg_size() = " << CalledFunc->arg_size() << "\n";
  if (ArgCount < CalledFunc->arg_size()) ArgCount = CalledFunc->arg_size();
  {auto &OS = Monitor::event_raw();
    OS << "Call Function: "; CalledFunc->getReturnType()->print(OS); OS << " " << CalledFunc->getName() << "/" << ArgCount << "(";
    for (unsigned i = 0; i < ArgCount; i++) {
      Value* Arg = CalledFunc->arg_begin() + i;
      if (i > 0) OS << ", ";
      OS << Arg->getName() << " ";
      if (i < CalledFunc->arg_size())
        Arg->getType()->print(OS);
      else
        OS << "...";
    }
    OS << ")\n";
  }

  std::vector<Value* > ArgVals;
  for (unsigned i = 0; i < ArgCount; ++i) {
    Value* ArgVal = nullptr;
    Type* Ty = CalledFunc->getFunctionType()->getParamType(i);
    if (i < 4) ArgVal = getRegValue(ARM::R0 + i, Type::getInt32Ty(Context), BB);
    else {
      auto I = stack_map.find((i - 4)*  4);
      assert(I != stack_map.end() && "Failed to find stack value");
      ArgVal = I->second;
    };

    if (ArgVal->getType() != Ty && i < CalledFunc->arg_size()) { // Skip variadic args
      // Handle special string value to pointer case, should obviously be moved
      Instruction* Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB);
      Monitor::event_Instruction(Cast);
      ArgVal = Cast;
    }
    ArgVals.push_back(ArgVal);
  }

  Instruction* Call = CallInst::Create(CalledFunc, ArgVals, "", BB);
  Monitor::event_Instruction(Call);

  if (CalledFunc->getReturnType()->isVoidTy()) {

  } else {
    if (CalledFunc->getReturnType()->isPointerTy()) {
      Instruction* Cast = new PtrToIntInst(Call, Type::getInt32Ty(Context), "", BB);
      Monitor::event_Instruction(Cast);
      setRegValue(ARM::R0, Cast, BB);
    } else
      setRegValue(ARM::R0, Call, BB);
  }

  if (CalledFunc->getName() == "exit" || CalledFunc->getName() == "__assert_fail") {
    Instruction* Unreachable = new UnreachableInst(BB->getContext(), BB);
    Monitor::event_Instruction(Unreachable);
  }

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseBX_RET(MachineInstr* MI) { // 718 | BX_RET CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(0).getImm();
  Register CPSR = MI->getOperand(1).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Type* RetTy = F->getReturnType();

  if (RetTy->isVoidTy()) {
    Instruction* Instr = ReturnInst::Create(Context, BB);
    Monitor::event_Instruction(Instr);
  } else {
    Instruction* Instr = ReturnInst::Create(Context, getRegValue(ARM::R0, Type::getInt32Ty(Context), BB), BB);
    Monitor::event_Instruction(Instr);
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseBcc(MachineInstr* MI) { // 720 | Bcc offset CC CPSR
  // int64_t offset = MI->getOperand(0).getImm();
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();

  Monitor::event_raw() << "Succ_size: " << MBB->succ_size() << "\n";
  if (MBB->succ_size() == 0) {
    // Folded call and return;
    ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);

    int64_t Imm = MI->getOperand(0).getImm();
    int64_t offset = MCIR->getMCInstIndex(*MI);
    uint64_t target = AMR.getTextSectionAddress() + offset + Imm + 8;
    Monitor::event_raw() << "address = " << target << "\n";

    Function* CalledFunc = AMR.getRaisedFunctionAt(target);
    Monitor::event_raw() << "Direct call target: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";

    if (!CalledFunc) {
      CalledFunc = AMR.getCalledFunctionUsingTextReloc(offset, 4);
      Monitor::event_raw() << "Call target using text reloc: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
    }

    if (!CalledFunc) {
      // Get CalledFunc using PLT
      const ELF32LEObjectFile* Elf32LEObjFile = dyn_cast<ELF32LEObjectFile>(AMR.getObjectFile());
      assert(Elf32LEObjFile != nullptr && "Only 32-bit ELF binaries supported at present!");
      unsigned char ExecType = Elf32LEObjFile->getELFFile().getHeader().e_type;
      assert((ExecType == ELF::ET_DYN) || (ExecType == ELF::ET_EXEC));

      for (section_iterator SecIter : Elf32LEObjFile->sections()) {
        uint64_t SecStart = SecIter->getAddress();
        uint64_t SecEnd = SecStart + SecIter->getSize();
        if (SecStart > target || SecEnd < target)
          continue;

        auto NameOrErr = SecIter->getName();
        assert(NameOrErr && "Failed to get section name");
        StringRef SecName =* NameOrErr;
        Monitor::event_raw() << "Section name: " << SecName << "\n";
        if (SecName.compare(".plt") != 0)
          assert(false && "Unexpected section name of PLT offset");

        auto StrOrErr = SecIter->getContents();
        assert(StrOrErr && "Failed to get the content of section!");
        auto SecData =* StrOrErr;
        ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t* >(SecData.data()), SecData.size());

        Monitor::event_raw() << "PLT section size: " << Bytes.size() << "\n";

        uint64_t ignored;

        MCInst Inst1;
        bool Success = AMR.getMCDisassembler()->getInstruction(Inst1, ignored, Bytes.slice(target - SecStart), target, nulls());
        assert(Success && "Failed to disassemble instruction in PLT");

        MCInst Inst2;
        Success = AMR.getMCDisassembler()->getInstruction(Inst2, ignored, Bytes.slice(target + 4 - SecStart), target + 4, nulls());
        assert(Success && "Failed to disassemble instruction in PLT");

        MCInst Inst3;
        Success = AMR.getMCDisassembler()->getInstruction(Inst3, ignored, Bytes.slice(target + 8 - SecStart), target + 8, nulls());
        assert(Success && "Failed to disassemble instruction in PLT");

        uint64_t GotPltRelocOffset = target;

        if (
          Inst1.getOpcode() == ARM::ADDri &&
          Inst2.getOpcode() == ARM::ADDri &&
          Inst3.getOpcode() == ARM::LDR_PRE_IMM
        ) {
          unsigned Bits1 = Inst1.getOperand(2).getImm() & 0xFF;
          unsigned Rot1 = (Inst1.getOperand(2).getImm() & 0xF00) >> 7;
          unsigned Bits2 = Inst2.getOperand(2).getImm() & 0xFF;
          unsigned Rot2 = (Inst2.getOperand(2).getImm() & 0xF00) >> 7;

          GotPltRelocOffset +=
            static_cast<int64_t>(ARM_AM::rotr32(Bits1, Rot1)) +
            static_cast<int64_t>(ARM_AM::rotr32(Bits2, Rot2)) +
            Inst3.getOperand(3).getImm() +
            8;
        } else {
          assert(false && "Unexpected instruction sequence in PLT");
        }

        const RelocationRef* GotPltReloc =
            AMR.getDynRelocAtOffset(GotPltRelocOffset);
        assert(GotPltReloc != nullptr && "Failed to get dynamic relocation for jmp target of PLT entry");

        assert(GotPltReloc->getType() == ELF::R_ARM_JUMP_SLOT && "Unexpected relocation type for PLT jmp instruction");
        symbol_iterator CalledFuncSym = GotPltReloc->getSymbol();
        assert(CalledFuncSym != Elf32LEObjFile->symbol_end() && "Failed to find relocation symbol for PLT entry");
        Expected<StringRef> CalledFuncSymName = CalledFuncSym->getName();
        assert(CalledFuncSymName && "Failed to find symbol associated with dynamic relocation of PLT jmp target.");

        Monitor::event_raw() << "Found called function " << CalledFuncSymName.get() << "\n";

        CalledFunc = ExternalFunctions::Create(*CalledFuncSymName, AMR);
        assert(CalledFunc && "Failed to create external function");

        AMR.setSyscallMapping(target, CalledFunc);
        Monitor::event_raw() << "Set Syscall " << CalledFunc->getName() << " for " << target << "\n";

        break;
      }
      Monitor::event_raw() << "Call target using PLT: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
    }

    assert(CalledFunc && "No function found for call");
    unsigned ArgCount = AMR.getFunctionArgNum(target);
    if (ArgCount < CalledFunc->arg_size()) ArgCount = CalledFunc->arg_size();
    {auto &OS = Monitor::event_raw();
      OS << "Call Function: " << CalledFunc->getName() << "/" << ArgCount << "(";
      for (unsigned i = 0; i < ArgCount; i++) {
        Value* Arg = CalledFunc->arg_begin() + i;
        if (i > 0) OS << ", ";
        OS << Arg->getName() << " ";
        if (i < CalledFunc->arg_size())
          Arg->getType()->print(OS);
        else
          OS << "...";
      }
      OS << ")\n";
    }

    std::vector<Value* > ArgVals;
    for (unsigned i = 0; i < ArgCount; ++i) {
      Value* ArgVal = nullptr;
      Type* Ty = CalledFunc->getFunctionType()->getParamType(i);
      if (i < 4) ArgVal = getRegValue(ARM::R0 + i, Type::getInt32Ty(Context), BB);
      else {
        auto I = stack_map.find((i - 4)*  4);
        assert(I != stack_map.end() && "Failed to find stack value");
        ArgVal = I->second;
      };

      if (ArgVal->getType() != Ty && i < CalledFunc->arg_size()) { // Skip variadic args
        // Handle special string value to pointer case, should obviously be moved
        Instruction* Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB);
        Monitor::event_Instruction(Cast);
        ArgVal = Cast;
      }
      ArgVals.push_back(ArgVal);
    }

    Instruction* Instr = CallInst::Create(CalledFunc, ArgVals, "", BB);
    Monitor::event_Instruction(Instr);

    if (CalledFunc->getReturnType()->isVoidTy()) {

    } else {
      setRegValue(ARM::R0, Instr, BB);
    }

    if (F->getReturnType()->isVoidTy()) {
      Instruction* Instr = ReturnInst::Create(Context, BB);
      Monitor::event_Instruction(Instr);
    } else {
      Instruction* Instr = ReturnInst::Create(Context, getRegValue(ARM::R0, Type::getInt32Ty(Context), BB), BB);
      Monitor::event_Instruction(Instr);
    }
    return true;
  }

  if (CC == ARMCC::AL && CPSR == 0) {
    assert(
      MBB->succ_size() == 1 &&
      "Bcc: assuming normal unconditional branch for now"
    );
    auto succ_itt = MBB->succ_begin();
    BasicBlock* BranchBB = getBasicBlocks(*succ_itt).front();
    Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
    Instruction* Instr = BranchInst::Create(BranchBB, BB);
    Monitor::event_Instruction(Instr);
  } else {
    assert(
      MBB->succ_size() == 2 &&
      "Bcc: assuming normal conditional branch for now"
    );

    Value* Cond = ARMCCToValue(CC, BB);

    auto succ_itt = MBB->succ_begin();
    BasicBlock* BranchBB = getBasicBlocks(*succ_itt).front();
    Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
    BasicBlock* NextBB = getBasicBlocks(*++succ_itt).front();
    Monitor::event_raw() << "Next BB: " << NextBB->getName() << "\n";
    Instruction* Branch = BranchInst::Create(BranchBB, NextBB, Cond, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseCLZ(MachineInstr* MI) { //  754 | CLZ Rd Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).front();

  Register Rd = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Function* Ctlz = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ctlz, Type::getInt32Ty(Context));
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);
  Instruction* Call = CallInst::Create(Ctlz, {RmVal, ConstantInt::get(Type::getInt1Ty(Context), 1)}, "CLZ", BB);
  Monitor::event_Instruction(Call);

  setRegValue(Rd, Call, BB);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseCMNri(MachineInstr* MI) { // 755 | CMN Rn Op2 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  int64_t Op2 = MI->getOperand(1).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);
  Value* NImmVal = ConstantInt::get(Type::getInt32Ty(Context), -Imm);
  // Not entirely correct, but negative zero compares shouldnt be emitted by compilers anyways

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, NImmVal, "CMNriNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  BBStateMap[BB]->setCPSRNFlag(CmpNeg);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, NImmVal, "CMNriZero", BB);
  Monitor::event_Instruction(CmpZero);
  BBStateMap[BB]->setCPSRZFlag(CmpZero);

  // Carry flag
  Function* UAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(Context));
  Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, ImmVal}, "UAddInstrinsic", BB);
  Monitor::event_Instruction(CallUAdd);
  Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "CMNCFlag", BB);
  Monitor::event_Instruction(C_Flag);
  BBStateMap[BB]->setCPSRCFlag(C_Flag);

  // Overflow flag
  Function* SAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(Context));
  Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, ImmVal}, "SAddIntrinsic", BB);
  Monitor::event_Instruction(CallSAdd);
  Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "CMNVFlag", BB);
  Monitor::event_Instruction(V_Flag);
  BBStateMap[BB]->setCPSRVFlag(V_Flag);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseCMPri(MachineInstr* MI) { // 759 | CMP Rn Op2 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  int64_t Op2 = MI->getOperand(1).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();

  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "CMPriNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  BBStateMap[BB]->setCPSRNFlag(CmpNeg);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "CMPriZero", BB);
  Monitor::event_Instruction(CmpZero);
  BBStateMap[BB]->setCPSRZFlag(CmpZero);

  // Carry flag
  //Instruction* CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, imm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
  Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
  Instruction* CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
  Monitor::event_Instruction(CallUSub);
  Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPriCFlag", BB);
  Monitor::event_Instruction(C_Flag);
  BBStateMap[BB]->setCPSRCFlag(C_Flag);

  // Overflow flag
  //Instruction* CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, imm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
  Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
  Instruction* CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
  Monitor::event_Instruction(CallSSub);
  Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPriVFlag", BB);
  Monitor::event_Instruction(V_Flag);
  BBStateMap[BB]->setCPSRVFlag(V_Flag);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseCMPrr(MachineInstr* MI) { // 760 | CMP Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, RmVal, "CMPrrNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  BBStateMap[BB]->setCPSRNFlag(CmpNeg);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, RmVal, "CMPrrZero", BB);
  Monitor::event_Instruction(CmpZero);
  BBStateMap[BB]->setCPSRZFlag(CmpZero);

  // Carry flag
  Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
  Instruction* CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
  Monitor::event_Instruction(CallUSub);
  Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPrrCFlag", BB);
  Monitor::event_Instruction(C_Flag);
  BBStateMap[BB]->setCPSRCFlag(C_Flag);

  // Overflow flag
  Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
  Instruction* CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
  Monitor::event_Instruction(CallSSub);
  Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPrrVFlag", BB);
  Monitor::event_Instruction(V_Flag);
  BBStateMap[BB]->setCPSRVFlag(V_Flag);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseDMB(MachineInstr* MI) { // 773 | DMB Imm
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  // Call ARM DMB intrinsic

  int64_t Imm = MI->getOperand(0).getImm();

  Function* DMB = Intrinsic::getDeclaration(MR.getModule(),Intrinsic::arm_dmb);
  Instruction* Instr = CallInst::Create(DMB, { ConstantInt::get(Type::getInt32Ty(Context), Imm) }, "", BB);
  Monitor::event_Instruction(Instr);

  return true;
}
bool ARMLinearRaiserPass::raiseEORrr(MachineInstr* MI) { // 776 | EOR Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Xor, RnVal, RmVal, "EORrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    assert(false && "EORrr update flags not yet implemented");
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseFMSTAT(MachineInstr* MI) { //  786 | FMRX CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  // FMRX r15, FPSCR Alias; which doesnt move to PC, but instead targets CPSR
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(0).getImm();
  Register CPSR = MI->getOperand(1).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Monitor::event_raw() << "Moving FPSCR flags to CPSR\n";
  BBStateMap[BB]->setCPSRNFlag(BBStateMap[BB]->getFPSCRNFlag());
  BBStateMap[BB]->setCPSRZFlag(BBStateMap[BB]->getFPSCRZFlag());
  BBStateMap[BB]->setCPSRCFlag(BBStateMap[BB]->getFPSCRCFlag());
  BBStateMap[BB]->setCPSRVFlag(BBStateMap[BB]->getFPSCRVFlag());

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseFCONSTD(MachineInstr* MI) { // 780 | VMOV.F64 Dd Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  uint8_t Sign = (Imm >> 7) & 0x1;
  uint8_t Exp = (Imm >> 4) & 0x7;
  uint8_t Mantissa = Imm & 0xf;
  float FImm = bit_cast<float>(
      (Sign << 31)
    | (((Exp & 0x4) != 0 ? 0 : 1) << 30)
    | (((Exp & 0x4) != 0 ? 0x1f : 0) << 25)
    | ((Exp & 0x3) << 23)
    | (Mantissa << 19)
  );
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* ImmVal = ConstantFP::get(Type::getDoubleTy(Context), FImm);
  setDRegValueF64(Dd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseFCONSTS(MachineInstr* MI) { // 782 | VMOV.F32 Sd Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Sd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  uint8_t Sign = (Imm >> 7) & 0x1;
  uint8_t Exp = (Imm >> 4) & 0x7;
  uint8_t Mantissa = Imm & 0xf;
  float F = bit_cast<float>(
      (Sign << 31)
    | (((Exp & 0x4) != 0 ? 0 : 1) << 30)
    | (((Exp & 0x4) != 0 ? 0x1f : 0) << 25)
    | ((Exp & 0x3) << 23)
    | (Mantissa << 19)
  );
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* ImmVal = ConstantFP::get(Type::getFloatTy(Context), F);
  Monitor::event_raw() << "Reg: " << Sd << " <= " << F << "\n";
  setRegValue(Sd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseHINT(MachineInstr* MI) { //  790 | HINT Imm CC CPSR
  // MachineBasicBlock* MBB = MI->getParent();
  // BasicBlock* BB = getBasicBlocks(MBB).back();

  int64_t Imm = MI->getOperand(0).getImm();
  // ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  // Register CPSR = MI->getOperand(2).getReg();
  // bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  // BasicBlock* MergeBB;
  // if (conditional_execution) {
  //   Value* Cond = ARMCCToValue(CC, BB);
  //   BasicBlock* CondExecBB = createBasicBlock(MBB);
  //   MergeBB = createBasicBlock(MBB);
  //   Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
  //   Monitor::event_Instruction(CondBranch);
  //   BB = CondExecBB;
  // }

  Monitor::event_raw() << "HINT" << Imm << "\n";

  // if (conditional_execution) {
  //   Instruction* Branch = BranchInst::Create(MergeBB, BB);
  //   Monitor::event_Instruction(Branch);
  // }

  return true;
}
bool ARMLinearRaiserPass::raiseLDMIA(MachineInstr* MI) { //  821 | LDMIA Rt CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(3).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(Rt, 0, Type::getInt32Ty(Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Ptr, "LDMIA", BB);
    Monitor::event_Instruction(Load);

    setRegValue(Rn, Load, BB);

    return true;
  }

  assert(Rt != ARM::PC && "LDMIA: Rt == PC");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(Context), "LDMIA", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Ptr, "LDMIA", BB);
  Monitor::event_Instruction(Load);

  setRegValue(Rn, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDMIA_UPD(MachineInstr* MI) { // 822 | LDMIA Rt! {Rwb} CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(Rt, 0, Type::getInt32Ty(Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Ptr, "LDMIA_UPD", BB);
    Monitor::event_Instruction(Load);

    setRegValue(Rn, Load, BB);

    Monitor::event_raw() << "incrementing SP by 4\n";
    BBStateMap[BB]->SP_offset += 4;
    return true;
  }

  assert(Rt != ARM::PC && "LDMIA_UPD: Rt == PC");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(Context), "LDMIA_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Ptr, "LDMIA_UPD", BB);
  Monitor::event_Instruction(Load);

  setRegValue(Rn, Load, BB);

  Monitor::event_raw() << "LDMIA_UPD: incrementing Rt by 4\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Add, RtVal, ConstantInt::get(Type::getInt32Ty(Context), 4), "LDMIA_UPD", BB);
  Monitor::event_Instruction(Add);

  setRegValue(Rt, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDRB_PRE_REG(MachineInstr* MI) { // 830 | LDRB_PRE_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(3).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(Rn, Rn, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt8PtrTy(Context), "LDRB_PRE_REGCast", BB);
  Monitor::event_Instruction(Cast);

  Instruction* Load = new LoadInst(Type::getInt8Ty(Context), Cast, "LDRB_PRE_REGLoad", BB);
  Monitor::event_Instruction(Load);

  Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(Context), "LDRB_PRE_REGZExt", BB);
  Monitor::event_Instruction(ZExt);

  setRegValue(Rt, ZExt, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDRBi12(MachineInstr* MI) { // 831 | LDRB Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
    Addr = getOrCreateStackAlloca(Rn, Imm12, Type::getInt8PtrTy(Context), BB);
  } else {
    Addr = getRegValue(Rn, Type::getInt8PtrTy(Context), BB);
    Value* Offset = ConstantInt::get(Type::getInt32Ty(Context), Imm12);
    Addr = getRegValue(Rn, Type::getInt8PtrTy(Context), BB);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt8Ty(Context), Addr, Offset, "LDRBi12GEP", BB);
    Monitor::event_Instruction(GEP);
    Addr = GEP;
  }
  // Load
  Instruction* Load = new LoadInst(Type::getInt8Ty(Context), Addr, "LDRBi12", BB);
  Monitor::event_Instruction(Load);
  // Zero extend
  Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(Context), "LDRBi12ZExt", BB);
  Monitor::event_Instruction(ZExt);

  setRegValue(Rt, ZExt, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDREX(MachineInstr* MI) { // 836 | LDREX Rt Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Ptr = getRegValue(Rn, Type::getInt32PtrTy(Context), BB);

  Function* LDREX = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_ldrex, {Type::getInt32PtrTy(Context)});
  Instruction* Instr = CallInst::Create(LDREX, { Ptr }, "LDREX", BB);
  Monitor::event_Instruction(Instr);
  setRegValue(Rt, Instr, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDRH(MachineInstr* MI) { // 840 | LDRH Rt Rn AM3Reg AM3Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register AM3Reg = MI->getOperand(2).getReg();
  int64_t AM3Imm = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
    assert(AM3Reg == 0 && "ARM::STRH: accessing stack with reg offset not supported yet");
    Addr = getOrCreateStackAlloca(Rn, AM3Imm, Type::getInt16PtrTy(Context), BB);
  } else {
    Addr = getRegValue(Rn, Type::getInt16PtrTy(Context), BB);
    Value* Offset;
    if (AM3Reg == 0)
      Offset = ConstantInt::get(Type::getInt32Ty(Context), AM3Imm);
    else
      Offset = getRegValue(AM3Reg, Type::getInt32Ty(Context), BB);
    Addr = getRegValue(Rn, Type::getInt16PtrTy(Context), BB);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(Context), Addr, Offset, "LDRH_GEP", BB);
    Monitor::event_Instruction(GEP);
    Addr = GEP;
  }
  // Load
  Instruction* Load = new LoadInst(Type::getInt16Ty(Context), Addr, "LDRH", BB);
  Monitor::event_Instruction(Load);
  // Zero extend
  Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(Context), "LDRHZExt", BB);
  Monitor::event_Instruction(ZExt);

  setRegValue(Rt, ZExt, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseLDR_POST_IMM(MachineInstr* MI) { // 857 | LDR_POST_IMM Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(Rn, Rn, 0, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "LDR_POST_IMMCast", BB);
  Monitor::event_Instruction(Cast);
  Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDR_POST_IMM", BB);
  Monitor::event_Instruction(Load);
  setRegValue(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDR_POST_REG(MachineInstr* MI) { //  858 | LDR_POST_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(3).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(Rn, Rn, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "LDR_POST_REGCast", BB);
  Monitor::event_Instruction(Cast);
  Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDR_POST_REG", BB);
  Monitor::event_Instruction(Load);
  setRegValue(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDRi12(MachineInstr* MI) { // 862 | LDR Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rn == ARM::PC) { // Load PC-relative GlobalValue
    Value* GV = getGlobalValueByOffset(MCIR->getMCInstIndex(*MI), Imm12 + 8, Type::getInt32Ty(Context));
    Monitor::event_raw() << "Global Value: " << GV->getName() << "\n";
    Instruction* Cast = new PtrToIntInst(GV, Type::getInt32Ty(Context), "LDRi12CastDown", BB);
    Monitor::event_Instruction(Cast);
    setRegValue(Rt, Cast, BB);
    return true;
  }

  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) { // Load StackValue
    AllocaInst* StackValue = getOrCreateStackAlloca(Rn, Imm12, Type::getInt32Ty(Context), BB);
    Instruction* Load = new LoadInst(Type::getInt32Ty(Context), StackValue, "LDRi12Load", BB);
    Monitor::event_Instruction(Load);
    setRegValue(Rt, Load, BB);
    return true;
  }

  Value* Ptr = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* Offset = ConstantInt::get(Type::getInt32Ty(Context), Imm12);

  Instruction* Add = BinaryOperator::Create(Instruction::Add, Ptr, Offset, "LDRi12Add", BB);
  Monitor::event_Instruction(Add);
  Instruction* Cast = new IntToPtrInst(Add, Type::getInt32PtrTy(Context), "LDRi12Cast", BB);
  Monitor::event_Instruction(Cast);

  Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDRi12Load", BB);
  Monitor::event_Instruction(Load);
  setRegValue(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseLDRrs(MachineInstr* MI) { // 863 | LDR Rt Rn Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t AM2Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(Rn, 0, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "LDRrsCast", BB);
  Monitor::event_Instruction(Cast);
  Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDRrsLoad", BB);
  Monitor::event_Instruction(Load);

  setRegValue(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMLA(MachineInstr* MI) { // 868 | MLA Rd Rn Rm Ra CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  Register Ra = MI->getOperand(3).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(6).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(
    S == 0 &&
    "ARM::MLA: assuming no S flag for now"
  );

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);
  Value* RaVal = getRegValue(Ra, Type::getInt32Ty(Context), BB);

  Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MLA", BB);
  Monitor::event_Instruction(Mul);
  Instruction* Add = BinaryOperator::Create(Instruction::Add, Mul, RaVal, "MLA", BB);
  Monitor::event_Instruction(Add);
  setRegValue(Rd, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMOVTi16(MachineInstr* MI) { // 871 |  Rd Rd Imm16 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  assert(MI->getOperand(1).getReg() == Rd && "ARM::MOVTi16: expecting doubled Rd entry");
  int64_t Imm16 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Val = getRegValue(Rd, Type::getInt32Ty(Context), BB);

  Instruction* Trunc = new TruncInst(Val, Type::getInt16Ty(Context), "MOVTi16Trunc", BB);
  Monitor::event_Instruction(Trunc);
  Val = Trunc;

  Instruction* ZExt = new ZExtInst(Val, Type::getInt32Ty(Context), "MOVTi16ZExt", BB);
  Monitor::event_Instruction(ZExt);
  Val = ZExt;

  Instruction* Add = BinaryOperator::Create(Instruction::Add, Val, ConstantInt::get(Type::getInt32Ty(Context), Imm16 << 16), "MOVTi16Add", BB);
  Monitor::event_Instruction(Add);
  Val = Add;

  assert(Val->getType() == Type::getInt32Ty(Context) && "ARM::MOVTi16: expecting output type to be Ty32");
  setRegValue(Rd, Val, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMOVi(MachineInstr* MI) { // 872 | MOV Rt Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  int64_t Op2 = MI->getOperand(1).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(4).getReg();

  assert(
    S == 0 &&
    "ARM::MOVi: assuming no S flag for now"
  );

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  setRegValue(Rt, ConstantInt::get(Type::getInt32Ty(Context), Imm), BB);
  Monitor::event_raw() << "Reg " << Rt << " <= " << Imm << "\n";

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMOVi16(MachineInstr* MI) { // 873 | MOV Rd Imm16 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  int64_t Imm16 = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  setRegValue(Rd, ConstantInt::get(Type::getInt32Ty(Context), Imm16), BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMOVr(MachineInstr* MI) { // 874 | MOV Rd Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rd == ARM::R11 && Rn == ARM::SP) {
    BBStateMap[BB]->FP_offset = BBStateMap[BB]->SP_offset;
    BBStateMap[BB]->R11_is_FP = true;
    Monitor::event_raw() << "ARM::MOVr: Updating FP_offset\n";
    return true;
  }
  if (Rd == ARM::SP && Rn == ARM::R11) {
    BBStateMap[BB]->SP_offset = BBStateMap[BB]->FP_offset;
    Monitor::event_raw() << "ARM::MOVr: Updating SP_offset\n";
    return true;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Monitor::event_raw() << "Reg " << Rd << " <= " << Rn << "\n";
  setRegValue(Rd, RnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMOVsi(MachineInstr* MI) { // 876 | MOV Rd Rm Shift CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  int64_t Shift = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(
    S == 0 &&
    "ARM::MOVsi: assuming no S flag for now"
  );

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "MOVsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(Context), ShiftAmount), "MOVsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);
  setRegValue(Rd, ShiftInstr, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMUL(MachineInstr* MI) { // 888 | MUL Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MUL", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "CmpNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "CMPriZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag and Overflow flag are corrupted in ARMv4, unaffected in ARMv5T and above
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
bool ARMLinearRaiserPass::raiseMVNi(MachineInstr* MI) { // 1736 | Rd Imm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(4).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(!update_flags && "MVNi: assuming no flags for now");

  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), ~Imm);
  Monitor::event_raw() << "Reg: " << Rd << " <= " << ~Imm << "\n";
  setRegValue(Rd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseMVNr(MachineInstr* MI) { // 1737 | MVN Rd Rn CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(4).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(!update_flags && "MVNi: assuming no flags for now");

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  Instruction* Xor = BinaryOperator::Create(Instruction::Xor, RnVal, ConstantInt::get(Type::getInt32Ty(Context), -1), "MVN", BB);
  Monitor::event_Instruction(Xor);
  Instruction* Add = BinaryOperator::Create(Instruction::Add, Xor, ConstantInt::get(Type::getInt32Ty(Context), 1), "MVN", BB);
  Monitor::event_Instruction(Add);

  setRegValue(Rd, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseORRri(MachineInstr* MI) { // 1748 | ORR Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::Or, RnVal, ImmVal, "ORRri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag update cant occur for constant Op2
    // Overflow flag is not updated
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseORRrr(MachineInstr* MI) { // 1749 | ORR Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Or, RnVal, RmVal, "ORRrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag update cant occur for direct register Op2
    // Overflow flag is not updated
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseRSBri(MachineInstr* MI) { // 1782 | RSB Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::Sub, ImmVal, RnVal, "RSBri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, ImmVal, RnVal, "RSBSriNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, ImmVal, RnVal, "RSBSriZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallUSub = CallInst::Create(USub, {ImmVal, RnVal}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "RSBSriCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    BBStateMap[BB]->setCPSRCFlag(C_Flag);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallSSub = CallInst::Create(SSub, {ImmVal, RnVal}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "RSBSriVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    BBStateMap[BB]->setCPSRVFlag(V_Flag);
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSBCrsi(MachineInstr* MI) { // 1796 | SBC Rd Rn Rm Shift CC CPSR S
    MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(6).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "SBCrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(Context), ShiftAmount), "SBCrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);

  Value* Carry = BBStateMap[BB]->getCPSRCFlag();
  Instruction* ZExt = new ZExtInst(Carry, Type::getInt32Ty(Context), "SBCrsiCarry", BB);
  Monitor::event_Instruction(ZExt);
  Instruction* SubCarry = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Type::getInt32Ty(Context), 1), ZExt, "SBCrsiSubCarry", BB);
  Monitor::event_Instruction(SubCarry);
  Instruction* CarryAdd = BinaryOperator::Create(Instruction::Add, ShiftInstr, SubCarry, "SBCrsiCarryAdd", BB);
  Monitor::event_Instruction(CarryAdd);

  Instruction* Result = BinaryOperator::Create(Instruction::Sub, RnVal, CarryAdd, "SBCrsiResult", BB);
  Monitor::event_Instruction(Result);

  setRegValue(Rd, Result, BB);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, CarryAdd, "SBCrsiNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, CarryAdd, "SUBSriZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallUSub = CallInst::Create(USub, {RnVal, CarryAdd}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSriCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    BBStateMap[BB]->setCPSRCFlag(C_Flag);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallSSub = CallInst::Create(SSub, {RnVal, CarryAdd}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSriVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    BBStateMap[BB]->setCPSRVFlag(V_Flag);
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSMMUL(MachineInstr* MI) { // 1843 | SMMUL Rd Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* RnExt = new SExtInst(RnVal, Type::getInt64Ty(Context), "SMMULRnExt", BB);
  Monitor::event_Instruction(RnExt);
  Instruction* RmExt = new SExtInst(RmVal, Type::getInt64Ty(Context), "SMMULRmExt", BB);
  Monitor::event_Instruction(RmExt);

  Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnExt, RmExt, "SMMUL", BB);
  Monitor::event_Instruction(Mul);

  Instruction* Shr = BinaryOperator::Create(Instruction::LShr, Mul, ConstantInt::get(Type::getInt64Ty(Context), 32), "SMMULShr", BB);
  Monitor::event_Instruction(Shr);
  Instruction* Trunc = new TruncInst(Shr, Type::getInt32Ty(Context), "SMMULTrunc", BB);
  Monitor::event_Instruction(Trunc);

  setRegValue(Rd, Trunc, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSMULL(MachineInstr* MI) { // 1849 | SMULL RdLo RdHi Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register RdLo = MI->getOperand(0).getReg();
  Register RdHi = MI->getOperand(1).getReg();
  Register Rn = MI->getOperand(2).getReg();
  Register Rm = MI->getOperand(3).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(6).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(
    S == 0 &&
    "ARM::SMULL: assuming no S flag for now"
  );

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* RnExt = new SExtInst(RnVal, Type::getInt64Ty(Context), "SMULLRnExt", BB);
  Monitor::event_Instruction(RnExt);
  Instruction* RmExt = new SExtInst(RmVal, Type::getInt64Ty(Context), "SMULLRmExt", BB);
  Monitor::event_Instruction(RmExt);

  Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnExt, RmExt, "SMULL", BB);
  Monitor::event_Instruction(Mul);

  Instruction* Lo = new TruncInst(Mul, Type::getInt32Ty(Context), "SMULLLo", BB);
  Monitor::event_Instruction(Lo);
  Instruction* Hi = BinaryOperator::Create(Instruction::LShr, Mul, ConstantInt::get(Type::getInt64Ty(Context), 32), "SMULLHi", BB);
  Monitor::event_Instruction(Hi);
  Instruction* CastHi = new TruncInst(Hi, Type::getInt32Ty(Context), "SMULLHiCast", BB);
  Monitor::event_Instruction(CastHi);

  setRegValue(RdLo, Lo, BB);
  setRegValue(RdHi, CastHi, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTMDB_UPD(MachineInstr* MI) { // 1895 | STMDB Rt! {Rwb} CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Monitor::event_raw() << "STMDB_UPD: decrementing SP by 4\n";
    BBStateMap[BB]->SP_offset -= 4;

    Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
    Value* Ptr = getOrCreateStackAlloca(Rt, 0, Type::getInt32Ty(Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
    Monitor::event_Instruction(Store);

    return true;
  }

  assert(Rt != ARM::PC && "STMDB_UPD: Rt == PC");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  Monitor::event_raw() << "STMDB_UPD: decrementing Rt by 4\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Sub, RtVal, ConstantInt::get(Type::getInt32Ty(Context), 4), "STMDB_UPD", BB);
  Monitor::event_Instruction(Add);

  setRegValue(Rt, Add, BB);

  Instruction* Ptr = new IntToPtrInst(Add, Type::getInt32PtrTy(Context), "STMDB_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTMIA(MachineInstr* MI) { // 1896 | STMIA Rt CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(3).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
    Value* Ptr = getOrCreateStackAlloca(Rt, 0, Type::getInt32Ty(Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
    Monitor::event_Instruction(Store);

    return true;
  }

  assert(Rt != ARM::PC && "STMIA: Rt == PC");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(Context), "STMIA", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTMIB(MachineInstr* MI) { // 1898 | STMIB Rt CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(3).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
    Value* Ptr = getOrCreateStackAlloca(Rt, 4, Type::getInt32Ty(Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << (BBStateMap[BB]->SP_offset+4) << "] = " << Ptr << "\n";

    Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
    Monitor::event_Instruction(Store);

    return true;
  }

  assert(Rt != ARM::PC && "STMIB: Rt == PC");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(Context), "STMIB", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTRB_POST_IMM(MachineInstr* MI) { // 1902 | STRB Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(Rn, Rn, 0, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt8PtrTy(Context), "STRB_POST_IMMCast", BB);
  Monitor::event_Instruction(Cast);

  Value* Val = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Instruction* Trunc = new TruncInst(Val, Type::getInt8Ty(Context), "STRB_POST_IMMValTrunc", BB);
  Monitor::event_Instruction(Trunc);

  Instruction* Store = new StoreInst(Trunc, Cast, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTRBi12(MachineInstr* MI) { // 1906 | STRB Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Instruction* Trunc = new TruncInst(RtVal, Type::getInt8Ty(Context), "STRBi12_ValTrunc", BB);
  Monitor::event_Instruction(Trunc);
  RtVal = Trunc;

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
    Addr = getOrCreateStackAlloca(Rn, Imm12, Type::getInt8PtrTy(Context), BB);
  } else {
    Addr = getRegValue(Rn, Type::getInt8PtrTy(Context), BB);
    Value* Offset = ConstantInt::get(Type::getInt32Ty(Context), Imm12);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(Context), Addr, Offset, "STRBi12_GEP", BB);
    Monitor::event_Instruction(GEP);
    Addr = GEP;
  }

  Instruction* Store = new StoreInst(RtVal, Addr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTREX(MachineInstr* MI) { // 1911 | STREX Rd Rt Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rt = MI->getOperand(1).getReg();
  Register Rn = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Ptr = getRegValue(Rt, Type::getInt32PtrTy(Context), BB);
  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  Function* STREX = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_strex, {Type::getInt32PtrTy(Context)});
  Instruction* Call = CallInst::Create(STREX, { RnVal, Ptr }, "STREX", BB);
  Monitor::event_Instruction(Call);
  setRegValue(Rd, Call, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTRH(MachineInstr* MI) { // 1915 | STRH Rt Rn AM3Reg AM3Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register AM3Reg = MI->getOperand(2).getReg();
  int64_t AM3Imm = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Instruction* Trunc = new TruncInst(RtVal, Type::getInt16Ty(Context), "STRH_ValTrunc", BB);
  Monitor::event_Instruction(Trunc);
  RtVal = Trunc;

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
    assert(AM3Reg == 0 && "ARM::STRH: accessing stack with reg offet not supported yet");
    Addr = getOrCreateStackAlloca(Rn, AM3Imm, Type::getInt16PtrTy(Context), BB);
  } else {
    Addr = getRegValue(Rn, Type::getInt16PtrTy(Context), BB);
    Value* Offset;
    if (AM3Reg == 0)
      Offset = ConstantInt::get(Type::getInt32Ty(Context), AM3Imm);
    else
      Offset = getRegValue(AM3Reg, Type::getInt32Ty(Context), BB);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(Context), Addr, Offset, "STRH_GEP", BB);
    Monitor::event_Instruction(GEP);
    Addr = GEP;
  }

  Instruction* Store = new StoreInst(RtVal, Addr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTRi12(MachineInstr* MI) { // 1926 | STR Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Val = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Value* Ptr;

  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
    Ptr = getOrCreateStackAlloca(Rn, Imm12, Type::getInt32Ty(Context), BB);
  } else {
    Ptr = getRegValue(Rn, Type::getInt32PtrTy(Context), BB);
    if (Imm12 != 0) {
      Instruction* GEP = GetElementPtrInst::Create(Type::getInt32Ty(Context), Ptr, ConstantInt::get(Type::getInt32Ty(Context), Imm12, false), "STRi12GEP", BB);
      Monitor::event_Instruction(GEP);
      Ptr = GEP;
    }
  }

  Instruction* Store = new StoreInst(Val, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSTRrs(MachineInstr* MI) { // 1927 | STR Rt Rn Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t AM2Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(Rn, 0, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "STRrsPtrCast", BB);
  Monitor::event_Instruction(Cast);
  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Instruction* Store = new StoreInst(RtVal, Cast, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSUBri(MachineInstr* MI) { // 1928 | SUB Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Op2 = MI->getOperand(2).getImm();
  unsigned Bits = Op2 & 0xFF;
  unsigned Rot = (Op2 & 0xF00) >> 7;
  int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rd == ARM::SP && Rn == ARM::SP) {
    Monitor::event_raw() << "decrementing SP by " << Imm << "\n";
    BBStateMap[BB]->SP_offset -= Imm;
    return true;
  }

  if (Rd == ARM::SP && Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP) {
    Monitor::event_raw() << "decrementing SP based on FP by " << Imm << "\n";
    BBStateMap[BB]->SP_offset = BBStateMap[BB]->FP_offset - Imm;
    return true;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Instr = BinaryOperator::CreateSub(RnVal, ImmVal, "SUBri", BB);
  Monitor::event_Instruction(Instr);
  setRegValue(Rd, Instr, BB);

  if (S == ARM::CPSR) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "SUBSriNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "SUBSriZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSriCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    BBStateMap[BB]->setCPSRCFlag(C_Flag);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSriVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    BBStateMap[BB]->setCPSRVFlag(V_Flag);
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSUBrr(MachineInstr* MI) { // 1929 | SUB Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Sub, RnVal, RmVal, "SUBrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "SUBSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    BBStateMap[BB]->setCPSRNFlag(CmpNeg);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "SUBSZero", BB);
    Monitor::event_Instruction(CmpZero);
    BBStateMap[BB]->setCPSRZFlag(CmpZero);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    BBStateMap[BB]->setCPSRCFlag(C_Flag);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
    Instruction* CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    BBStateMap[BB]->setCPSRVFlag(V_Flag);
  }

  setRegValue(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseSUBrsi(MachineInstr* MI) { // 1930 | SUB Rd Rn Rm Shift CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(6).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "SUBrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(Context), ShiftAmount), "SUBrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);
  Instruction* Instr = BinaryOperator::Create(Instruction::Sub, RnVal, ShiftInstr, "SUBrsi", BB);
  Monitor::event_Instruction(Instr);
  setRegValue(Rd, Instr, BB);

  assert(!update_flags);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseTEQrr(MachineInstr* MI) { // 1942 | TEQ Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Xor, RnVal, RmVal, "TEQrr", BB);
  Monitor::event_Instruction(Result);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "TEQrrNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  BBStateMap[BB]->setCPSRNFlag(CmpNeg);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "TEQrrZero", BB);
  Monitor::event_Instruction(CmpZero);
  BBStateMap[BB]->setCPSRZFlag(CmpZero);

  // Carry flag update cant occur for constant Op2
  // Overflow flag is not updated

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseTSTri(MachineInstr* MI) { // 1948 | TST Rn Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "TSTri", BB);
  Monitor::event_Instruction(Result);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "TSTriNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  BBStateMap[BB]->setCPSRNFlag(CmpNeg);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "TSTriZero", BB);
  Monitor::event_Instruction(CmpZero);
  BBStateMap[BB]->setCPSRZFlag(CmpZero);

  // Carry flag update cant occur for constant Op2
  // Overflow flag is not updated

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseTSTrr(MachineInstr* MI) { // 1949 | TST Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
  Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, RmVal, "TSTrr", BB);
  Monitor::event_Instruction(Result);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "TSTrrNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  BBStateMap[BB]->setCPSRNFlag(CmpNeg);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "TSTrrZero", BB);
  Monitor::event_Instruction(CmpZero);
  BBStateMap[BB]->setCPSRZFlag(CmpZero);

  // Carry flag update cant occur for constant Op2
  // Overflow flag is not updated

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVABSD(MachineInstr* MI) { // 2026 | VABS.F64 Dd Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DmVal = getDRegValueF64(Dm, BB);

  Instruction* Cmp = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OLT, DmVal, ConstantFP::get(Context, APFloat(0.0)), "VABSDCmp", BB);
  Monitor::event_Instruction(Cmp);
  Instruction* Neg = BinaryOperator::Create(Instruction::FSub, ConstantFP::get(Context, APFloat(0.0)), DmVal, "VABSDNeg", BB);
  Monitor::event_Instruction(Neg);

  Instruction* Sel = SelectInst::Create(Cmp, Neg, DmVal, "VABSDSelect", BB);
  Monitor::event_Instruction(Sel);

  setDRegValueF64(Dd, Sel, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVADDD(MachineInstr* MI) { // 2047 | VADD.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = getDRegValueF64(Dn, BB);
  Value* DmVal = getDRegValueF64(Dm, BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FAdd, DnVal, DmVal, "VADDD", BB);
  Monitor::event_Instruction(Result);

  setDRegValueF64(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVADDS(MachineInstr* MI) { // 2058 | VADD.F32 Sd Sn Sm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Sd = MI->getOperand(0).getReg();
  Register Sn = MI->getOperand(1).getReg();
  Register Sm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* SnVal = getRegValue(Sn, Type::getFloatTy(Context), BB);
  Value* SmVal = getRegValue(Sm, Type::getFloatTy(Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FAdd, SnVal, SmVal, "VADDS", BB);
  Monitor::event_Instruction(Result);

  setRegValue(Sd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVCMPD(MachineInstr* MI) { // 2213 | VCMP.F64 Dd Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = getDRegValueF64(Dd, BB);
  Value* DmVal = getDRegValueF64(Dm, BB);

  // TODO: Handle NaN

  Instruction* CmpLT = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OLT, DdVal, DmVal, "VCMPDCmp", BB);
  Monitor::event_Instruction(CmpLT);
  BBStateMap[BB]->setFPSCRNFlag(CmpLT);

  Instruction* CmpEQ = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OEQ, DdVal, DmVal, "VCMPDCmp", BB);
  Monitor::event_Instruction(CmpEQ);
  BBStateMap[BB]->setFPSCRZFlag(CmpEQ);

  Instruction* CmpGE = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OGE, DdVal, DmVal, "VCMPDCmp", BB);
  Monitor::event_Instruction(CmpGE);
  BBStateMap[BB]->setFPSCRCFlag(CmpGE);

  BBStateMap[BB]->setFPSCRVFlag(ConstantInt::get(Type::getInt1Ty(Context), 0));

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVCMPZD(MachineInstr* MI) { // 2222 | VCMP.F64 Dd [0] CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = getDRegValueF64(Dd, BB);

  // TODO: Handle NaN

  Instruction* CmpLT = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OLT, DdVal, ConstantFP::get(Context, APFloat(0.0)), "VCMPZDCmp", BB);
  Monitor::event_Instruction(CmpLT);
  BBStateMap[BB]->setFPSCRNFlag(CmpLT);

  Instruction* CmpEQ = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OEQ, DdVal, ConstantFP::get(Context, APFloat(0.0)), "VCMPZDCmp", BB);
  Monitor::event_Instruction(CmpEQ);
  BBStateMap[BB]->setFPSCRZFlag(CmpEQ);

  Instruction* CmpGE = CmpInst::Create(Instruction::FCmp, CmpInst::FCMP_OGE, DdVal, ConstantFP::get(Context, APFloat(0.0)), "VCMPZDCmp", BB);
  Monitor::event_Instruction(CmpGE);
  BBStateMap[BB]->setFPSCRCFlag(CmpGE);

  BBStateMap[BB]->setFPSCRVFlag(ConstantInt::get(Type::getInt1Ty(Context), 0));

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVDIVD(MachineInstr* MI) { // 2327 | VDIV.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = getDRegValueF64(Dn, BB);
  Value* DmVal = getDRegValueF64(Dm, BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FDiv, DnVal, DmVal, "VDIVD", BB);
  Monitor::event_Instruction(Result);

  setDRegValueF64(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVLDMDIA_UPD(MachineInstr* MI) { // 2778 | VLDM.F64 Rt! {Rwb} CC CPSR Dn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Dn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(Rt, 0, Type::getDoubleTy(Context), BB);
    Monitor::event_raw() << "Reg " << Dn << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getDoubleTy(Context), Ptr, "VLDMDIA_UPD", BB);
    Monitor::event_Instruction(Load);

    setRegValue(Dn, Load, BB);

    Monitor::event_raw() << "incrementing SP by 8\n";
    BBStateMap[BB]->SP_offset += 8;
    return true;
  }

  assert(Rt != ARM::PC && "VLDMDIA_UPD: Rt == PC");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getDoublePtrTy(Context), "VLDMDIA_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getDoubleTy(Context), Ptr, "VLDMDIA_UPD", BB);
  Monitor::event_Instruction(Load);

  setRegValue(Dn, Load, BB);

  Monitor::event_raw() << "VLDMDIA_UPD: incrementing Rt by 8\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Add, RtVal, ConstantInt::get(Type::getInt32Ty(Context), 8), "VLDMDIA_UPD", BB);
  Monitor::event_Instruction(Add);

  setRegValue(Rt, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVLDRD(MachineInstr* MI) { // 2783 | VLDR.F64 Dd Rn Imm/4 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rn == ARM::PC) { // Load PC-relative GlobalValue
    // A direct offset has to be a multiple of 4, and a PC-relative offset
    // created by an assembler based on a label or created by adding to the
    // double literal pool has to be word boundary aligned, which is also a
    // multiple of 4. Because of this, LLVM thought it was a great idea to
    // save a whopping 2 bits by using the offset/4 in the instruction.
    Value* GV = getGlobalValueByOffset(MCIR->getMCInstIndex(*MI), Imm * 4 + 8, Type::getDoubleTy(Context));
    Monitor::event_raw() << "Global Value: " << GV->getName() << "\n";
    Instruction* Load = new LoadInst(Type::getDoubleTy(Context), GV, "VLDRD", BB);
    Monitor::event_Instruction(Load);
    setDRegValueF64(Dd, Load, BB);
    return true;
  }

  if (Rn == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(Rn, Imm*4, Type::getDoubleTy(Context), BB);
    Monitor::event_raw() << "Reg " << Dd << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getDoubleTy(Context), Ptr, "VLDRS", BB);
    Monitor::event_Instruction(Load);

    setDRegValueF64(Dd, Load, BB);
    return true;
  }

  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  if (Imm != 0) {
    Instruction* Add = BinaryOperator::Create(Instruction::Add, RnVal, ConstantInt::get(Type::getInt32Ty(Context), Imm * 4), "VLDRD", BB);
    Monitor::event_Instruction(Add);
    RnVal = Add;
  }

  Instruction* Ptr = new IntToPtrInst(RnVal, Type::getDoublePtrTy(Context), "VLDRD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getDoubleTy(Context), Ptr, "VLDRD", BB);
  Monitor::event_Instruction(Load);

  setDRegValueF64(Dd, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return false;
}
bool ARMLinearRaiserPass::raiseVLDRS(MachineInstr* MI) { // 2785 | VLDR.F32 Sd Rn Imm/4 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Sd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rn == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(Rn, Imm*4, Type::getFloatTy(Context), BB);
    Monitor::event_raw() << "Reg " << Sd << " ~ stack[" << BBStateMap[BB]->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getFloatTy(Context), Ptr, "VLDRS", BB);
    Monitor::event_Instruction(Load);

    setRegValue(Sd, Load, BB);
    return true;
  }

  assert(Rn == ARM::SP && "VLDRS not yet implemented for non-SP registers");

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return false;
}
bool ARMLinearRaiserPass::raiseVMLAD(MachineInstr* MI) { // 2838 | VMLA.F64 Dd {Dwb} Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(2).getReg();
  Register Dm = MI->getOperand(3).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = getDRegValueF64(Dd, BB);
  Value* DnVal = getDRegValueF64(Dn, BB);
  Value* DmVal = getDRegValueF64(Dm, BB);

  Instruction* Mul = BinaryOperator::Create(Instruction::FMul, DnVal, DmVal, "VMLA.F64", BB);
  Monitor::event_Instruction(Mul);
  Instruction* Add = BinaryOperator::Create(Instruction::FAdd, DdVal, Mul, "VMLA.F64", BB);
  Monitor::event_Instruction(Add);
  setDRegValueF64(Dd, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMOVD(MachineInstr* MI) { // 2901 | VMOV.F64 Dd Dn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = getRegValue(Dn, Type::getDoubleTy(Context), BB);
  Monitor::event_raw() << "Reg: " << Dn << " <= " << DnVal << "\n";
  setRegValue(Dd, DnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMOVDRR(MachineInstr* MI) { // 2902 | VMOV Dm Rd Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dm = MI->getOperand(0).getReg();
  Register Rd = MI->getOperand(1).getReg();
  Register Rn = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RdVal = getRegValue(Rd, Type::getInt32Ty(Context), BB);
  Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

  setDRegValueV2I32(Dm, RdVal, RnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMOVRRD(MachineInstr* MI) { // 2915 | VMOV Rd Rn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  std::pair<Value*, Value*> DmVals = getDRegValueV2I32(Dm, BB);

  setRegValue(Rd, DmVals.first, BB);
  setRegValue(Rn, DmVals.second, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMOVRS(MachineInstr* MI) { // 2917 | VMOV.F32 Rd Sn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Sn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* SnVal = getRegValue(Sn, Type::getFloatTy(Context), BB);
  Monitor::event_raw() << "Reg: " << Rd << " <= " << SnVal << "\n";
  setRegValue(Rd, SnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMOVSR(MachineInstr* MI) { // 2919 VMOV.F32 St Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register St = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = getRegValue(Rn,  Type::getFloatTy(Context), BB);
  Monitor::event_raw() << "Reg: " << St << " <= " << RnVal << "\n";
  setRegValue(St, RnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMOVv2i32(MachineInstr* MI) { // 2924 | VMOV.I32 Dd Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  double_t DImm; memcpy(&DImm, &Imm, sizeof(DImm));
  Value* ImmVal = ConstantFP::get(Type::getDoubleTy(Context), DImm);
  Monitor::event_raw() << "Reg: " << Dd << " <= " << ImmVal << "\n";
  setRegValue(Dd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVMULD(MachineInstr* MI) { // 2954 | VMUL.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = getDRegValueF64(Dn, BB);
  Value* DmVal = getDRegValueF64(Dm, BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FMul, DnVal, DmVal, "VMULD", BB);
  Monitor::event_Instruction(Result);
  setDRegValueF64(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVNEGD(MachineInstr* MI) { // 2995 | VNEG.F64 Dd Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = getDRegValueF64(Dm, BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FSub, ConstantFP::get(Type::getDoubleTy(Context), 0), DnVal, "VNEGD", BB);
  Monitor::event_Instruction(Result);

  setDRegValueF64(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVORRd(MachineInstr* MI) { // 3019 | VORR Dd Dn Dm {CC CPSR}
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  assert(!conditional_execution && "ARM VORR must be unconditional");

  Value* DnVal = getRegValue(Dn, Type::getDoubleTy(Context), BB);
  Value* DmVal = getRegValue(Dm, Type::getDoubleTy(Context), BB);

  Instruction* DnCast = CastInst::Create(Instruction::BitCast, DnVal, Type::getInt64Ty(Context), "DnCast", BB);
  Monitor::event_Instruction(DnCast);
  Instruction* DmCast = CastInst::Create(Instruction::BitCast, DmVal, Type::getInt64Ty(Context), "DmCast", BB);
  Monitor::event_Instruction(DmCast);
  Instruction* Result = BinaryOperator::Create(Instruction::Or, DnCast, DmCast, "VORR", BB);
  Monitor::event_Instruction(Result);
  Instruction* ResultCast = CastInst::Create(Instruction::BitCast, Result, Type::getDoubleTy(Context), "ResultCast", BB);
  Monitor::event_Instruction(ResultCast);
  setRegValue(Dd, ResultCast, BB);

  return true;
}
bool ARMLinearRaiserPass::raiseVSITOD(MachineInstr* MI) { // 3463 | VCVT.F64.S32 Dd Sm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Sm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* SmVal = getRegValue(Sm, Type::getInt32Ty(Context), BB);

  Instruction* Cast = CastInst::Create(Instruction::SIToFP, SmVal, Type::getDoubleTy(Context), "VSITOD", BB);
  Monitor::event_Instruction(Cast);
  setDRegValueF64(Dd, Cast, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVSTMDDB_UPD(MachineInstr* MI) { // 3763 VSTM.F64 Rt! {Rwb} CC CPSR Dn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Dn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(Rt == ARM::SP && "VSTMDDB_UPD not yet implemented for non SP registers");

  // Value* RegVal = getRegValue(Reg, Type::getDoubleTy(Context), BB);

  Monitor::event_raw() << "VSTMDDB_UPD: decrementing SP by 8\n";
  BBStateMap[BB]->SP_offset -= 8;

  Monitor::event_raw() << "stack[" << BBStateMap[BB]->SP_offset << "] ~ Reg " << Dn << ", ignored under assumption of function prologue\n";

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVSTMDIA_UPD(MachineInstr* MI) { // 3765 | VSTM.F64 Rt! {Rwb} CC CPSR Dn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Dn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(Rt != ARM::SP && "VSTMDIA_UPD not yet implemented for SP registers");

  Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
  Value* DnVal = getRegValue(Dn, Type::getDoubleTy(Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getDoublePtrTy(Context), "VSTMDIA_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(DnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  Monitor::event_raw() << "VSTMDIA_UPD: incrementing Rt by 8\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Add, RtVal, ConstantInt::get(Type::getInt32Ty(Context), 8), "VSTMDIA_UPD", BB);
  Monitor::event_Instruction(Add);

  setRegValue(Rt, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVSTRD(MachineInstr* MI) { // 3770 | VSTR.F64 Dd Rn Imm/4 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  uint64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DdVal = getDRegValueF64(Dd, BB);
  Value* Ptr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
    Ptr = getOrCreateStackAlloca(Rn, Imm*4, Type::getDoubleTy(Context), BB);
  } else {
    Ptr = getRegValue(Rn, Type::getDoublePtrTy(Context), BB);
    if (Imm != 0) {
      Instruction* GEP = GetElementPtrInst::Create(Type::getDoubleTy(Context), Ptr, ConstantInt::get(Type::getInt32Ty(Context), Imm*4, false), "VSTRDGEP", BB);
      Monitor::event_Instruction(GEP);
      Ptr = GEP;
    }
  }

  Instruction* Store = new StoreInst(DdVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
bool ARMLinearRaiserPass::raiseVSUBD(MachineInstr* MI) { // 3791 | VSUB.F64 Dd Dn Dm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  Register Dd = MI->getOperand(0).getReg();
  Register Dn = MI->getOperand(1).getReg();
  Register Dm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(CC, BB);
    BasicBlock* CondExecBB = createBasicBlock(MBB);
    MergeBB = createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* DnVal = getDRegValueF64(Dn, BB);
  Value* DmVal = getDRegValueF64(Dm, BB);

  Instruction* Result = BinaryOperator::Create(Instruction::FSub, DnVal, DmVal, "VSUBD", BB);
  Monitor::event_Instruction(Result);

  setDRegValueF64(Dd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
#undef DEBUG_TYPE