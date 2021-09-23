
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
  BBStateMap[EBB]->SP_offset = 0;

  // Add arguments to RegValueMap
  for (unsigned i = 0; i < 4 && i < F->arg_size(); ++i) {
    Register reg = ARM::R0 + i;
    Argument* arg = F->arg_begin() + i;
    Monitor::event_raw() << "Add argument " << arg->getName() << " to RegValueMap reg " << reg << "\n";
    BBStateMap[EBB]->Q_RegValueMap[reg] = arg;
  }

  // Add excess arguments to StackArgMap
  for (unsigned i = 4; i < F->arg_size(); ++i) {
    Monitor::event_raw() << "Add argument " << (F->arg_begin() + i)->getName() << " to StackArgMap at offset " << ((i-4)*4) << "\n";
    stack_map[(i-4)*4] = F->arg_begin() + i;
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
      for (unsigned int i = 0; i < BB.getTerminator()->getNumSuccessors(); ++i) {
        BasicBlock* S = BB.getTerminator()->getSuccessor(i);
        S->removePredecessor(&BB);
      }

      for (Instruction* I : rem) {
        Monitor::event_Instruction(I);
        I->eraseFromParent();
      }
    }
  }

  // Link phi nodes
  for (std::pair<BasicBlock*, ARMBasicBlockState*> BBStatePair : BBStateMap) {
    BasicBlock* BB = BBStatePair.first;
    ARMBasicBlockState* BBState = BBStatePair.second;
    for (std::pair<Register, PHINode*> RegPHIPair : BBState->P_RegValueMap) {
      Register reg = RegPHIPair.first;
      PHINode* phi = RegPHIPair.second;
      for (BasicBlock* PBB : predecessors(BB)) {
        phi->addIncoming(BBStateMap[PBB]->Q_RegValueMap[reg], PBB);
      }
    }
  }

  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass end.\n");
  Monitor::event_end("ARMLinearRaiserPass");
  return true;
}

Value* ARMLinearRaiserPass::getRegValue(Register Reg, Type* Ty, BasicBlock* BB) {
  Value* V = BBStateMap[BB]->getRegValue(Reg);
  if (V->getType() == Ty)
    return V;
  if (V->getType() == Type::getInt32Ty(Context) && Ty->isPointerTy()) {
    Instruction* Cast = new IntToPtrInst(V, Ty, "Cast", BB);
    Monitor::event_Instruction(Cast);
    return Cast;
  }
  if (V->getType()->isPointerTy() && Ty->isIntegerTy()) {
    Instruction* Cast = new PtrToIntInst(V, Ty, "Cast", BB);
    Monitor::event_Instruction(Cast);
    return Cast;
  }

  Instruction* Cast = new BitCastInst(V, Ty, "Cast", BB);
  Monitor::event_Instruction(Cast);
  return Cast;
}
void ARMLinearRaiserPass::setRegValue(Register Reg, Value* V, BasicBlock* BB) {
  assert(Reg != ARM::SP && "getRegValue: SP is not allowed");
  assert(Reg != ARM::PC && "getRegValue: PC is not allowed");
  assert((V->getType() == Type::getInt32Ty(Context) || V->getType()->isPointerTy())
         && "Value must be of type i32 or x*");
  if (Reg == ARM::R11) BBStateMap[BB]->R11_is_FP = false;
  BBStateMap[BB]->setRegValue(Reg, V);
}

Value* ARMLinearRaiserPass::getStackValue(Register Reg, int64_t Offset, Type* Ty, BasicBlock* BB) {
  if (Reg == ARM::SP)
    return getStackValue(BBStateMap[BB]->SP_offset + Offset, Ty, BB);
  if (Reg == ARM::R11 && BBStateMap[BB]->R11_is_FP)
    return getStackValue(BBStateMap[BB]->FP_offset + Offset, Ty, BB);
  if (Reg == ARM::R11 && !BBStateMap[BB]->R11_is_FP)
    assert(false && "getStackValue: R11 was used after being invalidated");

  assert(false && "getStackValue: only stack registers are allowed");
}
Value* ARMLinearRaiserPass::getStackValue(int64_t offset, Type* Ty, BasicBlock* BB) {
  Monitor::event_raw() << "getStackValue: offset " << offset << "\n";
  auto I = stack_map.find(offset);
  if (I != stack_map.end()) {
    Value* V = I->second;
    dbgs() << "getStackValue: "; I->second->getType()->print(dbgs()); dbgs() << " at offset " << offset << "\n";
    if (V->getType() == Ty)
      return V;
    if (V->getType() == Type::getInt32Ty(Context) && Ty->isPointerTy()) {
      Instruction* Cast = new IntToPtrInst(V, Ty, "Cast", BB);
      Monitor::event_Instruction(Cast);
      return Cast;
    }
    if (V->getType()->isPointerTy() && Ty->isIntegerTy()) {
      Instruction* Cast = new PtrToIntInst(V, Ty, "Cast", BB);
      Monitor::event_Instruction(Cast);
      return Cast;
    }

    Instruction* Cast = new BitCastInst(V, Ty, "Cast", BB);
    Monitor::event_Instruction(Cast);
    return Cast;
  }

  // Match closest stack value
  I = --stack_map.lower_bound(offset);
  Monitor::event_raw() << "Can't find StackValue directly; using offset from value at " << I->first << "\n";
  dbgs() << "getStackValue: "; I->second->getType()->print(dbgs()); dbgs() << " at offset " << offset << "\n";
  int64_t diff = offset - I->first;
  Value* StackValue = I->second;
  if (!StackValue->getType()->isPointerTy()) {
    Instruction* Cast = new IntToPtrInst(StackValue, Type::getInt32PtrTy(Context), "CastUp", BB);
    Monitor::event_Instruction(Cast);
    StackValue = Cast;
  }
  Instruction* GEP = GetElementPtrInst::Create(Type::getInt32Ty(Context), StackValue, ConstantInt::get(Type::getInt32Ty(Context), diff), "GEP", BB);
  Monitor::event_Instruction(GEP);
  return GEP;
}
Value* ARMLinearRaiserPass::getOrCreateStackValue(Register Reg, int64_t Offset, Type* Ty, BasicBlock* BB) {
  if (Reg == ARM::SP)
    return getOrCreateStackValue(BBStateMap[BB]->SP_offset + Offset, Ty, BB);
  if (Reg == ARM::R11 && BBStateMap[BB]->R11_is_FP)
    return getOrCreateStackValue(BBStateMap[BB]->FP_offset + Offset, Ty, BB);
  if (Reg == ARM::R11 && !BBStateMap[BB]->R11_is_FP)
    assert(false && "getOrCreateStackValue: R11 was used after being invalidated");

  assert(false && "getOrCreateStackValue: only stack registers are allowed");
}
Value* ARMLinearRaiserPass::getOrCreateStackValue(int64_t offset, Type* Ty, BasicBlock* BB) {
  Monitor::event_raw() << "getOrCreateStackValue: offset " << offset << "\n";
  auto I = stack_map.find(offset);
  if (I != stack_map.end()) {
    Value* V = I->second;
    {auto &OS=Monitor::event_raw(); OS << "getOrCreateStackValue: "; I->second->getType()->print(OS); OS << " at offset " << offset << "\n";}
    if (V->getType() == Ty)
      return V;
    if (V->getType() == Type::getInt32Ty(Context) && Ty->isPointerTy()) {
      Instruction* Cast = new IntToPtrInst(V, Ty, "Cast", BB);
      Monitor::event_Instruction(Cast);
      return Cast;
    }
    if (V->getType()->isPointerTy() && Ty->isIntegerTy()) {
      Instruction* Cast = new PtrToIntInst(V, Ty, "Cast", BB);
      Monitor::event_Instruction(Cast);
      return Cast;
    }

    Instruction* Cast = new BitCastInst(V, Ty, "Cast", BB);
    Monitor::event_Instruction(Cast);
    return Cast;
  }
  {auto &OS=Monitor::event_raw(); OS << "getOrCreateStackValue: creating new StackValue of type "; Ty->print(OS); OS << "\n";}

  // Create new stack value
  Type* AllocTy = Ty->isPointerTy() ? Ty->getPointerElementType() : Ty;

  Instruction* Alloca = new AllocaInst(AllocTy, 0, "StackAlloca", stack_insertion_point);
  Monitor::event_Instruction(Alloca);
  stack_map[offset] = Alloca;

  if (Ty == Type::getInt32Ty(Context)) {
    Instruction* Cast = new PtrToIntInst(Alloca, Ty, "Cast", BB);
    Monitor::event_Instruction(Cast);
    return Cast;
  }

  return Alloca;
}
void ARMLinearRaiserPass::setStackValue(int64_t offset, Value* V, BasicBlock* BB) {
  assert(false && "NYI");
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
GlobalValue* ARMLinearRaiserPass::getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset) {
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
    auto Iter = MCIR->getMCInstAt(Offset - TextSecAddr);
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

  LLVMContext &LCTX = MR.getModule()->getContext();
  if (Symbol != nullptr) {
    // If the symbol is found.
    Expected<StringRef> SymNameVal = Symbol->getName();
    assert(SymNameVal &&
           "Failed to find symbol associated with dynamic relocation.");
    auto SymName = SymNameVal.get();
    Monitor::event_raw() << "Found symbol " << SymName << "\n";
    GlobVal = MR.getModule()->getGlobalVariable(SymName);
    if (GlobVal == nullptr) {
      Monitor::event_raw() << "Symbol is unregistered; creating GlobalValue\n";
      DataRefImpl SymImpl = Symbol->getRawDataRefImpl();
      auto SymbOrErr = ObjFile->getSymbol(SymImpl);
      assert(SymbOrErr && "Failed to find symbol!");
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
      Type* GlobValTy = nullptr;
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

      GlobVal = new GlobalVariable(*MR.getModule(), GlobValTy, false /* isConstant */,
                                        Linkage, GlobInit, SymName);
    }
  } else {
    Monitor::event_raw() << "Failed to find symbol associated with dynamic relocation, reading as ROData.\n";
    // If can not find the corresponding symbol.
    const Value* ROVal = AMR.getRODataValueAt(Offset);
    if (ROVal != nullptr) {
      GlobVal = const_cast<GlobalVariable*>(dyn_cast<GlobalVariable>(ROVal));
      Monitor::event_raw() << "Found ROVal " << GlobVal->getName() << "\n";
      assert(GlobVal && "Failed to cast the value to global variable!");
    } else {
      uint64_t Index = Offset - TextSecAddr;
      if (MCIR->getMCInstAt(Index) == MCIR->const_mcinstr_end()) {
        Monitor::event_raw() << "Failed to read data at " << Index << "\n";
        assert(false && "Index out of bounds");
      }
      std::string LocalName("ROConst");
      LocalName.append(std::to_string(Index));
      // Find if a global value associated with symbol name is already
      // created
      StringRef LocalNameRef(LocalName);
      Monitor::event_raw() << "Looking for " << LocalName << "\n";
      GlobVal = MR.getModule()->getGlobalVariable(LocalNameRef);
      if (GlobVal == nullptr) {
        Monitor::event_raw() << "Creating GlobalValue " << LocalName << "\n";
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
                  ConstantDataArray::getString(LCTX, ROStringRef);
              GlobalValue* GlobalStrConstVal = new GlobalVariable(
                 * MR.getModule(), StrConstant->getType(), /* isConstant */ true,
                  GlobalValue::PrivateLinkage, StrConstant, "RO-String");
              // Record the mapping between offset and global value
              AMR.addRODataValueAt(GlobalStrConstVal, Offset);
              GlobVal = GlobalStrConstVal;
              break;
            }
          }
        }

        if (GlobVal == nullptr) {
          Type* ty = Type::getInt32Ty(LCTX);
          Constant* GlobInit = ConstantInt::get(ty, Data);
          GlobVal = new GlobalVariable(*MR.getModule(), ty, /* isConstant */ true,
                                            GlobalValue::PrivateLinkage,
                                            GlobInit, LocalNameRef);
        }
      }
    }
  }

  return GlobVal;
}

Value* ARMLinearRaiserPass::ARMCCToValue(int Cond, BasicBlock* BB) {
  // Why do ICmpInst constructors put &InsertBefore/&InsertAtEnd as the first
  // operand instead of* InsertBefore/*InsertAtEnd as the last one? Who knows.
  switch (Cond) {
    default:
      llvm_unreachable("Unknown condition code!");
    case ARMCC::EQ: { // Z = 1
      Value* Z = BBStateMap[BB]->getZFlag();
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS");
      Monitor::event_Instruction(ZS);
      return ZS;
    } break;
    case ARMCC::NE: { // Z = 0
      Value* Z = BBStateMap[BB]->getZFlag();
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC");
      Monitor::event_Instruction(ZC);
      return ZC;
    } break;
    case ARMCC::HS: { // C = 1
      Value* C = BBStateMap[BB]->getCFlag();
      Instruction* CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS");
      Monitor::event_Instruction(CS);
      return CS;
    } break;
    case ARMCC::LO: { // C = 0
      Value* C = BBStateMap[BB]->getCFlag();
      Instruction* CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC");
      Monitor::event_Instruction(CC);
      return CC;
    } break;
    case ARMCC::MI: { // N = 1
      Value* N = BBStateMap[BB]->getNFlag();
      Instruction* NS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getTrue(Context), "NS");
      Monitor::event_Instruction(NS);
      return NS;
    } break;
    case ARMCC::PL: { // N = 0
      Value* N = BBStateMap[BB]->getNFlag();
      Instruction* NC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getFalse(Context), "NC");
      Monitor::event_Instruction(NC);
      return NC;
    } break;
    case ARMCC::VS: { // V = 1
      Value* V = BBStateMap[BB]->getVFlag();
      Instruction* VS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getTrue(Context), "VS");
      Monitor::event_Instruction(VS);
      return VS;
    } break;
    case ARMCC::VC: { // V = 0
      Value* V = BBStateMap[BB]->getVFlag();
      Instruction* VC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getFalse(Context), "VC");
      Monitor::event_Instruction(VC);
      return VC;
    } break;
    case ARMCC::HI: { // C = 1 && Z = 0
      Value* C = BBStateMap[BB]->getCFlag();
      Instruction* CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS");
      Monitor::event_Instruction(CS);
      Value* Z = BBStateMap[BB]->getZFlag();
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC");
      Monitor::event_Instruction(ZC);
      Instruction* HI = BinaryOperator::Create(Instruction::Add, CS, ZC, "HI", BB);
      Monitor::event_Instruction(HI);
      return HI;
    } break;
    case ARMCC::LS: { // C = 0 || Z = 1
      Value* C = BBStateMap[BB]->getCFlag();
      Instruction* CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC");
      Monitor::event_Instruction(CC);
      Value* Z = BBStateMap[BB]->getZFlag();
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS");
      Monitor::event_Instruction(ZS);
      Instruction* LS = BinaryOperator::Create(Instruction::Or, CC, ZS, "LS", BB);
      Monitor::event_Instruction(LS);
      return LS;
    } break;
    case ARMCC::GE: { // N = V
      Value* N = BBStateMap[BB]->getNFlag();
      Value* V = BBStateMap[BB]->getVFlag();
      Instruction* GE = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "GE");
      Monitor::event_Instruction(GE);
      return GE;
    } break;
    case ARMCC::LT: { // N != V
      Value* N = BBStateMap[BB]->getNFlag();
      Value* V = BBStateMap[BB]->getVFlag();
      Instruction* LT = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "LT");
      Monitor::event_Instruction(LT);
      return LT;
    } break;
    case ARMCC::GT: { // Z = 0 && N = V
      Value* Z = BBStateMap[BB]->getZFlag();
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC");
      Monitor::event_Instruction(ZC);
      Value* N = BBStateMap[BB]->getNFlag();
      Value* V = BBStateMap[BB]->getVFlag();
      Instruction* Sign = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "S");
      Monitor::event_Instruction(Sign);
      Instruction* GT = BinaryOperator::Create(Instruction::And, ZC, Sign, "GT", BB);
      Monitor::event_Instruction(GT);
      return GT;
    } break;
    case ARMCC::LE: { // Z = 1 || N != V
      Value* Z = BBStateMap[BB]->getZFlag();
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS");
      Monitor::event_Instruction(ZS);
      Value* N = BBStateMap[BB]->getNFlag();
      Value* V = BBStateMap[BB]->getVFlag();
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

  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = getBasicBlocks(MBB).back();

  switch (MI->getOpcode()) {
    default: {
      auto OS = WithColor(errs(), HighlightColor::Warning);
      OS << "ARMLinearRaiserPass encountered unhandled opcode in instruction ";
      Monitor::printMachineInstr(MI, true, OS);
      assert(false && "Unhandled opcode");
      return false;
    } break;
    case ARM::ADDri: { // 684 | ADD Rd Rn Op2 CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Op2 = MI->getOperand(2).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "you know"
      );

      if (Rd == ARM::SP && Rn == ARM::SP) {
        BBStateMap[BB]->SP_offset += Imm;
        Monitor::event_raw() << "Incrementing stack by " << Imm << "\n";
        break;
      }

      Value* Val;

      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) { // Load StackValue
        Val = getOrCreateStackValue(Rn, Imm, Type::getInt32Ty(Context), BB);
      } else {
        Val = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      }

      if (Imm != 0) {
        Instruction* Add = BinaryOperator::Create(Instruction::Add, Val, ConstantInt::get(Type::getInt32Ty(Context), Imm), "Add", BB);
        Monitor::event_Instruction(Add);
        Val = Add;
      }

      setRegValue(Rd, Val, BB);
    } break;
    case ARM::ADDrr: { // 685 | ADD Rd Rn Rm CC CPSR S
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
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ADDSZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag
        Function* UAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(Context));
        Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, RmVal}, "UAddInstrinsic", BB);
        Monitor::event_Instruction(CallUAdd);
        Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "ADDSCFlag", BB);
        Monitor::event_Instruction(C_Flag);
        BBStateMap[BB]->setCFlag(C_Flag);

        // Overflow flag
        Function* SAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(Context));
        Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, RmVal}, "SAddIntrinsic", BB);
        Monitor::event_Instruction(CallSAdd);
        Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "ADDSVFlag", BB);
        Monitor::event_Instruction(V_Flag);
        BBStateMap[BB]->setVFlag(V_Flag);
      }

      setRegValue(Rd, Result, BB);

      if (conditional_execution) {
        Instruction* Branch = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(Branch);
      }
    } break;
    case ARM::ADDrsi: { // 686 | ADD Rd Rn Rm Shift CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      int64_t Shift = MI->getOperand(3).getImm();
      int64_t CC = MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();
      Register S = MI->getOperand(6).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ADDrsi: assuming no flags for now"
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
    } break;
    case ARM::ANDri: { // 693 | AND Rd Rn Op2 CC CPSR S
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

      Value* RdVal;
      BasicBlock* BaseBB;
      BasicBlock* MergeBB;
      if (conditional_execution) {
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        RdVal = getRegValue(Rd, Type::getInt32Ty(Context), BB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

      Instruction* Instr = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "ANDri", BB);
      Monitor::event_Instruction(Instr);

      if (update_flags) {
        // Negative flag
        Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Instr, ConstantInt::get(Type::getInt32Ty(Context), 0), "ANDSNeg", BB);
        Monitor::event_Instruction(CmpNeg);
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Instr, ConstantInt::get(Type::getInt32Ty(Context), 0), "ANDSZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag update cant occur for constant Op2
        // Overflow flag is not updated
      }

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode* phi = PHINode::Create(Type::getInt32Ty(Context), 2, "Phi", MergeBB);
        phi->addIncoming(RdVal, BaseBB);
        phi->addIncoming(Instr, BB);
        setRegValue(Rd, phi, MergeBB);
      } else {
        setRegValue(Rd, Instr, BB);
      }
    } break;
    case ARM::ANDrr: { // 694 | AND Rd Rn Rm CC CPSR S
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
    } break;
    case ARM::BICri: { // 706 | BIC Rd Rn Op2 CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Op2 = MI->getOperand(2).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "you know"
      );

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), -Imm);
      Instruction* Instr = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "BICri", BB);
      Monitor::event_Instruction(Instr);
      setRegValue(Rd, Instr, BB);
    } break;
    case ARM::BL: { // 711 | BL Imm
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

      if (CalledFunc->getName() == "exit" || CalledFunc->getName() == "__assert_fail") {
        Instruction* Unreachable = new UnreachableInst(BB->getContext(), BB);
        Monitor::event_Instruction(Unreachable);
      }
    } break;
    case ARM::BX_RET: { // 718 | BX_RET CC CPSR
      int64_t CC = MI->getOperand(0).getImm();
      Register CPSR = MI->getOperand(1).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "BX_RET: assuming no flags for now"
      );

      Type* RetTy = F->getReturnType();

      if (RetTy->isVoidTy()) {
        Instruction* Instr = ReturnInst::Create(Context, BB);
        Monitor::event_Instruction(Instr);
      } else {
        Instruction* Instr = ReturnInst::Create(Context, getRegValue(ARM::R0, Type::getInt32Ty(Context), BB), BB);
        Monitor::event_Instruction(Instr);
      }
    } break;
    case ARM::Bcc: { // 720 | Bcc offset CC CPSR
      // int64_t offset = MI->getOperand(0).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
      Register CPSR = MI->getOperand(2).getReg();

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
        break;
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
    } break;
    case ARM::CMNri: { // 755 | CMN Rn Op2 CC CPSR
      Register Rn = MI->getOperand(0).getReg();
      int64_t Op2 = MI->getOperand(1).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "something something conditional execution"
      );

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);
      Value* NImmVal = ConstantInt::get(Type::getInt32Ty(Context), -Imm);
      // Not entirely correct, but negative zero compares shouldnt be emitted by compilers anyways

      // Negative flag
      Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, NImmVal, "CMNriNeg", BB);
      Monitor::event_Instruction(CmpNeg);
      BBStateMap[BB]->setNFlag(CmpNeg);

      // Zero flag
      Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, NImmVal, "CMNriZero", BB);
      Monitor::event_Instruction(CmpZero);
      BBStateMap[BB]->setZFlag(CmpZero);

      // Carry flag
      Function* UAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(Context));
      Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, ImmVal}, "UAddInstrinsic", BB);
      Monitor::event_Instruction(CallUAdd);
      Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "CMNCFlag", BB);
      Monitor::event_Instruction(C_Flag);
      BBStateMap[BB]->setCFlag(C_Flag);

      // Overflow flag
      Function* SAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(Context));
      Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, ImmVal}, "SAddIntrinsic", BB);
      Monitor::event_Instruction(CallSAdd);
      Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "CMNVFlag", BB);
      Monitor::event_Instruction(V_Flag);
      BBStateMap[BB]->setVFlag(V_Flag);
    } break;
    case ARM::CMPri: { // 759 | CMP Rn Op2 CC CPSR
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
        // Split on condition
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BB = CondExecBB;
      }

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

      // Negative flag
      Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "CMPriNeg", BB);
      Monitor::event_Instruction(CmpNeg);
      BBStateMap[BB]->setNFlag(CmpNeg);

      // Zero flag
      Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "CMPriZero", BB);
      Monitor::event_Instruction(CmpZero);
      BBStateMap[BB]->setZFlag(CmpZero);

      // Carry flag
      //Instruction* CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, imm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
      Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
      Instruction* CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
      Monitor::event_Instruction(CallUSub);
      Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPriCFlag", BB);
      Monitor::event_Instruction(C_Flag);
      BBStateMap[BB]->setCFlag(C_Flag);

      // Overflow flag
      //Instruction* CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, imm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
      Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
      Instruction* CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
      Monitor::event_Instruction(CallSSub);
      Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPriVFlag", BB);
      Monitor::event_Instruction(V_Flag);
      BBStateMap[BB]->setVFlag(V_Flag);

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
      }
    } break;
    case ARM::CMPrr: { // 760 | CMP Rn Rm CC CPSR
      Register Rn = MI->getOperand(0).getReg();
      Register Rm = MI->getOperand(1).getReg();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
      BasicBlock* MergeBB;
      if (conditional_execution) {
        // Split on condition
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BB = CondExecBB;
      }

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

      // Negative flag
      Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, RmVal, "CMPrrNeg", BB);
      Monitor::event_Instruction(CmpNeg);
      BBStateMap[BB]->setNFlag(CmpNeg);

      // Zero flag
      Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, RmVal, "CMPrrZero", BB);
      Monitor::event_Instruction(CmpZero);
      BBStateMap[BB]->setZFlag(CmpZero);

      // Carry flag
      Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
      Instruction* CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
      Monitor::event_Instruction(CallUSub);
      Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPrrCFlag", BB);
      Monitor::event_Instruction(C_Flag);
      BBStateMap[BB]->setCFlag(C_Flag);

      // Overflow flag
      Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
      Instruction* CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
      Monitor::event_Instruction(CallSSub);
      Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPrrVFlag", BB);
      Monitor::event_Instruction(V_Flag);
      BBStateMap[BB]->setVFlag(V_Flag);

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
      }
    } break;
    case ARM::DMB: { // 773 | DMB Imm
      // Call ARM DMB intrinsic

      int64_t Imm = MI->getOperand(0).getImm();

      Function* DMB = Intrinsic::getDeclaration(MR.getModule(),Intrinsic::arm_dmb);
      Instruction* Instr = CallInst::Create(DMB, { ConstantInt::get(Type::getInt32Ty(Context), Imm) }, "", BB);
      Monitor::event_Instruction(Instr);
    } break;
    /*
    case ARM::DSB: { // 774 | DSB <option> => UNDEF
      assert(false && "ARM::DSB not yet implemented; fence");
    }
    case ARM::EORri: { // 775 | EOR{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn ^ Imm
      Value* Rn = getOperandValue(MI, 1);
      Value* imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());

      Instruction* Instr = BinaryOperator::Create(Instruction::Xor, Rn, imm, "EORri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    */
    case ARM::EORrr: { // 776 | EOR{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn ^ Rm
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
    } break;
    /*
    case ARM::ISB: { // 793 | ISB <option> => UNDEF
      assert(false && "ARM::ISB not yet implemented; fence");
    } break;
    case ARM::LDMIA: { // 821 | LDMIA <Rn>{!}, <registers> => Rn = Rn + 4*  registers
      assert(false && "ARM::LDMIA not yet implemented");
    } break;
    */
    case ARM::LDMIA_UPD: { // 822 | LDMIA Rn Rn CC CPSR Reg
      Register Rn = MI->getOperand(0).getReg();
      assert(MI->getOperand(1).getReg() == Rn && "ARM::LDMIA_UPD: expecting doubled Rn");
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();
      Register Reg = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::LDMIA_UPD: assuming no flags for now"
      );

      assert(
        Rn == ARM::SP &&
        "ARM::LDMIA_UPD: assuming SP for now"
      );

      Monitor::event_raw() << "Reg " << Reg << " ~ stack[" << BBStateMap[BB]->SP_offset << "], ignoring under assumption of function epilogue\n";
      // setRegValue(Reg, stack_map[BBStateMap[BB]->SP_offset], BB);

      // Increment SP
      Monitor::event_raw() << "incrementing SP by 4\n";
      BBStateMap[BB]->SP_offset += 4;
    } break;
    case ARM::LDRB_PRE_REG: { // 830 | LDRB_PRE_REG Rt Rn Rs Rm - CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rs = MI->getOperand(2).getReg();
      assert(Rn == Rs && "ARM::LDRB_PRE_REG: expecting Rn as write-back register");
      Register Rm = MI->getOperand(3).getReg();
      int64_t AM2Shift = MI->getOperand(4).getImm();
      assert(AM2Shift == 16384 && "ARM::LDRB_PRE_REG: shift ignored");
      int64_t CC = MI->getOperand(5).getImm();
      Register CPSR = MI->getOperand(6).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::LDRB_PRE_REG: assuming no flags for now"
      );

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

      Instruction* Add = BinaryOperator::Create(Instruction::Add, RnVal, RmVal, "LDRB_PRE_REGAdd", BB);
      Monitor::event_Instruction(Add);
      setRegValue(Rs, Add, BB);

      Instruction* Cast = new IntToPtrInst(Add, Type::getInt8PtrTy(Context), "LDRB_PRE_REGCast", BB);
      Monitor::event_Instruction(Cast);

      Instruction* Load = new LoadInst(Type::getInt8Ty(Context), Cast, "LDRB_PRE_REGLoad", BB);
      Monitor::event_Instruction(Load);

      Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(Context), "LDRB_PRE_REGZExt", BB);
      Monitor::event_Instruction(ZExt);

      setRegValue(Rt, ZExt, BB);
    } break;
    case ARM::LDRBi12: { // 831 | LDRB Rt Rn Imm12 CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Imm12 = MI->getOperand(2).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

      Value* RtVal;
      BasicBlock* BaseBB;
      BasicBlock* MergeBB;
      if (conditional_execution) {
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value* Addr;
      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
        Addr = getOrCreateStackValue(Rn, Imm12, Type::getInt8PtrTy(Context), BB);
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

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode* phi = PHINode::Create(Type::getInt32Ty(Context), 2, "Phi", MergeBB);
        phi->addIncoming(RtVal, BaseBB);
        phi->addIncoming(ZExt, BB);
        setRegValue(Rt, phi, MergeBB);
      } else {
        setRegValue(Rt, ZExt, BB);
      }
    } break;
    case ARM::LDREX: { // 836 | LDREX Rt Rn CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "LDREX: assuming no flags for now"
      );

      Value* Ptr = getRegValue(Rn, Type::getInt32PtrTy(Context), BB);

      Function* LDREX = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_ldrex, {Type::getInt32PtrTy(Context)});
      Instruction* Instr = CallInst::Create(LDREX, { Ptr }, "LDREX", BB);
      Monitor::event_Instruction(Instr);

      setRegValue(Rt, Instr, BB);
    } break;
    case ARM::LDRH: { // 840 | LDRH Rt Rn AM3Reg AM3Imm CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register AM3Reg = MI->getOperand(2).getReg();
      int64_t AM3Imm = MI->getOperand(3).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();
      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

      Value* RtVal;
      BasicBlock* BaseBB;
      BasicBlock* MergeBB;
      if (conditional_execution) {
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value* Addr;
      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
        assert(AM3Reg == 0 && "ARM::STRH: accessing stack with reg offet not supported yet");
        Addr = getOrCreateStackValue(Rn, AM3Imm, Type::getInt16PtrTy(Context), BB);
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

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode* phi = PHINode::Create(Type::getInt32Ty(Context), 2, "Phi", MergeBB);
        phi->addIncoming(RtVal, BaseBB);
        phi->addIncoming(ZExt, BB);
        setRegValue(Rt, phi, MergeBB);
      } else {
        setRegValue(Rt, ZExt, BB);
      }
    } break;
    case ARM::LDR_POST_IMM: { // 857 | LDR_POST_IMM Rd Rn Rs Rm=0 AM2Shift CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rs = MI->getOperand(2).getReg();
      Register Rm = MI->getOperand(3).getReg();
      int64_t AM2Shift = MI->getOperand(4).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
      Register CPSR = MI->getOperand(6).getReg();

      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
      assert(!conditional_execution && "ARM::LDR_POST_IMM: conditional execution not supported");

      Value* Addr = resolveAM2Shift(Rn, Rs, Rm, AM2Shift, BB);
      Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "LDR_POST_IMMCast", BB);
      Monitor::event_Instruction(Cast);
      Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDR_POST_IMM", BB);
      Monitor::event_Instruction(Load);
      setRegValue(Rt, Load, BB);
    } break;
    case ARM::LDRi12: { // 862 | LDR Rt Rn Imm12 CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Imm12 = MI->getOperand(2).getImm();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::LDRi12: assuming no flags for now"
      );

      if (Rn == ARM::PC) { // Load PC-relative GlobalValue
        Value* GV = getGlobalValueByOffset(MCIR->getMCInstIndex(*MI), Imm12 + 8);
        Monitor::event_raw() << "Global Value: " << GV->getName() << "\n";
        Instruction* Cast = new PtrToIntInst(GV, Type::getInt32Ty(Context), "LDRi12CastDown", BB);
        Monitor::event_Instruction(Cast);
        setRegValue(Rt, Cast, BB);
        break;
      }

      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) { // Load StackValue
        Value* StackValue = getStackValue(Rn, Imm12, Type::getInt32PtrTy(Context), BB);
        Instruction* Load = new LoadInst(Type::getInt32Ty(Context), StackValue, "LDRi12Load", BB);
        Monitor::event_Instruction(Load);
        setRegValue(Rt, Load, BB);
        break;
      }

      Value* Ptr = getRegValue(Rn, Type::getInt32PtrTy(Context), BB);

      // GEP if there is an offset
      if (Imm12 != 0) {
        Instruction* GEP = GetElementPtrInst::Create(Type::getInt32Ty(Context), Ptr, { ConstantInt::get(Type::getInt32Ty(Context), Imm12) }, "LDRi12GEP", BB);
        Monitor::event_Instruction(GEP);
        Ptr = GEP;
      }

      Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Ptr, "LDRi12Load", BB);
      Monitor::event_Instruction(Load);

      setRegValue(Rt, Load, BB);
    } break;
    case ARM::LDRrs: { // 863 | LDR Rt Rn Rm AM2Shift CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      int64_t AM2Shift = MI->getOperand(3).getImm();
      int64_t CC = MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::LDRrs: assuming no flags for now"
      );

      Value* Addr = resolveAM2Shift(Rn, 0, Rm, AM2Shift, BB);
      Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "LDRrsCast", BB);
      Monitor::event_Instruction(Cast);
      Instruction* Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDRrsLoad", BB);
      Monitor::event_Instruction(Load);

      setRegValue(Rt, Load, BB);
    } break;
    case ARM::MLA: { // 868 | MLA Rd Rn Rm Ra CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      Register Ra = MI->getOperand(3).getReg();
      int64_t CC = MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();
      Register S = MI->getOperand(6).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::MLA: assuming no flags for now"
      );

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);
      Value* RaVal = getRegValue(Ra, Type::getInt32Ty(Context), BB);

      Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MLA", BB);
      Monitor::event_Instruction(Mul);
      Instruction* Add = BinaryOperator::Create(Instruction::Add, Mul, RaVal, "MLA", BB);
      Monitor::event_Instruction(Add);
      setRegValue(Rd, Add, BB);
    } break;
    /*
    case ARM::MOVPCLR: { // 870 | MOV<c> PC, LR => PC = LR

    } break;
    */
    case ARM::MOVTi16: { // 871 |  Rd Rd Imm16 CC CPSR
      Register Rd = MI->getOperand(0).getReg();
      assert(MI->getOperand(1).getReg() == Rd && "ARM::MOVTi16: expecting doubled Rd entry");
      int64_t Imm16 = MI->getOperand(2).getImm();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::MOVTi16: assuming no flags for now"
      );

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
    } break;
    case ARM::MOVi: { // 872 | MOV Rt Op2 CC CPSR S
      Register Rt = MI->getOperand(0).getReg();
      int64_t Op2 = MI->getOperand(1).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();
      Register S = MI->getOperand(4).getReg();

      assert(
        S == 0 &&
        "ARM::MOVi: assuming no S flag for now"
      );

      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
      if (conditional_execution) {
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        BasicBlock* MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        Instruction* MergeIf = BranchInst::Create(MergeBB, CondExecBB);
        Monitor::event_Instruction(MergeIf);

        PHINode* phi = PHINode::Create(Type::getInt32Ty(Context), 2, "Phi", MergeBB);
        phi->addIncoming(RtVal, BB);
        phi->addIncoming(ConstantInt::get(Type::getInt32Ty(Context), Imm), CondExecBB);
        setRegValue(Rt, phi, MergeBB);
        Monitor::event_raw() << "Reg " << Rt << " <= phi(" << Imm << ")\n";
      } else {
        setRegValue(Rt, ConstantInt::get(Type::getInt32Ty(Context), Imm), BB);
        Monitor::event_raw() << "Reg " << Rt << " <= " << Imm << "\n";
      }
    } break;
    case ARM::MOVi16: { // 873 | MOV Rd Imm16 CC CPSR
      Register Rd = MI->getOperand(0).getReg();
      int64_t Imm16 = MI->getOperand(1).getImm();
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::MOVi16: assuming no flags for now"
      );

      setRegValue(Rd, ConstantInt::get(Type::getInt32Ty(Context), Imm16), BB);
    } break;
    case ARM::MOVr: { // 874 | MOV Rd Rn CC CPSR
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::MOVr: assuming no flags for now"
      );

      if (Rd == ARM::R11 && Rn == ARM::SP) {
        BBStateMap[BB]->FP_offset = BBStateMap[BB]->SP_offset;
        BBStateMap[BB]->R11_is_FP = true;
        Monitor::event_raw() << "ARM::MOVr: Updating FP_offset\n";
        break;
      }
      if (Rd == ARM::SP && Rn == ARM::R11) {
        BBStateMap[BB]->SP_offset = BBStateMap[BB]->FP_offset;
        Monitor::event_raw() << "ARM::MOVr: Updating SP_offset\n";
        break;
      }

      setRegValue(Rd, getRegValue(Rn, Type::getInt32Ty(Context), BB), BB);
      Monitor::event_raw() << "Reg " << Rd << " <= " << Rn << "\n";
    } break;
    case ARM::MOVsi: { // 876 | MOV Rd Rm Shift CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rm = MI->getOperand(1).getReg();
      int64_t Shift = MI->getOperand(2).getImm();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::MOVsi: assuming no flags for now"
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
    } break;
    case ARM::MUL: { // 888 | MUL Rd Rn Rm CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
      Register S = MI->getOperand(5).getReg();
      bool update_flags = (S != 0);

      Value* RdVal;
      BasicBlock* BaseBB;
      BasicBlock* MergeBB;
      if (conditional_execution) {
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        RdVal = getRegValue(Rd, Type::getInt32Ty(Context), BB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* RmVal = getRegValue(Rm, Type::getInt32Ty(Context), BB);

      Instruction* Instr = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MUL", BB);
      Monitor::event_Instruction(Instr);

      if (update_flags) {
        // Negative flag
        Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Instr, ConstantInt::get(Type::getInt32Ty(Context), 0), "CmpNeg", BB);
        Monitor::event_Instruction(CmpNeg);
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Instr, ConstantInt::get(Type::getInt32Ty(Context), 0), "CMPriZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag and Overflow flag are corrupted in ARMv4, unaffected in ARMv5T and above
      }

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode* phi = PHINode::Create(Type::getInt32Ty(Context), 2, "Phi", MergeBB);
        phi->addIncoming(RdVal, BaseBB);
        phi->addIncoming(Instr, BB);
        setRegValue(Rd, phi, MergeBB);
      } else {
        setRegValue(Rd, Instr, BB);
      }
    } break;
    case ARM::ORRri: { // 1748 | ORR Rd Rn Op2 CC CPSR S
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

      Value* RdVal;
      BasicBlock* BaseBB;
      BasicBlock* MergeBB;
      if (conditional_execution) {
        Monitor::event_raw() << "splitting on condition\n";
        BasicBlock* CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        RdVal = getRegValue(Rd, Type::getInt32Ty(Context), BB);
        Value* Cond = ARMCCToValue(CC, BB);
        Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);
      Value* ImmVal = ConstantInt::get(Type::getInt32Ty(Context), Imm);

      Instruction* Instr = BinaryOperator::Create(Instruction::Or, RnVal, ImmVal, "ORRri", BB);
      Monitor::event_Instruction(Instr);

      if (update_flags) {
        // Negative flag
        Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Instr, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSNeg", BB);
        Monitor::event_Instruction(CmpNeg);
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Instr, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag update cant occur for constant Op2
        // Overflow flag is not updated
      }

      if (conditional_execution) {
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode* phi = PHINode::Create(Type::getInt32Ty(Context), 2, "Phi", MergeBB);
        phi->addIncoming(RdVal, BaseBB);
        phi->addIncoming(Instr, BB);
        setRegValue(Rd, phi, MergeBB);
      } else {
        setRegValue(Rd, Instr, BB);
      }
    } break;
    case ARM::ORRrr: { // 1749 | ORR Rd Rn Rm CC CPSR S
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
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "ORRSZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag update cant occur for direct register Op2
        // Overflow flag is not updated
      }

      setRegValue(Rd, Result, BB);

      if (conditional_execution) {
        Instruction* Branch = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(Branch);
      }
    } break;
    case ARM::SMULL: { // 1849 | SMULL RdLo RdHi Rn Rm CC CPSR S
      Register RdLo = MI->getOperand(0).getReg();
      Register RdHi = MI->getOperand(1).getReg();
      Register Rn = MI->getOperand(2).getReg();
      Register Rm = MI->getOperand(3).getReg();
      int64_t CC = MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();
      Register S = MI->getOperand(6).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::SMULL: assuming no flags for now"
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
    } break;
    case ARM::STMDB_UPD: { // 1895 | STMDB Rn Rn CC CPSR Reg
      Register Rn = MI->getOperand(0).getReg();
      assert(MI->getOperand(1).getReg() == Rn && "ARM::STMDB_UPD: expecting doubled Rn");
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();
      Register Reg = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::STMDB_UPD: assuming no flags for now"
      );

      assert(
        Rn == ARM::SP &&
        "ARM::STMDB_UPD: assuming SP for now"
      );

      // Decrement SP by 4
      Monitor::event_raw() << "ARM::STMDB_UPD: decrementing SP by 4\n";
      BBStateMap[BB]->SP_offset -= 4;

      Monitor::event_raw() << "stack[" << BBStateMap[BB]->SP_offset << "] ~ Reg " << Reg << ", ignored under assumption of function prologue\n";
      // stack_map[BBStateMap[BB]->SP_offset] = getRegValue(Reg, Type::getInt32Ty(Context), BB);
    } break;
    /*
    case ARM::STMIA: { // 1896 | STMIA<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMIA not yet implemented");
    } break;
    case ARM::STMIA_UPD: { // 1897 | STMIA<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMIA_UPD not yet implemented");
    } break;
    case ARM::STMIB: { // 1898 | STMIB<c> <Rn>{!}, <registers>
      assert(false && "ARM::STMIB not yet implemented");
    } break;
    */
    case ARM::STRB_POST_IMM: { // 1902 | STRB Rt Rn Rs Rm=0 AM2Shift CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rs = MI->getOperand(2).getReg();
      Register Rm = MI->getOperand(3).getReg();
      int64_t AM2Shift = MI->getOperand(4).getImm();
      int64_t CC = MI->getOperand(5).getImm();
      Register CPSR = MI->getOperand(6).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::STRB_POST_IMM: assuming no flags for now"
      );

      Value* Addr = resolveAM2Shift(Rn, Rs, Rm, AM2Shift, BB);
      Instruction* Cast = new IntToPtrInst(Addr, Type::getInt8PtrTy(Context), "STRB_POST_IMMCast", BB);
      Monitor::event_Instruction(Cast);

      Value* Val = getRegValue(Rt, Type::getInt32Ty(Context), BB);
      Instruction* Trunc = new TruncInst(Val, Type::getInt8Ty(Context), "STRB_POST_IMMValTrunc", BB);
      Monitor::event_Instruction(Trunc);

      Instruction* Instr = new StoreInst(Trunc, Cast, false, BB);
      Monitor::event_Instruction(Instr);
    } break;
    case ARM::STRBi12: { // 1906 | STRB Rt Rn Imm12 CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Imm12 = MI->getOperand(2).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
      assert(!conditional_execution && "ARM::STRBi12: conditional execution not supported yet");

      Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
      Instruction* Trunc = new TruncInst(RtVal, Type::getInt8Ty(Context), "STRBi12_ValTrunc", BB);
      Monitor::event_Instruction(Trunc);
      RtVal = Trunc;

      Value* Addr;
      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
        Addr = getOrCreateStackValue(Rn, Imm12, Type::getInt8PtrTy(Context), BB);
      } else {
        Addr = getRegValue(Rn, Type::getInt8PtrTy(Context), BB);
        Value* Offset = ConstantInt::get(Type::getInt32Ty(Context), Imm12);
        Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(Context), Addr, Offset, "STRBi12_GEP", BB);
        Monitor::event_Instruction(GEP);
        Addr = GEP;
      }

      Instruction* Store = new StoreInst(RtVal, Addr, false, BB);
      Monitor::event_Instruction(Store);
    } break;
    case ARM::STREX: { // 1911 | STREX Rd Rt Rn CC CPSR
      Register Rd = MI->getOperand(0).getReg();
      Register Rt = MI->getOperand(1).getReg();
      Register Rn = MI->getOperand(2).getReg();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "STREX: assuming no flags for now"
      );

      Value* Ptr = getRegValue(Rt, Type::getInt32PtrTy(Context), BB);
      Value* RnVal = getRegValue(Rn, Type::getInt32Ty(Context), BB);

      Function* STREX = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_strex, {Type::getInt32PtrTy(Context)});
      Instruction* Instr = CallInst::Create(STREX, { RnVal, Ptr }, "STREX", BB);
      Monitor::event_Instruction(Instr);

      setRegValue(Rd, Instr, BB);
    } break;
    case ARM::STRH: { // 1915 | STRH Rt Rn AM3Reg AM3Imm CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register AM3Reg = MI->getOperand(2).getReg();
      int64_t AM3Imm = MI->getOperand(3).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();
      bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

      assert(!conditional_execution && "ARM::STRH: conditional execution not supported yet");

      Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
      Instruction* Trunc = new TruncInst(RtVal, Type::getInt16Ty(Context), "STRH_ValTrunc", BB);
      Monitor::event_Instruction(Trunc);
      RtVal = Trunc;

      Value* Addr;
      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
        assert(AM3Reg == 0 && "ARM::STRH: accessing stack with reg offet not supported yet");
        Addr = getOrCreateStackValue(Rn, AM3Imm, Type::getInt16PtrTy(Context), BB);
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
    } break;
    case ARM::STRi12: { // 1926 | STR Rt Rn Imm12 CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Imm12 = MI->getOperand(2).getImm();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::STRi12: assuming no flags for now"
      );

      Value* Val = getRegValue(Rt, Type::getInt32Ty(Context), BB);
      Value* Ptr;

      if (Rn == ARM::SP || (Rn == ARM::R11 && BBStateMap[BB]->R11_is_FP)) {
        Ptr = getOrCreateStackValue(Rn, Imm12, Type::getInt32PtrTy(Context), BB);
      } else {
        Ptr = getRegValue(Rn, Type::getInt32PtrTy(Context), BB);
        if (Imm12 != 0) {
          Instruction* GEP = GetElementPtrInst::Create(Type::getInt32Ty(Context), Ptr, ConstantInt::get(Type::getInt32Ty(Context), Imm12, false), "STRi12GEP", BB);
          Monitor::event_Instruction(GEP);
          Ptr = GEP;
        }
      }

      Instruction* Instr = new StoreInst(Val, Ptr, false, BB);
      Monitor::event_Instruction(Instr);
    } break;
    case ARM::STRrs: { // 1927 | STR Rt Rn Rm AM2Shift CC CPSR
      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      int64_t AM2Shift = MI->getOperand(3).getImm();
      int64_t CC = MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "assuming no flags for now"
      );

      Value* Addr = resolveAM2Shift(Rn, 0, Rm, AM2Shift, BB);
      Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "STRrsPtrCast", BB);
      Monitor::event_Instruction(Cast);
      Value* RtVal = getRegValue(Rt, Type::getInt32Ty(Context), BB);
      Instruction* Store = new StoreInst(RtVal, Cast, false, BB);
      Monitor::event_Instruction(Store);
    } break;
    case ARM::SUBri: { // 1928 | SUB Rd Rn Op2 CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Op2 = MI->getOperand(2).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));

      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::SUBri: assuming no conditional flags for now"
      );

      if (Rd == ARM::SP && Rn == ARM::SP) {
        Monitor::event_raw() << "decrementing SP by " << Imm << "\n";
        BBStateMap[BB]->SP_offset -= Imm;
        break;
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
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "SUBSriZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag
        Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
        Instruction* CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
        Monitor::event_Instruction(CallUSub);
        Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSriCFlag", BB);
        Monitor::event_Instruction(C_Flag);
        BBStateMap[BB]->setCFlag(C_Flag);

        // Overflow flag
        Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
        Instruction* CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
        Monitor::event_Instruction(CallSSub);
        Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSriVFlag", BB);
        Monitor::event_Instruction(V_Flag);
        BBStateMap[BB]->setVFlag(V_Flag);
      }
    } break;
    case ARM::SUBrr: { // 1929 | SUB Rd Rn Rm CC CPSR S
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
        BBStateMap[BB]->setNFlag(CmpNeg);

        // Zero flag
        Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(Context), 0), "SUBSZero", BB);
        Monitor::event_Instruction(CmpZero);
        BBStateMap[BB]->setZFlag(CmpZero);

        // Carry flag
        Function* USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(Context));
        Instruction* CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
        Monitor::event_Instruction(CallUSub);
        Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSCFlag", BB);
        Monitor::event_Instruction(C_Flag);
        BBStateMap[BB]->setCFlag(C_Flag);

        // Overflow flag
        Function* SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(Context));
        Instruction* CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
        Monitor::event_Instruction(CallSSub);
        Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSVFlag", BB);
        Monitor::event_Instruction(V_Flag);
        BBStateMap[BB]->setVFlag(V_Flag);
      }

      setRegValue(Rd, Result, BB);

      if (conditional_execution) {
        Instruction* Branch = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(Branch);
      }
    } break;
    /*
    case ARM::SVC: { // 1932 | SVC<c> #<imm>
      assert(false && "ARM::SVC not yet implemented");
    } break;
    */
    case ARM::VADDS: { // 2058 | FADDS Fd Fn Fm CC CPSR
      assert(false && "ARM::VADDS not yet implemented");
    }

  }
  Monitor::event_end("ARMLinearRaiserPass::RaiseMachineInstr");
  return true;
}

#undef DEBUG_TYPE
