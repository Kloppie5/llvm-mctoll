
#include "ARMRaiser.h"

#include "Monitor.h"

#include "ARMModuleRaiser.h"
#include "IncludedFileInfo.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"

using namespace llvm;

/* static */ AllocaInst* ARMRaiser::getOrCreateStackAlloca(ARMRaiserState* State, Register Reg, int64_t offset, Type* Ty, BasicBlock* BB) {
  return nullptr;
}
/* static */ AllocaInst* ARMRaiser::getOrCreateStackAlloca(ARMRaiserState* State, int64_t offset, Type* Ty, BasicBlock* BB) {
  return nullptr;
}


/* static */ Value* ARMRaiser::resolveAM2Shift(ARMRaiserState* State, Register Rn, Register Rs, Register Rm, int64_t AM2Shift, BasicBlock* BB) {
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

  Value* Val = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* Offset;

  if (Rm != 0) {
    Offset = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);
    if (!(ShiftOpcode == ARM_AM::lsr && Imm12 == 0)) {
      Instruction* Shift = BinaryOperator::Create(ShiftOp, Offset, ConstantInt::get(Type::getInt32Ty(State->Context), Imm12), "AM2Shift", BB);
      Monitor::event_Instruction(Shift);
      Offset = Shift;
    }
  } else {
    Offset = ConstantInt::get(Type::getInt32Ty(State->Context), Imm12);
  }

  Instruction* Add = BinaryOperator::Create(isSub ? Instruction::Sub : Instruction::Add, Val, Offset, "AM2Shift", BB);
  Monitor::event_Instruction(Add);

  if (Rs != 0)
    State->setReg(Rs, Add, BB);

  if (IdxMode == ARMII::IndexModeNone || IdxMode == ARMII::IndexModePre)
    return Add;
  if (IdxMode == ARMII::IndexModePost)
    return Val;

  assert(false && "AM2Shift: invalid index mode");
}

// TODO: merge with X86MachineInstructionRaiser::createPCRelativeAccesssValue
/* static */ GlobalValue* ARMRaiser::getGlobalValueByOffset(ARMRaiserState* State, int64_t MCInstOffset, uint64_t PCOffset, Type* Ty) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser&>(State->MR);
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

  assert(State->MCIR != nullptr && "MCInstRaiser was not initialized!");
  if (Symbol == nullptr) {
    auto Iter = State->MCIR->getMCInstAt(Target);
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
    GlobVal = State->MR.getModule()->getGlobalVariable(SymName);
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
        GlobValTy = Type::getInt32Ty(State->Context);
        break;
      case 2:
        GlobValTy = Type::getInt16Ty(State->Context);
        break;
      case 1:
        GlobValTy = Type::getInt8Ty(State->Context);
        break;
      default:
        GlobValTy = ArrayType::get(Type::getInt8Ty(State->Context), SymSz);
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

    return new GlobalVariable(*State->MR.getModule(), GlobValTy, false /* isConstant */,
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

    if (State->MCIR->getMCInstAt(Target) == State->MCIR->const_mcinstr_end()) {
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
    GlobVal = State->MR.getModule()->getGlobalVariable(LocalNameRef);
    if (GlobVal) {
      Monitor::event_raw() << "Found " << LocalName << "\n";
      return GlobVal;
    }

    Monitor::event_raw() << "Creating GlobalValue " << LocalName << "\n";

    if (Ty == Type::getInt32Ty(State->Context)) {
      uint32_t Data = State->MCIR->getMCInstAt(Target)->second.getData();
      return new GlobalVariable(*State->MR.getModule(), Ty, false /* isConstant */,
                                GlobalValue::InternalLinkage,
                                ConstantInt::get(Ty, Data), LocalName);
    }
    if (Ty == Type::getDoubleTy(State->Context)) {
      uint32_t firsthalf = State->MCIR->getMCInstAt(Target)->second.getData();
      uint32_t secondhalf = State->MCIR->getMCInstAt(Target + 4)->second.getData();
      uint64_t InitVal = ((uint64_t) firsthalf << 32) | secondhalf;
      // double_t InitValD = *reinterpret_cast<double_t*>(&InitVal); Somewhat defined undefined behaviour because of type pruning
      double_t InitValD; memcpy(&InitValD, &InitVal, sizeof(InitValD));
      return new GlobalVariable(*State->MR.getModule(), Ty, false /* isConstant */,
                                GlobalValue::InternalLinkage,
                                ConstantFP::get(State->Context, APFloat(InitValD)), LocalName);
    }

    MCInstOrData MD = State->MCIR->getMCInstAt(Target)->second;
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
              ConstantDataArray::getString(State->Context, ROStringRef);
          GlobalValue* GlobalStrConstVal = new GlobalVariable(
              * State->MR.getModule(), StrConstant->getType(), /* isConstant */ true,
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

/* static */ Value* ARMRaiser::ARMCCToValue(ARMRaiserState* State, int Cond, BasicBlock* BB) {
  // Why do ICmpInst constructors put &InsertBefore/&InsertAtEnd as the first
  // operand instead of *InsertBefore/*InsertAtEnd as the last one? Who knows.
  switch (Cond) {
    default:
      llvm_unreachable("Unknown condition code!");
    case ARMCC::EQ: { // Z = 1
      Value* Z = State->getStatus(ARMState::CPSR_Z, Type::getInt1Ty(State->Context), BB);
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(State->Context), "ZS");
      Monitor::event_Instruction(ZS);
      return ZS;
    } break;
    case ARMCC::NE: { // Z = 0
      Value* Z = State->getStatus(ARMState::CPSR_Z, Type::getInt1Ty(State->Context), BB);
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(State->Context), "ZC");
      Monitor::event_Instruction(ZC);
      return ZC;
    } break;
    case ARMCC::HS: { // C = 1
      Value* C = State->getStatus(ARMState::CPSR_C, Type::getInt1Ty(State->Context), BB);
      Instruction* CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(State->Context), "CS");
      Monitor::event_Instruction(CS);
      return CS;
    } break;
    case ARMCC::LO: { // C = 0
      Value* C = State->getStatus(ARMState::CPSR_C, Type::getInt1Ty(State->Context), BB);
      Instruction* CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(State->Context), "CC");
      Monitor::event_Instruction(CC);
      return CC;
    } break;
    case ARMCC::MI: { // N = 1
      Value* N = State->getStatus(ARMState::CPSR_N, Type::getInt1Ty(State->Context), BB);
      Instruction* NS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getTrue(State->Context), "NS");
      Monitor::event_Instruction(NS);
      return NS;
    } break;
    case ARMCC::PL: { // N = 0
      Value* N = State->getStatus(ARMState::CPSR_N, Type::getInt1Ty(State->Context), BB);
      Instruction* NC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getFalse(State->Context), "NC");
      Monitor::event_Instruction(NC);
      return NC;
    } break;
    case ARMCC::VS: { // V = 1
      Value* V = State->getStatus(ARMState::CPSR_V, Type::getInt1Ty(State->Context), BB);
      Instruction* VS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getTrue(State->Context), "VS");
      Monitor::event_Instruction(VS);
      return VS;
    } break;
    case ARMCC::VC: { // V = 0
      Value* V = State->getStatus(ARMState::CPSR_V, Type::getInt1Ty(State->Context), BB);
      Instruction* VC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getFalse(State->Context), "VC");
      Monitor::event_Instruction(VC);
      return VC;
    } break;
    case ARMCC::HI: { // C = 1 && Z = 0
      Value* C = State->getStatus(ARMState::CPSR_C, Type::getInt1Ty(State->Context), BB);
      Instruction* CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(State->Context), "CS");
      Monitor::event_Instruction(CS);
      Value* Z = State->getStatus(ARMState::CPSR_Z, Type::getInt1Ty(State->Context), BB);
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(State->Context), "ZC");
      Monitor::event_Instruction(ZC);
      Instruction* HI = BinaryOperator::Create(Instruction::Add, CS, ZC, "HI", BB);
      Monitor::event_Instruction(HI);
      return HI;
    } break;
    case ARMCC::LS: { // C = 0 || Z = 1
      Value* C = State->getStatus(ARMState::CPSR_C, Type::getInt1Ty(State->Context), BB);
      Instruction* CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(State->Context), "CC");
      Monitor::event_Instruction(CC);
      Value* Z = State->getStatus(ARMState::CPSR_Z, Type::getInt1Ty(State->Context), BB);
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(State->Context), "ZS");
      Monitor::event_Instruction(ZS);
      Instruction* LS = BinaryOperator::Create(Instruction::Or, CC, ZS, "LS", BB);
      Monitor::event_Instruction(LS);
      return LS;
    } break;
    case ARMCC::GE: { // N = V
      Value* N = State->getStatus(ARMState::CPSR_N, Type::getInt1Ty(State->Context), BB);
      Value* V = State->getStatus(ARMState::CPSR_V, Type::getInt1Ty(State->Context), BB);
      Instruction* GE = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "GE");
      Monitor::event_Instruction(GE);
      return GE;
    } break;
    case ARMCC::LT: { // N != V
      Value* N = State->getStatus(ARMState::CPSR_N, Type::getInt1Ty(State->Context), BB);
      Value* V = State->getStatus(ARMState::CPSR_V, Type::getInt1Ty(State->Context), BB);
      Instruction* LT = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "LT");
      Monitor::event_Instruction(LT);
      return LT;
    } break;
    case ARMCC::GT: { // Z = 0 && N = V
      Value* Z = State->getStatus(ARMState::CPSR_Z, Type::getInt1Ty(State->Context), BB);
      Instruction* ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(State->Context), "ZC");
      Monitor::event_Instruction(ZC);
      Value* N = State->getStatus(ARMState::CPSR_N, Type::getInt1Ty(State->Context), BB);
      Value* V = State->getStatus(ARMState::CPSR_V, Type::getInt1Ty(State->Context), BB);
      Instruction* Sign = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "S");
      Monitor::event_Instruction(Sign);
      Instruction* GT = BinaryOperator::Create(Instruction::And, ZC, Sign, "GT", BB);
      Monitor::event_Instruction(GT);
      return GT;
    } break;
    case ARMCC::LE: { // Z = 1 || N != V
      Value* Z = State->getStatus(ARMState::CPSR_Z, Type::getInt1Ty(State->Context), BB);
      Instruction* ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(State->Context), "ZS");
      Monitor::event_Instruction(ZS);
      Value* N = State->getStatus(ARMState::CPSR_N, Type::getInt1Ty(State->Context), BB);
      Value* V = State->getStatus(ARMState::CPSR_V, Type::getInt1Ty(State->Context), BB);
      Instruction* Sign = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "S");
      Monitor::event_Instruction(Sign);
      Instruction* LE = BinaryOperator::Create(Instruction::Or, ZS, Sign, "LE", BB);
      Monitor::event_Instruction(LE);
      return LE;
    } break;
    case ARMCC::AL: { // always
      return ConstantInt::getTrue(State->Context);
    } break;
  }
}

// INSTRUCTIONS

/* static */ bool ARMRaiser::raiseADCri(ARMRaiserState* State, MachineInstr* MI) { // 680 | ADC Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Add = BinaryOperator::Create(Instruction::Add, RnVal, ImmVal, "ADCri", BB);
  Monitor::event_Instruction(Add);

  Value* Carry = State->getStatus(ARMState::CPSR_C, Type::getInt1Ty(State->Context), BB);
  Instruction* ZExt = new ZExtInst(Carry, Type::getInt32Ty(State->Context), "ZExt", BB);
  Monitor::event_Instruction(ZExt);
  Instruction* Result = BinaryOperator::Create(Instruction::Add, Add, ZExt, "ADCri", BB);
  Monitor::event_Instruction(Result);

  assert(!update_flags && "Unhandled instruction flags");

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseADDri(ARMRaiserState* State, MachineInstr* MI) { // 684 | ADD Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    State->BBStateMap[BB]->QState->SP_offset += Imm;
    Monitor::event_raw() << "Incrementing stack by " << Imm << "\n";
    return true;
  }

  if (Rn == ARM::SP && State->BBStateMap[BB]->QState->R13_is_SP) {
    assert(!conditional_execution && !update_flags && "Unhandled instruction flags");
    AllocaInst* alloca = getOrCreateStackAlloca(State, Rn, Imm, Type::getInt32Ty(State->Context), BB);
    Instruction* Cast = new PtrToIntInst(alloca, Type::getInt32Ty(State->Context), "StackAllocaDowncast", BB);
    Monitor::event_Instruction(Cast);
    State->setReg(Rd, Cast, BB);
    return true;
  }
  if (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP) { // Load StackValue
    assert(!conditional_execution && !update_flags && "Unhandled instruction flags");
    AllocaInst* alloca = getOrCreateStackAlloca(State, Rn, Imm, Type::getInt32Ty(State->Context), BB);
    Instruction* Cast = new PtrToIntInst(alloca, Type::getInt32Ty(State->Context), "StackAllocaDowncast", BB);
    Monitor::event_Instruction(Cast);
    State->setReg(Rd, Cast, BB);
    return true;
  }

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::Add, RnVal, ImmVal, "ADDri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ADDSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ADDSZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag
    Function* UAdd = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, ImmVal}, "UAddInstrinsic", BB);
    Monitor::event_Instruction(CallUAdd);
    Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "ADDSCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    State->setStatus(ARMState::CPSR_C, C_Flag, BB);

    // Overflow flag
    Function* SAdd = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, ImmVal}, "SAddIntrinsic", BB);
    Monitor::event_Instruction(CallSAdd);
    Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "ADDSVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    State->setStatus(ARMState::CPSR_V, V_Flag, BB);
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseADDrr(ARMRaiserState* State, MachineInstr* MI) { // 685 | ADD Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Add, RnVal, RmVal, "ADDrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ADDSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ADDSZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag
    Function* UAdd = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, RmVal}, "UAddInstrinsic", BB);
    Monitor::event_Instruction(CallUAdd);
    Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "ADDSCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    State->setStatus(ARMState::CPSR_C, C_Flag, BB);

    // Overflow flag
    Function* SAdd = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, RmVal}, "SAddIntrinsic", BB);
    Monitor::event_Instruction(CallSAdd);
    Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "ADDSVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    State->setStatus(ARMState::CPSR_V, V_Flag, BB);
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseADDrsi(ARMRaiserState* State, MachineInstr* MI) { // 686 | ADD Rd Rn Rm Shift CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
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

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "ADDrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(State->Context), ShiftAmount), "ADDrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);
  Instruction* Instr = BinaryOperator::Create(Instruction::Add, RnVal, ShiftInstr, "ADDrsi", BB);
  Monitor::event_Instruction(Instr);
  State->setReg(Rd, Instr, BB);

  assert(!update_flags && "ADDrsi: assuming no S flag for now");

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseANDri(ARMRaiserState* State, MachineInstr* MI) { // 693 | AND Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "ANDri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ANDSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ANDSZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag update cant occur for constant Op2
    // Overflow flag is not updated
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseANDrsi(ARMRaiserState* State, MachineInstr* MI) { // 695 | AND Rd Rn Rm Shift CC CPSR S
    MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "ANDrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(State->Context), ShiftAmount), "ANDrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, ShiftInstr, "ANDrsi", BB);

  State->setReg(Rd, Result, BB);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "CmpNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "CMPriZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag can be set by Operand2 calculation; TODO
    // Overflow flag is not updated
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseANDrr(ARMRaiserState* State, MachineInstr* MI) { // 694 | AND Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, RmVal, "ANDrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    assert(false && "ANDrr: update flags not implemented");
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseBFC(ARMRaiserState* State, MachineInstr* MI) { //  704 | BFC Rd {Rwb} Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RdVal = State->getReg(Rd, Type::getInt32Ty(State->Context), BB);
  // Instead of the lsb and width, llvm saves the expanded form
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RdVal, ImmVal, "BFC", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseBICri(ARMRaiserState* State, MachineInstr* MI) { // 706 | BIC Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), -Imm);
  Instruction* Instr = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "BICri", BB);
  Monitor::event_Instruction(Instr);
  State->setReg(Rd, Instr, BB);

  assert(!update_flags && "BICri: update flags not implemented");

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseBL(ARMRaiserState* State, MachineInstr* MI) { // 711 | BL Imm
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(State->MR);
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  int64_t Imm = MI->getOperand(0).getImm();
  int64_t offset = State->MCIR->getMCInstIndex(*MI);
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


      CalledFunc = IncludedFileInfo::CreateFunction(*CalledFuncSymName, AMR);
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
    if (i < 4) ArgVal = State->getReg(ARM::R0 + i, Type::getInt32Ty(State->Context), BB);
    else {
      auto I = State->BBStateMap[BB]->QState->stack_map.find((i - 4)*  4);
      assert(I != State->BBStateMap[BB]->QState->stack_map.end() && "Failed to find stack value");
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
      Instruction* Cast = new PtrToIntInst(Call, Type::getInt32Ty(State->Context), "", BB);
      Monitor::event_Instruction(Cast);
      State->setReg(ARM::R0, Cast, BB);
    } else
      State->setReg(ARM::R0, Call, BB);
  }

  if (CalledFunc->getName() == "exit" || CalledFunc->getName() == "__assert_fail") {
    Instruction* Unreachable = new UnreachableInst(BB->getContext(), BB);
    Monitor::event_Instruction(Unreachable);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseBL_pred(ARMRaiserState* State, MachineInstr* MI) { // 715 | BL Imm CC CPSR
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(State->MR);
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  int64_t Imm = MI->getOperand(0).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t offset = State->MCIR->getMCInstIndex(*MI);
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

      CalledFunc = IncludedFileInfo::CreateFunction(*CalledFuncSymName, AMR);
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
    if (i < 4) ArgVal = State->getReg(ARM::R0 + i, Type::getInt32Ty(State->Context), BB);
    else {
      auto I = State->BBStateMap[BB]->QState->stack_map.find((i - 4)*  4);
      assert(I != State->BBStateMap[BB]->QState->stack_map.end() && "Failed to find stack value");
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
      Instruction* Cast = new PtrToIntInst(Call, Type::getInt32Ty(State->Context), "", BB);
      Monitor::event_Instruction(Cast);
      State->setReg(ARM::R0, Cast, BB);
    } else
      State->setReg(ARM::R0, Call, BB);
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
/* static */ bool ARMRaiser::raiseBX_RET(ARMRaiserState* State, MachineInstr* MI) { // 718 | BX_RET CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(0).getImm();
  Register CPSR = MI->getOperand(1).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Type* RetTy = State->F->getReturnType();

  if (RetTy->isVoidTy()) {
    Instruction* Instr = ReturnInst::Create(State->Context, BB);
    Monitor::event_Instruction(Instr);
  } else {
    Instruction* Instr = ReturnInst::Create(State->Context, State->getReg(ARM::R0, Type::getInt32Ty(State->Context), BB), BB);
    Monitor::event_Instruction(Instr);
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseBcc(ARMRaiserState* State, MachineInstr* MI) { // 720 | Bcc offset CC CPSR
  // int64_t offset = MI->getOperand(0).getImm();
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();

  Monitor::event_raw() << "Succ_size: " << MBB->succ_size() << "\n";
  if (MBB->succ_size() == 0) {
    // Folded call and return;
    ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(State->MR);

    int64_t Imm = MI->getOperand(0).getImm();
    int64_t offset = State->MCIR->getMCInstIndex(*MI);
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

        CalledFunc = IncludedFileInfo::CreateFunction(*CalledFuncSymName, AMR);
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
      if (i < 4) ArgVal = State->getReg(ARM::R0 + i, Type::getInt32Ty(State->Context), BB);
      else {
        auto I = State->BBStateMap[BB]->QState->stack_map.find((i - 4)*  4);
        assert(I != State->BBStateMap[BB]->QState->stack_map.end() && "Failed to find stack value");
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
      State->setReg(ARM::R0, Instr, BB);
    }

    if (State->F->getReturnType()->isVoidTy()) {
      Instruction* Instr = ReturnInst::Create(State->Context, BB);
      Monitor::event_Instruction(Instr);
    } else {
      Instruction* Instr = ReturnInst::Create(State->Context, State->getReg(ARM::R0, Type::getInt32Ty(State->Context), BB), BB);
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
    BasicBlock* BranchBB = State->getBasicBlocks(*succ_itt).front();
    Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
    Instruction* Instr = BranchInst::Create(BranchBB, BB);
    Monitor::event_Instruction(Instr);
  } else {
    assert(
      MBB->succ_size() == 2 &&
      "Bcc: assuming normal conditional branch for now"
    );

    Value* Cond = ARMCCToValue(State, CC, BB);

    auto succ_itt = MBB->succ_begin();
    BasicBlock* BranchBB = State->getBasicBlocks(*succ_itt).front();
    Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
    BasicBlock* NextBB = State->getBasicBlocks(*++succ_itt).front();
    Monitor::event_raw() << "Next BB: " << NextBB->getName() << "\n";
    Instruction* Branch = BranchInst::Create(BranchBB, NextBB, Cond, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseCLZ(ARMRaiserState* State, MachineInstr* MI) { //  754 | CLZ Rd Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).front();

  Register Rd = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Function* Ctlz = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ctlz, Type::getInt32Ty(State->Context));
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);
  Instruction* Call = CallInst::Create(Ctlz, {RmVal, ConstantInt::get(Type::getInt1Ty(State->Context), 1)}, "CLZ", BB);
  Monitor::event_Instruction(Call);

  State->setReg(Rd, Call, BB);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseCMNri(ARMRaiserState* State, MachineInstr* MI) { // 755 | CMN Rn Op2 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);
  Value* NImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), -Imm);
  // Not entirely correct, but negative zero compares shouldnt be emitted by compilers anyways

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, NImmVal, "CMNriNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, NImmVal, "CMNriZero", BB);
  Monitor::event_Instruction(CmpZero);
  State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

  // Carry flag
  Function* UAdd = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::uadd_with_overflow, Type::getInt32Ty(State->Context));
  Instruction* CallUAdd = CallInst::Create(UAdd, {RnVal, ImmVal}, "UAddInstrinsic", BB);
  Monitor::event_Instruction(CallUAdd);
  Instruction* C_Flag = ExtractValueInst::Create(CallUAdd, 1, "CMNCFlag", BB);
  Monitor::event_Instruction(C_Flag);
  State->setStatus(ARMState::CPSR_C, C_Flag, BB);

  // Overflow flag
  Function* SAdd = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::sadd_with_overflow, Type::getInt32Ty(State->Context));
  Instruction* CallSAdd = CallInst::Create(SAdd, {RnVal, ImmVal}, "SAddIntrinsic", BB);
  Monitor::event_Instruction(CallSAdd);
  Instruction* V_Flag = ExtractValueInst::Create(CallSAdd, 1, "CMNVFlag", BB);
  Monitor::event_Instruction(V_Flag);
  State->setStatus(ARMState::CPSR_V, V_Flag, BB);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseCMPri(ARMRaiserState* State, MachineInstr* MI) { // 759 | CMP Rn Op2 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "CMPriNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "CMPriZero", BB);
  Monitor::event_Instruction(CmpZero);
  State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

  // Carry flag
  //Instruction* CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, imm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
  Function* USub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(State->Context));
  Instruction* CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
  Monitor::event_Instruction(CallUSub);
  Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPriCFlag", BB);
  Monitor::event_Instruction(C_Flag);
  State->setStatus(ARMState::CPSR_C, C_Flag, BB);

  // Overflow flag
  //Instruction* CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, imm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
  Function* SSub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(State->Context));
  Instruction* CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
  Monitor::event_Instruction(CallSSub);
  Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPriVFlag", BB);
  Monitor::event_Instruction(V_Flag);
  State->setStatus(ARMState::CPSR_V, V_Flag, BB);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseCMPrr(ARMRaiserState* State, MachineInstr* MI) { // 760 | CMP Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, RmVal, "CMPrrNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, RmVal, "CMPrrZero", BB);
  Monitor::event_Instruction(CmpZero);
  State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

  // Carry flag
  Function* USub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(State->Context));
  Instruction* CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
  Monitor::event_Instruction(CallUSub);
  Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPrrCFlag", BB);
  Monitor::event_Instruction(C_Flag);
  State->setStatus(ARMState::CPSR_C, C_Flag, BB);

  // Overflow flag
  Function* SSub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(State->Context));
  Instruction* CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
  Monitor::event_Instruction(CallSSub);
  Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPrrVFlag", BB);
  Monitor::event_Instruction(V_Flag);
  State->setStatus(ARMState::CPSR_V, V_Flag, BB);

  if (conditional_execution) {
    Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(MergeIf);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseDMB(ARMRaiserState* State, MachineInstr* MI) { // 773 | DMB Imm
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  // Call ARM DMB intrinsic

  int64_t Imm = MI->getOperand(0).getImm();

  Function* DMB = Intrinsic::getDeclaration(State->MR.getModule(),Intrinsic::arm_dmb);
  Instruction* Instr = CallInst::Create(DMB, { ConstantInt::get(Type::getInt32Ty(State->Context), Imm) }, "", BB);
  Monitor::event_Instruction(Instr);

  return true;
}
/* static */ bool ARMRaiser::raiseEORrr(ARMRaiserState* State, MachineInstr* MI) { // 776 | EOR Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Xor, RnVal, RmVal, "EORrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    assert(false && "EORrr update flags not yet implemented");
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseHINT(ARMRaiserState* State, MachineInstr* MI) { //  790 | HINT Imm CC CPSR
  // MachineBasicBlock* MBB = MI->getParent();
  // BasicBlock* BB = State->getBasicBlocks(MBB).back();

  int64_t Imm = MI->getOperand(0).getImm();
  // ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  // Register CPSR = MI->getOperand(2).getReg();
  // bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  // BasicBlock* MergeBB;
  // if (conditional_execution) {
  //   Value* Cond = ARMCCToValue(State, CC, BB);
  //   BasicBlock* CondExecBB = State->createBasicBlock(MBB);
  //   MergeBB = State->createBasicBlock(MBB);
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
/* static */ bool ARMRaiser::raiseLDMIA(ARMRaiserState* State, MachineInstr* MI) { //  821 | LDMIA Rt CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(3).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(State, Rt, 0, Type::getInt32Ty(State->Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Ptr, "LDMIA", BB);
    Monitor::event_Instruction(Load);

    State->setReg(Rn, Load, BB);

    return true;
  }

  assert(Rt != ARM::PC && "LDMIA: Rt == PC");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(State->Context), "LDMIA", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Ptr, "LDMIA", BB);
  Monitor::event_Instruction(Load);

  State->setReg(Rn, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDMIA_UPD(ARMRaiserState* State, MachineInstr* MI) { // 822 | LDMIA Rt! {Rwb} CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* Ptr = getOrCreateStackAlloca(State, Rt, 0, Type::getInt32Ty(State->Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Ptr, "LDMIA_UPD", BB);
    Monitor::event_Instruction(Load);

    State->setReg(Rn, Load, BB);

    Monitor::event_raw() << "incrementing SP by 4\n";
    State->BBStateMap[BB]->QState->SP_offset += 4;
    return true;
  }

  assert(Rt != ARM::PC && "LDMIA_UPD: Rt == PC");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(State->Context), "LDMIA_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Ptr, "LDMIA_UPD", BB);
  Monitor::event_Instruction(Load);

  State->setReg(Rn, Load, BB);

  Monitor::event_raw() << "LDMIA_UPD: incrementing Rt by 4\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Add, RtVal, ConstantInt::get(Type::getInt32Ty(State->Context), 4), "LDMIA_UPD", BB);
  Monitor::event_Instruction(Add);

  State->setReg(Rt, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDRB_PRE_REG(ARMRaiserState* State, MachineInstr* MI) { // 830 | LDRB_PRE_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(3).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(State, Rn, Rn, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt8PtrTy(State->Context), "LDRB_PRE_REGCast", BB);
  Monitor::event_Instruction(Cast);

  Instruction* Load = new LoadInst(Type::getInt8Ty(State->Context), Cast, "LDRB_PRE_REGLoad", BB);
  Monitor::event_Instruction(Load);

  Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(State->Context), "LDRB_PRE_REGZExt", BB);
  Monitor::event_Instruction(ZExt);

  State->setReg(Rt, ZExt, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDRBi12(ARMRaiserState* State, MachineInstr* MI) { // 831 | LDRB Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) {
    Addr = getOrCreateStackAlloca(State, Rn, Imm12, Type::getInt8PtrTy(State->Context), BB);
  } else {
    Addr = State->getReg(Rn, Type::getInt8PtrTy(State->Context), BB);
    Value* Offset = ConstantInt::get(Type::getInt32Ty(State->Context), Imm12);
    Addr = State->getReg(Rn, Type::getInt8PtrTy(State->Context), BB);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt8Ty(State->Context), Addr, Offset, "LDRBi12GEP", BB);
    Monitor::event_Instruction(GEP);
    Addr = GEP;
  }
  // Load
  Instruction* Load = new LoadInst(Type::getInt8Ty(State->Context), Addr, "LDRBi12", BB);
  Monitor::event_Instruction(Load);
  // Zero extend
  Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(State->Context), "LDRBi12ZExt", BB);
  Monitor::event_Instruction(ZExt);

  State->setReg(Rt, ZExt, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDREX(ARMRaiserState* State, MachineInstr* MI) { // 836 | LDREX Rt Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Ptr = State->getReg(Rn, Type::getInt32PtrTy(State->Context), BB);

  Function* LDREX = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::arm_ldrex, {Type::getInt32PtrTy(State->Context)});
  Instruction* Instr = CallInst::Create(LDREX, { Ptr }, "LDREX", BB);
  Monitor::event_Instruction(Instr);
  State->setReg(Rt, Instr, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDRH(ARMRaiserState* State, MachineInstr* MI) { // 840 | LDRH Rt Rn AM3Reg AM3Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register AM3Reg = MI->getOperand(2).getReg();
  int64_t AM3Imm = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) {
    assert(AM3Reg == 0 && "ARM::STRH: accessing stack with reg offset not supported yet");
    Addr = getOrCreateStackAlloca(State, Rn, AM3Imm, Type::getInt16PtrTy(State->Context), BB);
  } else {
    Addr = State->getReg(Rn, Type::getInt16PtrTy(State->Context), BB);
    Value* Offset;
    if (AM3Reg == 0)
      Offset = ConstantInt::get(Type::getInt32Ty(State->Context), AM3Imm);
    else
      Offset = State->getReg(AM3Reg, Type::getInt32Ty(State->Context), BB);
    Addr = State->getReg(Rn, Type::getInt16PtrTy(State->Context), BB);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(State->Context), Addr, Offset, "LDRH_GEP", BB);
    Monitor::event_Instruction(GEP);
    Addr = GEP;
  }
  // Load
  Instruction* Load = new LoadInst(Type::getInt16Ty(State->Context), Addr, "LDRH", BB);
  Monitor::event_Instruction(Load);
  // Zero extend
  Instruction* ZExt = new ZExtInst(Load, Type::getInt32Ty(State->Context), "LDRHZExt", BB);
  Monitor::event_Instruction(ZExt);

  State->setReg(Rt, ZExt, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseLDR_POST_IMM(ARMRaiserState* State, MachineInstr* MI) { // 857 | LDR_POST_IMM Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(State, Rn, Rn, 0, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(State->Context), "LDR_POST_IMMCast", BB);
  Monitor::event_Instruction(Cast);
  Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Cast, "LDR_POST_IMM", BB);
  Monitor::event_Instruction(Load);
  State->setReg(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDR_POST_REG(ARMRaiserState* State, MachineInstr* MI) { //  858 | LDR_POST_REG Rt Rn {Rwb} Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(3).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(State, Rn, Rn, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(State->Context), "LDR_POST_REGCast", BB);
  Monitor::event_Instruction(Cast);
  Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Cast, "LDR_POST_REG", BB);
  Monitor::event_Instruction(Load);
  State->setReg(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDRi12(ARMRaiserState* State, MachineInstr* MI) { // 862 | LDR Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rn == ARM::PC) { // Load PC-relative GlobalValue
    Value* GV = getGlobalValueByOffset(State, State->MCIR->getMCInstIndex(*MI), Imm12 + 8, Type::getInt32Ty(State->Context));
    Monitor::event_raw() << "Global Value: " << GV->getName() << "\n";
    Instruction* Cast = new PtrToIntInst(GV, Type::getInt32Ty(State->Context), "LDRi12CastDown", BB);
    Monitor::event_Instruction(Cast);
    State->setReg(Rt, Cast, BB);
    return true;
  }

  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) { // Load StackValue
    AllocaInst* StackValue = getOrCreateStackAlloca(State, Rn, Imm12, Type::getInt32Ty(State->Context), BB);
    Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), StackValue, "LDRi12Load", BB);
    Monitor::event_Instruction(Load);
    State->setReg(Rt, Load, BB);
    return true;
  }

  Value* Ptr = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* Offset = ConstantInt::get(Type::getInt32Ty(State->Context), Imm12);

  Instruction* Add = BinaryOperator::Create(Instruction::Add, Ptr, Offset, "LDRi12Add", BB);
  Monitor::event_Instruction(Add);
  Instruction* Cast = new IntToPtrInst(Add, Type::getInt32PtrTy(State->Context), "LDRi12Cast", BB);
  Monitor::event_Instruction(Cast);

  Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Cast, "LDRi12Load", BB);
  Monitor::event_Instruction(Load);
  State->setReg(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseLDRrs(ARMRaiserState* State, MachineInstr* MI) { // 863 | LDR Rt Rn Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t AM2Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(State, Rn, 0, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(State->Context), "LDRrsCast", BB);
  Monitor::event_Instruction(Cast);
  Instruction* Load = new LoadInst(Type::getInt32Ty(State->Context), Cast, "LDRrsLoad", BB);
  Monitor::event_Instruction(Load);

  State->setReg(Rt, Load, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMLA(ARMRaiserState* State, MachineInstr* MI) { // 868 | MLA Rd Rn Rm Ra CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(
    S == 0 &&
    "ARM::MLA: assuming no S flag for now"
  );

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);
  Value* RaVal = State->getReg(Ra, Type::getInt32Ty(State->Context), BB);

  Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MLA", BB);
  Monitor::event_Instruction(Mul);
  Instruction* Add = BinaryOperator::Create(Instruction::Add, Mul, RaVal, "MLA", BB);
  Monitor::event_Instruction(Add);
  State->setReg(Rd, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMOVTi16(ARMRaiserState* State, MachineInstr* MI) { // 871 |  Rd Rd Imm16 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  assert(MI->getOperand(1).getReg() == Rd && "ARM::MOVTi16: expecting doubled Rd entry");
  int64_t Imm16 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Val = State->getReg(Rd, Type::getInt32Ty(State->Context), BB);

  Instruction* Trunc = new TruncInst(Val, Type::getInt16Ty(State->Context), "MOVTi16Trunc", BB);
  Monitor::event_Instruction(Trunc);
  Val = Trunc;

  Instruction* ZExt = new ZExtInst(Val, Type::getInt32Ty(State->Context), "MOVTi16ZExt", BB);
  Monitor::event_Instruction(ZExt);
  Val = ZExt;

  Instruction* Add = BinaryOperator::Create(Instruction::Add, Val, ConstantInt::get(Type::getInt32Ty(State->Context), Imm16 << 16), "MOVTi16Add", BB);
  Monitor::event_Instruction(Add);
  Val = Add;

  assert(Val->getType() == Type::getInt32Ty(State->Context) && "ARM::MOVTi16: expecting output type to be Ty32");
  State->setReg(Rd, Val, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMOVi(ARMRaiserState* State, MachineInstr* MI) { // 872 | MOV Rt Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  State->setReg(Rt, ConstantInt::get(Type::getInt32Ty(State->Context), Imm), BB);
  Monitor::event_raw() << "Reg " << Rt << " <= " << Imm << "\n";

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMOVi16(ARMRaiserState* State, MachineInstr* MI) { // 873 | MOV Rd Imm16 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  int64_t Imm16 = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  State->setReg(Rd, ConstantInt::get(Type::getInt32Ty(State->Context), Imm16), BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMOVr(ARMRaiserState* State, MachineInstr* MI) { // 874 | MOV Rd Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rd == ARM::R11 && Rn == ARM::SP) {
    State->BBStateMap[BB]->QState->FP_offset = State->BBStateMap[BB]->QState->SP_offset;
    State->BBStateMap[BB]->QState->R11_is_FP = true;
    Monitor::event_raw() << "ARM::MOVr: Updating FP_offset\n";
    return true;
  }
  if (Rd == ARM::SP && Rn == ARM::R11) {
    State->BBStateMap[BB]->QState->SP_offset = State->BBStateMap[BB]->QState->FP_offset;
    Monitor::event_raw() << "ARM::MOVr: Updating SP_offset\n";
    return true;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Monitor::event_raw() << "Reg " << Rd << " <= " << Rn << "\n";
  State->setReg(Rd, RnVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMOVsi(ARMRaiserState* State, MachineInstr* MI) { // 876 | MOV Rd Rm Shift CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  int64_t Shift = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(5).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
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

  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "MOVsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(State->Context), ShiftAmount), "MOVsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);
  State->setReg(Rd, ShiftInstr, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMUL(ARMRaiserState* State, MachineInstr* MI) { // 888 | MUL Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MUL", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "CmpNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "CMPriZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag and Overflow flag are corrupted in ARMv4, unaffected in ARMv5T and above
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }
  return true;
}
/* static */ bool ARMRaiser::raiseMVNi(ARMRaiserState* State, MachineInstr* MI) { // 1736 | Rd Imm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(4).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(!update_flags && "MVNi: assuming no flags for now");

  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), ~Imm);
  Monitor::event_raw() << "Reg: " << Rd << " <= " << ~Imm << "\n";
  State->setReg(Rd, ImmVal, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseMVNr(ARMRaiserState* State, MachineInstr* MI) { // 1737 | MVN Rd Rn CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register S = MI->getOperand(4).getReg();
  bool update_flags = (S != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(!update_flags && "MVNi: assuming no flags for now");

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);

  Instruction* Xor = BinaryOperator::Create(Instruction::Xor, RnVal, ConstantInt::get(Type::getInt32Ty(State->Context), -1), "MVN", BB);
  Monitor::event_Instruction(Xor);
  Instruction* Add = BinaryOperator::Create(Instruction::Add, Xor, ConstantInt::get(Type::getInt32Ty(State->Context), 1), "MVN", BB);
  Monitor::event_Instruction(Add);

  State->setReg(Rd, Add, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseORRri(ARMRaiserState* State, MachineInstr* MI) { // 1748 | ORR Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::Or, RnVal, ImmVal, "ORRri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ORRSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ORRSZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag update cant occur for constant Op2
    // Overflow flag is not updated
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseORRrr(ARMRaiserState* State, MachineInstr* MI) { // 1749 | ORR Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Or, RnVal, RmVal, "ORRrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ORRSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "ORRSZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag update cant occur for direct register Op2
    // Overflow flag is not updated
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseRSBri(ARMRaiserState* State, MachineInstr* MI) { // 1782 | RSB Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::Sub, ImmVal, RnVal, "RSBri", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, ImmVal, RnVal, "RSBSriNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, ImmVal, RnVal, "RSBSriZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallUSub = CallInst::Create(USub, {ImmVal, RnVal}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "RSBSriCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    State->setStatus(ARMState::CPSR_C, C_Flag, BB);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallSSub = CallInst::Create(SSub, {ImmVal, RnVal}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "RSBSriVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    State->setStatus(ARMState::CPSR_V, V_Flag, BB);
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSBCrsi(ARMRaiserState* State, MachineInstr* MI) { // 1796 | SBC Rd Rn Rm Shift CC CPSR S
    MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "SBCrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(State->Context), ShiftAmount), "SBCrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);

  Value* Carry = State->getStatus(ARMState::CPSR_C, Type::getInt1Ty(State->Context), BB);
  Instruction* ZExt = new ZExtInst(Carry, Type::getInt32Ty(State->Context), "SBCrsiCarry", BB);
  Monitor::event_Instruction(ZExt);
  Instruction* SubCarry = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Type::getInt32Ty(State->Context), 1), ZExt, "SBCrsiSubCarry", BB);
  Monitor::event_Instruction(SubCarry);
  Instruction* CarryAdd = BinaryOperator::Create(Instruction::Add, ShiftInstr, SubCarry, "SBCrsiCarryAdd", BB);
  Monitor::event_Instruction(CarryAdd);

  Instruction* Result = BinaryOperator::Create(Instruction::Sub, RnVal, CarryAdd, "SBCrsiResult", BB);
  Monitor::event_Instruction(Result);

  State->setReg(Rd, Result, BB);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, CarryAdd, "SBCrsiNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, CarryAdd, "SUBSriZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallUSub = CallInst::Create(USub, {RnVal, CarryAdd}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSriCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    State->setStatus(ARMState::CPSR_C, C_Flag, BB);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallSSub = CallInst::Create(SSub, {RnVal, CarryAdd}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSriVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    State->setStatus(ARMState::CPSR_V, V_Flag, BB);
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSMMUL(ARMRaiserState* State, MachineInstr* MI) { // 1843 | SMMUL Rd Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* RnExt = new SExtInst(RnVal, Type::getInt64Ty(State->Context), "SMMULRnExt", BB);
  Monitor::event_Instruction(RnExt);
  Instruction* RmExt = new SExtInst(RmVal, Type::getInt64Ty(State->Context), "SMMULRmExt", BB);
  Monitor::event_Instruction(RmExt);

  Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnExt, RmExt, "SMMUL", BB);
  Monitor::event_Instruction(Mul);

  Instruction* Shr = BinaryOperator::Create(Instruction::LShr, Mul, ConstantInt::get(Type::getInt64Ty(State->Context), 32), "SMMULShr", BB);
  Monitor::event_Instruction(Shr);
  Instruction* Trunc = new TruncInst(Shr, Type::getInt32Ty(State->Context), "SMMULTrunc", BB);
  Monitor::event_Instruction(Trunc);

  State->setReg(Rd, Trunc, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSMULL(ARMRaiserState* State, MachineInstr* MI) { // 1849 | SMULL RdLo RdHi Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  assert(
    S == 0 &&
    "ARM::SMULL: assuming no S flag for now"
  );

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* RnExt = new SExtInst(RnVal, Type::getInt64Ty(State->Context), "SMULLRnExt", BB);
  Monitor::event_Instruction(RnExt);
  Instruction* RmExt = new SExtInst(RmVal, Type::getInt64Ty(State->Context), "SMULLRmExt", BB);
  Monitor::event_Instruction(RmExt);

  Instruction* Mul = BinaryOperator::Create(Instruction::Mul, RnExt, RmExt, "SMULL", BB);
  Monitor::event_Instruction(Mul);

  Instruction* Lo = new TruncInst(Mul, Type::getInt32Ty(State->Context), "SMULLLo", BB);
  Monitor::event_Instruction(Lo);
  Instruction* Hi = BinaryOperator::Create(Instruction::LShr, Mul, ConstantInt::get(Type::getInt64Ty(State->Context), 32), "SMULLHi", BB);
  Monitor::event_Instruction(Hi);
  Instruction* CastHi = new TruncInst(Hi, Type::getInt32Ty(State->Context), "SMULLHiCast", BB);
  Monitor::event_Instruction(CastHi);

  State->setReg(RdLo, Lo, BB);
  State->setReg(RdHi, CastHi, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSTMDB_UPD(ARMRaiserState* State, MachineInstr* MI) { // 1895 | STMDB Rt! {Rwb} CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(4).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Monitor::event_raw() << "STMDB_UPD: decrementing SP by 4\n";
    State->BBStateMap[BB]->QState->SP_offset -= 4;

    Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
    Value* Ptr = getOrCreateStackAlloca(State, Rt, 0, Type::getInt32Ty(State->Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
    Monitor::event_Instruction(Store);

    return true;
  }

  assert(Rt != ARM::PC && "STMDB_UPD: Rt == PC");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);

  Monitor::event_raw() << "STMDB_UPD: decrementing Rt by 4\n";
  Instruction* Add = BinaryOperator::Create(Instruction::Sub, RtVal, ConstantInt::get(Type::getInt32Ty(State->Context), 4), "STMDB_UPD", BB);
  Monitor::event_Instruction(Add);

  State->setReg(Rt, Add, BB);

  Instruction* Ptr = new IntToPtrInst(Add, Type::getInt32PtrTy(State->Context), "STMDB_UPD", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSTMIA(ARMRaiserState* State, MachineInstr* MI) { // 1896 | STMIA Rt CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(3).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
    Value* Ptr = getOrCreateStackAlloca(State, Rt, 0, Type::getInt32Ty(State->Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << State->BBStateMap[BB]->QState->SP_offset << "] = " << Ptr << "\n";

    Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
    Monitor::event_Instruction(Store);

    return true;
  }

  assert(Rt != ARM::PC && "STMIA: Rt == PC");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(State->Context), "STMIA", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSTMIB(ARMRaiserState* State, MachineInstr* MI) { // 1898 | STMIB Rt CC CPSR Rn
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(1).getImm();
  Register CPSR = MI->getOperand(2).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);
  Register Rn = MI->getOperand(3).getReg();

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rt == ARM::SP) {
    Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
    Value* Ptr = getOrCreateStackAlloca(State, Rt, 4, Type::getInt32Ty(State->Context), BB);
    Monitor::event_raw() << "Reg " << Rn << " ~ stack[" << (State->BBStateMap[BB]->QState->SP_offset+4) << "] = " << Ptr << "\n";

    Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
    Monitor::event_Instruction(Store);

    return true;
  }

  assert(Rt != ARM::PC && "STMIB: Rt == PC");

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);

  Instruction* Ptr = new IntToPtrInst(RtVal, Type::getInt32PtrTy(State->Context), "STMIB", BB);
  Monitor::event_Instruction(Ptr);

  Instruction* Store = new StoreInst(RnVal, Ptr, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSTRB_POST_IMM(ARMRaiserState* State, MachineInstr* MI) { // 1902 | STRB Rt Rn {Rwb} {Rm} AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t AM2Shift = MI->getOperand(4).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(5).getImm();
  Register CPSR = MI->getOperand(6).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(State, Rn, Rn, 0, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt8PtrTy(State->Context), "STRB_POST_IMMCast", BB);
  Monitor::event_Instruction(Cast);

  Value* Val = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Instruction* Trunc = new TruncInst(Val, Type::getInt8Ty(State->Context), "STRB_POST_IMMValTrunc", BB);
  Monitor::event_Instruction(Trunc);

  Instruction* Store = new StoreInst(Trunc, Cast, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSTRBi12(ARMRaiserState* State, MachineInstr* MI) { // 1906 | STRB Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Instruction* Trunc = new TruncInst(RtVal, Type::getInt8Ty(State->Context), "STRBi12_ValTrunc", BB);
  Monitor::event_Instruction(Trunc);
  RtVal = Trunc;

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) {
    Addr = getOrCreateStackAlloca(State, Rn, Imm12, Type::getInt8PtrTy(State->Context), BB);
  } else {
    Addr = State->getReg(Rn, Type::getInt8PtrTy(State->Context), BB);
    Value* Offset = ConstantInt::get(Type::getInt32Ty(State->Context), Imm12);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(State->Context), Addr, Offset, "STRBi12_GEP", BB);
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
/* static */ bool ARMRaiser::raiseSTREX(ARMRaiserState* State, MachineInstr* MI) { // 1911 | STREX Rd Rt Rn CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rd = MI->getOperand(0).getReg();
  Register Rt = MI->getOperand(1).getReg();
  Register Rn = MI->getOperand(2).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Ptr = State->getReg(Rt, Type::getInt32PtrTy(State->Context), BB);
  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);

  Function* STREX = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::arm_strex, {Type::getInt32PtrTy(State->Context)});
  Instruction* Call = CallInst::Create(STREX, { RnVal, Ptr }, "STREX", BB);
  Monitor::event_Instruction(Call);
  State->setReg(Rd, Call, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSTRH(ARMRaiserState* State, MachineInstr* MI) { // 1915 | STRH Rt Rn AM3Reg AM3Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register AM3Reg = MI->getOperand(2).getReg();
  int64_t AM3Imm = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes)MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Instruction* Trunc = new TruncInst(RtVal, Type::getInt16Ty(State->Context), "STRH_ValTrunc", BB);
  Monitor::event_Instruction(Trunc);
  RtVal = Trunc;

  Value* Addr;
  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) {
    assert(AM3Reg == 0 && "ARM::STRH: accessing stack with reg offet not supported yet");
    Addr = getOrCreateStackAlloca(State, Rn, AM3Imm, Type::getInt16PtrTy(State->Context), BB);
  } else {
    Addr = State->getReg(Rn, Type::getInt16PtrTy(State->Context), BB);
    Value* Offset;
    if (AM3Reg == 0)
      Offset = ConstantInt::get(Type::getInt32Ty(State->Context), AM3Imm);
    else
      Offset = State->getReg(AM3Reg, Type::getInt32Ty(State->Context), BB);
    Instruction* GEP = GetElementPtrInst::Create(Type::getInt16Ty(State->Context), Addr, Offset, "STRH_GEP", BB);
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
/* static */ bool ARMRaiser::raiseSTRi12(ARMRaiserState* State, MachineInstr* MI) { // 1926 | STR Rt Rn Imm12 CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  int64_t Imm12 = MI->getOperand(2).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
  Register CPSR = MI->getOperand(4).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Val = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Value* Ptr;

  if (Rn == ARM::SP || (Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP)) {
    Ptr = getOrCreateStackAlloca(State, Rn, Imm12, Type::getInt32Ty(State->Context), BB);
  } else {
    Ptr = State->getReg(Rn, Type::getInt32PtrTy(State->Context), BB);
    if (Imm12 != 0) {
      Instruction* GEP = GetElementPtrInst::Create(Type::getInt32Ty(State->Context), Ptr, ConstantInt::get(Type::getInt32Ty(State->Context), Imm12, false), "STRi12GEP", BB);
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
/* static */ bool ARMRaiser::raiseSTRrs(ARMRaiserState* State, MachineInstr* MI) { // 1927 | STR Rt Rn Rm AM2Shift CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rt = MI->getOperand(0).getReg();
  Register Rn = MI->getOperand(1).getReg();
  Register Rm = MI->getOperand(2).getReg();
  int64_t AM2Shift = MI->getOperand(3).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
  Register CPSR = MI->getOperand(5).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* Addr = resolveAM2Shift(State, Rn, 0, Rm, AM2Shift, BB);
  Instruction* Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(State->Context), "STRrsPtrCast", BB);
  Monitor::event_Instruction(Cast);
  Value* RtVal = State->getReg(Rt, Type::getInt32Ty(State->Context), BB);
  Instruction* Store = new StoreInst(RtVal, Cast, false, BB);
  Monitor::event_Instruction(Store);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSUBri(ARMRaiserState* State, MachineInstr* MI) { // 1928 | SUB Rd Rn Op2 CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  if (Rd == ARM::SP && Rn == ARM::SP) {
    Monitor::event_raw() << "decrementing SP by " << Imm << "\n";
    State->BBStateMap[BB]->QState->SP_offset -= Imm;
    return true;
  }

  if (Rd == ARM::SP && Rn == ARM::R11 && State->BBStateMap[BB]->QState->R11_is_FP) {
    Monitor::event_raw() << "decrementing SP based on FP by " << Imm << "\n";
    State->BBStateMap[BB]->QState->SP_offset = State->BBStateMap[BB]->QState->FP_offset - Imm;
    return true;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Instr = BinaryOperator::CreateSub(RnVal, ImmVal, "SUBri", BB);
  Monitor::event_Instruction(Instr);
  State->setReg(Rd, Instr, BB);

  if (S == ARM::CPSR) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "SUBSriNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "SUBSriZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSriCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    State->setStatus(ARMState::CPSR_C, C_Flag, BB);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSriVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    State->setStatus(ARMState::CPSR_V, V_Flag, BB);
  }

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSUBrr(ARMRaiserState* State, MachineInstr* MI) { // 1929 | SUB Rd Rn Rm CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Sub, RnVal, RmVal, "SUBrr", BB);
  Monitor::event_Instruction(Result);

  if (update_flags) {
    // Negative flag
    Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "SUBSNeg", BB);
    Monitor::event_Instruction(CmpNeg);
    State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

    // Zero flag
    Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "SUBSZero", BB);
    Monitor::event_Instruction(CmpZero);
    State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

    // Carry flag
    Function* USub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::usub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
    Monitor::event_Instruction(CallUSub);
    Instruction* C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSCFlag", BB);
    Monitor::event_Instruction(C_Flag);
    State->setStatus(ARMState::CPSR_C, C_Flag, BB);

    // Overflow flag
    Function* SSub = Intrinsic::getDeclaration(State->MR.getModule(), Intrinsic::ssub_with_overflow, Type::getInt32Ty(State->Context));
    Instruction* CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
    Monitor::event_Instruction(CallSSub);
    Instruction* V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSVFlag", BB);
    Monitor::event_Instruction(V_Flag);
    State->setStatus(ARMState::CPSR_V, V_Flag, BB);
  }

  State->setReg(Rd, Result, BB);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseSUBrsi(ARMRaiserState* State, MachineInstr* MI) { // 1930 | SUB Rd Rn Rm Shift CC CPSR S
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

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
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  int64_t ShiftAmount = Shift >> 3;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) (Shift & 0x7);

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
    case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
    case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
    default:
      assert(false && "SUBrsi: unknown shift opcode");
  }

  Instruction* ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(State->Context), ShiftAmount), "SUBrsiShift", BB);
  Monitor::event_Instruction(ShiftInstr);
  Instruction* Instr = BinaryOperator::Create(Instruction::Sub, RnVal, ShiftInstr, "SUBrsi", BB);
  Monitor::event_Instruction(Instr);
  State->setReg(Rd, Instr, BB);

  assert(!update_flags);

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseTEQrr(ARMRaiserState* State, MachineInstr* MI) { // 1942 | TEQ Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::Xor, RnVal, RmVal, "TEQrr", BB);
  Monitor::event_Instruction(Result);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "TEQrrNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "TEQrrZero", BB);
  Monitor::event_Instruction(CmpZero);
  State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

  // Carry flag update cant occur for constant Op2
  // Overflow flag is not updated

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseTSTri(ARMRaiserState* State, MachineInstr* MI) { // 1948 | TST Rn Imm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  int64_t Imm = MI->getOperand(1).getImm();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* ImmVal = ConstantInt::get(Type::getInt32Ty(State->Context), Imm);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, ImmVal, "TSTri", BB);
  Monitor::event_Instruction(Result);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "TSTriNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "TSTriZero", BB);
  Monitor::event_Instruction(CmpZero);
  State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

  // Carry flag update cant occur for constant Op2
  // Overflow flag is not updated

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
/* static */ bool ARMRaiser::raiseTSTrr(ARMRaiserState* State, MachineInstr* MI) { // 1949 | TST Rn Rm CC CPSR
  MachineBasicBlock* MBB = MI->getParent();
  BasicBlock* BB = State->getBasicBlocks(MBB).back();

  Register Rn = MI->getOperand(0).getReg();
  Register Rm = MI->getOperand(1).getReg();
  ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
  Register CPSR = MI->getOperand(3).getReg();
  bool conditional_execution = (CC != ARMCC::AL) && (CPSR != 0);

  BasicBlock* MergeBB;
  if (conditional_execution) {
    Value* Cond = ARMCCToValue(State, CC, BB);
    BasicBlock* CondExecBB = State->createBasicBlock(MBB);
    MergeBB = State->createBasicBlock(MBB);
    Instruction* CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
    Monitor::event_Instruction(CondBranch);
    BB = CondExecBB;
  }

  Value* RnVal = State->getReg(Rn, Type::getInt32Ty(State->Context), BB);
  Value* RmVal = State->getReg(Rm, Type::getInt32Ty(State->Context), BB);

  Instruction* Result = BinaryOperator::Create(Instruction::And, RnVal, RmVal, "TSTrr", BB);
  Monitor::event_Instruction(Result);

  // Negative flag
  Instruction* CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "TSTrrNeg", BB);
  Monitor::event_Instruction(CmpNeg);
  State->setStatus(ARMState::CPSR_N, CmpNeg, BB);

  // Zero flag
  Instruction* CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, Result, ConstantInt::get(Type::getInt32Ty(State->Context), 0), "TSTrrZero", BB);
  Monitor::event_Instruction(CmpZero);
  State->setStatus(ARMState::CPSR_Z, CmpZero, BB);

  // Carry flag update cant occur for constant Op2
  // Overflow flag is not updated

  if (conditional_execution) {
    Instruction* Branch = BranchInst::Create(MergeBB, BB);
    Monitor::event_Instruction(Branch);
  }

  return true;
}
