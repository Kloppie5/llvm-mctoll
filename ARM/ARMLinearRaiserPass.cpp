
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

bool ARMLinearRaiserPass::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMLinearRaiserPass");
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass start.\n");

  this->MF = MF;
  this->F = F;

  stack_offset = 0;

  // Set up entry block.
  BasicBlock *EBB = &F->getEntryBlock();
  MachineBasicBlock *MBB = &MF->front();
  MBBBBMap[MBB].push_back(EBB);
  EBB->setName("entry");

  // Allocate NZCV flags.
  Type *Ty = Type::getInt1Ty(Context);
  Monitor::event_raw() << "Allocate NZCV flags for " << F->getName() << "\n";
  Align MALG(32);
  
  Instruction *AllocN = new AllocaInst(Ty, 0, nullptr, MALG, "", EBB);
  AllocN->setName("N_flag");
  Monitor::event_Instruction(AllocN);
  Flags.push_back(AllocN);
  
  Instruction *AllocZ = new AllocaInst(Ty, 0, nullptr, MALG, "", EBB);
  AllocZ->setName("Z_flag");
  Monitor::event_Instruction(AllocZ);
  Flags.push_back(AllocZ);
  
  Instruction *AllocC = new AllocaInst(Ty, 0, nullptr, MALG, "", EBB);
  AllocC->setName("C_flag");
  Monitor::event_Instruction(AllocC);
  Flags.push_back(AllocC);

  Instruction *AllocV = new AllocaInst(Ty, 0, nullptr, MALG, "", EBB);
  AllocV->setName("V_flag");
  Monitor::event_Instruction(AllocV);
  Flags.push_back(AllocV);

  // Add arguments to RegValueMap
  for (unsigned i = 0; i < 4 && i < F->arg_size(); ++i) {
    Monitor::event_raw() << "Add argument " << (F->arg_begin() + i)->getName() << " to RegValueMap reg " << (ARM::R0 + i) << "\n";
    RegValueMap[ARM::R0 + i] = F->arg_begin() + i;
  }
  
  // Add excess arguments to StackArgMap
  for (unsigned i = 4; i < F->arg_size(); ++i) {
    Monitor::event_raw() << "Add argument " << (F->arg_begin() + i)->getName() << " to StackArgMap at offset " << ((i-4)*4) << "\n";
    stack_map[(i-4)*4] = F->arg_begin() + i;
  }

  std::vector<MachineInstr *> Worklist;
  for (MachineBasicBlock &MBB : *MF)
    for (MachineInstr &MI : MBB)
      Worklist.push_back(&MI);

  Monitor::event_raw() << "LinearRaiser: " << Worklist.size() << " instructions to process.\n";
  for (MachineInstr *MI : Worklist)
    raiseMachineInstr(MI);

  LLVM_DEBUG(MF->dump());
  LLVM_DEBUG(F->dump());
  LLVM_DEBUG(dbgs() << "ARMLinearRaiserPass end.\n");
  Monitor::event_end("ARMLinearRaiserPass");
  return true;
}

Value *ARMLinearRaiserPass::getRegValue(Register Reg, BasicBlock *BB) {
  assert(Reg != ARM::SP && Reg != ARM::R11 && "getRegValue: stack registers are not allowed");
  assert(Reg != ARM::PC && "getRegValue: PC is not allowed");
  if (RegValueMap.find(Reg) == RegValueMap.end())
    assert(false && "Register not found in RegValueMap");
  
  Value *V = RegValueMap[Reg];
  if (V->getType()->isPointerTy()) {
    Instruction *Cast = new PtrToIntInst(V, Type::getInt32Ty(Context), "AM2Shift", BB);
    Monitor::event_Instruction(Cast);
    V = Cast;
  }
  return V;
}
void ARMLinearRaiserPass::setRegValue(Register Reg, Value *V, BasicBlock *BB) {
  assert(Reg != ARM::SP && Reg != ARM::R11 && "getRegValue: stack registers are not allowed");
  assert(Reg != ARM::PC && "getRegValue: PC is not allowed");
  assert((V->getType() == Type::getInt32Ty(Context) || V->getType()->isPointerTy())
         && "Value must be of type i32 or x*");
  RegValueMap[Reg] = V;
}

Value *ARMLinearRaiserPass::getStackValue(int64_t offset, BasicBlock *BB) {
  return nullptr;
}
void ARMLinearRaiserPass::setStackValue(int64_t offset, Value *V, BasicBlock *BB) {

}

Value *ARMLinearRaiserPass::resolveAM2Shift(Register Rn, Register Rm, int64_t AM2Shift, BasicBlock *BB) {
  unsigned Imm12 = AM2Shift & 0xFFF;
  ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) ((AM2Shift >> 13) & 0x7);
  bool isSub = (AM2Shift >> 12) & 0x1;
  unsigned IdxMode = AM2Shift >> 16;
  assert(IdxMode == 0 && "AM2Shift with index mode not supported");

  Instruction::BinaryOps ShiftOp;
  switch (ShiftOpcode) {
    case ARM_AM::lsr:
      ShiftOp = Instruction::LShr;
      break;
    default:
      assert(false && "assuming default shift for now");
  }

  Value *RnVal = getRegValue(Rn, BB);
  Value *RmVal = getRegValue(Rm, BB);

  Instruction *Shift = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Type::getInt32Ty(Context), Imm12), "AM2Shift", BB);
  Monitor::event_Instruction(Shift);
  Instruction *Add = BinaryOperator::Create(isSub ? Instruction::Sub : Instruction::Add, RnVal, Shift, "AM2Shift", BB);
  Monitor::event_Instruction(Add);

  return Add;
}


// TODO: merge with X86MachineInstructionRaiser::createPCRelativeAccesssValue
GlobalValue *ARMLinearRaiserPass::getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset) {
  ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser&>(MR);
  GlobalValue *GlobVal = nullptr;
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
      assert (SymOrErr && "Can not find the symbol!");

      uint64_t SymVirtAddr = *SymOrErr;
      auto SecOrErr = Symbol->getSection();
      assert(SecOrErr && "Can not find the section which is the symbol in!");

      section_iterator SecIter = *SecOrErr;
      Constant *GlobInit = nullptr;
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
        Monitor::event_raw() << "Symbol with default value " << InitVal << "\n";
        GlobInit = ConstantInt::get(GlobValTy, InitVal);
      }

      GlobVal = new GlobalVariable(*MR.getModule(), GlobValTy, false /* isConstant */,
                                        Linkage, GlobInit, SymName);
    }
  } else {
    Monitor::event_raw() << "Failed to find symbol associated with dynamic relocation, reading as ROData.\n";
    // If can not find the corresponding symbol.
    const Value *ROVal = AMR.getRODataValueAt(Offset);
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

  return GlobVal;
}

// TODO: Move CC to separate pass.
Value *ARMLinearRaiserPass::ARMCCToValue(int Cond, BasicBlock *BB) {
  // Why do ICmpInst constructors put &InsertBefore/&InsertAtEnd as the first
  // operand instead of *InsertBefore/*InsertAtEnd as the last one? Who knows.
  switch (Cond) {
    default:
      llvm_unreachable("Unknown condition code!");
    case ARMCC::EQ: { // Z = 1
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS"); Monitor::event_Instruction(ZS);
      return ZS;
    } break;
    case ARMCC::NE: { // Z = 0
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC"); Monitor::event_Instruction(ZC);
      return ZC;
    } break;
    case ARMCC::HS: { // C = 1
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS"); Monitor::event_Instruction(CS);
      return CS;
    } break;
    case ARMCC::LO: { // C = 0
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC"); Monitor::event_Instruction(CC);
      return CC;
    } break;
    case ARMCC::MI: { // N = 1
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *NS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getTrue(Context), "NS"); Monitor::event_Instruction(NS);
      return NS;
    } break;
    case ARMCC::PL: { // N = 0
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *NC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, ConstantInt::getFalse(Context), "NC"); Monitor::event_Instruction(NC);
      return NC;
    } break;
    case ARMCC::VS: { // V = 1
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *VS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getTrue(Context), "VS"); Monitor::event_Instruction(VS);
      return VS;
    } break;
    case ARMCC::VC: { // V = 0
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *VC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, V, ConstantInt::getFalse(Context), "VC"); Monitor::event_Instruction(VC);
      return VC;
    } break;
    case ARMCC::HI: { // C = 1 && Z = 0
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getTrue(Context), "CS"); Monitor::event_Instruction(CS);
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC"); Monitor::event_Instruction(ZC);
      Instruction *HI = BinaryOperator::Create(Instruction::Add, CS, ZC, "HI", BB); Monitor::event_Instruction(HI);
      return HI;
    } break;
    case ARMCC::LS: { // C = 0 || Z = 1
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[2], "C", BB); Monitor::event_Instruction(C);
      Instruction *CC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, C, ConstantInt::getFalse(Context), "CC"); Monitor::event_Instruction(CC);
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS"); Monitor::event_Instruction(ZS);
      Instruction *LS = BinaryOperator::Create(Instruction::Or, CC, ZS, "LS", BB); Monitor::event_Instruction(LS);
      return LS;
    } break;
    case ARMCC::GE: { // N = V
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *GE = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "GE"); Monitor::event_Instruction(GE);
      return GE;
    } break;
    case ARMCC::LT: { // N != V
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *LT = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "LT"); Monitor::event_Instruction(LT);
      return LT;
    } break;
    case ARMCC::GT: { // Z = 0 && N = V
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZC = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getFalse(Context), "ZC"); Monitor::event_Instruction(ZC);
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *Sign = new ICmpInst(*BB, ICmpInst::ICMP_EQ, N, V, "S"); Monitor::event_Instruction(Sign);
      Instruction *GT = BinaryOperator::Create(Instruction::And, ZC, Sign, "GT", BB); Monitor::event_Instruction(GT);
      return GT;
    } break;
    case ARMCC::LE: { // Z = 1 || N != V
      Instruction *Z = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Z", BB); Monitor::event_Instruction(Z);
      Instruction *ZS = new ICmpInst(*BB, ICmpInst::ICMP_EQ, Z, ConstantInt::getTrue(Context), "ZS"); Monitor::event_Instruction(ZS);
      Instruction *N = new LoadInst(Type::getInt1Ty(Context), Flags[0], "N", BB); Monitor::event_Instruction(N);
      Instruction *V = new LoadInst(Type::getInt1Ty(Context), Flags[3], "V", BB); Monitor::event_Instruction(V);
      Instruction *Sign = new ICmpInst(*BB, ICmpInst::ICMP_NE, N, V, "S"); Monitor::event_Instruction(Sign);
      Instruction *LE = BinaryOperator::Create(Instruction::Or, ZS, Sign, "LE", BB); Monitor::event_Instruction(LE);
      return LE;
    } break;
    case ARMCC::AL: { // always
      return ConstantInt::getTrue(Context);
    } break;
  }
}

std::vector<BasicBlock *> ARMLinearRaiserPass::getBasicBlocks(MachineBasicBlock *MBB) {
  auto I = MBBBBMap.find(MBB);
  if (I != MBBBBMap.end())
    return I->second;
  
  BasicBlock *BB = BasicBlock::Create(Context, "bb." + Twine(MBB->getNumber()), F);
  Monitor::event_raw() << "Created BB: " << BB->getName() << " in function " << F->getName() << "\n";
  MBBBBMap[MBB].push_back(BB);
  return MBBBBMap[MBB];
}

bool ARMLinearRaiserPass::raiseMachineInstr(MachineInstr *MI) {
  Monitor::event_start("ARMLinearRaiserPass::RaiseMachineInstr");
  Monitor::event_MachineInstr(MI);
  Monitor::event_stateswitch();

  MachineBasicBlock *MBB = MI->getParent();
  BasicBlock *BB = getBasicBlocks(MBB).back();

  switch (MI->getOpcode()) {
    default: {
      auto OS = WithColor(errs(), HighlightColor::Warning);
      OS << "ARMLinearRaiserPass encountered unhandled opcode in instruction ";
      Monitor::printMachineInstr(MI, true, OS);
      assert(false && "Unhandled opcode");
      return false;
    } break;
    /*
    case ARM::ASRi: { // 247 | ASR{S}<c> <Rd>, <Rn>, #<imm> => Rd = AShr(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::AShr, Rn, imm, "ASRi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ASRr: { // 248 | ASR{S}<c> <Rd>, <Rn>, <Rm> => Rd = AShr(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::AShr, Rn, Rm, "ASRr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSLi: { // 292 | LSL{S}<c> <Rd>, <Rn>, #<imm> => Rd = Shl(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::Shl, Rn, imm, "LSLi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSLr: { // 293 | LSL{S}<c> <Rd>, <Rn>, <Rm> => Rd = Shl(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Shl, Rn, Rm, "LSLr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSRi: { // 294 | LSR{S}<c> <Rd>, <Rn>, #<imm> => Rd = LShr(<Rn>, imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::LShr, Rn, imm, "LSRi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::LSRr: { // 295 | LSR{S}<c> <Rd>, <Rn>, <Rm> => Rd = LShr(<Rn>, <Rm>)
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::LShr, Rn, Rm, "LSRr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RORi: { // 325 | ROR{S}<c> <Rd>, <Rn>, #<imm> => Rd = OR(Shl(<Rn>, imm), LShr(<Rn>, Sub(32, imm)))
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Shl = BinaryOperator::Create(Instruction::Shl, Rn, imm, "RORiShl", BB); Monitor::event_Instruction(Shl);
      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Rn->getType(), 32), imm, "RORiSub", BB); Monitor::event_Instruction(Sub);
      Instruction *LShr = BinaryOperator::Create(Instruction::LShr, Rn, Sub, "RORiLShr", BB); Monitor::event_Instruction(LShr);
      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Shl, LShr, "RORiOR", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RORr: { // 326 | ROR{S}<c> <Rd>, <Rn>, <Rm> => Rd = OR(Shl(<Rn>, <Rm>), LShr(<Rn>, Sub(32, <Rm>)))
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Shl = BinaryOperator::Create(Instruction::Shl, Rn, Rm, "RORrShl", BB); Monitor::event_Instruction(Shl);
      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, ConstantInt::get(Rn->getType(), 32), Rm, "RORrSub", BB); Monitor::event_Instruction(Sub);
      Instruction *LShr = BinaryOperator::Create(Instruction::LShr, Rn, Sub, "RORrLShr", BB); Monitor::event_Instruction(LShr);
      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Shl, LShr, "RORrOR", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::RRX: { // 327 | RRX{S}<c> <Rd>, <Rm> => ?
      // Rotate Right with Extend
      Value *Rm = getOperandValue(MI, 1);
      Instruction *Shr = BinaryOperator::Create(Instruction::LShr, Rm, ConstantInt::get(Rm->getType(), 1), "RRXShr", BB); Monitor::event_Instruction(Shr);
      Instruction *C = new LoadInst(Type::getInt1Ty(Context), Flags[1], "Carry", BB); Monitor::event_Instruction(C);
      Instruction *ShlC = BinaryOperator::Create(Instruction::Shl, Shr, ConstantInt::get(Shr->getType(), 31), "RRXCShl", BB); Monitor::event_Instruction(ShlC);
      Instruction *Or = BinaryOperator::Create(Instruction::Or, Shr, ShlC, "RRXOr", BB); Monitor::event_Instruction(Or);
      setOperandValue(MI, 0, Or);
    } break;
    case ARM::ADCri: { // 680 | ADC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn + Imm
      assert(false && "ADCri not yet implemented; requires Carry flag");
    } break;
    */
    case ARM::ADDri: { // 684 | ADD Rd Rn Op2 CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *PtrTy32 = Type::getInt32PtrTy(Context);

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

      if (Rn == ARM::SP) {
        auto I = stack_map.find(stack_offset + Imm);
        if (I == stack_map.end()) {
          Monitor::event_raw() << "stack_map[" << stack_offset + Imm << "] is null, creating new Value\n";
          Instruction *Alloca = new AllocaInst(Ty32, 0, "", BB);
          Monitor::event_Instruction(Alloca);
          stack_map[stack_offset + Imm] = Alloca;
        }
        Monitor::event_raw() << "Reg " << Rd << " = stack[" << (stack_offset + Imm) << "]\n";
        RegValueMap[Rd] = stack_map[stack_offset + Imm];
        break;
      }

      Value *Val = RegValueMap[Rn];
      if (Imm != 0) {
        Instruction *Add = BinaryOperator::Create(Instruction::Add, Val, ConstantInt::get(Ty32, Imm), "Add", BB);
        Monitor::event_Instruction(Add);
        Val = Add;
      }
      RegValueMap[Rd] = Val;
    } break;
    case ARM::ADDrr: { // 685 | ADD Rd Rn Rm CC CPSR S      
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ADDrr: assuming no flags for now"
      );

      Value *RnVal = RegValueMap[Rn];
      Value *RmVal = RegValueMap[Rm];

      Instruction *Instr = BinaryOperator::Create(Instruction::Add, RnVal, RmVal, "ADDrr", BB);
      Monitor::event_Instruction(Instr);
      RegValueMap[Rd] = Instr;
    } break;
    case ARM::ADDrsi: { // 686 | ADD Rd Rn Rm Shift CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);

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

      Value *RnVal = RegValueMap[Rn];
      if (RnVal->getType()->isPointerTy()) {
        Instruction *Cast = new PtrToIntInst(RnVal, Ty32, "Cast", BB);
        Monitor::event_Instruction(Cast);
        RnVal = Cast;
      }
      Value *RmVal = RegValueMap[Rm];

      if (RmVal->getType()->isPointerTy()) {
        Instruction *Cast = new PtrToIntInst(RmVal, Ty32, "Cast", BB);
        Monitor::event_Instruction(Cast);
        RmVal = Cast;
      }

      Instruction::BinaryOps ShiftOp;
      switch (ShiftOpcode) {
        case ARM_AM::lsr:
          ShiftOp = Instruction::LShr;
          break;
        default:
          assert(false && "ADDrsi: unknown shift opcode");
      }

      Instruction *ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Ty32, ShiftAmount), "ADDrsiShift", BB);
      Monitor::event_Instruction(ShiftInstr);
      Instruction *Instr = BinaryOperator::Create(Instruction::Add, RnVal, ShiftInstr, "ADDrsi", BB);
      Monitor::event_Instruction(Instr);
      RegValueMap[Rd] = Instr;
    } break;
    /*
    case ARM::ANDri: { // 693 | AND{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & Imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      
      Instruction *Instr = BinaryOperator::Create(Instruction::And, Rn, imm, "ANDri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::BICri: { // 706 | BIC{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn & (0 - Imm)
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());
      Value *zero = ConstantInt::get(Rn->getType(), 0);

      Instruction *Sub = BinaryOperator::Create(Instruction::Sub, zero, imm, "BICriSub", BB); Monitor::event_Instruction(Sub);
      Instruction *Instr = BinaryOperator::Create(Instruction::And, Rn, Sub, "BICriAnd", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    */
    case ARM::BL: { // 711 | BL Imm
      ARMModuleRaiser &AMR = static_cast<ARMModuleRaiser &>(MR);

      Type *Ty32 = Type::getInt32Ty(Context);

      int64_t Imm = MI->getOperand(0).getImm();
      int64_t offset = MCIR->getMCInstIndex(*MI);
      uint64_t target = AMR.getTextSectionAddress() + offset + Imm + 8;
      Monitor::event_raw() << "address = " << target << "\n";

      Function *CalledFunc = AMR.getRaisedFunctionAt(target);
      Monitor::event_raw() << "Direct call target: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
      
      if (!CalledFunc) {
        CalledFunc = AMR.getCalledFunctionUsingTextReloc(offset, 4);
        Monitor::event_raw() << "Call target using text reloc: " << (CalledFunc ? CalledFunc->getName() : "null") << "\n";
      }
      
      if (!CalledFunc) {
        // Get CalledFunc using PLT
        const ELF32LEObjectFile *Elf32LEObjFile = dyn_cast<ELF32LEObjectFile>(AMR.getObjectFile());
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
          StringRef SecName = *NameOrErr;
          if (SecName.compare(".plt") != 0)
            assert(false && "Unexpected section name of PLT offset");

          auto StrOrErr = SecIter->getContents();
          assert(StrOrErr && "Failed to get the content of section!");
          auto SecData = *StrOrErr;
          ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t *>(SecData.data()), SecData.size());

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

          const RelocationRef *GotPltReloc =
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
          Value *Arg = CalledFunc->arg_begin() + i;
          if (i > 0) OS << ", ";
          OS << Arg->getName() << " ";
          Arg->getType()->print(OS);
        }
        OS << ")\n";
      }

      std::vector<Value *> ArgVals;
      for (unsigned i = 0; i < ArgCount; ++i) {
        Value *ArgVal = nullptr;
        Type *Ty = CalledFunc->getFunctionType()->getParamType(i);
        if (i < 4) ArgVal = RegValueMap[ARM::R0 + i];
        else {
          auto I = stack_map.find((i - 4) * 4);
          assert(I != stack_map.end() && "Failed to find stack value");
          ArgVal = I->second;
        };

        if (ArgVal->getType() != Ty && i < CalledFunc->arg_size()) { // Skip variadic args
          // Handle special string value to pointer case, should obviously be moved
          Instruction *Cast = CastInst::Create(CastInst::getCastOpcode(ArgVal, false, Ty, false), ArgVal, Ty, "", BB);
          Monitor::event_Instruction(Cast);
          ArgVal = Cast;
        }
        ArgVals.push_back(ArgVal);
      }

      Instruction *Instr = CallInst::Create(CalledFunc, ArgVals, "", BB);
      Monitor::event_Instruction(Instr);
      if (CalledFunc->getReturnType()->isVoidTy()) {
        // RegValueMap[ARM::R0] = ConstantInt::get(Type::getInt32Ty(BB->getContext()), 0);
      } else {
        RegValueMap[ARM::R0] = Instr;
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

      Type *RetTy = F->getReturnType();

      if (RetTy->isVoidTy()) {
        Instruction *Instr = ReturnInst::Create(Context, BB);
        Monitor::event_Instruction(Instr);
      } else {
        Instruction *Instr = ReturnInst::Create(Context, RegValueMap[ARM::R0], BB);
        Monitor::event_Instruction(Instr);
      }
    } break;
    case ARM::Bcc: { // 720 | Bcc offset CC CPSR
      // int64_t offset = MI->getOperand(0).getImm();
      int64_t CC = MI->getOperand(1).getImm();
      Register CPSR = MI->getOperand(2).getReg();
    
      if (CC == ARMCC::AL) {
        assert(
          MBB->succ_size() == 1 &&
          "Bcc: assuming normal unconditional branch for now"
        );
        auto succ_itt = MBB->succ_begin();
        BasicBlock *BranchBB = getBasicBlocks(*succ_itt).front();
        Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
        Instruction *Instr = BranchInst::Create(BranchBB, BB);
        Monitor::event_Instruction(Instr);
      } else {
        assert(
          MBB->succ_size() == 2 &&
          "Bcc: assuming normal conditional branch for now"
        );

        Value *CCVal = ARMCCToValue(CC, BB);
        
        auto succ_itt = MBB->succ_begin();
        BasicBlock *BranchBB = getBasicBlocks(*succ_itt).front();
        Monitor::event_raw() << "Branch BB: " << BranchBB->getName() << "\n";
        BasicBlock *NextBB = getBasicBlocks(*++succ_itt).front();
        Monitor::event_raw() << "Next BB: " << NextBB->getName() << "\n";          
        Instruction *Branch = BranchInst::Create(BranchBB, NextBB, CCVal, BB);
        Monitor::event_Instruction(Branch);
      }
    } break;
    case ARM::CMNri: { // 755 | CMN Rn Op2 CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);
      
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

      Value *RnVal = RegValueMap[Rn];
      Value *ImmVal = ConstantInt::get(Ty32, Imm);
      Value *NImmVal = ConstantInt::get(Ty32, -Imm);
      // Not entirely correct, but negative zero compares shouldnt be emitted by compilers anyways

      // Negative flag
      Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, NImmVal, "CMNriNeg", BB);
      Monitor::event_Instruction(CmpNeg);
      Instruction *StoreNeg = new StoreInst(CmpNeg, Flags[0], BB);
      Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, NImmVal, "CMNriZero", BB);
      Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, Flags[1], BB);
      Monitor::event_Instruction(StoreZero);

      // Carry flag
      Function *UAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::uadd_with_overflow, Ty32);
      Instruction *CallUAdd = CallInst::Create(UAdd, {RnVal, ImmVal}, "UAddInstrinsic", BB);
      Monitor::event_Instruction(CallUAdd);
      Instruction *C_Flag = ExtractValueInst::Create(CallUAdd, 1, "CMNCFlag", BB);
      Monitor::event_Instruction(C_Flag);
      Instruction *StoreCarry = new StoreInst(C_Flag, Flags[2], BB);
      Monitor::event_Instruction(StoreCarry);

      // Overflow flag
      Function *SAdd = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::sadd_with_overflow, Ty32);
      Instruction *CallSAdd = CallInst::Create(SAdd, {RnVal, ImmVal}, "SAddIntrinsic", BB);
      Monitor::event_Instruction(CallSAdd);
      Instruction *V_Flag = ExtractValueInst::Create(CallSAdd, 1, "CMNVFlag", BB);
      Monitor::event_Instruction(V_Flag);
      Instruction *StoreOverflow = new StoreInst(V_Flag, Flags[3], BB);
      Monitor::event_Instruction(StoreOverflow);
    } break;
    case ARM::CMPri: { // 759 | CMP Rn Op2 CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);
      
      Register Rn = MI->getOperand(0).getReg();
      int64_t Op2 = MI->getOperand(1).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      bool conditional_execution = (CC != ARMCC::AL) && (CC != 0);
      BasicBlock *MergeBB;
      if (conditional_execution) {
        // Split on condition
        Monitor::event_raw() << "splitting on condition\n";
        Value *Cond = ARMCCToValue(ARMCC::getOppositeCondition(CC), BB);
        BasicBlock *CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Instruction *CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        Instruction* MergeElse = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeElse);
        BB = CondExecBB;
      }

      Value *RnVal = RegValueMap[Rn];
      if (RnVal->getType()->isPointerTy()) {
        Instruction *Cast = new PtrToIntInst(RnVal, Ty32, "CMPriCast", BB);
        Monitor::event_Instruction(Cast);
        RnVal = Cast;
      }
      Value *ImmVal = ConstantInt::get(Ty32, Imm);

      // Negative flag
      Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "CMPriNeg", BB);
      Monitor::event_Instruction(CmpNeg);
      Instruction *StoreNeg = new StoreInst(CmpNeg, Flags[0], BB);
      Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "CMPriZero", BB);
      Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, Flags[1], BB);
      Monitor::event_Instruction(StoreZero);

      // Carry flag
      //Instruction *CmpCarry = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SGT, Rn, imm, "CMPriCarry", BB); Monitor::event_Instruction(CmpCarry);
      Function *USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Ty32);
      Instruction *CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
      Monitor::event_Instruction(CallUSub);
      Instruction *C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPriCFlag", BB);
      Monitor::event_Instruction(C_Flag);
      Instruction *StoreCarry = new StoreInst(C_Flag, Flags[2], BB);
      Monitor::event_Instruction(StoreCarry);

      // Overflow flag
      //Instruction *CmpOverflow = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_UGT, Rn, imm, "CMPriOverflow", BB); Monitor::event_Instruction(CmpOverflow);
      Function *SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Ty32);
      Instruction *CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
      Monitor::event_Instruction(CallSSub);
      Instruction *V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPriVFlag", BB);
      Monitor::event_Instruction(V_Flag);
      Instruction *StoreOverflow = new StoreInst(V_Flag, Flags[3], BB);
      Monitor::event_Instruction(StoreOverflow);

      if (conditional_execution) {      
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
      }
    } break;
    case ARM::CMPrr: { // 760 | CMP Rn Rm CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);
      
      Register Rn = MI->getOperand(0).getReg();
      Register Rm = MI->getOperand(1).getReg();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      bool conditional_execution = (CC != ARMCC::AL) && (CC != 0);
      BasicBlock *MergeBB;
      if (conditional_execution) {
        // Split on condition
        Monitor::event_raw() << "splitting on condition\n";
        Value *Cond = ARMCCToValue(ARMCC::getOppositeCondition(CC), BB);
        BasicBlock *CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Instruction *CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        Instruction* MergeElse = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeElse);
        BB = CondExecBB;
      }

      Value *RnVal = RegValueMap[Rn];
      if (RnVal->getType()->isPointerTy()) {
        Instruction *Cast = new PtrToIntInst(RnVal, Ty32, "CMPrrCast", BB);
        Monitor::event_Instruction(Cast);
        RnVal = Cast;
      }
      Value *RmVal = RegValueMap[Rm];
      if (RmVal->getType()->isPointerTy()) {
        Instruction *Cast = new PtrToIntInst(RmVal, Ty32, "CMPrrCast", BB);
        Monitor::event_Instruction(Cast);
        RmVal = Cast;
      }

      // Negative flag
      Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, RmVal, "CMPrrNeg", BB);
      Monitor::event_Instruction(CmpNeg);
      Instruction *StoreNeg = new StoreInst(CmpNeg, Flags[0], BB);
      Monitor::event_Instruction(StoreNeg);
      
      // Zero flag
      Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, RmVal, "CMPrrZero", BB);
      Monitor::event_Instruction(CmpZero);
      Instruction *StoreZero = new StoreInst(CmpZero, Flags[1], BB);
      Monitor::event_Instruction(StoreZero);

      // Carry flag
      Function *USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Ty32);
      Instruction *CallUSub = CallInst::Create(USub, {RnVal, RmVal}, "USubInstrinsic", BB);
      Monitor::event_Instruction(CallUSub);
      Instruction *C_Flag = ExtractValueInst::Create(CallUSub, 1, "CMPrrCFlag", BB);
      Monitor::event_Instruction(C_Flag);
      Instruction *StoreCarry = new StoreInst(C_Flag, Flags[2], BB);
      Monitor::event_Instruction(StoreCarry);

      // Overflow flag
      Function *SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Ty32);
      Instruction *CallSSub = CallInst::Create(SSub, {RnVal, RmVal}, "SSubIntrinsic", BB);
      Monitor::event_Instruction(CallSSub);
      Instruction *V_Flag = ExtractValueInst::Create(CallSSub, 1, "CMPrrVFlag", BB);
      Monitor::event_Instruction(V_Flag);
      Instruction *StoreOverflow = new StoreInst(V_Flag, Flags[3], BB);
      Monitor::event_Instruction(StoreOverflow);

      if (conditional_execution) {      
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
      }
    } break;
    case ARM::DMB: { // 773 | DMB Imm
      // Call ARM DMB intrinsic
      Type *Ty32 = Type::getInt32Ty(Context);

      int64_t Imm = MI->getOperand(0).getImm();

      Function *DMB = Intrinsic::getDeclaration(MR.getModule(),Intrinsic::arm_dmb);
      Instruction *Instr = CallInst::Create(DMB, { ConstantInt::get(Ty32, Imm) }, "", BB);
      Monitor::event_Instruction(Instr);
    } break;
    /*
    case ARM::DSB: { // 774 | DSB <option> => UNDEF
      assert(false && "ARM::DSB not yet implemented; fence");
    }
    case ARM::EORri: { // 775 | EOR{S}<c> <Rd>, <Rn>, #<imm> => Rd = Rn ^ Imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = ConstantInt::get(Rn->getType(), MI->getOperand(2).getImm());

      Instruction *Instr = BinaryOperator::Create(Instruction::Xor, Rn, imm, "EORri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::EORrr: { // 776 | EOR{S}<c> <Rd>, <Rn>, <Rm> => Rd = Rn ^ Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Xor, Rn, Rm, "EORrr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ISB: { // 793 | ISB <option> => UNDEF
      assert(false && "ARM::ISB not yet implemented; fence");
    } break;
    case ARM::LDMIA: { // 821 | LDMIA <Rn>{!}, <registers> => Rn = Rn + 4 * registers
      assert(false && "ARM::LDMIA not yet implemented");
    } break;
    */
    case ARM::LDMIA_UPD: { // 822 | LDMIA Rn Rn CC CPSR Reg
      Type *Ty32 = Type::getInt32Ty(Context);

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

      Monitor::event_raw() << "Reg " << Reg << " = stack[" << stack_offset << "]\n";
      RegValueMap[Reg] = stack_map[stack_offset];

      // Increment SP
      Monitor::event_raw() << "incrementing SP by 4\n";
      stack_offset += 4;
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

      Value *RnVal = getRegValue(Rn, BB);
      Value *RmVal = getRegValue(Rm, BB);

      Instruction *Add = BinaryOperator::Create(Instruction::Add, RnVal, RmVal, "LDRB_PRE_REGAdd", BB);
      Monitor::event_Instruction(Add);
      setRegValue(Rs, Add, BB);

      Instruction *Cast = new IntToPtrInst(Add, Type::getInt8PtrTy(Context), "LDRB_PRE_REGCast", BB);
      Monitor::event_Instruction(Cast);

      Instruction *Load = new LoadInst(Type::getInt8Ty(Context), Cast, "LDRB_PRE_REGLoad", BB);
      Monitor::event_Instruction(Load);

      Instruction *ZExt = new ZExtInst(Load, Type::getInt32Ty(Context), "LDRB_PRE_REGZExt", BB);
      Monitor::event_Instruction(ZExt);

      setRegValue(Rt, ZExt, BB);
    } break;
    case ARM::LDRBi12: { // 831 | LDRB Rt Rn Imm12 CC CPSR
      Type *Ty8 = Type::getInt8Ty(Context);
      Type *PtrTy8 = Type::getInt8PtrTy(Context);
      Type *Ty32 = Type::getInt32Ty(Context);

      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t Imm12 = MI->getOperand(2).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();

      bool conditional_execution = (CC != ARMCC::AL) && (CC != 0);
      BasicBlock *BaseBB;
      BasicBlock *MergeBB;
      if (conditional_execution) {
        // Split on condition
        Monitor::event_raw() << "splitting on condition\n";
        Value *Cond = ARMCCToValue(ARMCC::getOppositeCondition(CC), BB);
        BasicBlock *CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Instruction *CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        Instruction* MergeElse = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeElse);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value *Ptr = RegValueMap[Rn];
      auto &OS=Monitor::event_raw(); OS << "Loading type "; Ptr->getType()->print(OS); OS << "\n";
      // Cast to pointer if necessary
      if (Ptr->getType() == Ty32) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy8, "LDRBi12CastUp", BB);
        Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }
      // Bitcast pointer if necessary
      if (Ptr->getType() != Ty8) {
        Instruction *Bitcast = new BitCastInst(Ptr, PtrTy8, "LDRHBitcast", BB);
        Monitor::event_Instruction(Bitcast);
        Ptr = Bitcast;
      }
      // GEP if there is an offset
      if (Imm12 != 0) {
        Instruction *GEP = GetElementPtrInst::Create(Ty8, Ptr, { ConstantInt::get(Ty32, Imm12) }, "LDRBi12GEP", BB);
        Monitor::event_Instruction(GEP);
        Ptr = GEP;
      }
      // Load
      Instruction *Load = new LoadInst(Ty8, Ptr, "LDRBi12", BB);
      Monitor::event_Instruction(Load);
      Ptr = Load;
      // Truncate if necessary
      if (Ptr->getType() != Ty8) {
        Instruction *Trunc = new TruncInst(Ptr, Ty8, "LDRBi12Trunc", BB);
        Monitor::event_Instruction(Trunc);
        Ptr = Trunc;
      }
      // Zero extend
      Instruction *ZExt = new ZExtInst(Ptr, Ty32, "LDRBi12ZExt", BB);
      Monitor::event_Instruction(ZExt);
      Ptr = ZExt;
      
      if (conditional_execution) {      
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode *phi = PHINode::Create(Ty32, 2, "LDRBi12Phi", MergeBB);
        phi->addIncoming(RegValueMap[Rt], BaseBB);
        phi->addIncoming(Ptr, BB);
        Ptr = phi;
      }

      RegValueMap[Rt] = Ptr;
    } break;
    case ARM::LDREX: { // 836 | LDREX Rt Rn CC CPSR
      Type *PtrTy32 = Type::getInt32PtrTy(Context);

      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "LDREX: assuming no flags for now"
      );

      Value *Ptr = RegValueMap[Rn];

      if (Ptr->getType() != PtrTy32) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy32, "LDREXPtrCast", BB);
        Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }

      Function *LDREX = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_ldrex, {PtrTy32});
      Instruction *Instr = CallInst::Create(LDREX, { Ptr }, "LDREX", BB);
      Monitor::event_Instruction(Instr);

      RegValueMap[Rt] = Instr;
    } break;
    case ARM::LDRH: { // 840 | LDRH Rt Rn - Imm12 CC CPSR
      Type *Ty16 = Type::getInt16Ty(Context);
      Type *PtrTy16 = Type::getInt16PtrTy(Context);
      Type *Ty32 = Type::getInt32Ty(Context);

      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      assert(MI->getOperand(2).getReg() == 0 && "ARM::LDRH: expecting no secondary register");
      int64_t Imm12 = MI->getOperand(3).getImm();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(4).getImm();
      Register CPSR = MI->getOperand(5).getReg();

      bool conditional_execution = (CC != ARMCC::AL) && (CC != 0);
      BasicBlock *BaseBB;
      BasicBlock *MergeBB;
      if (conditional_execution) {
        // Split on condition
        Monitor::event_raw() << "splitting on condition\n";
        Value *Cond = ARMCCToValue(ARMCC::getOppositeCondition(CC), BB);
        BasicBlock *CondExecBB = BasicBlock::Create(Context, "CondExec", F);
        MBBBBMap[MBB].push_back(CondExecBB);
        MergeBB = BasicBlock::Create(Context, "Merge", F);
        MBBBBMap[MBB].push_back(MergeBB);
        Instruction *CondBranch = BranchInst::Create(CondExecBB, MergeBB, Cond, BB);
        Monitor::event_Instruction(CondBranch);
        Instruction* MergeElse = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeElse);
        BaseBB = BB;
        BB = CondExecBB;
      }

      Value *Ptr = RegValueMap[Rn];
      auto &OS=Monitor::event_raw(); OS << "Loading type "; Ptr->getType()->print(OS); OS << "\n";
      // Cast to pointer if necessary
      if (Ptr->getType() == Ty32) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy16, "LDRHCastUp", BB);
        Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }
      // Bitcast pointer if necessary
      if (Ptr->getType() != Ty16) {
        Instruction *Bitcast = new BitCastInst(Ptr, PtrTy16, "LDRHBitcast", BB);
        Monitor::event_Instruction(Bitcast);
        Ptr = Bitcast;
      }
      // GEP if there is an offset
      if (Imm12 != 0) {
        Instruction *GEP = GetElementPtrInst::Create(Ty16, Ptr, { ConstantInt::get(Ty32, Imm12) }, "LDRHGEP", BB);
        Monitor::event_Instruction(GEP);
        Ptr = GEP;
      }
      // Load
      Instruction *Load = new LoadInst(Ty16, Ptr, "LDRH", BB);
      Monitor::event_Instruction(Load);
      Ptr = Load;
      // Truncate if necessary
      if (Ptr->getType() != Ty16) {
        Instruction *Trunc = new TruncInst(Ptr, Ty16, "LDRHTrunc", BB);
        Monitor::event_Instruction(Trunc);
        Ptr = Trunc;
      }
      // Zero extend
      Instruction *ZExt = new ZExtInst(Ptr, Ty32, "LDRHZExt", BB);
      Monitor::event_Instruction(ZExt);
      Ptr = ZExt;
      
      if (conditional_execution) {      
        Instruction* MergeIf = BranchInst::Create(MergeBB, BB);
        Monitor::event_Instruction(MergeIf);
        PHINode *phi = PHINode::Create(Ty32, 2, "LDRHPhi", MergeBB);
        phi->addIncoming(RegValueMap[Rt], BaseBB);
        phi->addIncoming(Ptr, BB);
        Ptr = phi;
      }

      RegValueMap[Rt] = Ptr;
    } break;
    /*
    case ARM::LDRSB: { // 845 | LDR{type}{cond} Rt, [Rn {, #offset}]
      // SB | Load Signed Byte; Sign extend to 32 bits
      Type *Ty8 = Type::getInt8Ty(Context);
      Type *Ty32 = Type::getInt32Ty(Context);
      Value *Rn = getOperandValue(MI, 1);
      assert(MI->getOperand(2).getReg() == 0 && MI->getOperand(3).getImm() == 0
        && "ARM::LDRSB: assuming zero offsets for now");

      Instruction *Cast = new IntToPtrInst(Rn, Ty32->getPointerTo(), "LDRSBPtrCast", BB); Monitor::event_Instruction(Cast);
      Instruction *Instr = new LoadInst(Ty32, Cast, "LDRSBLoad", BB); Monitor::event_Instruction(Instr);
      Instruction *Trunc = new TruncInst(Instr, Ty8, "LDRSBTrunc", BB); Monitor::event_Instruction(Trunc);
      Instruction *SExt = new SExtInst(Trunc, Ty32, "LDRSBSExt", BB); Monitor::event_Instruction(SExt);
      setOperandValue(MI, 0, SExt);
    } break;
    */
    case ARM::LDRi12: { // 862 | LDR Rt Rn Imm12 CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *PtrTy32 = Type::getInt32PtrTy(Context);

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
        Value *GV = getGlobalValueByOffset(MCIR->getMCInstIndex(*MI), Imm12 + 8);
        Monitor::event_raw() << "Global Value: " << GV->getName() << "\n";
        Instruction *Cast = new PtrToIntInst(GV, Ty32, "LDRi12CastDown", BB);
        Monitor::event_Instruction(Cast); 
        RegValueMap[Rt] = Cast;
        break;
      }
      
      if (Rn == ARM::SP) { // Load top relative StackValue
        auto I = stack_map.find(stack_offset + Imm12);
        if (I != stack_map.end()) {
          Instruction *Load = new LoadInst(Ty32, I->second, "LDRi12Load", BB);
          Monitor::event_Instruction(Load);
          RegValueMap[Rt] = Load;
          break;
        }

        // Match closest stack value
        I = --stack_map.lower_bound(stack_offset + Imm12);
        Monitor::event_raw() << "Can't find StackValue directly; using offset from value at " << I->first << "\n";

        int64_t diff = stack_offset + Imm12 - I->first;
        Value *StackValue = I->second;
        if (!StackValue->getType()->isPointerTy()) {
          Instruction *Cast = new IntToPtrInst(StackValue, PtrTy32, "LDRi12CastUp", BB);
          Monitor::event_Instruction(Cast);
          StackValue = Cast;
        }
        Instruction *GEP = GetElementPtrInst::Create(Ty32, StackValue, ConstantInt::get(Ty32, diff), "LDRi12GEP", BB);
        Monitor::event_Instruction(GEP);
        Instruction *Load = new LoadInst(Ty32, GEP, "LDRi12Load", BB);
        Monitor::event_Instruction(Load);
        RegValueMap[Rt] = Load;
        break;
      }

      if (Rn == ARM::R11) { // Load bottom relative StackValue
        auto I = stack_map.find(Imm12);
        if (I != stack_map.end()) {
          {auto &OS = Monitor::event_raw();
            OS << "Found StackValue directly at " << I->first << "\n";
            OS << "StackValue: " << *I->second << " (type: ";I->second->getType()->print(OS); OS << ")\n";
          }
          Value *StackValue = I->second;
          if (!StackValue->getType()->isPointerTy()) {
            Instruction *Cast = new IntToPtrInst(StackValue, PtrTy32, "LDRi12CastUp", BB);
            Monitor::event_Instruction(Cast);
            StackValue = Cast;
          }
          Instruction *Load = new LoadInst(Ty32, StackValue, "LDRi12Load", BB);
          Monitor::event_Instruction(Load);
          RegValueMap[Rt] = Load;
          break;
        }

        // Match closest stack value
        I = --stack_map.lower_bound(Imm12);
        Monitor::event_raw() << "Can't find StackValue directly; using offset from value at " << I->first << "\n";
        
        int64_t diff = Imm12 - I->first;
        Value *StackValue = I->second;
        if (!StackValue->getType()->isPointerTy()) {
          Instruction *Cast = new IntToPtrInst(StackValue, PtrTy32, "LDRi12CastUp", BB);
          Monitor::event_Instruction(Cast);
          StackValue = Cast;
        }
        Instruction *GEP = GetElementPtrInst::Create(Ty32, StackValue, ConstantInt::get(Ty32, diff), "LDRi12GEP", BB);
        Monitor::event_Instruction(GEP);
        Instruction *Load = new LoadInst(Ty32, GEP, "LDRi12Load", BB);
        Monitor::event_Instruction(Load);
        RegValueMap[Rt] = Load;
        break;
      }
      
      Value *Ptr = RegValueMap[Rn];

      // Offset
      if (Imm12 != 0) {
        if (Ptr->getType()->isPointerTy()) {
          Instruction *Cast = new PtrToIntInst(Ptr, Ty32, "LDRi12CastDown", BB);
          Monitor::event_Instruction(Cast);
          Ptr = Cast;
        }
        Instruction *Add = BinaryOperator::Create(Instruction::Add, Ptr, ConstantInt::get(Ty32, Imm12), "LDRi12Add", BB);
        Monitor::event_Instruction(Add);
        Ptr = Add;
      }
      
      // Ptr cast
      Instruction *Cast = new IntToPtrInst(Ptr, PtrTy32, "LDRi12CastUp", BB);
      Monitor::event_Instruction(Cast);
      Ptr = Cast;

      Instruction *Instr = new LoadInst(Ty32, Ptr, "LDRi12Load", BB);
      Monitor::event_Instruction(Instr);
      
      RegValueMap[Rt] = Instr;
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
      
      Value *Addr = resolveAM2Shift(Rn, Rm, AM2Shift, BB);
      
      Instruction *Cast = new IntToPtrInst(Addr, Type::getInt32PtrTy(Context), "LDRrsCastUp", BB);
      Monitor::event_Instruction(Cast);
      Instruction *Load = new LoadInst(Type::getInt32Ty(Context), Cast, "LDRrsLoad", BB);
      Monitor::event_Instruction(Load);

      setRegValue(Rt, Load, BB);
    } break;
    /*
    case ARM::MLA: { // 868 | MLA<c> <Rd>, <Rn>, <Rm>, <Ra> => Rd = Rn * Ra + Rm
      assert(false && "ARM::MLA not yet implemented");
    } break;
    case ARM::MOVPCLR: { // 870 | MOV<c> PC, LR => PC = LR
      Type *RetTy = FuncInfo->Fn->getReturnType();

      if (RetTy->isVoidTy()) {
        Instruction *Instr = ReturnInst::Create(Context, BB); Monitor::event_Instruction(Instr);
      } else if (RetTy->isIntegerTy()) {
        Instruction *Instr = ReturnInst::Create(Context, RegValueMap[ARM::R0], BB); Monitor::event_Instruction(Instr);
      } else
        assert(false && "Unsupported return type");
    } break;
    */
    case ARM::MOVTi16: { // 871 |  Rd Rd Imm16 CC CPSR
      Type *Ty16 = Type::getInt16Ty(Context);
      Type *Ty32 = Type::getInt32Ty(Context);

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

      Value *Val = RegValueMap[Rd];
      
      Instruction *Trunc = new TruncInst(Val, Ty16, "MOVTi16Trunc", BB);
      Monitor::event_Instruction(Trunc);
      Val = Trunc;

      Instruction *ZExt = new ZExtInst(Val, Ty32, "MOVTi16ZExt", BB);
      Monitor::event_Instruction(ZExt);
      Val = ZExt;

      Instruction *Add = BinaryOperator::Create(Instruction::Add, Val, ConstantInt::get(Ty32, Imm16 << 16), "MOVTi16Add", BB);
      Monitor::event_Instruction(Add);
      Val = Add;

      assert(Val->getType() == Ty32 && "ARM::MOVTi16: expecting output type to be Ty32");
      RegValueMap[Rd] = Val;
    } break;
    case ARM::MOVi: { // 872 | MOV Rt Op2 CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);

      Register Rt = MI->getOperand(0).getReg();
      int64_t Op2 = MI->getOperand(1).getImm();
      unsigned Bits = Op2 & 0xFF;
      unsigned Rot = (Op2 & 0xF00) >> 7;
      int64_t Imm = static_cast<int64_t>(ARM_AM::rotr32(Bits, Rot));
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();
      Register S = MI->getOperand(4).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::MOVi: assuming no flags for now"
      );

      RegValueMap[Rt] = ConstantInt::get(Ty32, Imm);
      Monitor::event_raw() << "Reg " << Rt << " <= " << Imm << "\n";
    } break;
    case ARM::MOVi16: { // 873 | MOV Rd Imm16 CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);

      Register Rd = MI->getOperand(0).getReg();
      int64_t Imm16 = MI->getOperand(1).getImm();
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::MOVi16: assuming no flags for now"
      );

      setRegValue(Rd, ConstantInt::get(Ty32, Imm16), BB);

    } break;
    case ARM::MOVr: { // 874 | MOV Rd Rn CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);

      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      int64_t CC = MI->getOperand(2).getImm();
      Register CPSR = MI->getOperand(3).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::MOVr: assuming no flags for now"
      );

      if (Rn == ARM::SP) {
        assert(false && "ARM::MOVr: SP not yet implemented, Rn != SP");
        break;
      }
      
      RegValueMap[Rd] = RegValueMap[Rn];
      Monitor::event_raw() << "Reg " << Rd << " <= " << Rn << "\n";
    } break;
    case ARM::MOVsi: { // 876 | MOV Rd Rm Shift CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);

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

      Value *RmVal = RegValueMap[Rm];

      Instruction::BinaryOps ShiftOp;
      switch (ShiftOpcode) {
        case ARM_AM::asr: ShiftOp = Instruction::AShr; break;
        case ARM_AM::lsl: ShiftOp = Instruction::Shl; break;
        case ARM_AM::lsr: ShiftOp = Instruction::LShr; break;
        default:
          assert(false && "ADDrsi: unknown shift opcode");
      }

      Instruction *ShiftInstr = BinaryOperator::Create(ShiftOp, RmVal, ConstantInt::get(Ty32, ShiftAmount), "MOVsiShift", BB);
      Monitor::event_Instruction(ShiftInstr);
      RegValueMap[Rd] = ShiftInstr;
    } break;
    case ARM::MUL: { // 888 | MUL Rd Rn Rm CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);
      
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      int64_t CC = MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::MUL: assuming no flags for now"
      );

      Value *RnVal = RegValueMap[Rn];
      Value *RmVal = RegValueMap[Rm];

      Instruction *Mul = BinaryOperator::Create(Instruction::Mul, RnVal, RmVal, "MUL", BB);
      Monitor::event_Instruction(Mul);
      RegValueMap[Rd] = Mul;
    } break;
    /*
    case ARM::MVNi: { // 1736 | MVN<c> <Rd>, #<imm> => Rd = 0 - imm
      Type *Ty = Type::getInt32Ty(Context);
      Value *imm = getOperandValue(MI, 1, Ty);
      Value *zero = ConstantInt::get(Ty, 0);

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, zero, imm, "MVNi", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::MVNr: { // 1737 | MVN<c> <Rd>, <Rm> => Rd = 0 - Rm
      Value *Rm = getOperandValue(MI, 1);
      Value *zero = ConstantInt::get(Rm->getType(), 0);

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, zero, Rm, "MVNr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ORRri: { // 1748 | ORR<c> <Rd>, <Rn>, #<imm> => Rd = Rn OR imm
      Value *Rn = getOperandValue(MI, 1);
      Value *imm = getOperandValue(MI, 2, Rn->getType());

      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Rn, imm, "ORRri", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::ORRrr: { // 1749 | ORR<c> <Rd>, <Rn>, <Rm> => Rd = Rn OR Rm
      Value *Rn = getOperandValue(MI, 1);
      Value *Rm = getOperandValue(MI, 2);

      Instruction *Instr = BinaryOperator::Create(Instruction::Or, Rn, Rm, "ORRrr", BB); Monitor::event_Instruction(Instr);
      setOperandValue(MI, 0, Instr);
    } break;
    case ARM::SBCri: { // 1794 | SBC<c> <Rd>, <Rn>, #<imm> => Rd = Rn - imm - C
      assert(false && "ARM::SBCri not yet implemented; requires Carry flag");
    } break;
    case ARM::SBCrr: { // 1795 | SBC<c> <Rd>, <Rn>, <Rm> => Rd = Rn - Rm - C
      assert(false && "ARM::SBCrr not yet implemented; requires Carry flag");
    } break;
    case ARM::SMLAL: { // 1824 | SMLAL<c> <Rd>, <Rn>, <Rm>, <Ra> => Rd = Rn * Ra + Rm
      assert(false && "ARM::SMLAL not yet implemented");
    } break;
    */
    case ARM::SMULL: { // 1849 | SMULL RdLo RdHi Rn Rm CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *Ty64 = Type::getInt64Ty(Context);

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

      Value *RnVal = RegValueMap[Rn];
      Value *RmVal = RegValueMap[Rm];

      Instruction *RnExt = new SExtInst(RnVal, Ty64, "SMULLRnExt", BB);
      Monitor::event_Instruction(RnExt);
      Instruction *RmExt = new SExtInst(RmVal, Ty64, "SMULLRmExt", BB);
      Monitor::event_Instruction(RmExt);

      Instruction *Mul = BinaryOperator::Create(Instruction::Mul, RnExt, RmExt, "SMULL", BB);
      Monitor::event_Instruction(Mul);

      Instruction *Lo = new TruncInst(Mul, Ty32, "SMULLLo", BB);
      Monitor::event_Instruction(Lo);
      Instruction *Hi = BinaryOperator::Create(Instruction::LShr, Mul, ConstantInt::get(Ty64, 32), "SMULLHi", BB);
      Monitor::event_Instruction(Hi);
      Instruction *CastHi = new TruncInst(Hi, Ty32, "SMULLHiCast", BB);
      Monitor::event_Instruction(CastHi);

      RegValueMap[RdLo] = Lo;
      RegValueMap[RdHi] = CastHi;
    } break;
    case ARM::STMDB_UPD: { // 1895 | STMDB Rn Rn CC CPSR Reg
      Type *Ty32 = Type::getInt32Ty(Context);

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

      if (RegValueMap.count(Reg) == 0) {
        Monitor::event_raw() << "ARM::STMDB_UPD: encountered valueless register " << Reg << "\n";
        RegValueMap[Reg] = ConstantInt::get(Ty32, 0);
      }

      // Decrement SP by 4
      Monitor::event_raw() << "ARM::STMDB_UPD: decrementing SP by 4\n";
      stack_offset -= 4;

      Monitor::event_raw() << "stack[" << stack_offset << "] = Reg " << Reg << "]\n";
      stack_map[stack_offset] = RegValueMap[Reg];
      
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
    case ARM::STRB_POST_IMM: { // 1902 | STRB Rt Rn Rs - Shift CC CPSR
      Type *Ty8 = Type::getInt8Ty(Context);
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *PtrTy8 = Type::getInt8PtrTy(Context);

      Register Rt = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rs = MI->getOperand(2).getReg();
      assert(Rt == Rs && "ARM::STRB_POST_IMM: expecting Rt == Rs");
      assert(MI->getOperand(3).getReg() == 0 && "ARM::STRB_POST_IMM: expecting no secondary register");
      int64_t Shift = MI->getOperand(4).getImm();
      int64_t CC = MI->getOperand(5).getImm();
      Register CPSR = MI->getOperand(6).getReg();

      assert(
        CC == 14 &&
        CPSR == 0 &&
        "ARM::STRB_POST_IMM: assuming no flags for now"
      );

      unsigned Imm12 = Shift & 0xFFF;
      ARM_AM::ShiftOpc ShiftOpcode = (ARM_AM::ShiftOpc) ((Shift >> 13) & 0x7);
      bool isSub = (Shift >> 12) & 0x1;
      unsigned IdxMode = Shift >> 16;
      assert(ShiftOpcode == ARM_AM::lsl && "ARM::STRB_POST_IMM: expecting no shift opcode");
      assert(IdxMode == 2 && "ARM::STRB_POST_IMM: expecting post-increment mode");

      // 10 010 1 0000 0000 0001

      Instruction *Trunc = new TruncInst(RegValueMap[Rs], Ty8, "STRB_POST_IMMValTrunc", BB);
      Monitor::event_Instruction(Trunc);

      Value *Ptr = RegValueMap[Rn];
      if (Ptr->getType() != PtrTy8) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy8, "STRB_POST_IMMPtrCast", BB);
        Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }

      assert(Trunc->getType() == Ty8 && "ARM::STRB_POST_IMM: expecting i8 type value");
      assert(Ptr->getType() == PtrTy8 && "ARM::STRB_POST_IMM: expecting i8* type ptr");
      Instruction *Instr = new StoreInst(Trunc, Ptr, false, BB);
      Monitor::event_Instruction(Instr);

      Instruction *PostInc = BinaryOperator::Create(isSub ? Instruction::Sub : Instruction::Add, RegValueMap[Rs], ConstantInt::get(Ty32, Imm12), "STRB_POST_IMMPostInc", BB);
      Monitor::event_Instruction(PostInc);
      RegValueMap[Rt] = PostInc;
    } break; 
    /*
    case ARM::STRBi12: { // 1906 | STRB<c> <Rt>, [<Rn>, #-<imm>]!
      assert(false && "ARM::STRBi12 not yet implemented");
    } break;
    */
    case ARM::STREX: { // 1911 | STREX Rd Rt Rn CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *PtrTy32 = Type::getInt32PtrTy(Context);

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

      Value *Ptr = RegValueMap[Rt];
      Value *RnVal = RegValueMap[Rn];

      if (Ptr->getType() != PtrTy32) {
        Instruction *Cast = new IntToPtrInst(Ptr, PtrTy32, "STREXPtrCast", BB);
        Monitor::event_Instruction(Cast);
        Ptr = Cast;
      }

      if (RnVal->getType() != Ty32) {
        Instruction *Cast = new SExtInst(RnVal, Ty32, "STREXRnCast", BB);
        Monitor::event_Instruction(Cast);
        RnVal = Cast;
      }

      Function *STREX = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::arm_strex, {PtrTy32});
      Instruction *Instr = CallInst::Create(STREX, { RnVal, Ptr }, "STREX", BB);
      Monitor::event_Instruction(Instr);

      RegValueMap[Rd] = Instr;

    } break;
   /*
    case ARM::STRH: { // 1915 | STRH<c> <Rt>, [<Rn>, #<imm>]!
      assert(false && "ARM::STRH not yet implemented");
    } break;
    */
    case ARM::STRi12: { // 1926 | STR Rt Rn Imm12 CC CPSR
      Type *Ty32 = Type::getInt32Ty(Context);
      Type *PtrTy32 = Type::getInt32PtrTy(Context);

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

      if (Rn == ARM::SP) { // Store top relative StackValue
        auto I = stack_map.find(stack_offset + Imm12);
        if (I == stack_map.end()) {
          Instruction *Alloca = new AllocaInst(Ty32, 0, "STRi12Alloca", BB);
          Monitor::event_Instruction(Alloca);
          stack_map[stack_offset + Imm12] = Alloca;
        }
        Instruction *Store = new StoreInst(RegValueMap[Rt], stack_map[stack_offset + Imm12], false, BB);
        Monitor::event_Instruction(Store);
        break;
      }

      if (Rn == ARM::R11) { // Store bottom relative StackValue
        auto I = stack_map.find(Imm12);
        if (I == stack_map.end()) {
          Instruction *Alloca = new AllocaInst(Ty32, 0, "STRi12Alloca", BB);
          Monitor::event_Instruction(Alloca);
          stack_map[Imm12] = Alloca;
        }
        Instruction *Store = new StoreInst(RegValueMap[Rt], stack_map[Imm12], false, BB);
        Monitor::event_Instruction(Store);
        break;
      }

      Value *Ptr = RegValueMap[Rn];

      // Offset
      if (Imm12 != 0) {
        if (Ptr->getType()->isPointerTy()) {
          Instruction *Cast = new PtrToIntInst(Ptr, Ty32, "STRi12PtrCast", BB);
          Monitor::event_Instruction(Cast);
          Ptr = Cast;
        }
        Instruction *Add = BinaryOperator::Create(Instruction::Add, Ptr, ConstantInt::get(Ty32, Imm12), "STRi12Add", BB);
        Monitor::event_Instruction(Add);
        Ptr = Add;
      }
      
      // Ptr cast
      Instruction *Cast = new IntToPtrInst(Ptr, PtrTy32, "STRi12PtrCastUP", BB);
      Monitor::event_Instruction(Cast);
      Ptr = Cast;

      Instruction *Instr = new StoreInst(RegValueMap[Rt], Ptr, false, BB);
      Monitor::event_Instruction(Instr);
    } break;
    case ARM::SUBri: { // 1928 | SUB Rd Rn Op2 CC CPSR S
      Type *Ty32 = Type::getInt32Ty(Context);

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
        stack_offset -= Imm;
        break;
      }
    
      Instruction *Instr = BinaryOperator::CreateSub(RegValueMap[Rn], ConstantInt::get(Ty32, Imm), "SUBri", BB);
      Monitor::event_Instruction(Instr);
      RegValueMap[Rd] = Instr;

      if (S == ARM::CPSR) {
        Value *RnVal = RegValueMap[Rn];
        Value *ImmVal = ConstantInt::get(Ty32, Imm);
        
        // Negative flag
        Instruction *CmpNeg = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_SLT, RnVal, ImmVal, "SUBSriNeg", BB);
        Monitor::event_Instruction(CmpNeg);
        Instruction *StoreNeg = new StoreInst(CmpNeg, Flags[0], BB);
        Monitor::event_Instruction(StoreNeg);
        
        // Zero flag
        Instruction *CmpZero = ICmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, RnVal, ImmVal, "SUBSriZero", BB);
        Monitor::event_Instruction(CmpZero);
        Instruction *StoreZero = new StoreInst(CmpZero, Flags[1], BB);
        Monitor::event_Instruction(StoreZero);

        // Carry flag
        Function *USub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::usub_with_overflow, Ty32);
        Instruction *CallUSub = CallInst::Create(USub, {RnVal, ImmVal}, "USubInstrinsic", BB);
        Monitor::event_Instruction(CallUSub);
        Instruction *C_Flag = ExtractValueInst::Create(CallUSub, 1, "SUBSriCFlag", BB);
        Monitor::event_Instruction(C_Flag);
        Instruction *StoreCarry = new StoreInst(C_Flag, Flags[2], BB);
        Monitor::event_Instruction(StoreCarry);

        // Overflow flag
        Function *SSub = Intrinsic::getDeclaration(MR.getModule(), Intrinsic::ssub_with_overflow, Ty32);
        Instruction *CallSSub = CallInst::Create(SSub, {RnVal, ImmVal}, "SSubIntrinsic", BB);
        Monitor::event_Instruction(CallSSub);
        Instruction *V_Flag = ExtractValueInst::Create(CallSSub, 1, "SUBSriVFlag", BB);
        Monitor::event_Instruction(V_Flag);
        Instruction *StoreOverflow = new StoreInst(V_Flag, Flags[3], BB);
        Monitor::event_Instruction(StoreOverflow);
      }
    } break;
    case ARM::SUBrr: { // 1929 | SUB Rd Rn Rm CC CPSR S
      Register Rd = MI->getOperand(0).getReg();
      Register Rn = MI->getOperand(1).getReg();
      Register Rm = MI->getOperand(2).getReg();
      ARMCC::CondCodes CC = (ARMCC::CondCodes) MI->getOperand(3).getImm();
      Register CPSR = MI->getOperand(4).getReg();
      Register S = MI->getOperand(5).getReg();

      assert(
        CC == ARMCC::AL &&
        CPSR == 0 &&
        S == 0 &&
        "ARM::SUBrr: assuming no conditional flags for now"
      );

      Value *RnVal = RegValueMap[Rn];
      Value *RmVal = RegValueMap[Rm];

      Instruction *Instr = BinaryOperator::Create(Instruction::Sub, RnVal, RmVal, "ADDrr", BB);
      Monitor::event_Instruction(Instr);
      RegValueMap[Rd] = Instr;
    } break;
    /*
    case ARM::SVC: { // 1932 | SVC<c> #<imm>
      assert(false && "ARM::SVC not yet implemented");
    } break;
    */
  }
  Monitor::event_end("ARMLinearRaiserPass::RaiseMachineInstr");
  return true;
}

#undef DEBUG_TYPE
