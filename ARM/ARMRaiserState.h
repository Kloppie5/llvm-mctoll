
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMRAISERSTATE_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMRAISERSTATE_H

#include "Monitor.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

class ARMRaiserState {

public:
  ModuleRaiser &MR;
  LLVMContext &Context;
  MCInstRaiser* MCIR;
  MachineFunction* MF;
  Function* F;

  std::map<BasicBlock* , ARMBasicBlockState* > BBStateMap;
  std::map<MachineBasicBlock* , std::vector<BasicBlock* >> MBBBBMap;
  std::map<BasicBlock* , std::vector<BasicBlock* >> BBPredMap;

  ARMRaiserState(ModuleRaiser &MR, MCInstRaiser* MCIR) : MR(MR), Context(MR.getModule()->getContext()), MCIR(MCIR), MF(nullptr), F(nullptr) { }

  void setup(MachineFunction* MF, Function* F) {
    this->MF = MF;
    this->F = F;
  }

  BasicBlock* createBasicBlock(MachineBasicBlock* MBB) {
    BasicBlock* BB = BasicBlock::Create(Context, "bb." + Twine(MBB->getNumber()), F);
    BBStateMap[BB] = new ARMBasicBlockState(BB);
    MBBBBMap[MBB].push_back(BB);
    return BB;
  }

  std::vector<BasicBlock* > getBasicBlocks(MachineBasicBlock* MBB) {
    if (MBBBBMap.find(MBB) == MBBBBMap.end())
      return {createBasicBlock(MBB)};
    return MBBBBMap[MBB];
  }

  void setReg(Register reg, Value* V, BasicBlock* BB) {
    {auto &OS=Monitor::event_raw(); OS << "setRegValue: " << reg << ": "; V->getType()->print(OS); OS << "\n";}
    BBStateMap[BB]->setReg(reg, V);
  }
  Value* getReg(Register reg, Type* Ty, BasicBlock* BB) {
    assert(BBStateMap.count(BB) && "BBStateMap does not contain BB");
    Value* V = BBStateMap[BB]->getReg(reg, Ty->isPointerTy() ? Type::getInt32Ty(Context) : Ty);
    {auto &OS=Monitor::event_raw(); OS << "getRegValue: " << reg << ": "; if(Ty) {Ty->print(OS); OS << " <= ";} V->getType()->print(OS); OS << "\n";}
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

  void setStatus(ARMState::Status status, Value* V, BasicBlock* BB) {
    BBStateMap[BB]->setStatus(status, V);
  }
  // Value* getStatus(ARMState::Status status, Type* Ty = nullptr, BasicBlock* BB) {
  Value* getStatus(ARMState::Status status, BasicBlock* BB) {
    return getStatus(status, nullptr, BB);
  }
  Value* getStatus(ARMState::Status status, Type* Ty, BasicBlock* BB) {
    return BBStateMap[BB]->getStatus(status, Ty);
  }

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMRAISERSTATE_H
