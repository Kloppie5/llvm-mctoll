
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H

#include "Monitor.h"

using namespace llvm;

/*
  Because the state of the program doesn't change between two normal, sequentially executed instructions,
  instead of having to write to and read from the state, the SSA value can be used directly.
  BasicBlocks are like functions though. Where functions depend on arguments because of the different locations
  in the program the function can be called, so too do the values used in BasicBlocks depend on different
  program states based on the predecessor.
  In order to construct these states without requiring the construction of predecessors, since this would be
  impossible for basically all non-trival programs because of circular dependencies, the "state" of a
  BasicBlock is split between the P state, which holds phi nodes for the registers, and the Q state, which holds
  the last recorded value for the registers.
*/
class ARMBasicBlockState {

public:
  BasicBlock* BB;

  bool R11_is_FP = false;
  int64_t FP_offset = 0;
  int64_t SP_offset = 0;

  ARMBasicBlockState(BasicBlock *BB) : BB(BB) {
    Monitor::event_raw() << "Creating ARMBasicBlockState for BB " << BB->getName() << "\n";
  }

  bool updatePHINodes(BasicBlock *PBB, ARMBasicBlockState *PState) {
    bool changed = false;
    for (std::pair<Register, PHINode*> RegPHIPair : P_RegValueMap) {
      Register reg = RegPHIPair.first;
      PHINode* phi = RegPHIPair.second;

      if (phi->getBasicBlockIndex(PBB) == -1) {
        Value* V = PState->getRegValue(reg);
        {auto &OS=Monitor::event_raw(); OS << "Adding to reg " << reg << ", type "; phi->getType()->print(OS); OS << " <= "; V->getType()->print(OS); OS << "\n";}
        phi->addIncoming(V, PBB);
        changed = true;
      }
    }
    if (P_N_Flag && P_N_Flag->getBasicBlockIndex(PBB) == -1) {
      P_N_Flag->addIncoming(PState->getNFlag(), PBB);
      changed = true;
    }
    if (P_Z_Flag && P_Z_Flag->getBasicBlockIndex(PBB) == -1) {
      P_Z_Flag->addIncoming(PState->getZFlag(), PBB);
      changed = true;
    }
    if (P_C_Flag && P_C_Flag->getBasicBlockIndex(PBB) == -1) {
      P_C_Flag->addIncoming(PState->getCFlag(), PBB);
      changed = true;
    }
    if (P_V_Flag && P_V_Flag->getBasicBlockIndex(PBB) == -1) {
      P_V_Flag->addIncoming(PState->getVFlag(), PBB);
      changed = true;
    }
    return changed;
  }

  /*-+- RegValueMap -+-*/
  std::map<Register, PHINode*> P_RegValueMap;
  std::map<Register, Value*> Q_RegValueMap;
  void setRegValue(Register Reg, Value *V) {
    Q_RegValueMap[Reg] = V;
  }
  Value *getRegValue(Register Reg, Type* Ty = nullptr) {
    Value* V = Q_RegValueMap[Reg];
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for " << Reg << " in BB " << BB->getName() << "\n";
      PHINode *phi;
      if (BB->size() == 0)
        phi = PHINode::Create(Ty, 0, Twine(Reg) + ".phi", BB);
      else
        phi = PHINode::Create(Ty, 0, Twine(Reg) + ".phi", &*BB->begin());
      P_RegValueMap[Reg] = phi;
      V = phi;
      Q_RegValueMap[Reg] = V;
    }
    return V;
  }

  /*-+- N Flag -+-*/
  PHINode* P_N_Flag = nullptr;
  Value* Q_N_Flag = nullptr;
  void setNFlag(Value *V) {
    assert(V->getType() == Type::getInt1Ty(BB->getContext()) && "N flag must be of type i1");
    Q_N_Flag = V;
  }
  Value* getNFlag() {
    Value* V = Q_N_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for N in BB " << BB->getName() << "\n";
      PHINode *phi;
      if (BB->size() == 0)
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "N.phi", BB);
      else
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "N.phi", &*BB->begin());
      P_N_Flag = phi;
      V = phi;
      Q_N_Flag = V;
    }
    return V;
  }

  /*-+- Z Flag -+-*/
  PHINode* P_Z_Flag = nullptr;
  Value* Q_Z_Flag = nullptr;
  void setZFlag(Value *V) {
    assert(V->getType() == Type::getInt1Ty(BB->getContext()) && "Z flag must be of type i1");
    Q_Z_Flag = V;
  }
  Value* getZFlag() {
    Value* V = Q_Z_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for Z in BB " << BB->getName() << "\n";
      PHINode *phi;
      if (BB->size() == 0)
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "Z.phi", BB);
      else
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "Z.phi", &*BB->begin());
      P_Z_Flag = phi;
      V = phi;
      Q_Z_Flag = V;
    }
    return V;
  }

  /*-+- C Flag -+-*/
  PHINode* P_C_Flag = nullptr;
  Value* Q_C_Flag = nullptr;
  void setCFlag(Value *V) {
    assert(V->getType() == Type::getInt1Ty(BB->getContext()) && "C flag must be of type i1");
    Q_C_Flag = V;
  }
  Value* getCFlag() {
    Value* V = Q_C_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for C in BB " << BB->getName() << "\n";
      PHINode *phi;
      if (BB->size() == 0)
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "C.phi", BB);
      else
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "C.phi", &*BB->begin());
      P_C_Flag = phi;
      V = phi;
      Q_C_Flag = V;
    }
    return V;
  }

  /*-+- V Flag -+-*/
  PHINode* P_V_Flag = nullptr;
  Value* Q_V_Flag = nullptr;
  void setVFlag(Value *V) {
    assert(V->getType() == Type::getInt1Ty(BB->getContext()) && "V flag must be of type i1");
    Q_V_Flag = V;
  }
  Value* getVFlag() {
    Value* V = Q_V_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for V in BB " << BB->getName() << "\n";
      PHINode *phi;
      if (BB->size() == 0)
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "V.phi", BB);
      else
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "V.phi", &*BB->begin());
      P_V_Flag = phi;
      V = phi;
      Q_V_Flag = V;
    }
    return V;
  }

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H
