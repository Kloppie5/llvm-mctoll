
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

  /*-+- RegValueMap -+-*/
  std::map<Register, PHINode*> P_RegValueMap;
  std::map<Register, Value*> Q_RegValueMap;
  void setRegValue(Register Reg, Value *V) {
    Q_RegValueMap[Reg] = V;
  }
  Value *getRegValue(Register Reg) {
    Value* V = Q_RegValueMap[Reg];
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for " << Reg << " in BB " << BB->getName() << "\n";
      PHINode *phi = PHINode::Create(Type::getInt32Ty(BB->getContext()), 0, "", BB);
      P_RegValueMap[Reg] = phi;
      V = phi;
      Q_RegValueMap[Reg] = V;
    }
    return V;
  }

  /*-+- N Flag -+-*/
  PHINode* P_N_Flag;
  Value* Q_N_Flag;
  void setNFlag(Value *V) {
    Q_N_Flag = V;
  }
  Value* getNFlag() {
    Value* V = Q_N_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for N in BB " << BB->getName() << "\n";
      PHINode *phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "", BB);
      P_N_Flag = phi;
      V = phi;
      Q_N_Flag = V;
    }
    return V;
  }

  /*-+- Z Flag -+-*/
  PHINode* P_Z_Flag;
  Value* Q_Z_Flag;
  void setZFlag(Value *V) {
    Q_Z_Flag = V;
  }
  Value* getZFlag() {
    Value* V = Q_Z_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for Z in BB " << BB->getName() << "\n";
      PHINode *phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "", BB);
      P_Z_Flag = phi;
      V = phi;
      Q_Z_Flag = V;
    }
    return V;
  }

  /*-+- C Flag -+-*/
  PHINode* P_C_Flag;
  Value* Q_C_Flag;
  void setCFlag(Value *V) {
    Q_C_Flag = V;
  }
  Value* getCFlag() {
    Value* V = Q_C_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for C in BB " << BB->getName() << "\n";
      PHINode *phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "", BB);
      P_C_Flag = phi;
      V = phi;
      Q_C_Flag = V;
    }
    return V;
  }

  /*-+- V Flag -+-*/
  PHINode* P_V_Flag;
  Value* Q_V_Flag;
  void setVFlag(Value *V) {
    Q_V_Flag = V;
  }
  Value* getVFlag() {
    Value* V = Q_V_Flag;
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for V in BB " << BB->getName() << "\n";
      PHINode *phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, "", BB);
      P_V_Flag = phi;
      V = phi;
      Q_V_Flag = V;
    }
    return V;
  }

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H
