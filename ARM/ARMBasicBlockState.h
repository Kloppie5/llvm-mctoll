
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H

#include "Monitor.h"
#include "ARMState.h"

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

  ARMState* PState;
  ARMState* QState;

  ARMBasicBlockState(BasicBlock *BB) : BB(BB) {
    Monitor::event_raw() << "Creating ARMBasicBlockState for BB " << BB->getName() << "\n";
    PState = new ARMState();
    QState = new ARMState();
  }

  bool updatePHINodes(BasicBlock *PBB, ARMBasicBlockState *Predecessor) {
    bool changed = false;
    for (std::pair<Register, Value*> element : PState->register_map) {
      Register reg = element.first;
      PHINode* phi = static_cast<PHINode*>(element.second);
      if (!phi)
        continue;

      if (phi->getBasicBlockIndex(PBB) == -1) {
        Value* V = Predecessor->getReg(reg, phi->getType());
        phi->addIncoming(V, PBB);
        changed = true;
      }
    }
    for (std::pair<ARMState::Status, Value*> element : PState->status_map) {
      ARMState::Status status = element.first;
      PHINode* phi = static_cast<PHINode*>(element.second);
      if (!phi)
        continue;

      if (phi->getBasicBlockIndex(PBB) == -1) {
        Value* V = Predecessor->getStatus(status, phi->getType());
        phi->addIncoming(V, PBB);
        changed = true;
      }
    }
    return changed;
  }

  void setReg(Register reg, Value *V) {
    QState->setReg(reg, V);
  }
  Value *getReg(Register reg, Type* Ty = nullptr) {
    Value* V = QState->getReg(reg);
    if (V != nullptr)
      return V;

    Monitor::event_raw() << "Creating PHI node for register " << reg << " in BB " << BB->getName() << "\n";
    PHINode *phi = PHINode::Create(Ty, 0, "r." + Twine(reg) + ".phi");
    PState->setReg(reg, phi);
    QState->setReg(reg, phi);
    return phi;
  }

  void setStatus(ARMState::Status status, Value *V) {
    QState->setStatus(status, V);
  }
  Value* getStatus(ARMState::Status status, Type* Ty = nullptr) {
    Value* V = QState->getStatus(status);
    if (V != nullptr)
      return V;

    Monitor::event_raw() << "Creating PHI node for status " << status << " in BB " << BB->getName() << "\n";
    PHINode *phi = PHINode::Create(Ty, 0, "s." + Twine(status) + ".phi");
    PState->setReg(status, phi);
    QState->setReg(status, phi);
    return phi;
  }

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H
