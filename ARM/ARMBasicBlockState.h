
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

  bool R11_is_FP = true;
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
        Value* V = PState->getRegValue(reg, phi->getType());
        {auto &OS=Monitor::event_raw(); OS << "Adding to reg " << reg << ", type "; phi->getType()->print(OS); OS << " <= "; V->getType()->print(OS); OS << "\n";}
        phi->addIncoming(V, PBB);
        changed = true;
      }
    }
    for (std::pair<StateFlag, PHINode*> FlagPHIPair : P_FlagValueMap) {
      StateFlag flag = FlagPHIPair.first;
      PHINode* phi = FlagPHIPair.second;

      if (phi->getBasicBlockIndex(PBB) == -1) {
        Value* V = PState->getFlagValue(flag);
        {auto &OS=Monitor::event_raw(); OS << "Adding to flag " << flag << ", type "; phi->getType()->print(OS); OS << " <= "; V->getType()->print(OS); OS << "\n";}
        phi->addIncoming(V, PBB);
        changed = true;
      }
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

  /*-+- StateFlagMap -+-*/
  enum StateFlag {
    // Current Program State Register
    INVALID = 0,
    CPSR_N_Flag,
    CPSR_Z_Flag,
    CPSR_C_Flag,
    CPSR_V_Flag,

    // Floating-Point Status and Control Register
    FPSCR_N_Flag,
    FPSCR_Z_Flag,
    FPSCR_C_Flag,
    FPSCR_V_Flag,

  };
  std::map<StateFlag, PHINode*> P_FlagValueMap;
  std::map<StateFlag, Value*> Q_FlagValueMap;

  void setFlagValue(StateFlag Flag, Value *V) {
    assert(V->getType() == Type::getInt1Ty(BB->getContext()) && "flag must be of type i1");
    Q_FlagValueMap[Flag] = V;
  }
  Value* getFlagValue(StateFlag Flag) {
    Value* V = Q_FlagValueMap[Flag];
    if (V == nullptr) {
      Monitor::event_raw() << "Creating PHI node for " << Flag << " in BB " << BB->getName() << "\n";
      PHINode *phi;
      if (BB->size() == 0)
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, Twine(Flag) + ".phi", BB);
      else
        phi = PHINode::Create(Type::getInt1Ty(BB->getContext()), 0, Twine(Flag) + ".phi", &*BB->begin());
      P_FlagValueMap[Flag] = phi;
      V = phi;
      Q_FlagValueMap[Flag] = V;
    }
    return V;
  }

  // Could all be inlined in the linear raiser pass, but maybe leave the
  // internal logic of how flags are handled within this class.

  void setCPSRNFlag(Value *V) { setFlagValue(CPSR_N_Flag, V); }
  Value* getCPSRNFlag() { return getFlagValue(CPSR_N_Flag); }
  void setCPSRZFlag(Value *V) { setFlagValue(CPSR_Z_Flag, V); }
  Value* getCPSRZFlag() { return getFlagValue(CPSR_Z_Flag); }
  void setCPSRCFlag(Value *V) { setFlagValue(CPSR_C_Flag, V); }
  Value* getCPSRCFlag() { return getFlagValue(CPSR_C_Flag); }
  void setCPSRVFlag(Value *V) { setFlagValue(CPSR_V_Flag, V); }
  Value* getCPSRVFlag() { return getFlagValue(CPSR_V_Flag); }

  void setFPSCRNFlag(Value *V) { setFlagValue(FPSCR_N_Flag, V); }
  Value* getFPSCRNFlag() { return getFlagValue(FPSCR_N_Flag); }
  void setFPSCRZFlag(Value *V) { setFlagValue(FPSCR_Z_Flag, V); }
  Value* getFPSCRZFlag() { return getFlagValue(FPSCR_Z_Flag); }
  void setFPSCRCFlag(Value *V) { setFlagValue(FPSCR_C_Flag, V); }
  Value* getFPSCRCFlag() { return getFlagValue(FPSCR_C_Flag); }
  void setFPSCRVFlag(Value *V) { setFlagValue(FPSCR_V_Flag, V); }
  Value* getFPSCRVFlag() { return getFlagValue(FPSCR_V_Flag); }
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMBASICBLOCKSTATE_H
