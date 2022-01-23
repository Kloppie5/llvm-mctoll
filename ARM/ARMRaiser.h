
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMRAISER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMRAISER_H

#include "ARMBasicBlockState.h"
#include "ARMRaiserState.h"

using namespace llvm;

class ARMRaiser {

public:
  static AllocaInst* getOrCreateStackAlloca(ARMRaiserState* State, Register Reg, int64_t offset, Type* Ty, BasicBlock* BB);
  static AllocaInst* getOrCreateStackAlloca(ARMRaiserState* State, int64_t offset, Type* Ty, BasicBlock* BB);
  static std::pair<Register, Register> splitARMv7DRegister(Register DReg);
  static Value* resolveAM2Shift(ARMRaiserState* State, Register Rn, Register Rs, Register Rm, int64_t AM2Shift, BasicBlock* BB);
  static GlobalValue* getGlobalValueByOffset(ARMRaiserState* State, int64_t MCInstOffset, uint64_t PCOffset, Type* Ty); // TODO: Remove from static raise part
  static Value* ARMCCToValue(ARMRaiserState* State, int Cond, BasicBlock* BB);

  static bool raiseADCri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseADDri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseADDrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseADDrsi       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseANDri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseANDrsi       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseANDrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseBFC          (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseBICri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseBL           (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseBL_pred      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseBX_RET       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseBcc          (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseCLZ          (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseCMNri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseCMPri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseCMPrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseDMB          (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseEORrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseHINT         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDMIA        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDMIA_UPD    (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDRB_PRE_REG (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDRBi12      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDREX        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDRH         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDR_POST_IMM (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDR_POST_REG (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDRi12       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseLDRrs        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMLA          (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMOVTi16      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMOVi         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMOVi16       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMOVr         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMOVsi        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMUL          (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMVNi         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseMVNr         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseORRri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseORRrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseRSBri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSBCrsi       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSMMUL        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSMULL        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTMDB_UPD    (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTMIA        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTMIB        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTRB_POST_IMM(ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTRBi12      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTREX        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTRH         (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTRi12       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSTRrs        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSUBri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSUBrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseSUBrsi       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseTEQrr        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseTSTri        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseTSTrr        (ARMRaiserState* State, MachineInstr* MI);

  // VFP
  static bool raiseFMSTAT       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseFCONSTD      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseFCONSTS      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVABSD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVADDD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVADDS        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVCMPD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVCMPZD       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVDIVD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVLDMDIA_UPD  (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVLDRD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVLDRS        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMLAD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMOVD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMOVDRR      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMOVRRD      (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMOVRS       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMOVSR       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMOVv2i32    (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVMULD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVNEGD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVORRd        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVSITOD       (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVSTMDDB_UPD  (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVSTMDIA_UPD  (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVSTRD        (ARMRaiserState* State, MachineInstr* MI);
  static bool raiseVSUBD        (ARMRaiserState* State, MachineInstr* MI);
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMRAISER_H
