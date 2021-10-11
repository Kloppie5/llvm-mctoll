
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H

#include "RaiserPass.h"
#include "ARMBasicBlockState.h"
#include "MCInstRaiser.h"
#include "ModuleRaiser.h"

using namespace llvm;

class ARMLinearRaiserPass : public RaiserPass {
public:
  MachineFunction* MF;
  Function* F;

  std::map<MachineBasicBlock* , std::vector<BasicBlock* >> MBBBBMap;

  ARMLinearRaiserPass(ModuleRaiser &MR, MCInstRaiser* MCIR)
   : RaiserPass(MR), MCIR(MCIR) {}

  bool run (MachineFunction* MF, Function* F) override;

  std::map<BasicBlock* , ARMBasicBlockState* > BBStateMap;

  std::map<int64_t, AllocaInst*> stack_map;
  AllocaInst* getOrCreateStackAlloca(Register Reg, int64_t offset, Type* Ty, BasicBlock* BB);
  AllocaInst* getOrCreateStackAlloca(int64_t offset, Type* Ty, BasicBlock* BB);

  Value* getRegValue(Register Reg, Type* Ty, BasicBlock* BB);
  void setRegValue(Register Reg, Value* V, BasicBlock* BB);
  Value* getDRegValueF64(Register DReg, BasicBlock* BB);
  std::pair<Value*, Value*> getDRegValueV2F32(Register DReg, BasicBlock* BB);
  std::pair<Value*, Value*> getDRegValueV2I32(Register DReg, BasicBlock* BB);
  void setDRegValueF64(Register DReg, Value* V, BasicBlock* BB);
  void setDRegValueV2F32(Register DReg, Value* V1, Value* V2, BasicBlock* BB);
  void setDRegValueV2I32(Register DReg, Value* V1, Value* V2, BasicBlock* BB);

  std::pair<Register, Register> splitARMv7DRegister(Register DReg);

  Value* resolveAM2Shift(Register Rn, Register Rs, Register Rm, int64_t AM2Shift, BasicBlock* BB);

  GlobalValue* getGlobalValueByOffset(int64_t MCInstOffset, uint64_t PCOffset, Type* Ty);
  Value* ARMCCToValue(int Cond, BasicBlock* BB);

  BasicBlock* createBasicBlock(MachineBasicBlock* MBB);
  std::vector<BasicBlock* > getBasicBlocks(MachineBasicBlock* MBB);

  bool raiseMachineInstr(MachineInstr* MI);

private:
  MCInstRaiser* MCIR;

  bool raiseADCri(MachineInstr* MI);
  bool raiseADDri(MachineInstr* MI);
  bool raiseADDrr(MachineInstr* MI);
  bool raiseADDrsi(MachineInstr* MI);
  bool raiseANDri(MachineInstr* MI);
  bool raiseANDrsi(MachineInstr* MI);
  bool raiseANDrr(MachineInstr* MI);
  bool raiseBFC(MachineInstr* MI);
  bool raiseBICri(MachineInstr* MI);
  bool raiseBL(MachineInstr* MI);
  bool raiseBL_pred(MachineInstr* MI);
  bool raiseBX_RET(MachineInstr* MI);
  bool raiseBcc(MachineInstr* MI);
  bool raiseCLZ(MachineInstr* MI);
  bool raiseCMNri(MachineInstr* MI);
  bool raiseCMPri(MachineInstr* MI);
  bool raiseCMPrr(MachineInstr* MI);
  bool raiseDMB(MachineInstr* MI);
  bool raiseEORrr(MachineInstr* MI);
  bool raiseFMSTAT(MachineInstr* MI);
  bool raiseFCONSTD(MachineInstr* MI);
  bool raiseFCONSTS(MachineInstr* MI);
  bool raiseHINT(MachineInstr* MI);
  bool raiseLDMIA(MachineInstr* MI);
  bool raiseLDMIA_UPD(MachineInstr* MI);
  bool raiseLDRB_PRE_REG(MachineInstr* MI);
  bool raiseLDRBi12(MachineInstr* MI);
  bool raiseLDREX(MachineInstr* MI);
  bool raiseLDRH(MachineInstr* MI);
  bool raiseLDR_POST_IMM(MachineInstr* MI);
  bool raiseLDR_POST_REG(MachineInstr* MI);
  bool raiseLDRi12(MachineInstr* MI);
  bool raiseLDRrs(MachineInstr* MI);
  bool raiseMLA(MachineInstr* MI);
  bool raiseMOVTi16(MachineInstr* MI);
  bool raiseMOVi(MachineInstr* MI);
  bool raiseMOVi16(MachineInstr* MI);
  bool raiseMOVr(MachineInstr* MI);
  bool raiseMOVsi(MachineInstr* MI);
  bool raiseMUL(MachineInstr* MI);
  bool raiseMVNi(MachineInstr* MI);
  bool raiseMVNr(MachineInstr* MI);
  bool raiseORRri(MachineInstr* MI);
  bool raiseORRrr(MachineInstr* MI);
  bool raiseRSBri(MachineInstr* MI);
  bool raiseSBCrsi(MachineInstr* MI);
  bool raiseSMMUL(MachineInstr* MI);
  bool raiseSMULL(MachineInstr* MI);
  bool raiseSTMDB_UPD(MachineInstr* MI);
  bool raiseSTMIA(MachineInstr* MI);
  bool raiseSTMIB(MachineInstr* MI);
  bool raiseSTRB_POST_IMM(MachineInstr* MI);
  bool raiseSTRBi12(MachineInstr* MI);
  bool raiseSTREX(MachineInstr* MI);
  bool raiseSTRH(MachineInstr* MI);
  bool raiseSTRi12(MachineInstr* MI);
  bool raiseSTRrs(MachineInstr* MI);
  bool raiseSUBri(MachineInstr* MI);
  bool raiseSUBrr(MachineInstr* MI);
  bool raiseSUBrsi(MachineInstr* MI);
  bool raiseTEQrr(MachineInstr* MI);
  bool raiseTSTri(MachineInstr* MI);
  bool raiseTSTrr(MachineInstr* MI);
  bool raiseVABSD(MachineInstr* MI);
  bool raiseVADDD(MachineInstr* MI);
  bool raiseVADDS(MachineInstr* MI);
  bool raiseVCMPD(MachineInstr* MI);
  bool raiseVCMPZD(MachineInstr* MI);
  bool raiseVDIVD(MachineInstr* MI);
  bool raiseVLDMDIA_UPD(MachineInstr* MI);
  bool raiseVLDRD(MachineInstr* MI);
  bool raiseVLDRS(MachineInstr* MI);
  bool raiseVMLAD(MachineInstr* MI);
  bool raiseVMOVD(MachineInstr* MI);
  bool raiseVMOVDRR(MachineInstr* MI);
  bool raiseVMOVRRD(MachineInstr* MI);
  bool raiseVMOVRS(MachineInstr* MI);
  bool raiseVMOVSR(MachineInstr* MI);
  bool raiseVMOVv2i32(MachineInstr* MI);
  bool raiseVMULD(MachineInstr* MI);
  bool raiseVNEGD(MachineInstr* MI);
  bool raiseVORRd(MachineInstr* MI);
  bool raiseVSITOD(MachineInstr* MI);
  bool raiseVSTMDDB_UPD(MachineInstr* MI);
  bool raiseVSTMDIA_UPD(MachineInstr* MI);
  bool raiseVSTRD(MachineInstr* MI);
  bool raiseVSUBD(MachineInstr* MI);
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMLINEARRAISERPASS_H
