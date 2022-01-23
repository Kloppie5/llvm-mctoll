//===- IREmitter.h ----------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of IREmitter class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_DAG_IREMITTER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_DAG_IREMITTER_H

#include "DAGRaisingInfo.h"
#include "FunctionRaisingInfo.h"
#include "ModuleRaiser.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/IRBuilder.h"

/// Construct an emitter and set it to start inserting IR Value into
/// the given block.
class IREmitter {
  Function *FT;
  BasicBlock *BB;
  BasicBlock *CurBB;
  DAGRaisingInfo *DAGInfo;
  SelectionDAG *DAG;
  LLVMContext *CTX;
  FunctionRaisingInfo *FuncInfo;
  const DataLayout *DLT;
  ARMModuleRaiser *MR;
  IRBuilder<> IRB;

  std::vector<JumpTableInfo> jtList;

public:
  IREmitter(BasicBlock *bb, DAGRaisingInfo *dagInfo,
            FunctionRaisingInfo *funcInfo);
  /// Generate SDNode code for a node and needed dependencies.
  void emitNode(SDNode *Node) {
    if(Node->isMachineOpcode())
      emitMachineOpcodeNode(Node);
    else if (Node->isTargetOpcode())
      emitTargetDependentNode(Node);
    else
      emitTargetIndependentNode(Node);
  }
  BasicBlock *getBlock() { return BB; }
  void setBlock(BasicBlock *bb) { BB = bb; }
  /// Return the current basic block.
  BasicBlock *getCurBlock() { return CurBB; }

  bool setjtList(std::vector<JumpTableInfo> &List) {
    jtList = List;
    return true;
  }

private:
  /// Generate SDNode code for a target-independent node.
  /// Emit SDNode to Instruction and add to BasicBlock.
  void emitMachineOpcodeNode ( SDNode *Node );
  void emitTargetDependentNode ( SDNode *Node );
  void emitTargetIndependentNode ( SDNode *Node );
  /// Emit SDNodes of binary operations.
  void emitBinary(SDNode *Node);
  void emitCondCode(unsigned CondValue, BasicBlock *BB, BasicBlock *IfBB,
                    BasicBlock *ElseBB);
  void emitBinaryCPSR(Value *Inst, BasicBlock *BB, unsigned Opcode,
                      SDNode *Node);
  /// Update the N Z C V flags of global variable.
  void emitCPSR(Value *Operand0, Value *Operand1, BasicBlock *BB,
                unsigned Flag);
  void emitSpecialCPSR(Value *Result, BasicBlock *BB, unsigned Flag);
  /// Create PHINode for value use selection when running.
  PHINode *createAndEmitPHINode(SDNode *Node, BasicBlock *BB, BasicBlock *IfBB,
                                BasicBlock *ElseBB, Instruction *IfInst);
  IntegerType *getDefaultType() {
    return Type::getIntNTy(*CTX, DLT->getPointerSizeInBits());
  }
  PointerType *getPointerType() {
    return Type::getIntNPtrTy(*CTX, DLT->getPointerSizeInBits());
  }
  Type *getIntTypeByPtr(Type *pty);
  Value *getIRValue(SDValue val);
  // Wrapper to call new  Create*Load APIs
  LoadInst *CallCreateAlignedLoad(Value *ValPtr,
                                  MaybeAlign Align = MaybeAlign()) {
    return IRB.CreateAlignedLoad(ValPtr->getType()->getPointerElementType(),
                                 ValPtr, Align, "");
  }
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_DAG_IREMITTER_H
