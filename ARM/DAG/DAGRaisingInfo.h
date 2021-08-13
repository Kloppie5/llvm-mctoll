//===- DAGRaisingInfo.h -----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of DAGRaisingInfo class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_DAG_DAGRAISERINGINFO_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_DAG_DAGRAISERINGINFO_H

#include "ModuleRaiser.h"
#include "SelectionCommon.h"
#include "llvm/ADT/DenseMap.h"

/// This is an extention of SelectionDAG. It contains additional information
/// of DAG which is used by llvm-mctoll.
class DAGRaisingInfo {
public:
  DAGRaisingInfo() = delete;
  DAGRaisingInfo(SelectionDAG &dag);
  void clear();
  /// Gets corresponding SelectionDAG object.
  SelectionDAG &getCurDAG() { return DAG; }

  SelectionDAG &DAG;
  /// The map for each SDNode with its additional property.
  DenseMap<SDNode *, NodePropertyInfo *> NPMap;
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_DAG_DAGRAISERINGINFO_H
