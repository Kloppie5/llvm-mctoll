//===- DAGRaisingInfo.cpp - Binary raiser utility llvm-mctoll -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of DAGRaisingInfo class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "DAGRaisingInfo.h"

using namespace llvm;

DAGRaisingInfo::DAGRaisingInfo(SelectionDAG &dag) : DAG(dag) {}

void DAGRaisingInfo::clear() {
  for (auto &elmt : NPMap)
    delete elmt.second;

  NPMap.clear();
}
