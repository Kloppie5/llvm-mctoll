
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMV7MRAISER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMV7MRAISER_H

#include <iostream>
#include <vector>

#include "llvm/Object/ObjectFile.h"

#include "model/A32Disassembler.h"
#include "model/A32Instruction.h"
#include "model/T32Disassembler.h"
#include "model/T32Instruction.h"

/**
 *
 * General strategy:
 * - Disassemble the possible instructions in the entire file
 * - Locate instructions that cause control-flow (modify the program counter)
 * - Remove instructions that don't reach control-flow instructions
 * - Group instructions that are executed together
 *
*/
class ARMv8Raiser {

  private:
    llvm::object::ObjectFile* objectFile;

  public:

    ARMv8Raiser(llvm::object::ObjectFile* objectFile) {
      this->objectFile = objectFile;
    }

    void disassemble();
    void findTerminators();
    void removeUnreachable();
    void groupInstructions();
    void print();

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMV7MRAISER_H
