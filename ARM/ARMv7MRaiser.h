
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMV7MRAISER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMV7MRAISER_H

#include <iostream>
#include <vector>

#include "llvm/Object/ObjectFile.h"

#include "model/A32Disassembler.h"
#include "model/A32Instruction.h"
#include "model/T32Disassembler.h"
#include "model/T32Instruction.h"

class ARMv7MRaiser {

  private:
    llvm::object::ObjectFile* objectFile;

  public:

    ARMv7MRaiser(llvm::object::ObjectFile* objectFile) {
      this->objectFile = objectFile;
    }

    void disassemble() {
      A32Disassembler* A32disassembler = new A32Disassembler();
      T32Disassembler* T32disassembler = new T32Disassembler();
      for (uint64_t address = 0; address < objectFile->getData().size()-3; ++address) {
        const uint8_t* instruction = reinterpret_cast<const uint8_t*>(objectFile->getData().data())+address;

        A32Instruction* A32instruction = A32disassembler->disassemble(address, instruction);
        T32Instruction* T32instruction = T32disassembler->disassemble(address, instruction);

        if (A32instruction != nullptr)
          std::cout << *A32instruction << std::endl;
        else
          std::cout << "INVALID" << std::endl;

        if (T32instruction != nullptr)
          std::cout << *T32instruction << std::endl;
        else
          std::cout << "INVALID" << std::endl;
      }
    }
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMV7MRAISER_H
