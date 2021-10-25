
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMDISASSEMBLER_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMDISASSEMBLER_H

#include <cstdio>
#include <cstdint>
#include <string>

#include "ARMInstruction.h"

class ARMDisassembler {
  private:
    uint8_t* Data;
    uint64_t Size;

  public:
    ARMDisassembler(uint8_t* Data, uint64_t Size) : Data(Data), Size(Size) {

    };
    ~ARMDisassembler();

    ARMInstruction* disassembleInstruction(uint32_t instruction);
    bool match(std::string pattern, uint32_t instruction);

    void dump();
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMDISASSEMBLER_H
