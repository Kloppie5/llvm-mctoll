
#ifndef LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMINSTRUCTION_H
#define LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMINSTRUCTION_H

#include <cstdint>
#include <cstring>

class ARMInstruction {
  public:
    // "static"
    std::string Mnemonic;
    std::string Pattern;
    // dynamic
    uint32_t Address;
    uint32_t Realization;

  public:
    ARMInstruction(std::string mnemonic, std::string pattern, uint32_t address, uint32_t realization) : Mnemonic(mnemonic), Pattern(pattern), Address(address), Realization(realization) {

    }
    ~ARMInstruction();

    void dump() {
      printf("%s\n", Mnemonic.c_str());
    }
};

#endif // LLVM_TOOLS_LLVM_MCTOLL_ARM_ARMINSTRUCTION_H
