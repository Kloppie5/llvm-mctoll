
#include "ARMv8Raiser.h"

void ARMv8Raiser::disassemble() {
  A32Disassembler* disasm = new A32Disassembler();
  disasm->disassemble(objectFile->getData().data(), objectFile->getData().size());

  for (uint64_t address = 0; address < objectFile->getData().size()-3; address += 4) {
    A32Instruction* instr = disasm->getInstruction(address);
    if (instr)
      if (instr->address != 0)
        std::cout << *instr << std::endl;
  }
}
