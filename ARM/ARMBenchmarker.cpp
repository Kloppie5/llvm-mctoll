
#include "ARMBenchmarker.h"
#include "Monitor.h"

#include "ARMBaseInstrInfo.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

bool ARMBenchmarker::run(MachineFunction *MF, Function *F) {
  Monitor::event_start("ARMBenchmarker");

  std::map<unsigned int, std::vector<MachineInstr *>> Benchmark;

  for (MachineBasicBlock &MBB : *MF)
    for (MachineInstr &MI : MBB)
      Benchmark[MI.getOpcode()].push_back(&MI);

  for (std::pair<unsigned int, std::vector<MachineInstr *>> opcode : Benchmark) {
    Monitor::event_raw() << "Encountered " << opcode.second.size() << " instructions with opcode " << opcode.first << ":\n";
    for (MachineInstr *MI : opcode.second)
      Monitor::printMachineInstr(MI);
  }
  
  Monitor::event_end("ARMBenchmarker");
  return true;
}

#undef DEBUG_TYPE
