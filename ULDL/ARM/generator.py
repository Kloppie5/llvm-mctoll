import re
import ASLToULDLVisitor

#--- Disassembler ---#
def generate_disasm(tree, isa):
  with open(f"{isa}Instruction.cpp", 'w') as fcpp:
    with open(f"{isa}Instruction.h", 'w') as fh:
      fcpp.write(f"""/** This file is generated, do not edit **/\n""")
      fh.write  (f"""/** This file is generated, do not edit **/\n""")

      fh.write  (f"""#ifndef LLVM_MCTOLL_{isa.upper()}INSTRUCTION_H\n"""
                +f"""#define LLVM_MCTOLL_{isa.upper()}INSTRUCTION_H\n"""
                +f"""\n"""
                +f"""#include <iostream>\n"""
                +f"""#include <map>\n"""
                +f"""#include <string>\n"""
                +f"""\n"""
                +f"""#include "ULDL/Operation.h"\n"""
                +f"""class {isa}Instruction {{\n"""
                +f"""  public:\n"""
                +f"""    uint64_t address;\n"""
                +f"""    const uint8_t* data;\n"""
                +f"""    Operation* operation;\n"""
                +f"""\n"""
                +f"""    std::string mnemonic;\n"""
                +f"""    std::string constraints;\n"""
                +f"""    std::map<std::string, uint64_t> fields;\n"""
                +f"""}};\n"""
                +f"""\n"""
                +f"""std::ostream& operator<<(std::ostream& os, const {isa}Instruction& instruction);\n"""
                +f"""\n"""
                +f"""#endif // LLVM_MCTOLL_{isa.upper()}INSTRUCTION_H\n""")

      fcpp.write(f"""#include "{isa}Instruction.h"\n"""
                +f"""\n"""
                +f"""std::ostream& operator<<(std::ostream& os, const {isa}Instruction& instruction) {{\n"""
                +f"""  os << "0x" << std::hex << instruction.address << ": " << instruction.mnemonic << " ";\n"""
                +f"""  for (auto& field : instruction.fields) {{\n"""
                +f"""    os << field.first << "=" << std::hex << field.second << " ";\n"""
                +f"""  }}\n"""
                +f"""  return os;\n"""
                +f"""}}\n"""
                +f"""\n""")

  with open(f"{isa}Disassembler.cpp", 'w') as fcpp:
    with open(f"{isa}Disassembler.h", 'w') as fh:
      fcpp.write(f"""/** This file is generated, do not edit **/\n""")
      fh.write  (f"""/** This file is generated, do not edit **/\n""")

      fh.write  (f"""#include <cstdio>\n"""
                +f"""#include <cstdint>\n"""
                +f"""#include <map>\n"""
                +f"""#include <string>\n"""
                +f"""#include <vector>\n"""
                +f"""\n"""
                +f"""#include "{isa}Instruction.h"\n"""
                +f"""\n"""
                +f"""class {isa}Disassembler {{\n"""
                +f"""  \n"""
                +f"""  private:\n"""
                +f"""    std::map<uint64_t, {isa}Instruction> instructions;\n"""
                +f"""  \n"""
                +f"""  public:\n"""
                +f"""    {isa}Disassembler();\n"""
                +f"""    void addInstruction({isa}Instruction instruction);\n"""
                +f"""    {isa}Instruction* getInstruction(uint64_t address);\n"""
                +f"""\n"""
                +f"""    void disassemble(const char* data, size_t size);\n"""
                +f"""    {isa}Instruction* disassemble(const uint64_t address, const uint8_t* data);\n"""
                +f"""}};\n""")

      fcpp.write(f"""#include "{isa}Disassembler.h"\n"""
                +f"""\n"""
                +f"""{isa}Disassembler::{isa}Disassembler() {{\n"""
                +f"""}}\n"""
                +f"""\n"""
                +f"""void {isa}Disassembler::addInstruction({isa}Instruction instruction) {{\n"""
                +f"""  instructions[instruction.address] = instruction;\n"""
                +f"""}}\n"""
                +f"""\n"""
                +f"""{isa}Instruction* {isa}Disassembler::getInstruction(uint64_t address) {{\n"""
                +f"""  return &instructions[address];\n"""
                +f"""}}\n"""
                +f"""void {isa}Disassembler::disassemble(const char* data, uint64_t size) {{\n"""
                +f"""  for (uint64_t i = 0; i < size; i += 4) {{\n"""
                +f"""    /** {isa}Instruction* instruction = */ disassemble(i, (const uint8_t*)data + i);\n"""
                +f"""    // if (instruction)\n"""
                +f"""    //   std::cout << *instruction << std::endl;\n"""
                +f"""  }}\n"""
                +f"""}}\n"""
                +f"""\n"""
                +f"""{isa}Instruction* {isa}Disassembler::disassemble(const uint64_t address, const uint8_t* data) {{\n"""
                +f"""  {isa}Instruction* instruction = new {isa}Instruction();\n"""
                +f"""  if (instructions.find(address) != instructions.end()) {{\n"""
                +f"""    instruction = &instructions[address];\n"""
                +f"""    return instruction;\n"""
                +f"""  }}\n"""
                +f"""  \n"""
                +f"""  instruction->address = address;\n"""
                +f"""  instruction->data = data;\n""")

      generate_match(tree, fcpp, '  ')

      fcpp.write(f"""  return nullptr;\n"""
                +f"""}}\n""")

def generate_match(tree, f, indent = '', debug = False):
  if 'instructions' in tree:
    for instruction in tree['instructions']:
      if len(instruction['constraints']) > 0:
        f.write(f"{indent}// if {instruction['constraints']}\n")
      f.write(f"{indent}instruction->mnemonic = \"{('_'.join(instruction['mnemonics'])).replace('{', '').replace('}', '')}\";\n")
      f.write(f"{indent}instruction->constraints = \"{', '.join(instruction['constraints'])}\";\n")
      f.write(f"{indent}uint32_t BE = data[address + 3] << 24 | data[address + 2] << 16 | data[address + 1] << 8 | data[address];\n""")
      f.write(f"{indent}instruction->fields = {{\n")
      for part in instruction['pattern']:
        if 'contents' not in part or 'x' in part['contents']:
          f.write(f"{indent}  {{\"{part['name']}\", BE >> {int(part['hibit']) - int(part['width']) + 1} & 0b{'1' * int(part['width'])}}},\n")
        else:
          f.write(f"{indent}  {{\"{part['name']}\", 0b{part['contents'].replace('(', '').replace(')', '')}}},\n")
      f.write(f"{indent}}};\n")

      f.write(f"{indent}addInstruction(*instruction);\n"
             +f"{indent}return instruction;\n")
    return
  # else

  patterns = list(tree.keys())
  patterns.sort(reverse = True)
  for pattern in patterns:
    byte = '--------'
    index = -1
    for i in range(0, len(pattern)//8):
      if pattern[i*8:i*8+8] != '--------':
        byte = pattern[i*8:i*8+8]
        index = i
        break
    if debug:
      print(f"Matching {byte} at index {index}")
    byte_mask = byte.replace('0', '1').replace('-', '0').replace('x', '0')
    byte_value = byte.replace('-', '0').replace('x', '0')

    f.write(f"{indent}if ((data[address + {index}] & 0b{byte_mask}) == 0b{byte_value}) {{ // {pattern}\n")

    generate_match(tree[pattern], f, indent + '  ')

    f.write(f"{indent}}}\n")

#--- Raiser ---#
def generate_raiser(instructions, isa):
  with open(f"{isa}Raiser.cpp", 'w') as fcpp:
    with open(f"{isa}Raiser.h", 'w') as fh:
      fcpp.write(f"""/** This file is generated, do not edit **/\n""")
      fh.write  (f"""/** This file is generated, do not edit **/\n""")

      fcpp.write(f"""#include "{isa}Raiser.h"\n"""
                +f"""\n""")
      fh.write  (f"""#ifndef LLVM_MCTOLL_{isa.upper()}RAISER_H\n"""
                +f"""#define LLVM_MCTOLL_{isa.upper()}RAISER_H\n"""
                +f"""\n"""
                +f"""#include "llvm/IR/Instruction.h"\n"""
                +f"""\n"""
                +f"""using namespace llvm;\n"""
                +f"""\n"""
                +f"""class {isa}Raiser {{\n"""
                +f"""  public:\n""")

      counter = 0
      failed = []
      for instruction in instructions:
        counter += 1
        identifier = []
        for pattern in instruction['pattern']:
          if 'contents' in pattern:
            identifier.append(pattern['contents'])
          else:
            identifier.append(f"{pattern['name']}")
        identifier = re.sub('[\{\}\(\)<>:]', '', 'I_'+'_'.join(identifier))
        mnemonics = re.sub('[\{\}\(\)<>:]', '', '_'.join(instruction['mnemonics']))
        print(f"{counter}/{len(instructions)}: {mnemonics} | {identifier}")
        fh.write  (f"""    Instruction* raise_{identifier} (uint8_t* data, uint64_t address);\n""")
        fcpp.write(f"""Instruction* {isa}Raiser::raise_{identifier} (uint8_t* data, uint64_t address) {{\n"""
                  +f"""/**\n""")

        errors = ASLToULDLVisitor.ASLToULDLVisitor().generateULDL(instruction['pseudocode'], instruction['pattern'])
        if errors != 0:
          print(f"{isa}Raiser: Pseudocode for {mnemonics} is invalid")
          for line in instruction['pseudocode'].split('\n'):
            print(f"\"{line}\"")
          failed.append(instruction)

        errors = ASLToULDLVisitor.ASLToULDLVisitor().generateULDL(instruction['pseudocode_decode'], instruction['pattern'])
        if errors != 0:
          print(f"{isa}Raiser: Pseudocode for {mnemonics} decode is invalid")
          for line in instruction['pseudocode_decode'].split('\n'):
            print(f"\"{line}\"")
          failed.append(instruction)

        pattern = ''
        for part in instruction['pattern']:
          if 'contents' in part:
            pattern += part['contents']
          else:
            pattern += '-' * int(part['width'])
        fcpp.write(f"""  // {pattern}\n""")




        fcpp.write(f"""/**\n"""
                  +f"""{instruction['pseudocode']}\n"""
                  +f"""**/\n"""
                  +f"""  return nullptr;\n"""
                  +f"""}}\n""")

      print(f"{isa}Raiser: {len(failed)} instructions failed to be generated")
      for instruction in failed:
        print(f"{('_'.join(instruction['mnemonics']).replace('{', '').replace('}', ''))}")
      if len(failed) > 0:
        exit(1)

      fh.write  (f"""}};\n"""
                +f"""\n"""
                +f"""#endif // LLVM_MCTOLL_{isa.upper()}RAISER_H\n""")
