
#--- Disassembler ---#
def generate_disasm(tree, name):
  with open(f"{name}.cpp", 'w') as f:
    f.write(f"""
/** This file is generated, do not edit **/

#include <cstdio>
#include <cstdint>
#include <map>
#include <string>
#include <vector>

class {name} {{
  public:
    class Instruction {{
      public:
        uint64_t address;
        uint8_t* data;

        // Debug
        std::string mnemonic;
        std::string constraints;
        std::string pseudocode;
        std::map<std::string, uint64_t> fields;
    }};

  private:
    uint8_t* data;
    uint64_t size;
    std::vector<Instruction> instructions;
    std::map<uint64_t, Instruction*> address_map;

  public:

    {name}(uint8_t* data, uint64_t size) {{
      this->data = data;
      this->size = size;
    }}

    void add_instruction(Instruction instruction) {{
      instructions.push_back(instruction);
      address_map[instruction.address] = &instructions.back();
    }}

    Instruction* get_instruction(uint64_t address) {{
      auto it = address_map.find(address);
      if (it == address_map.end()) {{
        return nullptr;
      }}
      return it->second;
    }}

    Instruction* disassemble(uint64_t address) {{
      Instruction* instruction = new Instruction();\n""")

    generate_match(tree, f, '      ')

    f.write(f"    }}\n"
           +f"}};\n")

def generate_match(tree, f, indent = '', debug = False):
  if 'instructions' in tree:
    for instruction in tree['instructions']:
      if len(instruction['constraints']) > 0:
        f.write(f"{indent}// if {instruction['constraints']}\n")
      f.write(f"{indent}instruction->address = address;\n")
      f.write(f"{indent}instruction->data = data;\n")
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

      f.write(f"{indent}add_instruction(*instruction);\n"
             +f"{indent}return instruction;\n")
    return

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
def generate_raiser(instructions, name):
  with open(f"{name}.cpp", 'w') as fcpp:
    with open(f"{name}.h", 'w') as fh:
      fcpp.write(f"""/** This file is generated, do not edit **/\n""")
      fh.write  (f"""/** This file is generated, do not edit **/\n""")

      fcpp.write(f"""#include "{name}.h"\n"""
                +f"""\n""")
      fh.write  (f"""#ifndef LLVM_MCTOLL_{name.upper()}_H\n"""
                +f"""#define LLVM_MCTOLL_{name.upper()}_H\n"""
                +f"""\n"""
                +f"""#include "llvm/IR/Instruction.h"\n"""
                +f"""\n"""
                +f"""using namespace llvm;\n"""
                +f"""\n"""
                +f"""class {name} {{\n"""
                +f"""  public:\n""")

      for instruction in instructions:
        fh.write  (f"""    Instruction* raise_{('_'.join(instruction['mnemonics']).replace('{', '').replace('}', ''))} (uint8_t* data, uint64_t address);\n""")
        fcpp.write(f"""Instruction* {name}::raise_{('_'.join(instruction['mnemonics']).replace('{', '').replace('}', ''))} (uint8_t* data, uint64_t address) {{\n"""
                  +f"""/**\n""")

        pattern = ''
        for part in instruction['pattern']:
          if 'contents' in part:
            pattern += part['contents']
          else:
            pattern += '-' * int(part['width'])
        fcpp.write(f"""  // {pattern}\n""")

        fcpp.write(f"""/**\n"""
                  +f"""{instruction['pseudocode'][0]}\n"""
                  +f"""{instruction['pseudocode'][1]}\n"""
                  +f"""**/\n"""
                  +f"""  return nullptr;\n"""
                  +f"""}}\n""")

      fh.write  (f"""}};\n"""
                +f"""\n"""
                +f"""#endif // LLVM_MCTOLL_{name.upper()}_H\n""")
