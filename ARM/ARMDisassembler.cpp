
#include "ARMDisassembler.h"

#include "ARMInstruction.h"

ARMInstruction* ARMDisassembler::disassembleInstruction(uint32_t instruction) {
  if (match("CCCC 000 0000 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("AND<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0000 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0001 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("EOR<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0001 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0010 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("SUB<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0010 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0011 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("RSB<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0011 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0100 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("ADD<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0100 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0101 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("ADC<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0101 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0110 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("SBC<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0110 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 0111 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("RSC<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 0111 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1000 1 nnnn 0000 sssss tt 0 mmmm",     instruction)) return new ARMInstruction("TST<CC><S> Rn, Rm OP #",            "CCCC 000 1000 1 nnnn 0000 sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1001 1 nnnn 0000 sssss tt 0 mmmm",     instruction)) return new ARMInstruction("TEQ<CC><S> Rn, Rm OP #",            "CCCC 000 1001 1 nnnn 0000 sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1010 1 nnnn 0000 sssss tt 0 mmmm",     instruction)) return new ARMInstruction("CMP<CC><S> Rn, Rm OP #",            "CCCC 000 1010 1 nnnn 0000 sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1011 1 nnnn 0000 sssss tt 0 mmmm",     instruction)) return new ARMInstruction("CMN<CC><S> Rn, Rm OP #",            "CCCC 000 1011 1 nnnn 0000 sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1100 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("ORR<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 1100 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1101 S 0000 dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("MOV<CC><S> Rd, Rm OP #",            "CCCC 000 1101 S 0000 dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1110 S nnnn dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("BIC<CC><S> Rd, Rn, Rm OP #",        "CCCC 000 1110 S nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 000 1111 S 0000 dddd sssss tt 0 mmmm",     instruction)) return new ARMInstruction("MVN<CC><S> Rd, Rm OP #",            "CCCC 000 1111 S 0000 dddd sssss tt 0 mmmm", 0, instruction);

  if (match("CCCC 000 0000 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("AND<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0000 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0001 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("EOR<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0001 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0010 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("SUB<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0010 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0011 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("RSB<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0011 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0100 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("ADD<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0100 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0101 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("ADC<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0101 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0110 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("SBC<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0110 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 0111 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("RSC<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 0111 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1000 1 nnnn 0000 ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("TST<CC><S> Rn, Rm OP Rs",           "CCCC 000 1000 1 nnnn 0000 ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1001 1 nnnn 0000 ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("TEQ<CC><S> Rn, Rm OP Rs",           "CCCC 000 1001 1 nnnn 0000 ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1010 1 nnnn 0000 ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("CMP<CC><S> Rn, Rm OP Rs",           "CCCC 000 1001 1 nnnn 0000 ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1011 1 nnnn 0000 ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("CMN<CC><S> Rn, Rm OP Rs",           "CCCC 000 1011 1 nnnn 0000 ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1100 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("ORR<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 1100 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1101 S 0000 dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("MOV<CC><S> Rd, Rm OP Rs",           "CCCC 000 1101 S 0000 dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1110 S nnnn dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("BIC<CC><S> Rd, Rn, Rm OP Rs",       "CCCC 000 1110 S nnnn dddd ssss 0 tt 1 mmmm", 0, instruction);
  if (match("CCCC 000 1111 S 0000 dddd ssss 0 tt 1 mmmm",    instruction)) return new ARMInstruction("MVN<CC><S> Rd, Rm OP Rs",           "CCCC 000 1111 S 0000 dddd ssss 0 tt 1 mmmm", 0, instruction);

  if (match("CCCC 001 0000 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("AND<CC><S> Rd, Rn, #",              "CCCC 001 0000 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0001 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("EOR<CC><S> Rd, Rn, #",              "CCCC 001 0001 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0010 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("SUB<CC><S> Rd, Rn, #",              "CCCC 001 0010 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0011 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("RSB<CC><S> Rd, Rn, #",              "CCCC 001 0011 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0100 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("ADD<CC><S> Rd, Rn, #",              "CCCC 001 0100 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0101 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("ADC<CC><S> Rd, Rn, #",              "CCCC 001 0101 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0110 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("SBC<CC><S> Rd, Rn, #",              "CCCC 001 0110 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 0111 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("RSC<CC><S> Rd, Rn, #",              "CCCC 001 0111 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1000 1 nnnn 0000 rrrr aaaaaaaa",       instruction)) return new ARMInstruction("TST<CC><S> Rn, #",                  "CCCC 001 1000 1 nnnn 0000 rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1001 1 nnnn 0000 rrrr aaaaaaaa",       instruction)) return new ARMInstruction("TEQ<CC><S> Rn, #",                  "CCCC 001 1001 1 nnnn 0000 rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1001 0 ffff 1111 rrrr aaaaaaaa",       instruction)) return new ARMInstruction("MSR<CC> CPSR_ffff, #",              "CCCC 001 1001 0 ffff 1111 rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1010 1 nnnn 0000 rrrr aaaaaaaa",       instruction)) return new ARMInstruction("CMP<CC><S> Rn, #",                  "CCCC 001 1010 1 nnnn 0000 rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1011 1 nnnn 0000 rrrr aaaaaaaa",       instruction)) return new ARMInstruction("CMN<CC><S> Rn, #",                  "CCCC 001 1011 1 nnnn 0000 rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1011 0 ffff 1111 rrrr aaaaaaaa",       instruction)) return new ARMInstruction("MSR<CC> SPSR_ffff, #",              "CCCC 001 1011 0 ffff 1111 rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1100 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("ORR<CC><S> Rd, Rn, #",              "CCCC 001 1100 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1101 S 0000 dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("MOV<CC><S> Rd #",                   "CCCC 001 1101 S 0000 dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1110 S nnnn dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("BIC<CC><S> Rd, Rn, #",              "CCCC 001 1110 S nnnn dddd rrrr aaaaaaaa", 0, instruction);
  if (match("CCCC 001 1111 S 0000 dddd rrrr aaaaaaaa",       instruction)) return new ARMInstruction("MVN<CC><S> Rd, #",                  "CCCC 001 1111 S 0000 dddd rrrr aaaaaaaa", 0, instruction);

  if (match("CCCC 000 1000 0 1111 dddd 0000 0 00 0 0000",    instruction)) return new ARMInstruction("MRS<CC> Rd, CPSR",                  "CCCC 000 1000 0 1111 dddd 0000 0 00 0 0000", 0, instruction);
  if (match("CCCC 000 1000 0 nnnn dddd 0000 1 00 1 mmmm",    instruction)) return new ARMInstruction("SWP<CC> Rd, Rm, [Rn]",              "CCCC 000 1000 0 nnnn dddd 0000 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 1001 0 ffff 1111 0000 0 00 0 mmmm",    instruction)) return new ARMInstruction("MSR<CC> CPSR_ffff, Rm",             "CCCC 000 1001 0 ffff 1111 0000 0 00 0 mmmm", 0, instruction);
  if (match("CCCC 000 1001 0 1111 1111 1111 0 00 1 mmmm",    instruction)) return new ARMInstruction("BX<CC> Rm",                         "CCCC 000 1001 0 1111 1111 1111 0 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 1010 0 1111 dddd 0000 0 00 0 0000",    instruction)) return new ARMInstruction("MRS<CC> Rd, SPSR",                  "CCCC 000 1010 0 1111 dddd 0000 0 00 0 0000", 0, instruction);
  if (match("CCCC 000 1010 0 nnnn dddd 0000 1 00 1 mmmm",    instruction)) return new ARMInstruction("SWP<CC>B Rd, Rm, [Rn]",             "CCCC 000 1010 0 nnnn dddd 0000 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 1011 0 ffff 1111 0000 0 00 0 mmmm",    instruction)) return new ARMInstruction("MSR<CC> SPSR_ffff, Rm",             "CCCC 000 1011 0 ffff 1111 0000 0 00 0 mmmm", 0, instruction);

  if (match("CCCC 000 0000 S nnnn 0000 ssss 1 00 1 mmmm",    instruction)) return new ARMInstruction("MUL<CC><S> Rn, Rm, Rs",             "CCCC 000 0000 S nnnn 0000 ssss 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 0001 S nnnn dddd ssss 1 00 1 mmmm",    instruction)) return new ARMInstruction("MLA<CC><S> Rn, Rm, Rs, Rd",         "CCCC 000 0001 S nnnn dddd ssss 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 0100 S nnnn dddd ssss 1 00 1 mmmm",    instruction)) return new ARMInstruction("UMULL<CC><S> Rd, Rn, Rm, Rs",       "CCCC 000 0100 S nnnn dddd ssss 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 0101 S nnnn dddd ssss 1 00 1 mmmm",    instruction)) return new ARMInstruction("UMLAL<CC><S> Rd, Rn, Rm, Rs",       "CCCC 000 0101 S nnnn dddd ssss 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 0110 S nnnn dddd ssss 1 00 1 mmmm",    instruction)) return new ARMInstruction("SMULL<CC><S> Rd, Rn, Rm, Rs",       "CCCC 000 0110 S nnnn dddd ssss 1 00 1 mmmm", 0, instruction);
  if (match("CCCC 000 0111 S nnnn dddd ssss 1 00 1 mmmm",    instruction)) return new ARMInstruction("SMLAL<CC><S> Rd, Rn, Rm, Rs",       "CCCC 000 0111 S nnnn dddd ssss 1 00 1 mmmm", 0, instruction);

  if (match("CCCC 000 P U I W 0 nnnn dddd aaaa 1 01 1 aaaa", instruction)) return new ARMInstruction("STR<CC>H Rd, address",              "CCCC 000 P U I W 0 nnnn dddd aaaa 1 01 1 aaaa", 0, instruction);
  if (match("CCCC 000 P U I W 1 nnnn dddd aaaa 1 01 1 aaaa", instruction)) return new ARMInstruction("LDR<CC>H Rd, address",              "CCCC 000 P U I W 1 nnnn dddd aaaa 1 01 1 aaaa", 0, instruction);
  if (match("CCCC 000 P U I W 1 nnnn dddd aaaa 1 10 1 aaaa", instruction)) return new ARMInstruction("LDR<CC>SB Rd, address",             "CCCC 000 P U I W 1 nnnn dddd aaaa 1 10 1 aaaa", 0, instruction);
  if (match("CCCC 000 P U I W 1 nnnn dddd aaaa 1 11 1 aaaa", instruction)) return new ARMInstruction("LDR<CC>SH Rd, address",             "CCCC 000 P U I W 1 nnnn dddd aaaa 1 11 1 aaaa", 0, instruction);

  if (match("CCCC 010 PU0W0 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("STR<CC> Rd, Rn, #",                 "CCCC 010 PU0W0 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 PU0W1 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("LDR<CC> Rd, Rn, #",                 "CCCC 010 PU0W1 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 PU1W0 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("STR<CC>B Rd, Rn, #",                "CCCC 010 PU1W0 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 PU1W1 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("LDR<CC>B Rd, Rn, #",                "CCCC 010 PU1W1 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 0U010 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("STR<CC>T Rd, Rn, #",                "CCCC 010 0U010 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 0U011 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("LDR<CC>T Rd, Rn, #",                "CCCC 010 0U011 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 0U110 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("STR<CC>BT Rd, Rn, #",               "CCCC 010 0U110 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 010 0U111 nnnn dddd aaaaaaaaaaaa",         instruction)) return new ARMInstruction("LDR<CC>BT Rd, Rn, #",               "CCCC 010 0U111 nnnn dddd aaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 011 P U 0 W 0 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("STR<CC> Rd, Rn, Rm OP #",           "CCCC 011 P U 0 W 0 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 P U 0 W 1 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("LDR<CC> Rd, Rn, Rm OP #",           "CCCC 011 P U 0 W 1 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 P U 1 W 0 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("STR<CC>B Rd, Rn, Rm OP #",          "CCCC 011 P U 1 W 0 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 P U 1 W 1 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("LDR<CC>B Rd, Rn, Rm OP #",          "CCCC 011 P U 1 W 1 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 0 U 0 1 0 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("STR<CC>T Rd, Rn, Rm OP #",          "CCCC 011 0 U 0 1 0 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 0 U 0 1 1 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("LDR<CC>T Rd, Rn, Rm OP #",          "CCCC 011 0 U 0 1 1 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 0 U 1 1 0 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("STR<CC>BT Rd, Rn, Rm OP #",         "CCCC 011 0 U 1 1 0 nnnn dddd sssss tt 0 mmmm", 0, instruction);
  if (match("CCCC 011 0 U 1 1 1 nnnn dddd sssss tt 0 mmmm",  instruction)) return new ARMInstruction("LDR<CC>BT Rd, Rn, Rm OP #",         "CCCC 011 0 U 1 1 1 nnnn dddd sssss tt 0 mmmm", 0, instruction);

  if (match("CCCC 100 P U 0 W 0 nnnn rrrrrrrrrrrrrrrr",      instruction)) return new ARMInstruction("STM<CC><AM> Rm<!>, regs",           "CCCC 100 P U 0 W 0 nnnn rrrrrrrrrrrrrrrr", 0, instruction);
  if (match("CCCC 100 P U 0 W 1 nnnn rrrrrrrrrrrrrrrr",      instruction)) return new ARMInstruction("LDM<CC><AM> Rm<!>, regs",           "CCCC 100 P U 0 W 1 nnnn rrrrrrrrrrrrrrrr", 0, instruction);
  if (match("CCCC 100 P U 1 0 0 nnnn 0 rrrrrrrrrrrrrrr",     instruction)) return new ARMInstruction("STM<CC><AM> Rm, regs^",             "CCCC 100 P U 1 0 0 nnnn 0 rrrrrrrrrrrrrrr", 0, instruction);
  if (match("CCCC 100 P U 1 1 0 nnnn 0 rrrrrrrrrrrrrrr",     instruction)) return new ARMInstruction("LDM<CC><AM> Rm, regs^",             "CCCC 100 P U 1 1 0 nnnn 0 rrrrrrrrrrrrrrr", 0, instruction);
  if (match("CCCC 100 P U 1 W 0 nnnn 1 rrrrrrrrrrrrrrr",     instruction)) return new ARMInstruction("STM<CC><AM> Rm<!>, regs^",          "CCCC 100 P U 1 W 0 nnnn 1 rrrrrrrrrrrrrrr", 0, instruction);
  if (match("CCCC 100 P U 1 W 0 nnnn 1 rrrrrrrrrrrrrrr",     instruction)) return new ARMInstruction("LDM<CC><AM> Rm<!>, regs^",          "CCCC 100 P U 1 W 0 nnnn 1 rrrrrrrrrrrrrrr", 0, instruction);

  if (match("CCCC 101 0 aaaaaaaaaaaaaaaaaaaaaaaa",           instruction)) return new ARMInstruction("B<CC> address",                     "CCCC 101 0 aaaaaaaaaaaaaaaaaaaaaaaa", 0, instruction);
  if (match("CCCC 101 1 aaaaaaaaaaaaaaaaaaaaaaaa",           instruction)) return new ARMInstruction("BL<CC> address",                    "CCCC 101 1 aaaaaaaaaaaaaaaaaaaaaaaa", 0, instruction);

  if (match("CCCC 110 P U N W 0 nnnn dddd pppp aaaaaaaa",    instruction)) return new ARMInstruction("STC<CC> p<p>, CRd, #",              "CCCC 110 P U N W 0 nnnn dddd pppp aaaaaaaa", 0, instruction);
  if (match("CCCC 110 P U N W 1 nnnn dddd pppp aaaaaaaa",    instruction)) return new ARMInstruction("LDC<CC> p<p>, CRd, #",              "CCCC 110 P U N W 1 nnnn dddd pppp aaaaaaaa", 0, instruction);

  if (match("CCCC 111 0 pppp nnnn dddd iiii sss 0 mmmm",     instruction)) return new ARMInstruction("CDP<CC> p<i>, p, CRd, CRn, CRm, s", "CCCC 111 0 pppp nnnn dddd iiii sss 0 mmmm", 0, instruction);
  if (match("CCCC 111 0 ppp 0 nnnn dddd iiii sss 1 mmmm",    instruction)) return new ARMInstruction("MCR<CC> p<i>, p, Rd, Crn, CRm, s",  "CCCC 111 0 ppp 0 nnnn dddd iiii sss 1 mmmm", 0, instruction);
  if (match("CCCC 111 0 ppp 0 nnnn dddd iiii sss 1 mmmm",    instruction)) return new ARMInstruction("MRC<CC> p<i>, p, Rd, CRn, CRm, s",  "CCCC 111 0 ppp 0 nnnn dddd iiii sss 1 mmmm", 0, instruction);

  if (match("CCCC 111 1 nnnnnnnnnnnnnnnnnnnnnnnn",           instruction)) return new ARMInstruction("SWI n",                             "CCCC 111 1 nnnnnnnnnnnnnnnnnnnnnnnn", 0, instruction);

  return nullptr;
}

bool ARMDisassembler::match(std::string pattern, uint32_t instruction) {
  for ( int ip = 0, ii = 0; ip < pattern.size(); ++ip ) {
    if ( pattern[ip] == ' ' )
      continue;
    if ( pattern[ip] == '0' && (instruction & (1 << (31 - ii))) == 0 )
      return false;
    if ( pattern[ip] == '1' && (instruction & (1 << (31 - ii))) != 0 )
      return false;
    ++ii;
  }
  return true;
}

void ARMDisassembler::dump() {
  printf("ARMDisassembler::dump()\n");
  printf("size: %ld\n", Size);
  for (uint64_t i = 0x190; i < Size-4; ++i) {
    // little endian
    uint32_t instruction = (uint32_t) (Data[i + 3] << 24) | (Data[i + 2] << 16) | (Data[i + 1] << 8) | Data[i];
    ARMInstruction* armInstruction = disassembleInstruction(instruction);
    if (armInstruction) {
      armInstruction->dump();
    } else {
      printf("%08lx: %08lx\n", i, instruction);
    }
  }
}
