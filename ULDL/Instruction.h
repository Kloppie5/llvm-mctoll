#ifndef ULDL_INSTRUCTION_H
#define ULDL_INSTRUCTION_H

#include <iostream>
#include <vector>

#include "Operation.h"

/**
 * An Instruction is a sequence of operations and contains the
 * information needed to perform the operations.
 */
class Instruction {

public:
  std::string name;
  std::vector<Operation*> operations;

  Instruction(std::string name) : name(name), operations(std::vector<Operation*>()) {}

  void addOperation(Operation* operation) {
    operations.push_back(operation);
  }

  friend std::ostream& operator<<(std::ostream& os, const Instruction& instruction) {
    os << "Instruction:{";
    for (auto& operation : instruction.operations)
      os << *operation;
    os << "}";
    return os;
  }
};

#endif // ULDL_INSTRUCTION_H
