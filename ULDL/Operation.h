#ifndef ULDL_OPERATION_H
#define ULDL_OPERATION_H

#include <iostream>
#include <vector>

#include "Resource.h"

/**
 * Operation is the base class for all operations.
 * It describes the application of a function on a set of resources.
 */
class Operation {
  public:
    std::string name;
    std::vector<Resource*> resources;

    Operation(std::string name) : name(name), resources(std::vector<Resource*>()) {}

    void addResource(Resource* resource) {
      resources.push_back(resource);
    }

    friend std::ostream& operator<<(std::ostream& os, const Operation& operation) {
      os << "Operation:{";
      for (auto& resource : operation.resources)
        os << *resource;
      os << "}";
      return os;
    }

};

/**
 * WriteOperation is an operation that writes to a resource.
 */
class WriteOperation : public Operation {

};

/**
 * CopyOperation is an operation that copies from one resource to another.
 */
class CopyOperation : public Operation {

};

/**
 * ConditionalOperation is an operation that executes one of two operations
 * depending on the value of a resource.
 */
class ConditionalOperation : public Operation {

};


#endif // ULDL_OPERATION_H
