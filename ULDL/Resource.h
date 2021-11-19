#ifndef ULDL_RESOURCE_H
#define ULDL_RESOURCE_H

#include <iostream>

class Resource {
  public:
    std::string name;
    uint64_t address;
    uint64_t size;

    Resource(std::string name, uint64_t address, uint64_t size) : name(name), address(address), size(size) {}

    friend std::ostream& operator<<(std::ostream& os, const Resource& resource) {
      os << "Resource:{" << resource.name << ", " << resource.address << ", " << resource.size << "}";
      return os;
    }
};

class ResourceSet : public Resource {
  public:
    Resource* resources;
    size_t num_resources;

    ResourceSet(Resource* resources, size_t num_resources) :
        resources(resources), num_resources(num_resources) {}

    friend std::ostream& operator<<(std::ostream& os, const ResourceSet& resourceSet) {
      os << "ResourceSet:{";
      for (size_t i = 0; i < resourceSet.num_resources; i++)
        os << resourceSet.resources[i];
      os << "}";
      return os;
    }
};

#endif // ULDL_RESOURCE_H
