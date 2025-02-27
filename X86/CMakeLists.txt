include_directories(
  ${LLVM_MAIN_SRC_DIR}/lib/Target/X86
  ${LLVM_BINARY_DIR}/lib/Target/X86
  ${CMAKE_CURRENT_SOURCE_DIR}/..
)

if(NOT LLVM_MCTOLL_BUILT_STANDALONE)
  set(LLVM_MCTOLL_DEPS intrinsics_gen X86CommonTableGen)
endif()

llvm_add_library(X86Raiser STATIC
  X86AdditionalInstrInfo.cpp
  X86FuncPrototypeDiscovery.cpp
  X86JumpTables.cpp
  X86MachineInstructionRaiser.cpp
  X86MachineInstructionRaiserSSE.cpp
  X86MachineInstructionRaiserUtils.cpp
  X86ModuleRaiser.cpp
  X86RaisedValueTracker.cpp
  X86RegisterUtils.cpp

  DEPENDS
    ${LLVM_MCTOLL_DEPS}
  )
