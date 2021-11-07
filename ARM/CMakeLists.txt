include_directories(
  ${LLVM_MAIN_SRC_DIR}/lib/Target/ARM
  ${LLVM_BINARY_DIR}/lib/Target/ARM
  ${CMAKE_CURRENT_SOURCE_DIR}/..
  ${CMAKE_CURRENT_SOURCE_DIR}/.
)

if(NOT LLVM_MCTOLL_BUILT_STANDALONE)
  set(LLVM_MCTOLL_DEPS intrinsics_gen ARMCommonTableGen)
endif()

add_llvm_library(ARMRaiser
  ARMModuleRaiser.cpp
  ARMFunctionPrototype.cpp
  ARMBenchmarker.cpp
  ARMLinearRaiserPass.cpp
  ARMRaiser.cpp
  ARMRaiser/ARMRaiserCDE.cpp
  ARMRaiser/ARMRaiserMVE.cpp
  ARMRaiser/ARMRaiserNEON.cpp
  ARMRaiser/ARMRaiserThumb.cpp
  ARMRaiser/ARMRaiserThumb2.cpp
  ARMRaiser/ARMRaiserVFP.cpp
  model/A32Disassembler.cpp
  model/A32Instruction.cpp
  model/T32Disassembler.cpp
  model/T32Instruction.cpp
  ARMv7MRaiser.cpp
  ARMConcurrencyPatternMatcher.cpp
  ARMMachineInstructionRaiser.cpp

  DEPENDS
    ${LLVM_MCTOLL_DEPS}
  )
