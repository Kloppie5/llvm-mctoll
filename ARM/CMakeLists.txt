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
  ARMBenchmarker.cpp
  ARMFunctionPrototype.cpp
  ARMLinearRaiserPass.cpp
  ARMMachineInstructionRaiser.cpp
  ARMModuleRaiser.cpp
  ARMRaiser/ARMRaiser.cpp
  ARMRaiser/ARMRaiserCDE.cpp
  ARMRaiser/ARMRaiserMVE.cpp
  ARMRaiser/ARMRaiserNEON.cpp
  ARMRaiser/ARMRaiserThumb.cpp
  ARMRaiser/ARMRaiserThumb2.cpp
  ARMRaiser/ARMRaiserVFP.cpp

  DEPENDS
    ${LLVM_MCTOLL_DEPS}
  )
