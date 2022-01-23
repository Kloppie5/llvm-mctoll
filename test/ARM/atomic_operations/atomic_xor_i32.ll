; RUN: llc -o %t %s -filetype=obj -mtriple=armv7-none-linux-gnu
; RUN: llvm-mctoll -d -debug -o %t-dis.ll %t
; RUN: cat %t-dis.ll | FileCheck %s

@var32 = dso_local global i32 0

define i32 @test_atomic_xor_i32(i32 %offset) nounwind {
; CHECK: define i32 @test_atomic_xor_i32(i32 [[offset:%.*]]) {

   %old = atomicrmw xor i32* @var32, i32 %offset monotonic
   ; CHECK: [[old:%.+]] = atomicrmw xor i32* [[address:%.*]], i32 [[offset]] seq_cst, align 32

   ret i32 %old
   ; CHECK: ret i32 [[old]]
}