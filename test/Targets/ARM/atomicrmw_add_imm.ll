; RUN: llc -o %t %s -filetype=obj -mtriple=armv7-none-linux-gnu
; RUN: llvm-mctoll -o %t-dis.ll %t
; RUN: FileCheck %t-dis.ll

@var_i8 = dso_local global i8 0
; CHECK: @var_i8 = dso_local global i8 0

define i8 @test_atomicrmw_add_i8() {
; CHECK: define i8 @test_atomicrmw_add_i8() {

   %oldval = atomicrmw add i8* @var_i8, i8 10 seq_cst
; CHECK: [[TMP0:%.*]] = atomicrmw add i8* @var_i8, i8 1 seq_cst

   ret i8 %oldval
; CHECK: ret i8 [[TMP0]]
}