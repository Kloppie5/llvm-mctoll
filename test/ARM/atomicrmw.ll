; RUN: llc -o %t %s -filetype=obj -mtriple=armv7-none-linux-gnu
; RUN: llvm-mctoll -d -debug -o %t-dis.ll %t
; RUN: cat %t-dis.ll | FileCheck %s

@var32 = dso_local global i32 0

define i32 @test_atomic_load_add_i32(i32 %offset) nounwind {

entry:
   ; CHECK: entry:
   ; CHECK: [[N_FLAG:%.+]] = alloca i1
   ; CHECK: [[Z_FLAG:%.+]] = alloca i1

   %old = atomicrmw add i32* @var32, i32 %offset release
   ; CHECK: bb.[[BB1:[0-9]+]]:

   ; CHECK: [[LDREX:%.+]] = call i32 @llvm.arm.ldrex.p0i32(i32* [[LDREX_TARGET:%.+]])
   ; CHECK: [[ADDR:%.+]] = add i32 [[LDREX]], %0
   ; CHECK: [[STREX:%.+]] = call i32 @llvm.arm.strex.p0i32(i32 [[STREX_VALUE:%.+]], i32* [[STREX_TARGET:%.+]])
   
   ; CHECK: [[CMP:%.+]] = icmp eq i32 [[STREX]], 0
   ; CHECK: store i1 [[CMP]], i1* [[Z_FLAG]], align 1
   
   ; CHECK: [[Z:%.+]] = load i1, i1* [[Z_FLAG]], align 1
   ; CHECK: [[ZC:%.+]] = icmp eq i1 [[Z]], false
   ; CHECK: br i1 [[ZC:%.+]], label %bb.[[BB1]], label %bb.[[BB2:[0-9]+]]
   
   ret i32 %old
   ; CHECK: bb.[[BB2]]:
   ; CHECK: ret i32 [[LDREX]]
}