; RUN: llc -o %t %s -filetype=obj -mtriple=armv8-none-linux-gnu
; RUN: llvm-mctoll %t

@var_i8 = dso_local global i8 0

define dso_local i8 @test_atomic_load_add_i8 ( i8 %offset ) {
    %result = atomicrmw add i8* @var_i8, i8 %offset seq_cst
    ret i8 %result
}
