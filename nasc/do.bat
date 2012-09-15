@echo off
llvm-as -o=test.bc test.ir
lli test.bc