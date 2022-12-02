opt -mem2reg test.txt -S>u.txt
llc test.txt -o test.s
clang test.s -o test