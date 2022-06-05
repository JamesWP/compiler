all: gcc_main compiler

simple.o:
	gcc -c examples/01_simple.c -o simple.o

compiler: compiler.S simple.o
	as compiler.S -o compiler.o
	gcc compiler.o simple.o -o compiler

gcc_main: gcc.S simple.o
	as gcc.S -o gcc_main.o
	gcc gcc_main.o simple.o -o gcc_main

