.PHONY: example_01 example_01_ref example_02 example_02_ref example_03_ref


example_01: src/*.rs examples/01_driver.c examples/01_simple.c
	cargo run examples/01_simple.c
	gcc examples/01_driver.c -c -o examples/01_driver.o
	gcc examples/01_driver.o a.out -o example_01
	./example_01

example_01_ref: src/*.rs examples/01_driver.c examples/01_simple.c
	gcc examples/01_simple.c -c -o a.out
	gcc examples/01_driver.c -c -o examples/01_driver.o
	gcc examples/01_driver.o a.out -o example_01_ref
	./example_01_ref

example_02: src/*.rs examples/02_driver.c examples/02_addition.c
	cargo run examples/02_addition.c
	gcc examples/02_driver.c -c -o examples/02_driver.o
	gcc examples/02_driver.o a.out -o example_02
	./example_02

example_02_ref: src/*.rs examples/02_driver.c examples/02_addition.c
	gcc examples/02_addition.c -c -o a.out
	gcc examples/02_driver.c -c -o examples/02_driver.o
	gcc examples/02_driver.o a.out -o example_02_ref
	./example_02_ref

example_03_ref: src/*.rs examples/03_driver.c examples/03_variables.c
	gcc examples/03_variables.c -c -o a.out
	gcc examples/03_driver.c -c -o examples/03_driver.o
	gcc examples/03_driver.o a.out -o example_03_ref
	./example_03_ref

example_03: src/*.rs examples/03_driver.c examples/03_variables.c
	cargo run examples/03_variables.c
	gcc examples/03_driver.c -c -o examples/03_driver.o
	gcc examples/03_driver.o a.out -o example_03
	./example_03