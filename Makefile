example_01: src/*.rs examples/01_driver.c examples/01_simple.c
	cargo run
	gcc examples/01_driver.c -c -o examples/01_driver.o
	gcc examples/01_driver.o a.out -o example_01
