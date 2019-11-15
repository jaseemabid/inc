# Generate the testable executable inc
#
# This Makefile uses a bunch of magic variables, as explained here in the
# manual.
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
#
# $@ : The file name of the target of the rule.
# $< : The first prerequisite; usually the input file
#
# The C compiler flags used are,
#
# `-m64` forces to compile for 64bit target, this prevents accidental surprises.
#
# `-g3 -ggdb3` generates as much debug symbols as possible, notably the latter
# allows the use of macros in gdb prompt. As of now, only GCC seems to support
# this option.
#
# Omitting the frame pointer with `-fomit-frame-pointer` removes the standard
# function preamble and post when not needed. This makes the assembly slightly
# easier to read and harder to debug.
#
# `-fno-asynchronous-unwind-tables` gets rid of all the '.cfi' directives from
# the generated asm.
#
CFLAGS = -g -ggdb3 -m64 -Wall -Wno-override-module -fno-asynchronous-unwind-tables -fomit-frame-pointer

.DEFAULT_GOAL := inc
inc: runtime.c inc.s
	$(CC) -L./target/debug $(CFLAGS) $^ -linc -ldl -lpthread -o inc

# cargo install --force cbindgen
inc.h: src
	cbindgen -l C --crate inc --output inc.h

.PHONY: test
test:
	cargo test

.PHONY: clean
clean:
	rm -f a.out inc inc.s inc-*
	cargo clean

.PHONY: docker
docker:
	docker build -t inc:latest .

# Test inside the container
.PHONY: ctest
ctest: docker
	docker run inc cargo test
