# Copyright 2026 Blake McBride.  See ../LICENSE.
#
# The Dylan runtime library.
#
#   make          build/libdylan.a
#   make test     build and run the runtime's tests (needs sbcl)

CC      ?= gcc
CFLAGS  ?= -O2 -g
ALL_CFLAGS = $(CFLAGS) -std=gnu11 -Wall -Wextra -pthread -Ic
LDLIBS  = -lgc -lm -lpthread

SOURCES = c/object.c c/number.c c/string.c
OBJECTS = $(SOURCES:c/%.c=build/%.o)
TESTS   = ../tests/runtime

all: build/libdylan.a

build/%.o: c/%.c c/dylan.h | build
	$(CC) $(ALL_CFLAGS) -c $< -o $@

build/libdylan.a: $(OBJECTS)
	ar rcs $@ $^

build:
	mkdir -p build

# The tests link with static data the compiler generates.
build/test-data.c: $(TESTS)/generate.lisp $(wildcard ../compiler/*.lisp ../compiler/*/*.lisp) | build
	sbcl --script $(TESTS)/generate.lisp $@

build/test-runtime: $(TESTS)/test-runtime.c build/test-data.c build/libdylan.a
	$(CC) $(ALL_CFLAGS) $(TESTS)/test-runtime.c build/test-data.c build/libdylan.a $(LDLIBS) -o $@

test: build/test-runtime
	build/test-runtime

clean:
	rm -rf build

.PHONY: all test clean
