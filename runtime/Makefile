# Copyright 2026 Blake McBride.  See ../LICENSE.
#
# The Dylan runtime library.
#
#   make          build/libdylan.a, and build/libdylan-trampoline.a for
#                 programs built with -DDY_TRAMPOLINE
#   make test     build and run the runtime's tests (needs sbcl)

CC      ?= gcc
CFLAGS  ?= -O2 -g
ALL_CFLAGS = $(CFLAGS) -std=gnu11 -Wall -Wextra -pthread -ffunction-sections -fdata-sections -Ic
LDLIBS  = -lgc -lm -lpthread

SOURCES = c/object.c c/number.c c/string.c c/function.c c/objects.c c/control.c c/thread.c c/library.c c/primitives.c
OBJECTS = $(SOURCES:c/%.c=build/%.o)
TRAMPOLINE_OBJECTS = $(SOURCES:c/%.c=build/trampoline/%.o)
TESTS   = ../tests/runtime

all: build/libdylan.a build/libdylan-trampoline.a

build/%.o: c/%.c c/dylan.h c/primitive.h | build
	$(CC) $(ALL_CFLAGS) -c $< -o $@

# The same, for programs built to use trampolines instead of musttail.
build/trampoline/%.o: c/%.c c/dylan.h c/primitive.h | build/trampoline
	$(CC) $(ALL_CFLAGS) -DDY_TRAMPOLINE -c $< -o $@

build/libdylan.a: $(OBJECTS)
	ar rcs $@ $^

build/libdylan-trampoline.a: $(TRAMPOLINE_OBJECTS)
	ar rcs $@ $^

build build/trampoline:
	mkdir -p $@

# A variant of the library, for testing (CompilerPlan.md, Phase 11): made
# with   make VDIR=build/NAME VFLAGS="flags" build/NAME/libdylan.a
# by the compiler's driver, which knows the variants (compiler/driver.lisp).
ifdef VDIR
$(VDIR)/%.o: c/%.c c/dylan.h c/primitive.h | $(VDIR)
	$(CC) $(ALL_CFLAGS) $(VFLAGS) -c $< -o $@

$(VDIR)/libdylan.a: $(SOURCES:c/%.c=$(VDIR)/%.o)
	ar rcs $@ $^

$(VDIR):
	mkdir -p $@
endif

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
