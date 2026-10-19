# Copyright 2026 Blake McBride.  See ../LICENSE.
#
# The Dylan runtime library.
#
#   make          build/libdylan.a, and build/libdylan-trampoline.a for
#                 programs built with -DDY_TRAMPOLINE
#   make test     build and run the runtime's tests (needs sbcl)
#
# The library has the garbage collector in it: GC=mps (the default), the
# Memory Pool System in mps/, compiled into it; or GC=boehm, the
# Boehm-Demers-Weiser collector, which programs then link with (-lgc).
#
# A variant of the library -- for testing (CompilerPlan.md, Phase 11), or
# with the other collector -- is made with
#     make VDIR=build/NAME VFLAGS="flags" [GC=boehm] build/NAME/libdylan.a
# by the compiler's driver, which knows the variants (compiler/driver.lisp).

CC      ?= gcc
CFLAGS  ?= -O2 -g
GC      ?= mps
VDIR    ?= build
VFLAGS  ?=

ifeq ($(GC),mps)
# Static methods leave the collector's DyFunction.env_count 0.
GC_CFLAGS = -DDY_MPS -Wno-missing-field-initializers
GC_OBJECT = $(VDIR)/mps.o
LDLIBS    = -lm -lpthread
else
LDLIBS    = -lgc -lm -lpthread
endif

ALL_CFLAGS = $(CFLAGS) $(VFLAGS) -std=c11 -Wall -Wextra -pthread -ffunction-sections -fdata-sections -Ic -Imps/code $(GC_CFLAGS)

SOURCES = c/object.c c/number.c c/string.c c/function.c c/objects.c c/control.c c/thread.c c/library.c c/primitives.c c/gc-mps.c
OBJECTS = $(SOURCES:c/%.c=$(VDIR)/%.o)
TRAMPOLINE_OBJECTS = $(SOURCES:c/%.c=$(VDIR)/trampoline/%.o)
TESTS   = ../tests/runtime

all: $(VDIR)/libdylan.a $(VDIR)/libdylan-trampoline.a

$(VDIR)/%.o: c/%.c c/dylan.h c/primitive.h Makefile | $(VDIR)
	$(CC) $(ALL_CFLAGS) -c $< -o $@

# The same, for programs built to use trampolines instead of musttail.
$(VDIR)/trampoline/%.o: c/%.c c/dylan.h c/primitive.h Makefile | $(VDIR)/trampoline
	$(CC) $(ALL_CFLAGS) -DDY_TRAMPOLINE -c $< -o $@

$(VDIR)/libdylan.a: $(OBJECTS) $(GC_OBJECT)
	$(AR) rcs $@ $^

$(VDIR)/libdylan-trampoline.a: $(TRAMPOLINE_OBJECTS) $(GC_OBJECT)
	$(AR) rcs $@ $^

$(VDIR) $(VDIR)/trampoline:
	mkdir -p $@

# The MPS, compiled whole as its makers recommend: its own flags, not ours.
# Its rash variety, which checks nothing; for testing (GC stress, the
# sanitizers), its hot one, which checks much of what the program gives it.
# The sanitizers themselves are not applied to it: its conservative scans
# read what AddressSanitizer forbids, and its barrier's signal handling
# deadlocks against ThreadSanitizer's, so for that it is built without
# memory protection (CONFIG_PROT_NONE, mps/code/mps.c).
MPS_VARIETY = $(if $(findstring DY_GC_STRESS,$(VFLAGS))$(findstring -fsanitize,$(VFLAGS)),,-DCONFIG_VAR_RASH)
MPS_PROTECTION = $(if $(findstring -fsanitize=thread,$(VFLAGS)),-DCONFIG_PROT_NONE)

$(VDIR)/mps.o: mps/code/mps.c $(wildcard mps/code/*.h) $(wildcard mps/code/*.c) Makefile | $(VDIR)
	$(CC) -O2 -g -pthread $(MPS_VARIETY) $(MPS_PROTECTION) $(filter -flto%,$(VFLAGS)) -c $< -o $@

# The tests link with static data the compiler generates.
$(VDIR)/test-data.c: $(TESTS)/generate.lisp $(wildcard ../compiler/*.lisp ../compiler/*/*.lisp) | $(VDIR)
	sbcl --script $(TESTS)/generate.lisp $@

$(VDIR)/test-runtime: $(TESTS)/test-runtime.c $(VDIR)/test-data.c $(VDIR)/libdylan.a
	$(CC) $(ALL_CFLAGS) $(TESTS)/test-runtime.c $(VDIR)/test-data.c $(VDIR)/libdylan.a $(LDLIBS) -o $@

test: $(VDIR)/test-runtime
	$(VDIR)/test-runtime

clean:
	rm -rf build

.PHONY: all test clean
