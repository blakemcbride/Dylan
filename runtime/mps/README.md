# The Memory Pool System

This directory holds the Memory Pool System (MPS), Ravenbrook Limited's
garbage collector, release 1.118.0, from
<https://github.com/Ravenbrook/mps>. It is Ravenbrook's work, not part of
this system, and is under its own license, the BSD 2-clause license in
`license.txt`.

It is the collector of compiled Dylan programs, unless one is built with
`dylanc --gc boehm`, which chooses the Boehm-Demers-Weiser collector, not
in this tree. `runtime/c/gc-mps.c` is the runtime's side: the object
format, the roots and the pools.

## What is here

`code/` is the MPS's `code/` directory less its tests, tools and build
files: every header, and the C files that `mps.c` includes, for every
platform. The MPS is meant to be compiled as that
one file, with its own compiler flags rather than the program's, and
`runtime/Makefile` does so into `mps.o` in the library variant's
directory. The platforms this tree has been built on are Linux on x86-64
(`lii6ll`) and macOS on Apple silicon (`xca6ll`); the MPS also supports
FreeBSD, and Windows with Visual C.

## Changes from the release

- `code/ss.h`: on Linux x86-64 the stack context saved on entry to the
  MPS also records the frame-pointer register plainly. The MPS saves the
  callee-saved registers with `setjmp` and scans the `jmp_buf` for
  references; the GNU C library stores `rbp` there mangled (xored with a
  secret and rotated), so a heap reference held only in that register,
  which C compilers use as a general register when they omit the frame
  pointer, was invisible to the collector and its object could be moved
  from under it.

- `code/mps.c`: on Linux x86-64, defining `CONFIG_PROT_NONE` builds the
  MPS without memory protection (`protan.c` in place of `protix.c` and
  `protsgix.c`). `runtime/Makefile` does so for programs built with
  ThreadSanitizer, whose signal handling runs with every signal blocked:
  a thread stopped at the collector's barrier could then never be
  suspended by the collector, which waited for it while it waited for
  the collector. Without protection the collector does the deferred work
  itself as the program leaves it, which is slower but sound.

Upgrading: take the new release's `code/`, keep the same subset, and
reapply the changes above.
