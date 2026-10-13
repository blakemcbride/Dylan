# Dylan-to-C Compiler Plan

## Goal

A compiler, written in Common Lisp (SBCL), that translates 1992 prefix-syntax
Dylan programs into portable C. A standard C compiler (gcc or clang) then
produces the executable. Garbage collection is the Boehm-Demers-Weiser
conservative collector (`libgc`), built for threads.

The targets are the language's original ones: **small and fast**.

- *Fast*: within a small factor (goal: 1–3×) of equivalent hand-written C
  on the programs in `bench/benchmarks.dyl`.
- *Small*: a "hello world" executable under ~100 KB statically linked
  against the runtime (excluding libc and libgc), through whole-program
  dead-code elimination.

The standard is the 1992 manual (`manual/Dylan-1992/book.annotated`). The
existing Common Lisp implementation is a testbed and a source of ideas,
not an oracle.

## Requirements

- **Static program.** Functions, methods, generic functions and classes
  do not change after compilation. Everything defined in the program is
  known to the compiler.
- **Proper tail calls.** Every call in tail position, known or unknown,
  runs in constant stack space.
- **Threads.** Multiple real OS threads via pthreads, running Dylan code
  in parallel.
- **Macros (future).** A Common Lisp–style macro system will be added
  later; macros are expanded at compile time.

## Not in the first version

- Bignums (integers are fixed-size; overflow signals an error).
- Complex numbers.
- `eval`, a REPL, or loading code at run time.
- Run-time changes to the program: `add-method`, `remove-method`, and
  `make` of a `<class>` or `<generic-function>` are compile-time errors.
  `seal` and `make-read-only` are accepted and do nothing (everything is
  already sealed).
- Infix syntax.
- Separate compilation of modules (see Phase 8).

Each of these deviations from the manual is listed in the compiler's
documentation.

## Overall design

```
 .dyl source
    │  reader (reuse dylan-read from src/compat.lisp, add source positions)
    ▼
 S-expressions
    │  module resolution (adapt src/comp-module.lisp)
    │  macro expansion (future; compile-time)
    ▼
 Core Dylan  ── special forms expanded to ~15 core forms
    │
    ▼
 IR (alpha-converted, A-normal form, explicit closures/exits)
    │  analyses: class hierarchy, call graph, type inference, escape
    │  optimizations: dispatch resolution, inlining, unboxing, DCE
    ▼
 Low-level IR (explicit C types, boxes, tags)
    │  C emitter
    ▼
 program.c  +  libdylan.a (runtime)  +  libgc (threaded)  +  pthreads
    │  cc -O2
    ▼
 executable
```

The compiler is a **whole-program** compiler. Because nothing changes at
run time, every class and generic function is effectively sealed: the
compiler knows every subclass of every class and every method of every
generic. Method selection still happens at run time wherever the
argument classes aren't known in advance, but it needs no caches: many
calls are resolved at compile time, and the rest go through a dispatch
function with a `switch` on the argument's class, generated at compile
time.

### Source layout

```
compiler/            the compiler (Common Lisp)
  package.lisp
  driver.lisp        command line / entry points: dylan->c, build
  front/             reader glue, modules, macros, expansion to core forms
  ir/                IR definitions, conversion, printers
  analysis/          CHA, call graph, type inference, escape analysis
  opt/               inlining, dispatch resolution, unboxing, DCE
  backend/           closure conversion, lowering, C emission
runtime/
  c/                 object model, GC glue, threads, dispatch support,
                     numbers, exits, conditions core, printer core, startup
  dylan/             the bulk of the library, written in Dylan and
                     compiled by this compiler
tests/               test programs and expected output from the manual
```

## Phase 1 — Front end

**Status: done.** `compiler/front/`; `(dylanc:front-end files)` answers a
program of core forms plus diagnostics. Tests: `sbcl --script
tests/front-end/run.lisp`.

1. **Reader.** Reuse `dylan-read` from `src/compat.lisp`, extended to
   record file and line for every form.
2. **Modules.** Adapt `src/comp-module.lisp` (design notes 32/33):
   resolve every identifier to a unique module-variable object.
3. **Macros (hook now, implement later).** Expansion runs in the compiler,
   before conversion to core forms, and is repeated until no macro forms
   remain. The future macro system needs to *run Dylan code at compile
   time* (the macro bodies). Options, in order of preference:
   - evaluate macro bodies with the existing Common Lisp Dylan
     implementation, loaded into the compiler — it already runs Dylan in
     the same Lisp image;
   - or compile macro bodies with this compiler's front end to Lisp and
     run them.
   Either way the generated C contains no trace of macros. Leave the
   expansion step as a table of expanders so special forms and future user
   macros share one mechanism.
4. **Expansion to core forms.** Rewrite every special form (`cond`, `case`,
   `select`, `for`, `dotimes`, `for-each`, `while`, `until`, `when`,
   `unless`, `and`, `or`, `let`/multiple-value binding, `local`,
   `method`/`define-method`, `define-generic-function`, `define-class`,
   `define-slot`, `handler-case`, `unwind-protect`, quasiquote, setter
   syntax, …) into a small core:

   `constant`, `variable-ref`, `set!`, `if`, `begin`, `let` (single and
   multiple values), `letrec`, `lambda` (with `#rest`/`#key`/`#next`
   lists), `call`, `bind-exit`, `unwind-protect`, `define-variable`,
   `primitive` (call into the C runtime).

   Class and generic definitions are evaluated by the compiler, not left
   for run time.
5. Errors are reported with file and line.

## Phase 2 — IR

**Status: done.** `compiler/ir/`; `(dylanc:to-ir program)` answers the
analyzed IR, and `check-ir` verifies its invariants. The inferred-type slot
on each variable is left empty for Phase 9. Tests: `sbcl --script
tests/ir/run.lisp`.

- Alpha-converted, A-normal form: every intermediate value is named, every
  call's arguments are trivial.
- Every call is marked tail or non-tail.
- Variables carry: assigned?, captured?, reference count, inferred type.
- Functions carry: parameter shape, known call sites, whether they escape.
- `bind-exit` exits are IR values whose uses are tracked, so purely local
  exits can become `goto`.

## Phase 3 — Data representation (runtime/c)

**Status: done.** `runtime/c/` (`dylan.h` is the representation);
`compiler/backend/classes.lisp` and `cdata.lisp` number the classes and emit
them, the program's symbols and its constants as static C. `#t` and `#f`
are instances of an internal `<boolean>` class (the manual names none);
`<complex>` remains, abstract, as `<real>`'s superclass. `<extended-float>`
is a C `double`. Tests: `make -C runtime test`.

All Dylan values are one machine word, `typedef uintptr_t D;`.

| Value | Representation |
|---|---|
| `<integer>` | tagged: low bit 1, 63-bit signed |
| heap objects | pointer, 8-byte aligned, low bits 000 |
| `<character>` | tagged immediate (tag `010`) |
| `#t`, `#f`, `()` | static objects (pointer compare) |
| `<ratio>`, floats | heap boxed (unboxed in locals where proven) |

Every heap object begins with a header word pointing to its class's
*wrapper* (class, class id, instance size, slot layout). Class ids are
assigned by the compiler in a preorder walk of the (complete, static)
hierarchy, so `instance?` against a single-inheritance subtree is a range
check; multiple inheritance uses a per-class bit vector.

**Numbers.** No bignums: `<integer>` is 63-bit. Arithmetic is open-coded
with `__builtin_add_overflow` and friends; overflow signals an error
rather than giving a wrong answer. `<ratio>` holds two integers, with the
same overflow rule. Floats follow IEEE; `<single-float>` is C `float`,
`<double-float>` and `<extended-float>` are C `double`. Contagion and
canonicalization follow chapter 14. No `<complex>`.

**Strings** are byte strings with a length; symbols and keywords are
interned through a static table built at compile time, with a locked
run-time table for symbols made by `as`.

**Memory.** All allocation goes through `GC_MALLOC` /
`GC_MALLOC_ATOMIC` (atomic for strings and float boxes). Boehm's
thread-local allocation makes small allocations lock-free; the compiler
emits small fixed-size allocations inline.

## Phase 4 — Functions, calls and proper tail calls

**Status: done**, except as noted. `compiler/backend/codegen.lisp` generates
the C; `runtime/c/function.c` holds the calling support. `(dylanc:build
files exe)` compiles and links a program. Local methods whose every call is
a jump are *contified*: compiled as labelled blocks of their host function,
their calls as `goto`. Programs can use only the provisional functions of
`runtime/c/primitives.c` until the library exists (Phase 8), and functions
are defined with `define` and `method` until generic functions exist
(Phase 5); a method's `#next` is `#f` when it is called directly. Not done
yet: specialized direct entries, compile-time `#key` parsing, and
stack-allocated `#rest` lists (all optimizations, Phase 9). Tests:
`sbcl --script tests/codegen/run.lisp` (gcc and clang, musttail and
trampolines, each with a 256 KB stack).

**Tail calls are the constraint that shapes the calling convention.** C
does not guarantee them, so:

1. **Self and local tail calls** (a function calling itself, or calls
   among `local`/`letrec` functions of one parent) compile to `goto` /
   loops inside one C function. This covers most loops.
2. **All other tail calls** use `__attribute__((musttail))` (clang ≥ 13,
   GCC ≥ 15), which guarantees the call is a jump. `musttail` requires the
   caller and callee to have the same C prototype, so every Dylan function
   has a **uniform entry**:

   ```c
   D f(D self, D a0, D a1, D a2, D a3, intptr_t argc);
   ```

   The first four arguments travel in registers; any others, and `#rest`
   overflow, go in a per-thread argument area that the callee consumes
   before it can be overwritten. `self` is the closure (or the generic
   function, or method record).
3. **Fallback**: a trampoline build mode for C compilers without
   `musttail` (each tail call returns the next function and its arguments
   to a dispatch loop). Slower, but correct and portable.

Non-tail calls to known functions may also use a **direct entry** with a
specialized prototype (exact arity, unboxed `intptr_t`/`double`
parameters) when that helps; tail calls always use the uniform entry.

Frames that a tail call must not jump out of — `unwind-protect`,
`bind-exit` frames that escape, condition handlers — make the calls
inside them non-tail, as the manual's semantics already imply.

**Arguments.** `#key` arguments are parsed at compile time when callee and
call site are known. `#rest` lists are stack-allocated when they don't
escape.

**Multiple values.** The first value is the C return value; the count and
extra values are in a **thread-local** values buffer.

**`next-method`** is passed only to methods that use it. Since every
method chain is known at compile time, `next-method` is almost always a
direct call.

**Closures.** Flat closure conversion; assigned captured variables live in
one-slot cells; closures with no free variables are static constants;
non-escaping closures are lambda-lifted.

## Phase 5 — Object system

**Status: done**, except as noted. `compiler/backend/objects.lisp`;
`runtime/c/objects.c`. Superclasses and specializers must be known at
compile time (classes, or singletons of constants or classes); a class or
generic function is a constant, and a class cannot be redefined. Each
generic's dispatch is a compiled decision tree whose leaves hold the sorted
method lists, computed at compile time; among classes neither of which is a
subclass of the other, the one earlier in the argument class's precedence
list is more specific (as in CLOS — the manual leaves this open). Slot
accessors, all five allocations, `make`/`initialize` with init-keyword
checking, `next-method` (with or without arguments), generic keyword
checking and the reflective functions of chapters 10 and 11 are in. Not
yet: static resolution and inlining of calls (Phase 9, with type
inference); methods on `make` or on other `dylan`-module functions except
`initialize` (Phase 8); slot descriptors; `define-slot` on singletons and
`add-method`/`remove-method` (not possible in a static program). Tests: the
object-system programs in `tests/codegen/cases-objects.lisp`.

**Classes.** The compiler builds the whole hierarchy: class precedence
lists (algorithm from `src/class.lisp`), slot layouts, initialization
plans. Each class is emitted as static C data. Since every subclass is
known, each slot's offset is fixed wherever the receiver's possible
classes agree; otherwise accessors index a per-wrapper offset table.

**`make`/`initialize`.** For each instantiated class, emit a specialized
allocator that fills defaults and init keywords directly, calling
`initialize` only when the program defines methods on it.

**Generic dispatch.** The *set* of methods on each generic is fixed at
compile time, but *which* method a call selects still depends on the
classes (or, for singletons, the values) of its arguments at run time.
Dispatch is therefore still a run-time operation in general; the static
method set only makes it cheaper:

1. *Static resolution* — where type inference proves the argument classes
   narrow enough that only one method can apply, call it directly or
   inline it. Slot accessors become a load/store.
2. *Run-time dispatch* — otherwise, the call goes to a C dispatch
   function emitted for that generic. It examines the actual arguments
   with a `switch` on class id or range tests, and pointer compares for
   singletons, then tail-calls the selected method. Since the method set
   never changes, this function needs no caches and no locks.
3. *No applicable method* (or an ambiguous one) is detected at run time
   and signals an error, as the manual requires.

Calls through first-class functions (a function held in a variable,
passed as an argument, or used by `apply`, `curry`, `map` etc.) are
likewise resolved at run time through the uniform entry.

**Reflection.** `object-class`, `subclass?`, `all-superclasses`,
`slot-descriptors`, `applicable-method?`, `sorted-applicable-methods`,
`find-method` etc. read static class and method data. Metadata the
program never queries is dropped.

## Phase 6 — Control and conditions

**Status: done.** A `bind-exit` whose exit is only called directly in its
own function compiles to jumps (with multiple values). Every other
`bind-exit`, and every `unwind-protect`, is lowered (`compiler/ir/control.lisp`)
into a closure run by a function of `runtime/c/control.c` that holds the
frame, so generated code never calls `setjmp` and can always make tail
calls. Handlers are frames in the same chain; `signal` and `next-handler`
are in C. The condition classes and `signal`, `error`, `cerror`,
`check-type`, `abort`, `break` and `default-handler` are written in Dylan
in `runtime/dylan/conditions.dyl` — the first file of the library, which
every program is now compiled with. The runtime's own errors (wrong
arguments, type errors, overflow, no applicable method, …) are signalled as
`<simple-error>` and `<type-error>` conditions. There is no interactive
debugger: an unhandled serious condition prints its message and exits with
status 1, `break` prints its message and returns `#f`, and `restart-query`
and `return-query` do nothing. Tests: `tests/codegen/cases-control.lisp`.

- **`bind-exit`** — if the exit procedure never leaves its function and is
  only called directly, it compiles to `goto`. Otherwise `_setjmp`/
  `_longjmp` on a frame linked into the thread's dynamic chain; calling an
  exit after its extent has ended is detected and signalled.
- **`unwind-protect`** — cleanup frames in the same chain; non-local exits
  run the intervening cleanups before jumping.
- **Conditions (chapter 13)** — handler stack as frames in the thread's
  dynamic chain; `signal`, `error`, `cerror`, handlers, restarts, `abort`.
  Written mostly in Dylan on three C primitives (push frame, pop frame,
  non-local transfer).
- Unhandled errors print a message and exit non-zero (in a thread other
  than the main one: see Phase 7).

## Phase 7 — Threads

**Status: done.** `runtime/c/thread.c`, `runtime/dylan/threads.dyl`. The API
as settled: `make-thread` (a function of no arguments), `join-thread`
(answers its values), `current-thread`, `thread-yield`; `<lock>` (error
checking) and `<recursive-lock>` with `acquire-lock`, `release-lock` and the
`with-lock` macro; `<condition-variable>` with `wait-condition-variable`,
`notify-condition-variable` and `notify-all-condition-variable`;
`<atomic-integer>` with `atomic-value`, `atomic-add!` and
`atomic-compare-and-swap!`; `define-thread-variable` and the `dynamic-bind`
macro. A thread made after a thread variable's definition has run starts
with its value. A serious condition a thread does not handle ends that
thread and `join-thread` signals a `<thread-error>` holding it. The program
ends when its top-level forms are done; it does not wait for other threads.
Tests: `tests/codegen/cases-threads.lisp`. (ThreadSanitizer has not been
tried yet.)

The 1992 manual has no threads, so this is an extension, documented as
such.

**Runtime.**
- Build Boehm with `GC_THREADS`; include `gc.h` before `pthread.h` so
  `pthread_create` is redirected to `GC_pthread_create`, which registers
  each thread's stack as a root.
- Per-thread state in one `_Thread_local` struct: values buffer, argument
  area, dynamic chain (exits, cleanups, handlers), current-thread object.
- Module top-level initialization finishes on the main thread before any
  user code can start a thread.

**Dylan API** (names to be settled):
- `<thread>`, `make-thread` (with a function), `join-thread` (answers the
  function's values), `current-thread`.
- `<lock>` (`pthread_mutex_t`), `with-lock`, `<recursive-lock>`.
- `<condition-variable>` with `wait`, `notify`, `notify-all`.
- Thread-local (fluid) variables: `define-thread-variable` and dynamic
  binding of it.
- Atomic operations on integer cells for simple counters.

**Semantics.**
- Assignments to module variables and slots are word stores; Dylan
  objects are never torn. Beyond that, sharing mutable data between
  threads requires locks.
- Library collections (`<table>`, `<stretchy-vector>`, `<deque>`) are not
  internally synchronized, but concurrent misuse must never crash the
  runtime or corrupt the GC heap. Internal shared runtime tables (symbol
  interning) take a lock.
- A condition not handled within a thread ends that thread; `join-thread`
  then signals an error in the joining thread carrying the original
  condition.
- Because the program is static there are no shared dispatch caches or
  class tables to protect.

## Phase 8 — The library

**Status: done.** The library is Dylan, in module `dylan`
(`runtime/dylan/`, read in file-name order and compiled with every
program), on the `%` primitives of `runtime/c/library.c`: chapter 9's
comparisons, chapter 14's numbers, characters and symbols, all of chapter
12 (the iteration protocol, the collection and sequence functions, lists,
vectors, strings, stretchy vectors, deques, ranges, tables, arrays),
chapter 17's functional operations, and printing (`print`, `write`,
`display`, `write-line`, `pp`, extended through `print-object`). `make` and
`as` are generic functions with methods on singletons of the built-in
classes. A call of `+`, `-`, `*`, `/`, `<`, `>` or `=` with two arguments
(or `-` or `/` with one) calls the binary generic function directly, and
unreachable library definitions are dropped (the tree shaking of Phase 9,
done early, since every program now carries the library). Floats print as
the shortest decimal that reads back (`21.0`; `1.5s0` for a single-float).
Slot descriptors (chapter 11) and `function-arguments` (chapter 10) are
in; two unbounded ranges intersect exactly (Chinese remainder theorem).
Streams, which the manual uses but does not define, are an extension:
`<stream>`, `*standard-output*`, `*standard-error*`, `<string-stream>` with
`stream-contents`, and `format`; every printing function takes `stream:`.
`add-method`, `remove-method`, `add-slot`, `remove-slot` and
`freeze-methods` are compile errors (the static program). Specializing
library code at each use is Phase 9. Tests:
`tests/codegen/cases-library.lisp`.

A thin C core, and most of the library written in Dylan:

- **C core**: object model, allocation, tags, integer/float primitives,
  string bytes, symbol table, threads and locks, exits/unwinding, I/O
  primitives, startup (`main` calls `GC_INIT`, initializes static data,
  runs module top-level forms in order).
- **Dylan library**: collections (lists, vectors, stretchy vectors,
  strings, deques, tables, ranges), the iteration protocol, sequence and
  collection functions, functional operations, comparisons, numeric
  methods, conditions, printing.

Whole-program optimization specializes library code at each use: `element`
on a known `<simple-object-vector>` is an indexed load, and `for-each` or
the iteration protocol over a list, vector, string or range becomes a C
loop with no state objects allocated.

## Phase 9 — Optimization

In order of expected payoff:

1. **Type inference** — flow-sensitive, over the IR, using the complete
   class hierarchy; seeds from literals, `make`, slot types, method
   specializers and declared variable types.
2. **Dispatch resolution** and **inlining** (small methods, slot
   accessors, arithmetic, `element`, iteration protocol).
3. **Integer and float unboxing** — known integers and floats in C
   `intptr_t`/`double` locals; boxed only on escape. Overflow checks stay
   unless a range is proven.
4. **Escape analysis** — stack-allocate non-escaping closures, `#rest`
   vectors and small objects.
5. **Constant folding, copy propagation, dead code elimination.**
6. **Tree shaking** — emit only reachable functions, classes, methods and
   metadata; for size builds add `-Os`, `-ffunction-sections
   -fdata-sections -Wl,--gc-sections` and LTO.

Later: separate compilation of the library (compiled once, with a summary
file of what callers may assume).

## Phase 10 — C output and build

- Emit C11 (`_Thread_local`, `_Static_assert`), one `.c` per module plus a
  shared header; readable output with `#line` directives pointing to the
  `.dyl` source.
- GCC/Clang extensions (`musttail`, overflow builtins, `__builtin_expect`)
  behind macros, with the trampoline mode as the portable fallback for
  `musttail`.
- Driver: `dylanc program.dyl -o program` runs the SBCL compiler → C →
  `cc` → link with `libdylan.a`, `-lgc` and `-lpthread`. Options for
  `-O0/-O2/-Os`, keep-C, trampoline mode, and a debug mode with run-time
  type checks everywhere.

## Phase 11 — Testing

The standard is the 1992 manual. Expected results come from the manual,
not from the Common Lisp implementation.

- **Manual conformance tests**: each example in the manual, with the
  result the manual gives for it, as a test (`dylan-examples.dyl` and
  `examples-from-book.text` are starting points, but each expected value
  is checked against the manual text). Where the manual specifies
  behavior without an example, write tests from the specification and
  cite the chapter and section in the test.
- **Feature tests**: integer overflow, float contagion, dispatch order,
  `next-method`, keyword parsing, exits through `unwind-protect`,
  conditions and restarts.
- **Tail-call tests**: deep mutual recursion through unknown functions,
  generic functions and closures (e.g. 10⁸ calls) must run in constant
  stack, in both normal and trampoline builds.
- **Thread tests**: many threads allocating heavily (GC under
  contention), locks and condition variables, unhandled conditions in
  threads, fluid variables, run under ThreadSanitizer in debug builds.
- **GC stress**: run the suite with a low `GC_set_free_space_divisor` and
  frequent forced collections.
- **Sanitizers**: AddressSanitizer/UBSan in debug builds.
- **Performance**: `bench/benchmarks.dyl` compiled to C, compared with
  hand-written C versions of the same programs.

## Milestones

| # | Deliverable | Proves |
|---|---|---|
| M0 | `fib`, `tak` with integers, `if`, top-level functions, `print` → C | pipeline end-to-end |
| M1 | uniform entry, `musttail`, local loops, trampoline mode | proper tail calls |
| M2 | closures, `letrec`/`local`, `set!`, `bind-exit`, `unwind-protect` | closure conversion, exits |
| M3 | integers with overflow checks, ratios, floats | numbers |
| M4 | classes, `make`, slots, generic functions, `next-method`, singletons | object model, dispatch |
| M5 | collections and iteration protocol in Dylan | self-hosted library |
| M6 | conditions and restarts | chapter 13 |
| M7 | modules, multiple files, manual conformance suite passing | language completeness |
| M8 | threads, locks, condition variables, fluid variables | pthreads support |
| M9 | type inference, inlining, unboxing, escape analysis | speed targets |
| M10 | tree shaking, size builds | size targets |
| M11 | compile-time macro system | macros |

Threads come after the single-threaded core works, but per-thread state
(values buffer, argument area, dynamic chain) is `_Thread_local` from M0
so nothing has to be retrofitted.

## Risks and open questions

- **`musttail` availability.** Needs clang ≥ 13 or GCC ≥ 15; otherwise
  the slower trampoline mode. Verify GCC's `musttail` on the uniform
  prototype early (M1), including with arguments in the per-thread area.
- **Uniform entry cost.** Four register arguments plus a count is a little
  slower than exact-arity C calls; direct entries for known non-tail calls
  recover most of it. Measure in M1.
- **Conservative GC and tagged values.** Tagged integers and characters
  are safe for Boehm; strings and float boxes must be allocated atomic.
  Pointers held only in the per-thread argument area must be visible to the
  collector (it lives in thread-local storage Boehm scans, or is
  registered as a root).
- **`setjmp`/`longjmp` cost** on non-local exits that can't be localized;
  measure on the `bind-exit` and condition benchmarks.
- **Integer width.** 63-bit integers with an error on overflow is a
  deviation from the manual's unbounded integers; bignums can be added
  later behind the same overflow checks.
- **Macro evaluation at compile time.** Decide between reusing the Common
  Lisp Dylan implementation and compiling macro bodies to Lisp (Phase 1).
- **Compile-time evaluation** of class and generic definitions must agree
  with the manual's semantics for top-level form order and side effects in
  init expressions; definitions that depend on run-time values stay lazy.
