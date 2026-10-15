# Dylan

Two implementations of the language described in *Dylan(TM): an
object-oriented dynamic language* (Apple Computer Eastern Research and
Technology, April 1992) — the original prefix-syntax Dylan, before the
language moved to an infix syntax:

- **An interactive implementation in Common Lisp** (`src/`). Dylan is
  compiled to Common Lisp as it is entered, at a read-eval-print loop or a
  file at a time, and run by the Lisp. It is the whole language of the
  manual, with classes and generic functions that change as the program
  runs.
- **A compiler from Dylan to C** (`compiler/`, `runtime/`, `bin/dylanc`).
  A whole, static program becomes a native executable: proper tail calls,
  generic dispatch compiled to decision trees, type inference, inlining
  and unboxed floats, the Boehm-Demers-Weiser collector, and real threads.
  It adds macros, threads and streams, which the manual lacks.

The standard both follow is the 1992 manual itself; an annotated copy is in
`manual/Dylan-1992/`. A user manual for this system — a tutorial, the
language, and both implementations — is in `manual/UserManual/`
(`Dylan_User_Manual.pdf`).

The home for this package is **<https://github.com/blakemcbride/Dylan>**.

> **License note up front:** this is *not* open source. Digital's 1992
> notice forbids sale or transfer for compensation. See
> [License](#license) below.

## Origins

The project began as a direct translation into Common Lisp of **Thomas**,
the Scheme implementation that Matt Birkholz, Jim Miller and Ron Weiss
wrote at Digital Equipment Corporation's Cambridge Research Laboratory in
1992. It has since gone far from that start:

- The Common Lisp implementation was corrected against the manual, chapter
  by chapter — `#f` and the empty list are distinct, modules (design notes
  32 and 33), keyword checking, sealing, slot options and much else — and
  rebuilt for speed: generic dispatch through caches, calls made directly,
  arithmetic, `element` and iteration done inline, slots read directly.
  Thomas's own goals explicitly did not include performing well.
- The compiler to C is new, written for this project. It shares only the
  reader with the Common Lisp implementation, which also runs its macros
  while compiling.

Thomas's sources are no longer here. Each file of `src/` still records
which Thomas file it grew from (see [`src/README`](src/README)), and
Thomas's 1992 README and release notes are kept as
[`README.thomas`](README.thomas) and [`RELEASE`](RELEASE).

## Quick start

The Common Lisp implementation:

```console
$ sbcl
* (load "src/load-dylan.lisp")
* (dylan:rep)

Entering Dylan read-eval-print-loop.
Exit by typing "dylan:done"

? (define-class <point> (<object>)
    (x init-value: 0 init-keyword: x:)
    (y init-value: 0 init-keyword: y:))

;Value: <point>

? (define-method norm ((p <point>))
    (sqrt (+ (* (x p) (x p)) (* (y p) (y p)))))

;Value: norm

? (norm (make <point> x: 3 y: 4))

;Value: 5.0

? dylan:done
```

The compiler to C:

```console
$ cat hello.dyl
(define-method greet ((name <string>))
  (format #t "Hello, ~a!~%" name))
(greet "world")
$ bin/dylanc hello.dyl
$ ./hello
Hello, world!
```

`(sqrt 25)` answers the float `5.0`, not an exact `5`: the manual defines
`sqrt`, `exp`, the trigonometric functions and their kin "as in Common
Lisp", and its examples show them answering floats. Rationals are
otherwise exact.

## Requirements

- **SBCL** for both (developed on 2.6.3). The Common Lisp implementation
  uses nothing SBCL-specific but weak hash tables and weak pointers and one
  query whether a symbol is special, all in `src/compat.lisp` with
  portable fallbacks.
- For the compiler to C, also a C compiler with `musttail` — GCC 15 or
  later, or Clang 13 or later (any C11 compiler will do with
  `--trampoline`) — and the Boehm-Demers-Weiser collector built for
  threads (`libgc` and its headers).

How to install these and build on each platform: [`BUILD.md`](BUILD.md)
(Linux), [`macOS.md`](macOS.md), [`Windows.md`](Windows.md).

The first use compiles the Lisp sources into a `fasl/` directory beside
them; later uses recompile only a changed file and those loaded after it.

## The Common Lisp implementation

There are three ways in.

**Interactive.** Load `src/load-dylan.lisp`.

| | |
|---|---|
| `(dylan:rep)` | start a read-eval-print loop that evaluates Dylan. Errors your Dylan code does not trap reach the Lisp debugger, which offers Dylan's restarts. |
| `(dylan:empty-environment!)` | forget the module variables earlier interactions created |
| `(dylan:in-module name)` | compile later input in module `name` (also typed at the `?` prompt) |

**Compiler to Lisp.** Load `src/load-compiler.lisp`.

| | |
|---|---|
| `(dylan:compile-file in out)` | compile a file of Dylan into a file of Lisp |
| `(dylan:compile-to-file out . forms)` | compile Dylan forms into a file of Lisp |

`dylan:compile-file` shadows the Common Lisp symbol of that name — inside
the `DYLAN` package it compiles Dylan. The forms handed to
`dylan:compile-to-file` must be Dylan forms, with their symbols interned in
the `DYLAN` package; that is what `dylan::dylan-read` produces, and
`dylan:compile-file` reads its input that way for you. A file is compiled
in the module its `module:` header names (design note 33), or in
`dylan-user`.

**Runtime only.** Load `src/load-runtime.lisp`, then `load` the Lisp files
the compiler produced. They are ordinary Common Lisp that calls only the
runtime. (`load-runtime.lisp` and `load-compiler.lisp` are kept for
Thomas's three ways in, but each loads the whole system, as
`load-dylan.lisp` does.)

```console
$ sbcl --eval '(load "src/load-compiler.lisp")' \
       --eval '(dylan:compile-file "program.dyl" "program.lisp")'
$ sbcl --eval '(load "src/load-runtime.lisp")' --eval '(load "program.lisp")'
```

**Modules**, in both implementations, follow design note 32, written in
prefix form:

```lisp
(define-module graphics (use dylan) (create draw-line erase-line))
(define-module lines
  (use dylan)
  (use graphics import: (draw-line (erase-line => erase))))
```

A `use` clause takes `import:`, `exclude:`, `prefix:`, `rename:` and
`export:`; `export` and `create` clauses name the variables a module
offers. `dylan` holds the predefined variables, and `dylan-user`, which
uses it, is where code goes by default.

## The compiler to C

```console
$ bin/dylanc hello.dyl          # makes ./hello
$ bin/dylanc -o app a.dyl b.dyl # several files, in order, one program
$ bin/dylanc -h                 # the options: -O0..-O3, -Os, --lto, --whole, -g, -k, -C, -E, ...
```

A program is compiled whole, together with the library (`runtime/dylan`,
written in Dylan) and a small runtime in C (`runtime/c`). The generated C
is `NAME.c` (data, dispatch and `main`), `NAME-MODULE.c` for each module's
functions, and `NAME.h`; `-k` keeps it, and its `#line` directives point
to the Dylan source. The library's functions, `NAME-dylan.c`, are the same
for most programs, so they are compiled once for each C compiler and set
of options and kept in `runtime/build/library/`; `-Os` and `--whole`
compile the library with the program instead, keeping only what the
program can reach. `-g` builds for debugging, with every inline operation
checking its arguments.

How it works, phase by phase, and how fast and small its programs are, is
in [`CompilerPlan.md`](CompilerPlan.md).

**Macros** (an extension; the 1992 manual has none) are Common Lisp's
kind, expanded while compiling:

```lisp
(define-macro swap! (a b)
  (bind ((tmp (gensym)))
    `(bind ((,tmp ,a)) (set! ,a ,b) (set! ,b ,tmp))))
```

`define-macro`, at top level, makes a variable of the module a macro: a
call of it is replaced by the value of its body, run with its parameters (a
method's parameter list) bound to the argument forms. The expansion's names
mean what they mean where it is used; `(gensym)` answers a name no program
can write. Macros are exported and imported as variables are, and a local
variable of the same name hides one. The bodies run in the Common Lisp
implementation of Dylan, with its library, `gensym`, and what
`(define-for-macros form ...)` defines — forms run only while compiling.
`dylanc -E` writes each use of a macro and its expansion.

**Threads** (an extension): `make-thread` with a function of no arguments,
`join-thread` (which answers its values), `current-thread` and
`thread-yield`; `<lock>` and `<recursive-lock>` with `with-lock`;
`<condition-variable>`; `<atomic-integer>`; `define-thread-variable` and
`dynamic-bind`. A condition a thread does not handle ends it, and
`join-thread` signals a `<thread-error>` holding it.

**Streams** (an extension; the manual uses them but does not define
them): `<stream>`, `*standard-output*`, `*standard-error*`,
`<string-stream>` with `stream-contents`, and `format`; every printing
function takes `stream:`.

What a static program cannot do, which the 1992 manual allows:

- **Change its classes or generic functions while running:** `add-method`,
  `remove-method`, `add-slot`, `remove-slot` and `freeze-methods` are
  reported when compiling. A `<generic-function>` made by `make` has no
  methods.
- **Specializers known only when running:** each must be a class or
  singleton the compiler can find — a literal, a constant variable,
  `(singleton x)` or `(make <singleton> object: x)` of a constant. A
  `define-slot` owner may be any singleton, but a class owner must be known
  when compiling.
- **Bignums and complex numbers:** integers are 63 bits (overflow is an
  error), and `<complex>` has no instances.
- **Evaluate or load code while running.**

## Testing

The Common Lisp implementation runs the examples from the Dylan book,
`dylan-examples.dyl`:

```console
$ cd src
$ sbcl --eval '(load "load-dylan.lisp")' \
       --eval '(load "full-test.lisp")' \
       --eval '(dylan:test-examples)'
```

The expected results are in `src/reference/`, the transcripts Thomas
produced from the same file in 1992. Where they and the manual disagree,
the manual decides; the few remaining differences are listed in
[`src/README`](src/README).

The compiler to C has its own suites. Each `run.lisp` takes a mode:
`quick`; the default, `all` — gcc and clang, with `musttail` and with
trampolines; `whole`, as `quick` but with the library compiled with each
program; `gc-stress`; `address`, for AddressSanitizer and
UndefinedBehaviorSanitizer; or `thread`, for ThreadSanitizer.

```console
$ sbcl --script tests/conformance/run.lisp  # the manual's examples, chapter by chapter
$ sbcl --script tests/codegen/run.lisp      # the compiler's own
$ sbcl --script tests/stress/run.lisp       # 10^8 tail calls; many threads
$ sh tests/driver/run.sh                    # bin/dylanc
$ make -C runtime test                      # the C runtime
```

## Benchmarks

`bench/benchmarks.dyl` holds small Dylan programs (tables, sorting,
strings, deques, stretchy vectors, ranges, non-local exits, conditions,
`next-method` chains, recursion, floating point), `bench/reference.lisp`
the same programs in plain Common Lisp, and `bench/reference.c` the same
in plain C.

```console
$ sbcl --script bench/run.lisp            # the Common Lisp implementation against plain Lisp
$ sbcl --script bench/run.lisp table tak  # just these
$ sbcl --script bench/run-c.lisp          # the compiler to C against C and plain Lisp
```

Each line gives the best of three runs for each, their ratios, and
`MISMATCH` if they answered differently.

## Differences from the 1992 manual

**Different behavior**

- **`!key`, `!rest` and `!next`** are read as `#key`, `#rest` and `#next`,
  as in Thomas, whose programs (the book's examples among them) use them;
  so they cannot name variables. Every other name is allowed.
- **Modules** are declared as design note 32 describes; the 1992 manual
  left that to the programming environment.
- **Backquote.** Chapter 3 defers it to "a separate section of this
  manual" that the manual does not contain, so `` ` ``, `,` and `,@` follow
  the usual Lisp rules, nesting included.
- **`map-into`** passes the function only the source collections'
  elements, as chapter 12's prose and design note 27 say. The chapter's
  one example, `(map-into x + '(1 2 3 4))` answering `(101 102 203 204)`,
  also passes the target's; here it answers `(1 2 3 4)`.
- **Irrational functions** (`sqrt`, `exp`, the trigonometric functions)
  answer floats for exact arguments, as in Common Lisp and the manual's
  examples.

**Additions**

- `display`, `newline`, `print` and `write-line`, and `pp`, which
  pretty-prints an object.
- In the Common Lisp implementation, `lisp-variable` and `lisp-procedure`
  reach Common Lisp values and functions from Dylan. Lisp's `NIL` arrives
  as the empty list, not `#f`.
- In the compiler to C: macros, threads and streams, above.

## Repository layout

| | |
|---|---|
| `src/` | the Common Lisp implementation — 38 Lisp files, ~14,800 lines |
| `src/README` | how it works, and which Thomas file each file grew from |
| `src/reference/` | Thomas's 1992 transcripts of the examples, the baseline its tests compare with |
| `dylan-examples.dyl` | the examples from the Dylan book |
| `compiler/` | the compiler to C, in Common Lisp — ~9,100 lines |
| `bin/dylanc` | its command line |
| `runtime/` | its runtime: C (`runtime/c`) and the library in Dylan (`runtime/dylan`) |
| `tests/` | its tests |
| `bench/` | benchmarks against plain Common Lisp and C |
| `BUILD.md`, `macOS.md`, `Windows.md` | building and running on Linux, macOS and Windows |
| `CompilerPlan.md` | the compiler's design, phase by phase, and where it stands |
| `SpeedComparison.md` | the benchmarks in plain SBCL, the Common Lisp implementation and the compiler to C |
| `manual/UserManual/` | the user manual — a tutorial, the language, both implementations (LaTeX; `make` builds the PDF) |
| `manual/Dylan-1992/` | the 1992 manual, annotated |
| `examples-from-book.text` | the raw extraction the examples were edited from |
| `README.thomas` | the original 1992 Thomas README |
| `RELEASE` | Thomas's 1992 release notes |
| `LICENSE` | Digital's notice, and what it permits |

## License

**This is source-available software, not open source.**

The original Thomas system is Copyright 1992 Digital Equipment Corporation;
the Common Lisp implementation in `src/`, which began as a translation of
it, is Copyright 2026 Blake McBride and is a derivative work of it.
Digital's terms govern the whole. In brief, they allow use, modification,
and redistribution at no charge to recipients who accept the same terms,
but **not sale or transfer for compensation** — which is why the notice
meets neither the Open Source Definition nor the Free Software Definition,
and is incompatible with the GPL and with MIT, BSD and Apache. See
[`LICENSE`](LICENSE).

Digital's notice also requires that it appear in all copies, derivative
works and modified versions, and any portion thereof. It does, in
[`LICENSE`](LICENSE) and at the head of every file that came from Thomas.
Please keep it there.

Putting any of this under a different license would need permission from the
present owner of Digital's copyright, by way of the Compaq (1998) and
Hewlett-Packard (2002) acquisitions and the 2015 Hewlett-Packard split.

## Credits

Thomas was written in four weeks by Matt Birkholz, Jim Miller and Ron Weiss
at Digital Equipment Corporation's Cambridge Research Laboratory, with
contributions from Joel Bartlett, Marc Feeley, Guillermo Rozas and Ralph
Swick. Their goals, in their own order of priority, were to learn about the
language by implementing it from the book, to help others learn from
readable source, and to write small programs in it. This project began
from their work.

The language is Apple's, from the 1992 book. The 1992 authors were careful
to name their system Thomas rather than Dylan, noting in the original
[`README.thomas`](README.thomas) that they had not received approval for
the trademark; this project uses the name Dylan for its package and entry
points.
