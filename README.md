# Dylan in Common Lisp

A Common Lisp implementation of the language described in *Dylan(TM): an
object-oriented dynamic language* (Apple Computer Eastern Research and
Technology, April 1992) — the original prefix-syntax Dylan, before the
language moved to an infix syntax.

It is a translation of **Thomas**, the Scheme implementation that Matt
Birkholz, Jim Miller and Ron Weiss wrote at Digital Equipment Corporation's
Cambridge Research Laboratory in 1992. The compiler now emits Common Lisp
rather than Scheme, and the runtime is a Common Lisp program. Every source
file records which file of Thomas it came from.

The home for this package is **<https://github.com/blakemcbride/Dylan>**.

> **License note up front:** this is *not* open source. Digital's 1992
> notice forbids sale or transfer for compensation. See
> [License](#license) below.

## Quick start

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

;Value: 5

? (map (curry + 1) '(3 4 5))

;Value: (4 5 6)

? dylan:done
```

Note that `(sqrt 25)` answers the exact `5`, not `5.0`: the arithmetic keeps
Scheme's exact/inexact distinction, which Common Lisp's rationals model
directly.

## Using it

There are three ways in, as in the original.

**Interactive.** Load `src/load-dylan.lisp`.

| | |
|---|---|
| `(dylan:rep)` | start a read-eval-print loop that evaluates Dylan. Errors your Dylan code does not trap reach the Lisp debugger. |
| `(dylan:empty-environment!)` | forget the module variables earlier interactions created |

**Compiler.** Load `src/load-compiler.lisp`.

| | |
|---|---|
| `(dylan:compile-file in out)` | compile a file of Dylan into a file of Lisp |
| `(dylan:compile-to-file out . forms)` | compile Dylan forms into a file of Lisp |

`dylan:compile-file` shadows the Common Lisp symbol of that name — inside
the `DYLAN` package it compiles Dylan. The forms handed to
`dylan:compile-to-file` must be Dylan forms, with their symbols interned in
the `DYLAN` package; that is what `dylan::dylan-read` produces, and
`dylan:compile-file` reads its input that way for you.

**Runtime only.** Load `src/load-runtime.lisp`, then `load` the Lisp files
the compiler produced. They are ordinary Common Lisp and need no compiler
present.

```console
$ sbcl --eval '(load "src/load-compiler.lisp")' \
       --eval '(dylan:compile-file "program.dyl" "program.lisp")'
$ sbcl --eval '(load "src/load-runtime.lisp")' --eval '(load "program.lisp")'
```

## Requirements

SBCL, which is what it is developed and tested on (currently 2.6.3).
Nothing is SBCL-specific except weak hash tables (`SB-EXT`), used for
class populations and the method tables, and one `SB-INT` query that the
compiler uses to tell whether a symbol is a special variable. All of them
are in `src/compat.lisp`, with portable fallbacks.

## Testing

`src/reference/` holds the transcripts the three original Scheme
implementations produced from `dylan-examples.dyl`, the examples extracted
from the Dylan book. To regenerate this implementation's transcript:

```console
$ cd src
$ sbcl --eval '(load "load-dylan.lisp")' \
       --eval '(load "full-test.lisp")' \
       --eval '(dylan:test-examples)'
```

It reproduces `src/reference/MIT.output` value for value, apart from a
handful of differences the original distribution already calls
implementation-dependent — chiefly the return value of `print`, on which
the three reference transcripts disagree with each other. The full list is
under "Differences from the Scheme version" in
[`src/README`](src/README).

## How the translation works

The details are in [`src/README`](src/README); the load-bearing decisions are
these.

**`NIL` is both false and the empty list.** Thomas was developed on MIT
CScheme 7.2, in which `#f` and `'()` are the same object — `generic.scm`
remarks on it — and the class heterarchy has no `<boolean>`: false is an
instance of `<empty-list>`. Common Lisp's `NIL` therefore models Thomas's
false/empty-list object exactly, which is what makes the truth-testing code
a mechanical translation rather than a redesign.

**Naming.** A colon cannot appear in a Common Lisp symbol without escaping,
so the two prefixes the Scheme used became:

| Scheme | Common Lisp | |
|---|---|---|
| `dylan:X` | `dylan/X` | visible to compiled Dylan code; variables holding procedures that take the two extra leading arguments of the Dylan calling convention |
| `dylan::X` | `dylan%X` | internal to the runtime and the compiler's output; ordinary Lisp calling conventions |

**Records → `defstruct`.** Every record type is statically known, so the
Scheme's runtime record package is gone and the six types are structures
with the accessor names the rest of the system already used.

**Escaping only.** `call-with-current-continuation` is used only to escape
— `bind-exit`, `handler-case`, `member?`, the test harness — so it became
`catch`/`throw`, and `dynamic-wind` became `unwind-protect`.

**Its own reader and printer.** Common Lisp's reader cannot read Dylan: a
colon is a package marker whose constituent trait no readtable may change,
and Dylan writes keywords with a trailing colon (`foo:`). Nor does it know
`#t` and `#f`. And Dylan wants `()`, `#t`, `foo:` and `9.` on output where
Lisp would print `NIL`, `T`, `:FOO` and `9.0`.

**Module variables are renamed.** In Scheme a Dylan module variable and a
Dylan lexical of the same name were one symbol, and shadowing came free. A
Common Lisp global must be proclaimed special, which would have made every
like-named lexical binding dynamic, so a module variable `X` is emitted as
`!MODULE/X/` and the compiler does the shadowing explicitly.

**Generated code.** `BEGIN` → `PROGN`, `SET!` → `SETQ`, dotted lambda lists
→ `&REST`, `LETREC` → `LET` plus `SETQ`. Scheme's `CASE` became a `COND`
over `MEMBER`: it compares with `EQV?` — Common Lisp's `EQL` — and Common
Lisp's `CASE` would read a clause whose key list is `(t)` as the default.

## Repository layout

| | |
|---|---|
| `src/` | the implementation — 36 files, ~12,000 lines |
| `src/README` | the full translation notes |
| `src/reference/` | the three 1992 Scheme transcripts, kept as the regression baseline |
| `dylan-examples.dyl` | the examples from the Dylan book, as the test suite |
| `examples-from-book.text` | the raw extraction those were edited from |
| `README.thomas` | the original 1992 Thomas README |
| `DIFFERENCES` | how the language here differs from the book |
| `RELEASE` | the 1992 release notes |
| `LICENSE` | Digital's notice, and what it permits |

Thomas's own Scheme tree — its `src/`, `portable/` and `kits/` directories —
is no longer here; `src/README` tabulates which Common Lisp file came from
which Scheme one.

## License

**This is source-available software, not open source.**

The original Thomas system is Copyright 1992 Digital Equipment Corporation;
this Common Lisp translation is Copyright 2026 Blake McBride and is a
derivative work of it. Digital's terms govern the whole. In brief, they
allow use, modification, and redistribution at no charge to recipients who
accept the same terms, but **not sale or transfer for compensation** — which
is why the notice meets neither the Open Source Definition nor the Free
Software Definition, and is incompatible with the GPL and with MIT, BSD and
Apache.

Digital's notice also requires that it appear in all copies, derivative
works and modified versions, and any portion thereof. It does, at the head
of every source file as well as in [`LICENSE`](LICENSE). Please keep it
there.

Putting any of this under a different license would need permission from the
present owner of Digital's copyright, by way of the Compaq (1998) and
Hewlett-Packard (2002) acquisitions and the 2015 Hewlett-Packard split.

## Credits

Thomas was written in four weeks by Matt Birkholz, Jim Miller and Ron Weiss
at Digital Equipment Corporation's Cambridge Research Laboratory, with
contributions from Joel Bartlett, Marc Feeley, Guillermo Rozas and Ralph
Swick. Their goals, in their own order of priority, were to learn about the
language by implementing it from the book, to help others learn from
readable source, and to write small programs in it — explicitly not to
perform well. This translation inherits all three, and the last one too.

The language is Apple's, from the 1992 book. The 1992 authors were careful
to name their system Thomas rather than Dylan, noting in the original
[`README.thomas`](README.thomas) that they had not received approval for
the trademark;
this translation uses the name Dylan for its package and entry points.
