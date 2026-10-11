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

;Value: 5.0

? (map (curry + 1) '(3 4 5))

;Value: (4 5 6)

? dylan:done
```

Note that `(sqrt 25)` answers the float `5.0`, not an exact `5`: the manual
defines `sqrt`, `exp`, the trigonometric functions and their kin "as in
Common Lisp", and its examples show them answering double-floats.
Rationals are otherwise exact, as in Common Lisp.

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

`dylan-examples.dyl` holds the examples from the Dylan book. To run them:

```console
$ cd src
$ sbcl --eval '(load "load-dylan.lisp")' \
       --eval '(load "full-test.lisp")' \
       --eval '(dylan:test-examples)'
```

The expected results are in `src/reference/`, the transcripts Thomas
produced from the same file in 1992. Where the two disagree, the manual
decides; the few remaining differences are listed in
[`src/README`](src/README).

## Differences from the 1992 manual

The standard is the manual itself (an annotated copy is in
`manual/Dylan-1992/`). Thomas's own 1992 list is in
[`DIFFERENCES`](DIFFERENCES); several of its items — `#key` and `#rest`,
sealed and abstract classes, `remove-slot`, collection alignment for
tables, the full set of character names — are no longer differences.

**Not implemented**

- **Modules.** There is one module. The manual leaves module operations to
  the programming environment, and none are provided.
- **Read-only module variables.** Not enforced: `set!` on a variable made
  by `define-class` or `define-generic-function` succeeds.
- **`(setter direct-superclasses)`**.
- **Virtual slots as "filtered slots"** (pp. 58–59). A virtual slot cannot
  use `next-method` to reach a hidden slot in the same object.
- **Ranges with `by: 0`**, such as `(range from: 1 by: 0 size: 3)`, are an
  error.

**Different behavior**

- **Symbol case** is not remembered: `'FooBar` prints as `foobar`. Symbols,
  keywords and variable names are case-insensitive, as in the manual.
- **Reserved names.** Variables may not begin with `!` or `dylan/`; the
  compiler uses those names.
- **`define-generic-function` options** are checked for keyword/value form
  and otherwise ignored.
- **Backquote.** The manual's section on it did not survive, so `` ` ``,
  `,` and `,@` follow the usual Lisp rules, nesting included.
- **`map-into`** passes the target's elements to the function, as the
  manual's example does, though its prose names only the sources.
- **Irrational functions** (`sqrt`, `exp`, the trigonometric functions)
  answer floats for exact arguments, as in Common Lisp and the manual's
  examples.

**Additions**

- `display`, `newline`, `print` and `write-line` (from Thomas).
- `pp` pretty-prints a Dylan object.
- `lisp-variable` and `lisp-procedure` reach Common Lisp values and
  functions from Dylan.

How the Scheme was turned into Common Lisp — naming, the treatment of
false and the empty list, the reader and printer — is described in
[`src/README`](src/README).

## Repository layout

| | |
|---|---|
| `src/` | the implementation — 36 files, ~12,000 lines |
| `src/README` | the translation notes, and which file came from which Thomas file |
| `src/reference/` | Thomas's 1992 transcripts of the examples, the regression baseline |
| `dylan-examples.dyl` | the examples from the Dylan book, as the test suite |
| `examples-from-book.text` | the raw extraction those were edited from |
| `README.thomas` | the original 1992 Thomas README |
| `DIFFERENCES` | Thomas's 1992 list of differences from the book |
| `RELEASE` | the 1992 release notes |
| `LICENSE` | Digital's notice, and what it permits |

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
