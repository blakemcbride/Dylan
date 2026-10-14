# Speed Comparison: SBCL, Common Lisp Dylan, and C Dylan

The same eleven small programs, run three ways:

- **SBCL** — plain Common Lisp (`bench/reference.lisp`), written the
  ordinary way, with no type declarations or `optimize` settings, compiled
  by SBCL.
- **Common Lisp Dylan** — the Dylan programs (`bench/benchmarks.dyl`) run
  by the Common Lisp implementation (`src/`): compiled to Lisp by
  `dylan:compile-file`, then by SBCL.
- **C Dylan** — the same Dylan programs compiled to C by `bin/dylanc`
  (`compiler/`), then by gcc `-O2`.

Hand-written C (`bench/reference.c`, gcc `-O2`) is shown too, as the
floor.

## Results

Best of three runs, each itself the best of three calls, at ten times the
benchmarks' usual sizes (`BENCH_SCALE=10`), in milliseconds. The last two
columns are each Dylan's time divided by SBCL's: below 1 is faster than
SBCL.

| Benchmark | SBCL | CL Dylan | C Dylan | C | CL Dylan ÷ SBCL | C Dylan ÷ SBCL |
|---|---:|---:|---:|---:|---:|---:|
| table — hash tables: insert, look up, update (2,000,000) | 108 | 1,585 | 120 | 118 | 14.7 | 1.1 |
| sort — sorting with `<` and with a method (1,000,000) | 362 | 1,154 | 343 | 150 | 3.2 | 0.95 |
| string — building strings (2,000,000) | 62 | 540 | 135 | 28 | 8.7 | 2.2 |
| deque — a deque as a queue (3,000,000) | 20 | 348 | 58 | 22 | 17 | 2.9 |
| stretchy — growing a stretchy vector (3,000,000) | 21 | 285 | 37 | 1 | 14 | 1.8 |
| range — iterating over a range (10,000,000) | 22 | 469 | 51 | 2 | 21 | 2.3 |
| exit — non-local exits (3,000,000) | 18 | 264 | 11 | 38 | 15 | 0.61 |
| condition — signalling and handling errors (300,000) | 7 | 186 | 48 | 2 | 27 | 6.9 |
| next-method — `next-method` chains (3,000,000) | 80 | 407 | 52 | 26 | 5.1 | 0.65 |
| tak — recursion with three arguments (3,000) | 344 | 4,401 | 206 | 164 | 13 | 0.60 |
| float — Mandelbrot set, floating point (3,000) | 5,443 | 8,930 | 333 | 225 | 1.6 | 0.06 |

The size each was run at is in parentheses. All three answered the same
on every benchmark.

**Totals:** SBCL 6.5 s, Common Lisp Dylan 18.6 s, C Dylan 1.4 s (and C
0.78 s).

## What the numbers say

**C Dylan is about as fast as plain SBCL, and on the numeric and calling
benchmarks faster.** It beats SBCL on tak, next-method, exit, sort and,
by a wide margin, float, and is within a factor of three of it on
everything else except condition. Against hand-written C it is within a
factor of 2.6 on all but the string, stretchy, range and condition
benchmarks, and is faster on exit.

- *float* (16 times faster than SBCL): the compiler infers that the
  values are `<double-float>`s and keeps them in C `double` variables, with
  no allocation. The plain Lisp, with no declarations, has SBCL box every
  intermediate float.
- *exit* (faster than SBCL and than C): an exit taken from the loop of the
  function that established it is compiled to a jump, as SBCL compiles its
  `return-from`, where the C uses `setjmp`/`longjmp`.
- *tak*, *next-method*: generic functions with few methods are dispatched
  by compiled decision trees or called directly, small integers are
  unboxed words, and `next-method` is a direct call of the next method.
- *condition* (7 times SBCL): signalling makes a condition object and
  searches the handler chain, and the handler is reached by a non-local
  exit with `_setjmp`; SBCL's `handler-case` is cheaper here.
- *stretchy*, *range*: the hand-written C and SBCL loops are a handful of
  instructions, which the C compiler sometimes reduces further; C Dylan
  still calls the library's generic functions for each element.

**Common Lisp Dylan is roughly 1.6 to 27 times slower than plain SBCL** —
10 times, as a geometric mean — because it keeps the language fully
dynamic: classes, methods and slots can change while the program runs,
so it dispatches generic functions through caches, checks its fast paths'
guards, and carries Dylan's multiple values and next-method arguments on
every call. It is closest on float (1.6), where boxed floats dominate both, and
on sort (3.2) and next-method (5.1).

**C Dylan runs from 3.4 to 27 times faster than Common Lisp Dylan** (13
times overall), the price of the dynamic language: a static program lets
the compiler know every class and method.

## The machine

- Intel Core i9-14900KF, 32 hardware threads, Linux 7.2.9 (Fedora 44)
- SBCL 2.6.3; gcc 16.2.1; the Boehm-Demers-Weiser collector 8.2.6

Times on another machine will differ, and benchmarks that take a few
milliseconds vary by a millisecond or two between runs.

## Reproducing

```console
$ BENCH_SCALE=10 sbcl --script bench/run.lisp     # SBCL and Common Lisp Dylan
$ BENCH_SCALE=10 sbcl --script bench/run-c.lisp   # C Dylan, C, and SBCL
```

Without `BENCH_SCALE` the sizes are a tenth of these, and several
programs finish in under a millisecond. `bench/run.lisp` reports
seconds and `bench/run-c.lisp` milliseconds; both time SBCL on
`reference.lisp`, and the SBCL column above is the better of the two.
