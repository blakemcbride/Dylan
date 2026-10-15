# Building and running on Linux

Linux is where this system is developed and tested (Fedora, SBCL 2.6.3,
GCC 15). For macOS see [`macOS.md`](macOS.md); for Windows,
[`Windows.md`](Windows.md).

## What you need

| | Common Lisp implementation | Compiler to C |
|---|---|---|
| SBCL | yes | yes |
| A C compiler with `musttail` (GCC 15+ or Clang 13+) | | yes |
| The Boehm-Demers-Weiser collector, built for threads (`libgc` and headers) | | yes |
| `make` | | yes |

Install them:

```console
# Fedora
$ sudo dnf install sbcl gcc make gc-devel

# Debian, Ubuntu
$ sudo apt install sbcl gcc make libgc-dev

# Arch
$ sudo pacman -S sbcl gcc make gc
```

Check the C compiler: `gcc --version`. If it is older than GCC 15 (Ubuntu
24.04 ships GCC 13, Debian 12 GCC 12), install Clang 13 or later
(`sudo apt install clang`) and use it:

```console
$ export CC=clang
```

or keep the older GCC and pass `--trampoline` to `dylanc` (any C11
compiler works that way, a little slower).

There is nothing to configure or install. Clone the repository and run it
where it is:

```console
$ git clone https://github.com/blakemcbride/Dylan.git
$ cd Dylan
```

## The Common Lisp implementation

```console
$ sbcl
* (load "src/load-dylan.lisp")
* (dylan:rep)
? (+ 1 2)

;Value: 3

? dylan:done
```

The first load compiles the Lisp sources into `src/fasl/`; later loads
recompile only what changed.

## The compiler to C

`bin/dylanc` compiles a whole program to a native executable:

```console
$ cat hello.dyl
(define-method greet ((name <string>))
  (format #t "Hello, ~a!~%" name))
(greet "world")
$ bin/dylanc hello.dyl
$ ./hello
Hello, world!
```

The first build makes the C runtime (`runtime/build/libdylan.a`) and
compiles the library; later builds reuse both. To use `dylanc` from
anywhere, put `bin/` on your `PATH` or link to it:

```console
$ ln -s "$PWD/bin/dylanc" ~/.local/bin/dylanc
```

`dylanc -h` lists the options. The C compiler is `$CC`, or `cc`, or
`--cc NAME`.

## Testing

```console
$ cd src
$ sbcl --eval '(load "load-dylan.lisp")' --eval '(load "full-test.lisp")' \
       --eval '(dylan:test-examples)'
$ cd ..
$ sbcl --script tests/conformance/run.lisp quick
$ sbcl --script tests/codegen/run.lisp quick
$ sbcl --script tests/stress/run.lisp quick
$ sh tests/driver/run.sh
$ make -C runtime test
```

The suites call `gcc` and `clang` by name, not `$CC`; `quick` uses `gcc`
with `musttail`, so it needs GCC 15. Without `quick` they run every
combination — GCC and Clang, with `musttail` and with trampolines — so both
compilers must be installed, and it takes much longer. The `address` and `thread` modes need the
sanitizer runtimes (`libasan`, `libubsan`, `libtsan`; on Fedora
`sudo dnf install libasan libubsan libtsan`).

## The user manual

`manual/UserManual/Dylan_User_Manual.pdf` is prebuilt. To rebuild it you
need a TeX distribution with `pdflatex` (Fedora: `texlive-scheme-medium`;
Debian/Ubuntu: `texlive-latex-extra`):

```console
$ make -C manual/UserManual
```

## Troubleshooting

| Message | Cause |
|---|---|
| `gc.h: No such file or directory` | the collector's headers are missing: install `gc-devel` / `libgc-dev` |
| `This C compiler cannot guarantee tail calls (musttail)` | the C compiler is too old: use Clang, GCC 15, or `--trampoline` |
| `sbcl: not found` | SBCL is not installed or not on `PATH` |
| `Could not make the runtime library.` | `make` is missing, or the runtime did not compile; run `make -C runtime` to see why |
