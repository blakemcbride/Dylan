# Building and running on macOS

The system is developed on Linux ([`BUILD.md`](BUILD.md)) and has not been
tested on macOS. What follows is what it needs there, and the differences
to expect. It applies to Apple Silicon and Intel Macs.

## What you need

- **The Xcode command line tools** — Apple Clang, `make`, `ar`:

  ```console
  $ xcode-select --install
  ```

  Apple Clang has `musttail` from Xcode 13.3 on; check with `cc --version`.
- **Homebrew** (<https://brew.sh>), for SBCL:

  ```console
  $ brew install sbcl
  ```

  The garbage collector, the Memory Pool System, is in the tree and
  supports Apple silicon. To build programs with the Boehm-Demers-Weiser
  collector instead (`dylanc --gc boehm`), also `brew install bdw-gc`.

- **macOS 13 (Ventura) or later.** `bin/dylanc` uses `readlink -f`, which
  older macOS lacks. On older systems, `brew install coreutils` and change
  `readlink` to `greadlink` in `bin/dylanc`.

If you installed `bdw-gc`: Homebrew's headers and libraries are not where
Apple Clang looks (`/opt/homebrew` on Apple Silicon, `/usr/local` on
Intel). Tell it, in `~/.zshrc`:

```sh
export CPATH="$(brew --prefix)/include"
export LIBRARY_PATH="$(brew --prefix)/lib"
```

Then clone the repository and run it where it is:

```console
$ git clone https://github.com/blakemcbride/Dylan.git
$ cd Dylan
```

## The Common Lisp implementation

Nothing differs from Linux:

```console
$ make
$ bin/dylan
? (+ 1 2)

;Value: 3

? dylan:done
```

`make` loads the system into SBCL and saves it as the executable
`bin/dylan`, which starts in the read-eval-print loop; `dylan:done`, `(exit)`
or end of file (Ctrl-D) leaves it. Run `make` again after changing `src/`. To run
`dylan` from anywhere, put `bin/` on your `PATH` or link to it:

```console
$ ln -s "$PWD/bin/dylan" ~/.local/bin/dylan
```

Or start a plain SBCL and load the system into it:

```console
$ sbcl
* (load "src/load-dylan.lisp")
* (dylan:rep)
? (+ 1 2)

;Value: 3

? dylan:done
*
```

There `dylan:done` returns to the Lisp prompt, and `(dylan:rep)` carries
on where you left off; `(exit)` quits SBCL too.

Either way, the first load compiles the Lisp sources into `src/fasl/`.

## The compiler to C

```console
$ cat hello.dyl
(define-method greet ((name <string>))
  (format #t "Hello, ~a!~%" name))
(greet "world")
$ bin/dylanc hello.dyl
$ ./hello
Hello, world!
```

The C compiler is `cc` (Apple Clang) unless `$CC` or `--cc` says
otherwise. `dylanc -h` lists the options. Differences from Linux:

- **`-Os` does not work.** It links with `-Wl,--gc-sections`, a GNU
  linker option Apple's linker rejects (its equivalent is `-dead_strip`).
  Use `-O2` or `-O3`.
- **`--lto`** needs a Clang that accepts `-flto=auto`; if Apple Clang
  rejects it, build without `--lto`.
- If Apple Clang is too old for `musttail` ("This C compiler cannot
  guarantee tail calls"), update Xcode, or pass `--trampoline`, or use
  Homebrew's Clang: `brew install llvm` and
  `export CC="$(brew --prefix llvm)/bin/clang"`.
- After changing C compilers, remove the old runtime: `make -C runtime clean`.

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

The suites call `gcc` and `clang` by name; on macOS both are Apple Clang,
so the GCC rows of the full (non-`quick`) runs test Clang twice. The
`thread` and `address` modes use the sanitizers Apple Clang includes.

## The user manual

`manual/UserManual/Dylan_User_Manual.pdf` is prebuilt. To rebuild it,
install MacTeX (`brew install --cask mactex-no-gui`) and run
`make -C manual/UserManual`.

## Troubleshooting

| Message | Cause |
|---|---|
| `'gc.h' file not found` / `library 'gc' not found` | `--gc boehm` with `CPATH` and `LIBRARY_PATH` not set to Homebrew's (above) |
| `readlink: illegal option -- f` | macOS older than 13; see above |
| `unknown option: --gc-sections` | `-Os`; see above |
| `This C compiler cannot guarantee tail calls (musttail)` | update Xcode, use Homebrew's Clang, or `--trampoline` |
