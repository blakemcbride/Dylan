# Building and running on Windows

The system is developed on Linux ([`BUILD.md`](BUILD.md)). On Windows:

- **The Common Lisp implementation** runs natively, under SBCL for Windows.
- **The compiler to C** should be run under **WSL 2** (the Windows
  Subsystem for Linux), where it is simply Linux. A native Windows build
  is not supported; see [Native Windows](#native-windows) for why.

## WSL 2 (recommended, both implementations)

In PowerShell, as Administrator:

```console
> wsl --install -d Ubuntu
```

Restart if asked, open **Ubuntu** from the Start menu, and set up a user.
Then, in the Ubuntu shell:

```console
$ sudo apt update
$ sudo apt install sbcl clang make libgc-dev git
$ export CC=clang
```

(Clang, because Ubuntu 24.04's GCC 13 lacks `musttail`; add the `export`
to `~/.bashrc`.) Clone the repository inside the Linux file system — not
under `/mnt/c`, which is much slower — and follow [`BUILD.md`](BUILD.md):

```console
$ cd ~
$ git clone https://github.com/blakemcbride/Dylan.git
$ cd Dylan
$ bin/dylanc hello.dyl
$ ./hello
```

Programs it builds are Linux executables; they run in WSL, not from
Windows directly.

## The Common Lisp implementation, natively

Install SBCL for Windows from <https://www.sbcl.org/platform-table.html>
(the installer puts `sbcl` on `PATH`). In a Command Prompt or PowerShell,
in the repository:

```console
> sbcl
* (load "src/load-dylan.lisp")
* (dylan:rep)
? (+ 1 2)

;Value: 3

? dylan:done
```

The first load compiles the Lisp sources into `src\fasl\`. Use
`dylan:compile-file` and the runtime as described in
[`README.md`](README.md); nothing there depends on Unix.

When cloning with Git for Windows, keep line endings as they are
(`git config --global core.autocrlf input`, or clone with
`-c core.autocrlf=false`), so the reference transcripts the tests compare
with stay byte-for-byte the same.

## Native Windows

The compiler to C has not been ported to native Windows. MSYS2 supplies
most of what it needs — SBCL on `PATH`, `pacman -S make
mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-gc`, `bin/dylanc` run from
the UCRT64 shell with `CC=gcc` — but these stand in the way:

- **Threads and the collector.** The runtime uses POSIX threads; under
  MinGW those are winpthreads, and the collector must be built to know
  about them (`GC_WIN32_PTHREADS`). MSYS2's build of it is not known to
  be.
- **`musttail`** under the Windows x64 calling convention is untested;
  `--trampoline` avoids it.
- **None of it has been run there.** The test suites and `bin/dylanc`
  are shell and Unix-path oriented, and SBCL for Windows finds `cc` and
  `make` only if MSYS2's directories are on its `PATH`.

Until these are dealt with, use WSL 2.

## The user manual

`manual/UserManual/Dylan_User_Manual.pdf` is prebuilt. To rebuild it, in
WSL: `sudo apt install texlive-latex-extra` and `make -C manual/UserManual`.
