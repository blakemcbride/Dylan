#!/bin/sh
# Copyright 2026 Blake McBride.  See ../../LICENSE.
#
# Tests of the compiler's command line, bin/dylanc (CompilerPlan.md,
# Phase 10), and of bin/dylan reading a file:  sh tests/driver/run.sh
# (bin/dylan is built by make)

here=$(dirname "$(readlink -f "$0")")
dylanc="$here/../../bin/dylanc"
dylan="$here/../../bin/dylan"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work" || exit 1

checks=0
failures=0
check() {   # check DESCRIPTION COMMAND...
    description=$1; shift
    checks=$((checks + 1))
    if ! "$@" >/dev/null 2>&1; then
        failures=$((failures + 1))
        echo "FAIL $description"
    fi
}
output_is() {   # output_is EXPECTED COMMAND...
    want=$1; shift
    [ "$("$@" 2>&1)" = "$want" ]
}

cat > greet.dyl <<'EOF'
(define-method greet (name) (format #t "Hello, ~a!~%" name))
EOF
cat > main.dyl <<'EOF'
(greet "world")
(print (+ 1.5 (* 2 3)))
EOF
cat > module.dyl <<'EOF'
(define-module counting (use dylan) (export count-to))
EOF
cat > counting.dyl <<'EOF'
module: counting

(define count-to (method (n) (for ((i 0 (+ i 1))) ((= i n) i))))
EOF
cat > use.dyl <<'EOF'
(define-module app (use dylan) (use counting))
EOF
cat > app.dyl <<'EOF'
module: app

(print (count-to 5))
EOF
cat > macmods.dyl <<'EOF'
(define-module macros (use dylan) (export repeat))
(define-module user (use dylan) (use macros))
EOF
cat > macdef.dyl <<'EOF'
module: macros

(define-macro repeat (n #rest body)
  (bind ((i (gensym)))
    `(dotimes (,i ,n) ,@body)))
EOF
cat > macuse.dyl <<'EOF'
module: user

(repeat 3 (print 'hi))
EOF
printf '(print (car 5))\n' > fails.dyl
printf '(foo\n' > bad.dyl
expected=$(printf 'Hello, world!\n7.5')

check "build, default name" "$dylanc" greet.dyl main.dyl
check "run" output_is "$expected" ./greet
check "no C left" test ! -e greet.c -a ! -e greet.h -a ! -e greet-dylan.c
check "-o" "$dylanc" -o hello greet.dyl main.dyl
check "-o runs" output_is "$expected" ./hello
check "-k" "$dylanc" -k -o kept greet.dyl main.dyl
check "-k keeps the C" test -f kept.c -a -f kept.h -a -f kept-dylan.c -a -f kept-dylan-user.c
check "#line to the source" grep -q '#line 1 ".*greet.dyl"' kept-dylan-user.c
check "#line back to the C" grep -q '#line [0-9]* ".*kept-dylan-user.c"' kept-dylan-user.c
check "--no-lines" "$dylanc" -k --no-lines -o nolines greet.dyl main.dyl
check "--no-lines writes none" sh -c '! grep -q "#line" nolines-dylan-user.c'
check "-C" "$dylanc" -C -o conly greet.dyl main.dyl
check "-C writes C only" test -f conly.c -a ! -e conly
check "-C C compiles by hand" sh -c "cc -O2 -std=c11 -pthread -DDY_MPS -I'$here/../../runtime/c' -I'$here/../../runtime/mps/code' conly.c conly-dylan.c conly-dylan-user.c '$here/../../runtime/build/libdylan.a' -lm -lpthread -o byhand"
check "-C C runs" output_is "$expected" ./byhand
check "--gc mps" "$dylanc" --gc mps -o withmps greet.dyl main.dyl
check "--gc mps runs" output_is "$expected" ./withmps
check "--gc boehm" "$dylanc" --gc boehm -o withboehm greet.dyl main.dyl
check "--gc boehm runs" output_is "$expected" ./withboehm
for o in -O0 -O1 -O3 -Os -g; do
    check "$o" "$dylanc" $o -o opt greet.dyl main.dyl
    check "$o runs" output_is "$expected" ./opt
done
check "--trampoline --cc clang" "$dylanc" --trampoline --cc clang -o tramp greet.dyl main.dyl
check "--trampoline runs" output_is "$expected" ./tramp
for c in gcc clang; do
    check "--lto --cc $c" "$dylanc" --lto --cc $c -o lto greet.dyl main.dyl
    check "--lto --cc $c runs" output_is "$expected" ./lto
done
check "--whole" "$dylanc" --whole -o whole greet.dyl main.dyl
check "--whole runs" output_is "$expected" ./whole
check "the library's object kept" sh -c "ls '$here/../../runtime/build/library/'*.o"
check "CC" env CC=clang "$dylanc" -o withcc greet.dyl main.dyl
check "CC runs" output_is "$expected" ./withcc
check "modules" "$dylanc" -k -o mods module.dyl counting.dyl use.dyl app.dyl
check "modules run" output_is 5 ./mods
check "a file per module" test -f mods-counting.c -a -f mods-app.c
check "the library's C the same for two programs" \
      sh -c 'sed s/kept-dylan/X/ kept-dylan.c > k.txt; sed s/mods-dylan/X/ mods-dylan.c > m.txt; cmp k.txt m.txt'
check "a macro exported from its module" "$dylanc" -o macs macmods.dyl macdef.dyl macuse.dyl
check "the macro runs" output_is "$(printf 'hi\nhi\nhi')" ./macs
check "-E" sh -c "'$dylanc' -E macmods.dyl macdef.dyl macuse.dyl > expanded.txt"
check "-E writes the expansion" grep -q '=> (dotimes' expanded.txt
check "-E writes no C" test ! -e macmods.c
check "a run-time error" "$dylanc" -o fails fails.dyl
check "a run-time error's status" sh -c '! ./fails'
check "-g reports it too" "$dylanc" -g -o failsg fails.dyl
check "-g's status" sh -c '! ./failsg'
"$dylanc" bad.dyl >/dev/null 2>&1; status=$?
check "a compile error's status is 1" test "$status" = 1
check "nothing built" test ! -e bad
"$dylanc" -x greet.dyl >/dev/null 2>&1; status=$?
check "an unknown option's status is 2" test "$status" = 2
"$dylanc" >/dev/null 2>&1; status=$?
check "no files: status 2" test "$status" = 2
"$dylanc" nonesuch.dyl >/dev/null 2>&1; status=$?
check "a missing file: status 2" test "$status" = 2
check "-h" sh -c "'$dylanc' -h | grep -q Usage"
# A file name with characters Common Lisp takes for wildcards, and ??,
# which C takes for the start of a trigraph
cp main.dyl 'w??!*[1].dyl'
check "a name with * ? [" "$dylanc" -k greet.dyl 'w??!*[1].dyl' -o 'w??!*[1]'
check "it runs" output_is "$expected" './w??!*[1]'
"$dylanc" 'none??.dyl' >/dev/null 2>&1; status=$?
check "a missing file with ? in its name: status 2" test "$status" = 2
# A C compiler that is not there is reported, not a Lisp backtrace
"$dylanc" --cc no-such-cc -o nocc greet.dyl main.dyl >nocc.out 2>&1; status=$?
check "no C compiler: status 1" test "$status" = 1
check "no C compiler: reported plainly" sh -c '! grep -q Backtrace nocc.out && grep -q "the C compiler failed" nocc.out'
check "no C compiler: no C left" test ! -e nocc.c -a ! -e nocc.h
# bin/dylan < file: an unhandled error ends the run, as it ends a
# compiled program, and break carries on
cat > batch.dyl <<'EOF'
(print 1)
(break "look ~a" 42)
(print 2)
(car 5)
(print 3)
EOF
"$dylan" < batch.dyl > batch.out 2> batch.err; status=$?
check "bin/dylan < file: an error's status is 1" test "$status" = 1
check "bin/dylan < file: stops at the error" sh -c 'grep -q "^2" batch.out && ! grep -q "^3" batch.out'
check "bin/dylan < file: the error reported" grep -q "^Dylan error: " batch.err
check "bin/dylan < file: break reported" grep -q "^Dylan break: look 42" batch.err
echo '(print 4)' | "$dylan" > batch.out 2>&1; status=$?
check "bin/dylan < file: status 0 without an error" test "$status" = 0
# The C compiler's warnings about a constant vector the library's bounds
# checks guard are not shown
echo '(print (third #(1 2) default: 5))' > bounds.dyl
check "no C warning for a guarded constant vector" sh -c '"$0" -o bounds bounds.dyl > bounds.out 2>&1 && ! grep -q warning bounds.out && [ "$(./bounds)" = 5 ]' "$dylanc"
# An unhandled warning is reported as a compiled program reports it
echo '(signal "low ~a" 1)' | "$dylan" > warn.out 2> warn.err
check "bin/dylan: an unhandled warning" grep -q "^Dylan warning: low 1$" warn.err
# A program may change a literal; the host compiler's warning about it is
# not part of the output
echo '(print (bind ((s "cab")) (set! (element s 0) #\z) s))' | "$dylan" > literal.out 2>&1
check "bin/dylan: changing a literal is quiet" sh -c '! grep -qi warning literal.out && grep -q "zab" literal.out'
# (exit) is the loop's command to quit, unless the program defines exit
printf "(print 'one)\n(exit)\n(print 'two)\n" | "$dylan" > exit.out 2>&1
check "bin/dylan: (exit) quits" sh -c 'grep -q "^one" exit.out && ! grep -q "^two" exit.out'
printf "(define-method exit () (print 'mine))\n(exit)\n(print 'two)\n" | "$dylan" > exit.out 2>&1
check "bin/dylan: (exit) calls a program's exit" sh -c 'grep -q "^mine" exit.out && grep -q "^two" exit.out'
# dylan:empty-environment! forgets what the loop defined, so that each
# name can be defined again: classes, generic functions and macros too
echo "(load \"$here/../../src/load-dylan.lisp\")" > empty.lisp
cat >> empty.lisp <<'EOF'
(in-package :dylan)
(defun run (text) (with-input-from-string (*standard-input* text) (rep)))
(run "(define-class <foo> (<object>)) (define-macro twice (x) `(begin ,x ,x)) (define-generic-function gg (x))")
(empty-environment!)
(run "(define <foo> 3) (define twice 4) (define gg 5) (print (list <foo> twice gg))")
EOF
check "dylan:empty-environment!" sh -c 'sbcl --script empty.lisp 2>&1 | grep -q "^(3 4 5)"'

echo "$checks checks, $failures failures"
[ "$failures" = 0 ]
