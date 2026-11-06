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
# No file of the user's is replaced or deleted but the executable
cp main.dyl src
"$dylanc" src >/dev/null 2>&1; status=$?
check "an executable that would replace its source: status 2" test "$status" = 2
check "the source kept" cmp -s main.dyl src
"$dylanc" -o src greet.dyl src >/dev/null 2>&1; status=$?
check "-o naming a source: status 2" test "$status" = 2
check "that source kept" cmp -s main.dyl src
echo '(print 3)' > three.c
"$dylanc" -k three.c >/dev/null 2>&1; status=$?
check "-k C that would replace its source: status 2" test "$status" = 2
check "nothing written" test ! -e three.h -a ! -e three-dylan.c
check "a source named .c" output_is 3 sh -c "'$dylanc' three.c && ./three"
check "a source named .c kept" output_is '(print 3)' cat three.c
echo mine > other.c; echo mine > other.h; cp main.dyl other.dyl
check "other.c beside other.dyl" "$dylanc" greet.dyl other.dyl -o other
check "other.c and other.h kept" sh -c 'test "$(cat other.c other.h)" = "mine
mine"'
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
# Read from a file, a program has no read-eval-print loop's restart:
# abort ends it as an unhandled error, and it sees no handlers of its own
cat > abort.dyl <<'EOF'
(define hs '())
(do-handlers (method (type test function description) (set! hs (cons type hs))))
(print hs)
(abort)
(print 5)
EOF
"$dylan" < abort.dyl > abort.out 2> abort.err; status=$?
check "bin/dylan < file: abort's status is 1" test "$status" = 1
check "bin/dylan < file: abort ends the run, with no handlers" sh -c 'grep -q "^()" abort.out && ! grep -q "^5" abort.out'
check "bin/dylan < file: abort reported" grep -q "^Dylan error: no handler was established for the restart" abort.err
# The C compiler's warnings about a constant vector the library's bounds
# checks guard are not shown
echo '(print (third #(1 2) default: 5))' > bounds.dyl
check "no C warning for a guarded constant vector" sh -c '"$0" -o bounds bounds.dyl > bounds.out 2>&1 && ! grep -q warning bounds.out && [ "$(./bounds)" = 5 ]' "$dylanc"
# Nor about a store into the car of (), which the library's test rules out
printf "(print (set! (car '()) 1))\n" > setcar.dyl
check "no C warning for (set! (car '()) 1)" sh -c '"$0" -o setcar setcar.dyl > setcar.out 2>&1 && ! grep -q warning setcar.out && ./setcar 2>&1 | grep -q "not a pair: ()"' "$dylanc"
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
# bin/dylan recurses as deep as a compiled program: a million calls
printf '(define-method count-up (n) (if (= n 0) 0 (+ 1 (count-up (- n 1)))))\n(print (count-up 1000000))\n' | "$dylan" > deep.out 2>&1; status=$?
check "bin/dylan: a recursion a million calls deep" sh -c "test $status = 0 && grep -q 1000000 deep.out"
# A recursion too deep is an error; one in its handler too is reported,
# and ends the run, in both
cat > deeper.dyl <<'EOF2'
(define-method count-up (n) (if (= n 0) 0 (+ 1 (count-up (- n 1)))))
(print 'before)
(handler-bind (<error> (method (c next) (count-up 100000000))) (count-up 100000000))
(print 'after)
EOF2
"$dylanc" -o deeper deeper.dyl >/dev/null 2>&1; ./deeper > deeper.out 2> deeper.err; status=$?
check "the stack exhausted in a handler: status 1" test "$status" = 1
check "the stack exhausted in a handler: reported" sh -c 'grep -q "^before" deeper.out && ! grep -q after deeper.out && grep -q "^Dylan error: the stack is exhausted" deeper.err'
"$dylan" < deeper.dyl > deeper.out 2> deeper.err; status=$?
check "bin/dylan: the stack exhausted in a handler: status 1" test "$status" = 1
check "bin/dylan: the stack exhausted in a handler: reported" sh -c 'grep -q "^before" deeper.out && ! grep -q after deeper.out && grep -q "^Dylan error: the stack is exhausted" deeper.err'
# An unhandled condition, a warning and a break are reported as display
# prints them, through the program's print-object, in both
cat > report.dyl <<'EOF2'
(define-class <pt> (<object>) (x init-keyword: x:))
(define-method print-object ((p <pt>) stream escape?) (format stream "{pt ~a}" (x p)))
(define-class <oops> (<error>))
(define-method print-object ((c <oops>) stream escape?) (format stream "OOPS ~a" (make <pt> x: 1)))
(signal "warn ~a" (make <pt> x: 2))
(break "brk ~s" (make <pt> x: 3))
(error (make <oops>))
EOF2
printf 'Dylan warning: warn {pt 2}\nDylan break: brk {pt 3}\nDylan error: OOPS {pt 1}\n' > report.want
"$dylanc" -o report report.dyl >/dev/null 2>&1; ./report > report.out 2> report.err; status=$?
check "a report through print-object" sh -c "test $status = 1 && cmp -s report.err report.want"
"$dylan" < report.dyl > report.out 2> report.err; status=$?
check "bin/dylan: a report through print-object" sh -c "test $status = 1 && cmp -s report.err report.want"
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

# Modules whose names differ only in characters a file name cannot hold,
# and one whose name ends in -dylan, each have a C file of their own
cat > mdefs.dyl <<'EOF2'
(define-module a/b (use dylan) (export f))
(define-module a*b (use dylan) (export g))
(define-module my-dylan (use dylan) (export h))
(define-module main (use dylan) (use a/b) (use a*b) (use my-dylan))
EOF2
printf 'module: a/b\n\n(define-method f () 1)\n' > m1.dyl
printf 'module: a*b\n\n(define-method g () 2)\n' > m2.dyl
printf 'module: my-dylan\n\n(define-method h () 3)\n' > m3.dyl
printf 'module: main\n\n(print (list (f) (g) (h)))\n' > m4.dyl
check "modules a/b, a*b and my-dylan" sh -c "'$dylanc' -k -o mods2 mdefs.dyl m1.dyl m2.dyl m3.dyl m4.dyl && test \"\$(./mods2)\" = '(1 2 3)' && test -f mods2-a_2f_b.c -a -f mods2-a_2a_b.c -a -f mods2-my-dylan.c"
# Inputs and outputs that cannot be read or written are reported, not
# Lisp backtraces
mkdir adir
check "a directory as the input" sh -c "'$dylanc' -o x adir 2>&1 | grep -q 'adir is a directory'"
printf '(print (size "a\377b"))\n' > badutf.dyl
check "a byte that is not UTF-8 is read as U+FFFD" sh -c "'$dylanc' badutf.dyl && test \"\$(./badutf)\" = 3"
check "an output directory that is missing" sh -c "'$dylanc' -k -o nodir/sub/p badutf.dyl 2>&1 | grep -q '^dylanc: cannot write'"
printf "(print 'ok)\n" > 'q"x.dyl'
printf 'x' > q
check "a \" in the file's name" sh -c "'$dylanc' 'q\"x.dyl' && test \"\$(./'q\"x')\" = ok"
# UTF-8 cannot encode a surrogate: both write U+FFFD for one
printf '(write-line (as <unicode-string> (list (as <character> #xD800) #\\a)))\n' > surr.dyl
check "a surrogate is written as U+FFFD" sh -c "'$dylanc' surr.dyl && ./surr > surr.c.out && '$dylan' < surr.dyl > surr.l.out 2>&1 && printf '\"\\357\\277\\275a\"\\n' > surr.want && cmp -s surr.c.out surr.want && grep -q \"\$(cat surr.want)\" surr.l.out"
# The options a program is compiled with do not change the methods it
# finds by reflection
printf '(print (size (generic-function-methods size)))\n' > methods.dyl
"$dylanc" -o methods methods.dyl >/dev/null 2>&1 && ./methods > methods1.out
"$dylanc" --whole -o methods methods.dyl >/dev/null 2>&1 && ./methods > methods2.out
"$dylanc" -Os -o methods methods.dyl >/dev/null 2>&1 && ./methods > methods3.out
check "--whole and -Os keep the methods reflection finds" sh -c 'test -s methods1.out && cmp -s methods1.out methods2.out && cmp -s methods1.out methods3.out'

echo "$checks checks, $failures failures"
[ "$failures" = 0 ]
