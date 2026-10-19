#!/bin/sh
# Copyright 2026 Blake McBride.  See ../../LICENSE.
#
# Tests of the compiler's command line, bin/dylanc (CompilerPlan.md,
# Phase 10):  sh tests/driver/run.sh

here=$(dirname "$(readlink -f "$0")")
dylanc="$here/../../bin/dylanc"
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

echo "$checks checks, $failures failures"
[ "$failures" = 0 ]
