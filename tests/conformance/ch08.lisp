;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Manual conformance tests: chapter 8, "Conditionals, Flow of Control,
;;;; and Evaluation", of the 1992 Dylan manual
;;;; (manual/Dylan-1992/book.annotated/ch8.html).  The expected results are
;;;; the manual's own, or, where it shows none, what its text specifies.
;;;; print writes like write and ends with a newline, so a string the
;;;; manual's transcripts show bare is expected here in quotes.

(
 ;; 8, True and False: #t, #f, not.  From the specification: #f is the only
 ;; false value (the empty list is true, footnote 17); not answers true for
 ;; #f and false otherwise.
 (ch08-true-false
  "(print #t)
   (print #f)
   (print (list (not #f) (not #t) (not 0) (not '()) (not \"\")))
   (print (if '() 'true 'false))
   (print (if 0 'true 'false))
   (print (if #f 'true 'false))"
  "#t
#f
(#t #f #f #f #f)
true
true
false
")

 ;; 8, Conditional Execution, if: the manual's examples test,
 ;; double-negative and show-and-tell.  From the specification: if returns
 ;; all the values of the form evaluated.
 (ch08-if
  "(define-method test ((thing <object>))
     (if thing
         #t
         #f))
   (define-method double-negative ((num <number>))
     (if (< num 0)
         (+ num num)
         num))
   (define-method show-and-tell ((thing <object>))
     (if thing
         (begin
           (print thing)
           #t)
         #f))
   (print (test 'hello))
   (print (test #t))
   (print (test #f))
   (print (double-negative 11))
   (print (double-negative -11))
   (print (show-and-tell \"hello\"))
   (print (show-and-tell #f))
   (bind ((a b (if #t (values 1 2) (values 3 4)))) (print (list a b)))
   (bind ((a b (if #f (values 1 2) (values 3 4)))) (print (list a b)))"
  "#t
#t
#f
11
-22
\"hello\"
#t
#f
(1 2)
(3 4)
")

 ;; 8, begin: \"(if #t (print ...) #t #f)\" is \"error: too many arguments
 ;; to if.\"
 (ch08-if-too-many-arguments
  "(if #t
       (print \"it was true\")
       #t
       #f)"
  ""
  :compile-error "if")

 ;; 8, if, footnote 18: \"The alternate clause is not optional in Dylan.\"
 ;; (from the specification)
 (ch08-if-alternate-required
  "(print (if #t 1))"
  ""
  :compile-error "if")

 ;; 8, when and unless (from the specification; the manual's examples use
 ;; undefined functions): the values of the last form when run, #f when
 ;; not run, #f when there are no forms.
 (ch08-when-unless
  "(print (when #t 1 2 3))
   (print (when #f (print 'not-run) 3))
   (print (when #t))
   (print (when 'yes (print 'ran) 'last))
   (bind ((a b (when 1 (values 5 6)))) (print (list a b)))
   (print (unless #f 1 2 3))
   (print (unless #t (print 'not-run) 3))
   (print (unless 0 (print 'not-run) 3))
   (print (unless #f))
   (bind ((a b (unless #f (values 7 8)))) (print (list a b)))"
  "3
#f
#f
ran
last
(5 6)
3
#f
#f
#f
(7 8)
")

 ;; 8, cond: the manual's example, with the positions as parameters.  From
 ;; the specification: later tests are not evaluated, a clause with no
 ;; consequents answers the test's first value, #t as the otherwise test,
 ;; #f when no test is true, the values of the last consequent.
 (ch08-cond
  "(define-method compare-positions (new-position old-position)
     (cond ((< new-position old-position)
            \"the new position is less\")
           ((= new-position old-position)
            \"the positions are equal\")
           (else: \"the new position is greater\")))
   (print (compare-positions 1 2))
   (print (compare-positions 2 2))
   (print (compare-positions 3 2))
   (print (cond (#f 1) ((begin (print 'second) #t) (print 'consequent) 2) ((print 'third) 3)))
   (print (cond (#f 1) ((+ 1 2))))
   (print (cond (#f 1) (#t 'otherwise)))
   (print (cond (#f 1) ((= 1 2) 2)))
   (bind ((a b (cond ((= 1 1) (values 'x 'y))))) (print (list a b)))"
  "\"the new position is less\"
\"the positions are equal\"
\"the new position is greater\"
second
consequent
2
3
otherwise
#f
(x y)
")

 ;; 8, case: the manual's career-choice example (its parentheses balanced,
 ;; and the target a parameter).  From the specification: matching with
 ;; id? against unevaluated numbers, characters, symbols and keywords; #f
 ;; for a matching clause with no consequents; #t or else: as the otherwise
 ;; clause; the values of the last consequent; an error when nothing
 ;; matches.
 (ch08-case
  "(define-method advise (career-choice)
     (case career-choice
       ((art music drama)
        (print \"Don't quit your day job.\"))
       ((literature history linguistics)
        (print \"That really is fascinating.\"))
       ((science math engineering)
        (print \"Say, can you fix my VCR?\"))
       (else: \"I wish you luck.\")))
   (advise 'music)
   (advise 'history)
   (advise 'math)
   (print (advise 'law))
   (define-method classify (x)
     (case x
       ((1 2 3) 'small)
       ((#\\a #\\b) 'letter)
       ((foo: bar:) 'keyword)
       ((nothing))
       ((x) (print 'first) 'symbol)
       (#t 'other)))
   (print (list (classify 2) (classify #\\b) (classify bar:) (classify 'nothing)))
   (print (classify 'x))
   (print (classify 99))
   (print (case 'quote ((quote) 'matched-quote) (else: 'no)))
   (bind ((a b (case 5 ((5) (values 1 2))))) (print (list a b)))
   (print (handler-case (case 7 ((1 2) 'a) ((3) 'b)) ((<error>) 'error)))"
  "\"Don't quit your day job.\"
\"That really is fascinating.\"
\"Say, can you fix my VCR?\"
\"I wish you luck.\"
(small letter keyword #f)
first
symbol
other
matched-quote
(1 2)
error
")

 ;; 8, select: the manual's typecase-like example, with built-in classes
 ;; for <window>, <view> and <rectangle> (which the program does not
 ;; have).  From the specification: the match-list elements are evaluated,
 ;; the test function compares, else: and #t otherwise clauses, #f for a
 ;; matching clause with no consequents, and (being like case) an error
 ;; when nothing matches.
 (ch08-select
  "(define-method what-is (my-object)
     (select my-object instance?
       ((<character> <string> <symbol>) \"it's a textual object\")
       ((<number> <list> <sequence>) \"it's something computational\")
       (else: \"Don't know what it is\")))
   (print (what-is #\\a))
   (print (what-is 3))
   (print (what-is #(1 2)))
   (print (what-is (method (x) x)))
   (define five 5)
   (print (select 6 = ((1 2) 'low) (((+ five 1) (* five 2)) 'six-or-ten) (#t 'other)))
   (print (select 10 = ((1 2) 'low) (((+ five 1) (* five 2)) 'six-or-ten) (#t 'other)))
   (print (select 3 = ((1 2) 'low) ((3)) (#t 'other)))
   (print (select 4 < ((1 2) 'low) ((3 5) 'mid) (else: 'other)))
   (print (select \"ab\" = ((\"xy\") 1) ((\"ab\") 2) (#t 3)))
   (print (handler-case (select 7 = ((1) 'a) ((2) 'b)) ((<error>) 'error)))"
  "\"it's a textual object\"
\"it's something computational\"
\"it's something computational\"
\"Don't know what it is\"
six-or-ten
six-or-ten
#f
mid
2
error
")

 ;; 8, or and and (from the specification): or answers the first true
 ;; value of a form before the last and stops; otherwise all the values of
 ;; the last form.  and answers #f when a form before the last answers #f,
 ;; and stops; otherwise all the values of the last form.
 (ch08-or-and
  "(print (or #f 2 (print 'not-run)))
   (print (or #f #f))
   (print (or #f #f 'last))
   (print (or 1))
   (bind ((a b (or #f (values 1 2)))) (print (list a b)))
   (bind ((a b (or (values 3 4) 5))) (print (list a b)))
   (print (and 1 2 3))
   (print (and 1 #f (print 'not-run)))
   (print (and #f))
   (print (and 1))
   (bind ((a b (and 1 (values 1 2)))) (print (list a b)))
   (bind ((a b (and (values #f 4) 5))) (print (list a b)))"
  "2
#f
last
1
(1 2)
(3 #f)
3
#f
#f
1
(1 2)
(#f #f)
")

 ;; 8, begin: the manual's example; from the specification, the values of
 ;; the last form, and #f when there are no forms.
 (ch08-begin
  "(print (if #t
             (begin (print \"it was true\")
                    #t)
             #f))
   (print (begin 1 2 3))
   (print (begin))
   (bind ((a b (begin 1 (values 2 3)))) (print (list a b)))"
  "\"it was true\"
#t
3
#f
(2 3)
")

 ;; 8, Iteration Constructs, for: the manual's factorial (whose results
 ;; are from the specification).  From the specification: the inits are
 ;; evaluated in a scope without the iteration variables; the steps see
 ;; the previous values of every variable; #f with no result forms; the
 ;; values of the results; the body forms run when the end test is #f.
 (ch08-for
  "(define-method factorial ((n <integer>))
     (for ((i n (- i 1))   ;variable clause 1
           (v 1 (* v i)))  ;variable clause 2
          ((<= i 0) v)))   ;end test and result
   (print (list (factorial 0) (factorial 1) (factorial 5) (factorial 10)))
   (print (for ((i 0 (+ i 1))) ((= i 3))))
   (print (for ((i 0 (+ i 1))) ((= i 3) (print 'first-result) 'last-result)))
   (bind ((i 10))
     (print (for ((i 0 (+ i 1))
                  (j i (+ j 1)))
                 ((= i 2) (list i j)))))
   (print (for ((i 0 (+ i 1))
                (a 0 b)
                (b 1 (+ a b)))
               ((= i 10) a)))
   (for ((i 0 (+ i 1)))
        ((= i 3))
     (print i)
     (print (* i i)))
   (bind ((a b (for ((i 0 (+ i 1))) ((= i 1) (values 'p 'q))))) (print (list a b)))"
  "(1 1 120 3628800)
#f
first-result
last-result
(2 12)
55
0
0
1
1
2
4
(p q)
")

 ;; 8, for-each: the manual's first-even and schedule-olympic-games
 ;; examples (whose results are from the specification), with a
 ;; schedule-game that prints.  From the specification: #f when a
 ;; collection is exhausted; the values of the last result when the end
 ;; test is true; #f with no results; body forms run in order.
 (ch08-for-each
  "(define-method first-even ((s <sequence>))
     (for-each ((number s))
               ((even? number) number)
                                ; No body forms needed
       ))
   (print (first-even '(1 3 5 4 7 9 10)))
   (print (first-even #(1 3 5)))
   (define-method schedule-game (city year)
     (print (list city year)))
   (define-method schedule-olympic-games ((cities <sequence>)
                                          (start-year <number>))
     (for-each ((year (range from: start-year by: 4))
                (city cities))
               ()              ; No end test needed.
       (schedule-game city year)))
   (print (schedule-olympic-games '(\"Barcelona\" \"Atlanta\" \"Sydney\") 1992))
   (print (for-each ((x '(1 2 3))) ((= x 2))))
   (print (for-each ((x '(1 2 3))) ((= x 2) (print 'first) (* x 10))))
   (print (for-each ((x '(1 2 3)) (y #(a b c d))) (#f) (print x) (print y)))"
  "4
#f
(\"Barcelona\" 1992)
(\"Atlanta\" 1996)
(\"Sydney\" 2000)
#f
#f
first
20
1
a
2
b
3
c
#f
")

 ;; 8, dotimes: the manual's example.  From the specification: the
 ;; variable runs from zero to the count less one; no evaluation for a
 ;; count of zero or less; the values of the result form, else #f.
 (ch08-dotimes
  "(begin
     (dotimes (i 6) (print \"bang!\"))
     (print \"click!\"))
   (print (dotimes (i 3) (print i)))
   (print (dotimes (i 0) (print 'not-run)))
   (print (dotimes (i -2) (print 'not-run)))
   (define total 0)
   (print (dotimes (i 5 total) (set! total (+ total i))))"
  "\"bang!\"
\"bang!\"
\"bang!\"
\"bang!\"
\"bang!\"
\"bang!\"
\"click!\"
0
1
2
#f
#f
#f
10
")

 ;; 8, while and until (from the specification): the test is evaluated at
 ;; the start of each pass, and both answer #f.
 (ch08-while-until
  "(define n 0)
   (print (while (< n 3) (print n) (set! n (+ n 1))))
   (print (while #f (print 'not-run)))
   (print (until (= n 0) (set! n (- n 1)) (print n)))
   (print (until #t (print 'not-run)))"
  "0
1
2
#f
#f
2
1
0
#f
#f
")

 ;; 8, Simple Exits, bind-exit: the manual's first-even example.  From
 ;; the specification: the values of the last form on a normal exit, #f
 ;; with no forms, the exit procedure's arguments as the values, an exit
 ;; procedure passed to another function and stored in a data structure.
 (ch08-bind-exit
  "(define-method first-even ((seq <sequence>))
     (bind-exit (exit)
       (do (method (item)
             (when (even? item)
               (exit item)))
           seq)))
   (print (first-even '(1 3 5 4 7 9 10)))
   (print (bind-exit (exit) 1 2 3))
   (print (bind-exit (exit)))
   (bind ((a b c (bind-exit (exit) (exit 1 2 3) 4))) (print (list a b c)))
   (define-method call-it (f) (f 'from-call-it) 'not-returned)
   (print (bind-exit (exit) (call-it exit) 'not-here))
   (print (bind-exit (exit) (bind ((v (vector exit))) ((element v 0) 'from-vector)) 'not-here))
   (print (bind-exit (outer) (bind-exit (inner) (outer 'outer) 'inner-done) 'outer-done))
   (print (bind-exit (outer) (bind-exit (inner) (inner 'inner)) ))"
  "4
3
#f
(1 2 3)
from-call-it
from-vector
outer
inner
")

 ;; 8, unwind-protect (from the specification): the protected form's
 ;; values on a normal exit; the cleanup forms run in order, also when the
 ;; protected form is ended by a non-local exit.
 (ch08-unwind-protect
  "(print (unwind-protect (begin (print 'protected) 'value) (print 'cleanup1) (print 'cleanup2)))
   (bind ((a b (unwind-protect (values 1 2) 3))) (print (list a b)))
   (print (bind-exit (exit)
            (unwind-protect (begin (exit 'exited) 'not-here)
              (print 'cleanup-on-exit))))
   (print (handler-case (unwind-protect (error \"oops\") (print 'cleanup-on-error))
            ((<error>) 'error)))
   (print (bind-exit (exit)
            (unwind-protect
              (unwind-protect (exit 'nested) (print 'inner-cleanup))
              (print 'outer-cleanup))))"
  "protected
cleanup1
cleanup2
value
(1 2)
cleanup-on-exit
exited
cleanup-on-error
error
inner-cleanup
outer-cleanup
nested
")

 ;; 8, Evaluation, quote: the manual's examples ({the generic function +}
 ;; for + itself has no readable form, so that it is a function is
 ;; tested instead).
 (ch08-quote
  "(print (instance? + <function>))
   (print '+)
   (print (quote +))
   (print ''+)
   (print (+ 10 10))
   (print '(+ 10 10))
   (print (quote (+ 10 10)))
   (print (list 'a 'Hello #\\x \"str\" '#(1 2)))"
  "#t
+
+
(quote +)
20
(+ 10 10)
(+ 10 10)
(a Hello #\\x \"str\" #(1 2))
")

 ;; 8, apply: the manual's examples (the list's fourth element, lost from
 ;; the manual's text, taken as -; the printed list of methods has no
 ;; readable form and is tested by its size and first element).  From the
 ;; specification: the last argument may be any sequence.
 (ch08-apply
  "(print (apply + 1 '(2 3)))
   (print (+ 1 2 3))
   (define math-functions (list + * / -))
   (print (size math-functions))
   (print (id? (first math-functions) +))
   (print (apply (first math-functions) 1 2 '(3 4)))
   (print (apply * #(2 3 4)))
   (print (apply list 1 2 '()))
   (print (apply list '(a b c)))"
  "6
6
4
#t
10
24
(1 2)
(a b c)
")
)
