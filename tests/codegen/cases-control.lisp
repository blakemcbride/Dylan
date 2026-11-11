;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for exits, cleanups and conditions (Phase 6), in the
;;;; form of cases.lisp.

(
 ;; Chapter 8: an exit returns its arguments as bind-exit's values;
 ;; with none, #f; a bind-exit whose body finishes answers its values.
 (bind-exit
  "(print (bind-exit (done) (done 1) 2))
   (print (bind-exit (done) 3))
   (print (bind-exit (done) (done)))
   (bind ((a b (bind-exit (e) (e 1 2) 3))) (print (list a b)))
   (bind ((a b (bind-exit (e) (values 4 5)))) (print (list a b)))
   (print (bind-exit (done) (dotimes (i 10) (when (= i 3) (done (* i 10)))) 'never))
   (define find-first (method (pred l)
     (bind-exit (return)
       (bind-methods ((walk (l) (unless (id? l '()) (when (pred (car l)) (return (car l))) (walk (cdr l)))))
         (walk l))
       #f)))
   (print (find-first even? '(1 3 4 5 6)))
   (print (find-first even? '(1 3)))"
  "1
3
#f
(1 2)
(4 5)
30
4
#f
")

 ;; An exit with other than one value, taken into a multiple-value bind in
 ;; a method that then returns one value: the method's caller must see
 ;; that one value, not the exit's.
 (bind-exit-values-then-return
  "(define-method f1 (x) (bind ((a b (bind-exit (k) (k 1 2)))) x))
   (define-method f2 (x) (bind ((a b (bind-exit (k) (k)))) x))
   (define-method f3 (x) (bind ((#rest r (bind-exit (k) (k 1 2 3)))) (+ x 1)))
   (print (list (f1 7) (f2 7) (f3 7)))
   (bind ((#rest r (f1 7))) (print r))
   (bind ((#rest r (f2 7))) (print r))
   (bind ((#rest r (f3 7))) (print r))
   (print (handler-case (f1 7) ((<error>) 'error)))"
  "(7 7 8)
(7)
(7)
(8)
7
")

 ;; The exit is a first-class value, used from another function -- the
 ;; manual's first-even, with a do of our own.
 (escaping-exit
  "(define each (method (f l) (unless (id? l '()) (f (car l)) (each f (cdr l)))))
   (define first-even (method (seq)
     (bind-exit (exit) (each (method (item) (when (even? item) (exit item))) seq) #f)))
   (print (first-even '(1 3 5 4 7 9 10)))
   (print (first-even '(1 3)))
   (define saved #f)
   (print (bind-exit (e) (set! saved e) 'inside))
   (saved 'too-late)"
  "4
#f
inside
"
  :error "called after its bind-exit returned")

 ;; unwind-protect: the cleanup runs on the way out, normally or by an
 ;; exit, innermost first; the protected form's values are returned.
 (unwind-protect
  "(bind ((a b (unwind-protect (values 1 2) (print 'cleanup)))) (print (list a b)))
   (print (bind-exit (out)
            (unwind-protect
              (unwind-protect (out 'exited) (print 'inner))
              (print 'outer))))
   (print (bind-exit (out)
            (unwind-protect (out 'a 'b) (print 'during))))"
  "cleanup
(1 2)
inner
outer
exited
during
a
")

 ;; Chapter 13: handler-case, its clauses tried in order, the condition
 ;; bound, and the runtime's own errors signalled as conditions.
 (handler-case
  "(print (handler-case (error \"bad ~a\" 1)
            ((<simple-error> condition: c)
             (list (condition-format-string c) (condition-format-arguments c)))))
   (print (handler-case (+ 1 2) ((<error>) 'no)))
   (bind ((a b (handler-case (values 3 4) ((<error>) 'no)))) (print (list a b)))
   (print (handler-case (check-type 'a <integer>)
            ((<type-error> condition: c) (list (type-error-value c) (type-error-expected-type c)))
            ((<error>) 'error)))
   (print (handler-case (car 5)
            ((<simple-error> condition: c) (condition-format-arguments c))))
   (print (handler-case (/ 1 0) ((<error>) 'overflow)))
   (print (handler-case (undefined-function 1) ((<error>) 'caught)))
   (define undefined-function 3)"
  "(\"bad ~a\" (1))
3
(3 4)
(a {the class <integer>})
(5)
overflow
caught
")

 ;; Handlers that return: signal answers their values.  next-handler
 ;; passes the condition outward; test: chooses.  With no handler, a
 ;; warning returns #f.
 (handler-bind
  "(handler-bind (<warning> (method (c next) 'handled))
     (print (signal \"w\")))
   (handler-bind (<warning> (method (c next) 'outer))
     (handler-bind (<warning> (method (c next) (next)))
       (print (signal \"w\"))))
   (handler-bind (<warning> (method (c next) 'tested) test: (method (c) #f))
     (handler-bind (<warning> (method (c next) 'chosen) test: (method (c) #t))
       (print (signal \"w\"))))
   (print (signal \"nobody is listening\"))
   (define-class <note> (<condition>))
   (print (signal (make <note>)))"
  "handled
outer
chosen
#f
#f
")

 ;; Restarts: cerror returns #f when a handler signals a <simple-restart>;
 ;; abort is caught like any restart.
 (restarts
  "(handler-bind (<error> (method (c next) (signal (make <simple-restart>))))
     (print (cerror \"continue anyway\" \"problem ~a\" 1))
     (print 'after))
   (print (handler-case (abort) ((<abort>) 'aborted)))
   (signal (make <simple-restart>))"
  "#f
after
aborted
"
  :error "no handler was established for the restart")

 ;; Conditions of the program's own classes.
 (user-conditions
  "(define-class <my-error> (<error>) (code init-keyword: code:))
   (print (handler-case (error (make <my-error> code: 42)) ((<my-error> condition: c) (code c))))
   (print (handler-case (error (make <my-error> code: 7)) ((<error> condition: c) (code c))))
   (error (make <my-error> code: 0))"
  "42
7
"
  :error "instance of <my-error>")

 ;; do-handlers sees each active handler.
 (do-handlers
  "(handler-bind (<error> (method (c n) 1))
     (handler-bind (<warning> (method (c n) 2) description: \"warn\")
       (bind ((count 0) (descriptions '()))
         (do-handlers (method (type test function description)
                        (set! count (+ count 1))
                        (set! descriptions (cons description descriptions))))
         (print (list count descriptions)))))"
  "(2 (#f \"warn\"))
")

 ;; Many handled errors in a loop: frames and error state are restored
 ;; each time.
 (handled-errors-in-a-loop
  "(define tries (method (n) (bind ((caught 0))
     (dotimes (i n) (handler-case (car i) ((<error>) (set! caught (+ caught 1)))))
     caught)))
   (print (tries 100000))"
  "100000
")

 ;; Unhandled errors stop the program with the message.
 (unhandled-error
  "(print 'before) (error \"something ~s went ~a\" \"x\" 'wrong)"
  "before
"
  :error "Dylan error: something \"x\" went wrong")

 ;; Chapters 2 and 13: the abstract condition classes cannot be made;
 ;; their subclasses can.
 (abstract-conditions
  "(define try (method (c) (handler-case (begin (make c) 'made) ((<error>) 'refused))))
   (print (map try (list <condition> <serious-condition> <error> <warning> <restart>)))
   (define-class <my-error> (<error>))
   (print (instance? (make <my-error>) <error>))
   (print (instance? (make <simple-error> format-string: \"x\") <error>))
   (define-class <my-restart> (<restart>))
   (print (instance? (make <my-restart>) <restart>))"
  "(refused refused refused refused refused)
#t
#t
#t
")

 ;; An exit taken from an unwind-protect's cleanup returns its own values,
 ;; and none left over from the protected form's.
 (exit-from-cleanup-values
  "(bind ((a b (bind-exit (e) (unwind-protect (values 1 2) (e 3))))) (print (list a b)))
   (bind ((a b c (bind-exit (e) (unwind-protect (values 1 2 3) (e 4 5))))) (print (list a b c)))
   (bind ((#rest r (bind-exit (e) (unwind-protect (values 1 2) (e))))) (print r))
   (bind ((#rest r (bind-exit (e) (unwind-protect (values 1 2) (e 7))))) (print r))
   (define-method f () (bind-exit (e) (unwind-protect (values 1 2) (e 3))))
   (bind ((#rest r (f))) (print r))
   (bind ((a b (values 1 2))) (bind ((c d (values 3))) (print (list a b c d))))"
  "(3 #f)
(4 5 #f)
()
(7)
(3)
(1 2 3 #f)
")

 ;; Chapter 8: it is an error for a case key to appear in more than one
 ;; match list; no two strings are id?.
 (case-key-twice
  "(print (case 1 ((1 2) 'a) ((2 3) 'b)))"
  ""
  :compile-error "2 appears in more than one match list")
 (case-strings-not-twice
  "(print (case \"a\" ((\"a\" \"a\") 1) (else: 2)))"
  "2
")

 ;; Chapter 13: a handler-case clause takes the options condition:, a
 ;; variable, test: and description:, and no others.
 (handler-case-options
  "(print (handler-case (error \"x\")
            ((<error> condition: c test: (method (c) #t) description: \"d\")
             (condition-format-string c))))"
  "\"x\"
")
 (handler-case-unknown-option
  "(print (handler-case 1 ((<error> foo: 3) 2)))"
  ""
  :compile-error "handler-case: unknown option foo:")
 (handler-case-odd-options
  "(print (handler-case 1 ((<error> condition:) 2)))"
  ""
  :compile-error "handler-case: bad keyword options")
 (handler-case-condition-not-a-variable
  "(print (handler-case 1 ((<error> condition: 5) 2)))"
  ""
  :compile-error "5 is not a variable name")

 ;; A for binds each variable once.
 (for-variable-twice
  "(for ((i 0 (+ i 1)) (i 0 (+ i 1))) ((> i 2)) (print i))"
  ""
  :compile-error "for: i appears twice")

 ;; So does a for-each.
 (for-each-variable-twice
  "(for-each ((x (list 1)) (x (list 2))) () (print x))"
  ""
  :compile-error "for-each: x appears twice")

 ;; Chapter 10: make of a <generic-function> needs a count of required
 ;; arguments.
 (generic-function-required-count
  "(print (function-arguments (make <generic-function> required: 2)))
   (make <generic-function> required: -1)"
  "2
"
  :error "needs required:, an integer not below 0")

 ;; Chapter 13: a <simple-error> or <simple-warning> made without a
 ;; format string has an empty message; check-type signals an error, from
 ;; which a handler may not return.
 (condition-defaults-and-check-type
  "(print (make <simple-error>))
   (print (make <simple-warning>))
   (print (list (condition-format-string (make <simple-error>))
                (condition-format-arguments (make <simple-warning>))))
   (print (handler-case (check-type 'a <integer>) ((<type-error>) 'caught)))
   (handler-bind (<type-error> (method (c next) 'returned))
     (print (check-type 'a <integer>)))
   (print 'not-reached)"
  "

(\"\" ())
caught
"
  :error "a handler returned from error: a is not an instance of {the class <integer>}")

 ;; Error messages as a compiled program words them: the count of the
 ;; arguments a call gave, and an unassigned setter variable by its name.
 (argument-count-message
  "((method () 1) 1 2)"
  ""
  :error "wrong number of arguments (2)")
 (argument-count-message-generic
  "(define-method f2 (a b) a)
   (f2 1)"
  ""
  :error "wrong number of arguments (1)")
 (unassigned-setter-message
  "(set! (VV 1) 2)"
  ""
  :error "the variable (setter VV)@dylan-user has no value")
 (unassigned-setter-variable-message
  "(set! (setter undefined-thing) 5)"
  ""
  :error "the variable (setter undefined-thing)@dylan-user has no value")

 ;; A handler may make an error from a trap -- a call with the wrong
 ;; number of arguments, a floating-point overflow, division by zero, a
 ;; value of the wrong type -- whose handler makes another, more deeply
 ;; than the host's signal handling nests.
 (trap-errors-nest-deeply
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
   (define big 1.0e300)
   (define-method nest (n k thunk)
     (if (= n 0)
         (k 'bottom)
         (handler-bind (<error> (method (c next) (print (format #f \"~a: ~a\" n c)) (nest (- n 1) k thunk)))
           (thunk))))
   (print (try (method () (bind-exit (k) (nest 12 k (method () ((method (a) a))))))))
   (print (try (method () (bind-exit (k) (nest 12 k (method () (* big big)))))))
   (print (try (method () (bind-exit (k) (nest 12 k (method () (/ 1 0)))))))
   (print (try (method () (bind-exit (k) (nest 12 k (method () (car 5)))))))
   (print 'after)"
  "\"12: wrong number of arguments (0)\"
\"11: wrong number of arguments (0)\"
\"10: wrong number of arguments (0)\"
\"9: wrong number of arguments (0)\"
\"8: wrong number of arguments (0)\"
\"7: wrong number of arguments (0)\"
\"6: wrong number of arguments (0)\"
\"5: wrong number of arguments (0)\"
\"4: wrong number of arguments (0)\"
\"3: wrong number of arguments (0)\"
\"2: wrong number of arguments (0)\"
\"1: wrong number of arguments (0)\"
bottom
\"12: arithmetic error: floating-point-overflow\"
\"11: arithmetic error: floating-point-overflow\"
\"10: arithmetic error: floating-point-overflow\"
\"9: arithmetic error: floating-point-overflow\"
\"8: arithmetic error: floating-point-overflow\"
\"7: arithmetic error: floating-point-overflow\"
\"6: arithmetic error: floating-point-overflow\"
\"5: arithmetic error: floating-point-overflow\"
\"4: arithmetic error: floating-point-overflow\"
\"3: arithmetic error: floating-point-overflow\"
\"2: arithmetic error: floating-point-overflow\"
\"1: arithmetic error: floating-point-overflow\"
bottom
\"12: division by zero\"
\"11: division by zero\"
\"10: division by zero\"
\"9: division by zero\"
\"8: division by zero\"
\"7: division by zero\"
\"6: division by zero\"
\"5: division by zero\"
\"4: division by zero\"
\"3: division by zero\"
\"2: division by zero\"
\"1: division by zero\"
bottom
\"12: not a list: 5\"
\"11: not a list: 5\"
\"10: not a list: 5\"
\"9: not a list: 5\"
\"8: not a list: 5\"
\"7: not a list: 5\"
\"6: not a list: 5\"
\"5: not a list: 5\"
\"4: not a list: 5\"
\"3: not a list: 5\"
\"2: not a list: 5\"
\"1: not a list: 5\"
bottom
after
")

 ;; Chapter 3 lets the function of a call be evaluated before, during or
 ;; after the arguments; both evaluate it first.
 (call-operator-first
  "(define-method msg (thunk)
     (handler-case (thunk)
       ((<error> condition: c)
        (apply format #f (condition-format-string c) (condition-format-arguments c)))))
   (define trail '())
   (define-method note (x) (set! trail (cons x trail)) x)
   (define f (method (a b) (list 'f a b)))
   (define g (method (a b) (list 'g a b)))
   ((note f) (note 1) (note 2))
   (print (map (method (x) (if (instance? x <function>) 'fn x)) (reverse trail)))
   (print (f (begin (set! f g) 1) 2))
   (set! trail '())
   (print (msg (method () (5 (note 'arg)))))
   (print (msg (method () (undefined-function (note 'never)))))
   (print (reverse trail))"
  "(fn 1 2)
(f 1 2)
\"not a function: 5\"
\"the variable undefined-function@dylan-user has no value\"
(arg)
")

 ;; Chapter 10: the scope of bind-methods' names includes the parameter
 ;; lists, so a specializer may name a method made before its own.
 (bind-methods-specializers-name-methods
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
   (print (bind-methods ((g () 'gee) (f ((x (singleton g))) 'is-g)) (f g)))
   (print (try (method () (bind-methods ((g () 'gee) (f ((x (singleton g))) 'is-g)) (f 1)))))
   (print (bind-methods ((g () <integer>) (f ((x (g))) (list 'int x))) (f 5)))
   (define-method mk (flag)
     (bind-methods ((even? (n) (if (= n 0) #t (odd? (- n 1))))
                    (odd? (n) (if (= n 0) #f (even? (- n 1))))
                    (pick ((x (if flag (singleton even?) <integer>)))
                      (list 'pick (if (instance? x <integer>) (odd? x) (x 4)))))
       (pick (if flag even? 7))))
   (print (list (mk #f) (mk #t)))"
  "is-g
\"ERR: 1 is not an instance of {singleton {an anonymous method}}\"
(int 5)
((pick #t) (pick #t))
")

 ;; Chapter 10, bind-methods, and chapter 7, set!: a variable bind-methods
 ;; binds can be assigned like any other lexical variable; its function
 ;; is then a value whose calls are unknown.
 (bind-methods-assigned
  "(print (bind-methods ((f () 1)) (set! f 2) f))
   (define-method h (n)
     (bind-methods ((even (m) (if (= m 0) #t (odd (- m 1))))
                    (odd (m) (if (= m 0) #f (even (- m 1)))))
       (bind ((first (even n)))
         (set! odd (method (m) 'replaced))
         (list first (even 3)))))
   (print (h 10))
   (define-method k ()
     (bind-methods ((f () 'a))
       (bind ((old f))
         (set! f (method () 'b))
         (list (old) (f)))))
   (print (k))"
  "2
(#t replaced)
(a b)
")

 ;; Chapter 13, handler-bind: the type, function and test are checked when
 ;; the handler is established, so that a bad one is an error a handler
 ;; outside can catch; any class or singleton will do.
 (handler-bind-checks
  "(define-method msg (thunk) (handler-case (thunk) ((<error> condition: c) (format #f \"~a\" c))))
   (print (msg (method () (handler-bind (5 (method (c n) 1)) (signal \"x\")))))
   (print (msg (method () (handler-case (signal \"x\") ((5) 1)))))
   (print (msg (method () (handler-bind (<error> 5) 1))))
   (print (msg (method () (handler-bind (<error> (method (c n) 1) test: 7) 1))))
   (print (handler-case (error \"x\") ((<object>) 'caught)))
   (print (handler-bind ((singleton 3) (method (c n) 1)) 'body))
   (print (handler-bind (<warning> (method (c n) 'handled) test: #f) (signal \"w\")))"
  "\"handler-bind: not a class or singleton: 5\"
\"handler-bind: not a class or singleton: 5\"
\"handler-bind: not a function: 5\"
\"handler-bind: the test: is not a function: 7\"
caught
body
handled
")

 ;; Chapter 13 has no condition wall: an error a handler causes is
 ;; signalled to the handlers, however deeply such errors nest -- the
 ;; runtime's own errors (here no method for binary+, and a type error)
 ;; as much as error's.
 (handler-error-nesting
  "(define count 0)
   (print (bind-exit (k)
     (handler-bind (<error> (method (c next) (set! count (+ count 1))
                              (if (< count 50) (+ count 'x) (k count))))
       (+ 0 'x))))
   (set! count 0)
   (print (bind-exit (k)
     (handler-bind (<error> (method (c next) (set! count (+ count 1))
                              (if (< count 50) (check-type count <string>) (k count))))
       (check-type 1 <string>))))"
  "50
50
")

 ;; Chapter 8, unwind-protect, and chapter 13, handler-bind: they return
 ;; the values of their forms, and a bind-exit the values of its body, even
 ;; at the end of a function that had called one returning two values.
 (protected-values-at-return
  "(define-method two () (values 1 2))
   (define-method g (x) (two) (unwind-protect x 'cleanup))
   (define-method h (x) (two) (handler-bind (<error> (method (c n) 'h)) x))
   (define keep #f)
   (define-method k (x) (two) (bind-exit (e) (set! keep e) x))
   (bind ((a b (apply g '(5)))) (print (list a b)))
   (bind ((a b (apply h '(5)))) (print (list a b)))
   (bind ((a b (apply k '(5)))) (print (list a b)))
   (bind ((a b (g 5))) (print (list a b)))
   (bind ((a b (h 5))) (print (list a b)))
   (bind ((a b (k 5))) (print (list a b)))"
  "(5 #f)
(5 #f)
(5 #f)
(5 #f)
(5 #f)
(5 #f)
")

 ;; Chapter 13, signal: "If a handler returns, all the values that it
 ;; returned are returned from signal" -- not its test's; and do-handlers
 ;; answers #f alone, whatever its function answers.
 (signal-answers-handler-values
  "(define-class <note> (<warning>))
   (define-method run ()
     (handler-bind (<note> (method (c n) 5) test: (method (c) (values #t 'x)))
       (signal (make <note>))))
   (bind ((a #rest r (run))) (print (list a r)))
   (define-method run2 ()
     (handler-bind (<note> (method (c n) 'outer))
       (handler-bind (<note> (method (c n) (n)) test: (method (c) (values #t 'y 'z)))
         (signal (make <note>)))))
   (bind ((a #rest r (run2))) (print (list a r)))
   (bind ((a #rest r (handler-bind (<note> (method (c n) 'h))
                       (do-handlers (method (#rest x) (values 1 2))))))
     (print (list a r)))"
  "(5 ())
(outer ())
(#f ())
")
 ;; A module variable used before its definition has no value, though
 ;; the use is unused, through an alias, or in a call inlined
 (unbound-before-definition
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-method f () (bind ((x later-var)) 'ok))
(print (try (method () (f))))
(define-method g () (my-car '(1 2)))
(print (try (method () (g))))
(define-method h () (ff 1))
(print (try (method () (h))))
(define later-var 1)
(define my-car car)
(define ff (method (x) (+ x 1)))
(print (list (f) (g) (h)))"
  "\"ERR: the variable later-var@dylan-user has no value\"
\"ERR: the variable my-car@dylan-user has no value\"
\"ERR: the variable ff@dylan-user has no value\"
(ok 1 2)
")
 ;; Chapter 6: with #key, the arguments after the required ones are
 ;; keyword/value pairs, with #rest too; #key alone accepts no keyword,
 ;; however the method is called
 (keyword-pairs-checked
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define mr (method (#rest r #key a) (list r a)))
(print (try (method () (mr 1 2))))
(print (try (method () (mr b: 1 a: 2))))
(define-method gk (#key) 'ok)
(print (try (method () (gk 2 3))))
(print (try (method () (gk))))
(define-method h (x #key) x)
(print (try (method () (h 1 color: 'red))))
(print (try (method () ((identity h) 1 a: 2))))
(print (try (method () ((method (a #key b) a) 1 2 3))))
(print (try (method () ((method (a #key b) (list a b)) 1 b: 3))))
(define-method k2 (x #key y) (list x y))
(print (try (method () (k2 1 2 3))))
(print (try (method () (k2 1 y: 3))))
(define-method k3 (x #rest r #key y) (list x r y))
(print (try (method () (k3 1 2 3))))
(print (try (method () (k3 1 z: 3))))"
  "\"ERR: the arguments after the required ones are not keyword/value pairs\"
((b: 1 a: 2) 2)
\"ERR: the arguments after the required ones are not keyword/value pairs\"
ok
\"ERR: keyword argument not accepted: color:\"
\"ERR: keyword argument not accepted: a:\"
\"ERR: the arguments after the required ones are not keyword/value pairs\"
(1 3)
\"ERR: the arguments after the required ones are not keyword/value pairs\"
(1 3)
\"ERR: the arguments after the required ones are not keyword/value pairs\"
(1 (z: 3) #f)
")
 ;; #rest with an empty #key accepts any keyword (chapter 6), still in
 ;; keyword/value pairs
 (rest-with-empty-key
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(print ((method (#rest r #key) r) a: 1))
(print (try (method () ((method (#rest r #key) r) a:))))
(print (try (method () ((method (#rest r #key) r) 5 6))))
(print (try (method () ((method (#key) 'ok) a: 1))))
(print ((method (#key) 'ok)))
(define-method m7 (x #rest r #key) (list x r))
(print (m7 1 b: 2 c: 3))
(define-generic-function g7 (x #rest r #key))
(define-method g7 (x #rest r #key) r)
(print (g7 1 z: 2))"
  "(a: 1)
\"ERR: the arguments after the required ones are not keyword/value pairs\"
\"ERR: the arguments after the required ones are not keyword/value pairs\"
\"ERR: keyword argument not accepted: a:\"
ok
(1 (b: 2 c: 3))
(z: 2)
")
 ;; A recursion too deep for the stack is an error a handler can catch,
 ;; again and again
 (stack-exhausted
  "(define-method count-up (n) (if (= n 0) 0 (+ 1 (count-up (- n 1)))))
(print (handler-case (count-up 100000000) ((<error> condition: c) (list 'caught c))))
(print (count-up 1000))
(print (handler-case (count-up 100000000) ((<error> condition: c) (list 'again c))))
(define-method deep (n) (handler-case (if (= n 0) 0 (+ 1 (deep (- n 1)))) ((<error>) 'inner)))
(print (handler-case (deep 100000000) ((<error>) 'outer)))
(print 'after)"
  "(caught the stack is exhausted)
1000
(again the stack is exhausted)
inner
after
")
 ;; A condition's message prints its arguments through print-object, ~a as
 ;; display and ~s as write, and a condition prints through its class's
 ;; print-object, or as its class if it has no message.
 (condition-messages
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <pt> (<object>) (x init-keyword: x:))
(define-method print-object ((p <pt>) stream escape?) (format stream \"{pt ~a}\" (x p)))
(define pt (make <pt> x: 3))
(print (format #f \"~a\" pt))
(print (try (method () (error \"bad ~a\" pt))))
(print (try (method () (error \"bad ~s\" pt))))
(print (try (method () (error \"list ~a\" '(\"s\" #\\c)))))
(print (try (method () (error \"list ~s\" '(\"s\" #\\c)))))
(print (try (method () (error \"tbl ~a\" (make <table>)))))
(print (try (method () (error \"rng ~a\" (range from: 1 size: 2)))))
(print (try (method () (error \"dq ~a\" (make <deque> size: 2)))))
(print (try (method () (error \"cond ~a\" (make <simple-error> format-string: \"inner ~a\" format-arguments: '(1))))))
(print (try (method () (error \"str ~a and ~s\" \"x\" \"y\"))))
(print (try (method () (error \"chr ~a and ~s\" #\\x #\\y))))
(print (try (method () (error \"extra\" pt \"s\"))))
(print (try (method () (check-type pt <integer>))))
(print (format #f \"~a / ~s\" (make <table>) (range from: 1 size: 2)))
(define-class <oops> (<error>))
(define-method print-object ((c <oops>) stream escape?) (format stream \"OOPS!\"))
(print (try (method () (error (make <oops>)))))
(define-class <w> (<warning>))
(define-method print-object ((c <w>) stream escape?) (format stream \"W!\"))
(define-class <sw> (<simple-warning>))
(define-method print-object ((c <sw>) stream escape?) (format stream \"SW!\"))
(define-class <plain> (<error>))
(print (try (method () (error (make <plain>)))))
(print (format #f \"~s\" (make <plain>)))
(print (try (method () (error \"sing ~a\" (singleton 3)))))
(print (try (method () (error \"nested ~a\" (make <type-error> value: (make <pt> x: 1) type: <integer>)))))
(define-class <sb> (<simple-error>))
(define-method print-object ((c <sb>) stream escape?) (format stream \"SB!\"))
(print (try (method () (error (make <sb> format-string: \"x\")))))"
  "\"{pt 3}\"
\"ERR: bad {pt 3}\"
\"ERR: bad {pt 3}\"
\"ERR: list (s c)\"
\"ERR: list (\\\"s\\\" #\\\\c)\"
\"ERR: tbl {table}\"
\"ERR: rng {range 1 by 1 size 2}\"
\"ERR: dq {<deque> #f #f}\"
\"ERR: cond inner 1\"
\"ERR: str x and \\\"y\\\"\"
\"ERR: chr x and #\\\\y\"
\"ERR: extra {pt 3} s\"
\"ERR: {pt 3} is not an instance of {the class <integer>}\"
\"{table} / {range 1 by 1 size 2}\"
\"ERR: OOPS!\"
\"ERR: {instance of <plain>}\"
\"{instance of <plain>}\"
\"ERR: sing {singleton 3}\"
\"ERR: nested {pt 1} is not an instance of {the class <integer>}\"
\"ERR: SB!\"
")
 ;; disjoin and conjoin answer all the values of the last predicate, as or
 ;; and and do, and are new functions even of one predicate.
 (disjoin-conjoin-values
  "(define two (method (x) (values x 'second)))
(define no (method (x) #f))
(define yes (method (x) 'yes))
(print (bind ((a b ((disjoin no two) 1))) (list a b)))
(print (bind ((a b ((disjoin two no) 1))) (list a b)))
(print (bind ((a b ((conjoin yes two) 1))) (list a b)))
(print (bind ((a b ((conjoin no two) 1))) (list a b)))
(print (bind ((a b ((disjoin two) 1))) (list a b)))
(print (bind ((a b ((conjoin two) 1))) (list a b)))
(print (id? (disjoin even?) even?))
(print (id? (conjoin even?) even?))
(print (map (disjoin even? zero?) '(1 2 3)))
(print (map (conjoin odd? (method (x) (> x 1))) '(1 2 3)))
(print ((disjoin no no) 1))
(print ((conjoin yes yes) 1))"
  "(1 second)
(1 #f)
(1 second)
(#f #f)
(1 second)
(1 second)
#f
#f
(#f #t #f)
(#f #f #t)
#f
yes
")

 ;; Chapter 8 forbids a match in two match lists, not twice in one.
 (case-repeated-match
  "(print (case 1 ((1 1) 'a) (else: 'b)))
   (print (case 2 ((1 2 2) 'x) ((3) 'y)))"
  "a
x
")

 (case-match-in-two-lists
  "(print (case 1 ((1 2) 'a) ((3 1) 'b)))"
  ""
  :compile-error "case: 1 appears in more than one match list")

 ;; Both dylan modules export the same names: the threads, gensym,
 ;; lisp-variable and lisp-procedure, whether or not they work there.
 (dylan-module-thread-names
  "(define make-thread 3)"
  ""
  :compile-error "define of make-thread: the variable is imported from module dylan")

 (dylan-module-interactive-names
  "(define gensym 1)"
  ""
  :compile-error "define of gensym: the variable is imported from module dylan")

 (dylan-module-thread-classes
  "(print (list (instance? 1 <lock>) (subclass? <recursive-lock> <lock>)
               (subclass? <thread-error> <error>)
               (thread-error-condition (make <thread-error> condition: 5))))
   (handler-case (error (make <thread-error> condition: 5))
     ((<error> condition: c) (print (format #f \"~a\" c))))"
  "(#f #t #t 5)
\"a thread ended with an unhandled condition: 5\"
")
 ;; An error a handler signals, even one the implementation's own operations
 ;; signal, goes to the handlers in effect -- the same one too: there is no
 ;; condition wall (chapter 13).
 (error-in-handler-is-signalled
  "(define count 0)
(print (bind-exit (k)
  (handler-bind (<error> (method (c n) (set! count (+ count 1))
                           (if (< count 2) (car 5) (k 'done))))
    (car 5))))"
  "done
")
 ;; A handler called with the wrong number of arguments is an error, which
 ;; the handlers in effect -- that one again -- are offered, until the
 ;; error is reported (chapter 13).
 (handler-of-wrong-arity
  "(handler-bind (<error> (method (c) 'x)) (error \"e\"))
(print 'after)"
  ""
  :error "wrong number of arguments (2)")
 ;; Errors signalled by handlers, one inside another, end as a recursion
 ;; too deep does: "the stack is exhausted" is signalled, and a handler can
 ;; catch it.
 (stack-exhausted-in-handlers
  "(define depth 0)
(define deepest 0)
(print (bind-exit (done)
  (handler-bind (<error> (method (c n)
                           (set! depth (+ depth 1))
                           (set! deepest (max depth deepest))
                           (if (= (condition-format-string c) \"the stack is exhausted\")
                               (done (condition-format-string c))
                               (error \"again\"))))
    (error \"e\"))))
(print (> deepest 1000))
(print (bind-exit (done)
  (handler-bind (<error> (method (c n)
                           (if (= (condition-format-string c) \"the stack is exhausted\")
                               (done (condition-format-string c))
                               (car 5))))
    (car 5))))
(define n 0)
(print (bind-exit (done)
  (handler-bind (<error> (method (c next) (set! n (+ n 1)) (if (> n 5) (done n) (car 5))))
    (car 5))))
(print 'after)"
  "\"the stack is exhausted\"
#t
\"the stack is exhausted\"
6
after
")
 ;; A handler that signals an error each time it is called ends the
 ;; program as a recursion too deep would.
 (handler-signals-without-end
  "(handler-bind (<error> (method (c n) (error \"again\"))) (error \"e\"))
(print (quote after))"
  ""
  :error "the stack is exhausted")
 ;; Errors of the implementation's own operations are worded alike in both
 ;; implementations (the user manual, chapter 11).
 (error-wording-agrees
  "(define-method try (n f) (print (list n (handler-case (f) ((<error> condition: c) (format #f \"~a\" c))))))
(define-class <rk> (<object>) (y required-init-keyword: y:))
(try 1 (method () (case 9 ((3) 'three))))
(try 2 (method () (select 5 < ((1) 'a))))
(try 3 (method () (make <rk>)))
(try 4 (method () (make <condition>)))
(try 5 (method () (make <error>)))
(try 6 (method () (make <generic-function>)))
(try 7 (method () (as <character> -1)))
(try 8 (method () (as <character> 2000000)))
(try 9 (method () (as <integer> 2.5)))
(try 10 (method () (handler-bind (<error> 5) (error \"x\"))))
(try 11 (method () (member? 1 '(1 2) test: 5)))
(try 12 (method () (apply list 1 2)))
(try 13 (method () (apply list)))
(try 14 (method () (apply list (make <table>))))
(try 15 (method () (for-each ((x 5)) () x)))
(try 16 (method () (slot-value 5 5)))
(try 17 (method () (instance? 1 2)))
(try 18 (method () (subclass? <integer> 5)))
(try 19 (method () (seal 5)))
(try 20 (method () (make-read-only 5)))
(try 21 (method () (logbit? -1 5)))
(try 22 (method () (dotimes (i 'x) i)))
(try 23 (method () (check-type 4 (singleton 3))))
(try 24 (method () (bind (((x 5) 4)) x)))
(define x 5)
(try 25 (method () `(1 ,@x)))
(try 26 (method () (pop (make <deque>))))
(try 27 (method () (reduce1 + '())))
(try 28 (method () (reverse (range from: 1))))
(try 29 (method () (pop-last (make <deque>))))
(try 30 (method () (format 5 \"x\")))
(try 31 (method () (format #f 5)))
(try 35 (method () (reverse! (range from: 1))))
(try 36 (method () (print 1 stream: 5)))
(try 37 (method () (format #f \"a~ab\" 1)))
(try 38 (method () (write-line \"x\" stream: 'y)))"
  "(1 \"case: no clause matches 9\")
(2 \"select: no clause matches 5\")
(3 \"make: the required init-keyword is missing: y:\")
(4 \"cannot make an instance of this class: {the class <condition>}\")
(5 \"cannot make an instance of this class: {the class <error>}\")
(6 \"make: a <generic-function> needs required:, an integer not below 0\")
(7 \"not a character code: -1\")
(8 \"not a character code: 2000000\")
(9 \"as: 2.5 cannot be converted to {the class <integer>}\")
(10 \"handler-bind: not a function: 5\")
(11 \"not a function: 5\")
(12 \"apply: the last argument is not a sequence: 2\")
(13 \"wrong number of arguments (1)\")
(14 \"apply: the last argument is not a sequence: {table}\")
(15 \"no applicable method for initial-state\")
(16 \"not a slot descriptor: 5\")
(17 \"not a class or singleton: 2\")
(18 \"not a class: 5\")
(19 \"no applicable method for seal\")
(20 \"no applicable method for make-read-only\")
(21 \"logbit?: negative index: -1\")
(22 \"no applicable method for binary<\")
(23 \"4 is not an instance of {singleton 3}\")
(24 \"not a class or singleton: 5\")
(25 \"as: 5 cannot be converted to {the class <list>}\")
(26 \"pop: the deque is empty\")
(27 \"reduce1: the collection is empty\")
(28 \"reverse: the range is unbounded\")
(29 \"pop-last: the deque is empty\")
(30 \"not a stream: 5\")
(31 \"format: not a string: 5\")
(35 \"reverse: the range is unbounded\")
(36 \"not a stream: 5\")
(37 \"a1b\")
(38 \"not a stream: y\")
")
 ;; Two loops in a function with a specialized entry, one in an argument
 ;; of a call: each label once in each of the function's C functions.
 (loop-labels-in-each-entry
  "(define-method f (a)
  (list 0) (list 1) (list 2) (list 3) (list 4) (list 5) (list 6) (list 7)
  (if a
      (list (for ((i 0 (+ i 1))) ((= i 3) i)))
      (for ((j 0 (+ j 1))) ((= j 4) j))))
(print (list (f #t) (f #f)))
(define-method g ((x <double-float>))
  (list 0) (list 1) (list 2) (list 3) (list 4) (list 5) (list 6) (list 7)
  (if (> x 0.0)
      (list (for ((i 0 (+ i 1))) ((= i 3) (* x i))))
      (for ((j 0 (+ j 1))) ((= j 4) (* x j)))))
(print (list (g 1.5) (g -1.5)))"
  "((3) 4)
((4.5) -6.0)
")
 ;; A symbol is not a function, though the library calls it: not the
 ;; Lisp function of its name in the interpreter.
 (library-calls-only-functions
  "(define-method msg (f) (handler-case (f) ((<error> condition: c) (format #f \"~a\" c))))
(print (msg (method () (sort '(3 1 2) test: 'list))))
(print (msg (method () ((curry 'list 1) 2))))
(print (msg (method () ((rcurry 'list 1) 2))))
(print (msg (method () ((compose 'list 'list) 2))))
(print (msg (method () ((complement 'list) 2))))
(print (msg (method () (member? 1 '(1 2) test: 'list))))
(print (msg (method () (remove '(1 2) 1 test: 'list))))
(print (msg (method () (do-handlers 'list))))
(print (msg (method () (remove-duplicates '(1 2) test: 'list))))
(print (msg (method () (intersection '(1 2) '(2) test: 'list))))
(print (msg (method () (subsequence-position \"abc\" \"b\" test: 'list))))
(define-class <q> (<object>) (a init-function: 'cons))
(print (msg (method () (a (make <q>)))))"
  "\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: list\"
\"not a function: cons\"
")
 ;; for-each over collections that are not all sequences aligns them by
 ;; key, as do does (chapter 12, "Collection Alignment"); sequences alone
 ;; are walked in step.
 (for-each-aligns-by-key
  "(define t2 (make <table>)) (set! (element t2 'a) 1) (set! (element t2 'b) 2)
(define t3 (make <table>)) (set! (element t3 'b) 20) (set! (element t3 'c) 30)
(for-each ((x t2) (y t3)) () (print (list x y)))
(define t4 (make <table>))
(set! (element t4 2) 'two) (set! (element t4 0) 'zero) (set! (element t4 7) 'seven)
(for-each ((x '(a b c)) (y t4)) () (print (list x y)))
(for-each ((x (range from: 0)) (y t4)) () (print (list x y)))
(for-each ((x (make <array> dimensions: '(2 2) fill: 'e)) (y '(1 2 3))) () (print 'never))
(print (for-each ((x t2) (y t3)) ((= x 2) (list 'stop x y)) (print 'no)))
(print (for-each ((x '(1 2 3)) (y #(4 5 6))) ((= x 2) 'stop) (print (list x y))))"
  "(2 20)
(a zero)
(c two)
(0 zero)
(2 two)
(7 seven)
(stop 2 20)
(1 4)
stop
")
 ;; case's and select's clauses, and quote's form, are checked alike in
 ;; both implementations, with the same words.
 (case-else-not-last
  "(case 1 (else: 'e) ((1) 'one))"
  ""
  :compile-error "case: the else: clause must be the last")
 (case-bad-match-list
  "(case 1 (5 'a))"
  ""
  :compile-error "case: bad match list 5")
 (select-bad-clause
  "(select 1 = 5)"
  ""
  :compile-error "select: bad clause 5")
 (quote-bad-syntax
  "(print (quote . a))"
  ""
  :compile-error "quote: bad syntax")
 ;; A handler may not return from an error the implementation signals,
 ;; as from one error signals (chapter 13).
 (handler-returns-from-trap-error
  "(print 1)
(print (handler-bind (<error> (method (c next) 'ret)) (car 5)))
(print 2)"
  "1
"
  :error "a handler returned from error: not a list: 5")
 ;; An error raised each time a condition is made, for the error before
 ;; it, is reported at last, not signalled without end: both stop at once
 (error-making-each-condition
  "(define-method initialize ((x <object>) #key) (next-method))
(print 'before)
(car 5)"
  "before
"
  :error "keyword argument not accepted: format-string:")
 (error-in-each-condition-initialize
  "(define-method initialize ((x <simple-error>) #rest r) (car 5))
(print 'before)
(print (handler-case (car 6) ((<error> condition: c) 'caught)))
(print 'after)"
  "before
"
  :error "not a list: 5")
 ;; A bind-methods specializer naming a method made after its own: the name
 ;; has no value yet when the specializer is evaluated.
 (bind-methods-later-name-unbound
  "(define-method try (f)
  (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(print (try (method () (bind-methods ((f ((x (singleton g))) 'is-g) (g () 'gee)) (f '())))))
(print (try (method () (bind-methods ((f ((x (singleton g))) 'is-g) (g () 'gee)) (f g)))))
(print (try (method () (bind-methods ((f () (g)) (g () 'gee)) (f)))))
(print (try (method () (bind-methods ((g () 'gee) (f ((x (singleton g))) 'is-g)) (f g)))))"
  "\"ERR: the variable g has no value\"
\"ERR: the variable g has no value\"
gee
is-g
")
 ;; Chapter 8: a key in two match lists is an error -- an extended float
 ;; too, keys being compared as case compares them, by id?.  One list
 ;; may repeat a key.
 (case-extended-float-key-twice
  "(define e 2.0l0)
(print (list (case e ((2.0l0 2.0l0 1.0l0) 'a) (else: 'c))
             (case e ((2.0l0) 'a) ((2.0l0) 'b) (else: 'c))))"
  ""
  :compile-error "case: 2.0l0 appears in more than one match list")
 ;; A malformed special form is reported in the same words by both
 ;; implementations: each names the form and the part that is wrong.
 (bind-binding-without-init
  "(bind ((x)) x)"
  ""
  :compile-error "bind: bad binding (x)")
 (bind-binding-not-a-list
  "(bind (x) 1)"
  ""
  :compile-error "bind: bad binding x")
 (bind-bad-variable
  "(bind ((1 2)) 1)"
  ""
  :compile-error "bind: bad variable 1")
 (bind-bad-rest
  "(bind ((#rest a b 1)) 1)"
  ""
  :compile-error "bind: bad #rest in (#rest a b 1)")
 (define-setter-not-a-variable
  "(define setter 5)"
  ""
  :compile-error "define: setter is not a variable name")
 (dotimes-bad-clause
  "(dotimes (i) 1)"
  ""
  :compile-error "dotimes: bad (var count [result]) (i)")
 (for-bad-variable-clause
  "(for ((i 0 1 2)) (#t))"
  ""
  :compile-error "for: bad variable clauses ((i 0 1 2))")
 (for-bad-end-test
  "(for ((i 0 1)) 5)"
  ""
  :compile-error "for: bad end test 5")
 (for-each-bindings-not-a-list
  "(for-each (x '(1)) () 1)"
  ""
  :compile-error "for-each: bad bindings (x (quote (1)))")
 (for-each-bad-variable
  "(for-each ((1 '(1))) () 1)"
  ""
  :compile-error "for-each: bad bindings ((1 (quote (1))))")
 (begin-dotted
  "(begin . 5)"
  ""
  :compile-error "begin: bad syntax")
 (and-dotted
  "(and . 5)"
  ""
  :compile-error "and: bad syntax")
 (call-dotted
  "(define-method f (x) x)
(f . 1)"
  ""
  :compile-error "(f . 1) is not a Dylan expression")
 (bind-exit-bad-variable
  "(bind-exit k 1)"
  ""
  :compile-error "bind-exit: bad exit variable k")
 (method-parameters-not-a-list
  "(method x 1)"
  ""
  :compile-error "method: bad parameter list x")
 (method-next-after-rest
  "(method (#rest r #next n) r)"
  ""
  :compile-error "method: unexpected #next in parameter list")
 (method-key-twice
  "(method (a #key k k) k)"
  ""
  :compile-error "method: k appears twice")
 (define-method-not-a-variable
  "(define-method 5 () 1)"
  ""
  :compile-error "define-method: 5 is not a variable name")
 (define-generic-function-specialized
  "(define-generic-function g ((a <integer>)))"
  ""
  :compile-error "define-generic-function: bad parameter (a <integer>)")
 (bind-methods-bad-binding
  "(bind-methods ((f)) 1)"
  ""
  :compile-error "bind-methods: bad binding (f)")
 (bind-methods-bad-name
  "(bind-methods ((5 () 1)) 1)"
  ""
  :compile-error "bind-methods: bad binding (5 () 1)")
)
