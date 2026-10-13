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
   (print (handler-case (* 4611686018427387903 2) ((<error>) 'overflow)))
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
)
