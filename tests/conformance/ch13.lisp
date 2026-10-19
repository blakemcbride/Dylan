;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 13 of the 1992 manual, "Conditions"
;;;; (manual/Dylan-1992/book.annotated/ch13.html), in the form of
;;;; tests/codegen/runner.lisp.  The chapter shows no results for its
;;;; examples, so every expected value here is what its specification
;;;; says; the chapter's examples are run in the form described with
;;;; ch13-file-not-found.

(
 ;; "Classes": the hierarchy of the picture and the class descriptions:
 ;; <serious-condition> under <condition>, <error> under
 ;; <serious-condition>, <simple-error> and <type-error> under <error>,
 ;; <warning> under <condition>, <simple-warning> under <warning>,
 ;; <restart> under <condition>, <simple-restart> and <abort> under
 ;; <restart>.  From the specification.
 (ch13-classes
  "(print (list (subclass? <condition> <object>) (subclass? <serious-condition> <condition>)
                (subclass? <error> <serious-condition>) (subclass? <simple-error> <error>)
                (subclass? <type-error> <error>)))
   (print (list (subclass? <warning> <condition>) (subclass? <simple-warning> <warning>)
                (subclass? <restart> <condition>) (subclass? <simple-restart> <restart>)
                (subclass? <abort> <restart>)))
   (print (list (subclass? <warning> <serious-condition>) (subclass? <restart> <serious-condition>)
                (subclass? <error> <warning>) (subclass? <abort> <simple-restart>)))
   (print (list (instance? (make <simple-error>) <condition>) (instance? (make <simple-warning>) <warning>)
                (instance? (make <simple-restart>) <restart>) (instance? (make <abort>) <condition>)
                (instance? (make <type-error> value: 1 type: <string>) <serious-condition>)))"
  "(#t #t #t #t #t)
(#t #t #t #t #t)
(#f #f #f #f)
(#t #t #t #t #t)
")

 ;; <simple-error>, <simple-warning> and <type-error>: their
 ;; initialization arguments and accessor functions.  <restart> accepts
 ;; condition:.  From the specification.
 (ch13-condition-accessors
  "(define e (make <simple-error> format-string: \"bad ~a\" format-arguments: '(1 2)))
   (print (list (condition-format-string e) (condition-format-arguments e)))
   (define w (make <simple-warning> format-string: \"careful ~s\" format-arguments: (list \"x\")))
   (print (list (condition-format-string w) (condition-format-arguments w)))
   (define te (make <type-error> value: 'a type: <integer>))
   (print (list (type-error-value te) (id? (type-error-expected-type te) <integer>)))
   (print (instance? (make <simple-restart> condition: e) <simple-restart>))
   (print (instance? (make <abort> condition: e) <abort>))"
  "(\"bad ~a\" (1 2))
(\"careful ~s\" (\"x\"))
(a #t)
#t
#t
")

 ;; signal [Sealed Function]: tries each active dynamic handler, the most
 ;; recent first; the values a handler returns are returned from signal;
 ;; a handler declines by calling next-handler with no arguments; if all
 ;; decline, (default-handler condition), which for <condition> returns
 ;; #f.  handler-bind [Macro]: returns the values of the last form, or #f
 ;; if there are none.  From the specification.
 (ch13-signal-and-handler-bind
  "(define-class <note> (<condition>))
   (print (signal (make <note>)))
   (print (handler-bind (<note> (method (c next-handler) (values 1 2)))
            (bind ((a b (signal (make <note>)))) (list a b))))
   (print (handler-bind (<note> (method (c next-handler) 'outer))
            (handler-bind (<note> (method (c next-handler) 'inner))
              (signal (make <note>)))))
   (print (handler-bind (<note> (method (c next-handler) 'outer))
            (handler-bind (<note> (method (c next-handler) (next-handler)))
              (signal (make <note>)))))
   (print (handler-bind (<note> (method (c next-handler) (next-handler)))
            (signal (make <note>))))
   (print (handler-bind (<note> (method (c next-handler) 'ignored))))
   (print (handler-bind (<note> (method (c next-handler) 'ignored)) 1 2 3))
   (bind ((x (make <note>)))
     (print (handler-bind (<note> (method (c next-handler) (id? c x))) (signal x))))"
  "#f
(1 2)
inner
outer
#f
#f
3
#t
")

 ;; handler-bind: type is evaluated and is a class or a singleton; a
 ;; handler applies to general instances of type; test, when given, must
 ;; also return true.  From the specification.
 (ch13-handler-bind-type-and-test
  "(define-class <note> (<condition>) (level init-keyword: level: init-value: 0))
   (define-class <loud-note> (<note>))
   (define note-class (method () <note>))
   (print (handler-bind ((note-class) (method (c n) 'by-class))
            (signal (make <loud-note>))))
   (print (handler-bind (<loud-note> (method (c n) 'loud))
            (signal (make <note>))))
   (bind ((x (make <note>)))
     (print (handler-bind ((singleton x) (method (c n) 'this-one))
              (list (signal x) (signal (make <note>))))))
   (define high? (method (c) (> (level c) 5)))
   (print (handler-bind (<note> (method (c n) 'high) test: high?)
            (list (signal (make <note> level: 9)) (signal (make <note> level: 1)))))
   (print (handler-bind (<note> (method (c n) 'outer))
            (handler-bind (<note> (method (c n) 'high) test: high?)
              (list (signal (make <note> level: 9)) (signal (make <note> level: 1))))))"
  "by-class
#f
(this-one #f)
(high #f)
(high outer)
")

 ;; handler-bind: "There is no condition wall, i.e., when executing
 ;; function the set of available handlers is not reset to the handlers
 ;; that were in effect when the handler-bind was entered."  So a handler
 ;; that signals reaches handlers established inside its handler-bind.
 ;; From the specification.
 (ch13-no-condition-wall
  "(define-class <a> (<condition>))
   (define-class <b> (<condition>))
   (print (handler-bind (<a> (method (c n) 'outer))
            (handler-bind (<b> (method (c n) (signal (make <a>))))
              (handler-bind (<a> (method (c n) 'inner))
                (signal (make <b>))))))"
  "inner
")

 ;; A handler may take a non-local exit (Overview), here through
 ;; bind-exit; the signal then does not return.  error with a string
 ;; signals a <simple-error> whose format string and arguments are
 ;; those given (\"Full set of Operators for Signaling\").  From the
 ;; specification.
 (ch13-error
  "(print (bind-exit (exit)
            (handler-bind (<error> (method (c n) (exit (list 'exited (condition-format-string c)
                                                           (condition-format-arguments c)))))
              (error \"oops ~a ~a\" 1 2)
              'not-reached)))
   (print (handler-case (error \"x\") ((<simple-error>) 'simple-error)))
   (define-class <my-error> (<error>) (code init-keyword: code:))
   (print (handler-case (error (make <my-error> code: 42)) ((<my-error> condition: c) (code c))))"
  "(exited \"oops ~a ~a\" (1 2))
simple-error
42
")

 ;; error never returns; \"if a handler returns, error bombs directly to
 ;; the debugger\", which ends this program.  From the specification.
 (ch13-error-handler-returns
  "(print 'before)
   (handler-bind (<error> (method (c n) 'returned))
     (error \"handler returned from this\")
     (print 'not-reached))"
  "before
"
  :error "handler returned from this")

 ;; signal with a string signals a <simple-warning>; the default handler
 ;; for <warning> displays the warning \"in a user-interface dependent
 ;; way\" and returns #f.  From the specification.
 (ch13-signal-string
  "(print (handler-case (signal \"foo ~a\" 1)
            ((<simple-warning> condition: c)
             (list (condition-format-string c) (condition-format-arguments c)))))
   (print (handler-bind (<warning> (method (c n) (instance? c <simple-warning>)))
            (signal \"bar\")))
   (print (signal \"unhandled warning\"))"
  "(\"foo ~a\" (1))
#t
#f
")

 ;; handler-case [Macro]: the manual's \"very trivial use\", with
 ;; some-function supplied; clauses are checked in order; a clause with
 ;; no forms answers #f; when the protected form completes, handler-case
 ;; answers its values; condition: names a variable bound to the
 ;; condition; test: as for handler-bind; the clause's forms run after
 ;; the handlers of the handler-case are no longer active.  From the
 ;; specification.
 (ch13-handler-case
  "(define some-function
     (method (what)
       (select what id?
         (('type) (check-type 'a <integer>))
         (('error) (error \"an error\"))
         (('warning) (signal \"a warning\"))
         (else: 'fine))))
   (define try
     (method (what)
       (handler-case (some-function what)
         ((<type-error>) \"there was a type-error\")
         ((<error>) \"there was an error\")
         ((<warning>) \"there was a warning\"))))
   (print (map try '(type error warning none)))
   (print (handler-case (error \"x\") ((<error>) 'first) ((<simple-error>) 'second)))
   (print (handler-case (error \"x\") ((<error>))))
   (bind ((a b (handler-case (values 1 2) ((<error>) 0)))) (print (list a b)))
   (print (handler-case (error \"msg ~a\" 5)
            ((<error> condition: c) (condition-format-arguments c))))
   (print (handler-case (error \"two\")
            ((<error> test: (method (c) (= (condition-format-string c) \"one\"))) 'one)
            ((<error> test: (method (c) (= (condition-format-string c) \"two\"))) 'two)))
   (print (handler-case
              (handler-case (error \"a\")
                ((<error>) (error \"b\")))
            ((<error> condition: c) (condition-format-string c))))
   (print (handler-case (signal \"w\")
            ((<warning> description: \"a description\") 'described)))"
  "(\"there was a type-error\" \"there was an error\" \"there was a warning\" fine)
first
#f
(1 2)
(5)
two
\"b\"
described
")

 ;; cerror [Sealed Function]: like error, but first establishes a handler
 ;; for <simple-restart> whose description is restart-description; if that
 ;; restart handler is invoked, cerror returns #f.  do-handlers shows the
 ;; restart handler and its description.  From the specification.
 (ch13-cerror
  "(print (handler-bind (<error> (method (c n) (signal (make <simple-restart>))))
            (list (cerror \"use zero\" \"bad value ~a\" 3) 'continued)))
   (print (handler-bind (<error>
                         (method (c n)
                           (bind ((found #f))
                             (do-handlers (method (type test function description)
                                            (if (and (not found) (id? type <simple-restart>))
                                                (set! found description) #f)))
                             (list found (condition-format-string c) (condition-format-arguments c)))))
            (bind-exit (exit)
              (handler-bind (<error> (method (c n) (exit (n))))
                (cerror \"use zero\" \"bad value ~a\" 3)))))
   (print (handler-case (cerror \"go on\" \"failed\")
            ((<simple-error> condition: c) (condition-format-string c))))
   (define-class <my-error> (<error>))
   (print (handler-bind (<my-error> (method (c n) (signal (make <simple-restart>))))
            (cerror \"continue\" (make <my-error>))))"
  "(#f continued)
(\"use zero\" \"bad value ~a\" (3))
\"failed\"
#f
")

 ;; break [Sealed Function]: invokes the debugger without signaling and
 ;; establishes a <simple-restart> so it can continue; \"break always
 ;; returns #f\".  The debugger here cannot intervene, so it continues.
 ;; From the specification.
 (ch13-break
  "(define-class <note> (<condition>))
   (print (handler-bind (<note> (method (c n) 'signaled))
            (list (break) (break \"stop ~a\" 1) (break (make <note>)))))"
  "(#f #f #f)
")

 ;; check-type [Sealed Function]: returns value, or signals a <type-error>
 ;; if (instance? value type) is false; it works with singletons too.
 ;; From the specification.
 (ch13-check-type
  "(print (list (check-type 3 <integer>) (check-type \"s\" <string>) (check-type 'x (singleton 'x))))
   (print (handler-case (check-type 'a <integer>)
            ((<type-error> condition: c)
             (list (type-error-value c) (id? (type-error-expected-type c) <integer>)))))
   (print (handler-case (check-type 4 (singleton 3)) ((<type-error>) 'type-error)))"
  "(3 \"s\" x)
(a #t)
type-error
")

 ;; abort [Sealed Function]: performs (error (make <abort>)).  The default
 ;; handler for <restart> signals an error.  From the specification.
 (ch13-abort
  "(print (handler-case (abort) ((<abort>) 'aborted)))
   (print (handler-case (abort) ((<restart>) 'restart)))
   (print (handler-case (abort) ((<error>) 'error)))
   (print (bind-exit (exit)
            (handler-bind (<abort> (method (c n) (exit (instance? c <abort>))))
              (abort))))"
  "aborted
restart
error
#t
")

 ;; abort never returns: when its handler returns, error bombs to the
 ;; debugger, ending the program.  From the specification.
 (ch13-abort-handler-returns
  "(print 'before)
   (handler-bind (<abort> (method (c n) 'returned))
     (abort)
     (print 'not-reached))"
  "before
"
  :error "abort")

 ;; Restarts (Overview): \"A handler restarts by signaling a restart ...
 ;; If the restart handler returns some values, signal returns those values
 ;; and the handler that called signal also returns them.  The call to
 ;; signal from the signaling unit that signaled the original condition
 ;; returns the same values\".  The default handler for <restart> signals
 ;; an error.  From the specification.
 (ch13-restarts
  "(define-class <need-value> (<condition>))
   (define-class <use-value> (<restart>) (value init-keyword: value:))
   (define signaling-unit
     (method ()
       (handler-bind (<use-value> (method (r n) (value r)))
         (list 'recovered-with (signal (make <need-value>))))))
   (print (handler-bind (<need-value> (method (c n) (signal (make <use-value> value: 17))))
            (signaling-unit)))
   (print (handler-case (signal (make <use-value> value: 1))
            ((<error>) 'error)))
   (print (handler-case (signal (make <simple-restart>))
            ((<error>) 'error)))"
  "(recovered-with 17)
error
error
")

 ;; A restart with no handler, unhandled: the error the default handler
 ;; signals ends the program.  From the specification.
 (ch13-unhandled-restart
  "(print 'before)
   (signal (make <simple-restart>))
   (print 'not-reached)"
  "before
"
  :error "restart")

 ;; default-handler [Generic Function]: the methods for <condition>
 ;; (returns #f), <warning> (prints the message and returns #f), <restart>
 ;; (signals an error) and <serious-condition> (calls the debugger, which
 ;; here ends the program).  From the specification.
 (ch13-default-handler
  "(define-class <note> (<condition>))
   (define-class <my-serious> (<serious-condition>))
   (print (default-handler (make <note>)))
   (print (default-handler (make <simple-warning> format-string: \"a warning\")))
   (print (handler-case (default-handler (make <simple-restart>)) ((<error>) 'error)))
   (print (handler-case (signal (make <my-serious>)) ((<serious-condition>) 'serious)))
   (print 'last)
   (default-handler (make <simple-error> format-string: \"fatal ~a\" format-arguments: '(7)))
   (print 'not-reached)"
  "#f
#f
error
serious
last
"
  :error "fatal 7")

 ;; An unhandled serious condition reaches the default handler, which
 ;; calls the debugger, here ending the program.  From the specification.
 (ch13-unhandled-error
  "(print 'before)
   (signal (make <simple-error> format-string: \"unhandled ~a\" format-arguments: '(error)))
   (print 'not-reached)"
  "before
"
  :error "unhandled error")

 ;; "Operators for Interactive Handling" and "Operators for
 ;; Introspection": restart-query (the default method for <restart> does
 ;; nothing), return-query, return-allowed? (the default method for
 ;; <condition> returns #f; a warning's recovery protocol is that any value
 ;; can be returned, so for a warning it is #t) and return-description,
 ;; with methods for a class of the program's own whose recovery protocol
 ;; allows returning.  From the specification.
 (ch13-introspection
  "(define-class <returnable> (<condition>))
   (define-method return-allowed? ((c <returnable>)) #t)
   (define-method return-description ((c <returnable>)) \"return a number\")
   (define-method return-query ((c <returnable>)) (values 1 2))
   (print (list (return-allowed? (make <simple-error>)) (return-allowed? (make <simple-warning>))
                (return-allowed? (make <returnable>))))
   (print (return-description (make <returnable>)))
   (bind ((a b (return-query (make <returnable>)))) (print (list a b)))
   (define r (make <simple-restart>))
   (restart-query r)
   (print (instance? r <simple-restart>))"
  "(#f #t #t)
\"return a number\"
(1 2)
#t
")

 ;; do-handlers [Sealed Function]: applies funarg to all dynamically
 ;; active handlers, the most recently established first, with type, test,
 ;; function and description; test defaults to a function that always
 ;; returns #t; description is #f if unsupplied, else a function or a
 ;; string.  From the specification.
 (ch13-do-handlers
  "(define-class <note> (<condition>))
   (define h1 (method (c n) 1))
   (define h2 (method (c n) 2))
   (define t2 (method (c) #f))
   (define d3 (method (stream) #f))
   (handler-bind (<error> h1)
     (handler-bind (<note> h2 test: t2 description: \"note handler\")
       (handler-bind (<warning> h1 description: d3)
         (bind ((seen '()))
           (do-handlers (method (type test function description)
                          (set! seen (cons (list type test function description) seen))))
           (bind ((all (reverse seen)))
             (print (size all))
             (bind ((w (first all)) (n (second all)) (e (third all)))
               (print (list (id? (first w) <warning>) (id? (third w) h1) (id? (element w 3) d3)
                            ((second w) (make <simple-warning>))))
               (print (list (id? (first n) <note>) (id? (second n) t2) (id? (third n) h2) (element n 3)))
               (print (list (id? (first e) <error>) (id? (third e) h1) (element e 3)
                            ((second e) (make <simple-error>))))))))))"
  "3
(#t #t #t #t)
(#t #t #t \"note handler\")
(#t #t #f #t)
")

 ;; The chapter's examples (\"Examples\"), with the operating system
 ;; replaced: guts-of-open finds only \"my-emergency-backup-file\".  The
 ;; slot lists are written as chapter 6's define-class takes them, the
 ;; first example's format to *error-output* is a print, and the print
 ;; methods with verbose: are left out (print here is the added one).
 ;; The values are what the examples' text describes: the first reports
 ;; the missing file; the second reads the backup file instead.
 (ch13-file-not-found
  "(define-class <file-not-found> (<error>)
     (file-name init-keyword: file-name:))
   (define-class <try-a-different-file> (<restart>)
     (file-name init-keyword: file-name:))
   (define-method guts-of-open (the-file)
     (if (= the-file \"my-emergency-backup-file\")
         (list 'opened the-file)
         (error (make <file-not-found> file-name: the-file))))
   (define-method open (the-file)
     (handler-case (guts-of-open the-file)
       ((<try-a-different-file>
         description: (method (stream)
                        (format stream \"Read a different file instead of ~A\"
                                the-file))
         condition: restart)
        (open (file-name restart)))))
   (handler-case (open \"file-that-doesnt-exist\")
     ((<file-not-found> condition: c)
      (print (list 'not-found (file-name c)))))
   (print (handler-bind (<file-not-found>
                         (method (condition next-handler)
                           (signal (make <try-a-different-file>
                                         file-name: \"my-emergency-backup-file\"))))
            (open \"file-that-doesnt-exist\")))"
  "(not-found \"file-that-doesnt-exist\")
(opened \"my-emergency-backup-file\")
")

 ;; The improved example: the restart handler applies only to a restart
 ;; whose condition: is the condition guts-of-open signaled (its test:
 ;; (compose (curry id? condition) restart-condition)); a restart made
 ;; for another condition finds no handler, and the default handler for
 ;; <restart> signals an error.
 (ch13-file-not-found-test
  "(define-class <file-not-found> (<error>)
     (file-name init-keyword: file-name:))
   (define-class <try-a-different-file> (<restart>)
     (restart-condition init-keyword: condition:)
     (file-name init-keyword: file-name:))
   (define-method guts-of-open (the-file)
     (if (= the-file \"my-emergency-backup-file\")
         (list 'opened the-file)
         (bind ((condition (make <file-not-found> file-name: the-file)))
           (handler-case (error condition)
             ((<try-a-different-file>
               test: (compose (curry id? condition) restart-condition)
               description: (method (stream)
                              (format stream
                                \"Read a different file instead of ~A\"
                                the-file))
               condition: restart)
              (open (file-name restart)))))))
   (define-method open (the-file)
     (guts-of-open the-file))
   (print (handler-bind (<file-not-found>
                         (method (condition next-handler)
                           (signal (make <try-a-different-file>
                                         condition: condition
                                         file-name: \"my-emergency-backup-file\"))))
            (open \"file-that-doesnt-exist\")))
   (print (handler-case
              (handler-bind (<file-not-found>
                             (method (condition next-handler)
                               (signal (make <try-a-different-file>
                                             condition: (make <file-not-found> file-name: \"other\")
                                             file-name: \"my-emergency-backup-file\"))))
                (open \"file-that-doesnt-exist\"))
            ((<file-not-found>) 'file-not-found)
            ((<error>) 'no-restart-handler)))"
  "(opened \"my-emergency-backup-file\")
no-restart-handler
")
)
