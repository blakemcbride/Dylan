;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 3 of the 1992 manual, Syntax and
;;;; Semantics.  Most are the chapter's own examples with the results it
;;;; shows; the rest are marked as coming from its text.

(
 ;; 3 Lexical Notation: white space, lists, vectors, symbols (not case
 ;; sensitive, though they keep their case), keywords (a class distinct
 ;; from symbols), numbers, strings and their backslash, characters and
 ;; their names, comments, quote, #t and #f.  (Specification; the
 ;; numbers are the chapter's own list.)
 (ch03-lexical-notation
  "(print   (list 1
                 2    3))
   (print '(a (b c) ()))
   (print '#(1 2 3))
   (print (instance? '#(1 2) <vector>))
   (print (list 'Foo (id? 'foo 'FOO) (id? 'Foo 'fOO)))
   (print (list (instance? foo: <keyword>) (instance? foo: <symbol>) (instance? 'foo <symbol>)))
   (print (list 123 1.5 -4.0 +57 #x1f4e))
   (print (list \"abc\" (size \"a b\") (size \"a\\\"b\") (size \"a\\\\b\") (element \"a\\\"b\" 1)))
   (print \"q\\\"uote\")
   (print (list #\\a (element \"a b\" 1)))
   (print (= #\\space (element \"a b\" 1)))
   (print (every? (method (c) (instance? c <character>))
                  (list #\\newline #\\space #\\rubout #\\page #\\tab #\\backspace #\\return
                        #\\linefeed)))
   (print 1) ; a comment (print 2)
   #| an extended comment (print 3)
      #| nested |# (print 4)
      still a comment |#
   (print 5)
   (print (list 'x (quote x)))
   (print (list #t #f))"
  "(1 2 3)
(a (b c) ())
#(1 2 3)
#t
(Foo #t #t)
(#t #f #t)
(123 1.5 -4.0 57 8014)
(\"abc\" 3 3 3 #\\\")
\"q\\\"uote\"
(#\\a #\\space)
#t
#t
1
5
(x x)
(#t #f)
")

 ;; 3 Conventions: a getter and its setter.  "These two expressions are
 ;; roughly equivalent; they both set the color of my-window to green."
 (ch03-setter-convention
  "(define-class <window> (<object>) (window-color init-value: 'red))
   (define my-window (make <window>))
   (define green 'green)
   (print (window-color my-window))
   ((setter window-color) my-window green)
   (print (window-color my-window))
   (set! (window-color my-window) 'blue)
   (print (window-color my-window))
   (set! (window-color my-window) green)
   (print (window-color my-window))"
  "red
green
blue
green
")

 ;; 3 Literal Constants: the chapter's examples.
 (ch03-literal-constants
  "(print \"abc\")
   (print 123)
   (print foo:)
   (print #\\a)
   (print #t)
   (print #f)
   (print (quote foo))
   (print 'foo)
   (print '(1 2 3))"
  "\"abc\"
123
foo:
#\\a
#t
#f
foo
foo
(1 2 3)
")

 ;; 3 Variable References: the chapter's examples.  <window> is a class,
 ;; concatenate and (setter element) generic functions (tested as such,
 ;; since their printed forms are not readable).
 (ch03-variable-references
  "(define-class <window> (<object>))
   (print (instance? <window> <class>))
   (print (instance? concatenate <generic-function>))
   (define my-variable 25)
   (print my-variable)
   (print (bind ((x 50))
            (+ x x)))
   (print (instance? (setter element) <generic-function>))
   (define (setter my-variable) 20)
   (print (setter my-variable))
   (print my-variable)
   (bind ((position 1) (my-var 2) ((setter position) 3) ((setter another-variable) 4))
     (print (list position my-var (setter position) (setter another-variable))))"
  "#t
#t
25
100
#t
20
25
(1 2 3 4)
")

 ;; 3 Function Calls: the chapter's examples.
 (ch03-function-calls
  "(define my-variable 25)
   (print (+ 3 4))
   (print (* my-variable 3))
   (print (* (+ 3 4) 5))
   (print ((if #t + *) 4 5))"
  "7
75
35
9
")

 ;; 3 Function Calls: \"The arguments to a function are evaluated in
 ;; left-to-right order.\"  (Specification.)
 (ch03-argument-order
  "(define trail '())
   (define-method note (x) (set! trail (cons x trail)) x)
   (print (list (note 1) (note 2) (note 3)))
   (print (reverse trail))"
  "(1 2 3)
(1 2 3)
")

 ;; 3 Syntax Forms: the chapter's examples; their results are those its
 ;; comments give (set! returns the new value, chapter 7).
 (ch03-syntax-forms
  "(define my-variable 25)
   (print my-variable)
   (print (set! my-variable 12))
   (print my-variable)
   (print (bind ((x 10) (y 20))
            (+ x y)))
   (define m (method ((a <number>) (b <number>))
               (list (- a b) (+ a b))))
   (print (m 5 3))
   (print (instance? m <method>))"
  "25
12
12
30
(2 8)
#t
")

 ;; 3 Multiple Values: the chapter's examples, the values received with
 ;; bind.
 (ch03-multiple-values
  "(bind ((a b c (values 1 2 3)))
     (print (list a b c)))
   (define-method edges ((center <number>)(radius <number>))
     (values (- center radius) (+ center radius)))
   (bind ((lo hi (edges 100 2)))
     (print lo)
     (print hi))
   (bind ((#rest all (values)))
     (print (empty? all)))"
  "(1 2 3)
98
102
#t
")
)
