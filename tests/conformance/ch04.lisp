;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 4 of the 1992 manual, Variables.  Most
;;;; are the chapter's own examples with the results it shows; the rest
;;;; are marked as coming from its text.

(
 ;; 4 Module Variables, define: the chapter's example, without the
 ;; redefinition (the next test) and the unbound references (the ones
 ;; after).  The variable name may also be (setter symbol).
 (ch04-define
  "(define foo 10)
   (print foo)
   (print (+ foo 100))
   (define bar foo)
   (print bar)
   (define (setter baz) 'setter-value)
   (print (setter baz))"
  "10
110
10
setter-value
")

 ;; 4 Module Variables, define: the chapter's example redefines foo
 ;; (\"warning: redefining variable foo\"); bar keeps the value it was
 ;; given.
 (ch04-define-redefinition
  "(define foo 10)
   (define bar foo)
   (define foo 20)
   (print foo)
   (print bar)
   (print (+ foo bar))"
  "20
10
30
")

 ;; 4 Module Variables, define: \"? foo  error: unbound variable foo\" --
 ;; a reference to a variable before define has given it a value.
 (ch04-define-unbound
  "(print 'before)
   (print foo)
   (define foo 10)"
  "before
"
  :error "foo")

 ;; 4 Lexical Variables, bind: the chapter's examples.  (The first has a
 ;; misplaced parenthesis in the manual -- ((number1 20)) (number2 30)) --
 ;; corrected here to the binding list its result implies.)  With no
 ;; forms, bind returns #f (specification).
 (ch04-bind
  "(print (bind ((number1 20)
                 (number2 30))
            (+ number1 number2)))
   (print (bind ((x 20)
                 (y (+ x x)))
            (+ y y)))
   (define foo 10)
   (print (+ foo foo))
   (print (bind ((foo 35))
            (+ foo foo)))
   (print (bind ((foo 20))
            (bind ((foo 50))
              (+ foo foo))))
   (print foo)
   (print (bind ((x 1))))"
  "50
80
20
70
100
10
#f
")

 ;; 4 Checking Init Types: the chapter's example, and the text: the type
 ;; is any form, evaluated before the init, and must evaluate to a class
 ;; or a singleton; an init of another type signals an error.
 (ch04-bind-types
  "(print (handler-case (bind (((x <integer>) (sqrt 2)))
                          x)
            ((<error>) 'error)))
   (print (bind (((x <integer>) 7)) x))
   (print (bind (((x (if #t <string> <integer>)) \"s\")) x))
   (print (bind (((x (singleton 'a)) 'a)) x))
   (print (handler-case (bind (((x (singleton 'a)) 'b)) x)
            ((<error>) 'error)))
   (define trail '())
   (print (bind (((x (begin (set! trail (cons 'type trail)) <integer>))
                  (begin (set! trail (cons 'init trail)) 1)))
            x))
   (print (reverse trail))"
  "error
7
\"s\"
a
error
1
(type init)
")

 ;; 4 Checking Init Types: the chapter's example as it stands, at top
 ;; level, where the error ends the program.
 (ch04-bind-type-error
  "(print (bind (((x <integer>) (sqrt 2)))
            x))"
  ""
  :error "<integer>")

 ;; 4 Binding Multiple Values: the chapter's examples, and the text:
 ;; extra values are ignored, missing ones bind #f, #rest takes the
 ;; remaining values as a sequence.
 (ch04-bind-multiple-values
  "(print (bind ((foo bar baz (values 1 2 3)))
            (list foo bar baz)))
   (define-method edges ((center <number>) (radius <number>))
     (values (- center radius) (+ center radius)))
   (define-method opposite-edges ((center <number>)
                                  (radius <number>))
     (bind ((min max (edges center radius)))
       (values max min)))
   (bind ((a b (opposite-edges 100 2)))
     (print a)
     (print b))
   (print (bind ((x 10)
                 (y 20))
            (bind ((x y (values y x)))
              (list x y))))
   (print (bind ((#rest nums (edges 100 2)))
            nums))
   (print (bind ((a (values 1 2 3))) a))
   (print (bind ((a b c (values 1))) (list a b c)))
   (print (bind ((a #rest r (values 1 2 3))) (list a (size r) (first r) (second r))))
   (print (bind ((a #rest r (values 1))) (list a (empty? r))))
   (print (bind ((a b (values 1 2)) (c (+ a b))) c))"
  "(1 2 3)
102
98
(20 10)
(98 102)
1
(1 #f #f)
(1 2 2 3)
(1 #t)
3
")
)
