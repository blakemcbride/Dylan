;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 17 of the 1992 manual, "Functional
;;;; Operations" (manual/Dylan-1992/book.annotated/ch17.html), in the form
;;;; of tests/codegen/runner.lisp: the chapter's examples with the results
;;;; it shows, and the rest of each function's specification.

(
 ;; 17 compose, the example: distance, (compose sqrt sum square-all),
 ;; gives 7.0710678118654755 for (3 4 5).  (The manual's square-all lacks
 ;; a closing parenthesis.)
 (ch17-compose-example
  "(define-method sum ((numbers <sequence>))
     (reduce1 + numbers))
   (define-method square ((x <number>)) (* x x))
   (define-method square-all ((coords <sequence>))
     (map square coords))
   (define distance (compose sqrt sum square-all))
   (print (distance '(3 4 5)))"
  "7.0710678118654755
")

 ;; 17 compose: with one argument it returns that argument; with two, a
 ;; function applying the second function to its arguments and the first
 ;; to the result (cadr is (compose car cdr)); with more, the pairs are
 ;; composed.  From the specification.
 (ch17-compose
  "(print (id? (compose car) car))
   (define my-cadr (compose car cdr))
   (print (my-cadr '(1 2 3)))
   (print ((compose car cdr cdr) '(1 2 3)))
   (print ((compose - +) 1 2 3))
   (print ((compose list - *) 2 3 4))
   (print ((compose (method (x) (* x 10)) (method (x) (+ x 1)) (method (x y) (- x y))) 5 2))"
  "#t
2
3
-6
(-24)
40
")

 ;; 17 complement, the example: female? and (complement female?) mapped
 ;; over michelle, arnold and roseanne give (#t #f #t) and (#f #t #f).
 ;; (The manual does not show its people; here they are a class.)
 (ch17-complement-example
  "(define-class <person> (<object>) (sex init-keyword: sex:))
   (define michelle (make <person> sex: 'female))
   (define arnold (make <person> sex: 'male))
   (define roseanne (make <person> sex: 'female))
   (define-method female? ((p <person>)) (id? (sex p) 'female))
   (print (map female? (list michelle arnold roseanne)))
   (print (map (complement female?) (list michelle arnold roseanne)))"
  "(#t #f #t)
(#f #t #f)
")

 ;; 17 complement: applies the predicate to its arguments, answering #t
 ;; if it returns #f, else #f; odd? could be (complement even?).  From the
 ;; specification.
 (ch17-complement
  "(define my-odd? (complement even?))
   (print (map my-odd? '(1 2 3 4)))
   (print (list ((complement (method (x) x)) 5) ((complement (method (x) x)) #f)
                ((complement (method (x) x)) '())))
   (print (list ((complement <) 1 2) ((complement <) 2 1) ((complement (method () #f)))))"
  "(#t #f #t #f)
(#f #t #f)
(#f #t #t)
")

 ;; 17 disjoin: applies the predicates in order to any number of
 ;; arguments; the first true result is returned and the rest are not
 ;; applied; otherwise all are applied and #f returned.  From the
 ;; specification.
 (ch17-disjoin
  "(define calls '())
   (define-method note (name result)
     (method (#rest args) (set! calls (cons name calls)) result))
   (print ((disjoin (method (x) #f) (method (x) (list x)) (method (x) 'no)) 3))
   (print (list ((disjoin odd? even?) 2) ((disjoin odd? zero?) 2) ((disjoin <) 1 2)
                ((disjoin < >) 1 2) ((disjoin < >) 2 1) ((disjoin < >) 2 2)))
   (print ((disjoin (note 'a #f) (note 'b 'yes) (note 'c 'no)) 1 2))
   (print (reverse calls))
   (set! calls '())
   (print ((disjoin (note 'a #f) (note 'b #f) (note 'c #f))))
   (print (reverse calls))"
  "(3)
(#t #f #t #t #t #f)
yes
(a b)
#f
(a b c)
")

 ;; 17 conjoin: applies the predicates in order to any number of
 ;; arguments; if one returns #f the rest are not applied and #f is
 ;; returned; otherwise all are applied and the last result returned.
 ;; From the specification.
 (ch17-conjoin
  "(define calls '())
   (define-method note (name result)
     (method (#rest args) (set! calls (cons name calls)) result))
   (print ((conjoin odd? (method (x) (list x))) 3))
   (print (list ((conjoin odd? positive?) 3) ((conjoin odd? positive?) -3)
                ((conjoin odd? positive?) 4) ((conjoin < (method (x y) (= y 2))) 1 2)
                ((conjoin < (method (x y) (= y 2))) 1 3)))
   (print ((conjoin (note 'a 1) (note 'b #f) (note 'c 3)) 'x))
   (print (reverse calls))
   (set! calls '())
   (print ((conjoin (note 'a 1) (note 'b 2) (note 'c 'last))))
   (print (reverse calls))"
  "(3)
(#t #f #f #t #f)
#f
(a b)
last
(a b c)
")

 ;; 17 curry, the example (map (curry + 1) '(3 4 5)) => (4 5 6), and the
 ;; text's: (curry id? 'x) tests for identity with x, (curry > 6) is true
 ;; for values less than 6, (curry concatenate \"set-\") prepends
 ;; \"set-\".  curry applies the function to the curried arguments
 ;; followed by its own.
 (ch17-curry
  "(print (map (curry + 1) '(3 4 5)))
   (print (list ((curry id? 'x) 'x) ((curry id? 'x) 'y)))
   (print (map (curry > 6) '(5 6 7)))
   (print ((curry concatenate \"set-\") \"foo\"))
   (print ((curry concatenate \"set-\") \"a\" \"b\"))
   (print ((curry list 1 2) 3 4))
   (print ((curry list)))
   (print ((curry list 1 2)))"
  "(4 5 6)
(#t #f)
(#t #f #f)
\"set-foo\"
\"set-ab\"
(1 2 3 4)
()
(1 2)
")

 ;; 17 rcurry, the example: (rcurry concatenate \", ayup\") applied to
 ;; \"I'm from New Hampsha\" gives \"I'm from New Hampsha, ayup\"; and the
 ;; text's (rcurry > 6), true for values greater than 6.  rcurry's
 ;; arguments come after the function's own.
 (ch17-rcurry
  "(define yuppify (rcurry concatenate \", ayup\"))
   (print (yuppify \"I'm from New Hampsha\"))
   (print (map (rcurry > 6) '(5 6 7)))
   (print ((rcurry list 3 4) 1 2))
   (print ((rcurry - 1) 10))"
  "\"I'm from New Hampsha, ayup\"
(#f #f #t)
(1 2 3 4)
9
")

 ;; 17 always, the examples ((always 1) 'x 'y 'z) => 1 and
 ;; ((always #t) #f #f) => #t; the function takes any number of
 ;; arguments.
 (ch17-always
  "(print ((always 1) 'x 'y 'z))
   (print ((always #t) #f #f))
   (print ((always 'a)))
   (define l (list 1))
   (print (id? ((always l) 1 2 3 4 5 6 7 8 9 10) l))
   (print (map (always 0) '(1 2 3)))"
  "1
#t
a
#t
(0 0 0)
")
)
