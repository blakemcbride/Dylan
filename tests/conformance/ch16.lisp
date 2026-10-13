;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 16 of the 1992 manual, "Characters"
;;;; (manual/Dylan-1992/book.annotated/ch16.html), in the form of
;;;; tests/codegen/runner.lisp.  The chapter shows no examples; every
;;;; expected value comes from its specification.

(
 ;; 16 <character>: a subclass of <object>.  From the specification.
 (ch16-class
  "(print (list (subclass? <character> <object>) (instance? #\\a <character>)
                (instance? \"a\" <character>) (instance? 'a <character>)))"
  "(#t #t #f #f)
")

 ;; 16 as-uppercase: the upper-case equivalent; a character already upper
 ;; case, or not in two cases, is returned unchanged.  From the
 ;; specification.
 (ch16-as-uppercase
  "(print (list (as-uppercase #\\a) (as-uppercase #\\z) (as-uppercase #\\A) (as-uppercase #\\Q)
                (as-uppercase #\\1) (as-uppercase #\\%)))
   (print (list (id? (as-uppercase #\\space) #\\space) (id? (as-uppercase #\\newline) #\\newline)))"
  "(#\\A #\\Z #\\A #\\Q #\\1 #\\%)
(#t #t)
")

 ;; 16 as-lowercase: the lower-case equivalent; a character already lower
 ;; case, or not in two cases, is returned unchanged.  From the
 ;; specification.
 (ch16-as-lowercase
  "(print (list (as-lowercase #\\A) (as-lowercase #\\Z) (as-lowercase #\\a) (as-lowercase #\\q)
                (as-lowercase #\\7) (as-lowercase #\\-)))
   (print (list (id? (as-lowercase #\\space) #\\space) (id? (as-lowercase #\\tab) #\\tab)))"
  "(#\\a #\\z #\\a #\\q #\\7 #\\-)
(#t #t)
")

 ;; 16 as <integer> and as <character>: a numeric equivalent of a
 ;; character, and the character equivalent to an integer.  The integers
 ;; are implementation dependent, so only the round trip and that
 ;; different characters have different codes are tested.  From the
 ;; specification.
 (ch16-as-integer-character
  "(print (list (instance? (as <integer> #\\a) <integer>)
                (as <character> (as <integer> #\\a)) (as <character> (as <integer> #\\Z))
                (as <character> (as <integer> #\\0))
                (= (as <integer> #\\a) (as <integer> #\\b))
                (= (as <integer> #\\a) (as <integer> #\\a))))
   (bind ((n (as <integer> #\\x)))
     (print (list (instance? (as <character> n) <character>) (id? (as <character> n) #\\x))))"
  "(#t #\\a #\\Z #\\0 #f #t)
(#t #t)
")
)
