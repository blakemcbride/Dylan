;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 18 of the 1992 manual, "Miscellaneous
;;;; Operations" (manual/Dylan-1992/book.annotated/ch18.html), in the form
;;;; of tests/codegen/runner.lisp.  The chapter shows no examples; the
;;;; expected values come from its specification.

(
 ;; 18 identity: "identity simply returns object."  From the
 ;; specification.
 (ch18-identity
  "(define l (list 1 2))
   (print (list (identity 3) (identity 'a) (identity \"s\") (identity #f) (identity '())
                (id? (identity l) l)))
   (print (map identity '(1 2 3)))
   (print (instance? identity <function>))"
  "(3 a \"s\" #f () #t)
(1 2 3)
#t
")
)
