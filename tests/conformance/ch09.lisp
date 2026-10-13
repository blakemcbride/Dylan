;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Manual conformance tests: chapter 9, "Equality and Magnitude
;;;; Comparisons", of the 1992 Dylan manual
;;;; (manual/Dylan-1992/book.annotated/ch9.html).  The chapter has no
;;;; examples, so every expected result is from its specification.

(
 ;; 9, Equality Comparisons, id? (from the specification and footnote 24):
 ;; identical means computationally equivalent; numbers and characters
 ;; with the same value are identical; any number of objects.
 (ch09-id
  "(define s \"abc\")
   (define l (list 1 2))
   (print (list (id? 'a 'a) (id? 'a 'b) (id? s s) (id? l l) (id? l (list 1 2))))
   (print (list (id? 1 1) (id? 1 1.0) (id? 2.5 2.5) (id? 1/2 1/2) (id? #\\a #\\a) (id? #\\a #\\A)))
   (print (list (id? #t #t) (id? #f #f) (id? '() '()) (id? + +)))
   (print (list (id? 3 3 3) (id? 3 3 4) (id? 'x 'x 'x 'x)))"
  "(#t #f #t #t #f)
(#t #f #t #t #t #f)
(#t #t #t #t)
(#t #f #t)
")

 ;; 9, =, /= and binary= (from the specification): built-in numbers by
 ;; mathematical value; collections by keys and corresponding = values
 ;; (sequences element by element, so of the same size; a sequence and a
 ;; table with the same keys); dotted lists by elements and final cdrs;
 ;; other objects with id?; = on every pair of any number of objects; /=
 ;; the negation of binary=.
 (ch09-equal
  "(print (list (= 1 1.0) (= 1/2 0.5) (= 2 2s0) (= 1 2) (= 3 3 3) (= 3 3 4) (= 1 1.0 1/1)))
   (print (list (= \"abc\" \"abc\") (= \"abc\" \"abd\") (= \"ab\" \"abc\") (= #\\a #\\a)))
   (print (list (= '(1 2 3) (list 1 2 3)) (= '(1 2) '(1 2 3)) (= '(1 (2 3)) '(1.0 (2 3.0)))))
   (print (list (= #(1 2) '(1 2)) (= (vector 1 \"x\") (list 1 \"x\")) (= #(1 2) #(2 1))))
   (print (list (= '(1 . 2) (cons 1 2)) (= '(1 2 . 3) '(1 2 . 4)) (= '(1 . 2) '(1 2))))
   (define t (make <table>))
   (set! (element t 0) 'a)
   (set! (element t 1) 'b)
   (print (list (= t '(a b)) (= '(a b) t) (= t '(a c)) (= t '(a b c))))
   (print (list (= 'a 'a) (= 'a 'b) (= + +) (= + -)))
   (print (list (/= 1 2) (/= 1 1.0) (/= \"ab\" \"ab\") (/= '(1) '(2))))
   (print (list (binary= 1 1.0) (binary= '(1 2) #(1 2)) (binary= 'a 'b)))"
  "(#t #t #t #f #t #f #t)
(#t #f #f #t)
(#t #f #t)
(#t #t #f)
(#t #f #f)
(#t #t #f #f)
(#t #f #t #f)
(#t #f #f #t)
(#t #t #f)
")

 ;; 9, binary= and =hash (from the specification): methods on binary=
 ;; extend =, /= and the comparison of collections; = calls binary= on
 ;; every pair; =hash answers an integer, the same one for = objects, and
 ;; programs extending binary= define methods on =hash to match.
 (ch09-extend-binary-equal
  "(define-class <money> (<object>) (cents init-keyword: cents:))
   (define-method binary= ((a <money>) (b <money>))
     (= (cents a) (cents b)))
   (define-method =hash ((m <money>))
     (=hash (cents m)))
   (define a (make <money> cents: 100))
   (define b (make <money> cents: 100))
   (define c (make <money> cents: 250))
   (print (list (id? a b) (= a b) (= a c) (= a b c) (= a a b)))
   (print (list (/= a b) (/= a c)))
   (print (list (= (list a c) (list b c)) (= (vector a) (vector c))))
   (print (= (=hash a) (=hash b)))
   (print (list (instance? (=hash a) <integer>) (instance? (=hash 'sym) <integer>)
                (instance? (=hash \"str\") <integer>) (instance? (=hash 2.5) <integer>)))
   (print (list (= (=hash 1) (=hash 1.0)) (= (=hash \"ab\") (=hash (concatenate \"a\" \"b\")))
                (= (=hash '(1 2)) (=hash (list 1 2)))))"
  "(#f #t #f #f #t)
(#f #t)
(#t #f)
#t
(#t #t #t #t)
(#t #t #t)
")

 ;; 9, Magnitude Comparisons, <, >, <=, >= and binary< (from the
 ;; specification): any number of objects compared two at a time; real
 ;; numbers by mathematical value; characters by their ordinal values,
 ;; case significant; strings lexicographically, a strict prefix being the
 ;; smaller.
 (ch09-magnitude
  "(print (list (< 1 2) (< 2 1) (< 1 1) (< 1 2 3) (< 1 3 2) (< 1 1.5 2) (< 1/2 0.6) (< 1/3 1/2)))
   (print (list (> 2 1) (> 1 2) (> 3 2 1) (> 3 1 2) (> 2 2) (> 2.5 2 3/2)))
   (print (list (<= 1 1) (<= 1 2 2 3) (<= 1 3 2) (<= 2 1)))
   (print (list (>= 1 1) (>= 3 2 2 1) (>= 3 1 2) (>= 1 2)))
   (print (list (< #\\a #\\b) (< #\\b #\\a) (< #\\A #\\a) (> #\\z #\\a) (<= #\\a #\\a)))
   (print (list (< \"abc\" \"abd\") (< \"abd\" \"abc\") (< \"ab\" \"abc\") (< \"abc\" \"ab\")
                (< \"b\" \"abc\") (< \"ABC\" \"abc\") (< \"abc\" \"abc\")))
   (print (list (> \"b\" \"a\") (<= \"ab\" \"ab\") (>= \"abc\" \"ab\") (< \"a\" \"b\" \"c\")))
   (print (list (binary< 1 2) (binary< 2 1) (binary< #\\a #\\b) (binary< \"a\" \"ab\")))"
  "(#t #f #f #t #f #t #t #t)
(#t #f #t #f #f #t)
(#t #t #f #f)
(#t #t #f #f)
(#t #f #t #t #t)
(#t #f #t #f #f #t #f)
(#t #t #t #t)
(#t #f #t #t)
")

 ;; 9, binary< (from the specification): methods on binary< extend <, >,
 ;; <= and >=, which are defined in terms of it.
 (ch09-extend-binary-less
  "(define-class <size> (<object>) (n init-keyword: n:))
   (define-method binary< ((a <size>) (b <size>))
     (< (n a) (n b)))
   (define s1 (make <size> n: 1))
   (define s2 (make <size> n: 2))
   (define s3 (make <size> n: 3))
   (print (list (< s1 s2) (< s2 s1) (< s1 s2 s3) (< s1 s3 s2)))
   (print (list (> s3 s2 s1) (> s1 s2) (<= s1 s1 s2) (<= s2 s1) (>= s3 s3 s1) (>= s1 s2)))"
  "(#t #f #t #f)
(#t #f #t #f #t #f)
")
)
