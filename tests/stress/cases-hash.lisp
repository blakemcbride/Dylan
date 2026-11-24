;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Identity hashes of many objects, most of which die: the table of them
;;;; keeps its empty places through collections, and its size follows the
;;;; keys hashed, not the largest burst (Bugs07 C18).

(
 ;; A burst of hashes of objects that die at once, then a few: a search
 ;; for a new key ends, and a key kept hashes alike throughout
 (stress-identity-hash-burst
  "(define-class <k> (<object>) (v init-value: 0))
   (define sum 0)
   (define *keep* #f)
   (dotimes (i 20000000)
     (set! sum (logxor sum (begin (set! *keep* (make <k>)) (=hash *keep*)))))
   (define kept *keep*)
   (define h (=hash kept))
   (dotimes (i 20000000)
     (set! *keep* (make <k>))
     (when (zero? (modulo i 100000)) (set! sum (logxor sum (=hash *keep*)))))
   (print (list (instance? sum <integer>) (= h (=hash kept))))"
  "(#t #t)
")
)
