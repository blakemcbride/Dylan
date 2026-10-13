;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Proper tail calls, 10^8 of them, in a 256 KB stack: through functions
;;;; the compiler cannot know, generic functions, closures and apply.  None
;;;; allocates per call, so they run in the gc-stress mode too.

(
 ;; A function passed itself, and two found in a vector
 (stress-unknown-functions
  "(define loop2 (method (n f) (if (= n 0) 'ok (f (- n 1) f))))
   (print (loop2 100000000 loop2))
   (define fs (vector #f #f))
   (define ping (method (n) (if (= n 0) 'ping ((element fs 1) (- n 1)))))
   (define pong (method (n) (if (= n 0) 'pong ((element fs 0) (- n 1)))))
   (set! (element fs 0) ping)
   (set! (element fs 1) pong)
   (print ((element fs 0) 100000001))"
  "ok
pong
")

 ;; Mutual recursion through two generic functions, each with methods
 ;; to choose among
 (stress-generic-functions
  "(define-method ping ((n <integer>)) (if (= n 0) 'ping-done (pong (- n 1))))
   (define-method ping ((s <symbol>)) s)
   (define-method pong ((n <integer>)) (if (= n 0) 'pong-done (ping (- n 1))))
   (define-method pong ((c <character>)) c)
   (print (ping 100000000))
   (print (list (ping 'x) (pong #\\y)))"
  "ping-done
(x #\\y)
")

 ;; Closures, each holding its own tag, calling one another
 (stress-closures
  "(define make-hop (method (tag) (method (n next self) (if (= n 0) tag (next (- n 1) self next)))))
   (define a (make-hop 'a))
   (define b (make-hop 'b))
   (print (a 100000000 b a))
   (print (a 99999999 b a))"
  "a
b
")

 ;; next-method in tail position, down a chain, many times
 (stress-next-method
  "(define-class <base> (<object>))
   (define-class <mid> (<base>))
   (define-class <leaf> (<mid>))
   (define-method walk ((x <base>) n) (if (= n 0) 'base (walk x (- n 1))))
   (define-method walk ((x <mid>) n) (next-method x n))
   (define-method walk ((x <leaf>) n) (next-method x n))
   (print (walk (make <leaf>) 30000000))"
  "base
")

 ;; apply in tail position (its list is the one allocation)
 (stress-apply
  "(define via-apply (method (n acc) (if (= n 0) acc (apply via-apply (list (- n 1) (+ acc 1))))))
   (print (via-apply 1000000 0))"
  "1000000
"))
