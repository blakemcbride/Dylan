; Benchmarks for the Dylan runtime.  Each BENCH-<NAME> takes a size N and
; answers a value that reference.lisp's function of the same name, written
; in plain Common Lisp, must also answer.  See run.lisp.

; A linear congruential generator, so both sides see the same numbers.
(define-method next-random (seed)
  (modulo (+ (* seed 1103515245) 12345) 2147483648))

(define-method random-list (n)
  (bind ((seed 42) (result '()))
    (for ((i 0 (+ i 1))) ((= i n) result)
      (set! seed (next-random seed))
      (set! result (cons (modulo seed 100000) result)))))

; Hash tables: insert, look up, update.
(define-method bench-table (n)
  (bind ((t (make <table>)) (sum 0))
    (for ((i 0 (+ i 1))) ((= i n))
      (set! (element t i) (* i 2)))
    (for ((i 0 (+ i 1))) ((= i n))
      (set! (element t i) (+ (element t i) 1)))
    (for ((i 0 (+ i 1))) ((= i n) sum)
      (set! sum (+ sum (element t i default: 0))))))

; Sorting with the default test and with a method.
(define-method bench-sort (n)
  (bind ((l (random-list n))
         (up (sort l))
         (down (sort (as <vector> l) test: (method (a b) (> a b)))))
    (+ (first up) (last up) (element down 0) (element down (- n 1)))))

; Building strings.
(define-method bench-string (n)
  (bind ((pieces '()) (total 0))
    (for ((i 0 (+ i 1))) ((= i n))
      (set! pieces (cons (concatenate "ab" "cde" (if (even? i) "x" "yz"))
                         pieces)))
    (for-each ((s pieces)) ()
      (set! total (+ total (size s))))
    total))

; Deques used as queues.
(define-method bench-deque (n)
  (bind ((d (make <deque>)) (sum 0))
    (for ((i 0 (+ i 1))) ((= i n))
      (push-last d i))
    (for ((i 0 (+ i 1))) ((= i n) sum)
      (set! sum (+ sum (pop d))))))

; Growing a stretchy vector.
(define-method bench-stretchy (n)
  (bind ((v (make <stretchy-vector>)) (sum 0))
    (for ((i 0 (+ i 1))) ((= i n))
      (add! v i))
    (for ((i 0 (+ i 1))) ((= i n) sum)
      (set! sum (+ sum (element v i))))))

; Iterating over a range.
(define-method bench-range (n)
  (bind ((sum 0))
    (for-each ((i (range from: 0 size: n))) ()
      (set! sum (+ sum i)))
    sum))

; Non-local exits.
(define-method find-first-over (l limit)
  (bind-exit (return)
    (for-each ((x l)) ()
      (if (> x limit) (return x) #f))
    #f))

(define-method bench-exit (n)
  (bind ((l '(1 2 3 4 5 6 7 8 9 10)) (sum 0))
    (for ((i 0 (+ i 1))) ((= i n) sum)
      (set! sum (+ sum (find-first-over l 5))))))

; Signalling and handling errors.
(define-method bench-condition (n)
  (bind ((caught 0))
    (for ((i 0 (+ i 1))) ((= i n) caught)
      (handler-case (if (even? i) (error "odd one out") i)
        ((<error>) (set! caught (+ caught 1)))))))

; A class hierarchy with NEXT-METHOD chains.
(define-class <animal> (<object>))
(define-class <mammal> (<animal>))
(define-class <dog> (<mammal>))
(define-class <puppy> (<dog>))
(define-method weight ((a <animal>)) 1)
(define-method weight ((m <mammal>)) (+ 2 (next-method)))
(define-method weight ((d <dog>)) (+ 3 (next-method)))
(define-method weight ((p <puppy>)) (+ 4 (next-method)))

(define-method bench-next-method (n)
  (bind ((animals (list (make <animal>) (make <mammal>)
                        (make <dog>) (make <puppy>)))
         (sum 0))
    (for ((i 0 (+ i 1))) ((= i n) sum)
      (for-each ((a animals)) ()
        (set! sum (+ sum (weight a)))))))

; Recursion with three arguments.
(define-method tak (x y z)
  (if (< y x)
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))
      z))

(define-method bench-tak (n)
  (bind ((sum 0))
    (for ((i 0 (+ i 1))) ((= i n) sum)
      (set! sum (+ sum (tak 18 12 6))))))

; Floating-point arithmetic: points of a grid inside the Mandelbrot set.
(define-method mandelbrot-inside? (cr ci)
  (bind ((zr 0.0) (zi 0.0))
    (for ((i 0 (+ i 1)))
         ((or (= i 50) (> (+ (* zr zr) (* zi zi)) 4.0))
          (= i 50))
      (bind ((new-zr (+ (- (* zr zr) (* zi zi)) cr)))
        (set! zi (+ (* 2.0 zr zi) ci))
        (set! zr new-zr)))))

(define-method bench-float (n)
  (bind ((count 0))
    (for ((y 0 (+ y 1))) ((= y n) count)
      (for ((x 0 (+ x 1))) ((= x n))
        (if (mandelbrot-inside? (- (/ (* 3.0 x) n) 2.0)
                                (- (/ (* 2.0 y) n) 1.0))
            (set! count (+ count 1))
            #f)))))
