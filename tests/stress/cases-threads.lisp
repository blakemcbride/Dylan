;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Many threads at once: allocating while the collector runs, contending
;;;; for locks and condition variables, interning symbols, signalling
;;;; conditions and binding thread variables.  What each prints does not
;;;; depend on the order the threads run in.

(
 (stress-thread-allocation
  "(define work
     (method ()
       (bind ((total 0) (t (make <table>)) (tsum 0))
         (dotimes (r 3)
           (bind ((l (for ((i 0 (+ i 1)) (l '() (cons (vector i (* i 2)) l))) ((= i 20000) l))))
             (for-each ((v l)) () (set! total (+ total (element v 1))))))
         (dotimes (i 5000) (set! (element t i) i))
         (dotimes (i 5000) (set! tsum (+ tsum (element t i))))
         (list total tsum))))
   (define threads (for ((i 0 (+ i 1)) (l '() (cons (make-thread work) l))) ((= i 8) l)))
   (for-each ((th threads)) () (print (join-thread th)))"
  "(1199940000 12497500)
(1199940000 12497500)
(1199940000 12497500)
(1199940000 12497500)
(1199940000 12497500)
(1199940000 12497500)
(1199940000 12497500)
(1199940000 12497500)
")

 ;; Four producers and four consumers sharing a buffer of 16
 (stress-bounded-buffer
  "(define lock (make <lock>))
   (define not-full (make <condition-variable>))
   (define not-empty (make <condition-variable>))
   (define buffer (make <deque>))
   (define put
     (method (x)
       (with-lock (lock)
         (while (= (size buffer) 16) (wait-condition-variable not-full lock))
         (push-last buffer x)
         (notify-all-condition-variable not-empty))))
   (define take
     (method ()
       (with-lock (lock)
         (while (empty? buffer) (wait-condition-variable not-empty lock))
         (bind ((x (pop buffer)))
           (notify-all-condition-variable not-full)
           x))))
   (define producers
     (for ((p 0 (+ p 1)) (l '() (cons (make-thread (method () (dotimes (i 5000) (put (+ i 1))))) l)))
          ((= p 4) l)))
   (define consumers
     (for ((c 0 (+ c 1))
           (l '() (cons (make-thread
                          (method ()
                            (bind ((sum 0)) (dotimes (i 5000) (set! sum (+ sum (take)))) sum)))
                        l)))
          ((= c 4) l)))
   (for-each ((p producers)) () (join-thread p))
   (print (reduce + 0 (map join-thread consumers)))"
  "50010000
")

 ;; Symbols interned by many threads at once are one symbol per name.
 (stress-symbols
  "(define intern-all
     (method () (for ((i 0 (+ i 1)) (l '() (cons (as <symbol> (format #f \"sym-~a\" i)) l)))
                     ((= i 2000) l))))
   (define results (map join-thread (for ((i 0 (+ i 1)) (l '() (cons (make-thread intern-all) l)))
                                         ((= i 8) l))))
   (print (every? (method (l) (every? id? l (first results))) results))
   (print (id? (first (first results)) 'sym-1999))"
  "#t
#t
")

 ;; Conditions signalled and handled in many threads at once
 (stress-conditions
  "(define-class <odd-error> (<error>) (value init-keyword: value:))
   (define count-handled
     (method (k)
       (bind ((handled 0))
         (dotimes (i 20000)
           (handler-case (if (odd? i) (error (make <odd-error> value: i)) i)
             ((<odd-error> condition: c) (set! handled (+ handled 1)))))
         (+ handled k))))
   (define threads (for ((k 0 (+ k 1)) (l '() (cons (make-thread (method () (count-handled k))) l)))
                        ((= k 8) l)))
   (print (reduce + 0 (map join-thread threads)))"
  "80028
")

 ;; Each thread's dynamic bindings are its own.
 (stress-thread-variables
  "(define-thread-variable *level* 0)
   (define descend
     (method (k depth)
       (if (= depth 0)
           *level*
           (dynamic-bind ((*level* (+ *level* k)))
             (thread-yield)
             (bind ((below (descend k (- depth 1))))
               (if (= *level* (* k (- 51 depth))) below 'wrong))))))
   (define threads (for ((k 1 (+ k 1)) (l '() (cons (make-thread (method () (descend k 50))) l)))
                        ((= k 9) l)))
   (print (reduce + 0 (map join-thread threads)))
   (print *level*)"
  "1800
0
")

 ;; Threads made by threads, in waves
 (stress-spawning
  "(define wave
     (method (base)
       (bind ((ts (for ((i 0 (+ i 1)) (l '() (cons (bind ((n (+ base i))) (make-thread (method () n))) l)))
                       ((= i 20) l))))
         (reduce + 0 (map join-thread ts)))))
   (define waves (for ((w 0 (+ w 1)) (l '() (cons (bind ((b (* w 20))) (make-thread (method () (wave b)))) l)))
                      ((= w 10) l)))
   (print (reduce + 0 (map join-thread waves)))"
  "19900
"))
