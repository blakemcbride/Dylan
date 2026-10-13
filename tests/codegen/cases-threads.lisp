;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for threads (Phase 7, an extension), in the form of
;;;; cases.lisp.  Threads run in any order, so the programs print only what
;;;; does not depend on it.  Each thread has a 256 KB stack too (glibc gives
;;;; threads the stack limit's size).

(
 ;; join-thread answers the function's values; current-thread.
 (thread-values
  "(define t1 (make-thread (method () (values 1 2))))
   (bind ((a b (join-thread t1))) (print (list a b)))
   (print (join-thread t1))
   (define t2 (make-thread (method () (current-thread))))
   (print (list (id? (join-thread t2) t2) (id? (current-thread) t2)))
   (print (instance? t2 <thread>))"
  "(1 2)
1
(#t #f)
#t
")

 ;; Eight threads add to one counter: under a lock, and atomically.
 (locks-and-atomics
  "(define spawn (method (n f) (for ((i 0 (+ i 1)) (l '() (cons (make-thread f) l))) ((= i n) l))))
   (define join-all (method (l) (unless (id? l '()) (join-thread (car l)) (join-all (cdr l)))))
   (define counter 0)
   (define lock (make <lock>))
   (join-all (spawn 8 (method () (dotimes (i 10000) (with-lock (lock) (set! counter (+ counter 1)))))))
   (print counter)
   (define atomic (make <atomic-integer>))
   (join-all (spawn 8 (method () (dotimes (i 10000) (atomic-add! atomic 1)))))
   (print (atomic-value atomic))
   (print (list (atomic-compare-and-swap! atomic 80000 5) (atomic-compare-and-swap! atomic 80000 6)
                (atomic-value atomic)))
   (define r (make <recursive-lock>))
   (print (with-lock (r) (with-lock (r) 'twice)))"
  "80000
80000
(#t #f 5)
twice
")

 ;; A producer and a consumer, with a condition variable.
 (condition-variable
  "(define lock (make <lock>))
   (define ready (make <condition-variable>))
   (define queue '())
   (define consumer
     (make-thread
       (method ()
         (bind-methods ((take (sum count)
                          (if (= count 100)
                              sum
                              (bind ((item (with-lock (lock)
                                             (while (id? queue '())
                                               (wait-condition-variable ready lock))
                                             (bind ((x (car queue))) (set! queue (cdr queue)) x))))
                                (take (+ sum item) (+ count 1))))))
           (take 0 0)))))
   (dotimes (i 100)
     (with-lock (lock) (set! queue (cons (+ i 1) queue)) (notify-condition-variable ready)))
   (print (join-thread consumer))"
  "5050
")

 ;; Each thread has its own value of a thread variable.
 (thread-variables
  "(define-thread-variable *depth* 0)
   (print (join-thread (make-thread (method () (set! *depth* 9) *depth*))))
   (print *depth*)
   (print (dynamic-bind ((*depth* 5)) *depth*))
   (print *depth*)
   (print (join-thread (make-thread (method () (dynamic-bind ((*depth* 7)) (+ *depth* 1))))))
   (print (bind-exit (out) (dynamic-bind ((*depth* 3)) (out *depth*))))
   (print *depth*)"
  "9
0
5
0
8
3
0
")

 ;; A serious condition a thread does not handle ends that thread, and
 ;; join-thread signals a <thread-error> holding it.
 (unhandled-in-thread
  "(define t1 (make-thread (method () (error \"boom ~a\" 1))))
   (print (handler-case (join-thread t1)
            ((<thread-error> condition: c)
             (condition-format-arguments (thread-error-condition c)))))
   (print (join-thread (make-thread (method () (handler-case (car 5) ((<error>) 'handled))))))
   (print 'alive)
   (join-thread (make-thread (method () (error \"again\"))))"
  "(1)
handled
alive
"
  :error "a thread ended with an unhandled condition: again")

 ;; Threads allocating at once, with the collector running.
 (allocation
  "(define sum-list (method (l s) (if (id? l '()) s (sum-list (cdr l) (+ s (car (car l)))))))
   (define work (method () (bind ((l (for ((i 0 (+ i 1)) (l '() (cons (list i) l))) ((= i 200000) l))))
                             (sum-list l 0))))
   (define ts (for ((i 0 (+ i 1)) (l '() (cons (make-thread work) l))) ((= i 6) l)))
   (bind-methods ((all (l) (unless (id? l '()) (print (join-thread (car l))) (all (cdr l)))))
     (all ts))"
  "19999900000
19999900000
19999900000
19999900000
19999900000
19999900000
")

 ;; A million tail calls on a thread's own stack.
 (thread-tail-calls
  "(define loop (method (n) (if (= n 0) 'done (loop (- n 1)))))
   (print (join-thread (make-thread (method () (loop 1000000)))))"
  "done
")

 (lock-not-held
  "(define l (make <lock>)) (print (handler-case (release-lock l) ((<error>) 'caught))) (acquire-lock l) (acquire-lock l)"
  "caught
"
  :error "holds the lock already")
)
