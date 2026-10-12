;;;; The benchmarks of benchmarks.dyl in plain Common Lisp, for comparison.
;;;; Each BENCH-<NAME> answers what the Dylan one does.

(defpackage #:dylan-bench-reference (:use #:common-lisp))
(in-package #:dylan-bench-reference)

(defun next-random (seed)
  (mod (+ (* seed 1103515245) 12345) 2147483648))

(defun random-list (n)
  (let ((seed 42) (result '()))
    (dotimes (i n result)
      (setf seed (next-random seed))
      (push (mod seed 100000) result))))

(defun bench-table (n)
  (let ((table (make-hash-table)) (sum 0))
    (dotimes (i n) (setf (gethash i table) (* i 2)))
    (dotimes (i n) (setf (gethash i table) (+ (gethash i table) 1)))
    (dotimes (i n sum) (incf sum (gethash i table 0)))))

(defun bench-sort (n)
  (let* ((l (random-list n))
         (up (sort (copy-list l) #'<))
         (down (sort (coerce l 'simple-vector) (lambda (a b) (> a b)))))
    (+ (first up) (car (last up)) (svref down 0) (svref down (- n 1)))))

(defun bench-string (n)
  (let ((pieces '()) (total 0))
    (dotimes (i n)
      (push (concatenate 'string "ab" "cde" (if (evenp i) "x" "yz")) pieces))
    (dolist (s pieces total) (incf total (length s)))))

(defun bench-deque (n)
  (let ((queue (make-array 0 :adjustable t :fill-pointer t)) (sum 0))
    (dotimes (i n) (vector-push-extend i queue))
    (dotimes (i n sum) (incf sum (aref queue i)))))

(defun bench-stretchy (n)
  (let ((v (make-array 0 :adjustable t :fill-pointer t)) (sum 0))
    (dotimes (i n) (vector-push-extend i v))
    (dotimes (i n sum) (incf sum (aref v i)))))

(defun bench-range (n)
  (loop for i from 0 below n sum i))

(defun find-first-over (l limit)
  (dolist (x l) (when (> x limit) (return-from find-first-over x)))
  nil)

(defun bench-exit (n)
  (let ((l '(1 2 3 4 5 6 7 8 9 10)) (sum 0))
    (dotimes (i n sum) (incf sum (find-first-over l 5)))))

(defun bench-condition (n)
  (let ((caught 0))
    (dotimes (i n caught)
      (handler-case (if (evenp i) (error "odd one out") i)
        (error () (incf caught))))))

(defclass animal () ())
(defclass mammal (animal) ())
(defclass dog (mammal) ())
(defclass puppy (dog) ())
(defgeneric weight (a))
(defmethod weight ((a animal)) 1)
(defmethod weight ((m mammal)) (+ 2 (call-next-method)))
(defmethod weight ((d dog)) (+ 3 (call-next-method)))
(defmethod weight ((p puppy)) (+ 4 (call-next-method)))

(defun bench-next-method (n)
  (let ((animals (list (make-instance 'animal) (make-instance 'mammal)
                       (make-instance 'dog) (make-instance 'puppy)))
        (sum 0))
    (dotimes (i n sum)
      (dolist (a animals) (incf sum (weight a))))))

(defun tak (x y z)
  (if (< y x)
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))
      z))

(defun bench-tak (n)
  (let ((sum 0))
    (dotimes (i n sum) (incf sum (tak 18 12 6)))))

;;; Dylan reads 3.0 as a double-float.
(defun mandelbrot-inside-p (cr ci)
  (let ((zr 0.0d0) (zi 0.0d0))
    (do ((i 0 (+ i 1)))
        ((or (= i 50) (> (+ (* zr zr) (* zi zi)) 4.0d0)) (= i 50))
      (let ((new-zr (+ (- (* zr zr) (* zi zi)) cr)))
        (setf zi (+ (* 2.0d0 zr zi) ci))
        (setf zr new-zr)))))

(defun bench-float (n)
  (let ((count 0))
    (dotimes (y n count)
      (dotimes (x n)
        (when (mandelbrot-inside-p (- (/ (* 3.0d0 x) n) 2.0d0)
                                   (- (/ (* 2.0d0 y) n) 1.0d0))
          (incf count))))))
