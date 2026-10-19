;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; <extended-float> arithmetic (chapter 14).
;;;;
;;;; An <extended-float> is a double-float in a structure of its own
;;;; (EXTENDED-FLOAT in compat.lisp), as in the compiler to C it is a C
;;;; double with a class of its own.  Each numeric generic function has
;;;; methods for it, which call the generic function again on the
;;;; double-floats such arguments hold and make a float result an
;;;; <extended-float>: the widest format, so that is the floating-point
;;;; contagion of Common Lisp the chapter adopts.
;;;;
;;;; Methods are ordered by their specializers from the left, so each
;;;; method has <extended-float> for one argument and, for those before it,
;;;; each exact class a number can be (or <object>): it is then the most
;;;; specific method applicable.

(in-package #:dylan)

(defun extended-result (value)
  "VALUE, a float made an <extended-float>."
  (if (floatp value) (make-extended-float (coerce value 'double-float)) value))

(defun extended-float-method (generic-function)
  "The function of the methods of GENERIC-FUNCTION for <extended-float>
arguments."
  (lambda (multiple-values next-method &rest arguments)
    (declare (ignore next-method))
    (let ((result (apply generic-function multiple-values next-method/not-generic
                         (mapcar (lambda (x)
                                   (if (extended-float? x) (extended-float-value x) x))
                                 arguments))))
      (if (and multiple-values (eq result multiple-values))
          ;; Every value: the vector holds the first ones and, in its last
          ;; element, the list of the rest (DYLAN/VALUES).
          (let ((last (1- (length multiple-values))))
            (dotimes (i last)
              (setf (aref multiple-values i) (extended-result (aref multiple-values i))))
            (setf (aref multiple-values last)
                  (mapcar #'extended-result (aref multiple-values last)))
            multiple-values)
          (extended-result result)))))

(let ((before (list <integer> <ratio> <single-float> <double-float>
                    <rectangular-complex> <object>)))
  (dolist (generic-function
           (list dylan/binary+ dylan/binary- dylan/binary* dylan/binary/
                 dylan/unary- dylan/unary/ dylan/binary< dylan/binary= dylan/=hash
                 dylan/abs dylan/sin dylan/cos dylan/tan dylan/asin dylan/acos
                 dylan/atan dylan/atan2 dylan/sinh dylan/cosh dylan/tanh
                 dylan/asinh dylan/acosh dylan/atanh dylan/exp dylan/log
                 dylan/expt dylan/sqrt dylan/modulo dylan/remainder
                 dylan/floor dylan/ceiling dylan/truncate dylan/round
                 dylan/floor/ dylan/ceiling/ dylan/truncate/ dylan/round/
                 dylan/numerator dylan/denominator dylan/rationalize
                 dylan/zero? dylan/positive? dylan/negative? dylan/integral?
                 dylan/odd? dylan/even? dylan/magnitude dylan/angle
                 dylan/real-part dylan/imag-part
                 dylan/make-rectangular dylan/make-polar))
    (let* ((data (generic-function-data generic-function))
           (n (generic-function-data.nrequired data)))
      (labels ((add (specializers)
                 (add-method generic-function
                             (dylan%dylan-callable->method
                              (make-param-list
                               (loop for s in specializers
                                     for i from 0
                                     collect (list (intern (format nil "ARG~d" i)) s))
                               nil
                               (generic-function-data.rest? data)
                               (generic-function-data.keys data))
                              ;; A closure of its own: a method is known by
                              ;; its function
                              (extended-float-method generic-function))))
               (specializers (position prefix)
                 ;; <extended-float> at POSITION; the arguments before it,
                 ;; PREFIX; those after, <object>
                 (append prefix (list <extended-float>)
                         (make-list (- n position 1) :initial-element <object>))))
        (dotimes (position n)
          (if (zerop position)
              (add (specializers 0 '()))
              ;; Two arguments at most among these
              (dolist (class before)
                (add (specializers position (list class))))))))))
