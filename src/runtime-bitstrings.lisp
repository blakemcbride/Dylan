;;;; -*- Mode: Lisp -*-
;;;;
;;;;              Copyright 1992 Digital Equipment Corporation
;;;;                         All Rights Reserved
;;;;
;;;; Permission to use, copy, and modify this software and its documentation is
;;;; hereby granted only under the following terms and conditions.  Both the
;;;; above copyright notice and this permission notice must appear in all copies
;;;; of the software, derivative works or modified versions, and any portions
;;;; thereof, and both notices must appear in supporting documentation.
;;;;
;;;; Users of this software agree to the terms and conditions set forth herein,
;;;; and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;;;; right and license under any changes, enhancements or extensions made to the
;;;; core functions of the software, including but not limited to those affording
;;;; compatibility with other hardware or software environments, but excluding
;;;; applications which incorporate this software.  Users further agree to use
;;;; their best efforts to return to Digital any such changes, enhancements or
;;;; extensions that they make and inform Digital of noteworthy uses of this
;;;; software.  Correspondence should be provided to Digital at:
;;;;
;;;;			Director, Cambridge Research Lab
;;;;			Digital Equipment Corp
;;;;			One Kendall Square, Bldg 700
;;;;			Cambridge MA 02139
;;;;
;;;; This software may be distributed (but not offered for sale or transferred
;;;; for compensation) to third parties, provided such third parties agree to
;;;; abide by the terms and conditions of this notice.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;;;; WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;;;; CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;;;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;;;; ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;; SOFTWARE.
;;;;
;;;; The Common Lisp translation is Copyright 2026 Blake McBride.  It is a
;;;; derivative work of Thomas and is governed by the notice above; see
;;;; ../LICENSE.
;;;;
;;;; Support for handling integers as bit strings
;;;;                              (was Thomas's src/runtime-bitstrings.scm)

(in-package #:dylan)

;;;
;;; UTILITY FUNCTIONS
;;;

(defun flip-bit (bit) (if (= 0 bit) 1 0))

(defun negate-bits (l)
  (loop with answer = '()
        with remainder = (reverse (mapcar #'flip-bit l))
        do (cond ((null remainder) (return l)) ; (0 0 0 0 ...) was input
                 ((= (car remainder) 0)
                  (return (append (reverse (cdr remainder)) '(1) answer)))
                 (t (push (flip-bit (car remainder)) answer)
                    (pop remainder)))))

(defun integer->bits (integer)
  (if (minusp integer)
      (let ((nbits (negate-bits (cdr (integer->bits (- integer))))))
        (if (zerop (car nbits))
            (cons 1 nbits)
            nbits))
      (loop with bits = '()
            with value = integer
            do (cond ((zerop value) (return (cons 0 bits)))
                     ((evenp value) (push 0 bits)
                                    (setf value (truncate value 2)))
                     (t (push 1 bits)
                        (setf value (truncate (- value 1) 2)))))))

(defun pad-bitstring-to (n bits)
  (let ((basic-bits (length bits)))
    (when (< n basic-bits)
      (dylan-call dylan/error "bitstring internal length error"))
    (append (make-list (- n basic-bits)
                       :initial-element (if (null bits) 0 (car bits)))
            bits)))

(defun integers->same-length (integer1 integer2 continue)
  (let* ((bits1 (integer->bits integer1))
         (bits2 (integer->bits integer2))
         (length1 (length bits1))
         (length2 (length bits2)))
    (cond ((= length1 length2) (funcall continue bits1 bits2))
          ((< length1 length2)
           (funcall continue (pad-bitstring-to length2 bits1) bits2))
          (t (funcall continue bits1 (pad-bitstring-to length1 bits2))))))

(defun bits->integer (bits)
  (flet ((unsigned->integer (bits)
           (let ((result 0))
             (dolist (bit bits result)
               (setf result (+ bit (* 2 result)))))))
    (cond ((zerop (car bits)) (unsigned->integer (cdr bits)))
          ((null (cdr bits)) -1)
          (t (- (unsigned->integer (negate-bits (cdr bits))))))))

(defun logical-bitstr (vals)
  "VALS is a vector #(0/0 0/1 1/0 1/1)."
  (lambda (int1 int2)
    (bits->integer
     (integers->same-length int1 int2
                            (lambda (bits1 bits2)
                              (mapcar (lambda (a b)
                                        (aref vals (+ (* 2 a) b)))
                                      bits1 bits2))))))

(defun logical-op-only-rest-args (no-arg-value vals)
  (lambda (&rest all-integers)
    (if (not all-integers)
        no-arg-value
        (let ((op-result (car all-integers)))
          (dolist (n (cdr all-integers) op-result)
            (setf op-result (funcall (logical-bitstr vals) op-result n)))))))

;;;
;;; DYLAN FUNCTIONS
;;;

(defparameter dylan/ash
  ;; Assume (ash int count) shifts int left by count bits if count>0, right
  ;; if count<0
  (dylan%generic-fn 'ash two-integers
                    (lambda (integer shift)
                      (dylan-call dylan/floor (* (expt 2 shift) integer)))))

(defparameter dylan/logand
  (dylan%generic-fn 'logand only-rest-args
                    (logical-op-only-rest-args -1 #(0 0 0 1))))

(defparameter dylan/logandc1
  (dylan%generic-fn 'logandc1 two-integers (logical-bitstr #(0 1 0 0))))

(defparameter dylan/logandc2
  (dylan%generic-fn 'logandc2 two-integers (logical-bitstr #(0 0 1 0))))

(defparameter dylan/logbit?
  ;; Assuming this is a bit index primitive with 0 being the low bit and
  ;; assuming it should treat the integer as sign extended.  Number
  ;; representation is 2's complement (sign extended).
  (dylan%generic-fn
   'logbit two-integers
   (lambda (index integer)
     (when (minusp index)
       (dylan-call dylan/error "logbit? -- negative index" index integer))
     (let* ((integer-bits (integer->bits integer))
            (bit-index (- (length integer-bits) index 1)))
       (if (minusp bit-index)
           (minusp integer)
           (= (nth bit-index integer-bits) 1))))))

(defparameter dylan/logeqv
  (dylan%generic-fn 'logeqv two-integers
                    (logical-op-only-rest-args -1 #(1 0 0 1))))

(defparameter dylan/logior
  (dylan%generic-fn 'logior only-rest-args
                    (logical-op-only-rest-args 0 #(0 1 1 1))))

(defparameter dylan/lognand
  (dylan%generic-fn 'lognand two-integers (logical-bitstr #(1 1 1 0))))

(defparameter dylan/lognor
  (dylan%generic-fn 'lognor two-integers (logical-bitstr #(1 0 0 0))))

(defparameter dylan/lognot
  (dylan%generic-fn
   'lognot one-integer
   (lambda (integer)
     (bits->integer (mapcar #'flip-bit (integer->bits integer))))))

(defparameter dylan/logorc1
  (dylan%generic-fn 'logorc1 two-integers (logical-bitstr #(1 1 0 1))))

(defparameter dylan/logorc2
  (dylan%generic-fn 'logorc2 two-integers (logical-bitstr #(1 0 1 1))))

(defparameter dylan/logxor
  (dylan%generic-fn 'logxor two-integers
                    (logical-op-only-rest-args 0 #(0 1 1 0))))
