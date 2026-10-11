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
;;;; Methods used in the Dylan environment
;;;;                                 (was Thomas's src/runtime-methods.scm)

(in-package #:dylan)

;;; Methods (not generically dispatched)
;;;
;;; DYLAN%FUNCTION->METHOD lives in generic.lisp -- it takes a Lisp
;;; function and converts it to a Dylan-callable procedure by ignoring the
;;; multiple-values and next-method arguments that come from Dylan.

(defparameter dylan/+
  (dylan%function->method
   only-rest-args
   (lambda (&rest rest-args)
     (if (null rest-args)
         0
         (let ((sum (car rest-args)))
           (dolist (x (cdr rest-args) sum)
             (setf sum (dylan-call dylan/binary+ sum x))))))))

(defparameter dylan/*
  (dylan%function->method
   only-rest-args
   (lambda (&rest rest-args)
     (if (null rest-args)
         1
         (let ((sum (car rest-args)))
           (dolist (x (cdr rest-args) sum)
             (setf sum (dylan-call dylan/binary* sum x))))))))

(defparameter dylan/-
  (dylan%function->method
   at-least-one-number
   (lambda (num &rest rest-num)
     (if (null rest-num)
         (- num)
         (let ((sum num))
           (dolist (x rest-num sum)
             (setf sum (dylan-call dylan/binary- sum x))))))))

(defparameter dylan//
  (dylan%function->method
   at-least-one-number
   (lambda (num &rest rest-num)
     (if (null rest-num)
         (/ num)
         (let ((sum num))
           (dolist (x rest-num sum)
             (setf sum (dylan-call dylan/binary/ sum x))))))))

(defparameter dylan/identity
  (dylan%function->method one-object #'identity))

(defparameter dylan/=
  (dylan%function->method
   at-least-two-objects
   (lambda (obj1 obj2 &rest rest-objs)
     (and (dylan-call dylan/binary= obj1 obj2)
          (every (lambda (x) (dylan-call dylan/binary= obj1 x)) rest-objs)
          t))))

(defparameter dylan//=
  (dylan%function->method
   two-objects
   (lambda (obj1 obj2) (not (dylan-call dylan/binary= obj1 obj2)))))

(defparameter dylan/<
  (dylan%function->method
   at-least-two-objects
   (lambda (obj1 obj2 &rest rest-objs)
     (and (dylan-call dylan/binary< obj1 obj2)
          (let ((prev-obj obj2))
            (dolist (x rest-objs t)
              (unless (dylan-call dylan/binary< prev-obj x) (return nil))
              (setf prev-obj x)))))))

(defparameter dylan/>=
  (dylan%function->method
   at-least-two-objects
   (lambda (obj1 obj2 &rest rest-objs)
     (and (not (dylan-call dylan/binary< obj1 obj2))
          (let ((prev-obj obj2))
            (dolist (x rest-objs t)
              (when (dylan-call dylan/binary< prev-obj x) (return nil))
              (setf prev-obj x)))))))

(defparameter dylan/>
  (dylan%function->method
   at-least-two-objects
   (lambda (obj1 obj2 &rest rest-objs)
     (and (not (or (dylan-call dylan/binary< obj1 obj2)
                   (dylan-call dylan/binary= obj1 obj2)))
          (let ((prev-obj obj2))
            (dolist (x rest-objs t)
              (when (or (dylan-call dylan/binary< prev-obj x)
                        (dylan-call dylan/binary= prev-obj x))
                (return nil))
              (setf prev-obj x)))))))

(defparameter dylan/<=
  (dylan%function->method
   at-least-two-objects
   (lambda (obj1 obj2 &rest rest-objs)
     (and (or (dylan-call dylan/binary< obj1 obj2)
              (dylan-call dylan/binary= obj1 obj2))
          (let ((prev-obj obj2))
            (dolist (x rest-objs t)
              (unless (or (dylan-call dylan/binary< prev-obj x)
                          (dylan-call dylan/binary= prev-obj x))
                (return nil))
              (setf prev-obj x)))))))

(defparameter dylan/always
  (dylan%function->method
   one-object
   (lambda (obj) (lambda (&rest args) (declare (ignore args)) obj))))

(defparameter dylan/id?
  (dylan%function->method
   at-least-two-objects
   (lambda (obj1 &rest others)
     (every (lambda (x) (id? obj1 x)) others))))

(defparameter dylan/min
  (dylan%function->method
   at-least-one-real
   (lambda (real1 &rest others)
     ;; N.B. the Scheme compared REAL1 rather than MIN-SO-FAR here (and
     ;; likewise in DYLAN/MAX below); fixed.
     (let ((min-so-far real1))
       (dolist (x others min-so-far)
         (setf min-so-far
               (if (dylan-call dylan/binary< min-so-far x) min-so-far x)))))))

(defparameter dylan/max
  (dylan%function->method
   at-least-one-real
   (lambda (real1 &rest others)
     ;; N.B. as in DYLAN/MIN, the Scheme compared REAL1 rather than
     ;; MAX-SO-FAR; fixed.
     (let ((max-so-far real1))
       (dolist (x others max-so-far)
         (setf max-so-far
               (if (not (dylan-call dylan/binary< max-so-far x)) max-so-far x)))))))

(defun reduce (l fn init-value)
  (let ((value init-value))
    (dolist (x l value)
      (setf value (funcall fn x value)))))

;;; Chapter 14: with no arguments lcm is 1 and gcd 0, with one each answers
;;; it, and otherwise the arguments are combined left to right.

(defparameter dylan/lcm
  (dylan%function->method
   only-rest-args
   (lambda (&rest args)
     (if (and args (null (cdr args)))
         (car args)
         (reduce args (lambda (x value) (dylan-call dylan/binary-lcm value x))
                 1)))))

(defparameter dylan/gcd
  (dylan%function->method
   only-rest-args
   (lambda (&rest args)
     (if (and args (null (cdr args)))
         (car args)
         (reduce args (lambda (x value) (dylan-call dylan/binary-gcd value x))
                 0)))))

;;; Special functions

(defparameter dylan/values
  (lambda (multiple-values? next-method &rest values)
    (declare (ignore next-method))
    (if (not multiple-values?)
        (if (null values) nil (car values))
        (let ((last-loc (- (length multiple-values?) 1)))
          (do ((index 0 (+ index 1))
               (rest values (cdr rest)))
              ((or (null rest) (= index last-loc))
               (setf (aref multiple-values? last-loc) rest)
               multiple-values?)        ; Return vector itself.  See BIND
            (setf (aref multiple-values? index) (car rest)))))))

(defparameter dylan/not (make-dylan-callable #'not 1))

;;; Generic functions

(defun dylan%generic-fn (name param-list lisp-operation)
  "LISP-OPERATION can be NIL, meaning \"no methods initially available\"."
  (let ((generic-function
          (dylan%create-generic-function name
                                         (param-list.nrequired param-list)
                                         (param-list.keys param-list)
                                         (param-list.rest? param-list))))
    (when lisp-operation
      (add-method generic-function
                  (dylan%function->method param-list lisp-operation)))
    generic-function))

(defun dylan%make-<object> (class &rest rest)
  (flet ((gather-from-slots (slot-fn)
           (let ((keywords '()))
             (map nil (lambda (slot)
                        (let ((k (and slot (slot-storage slot)
                                      (funcall slot-fn (slot-storage slot)))))
                          (when k (push k keywords))))
                  (class.slots class))
             keywords)))
    (dylan%keyword-validate nil rest t)
    (when (class.abstract? class)
      (dylan-call dylan/error "make -- cannot instantiate this class" class))
    ;; MIT Scheme's (MAKE-VECTOR n) left the elements #F, and the slot
    ;; initialisation relies on that for the slots it does not touch.
    (let ((instance-data (make-array (class.instance-data-size class)
                                     :initial-element nil))
          (slots (class.slots class)))
      (let ((required (gather-from-slots #'slot.required-init-keyword)))
        (dolist (k required)
          (dylan%find-keyword
           rest k (lambda ()
                    (dylan-call dylan/error
                                "make -- missing required keyword" k rest))))
        (vector-iterate slots
                        (lambda (i slot)
                          (declare (ignore i))
                          (when slot
                            (initialize-slot! slot rest instance-data
                                              '(instance)))))
        (let ((result (make-dylan-instance class nil instance-data)))
          (add-to-population! (class.instances class) result)
          (dylan-apply dylan/initialize result rest)
          result)))))

(defparameter dylan/make
  (dylan%generic-fn 'make
                    (make-param-list `((class ,<class>)) nil nil t)
                    #'dylan%make-<object>))

(defparameter dylan/initialize
  (dylan%generic-fn 'initialize
                    (make-param-list `((object ,<object>)) nil nil t)
                    (lambda (instance &rest rest)
                      (declare (ignore rest))
                      instance)))

(defparameter dylan/slot-initialized?
  (dylan%generic-fn
   'slot-initialized?
   (make-param-list `((instance ,<object>) (getter ,<generic-function>))
                    nil nil nil)
   (lambda (instance getter)
     (let* ((class (instance.class instance))
            (slots (class.slots class))
            (the-slot (same-slot-getter-in-slot-vector->slot getter slots)))
       (unless the-slot
         (dylan-call dylan/error "slot-initialized? -- no such slot"
                     instance getter class))
       (not (eq *the-uninitialized-slot-value*
                (case (slot.allocation the-slot)
                  ((virtual constant) 'initialized)
                  ((class) (let ((data-loc (slot.data-location the-slot)))
                             (aref (class.class-data (car data-loc))
                                   (cdr data-loc))))
                  ((each-subclass) (aref (class.class-data class)
                                         (slot.data-location the-slot)))
                  ((instance) (aref (instance.data instance)
                                    (slot.data-location the-slot)))
                  (t (dylan-call dylan/error
                                 "slot-initialized? -- bad allocation"
                                 (slot.allocation the-slot)
                                 instance getter class)))))))))

;;; Arithmetic

(defparameter dylan/odd? (dylan%generic-fn 'odd? one-integer #'oddp))
(defparameter dylan/even? (dylan%generic-fn 'even? one-integer #'evenp))
(defparameter dylan/zero? (dylan%generic-fn 'zero? one-number #'zerop))
(defparameter dylan/positive? (dylan%generic-fn 'positive? one-number #'plusp))
(defparameter dylan/negative? (dylan%generic-fn 'negative? one-real #'minusp))
(defparameter dylan/integral?
  ;; True of any number with an integer value, as Scheme's INTEGER?
  ;; (2.0 included).
  (dylan%generic-fn 'integral? one-number
                    (lambda (number)
                      (or (integerp number)
                          (and (floatp number)
                               (ignore-errors (= number (ffloor number))))))))

;;; Chapter 14: abs, the trigonometric and hyperbolic functions, exp, log,
;;; expt and sqrt are as defined in Common Lisp -- so the square root of an
;;; exact number is a float, even (sqrt 25).  The manual's own examples
;;; (chapters 4, 10 and 17) show these functions answering double-floats
;;; for exact arguments, (sqrt 2) being 1.4142135623730951, so exact
;;; arguments are made double-floats first: Common Lisp's rule with
;;; double-float as the default format.  (The Scheme followed Scheme, where
;;; (sqrt 25) is an exact 5.)

(defun ->inexact-argument (number)
  (typecase number
    (rational (float number 1d0))
    ((complex rational) (coerce number '(complex double-float)))
    (t number)))

(defun inexact-function (fn)
  (lambda (&rest numbers) (apply fn (mapcar #'->inexact-argument numbers))))

(defparameter dylan/abs (dylan%generic-fn 'abs one-number #'abs))
(defparameter dylan/sin (dylan%generic-fn 'sin one-number (inexact-function #'sin)))
(defparameter dylan/cos (dylan%generic-fn 'cos one-number (inexact-function #'cos)))
(defparameter dylan/tan (dylan%generic-fn 'tan one-number (inexact-function #'tan)))
(defparameter dylan/asin
  (dylan%generic-fn 'asin one-number (inexact-function #'asin)))
(defparameter dylan/acos
  (dylan%generic-fn 'acos one-number (inexact-function #'acos)))
(defparameter dylan/atan
  (dylan%generic-fn 'atan one-number (inexact-function #'atan)))
(defparameter dylan/atan2
  (dylan%generic-fn 'atan2 two-numbers (inexact-function #'atan)))
(defparameter dylan/exp (dylan%generic-fn 'exp one-number (inexact-function #'exp)))
(defparameter dylan/log (dylan%generic-fn 'log one-number (inexact-function #'log)))
(defparameter dylan/expt
  ;; Exact for an exact base and integer power, as in Common Lisp.
  (dylan%generic-fn 'expt two-numbers
                    (lambda (base power)
                      (if (and (rationalp base) (integerp power))
                          (expt base power)
                          (expt (->inexact-argument base)
                                (->inexact-argument power))))))
(defparameter dylan/sqrt
  (dylan%generic-fn 'sqrt one-number (inexact-function #'sqrt)))

(defparameter dylan/modulo
  (dylan%generic-fn
   'modulo two-reals
   (lambda (r1 r2)
     (let ((multiple-values (vector nil nil '())))
       (dylan-mv-call dylan/floor/ multiple-values r1 r2)
       (aref multiple-values 1)))))     ; The remainder, not the quotient

(defparameter dylan/remainder
  (dylan%generic-fn 'remainder two-reals
                    (lambda (real1 real2)
                      (- real1 (* real2 (truncate (/ real1 real2)))))))

(defparameter dylan/unary- (dylan%generic-fn 'unary- one-number #'-))
(defparameter dylan/unary/ (dylan%generic-fn 'unary/ one-number #'/))

(defparameter dylan/binary+ (dylan%generic-fn 'binary+ two-numbers #'+))
(defparameter dylan/binary* (dylan%generic-fn 'binary* two-numbers #'*))
(defparameter dylan/binary- (dylan%generic-fn 'binary- two-numbers #'-))
(defparameter dylan/binary/ (dylan%generic-fn 'binary/ two-numbers #'/))

;;; Class stuff

;;; Chapter 11: all-superclasses, direct-superclasses and slot-descriptors
;;; answer an empty sequence for a sealed class.

(defparameter dylan/all-superclasses
  (dylan%generic-fn 'all-superclasses one-class
                    (lambda (class)
                      (if (class.sealed? class)
                          '()
                          (map-over-all-superclasses! class #'identity)))))

(defparameter dylan/direct-superclasses
  (dylan%generic-fn 'direct-superclasses one-class
                    (lambda (class)
                      (if (class.sealed? class)
                          '()
                          (class.superclasses class)))))

(defparameter dylan/setter/direct-superclasses/
  (dylan%generic-fn 'direct-superclasses
                    (make-param-list `((class ,<class>)
                                       (new-superclass-list ,<sequence>))
                                     nil nil nil)
                    #'dylan%set-direct-superclasses))

(defparameter dylan/direct-subclasses
  (dylan%generic-fn 'direct-subclasses one-class
                    (lambda (class)
                      (population->list (class.subclasses class)))))

(defparameter dylan/instance?
  (dylan%generic-fn 'instance?
                    (make-param-list `((object ,<object>) (class ,<class>))
                                     nil nil nil)
                    (lambda (obj class) (subclass? (get-type obj) class))))

(defparameter dylan/subclass?
  (dylan%generic-fn 'subclass?
                    (make-param-list `((class-1 ,<class>) (class-2 ,<class>))
                                     nil nil nil)
                    #'subclass?))

(defparameter dylan/object-class
  (dylan%generic-fn 'object-class one-object #'get-type))

(defparameter dylan/slot-descriptors
  (dylan%generic-fn 'slot-descriptors one-class
                    (lambda (class)
                      (if (class.sealed? class)
                          (vector)
                          (class.slots class)))))

(defparameter dylan/slot-getter
  (dylan%generic-fn 'slot-getter one-slot #'slot.getter))
(defparameter dylan/slot-setter
  (dylan%generic-fn 'slot-setter one-slot #'slot.setter))
(defparameter dylan/slot-type
  (dylan%generic-fn 'slot-type one-slot #'slot.type))
(defparameter dylan/slot-allocation
  (dylan%generic-fn 'slot-allocation one-slot #'slot.allocation))

(defparameter dylan/binary< (dylan%generic-fn 'binary< two-numbers #'<))

(defparameter dylan/binary=
  ;; Use EQ if the objects are not of the same class.
  (dylan%generic-fn 'binary= two-objects #'eq))

(add-method dylan/binary= (dylan%function->method two-numbers #'=))

(defparameter dylan/as-lowercase
  ;; Takes <character>s or <string>s.
  (dylan%generic-fn 'as-lowercase one-object nil))

(add-method dylan/as-lowercase
            (dylan%function->method one-char #'char-downcase))

(defparameter dylan/as-uppercase
  ;; Takes <character>s or <string>s.
  (dylan%generic-fn 'as-uppercase one-object nil))

(add-method dylan/as-uppercase
            (dylan%function->method one-char #'char-upcase))

;;; =HASH (chapter 9): objects that are = must hash alike.  Objects that
;;; binary= compares with id? hash by identity; numbers by value (so that 1
;;; and 1.0 agree); collections by their elements, since collections are =
;;; when their keys and elements are (so a string and a list of the same
;;; characters hash alike).  The Scheme had methods for reals only, so
;;; tables could not hold symbol or string keys.

(defvar *object-ids*
  #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized nil)
  #-sbcl (make-hash-table :test 'eq))
(defvar *next-object-id* 0)

(defun object-id (object)
  (or (gethash object *object-ids*)
      (setf (gethash object *object-ids*) (incf *next-object-id*))))

(defun combine-hash (hash1 hash2)
  (logand (+ (* 31 hash1) hash2) most-positive-fixnum))

(defparameter dylan/=hash
  (dylan%generic-fn '=hash one-object
                    (lambda (object)
                      (if (or (symbolp object) (characterp object))
                          (sxhash object)
                          (object-id object)))))

(add-method dylan/=hash
            (dylan%function->method
             one-number
             (lambda (number)
               (flet ((real-hash (real)
                        (if (and (floatp real)
                                 (not (ignore-errors (rational real))))
                            0           ; Infinities and NaNs
                            (sxhash (rational real)))))
                 (if (complexp number)
                     (combine-hash (real-hash (realpart number))
                                   (real-hash (imagpart number)))
                     (real-hash number))))))

(add-method dylan/=hash
            (dylan%function->method
             one-sequence
             (lambda (sequence)
               (let ((hash 17))
                 (iterate-until (lambda (element)
                                  (setf hash (combine-hash
                                              hash
                                              (dylan-call dylan/=hash element)))
                                  nil)
                                sequence)
                 hash))))

(add-method dylan/=hash
            (dylan%function->method
             (make-param-list
              `((collection ,<explicit-key-collection>)) nil nil nil)
             (lambda (collection)
               ;; Independent of iteration order, which need not be stable.
               (let ((hash 0)
                     (keys (dylan-call dylan/key-sequence collection)))
                 (iterate-until
                  (lambda (key)
                    (setf hash
                          (logxor hash
                                  (combine-hash
                                   (dylan-call dylan/=hash key)
                                   (dylan-call dylan/=hash
                                               (dylan-call dylan/element
                                                           collection key)))))
                    nil)
                  keys)
                 hash))))

(defparameter dylan/floor (dylan%generic-fn 'floor one-real nil))

(add-method
 dylan/floor
 (dylan%dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    (declare (ignore next-method))
    (dylan-mv-call dylan/values multiple-values
                   (floor num) (- num (floor num))))))

(defparameter dylan/ceiling (dylan%generic-fn 'ceiling one-real nil))

(add-method
 dylan/ceiling
 (dylan%dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    (declare (ignore next-method))
    (dylan-mv-call dylan/values multiple-values
                   (ceiling num) (- num (ceiling num))))))

(defparameter dylan/truncate (dylan%generic-fn 'truncate one-real nil))

(add-method
 dylan/truncate
 (dylan%dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    (declare (ignore next-method))
    (dylan-mv-call dylan/values multiple-values
                   (truncate num) (- num (truncate num))))))

(defparameter dylan/round (dylan%generic-fn 'round one-real nil))

(add-method
 dylan/round
 (dylan%dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    (declare (ignore next-method))
    (dylan-mv-call dylan/values multiple-values
                   (round num) (- num (round num))))))

(defparameter dylan/floor/ (dylan%generic-fn 'floor/ two-reals nil))

(add-method
 dylan/floor/
 (dylan%dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    (declare (ignore next-method))
    (let ((floor-div-result (floor (/ real1 real2))))
      (dylan-mv-call dylan/values multiple-values
                     floor-div-result
                     (- real1 (* real2 floor-div-result)))))))

(defparameter dylan/ceiling/ (dylan%generic-fn 'ceiling/ two-reals nil))

(add-method
 dylan/ceiling/
 (dylan%dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    (declare (ignore next-method))
    (let ((ceiling-div-result (ceiling (/ real1 real2))))
      (dylan-mv-call dylan/values multiple-values
                     ceiling-div-result
                     (- real1 (* real2 ceiling-div-result)))))))

(defparameter dylan/truncate/ (dylan%generic-fn 'truncate/ two-reals nil))

(add-method
 dylan/truncate/
 (dylan%dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    (declare (ignore next-method))
    (let ((truncate-div-result (truncate (/ real1 real2))))
      (dylan-mv-call dylan/values multiple-values
                     truncate-div-result
                     (- real1 (* real2 truncate-div-result)))))))

(defparameter dylan/round/ (dylan%generic-fn 'round/ two-reals nil))

(add-method
 dylan/round/
 (dylan%dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    (declare (ignore next-method))
    (let ((round-div-result (round (/ real1 real2))))
      (dylan-mv-call dylan/values multiple-values
                     round-div-result
                     (- real1 (* real2 round-div-result)))))))

(defparameter dylan/add-method
  (let* ((params (make-param-list
                  `((generic-function ,<generic-function>) (method ,<method>))
                  nil nil nil))
         (generic-function (dylan%generic-fn 'add-method params nil)))
    (add-method
     generic-function
     (dylan%make-method
      params
      (lambda (multiple-values next-method generic-function method)
        (declare (ignore next-method))
        (add-method generic-function method
                    (lambda (new old)
                      (dylan-mv-call dylan/values multiple-values new old))))))
    generic-function))

(defparameter dylan/shallow-copy
  (dylan%generic-fn
   'shallow-copy one-object
   (lambda (obj)
     (dylan-call dylan/error
                 "shallow-copy -- not specialized for this object type" obj))))

(defparameter dylan/binary-gcd (dylan%generic-fn 'binary-gcd two-integers #'gcd))
(defparameter dylan/binary-lcm (dylan%generic-fn 'binary-lcm two-integers #'lcm))

;;; As in R4RS: an inexact argument's numerator and denominator are those
;;; of its exact value, made inexact.
(defparameter dylan/denominator
  (dylan%generic-fn 'denominator one-real
                    (lambda (real)
                      (if (floatp real)
                          (float (denominator (rational real)) real)
                          (denominator real)))))
(defparameter dylan/numerator
  (dylan%generic-fn 'numerator one-real
                    (lambda (real)
                      (if (floatp real)
                          (float (numerator (rational real)) real)
                          (numerator real)))))

(defparameter dylan/angle (dylan%generic-fn 'angle one-number #'phase))
(defparameter dylan/magnitude (dylan%generic-fn 'magnitude one-number #'abs))
(defparameter dylan/imag-part (dylan%generic-fn 'imag-part one-number #'imagpart))
(defparameter dylan/real-part (dylan%generic-fn 'real-part one-number #'realpart))
(defparameter dylan/rationalize
  (dylan%generic-fn 'rationalize one-number #'rationalize))

(defparameter dylan/init-function
  (dylan%generic-fn 'init-function one-slot #'slot.init-function))

(defparameter dylan/init-keyword
  (dylan%generic-fn 'init-keyword one-slot #'slot.init-keyword))

(defparameter dylan/init-value (dylan%generic-fn 'init-value one-slot nil))

(add-method
 dylan/init-value
 (dylan%dylan-callable->method
  one-slot
  (lambda (multiple-values next-method slot)
    (declare (ignore next-method))
    (if (slot.has-initial-value? slot)
        (dylan-mv-call dylan/values multiple-values (slot.init-value slot) t)
        (dylan-mv-call dylan/values multiple-values nil nil)))))

(defparameter dylan/applicable-method?
  (dylan%generic-fn
   'applicable-method?
   (make-param-list `((fn ,<function>)) nil t nil)
   (lambda (fn &rest args)
     ;; METHOD-APPLICABLE? answers a list of specificities; Dylan wants #t.
     (cond ((dylan%generic-function? fn)
            (and (any? (lambda (method) (method-applicable? method args))
                       (generic-function.methods fn))
                 t))
           ((dylan%method? fn) (and (method-applicable? fn args) t))
           (t nil)))))

(defparameter dylan/apply
  (dylan%generic-fn 'apply (make-param-list `((fn ,<function>)) nil t nil) nil))

(add-method
 dylan/apply
 (dylan%dylan-callable->method
  (make-param-list `((fn ,<function>)) nil t nil)
  (lambda (multiple-values next-method fn &rest args)
    (dylan-full-apply fn multiple-values next-method
                      (split-last
                       args
                       (lambda (early end)
                         (append early
                                 (if (null end)
                                     '()
                                     (iterate->list #'identity (car end))))))))))

(defparameter dylan/as
  (dylan%generic-fn
   'as
   (make-param-list `((class ,<class>) (object ,<object>)) nil nil nil)
   (lambda (class obj)
     (if (dylan-call dylan/instance? obj class)
         obj
         (dylan-call dylan/error "as -- not specialized for this class type"
                     class obj)))))

(labels ((no-change (class object) (declare (ignore class)) object)
         (->exact (class object) (declare (ignore class))
           (if (realp object)
               (inexact->exact object)
               (dylan-call dylan/error "as -- not a real number" class object)))
         (->integer (class object)
           (let ((exact (->exact class object)))
             (if (integerp exact)
                 exact
                 (dylan-call dylan/error "as -- not an integral value"
                             class object))))
         (->ratio (class object)
           (let ((exact (->exact class object)))
             (if (integerp exact)
                 (dylan-call dylan/error "as -- not representable as a ratio"
                             class object)
                 exact)))
         (->float-of (format)
           (lambda (class object)
             (if (realp object)
                 (coerce object format)
                 (dylan-call dylan/error "as -- not a real number"
                             class object))))
         (->inexact (class object) (declare (ignore class))
           (exact->inexact object)))
  ;; integer <-> character
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <integer>))
                                  (object ,<character>)) nil nil nil)
               (lambda (class object) (declare (ignore class)) (char-code object))))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <character>))
                                  (object ,<integer>)) nil nil nil)
               (lambda (class object) (declare (ignore class)) (code-char object))))
  ;; number conversions
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <number>))
                                  (object ,<number>)) nil nil nil)
               #'no-change))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <complex>))
                                  (object ,<number>)) nil nil nil)
               #'no-change))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <real>))
                                  (object ,<real>)) nil nil nil)
               #'no-change))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list
                `((class ,(dylan%make-singleton <rectangular-complex>))
                  (object ,<number>)) nil nil nil)
               #'no-change))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <rational>))
                                  (object ,<number>)) nil nil nil)
               #'->exact))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <float>))
                                  (object ,<number>)) nil nil nil)
               #'->inexact))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <integer>))
                                  (object ,<number>)) nil nil nil)
               #'->integer))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <ratio>))
                                  (object ,<number>)) nil nil nil)
               #'->ratio))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <single-float>))
                                  (object ,<number>)) nil nil nil)
               (->float-of 'single-float)))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <double-float>))
                                  (object ,<number>)) nil nil nil)
               (->float-of 'double-float)))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <extended-float>))
                                  (object ,<number>)) nil nil nil)
               (->float-of 'long-float)))
  ;; symbols, strings, and keywords
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <string>))
                                  (symbol ,<symbol>)) nil nil nil)
               (lambda (class symbol)
                 (declare (ignore class))
                 (dylan-symbol->string symbol))))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <string>))
                                  (keyword ,<keyword>)) nil nil nil)
               (lambda (class keyword)
                 (declare (ignore class))
                 ;; A Dylan keyword is a Common Lisp keyword here, so
                 ;; unlike the Scheme there is no trailing colon to strip.
                 (dylan-symbol->string keyword))))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <symbol>))
                                  (string ,<string>)) nil nil nil)
               (lambda (class string)
                 (declare (ignore class))
                 (remember-spelling
                  (dylan-user-symbol (canonicalize-string-for-symbol string))
                  (copy-seq string)))))
  (add-method dylan/as
              (dylan%function->method
               (make-param-list `((class ,(dylan%make-singleton <keyword>))
                                  (string ,<string>)) nil nil nil)
               (lambda (class string)
                 (declare (ignore class))
                 (remember-spelling
                  (intern (canonicalize-string-for-symbol string) :keyword)
                  (copy-seq string))))))

(defparameter dylan/complement
  (dylan%function->method
   one-function
   (lambda (fn)
     (make-dylan-callable
      (lambda (&rest args) (not (dylan-apply fn args)))))))

(defparameter dylan/compose
  (dylan%function->method
   at-least-one-function
   (lambda (fn &rest rest-fns)
     (if (null rest-fns)
         fn
         (lambda (multiple-values next-method &rest args)
           (declare (ignore next-method))
           (labels ((compose (fn rest-fns)
                      (if (null rest-fns)
                          (dylan-apply fn args)
                          (dylan-call fn (compose (car rest-fns)
                                                  (cdr rest-fns))))))
             (dylan-mv-call fn multiple-values
                            (compose (car rest-fns) (cdr rest-fns)))))))))

(defparameter dylan/disjoin
  (dylan%function->method
   at-least-one-function
   (lambda (fn &rest rest-fns)
     (if (null rest-fns)
         fn
         (lambda (multiple-values next-method &rest args)
           (declare (ignore next-method))
           (loop with current = fn
                 with remaining = rest-fns
                 do (if (null remaining)
                        (return (dylan-mv-apply current multiple-values args))
                        (let ((value (dylan-apply current args)))
                          (if value
                              (return value)
                              (progn (setf current (car remaining))
                                     (pop remaining)))))))))))

(defparameter dylan/conjoin
  (dylan%function->method
   at-least-one-function
   (lambda (fn &rest rest-fns)
     (if (null rest-fns)
         fn
         (lambda (multiple-values next-method &rest args)
           (declare (ignore next-method))
           (loop with current = fn
                 with remaining = rest-fns
                 do (if (null remaining)
                        (return (dylan-mv-apply current multiple-values args))
                        (if (dylan-apply current args)
                            (progn (setf current (car remaining))
                                   (pop remaining))
                            (return nil)))))))))

(defparameter dylan/curry
  (dylan%function->method
   function-and-arguments
   (lambda (fn &rest curried-args)
     (lambda (multiple-values next-method &rest args)
       (declare (ignore next-method))
       (dylan-mv-apply fn multiple-values (append curried-args args))))))

(defparameter dylan/rcurry
  (dylan%function->method
   function-and-arguments
   (lambda (fn &rest curried-args)
     (lambda (multiple-values next-method &rest args)
       (declare (ignore next-method))
       (dylan-mv-apply fn multiple-values (append args curried-args))))))
