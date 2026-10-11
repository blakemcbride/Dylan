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
;;;; Miscellaneous Dylan Functions
;;;;                               (was Thomas's src/runtime-functions.scm)

(in-package #:dylan)

;;;
;;; Specialized MAKE for some built in classes
;;;

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <generic-function>)))
                   nil nil '(:debug-name :required :rest? :key?))
  (lambda (class &rest rest)
    (let ((name (dylan%find-keyword rest :debug-name
                                    (lambda () "Anonymous Generic Function")))
          (nrequired
            (dylan%find-keyword
             rest :required
             (lambda ()
               (dylan-call dylan/error
                           "(make (singleton <generic-function>)) -- required: not supplied"
                           class rest))))
          (rest? (dylan%find-keyword rest :rest? (lambda () nil)))
          (keys (dylan%find-keyword rest :key? (lambda () nil))))
      (dylan%create-generic-function name nrequired keys rest?)))))

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <singleton>)))
                   nil nil '(:object))
  (lambda (class &rest rest)
    (let ((object (dylan%find-keyword
                   rest :object
                   (lambda ()
                     (dylan-call dylan/error
                                 "(make (singleton <singleton>)) -- object not supplied"
                                 class rest)))))
      (dylan%make-singleton object)))))

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <complex>)))
                   nil nil '(:real :imag :magnitude :angle))
  (lambda (class &rest rest)
    (let ((real (dylan%find-keyword rest :real (lambda () nil)))
          (imag (dylan%find-keyword rest :imag (lambda () nil)))
          (mag (dylan%find-keyword rest :magnitude (lambda () nil)))
          (angle (dylan%find-keyword rest :angle (lambda () nil))))
      (cond ((and (not real) (not imag) (not mag) (not angle))
             (dylan-call dylan/make-rectangular 0 0))
            ((and real (not imag) (not mag) (not angle))
             (dylan-call dylan/make-rectangular real 0))
            ((and (not real) imag (not mag) (not angle))
             (dylan-call dylan/make-rectangular 0 imag))
            ((and real imag (not mag) (not angle))
             (dylan-call dylan/make-rectangular real imag))
            ((and (not real) (not imag) mag (not angle))
             (dylan-call dylan/make-polar mag 0))
            ((and (not real) (not imag) (not mag) angle)
             (dylan-call dylan/make-polar 0 angle))
            ((and (not real) (not imag) mag angle)
             (dylan-call dylan/make-polar mag angle))
            (t (dylan-call dylan/error
                           "(make (singleton <complex>)) -- invalid keyword combination"
                           class rest)))))))

;;;
;;; Misc Generic Functions
;;;

(defparameter dylan/find-method
  (dylan%generic-fn 'find-method
                    (make-param-list `((generic-function ,<generic-function>)
                                       (specializers ,<list>))
                                     nil nil nil)
                    #'find-method))

(defparameter dylan/generic-function-methods
  (dylan%generic-fn 'generic-function-methods
                    (make-param-list `((generic-function ,<generic-function>))
                                     nil nil nil)
                    #'generic-function.methods))

;;; The hyperbolic functions are Common Lisp's (chapter 14), with exact
;;; arguments made double-floats as for the others in runtime-methods.lisp.

(defparameter dylan/acosh
  (dylan%generic-fn 'acosh one-number (inexact-function #'acosh)))
(defparameter dylan/asinh
  (dylan%generic-fn 'asinh one-number (inexact-function #'asinh)))
(defparameter dylan/atanh
  (dylan%generic-fn 'atanh one-number (inexact-function #'atanh)))
(defparameter dylan/cosh
  (dylan%generic-fn 'cosh one-number (inexact-function #'cosh)))
(defparameter dylan/sinh
  (dylan%generic-fn 'sinh one-number (inexact-function #'sinh)))
(defparameter dylan/tanh
  (dylan%generic-fn 'tanh one-number (inexact-function #'tanh)))

(defparameter dylan/add-slot
  (dylan%generic-fn 'add-slot
                    (make-param-list `((slot-owner ,<object>))
                                     nil nil
                                     '(:getter :setter :type :init-value
                                       :init-function :init-keyword
                                       :required-init-keyword :debug-name
                                       :allocation))
                    #'add-slot))

(defparameter dylan/freeze-methods
  (dylan%generic-fn 'freeze-methods
                    (make-param-list `((generic-function ,<generic-function>))
                                     nil t nil)
                    (lambda (generic-function &rest specializers)
                      (declare (ignore specializers)) ; Ignored for now
                      generic-function)))

(defparameter dylan/function-arguments
  (dylan%generic-fn 'function-arguments
                    (make-param-list `((fn ,<function>)) nil nil nil)
                    nil))

(progn
  (add-method
   dylan/function-arguments
   (dylan%dylan-callable->method
    (make-param-list `((method ,<method>)) nil nil nil)
    (lambda (multiple-values next-method method)
      (dylan-full-call dylan/values multiple-values next-method
                       (method.nrequired method)
                       (method.rest? method)
                       (method.keys method)))))
  (add-method
   dylan/function-arguments
   (dylan%dylan-callable->method
    (make-param-list `((generic-function ,<generic-function>)) nil nil nil)
    (lambda (multiple-values next-method generic-function)
      (dylan-full-call dylan/values multiple-values next-method
                       (generic-function.nrequired generic-function)
                       (generic-function.rest? generic-function)
                       (generic-function.keys generic-function))))))

(defparameter dylan/make-polar
  (dylan%generic-fn 'make-polar two-reals
                    (lambda (magnitude angle)
                      (if (eql angle 0)
                          magnitude
                          (let ((m (->inexact-argument magnitude))
                                (a (->inexact-argument angle)))
                            (complex (* m (cos a)) (* m (sin a))))))))

(defparameter dylan/make-rectangular
  (dylan%generic-fn 'make-rectangular two-reals
                    ;; An exact zero imaginary part gives a real; an
                    ;; inexact one is kept, and makes the result inexact.
                    (lambda (real imaginary)
                      (if (eql imaginary 0)
                          real
                          (complex real imaginary)))))

(defparameter dylan/method-specializers
  (dylan%generic-fn 'method-specializers
                    (make-param-list `((method ,<method>)) nil nil nil)
                    #'method.specializers))

(defparameter dylan/singleton
  (dylan%generic-fn 'singleton one-object #'dylan%make-singleton))

(defparameter dylan/sorted-applicable-methods
  (dylan%generic-fn 'sorted-applicable-methods
                    (make-param-list `((generic-function ,<generic-function>))
                                     nil t nil)
                    (lambda (generic-function &rest args)
                      (sorted-applicable-methods
                       (generic-function.methods generic-function)
                       args))))

(defparameter dylan/remove-method
  (dylan%generic-fn
   'remove-method
   (make-param-list `((generic-function ,<generic-function>)
                      (method ,<method>))
                    nil nil nil)
   (lambda (generic-function method)
     (when (generic-function.read-only? generic-function)
       (dylan-call dylan/error
                   "remove-method -- generic function is read-only"
                   generic-function method))
     (delete-method! generic-function method))))

(defparameter dylan/slot-descriptor
  ;; I'm ignoring the problem of making singletons of Lisp-native immutable
  ;; objects, and adding slots to the singleton.
  (dylan%generic-fn
   'slot-descriptor
   (make-param-list `((instance ,<object>) (getter ,<generic-function>))
                    nil nil nil)
   (lambda (obj getter)
     (or (and (instance? obj)
              (instance.singleton obj)
              (same-slot-getter-in-slot-vector->slot
               getter
               (singleton.extra-slot-descriptors (instance.singleton obj))))
         (same-slot-getter-in-slot-vector->slot
          getter (class.slots (get-type obj)))))))

(defparameter dylan/slot-value
  (dylan%generic-fn
   'slot-value
   (make-param-list `((instance ,<object>) (slot-desc ,<slot-descriptor>))
                    nil nil nil)
   (lambda (instance slot-desc)
     (let ((allocation (slot.allocation slot-desc))
           (loc (slot.data-location slot-desc)))
       ;; The Scheme applied VECTOR-REF straight to the instance and to the
       ;; class, which worked only because MIT Scheme records were vectors;
       ;; the data vectors are what was meant.
       (case allocation
         ((instance) (aref (instance.data instance) loc))
         ((class) (aref (class.class-data (car loc)) (cdr loc)))
         ((each-subclass) (aref (class.class-data (get-type instance)) loc))
         ((virtual) (dylan-call (slot.getter slot-desc) instance))
         ((constant) loc)
         (t (dylan-call dylan/error "slot-value -- internal error"
                        slot-desc allocation)))))))

(defparameter dylan/setter/slot-value/
  (dylan%generic-fn
   'slot-value
   (make-param-list `((instance ,<object>) (slot-desc ,<slot-descriptor>)
                      (new-value ,<object>))
                    nil nil nil)
   (lambda (instance slot-desc new-value)
     (unless (match-specializer? new-value (slot.type slot-desc))
       (dylan-call dylan/error "(setter slot-value) -- type conflict"
                   instance (slot.type slot-desc) slot-desc new-value))
     (let ((allocation (slot.allocation slot-desc))
           (loc (slot.data-location slot-desc)))
       (case allocation
         ((instance) (setf (aref (instance.data instance) loc) new-value))
         ((class)
          (setf (aref (class.class-data (car loc)) (cdr loc)) new-value))
         ((each-subclass)
          (setf (aref (class.class-data (get-type instance)) loc) new-value))
         ((virtual)
          (let ((setter (slot.setter slot-desc)))
            (unless setter
              (dylan-call dylan/error
                          "(setter slot-value) -- no setter for virtual slot"
                          instance slot-desc new-value))
            (dylan-call setter instance new-value)))
         ((constant)
          (dylan-call dylan/error
                      "(setter slot-value) -- can't set a constant slot"
                      instance slot-desc new-value))
         (t (dylan-call dylan/error
                        "(setter slot-value) -- internal error"
                        slot-desc allocation)))))))

(defparameter dylan/make-read-only
  (dylan%generic-fn 'make-read-only one-class
                    (lambda (class)
                      (setf (class.read-only? class) t)
                      class)))

(add-method
 dylan/make-read-only
 (dylan%function->method
  (make-param-list `((generic-function ,<generic-function>)) nil nil nil)
  (lambda (generic-function)
    (set-generic-function.read-only?! generic-function t))))

(defparameter dylan/seal
  (dylan%generic-fn 'seal one-class
                    ;; Sealing also makes the class read-only (chapter 11).
                    (lambda (class)
                      (setf (class.sealed? class) t)
                      (setf (class.read-only? class) t)
                      class)))

(defparameter dylan/remove-slot nil)

;;;;;;;;;;;; CRL additions

(defparameter dylan/display (make-dylan-callable #'dylan-display 1))
(defparameter dylan/newline (make-dylan-callable #'dylan-newline 0))
(defparameter dylan/write-line (make-dylan-callable #'dylan-write-line 1))
(defparameter dylan/print (make-dylan-callable #'dylan-write-line 1))
