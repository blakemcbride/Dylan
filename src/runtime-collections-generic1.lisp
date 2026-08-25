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
;;;; This file contains the implementation of most of the generic functions
;;;; and methods that deal with collections and their subtypes.  The
;;;; organization follows the Dylan manual.  The specializations of methods
;;;; based on collection type are in the file
;;;; runtime-collections-xxx.lisp (where xxx is a collection type).
;;;;                      (was Thomas's src/runtime-collections-generic1.scm)

(in-package #:dylan)

;;;;
;;;; Misc. functions
;;;;

(add-method
 dylan/binary=
 (dylan%function->method
  two-collections
  (lambda (coll-1 coll-2)
    (let ((key-sequence-1 (dylan-call dylan/key-sequence coll-1))
          (key-sequence-2 (dylan-call dylan/key-sequence coll-2))
          (size-1 (dylan-call dylan/size coll-1))
          (size-2 (dylan-call dylan/size coll-2)))
      (cond ((not (and size-1 size-2)) nil)
            ;; if either one is unbound, then false, since specific methods
            ;; will handle infinite ranges and circular lists
            ((not (= size-1 size-2)) nil)
            (t
             (loop with state = (dylan-call dylan/initial-state key-sequence-1)
                   do (if (not state)
                          (return t)
                          (let ((key (dylan-call dylan/current-element
                                                 key-sequence-1 state)))
                            (if (and (dylan-call dylan/member? key key-sequence-2
                                                 :test
                                                 (make-dylan-callable
                                                  (lambda (a b)
                                                    (dylan-call dylan/= a b))))
                                     (dylan-call
                                      dylan/=
                                      (dylan-call dylan/element coll-1 key)
                                      (dylan-call dylan/element coll-2 key)))
                                (setf state (dylan-call dylan/next-state
                                                        key-sequence-1 state))
                                (return nil)))))))))))

(add-method
 dylan/binary=
 (dylan%function->method
  two-sequences
  (lambda (seq-1 seq-2)
    (let ((size-1 (dylan-call dylan/size seq-1))
          (size-2 (dylan-call dylan/size seq-2)))
      (cond ((not (and size-1 size-2)) nil)
            ;; if either one is unbound, then false, since specific methods
            ;; will handle infinite ranges and circular lists
            ((not (= size-1 size-2)) nil)
            (t
             (do ((state-1 (dylan-call dylan/initial-state seq-1)
                           (dylan-call dylan/next-state seq-1 state-1))
                  (state-2 (dylan-call dylan/initial-state seq-2)
                           (dylan-call dylan/next-state seq-2 state-2)))
                 ((or (or (not state-1) (not state-2))
                      (not (dylan-call dylan/id?
                                       (dylan-call dylan/current-element
                                                   seq-1 state-1)
                                       (dylan-call dylan/current-element
                                                   seq-2 state-2))))
                  (if (or state-1 state-2) nil t)))))))))

(add-method
 dylan/shallow-copy
 (dylan%function->method
  (make-param-list `((collection ,<collection>)) nil nil nil)
  (lambda (coll)
    (dylan-call dylan/error
                "shallow-copy -- not specialized for this collection type"
                coll))))

(add-method
 dylan/shallow-copy
 (dylan%function->method
  (make-param-list `((sequence ,<sequence>)) nil nil nil)
  (lambda (seq) (dylan-call dylan/copy-sequence seq))))

(add-method
 dylan/as
 ;; This requires specialization of AS for classes that are not
 ;; superclasses of list (such as array, table, ...)
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <collection>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (dylan-call dylan/as <list> collection))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <explicit-key-collection>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (dylan-call dylan/as <vector> collection))))

;;;;
;;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;;

(defparameter dylan/size
  (dylan%generic-fn
   'size one-collection
   (lambda (collection)
     (let ((index 0))
       (iterate-until (lambda (elem)
                        (declare (ignore elem))
                        (incf index)
                        nil)
                      collection)
       index))))

(defparameter dylan/class-for-copy
  (dylan%generic-fn 'class-for-copy one-object
                    (lambda (x) (dylan-call dylan/object-class x))))

(defparameter dylan/empty?
  (dylan%generic-fn
   'empty?
   (make-param-list `((collection ,<collection>)) nil nil nil)
   (lambda (collection)
     (if (dylan-call dylan/initial-state collection) nil t))))

(defparameter dylan/do
  (dylan%generic-fn
   'do
   (make-param-list `((function ,<function>) (collection ,<collection>))
                    nil t nil)
   (lambda (fn &rest collections)
     (collections-iterate fn
                          (lambda (val) (declare (ignore val)) nil)
                          nil
                          collections))))

(defparameter dylan/map
  (dylan%generic-fn
   'map procedure-and-at-least-one-collection
   (lambda (proc li &rest rest)
     (let ((all-lists (cons li rest)))
       (loop with result = '()
             with next-states = (mapcar (lambda (li)
                                          (dylan-call dylan/initial-state li))
                                        all-lists)
             do (if (any? #'not next-states)
                    (return (dylan-call dylan/as
                                        (dylan-call dylan/class-for-copy li)
                                        (reverse result)))
                    (let ((next-value
                            (dylan-call dylan/apply
                                        proc
                                        (mapcar (lambda (li st)
                                                  (dylan-call
                                                   dylan/current-element li st))
                                                all-lists next-states))))
                      (push next-value result)
                      (setf next-states
                            (mapcar (lambda (li st)
                                      (dylan-call dylan/next-state li st))
                                    all-lists next-states)))))))))

(defparameter dylan/map-as
  (dylan%generic-fn
   'map-as
   (make-param-list `((class ,<class>) (proc ,<function>)
                      (collection ,<collection>))
                    nil t nil)
   (lambda (class proc collection &rest rest)
     (let ((result (dylan-call dylan/apply dylan/map
                               (cons proc (cons collection rest)))))
       (dylan-call dylan/as class result)))))

(defparameter dylan/map-into
  (dylan%generic-fn
   'map-into
   (make-param-list `((mutable-collection ,<mutable-collection>)
                      (procedure ,<function>)
                      (collection ,<collection>))
                    nil t nil)
   (lambda (mut-coll proc coll-1 &rest rest)
     (let ((current-state (dylan-call dylan/initial-state mut-coll)))
       (collections-iterate
        proc
        (lambda (new-value)
          (dylan-call dylan/setter/current-element/ mut-coll
                      current-state new-value)
          (setf current-state
                (dylan-call dylan/next-state mut-coll current-state))
          nil)
        mut-coll
        (list* mut-coll coll-1 rest))))))

(defparameter dylan/any?
  (dylan%generic-fn
   'any?
   (make-param-list `((procedure ,<function>) (collection ,<collection>))
                    nil t nil)
   (lambda (fn &rest collections)
     (collections-iterate fn
                          (lambda (val) (if val (lambda () val) nil))
                          nil
                          collections))))

(defparameter dylan/every?
  (dylan%generic-fn
   'every?
   (make-param-list `((procedure ,<function>) (collection ,<collection>))
                    nil t nil)
   (lambda (fn &rest collections)
     (collections-iterate fn
                          (lambda (val) (if val nil (lambda () nil)))
                          t
                          collections))))

(defparameter dylan/reduce
  (dylan%generic-fn
   'reduce
   (make-param-list `((procedure ,<function>) (init-value ,<object>)
                      (collection ,<collection>))
                    nil nil nil)
   (lambda (proc init-value collection)
     (if (dylan-call dylan/empty? collection)
         init-value
         (loop with cur-value = init-value
               with state = (dylan-call dylan/initial-state collection)
               do (if state
                      (let ((next-value
                              (dylan-call proc cur-value
                                          (dylan-call dylan/current-element
                                                      collection state))))
                        (setf state (dylan-call dylan/next-state
                                                collection state))
                        (setf cur-value next-value))
                      (return cur-value)))))))

(defparameter dylan/reduce1
  (dylan%generic-fn
   'reduce1
   (make-param-list `((procedure ,<function>) (collection ,<collection>))
                    nil nil nil)
   (lambda (proc collection)
     (if (dylan-call dylan/empty? collection)
         (dylan-call dylan/error "reduce1 -- collection argument is empty"
                     proc collection)
         (loop with cur-value = (dylan-call dylan/current-element collection
                                            (dylan-call dylan/initial-state
                                                        collection))
               with state = (dylan-call dylan/next-state collection
                                        (dylan-call dylan/initial-state
                                                    collection))
               do (if state
                      (let ((next-value
                              (dylan-call proc cur-value
                                          (dylan-call dylan/current-element
                                                      collection state))))
                        (setf state (dylan-call dylan/next-state
                                                collection state))
                        (setf cur-value next-value))
                      (return cur-value)))))))

(defparameter dylan/member?
  (dylan%generic-fn
   'member?
   (make-param-list `((value ,<object>) (collection ,<collection>))
                    nil nil '(:test))
   nil))

(add-method
 dylan/member?
 (dylan%dylan-callable->method
  (make-param-list `((value ,<object>) (collection ,<collection>))
                   nil nil '(:test))
  (lambda (multiple-values next-method value collection &rest keys)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method keys '(:test))
    (let ((test (dylan%find-keyword keys :test (lambda () dylan/id?))))
      (call-with-escape-continuation
       (lambda (return-value)
         (iterate! (lambda (obj)
                     (let ((test-value (dylan-call test value obj)))
                       (when test-value (funcall return-value test-value))))
                   collection)
         (funcall return-value nil)))))))

(defparameter dylan/find-key
  (dylan%generic-fn
   'find-key
   (make-param-list `((collection ,<collection>) (predicate ,<function>))
                    nil nil '(:skip :failure))
   nil))

(add-method
 dylan/find-key
 (dylan%dylan-callable->method
  (make-param-list `((collection ,<collection>) (predicate ,<function>))
                   nil nil '(:skip :failure))
  (lambda (multiple-values next-method collection predicate &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:skip :failure))
    (let ((skip (dylan%find-keyword rest :skip (lambda () 0)))
          (failure (dylan%find-keyword rest :failure (lambda () nil)))
          (key-sequence (dylan-call dylan/key-sequence collection)))
      (loop with key-state = (dylan-call dylan/initial-state key-sequence)
            with satisfied = 0
            do (cond ((not key-state) (return failure))
                     ((dylan-call predicate
                                  (dylan-call dylan/element
                                              collection
                                              (dylan-call dylan/current-element
                                                          key-sequence
                                                          key-state)))
                      (if (> satisfied (- skip 1))
                          (return (dylan-call dylan/current-element
                                              key-sequence key-state))
                          (progn
                            (setf key-state (dylan-call dylan/next-state
                                                        key-sequence key-state))
                            (incf satisfied))))
                     (t (setf key-state (dylan-call dylan/next-state
                                                    key-sequence
                                                    key-state)))))))))

(defparameter dylan/replace-elements!
  (dylan%generic-fn
   'replace-elements!
   (make-param-list `((collection ,<mutable-collection>)
                      (predicate ,<function>)
                      (new-val-fn ,<function>))
                    nil nil '(:count))
   nil))

(add-method
 dylan/replace-elements!
 (dylan%dylan-callable->method
  (make-param-list `((collection ,<mutable-collection>)
                     (predicate ,<function>)
                     (new-val-fn ,<function>))
                   nil nil '(:count))
  (lambda (multiple-values next-method coll predicate new-val-fn &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:count))
    (let* ((size (dylan-call dylan/size coll))
           (count (dylan%find-keyword rest :count (lambda () size))))
      (loop with state = (dylan-call dylan/initial-state coll)
            with num-changed = 0
            do (cond ((or (>= num-changed count) (not state)) (return coll))
                     ((dylan-call predicate
                                  (dylan-call dylan/current-element coll state))
                      (dylan-call dylan/setter/current-element/
                                  coll state
                                  (dylan-call new-val-fn
                                              (dylan-call dylan/current-element
                                                          coll state)))
                      (setf state (dylan-call dylan/next-state coll state))
                      (incf num-changed))
                     (t (setf state (dylan-call dylan/next-state
                                                coll state)))))))))

(defparameter dylan/fill!
  (dylan%generic-fn
   'fill!
   (make-param-list `((mutable-collection ,<mutable-collection>)
                      (value ,<object>))
                    nil nil '(:start :end))
   nil))

(add-method
 dylan/fill!
 (dylan%dylan-callable->method
  (make-param-list `((mutable-collection ,<mutable-collection>)
                     (value ,<object>))
                   nil nil '(:start :end))
  (lambda (multiple-values next-method collection new-value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:start :end))
    (let ((start (dylan%find-keyword rest :start (lambda () 0)))
          (end (dylan%find-keyword
                rest :end (lambda () (dylan-call dylan/size collection)))))
      (do ((state (dylan-call dylan/initial-state collection)
                  (dylan-call dylan/next-state collection state))
           (index 0 (+ index 1)))
          ((or (not state) (>= index end)) collection)
        (when (and state (>= index start))
          (dylan-call dylan/setter/current-element/
                      collection state new-value)))))))
