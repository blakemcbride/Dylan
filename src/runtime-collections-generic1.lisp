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
          (size-1 (collection-size coll-1))
          (size-2 (collection-size coll-2)))
      (bool
       (cond ((not (and size-1 size-2)) nil)
             ;; if either one is unbound, then false, since specific methods
             ;; will handle infinite ranges and circular lists
             ((not (= size-1 size-2)) nil)
             (t
              (loop with state = (initial-state key-sequence-1)
                    do (if (not state)
                           (return t)
                           (let ((key (current-element
                                                  key-sequence-1 state)))
                             (if (and (true?
                                       (dylan-call dylan/member? key key-sequence-2
                                                   :test
                                                   (make-dylan-callable
                                                    (lambda (a b)
                                                      (dylan-call dylan/= a b)))))
                                      (true?
                                       (dylan-call
                                        dylan/=
                                        (dylan-call dylan/element coll-1 key)
                                        (dylan-call dylan/element coll-2 key))))
                                 (setf state (next-state key-sequence-1 state))
                                 (return nil))))))))))))

(add-method
 dylan/binary=
 (dylan%function->method
  two-sequences
  (lambda (seq-1 seq-2)
    (let ((size-1 (collection-size seq-1))
          (size-2 (collection-size seq-2)))
      (bool
       (cond ((not (and size-1 size-2)) nil)
             ;; if either one is unbound, then false, since specific methods
             ;; will handle infinite ranges and circular lists
             ((not (= size-1 size-2)) nil)
             (t
              (do ((state-1 (initial-state seq-1)
                            (next-state seq-1 state-1))
                   (state-2 (initial-state seq-2)
                            (next-state seq-2 state-2)))
                  ;; Elements are compared with = (chapter 9), not id?.
                  ((or (or (not state-1) (not state-2))
                       (false? (dylan-call dylan/=
                                           (current-element
                                                       seq-1 state-1)
                                           (current-element
                                                       seq-2 state-2))))
                   (if (or state-1 state-2) nil t))))))))))

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
     (bool (not (initial-state collection))))))

(defparameter dylan/do
  (dylan%generic-fn
   'do
   (make-param-list `((function ,<function>) (collection ,<collection>))
                    nil t nil)
   (lambda (fn &rest collections)
     (collections-iterate fn
                          (lambda (val) (declare (ignore val)) nil)
                          dylan%false
                          collections))))

(defun sequence? (collection)
  (subclass? (get-type collection) <sequence>))

(defun aligned-keys (collections)
  "Chapter 12's collection alignment: the keys common to COLLECTIONS."
  (let ((test (make-dylan-callable (lambda (a b) (dylan-call dylan/= a b)))))
    (cl:reduce (lambda (keys collection)
                 (dylan-call dylan/intersection keys
                             (dylan-call dylan/key-sequence collection)
                             :test test))
               (cdr collections)
               :initial-value (dylan-call dylan/key-sequence (car collections)))))

(defun map-aligned-by-keys (proc first collections)
  "MAP over collections that are not all sequences: apply PROC to the
elements at each common key, and store the results under those keys in a
new instance of FIRST's class-for-copy."
  (let ((keys (iterate->list #'identity (aligned-keys collections)))
        (class (dylan-call dylan/class-for-copy first)))
    (when (sequence? first)
      (setf keys (sort keys #'<)))
    (flet ((value-at (key)
             (dylan-call dylan/apply proc
                         (mapcar (lambda (c) (dylan-call dylan/element c key))
                                 collections))))
      (if (subclass? class <sequence>)
          (dylan-call dylan/as class (mapcar #'value-at keys))
          (let ((result (dylan-call dylan/make class)))
            (dolist (key keys result)
              (dylan-call dylan/setter/element/ result key (value-at key))))))))

(defparameter dylan/map
  (dylan%generic-fn
   'map procedure-and-at-least-one-collection
   (lambda (proc li &rest rest)
     (let ((all-lists (cons li rest)))
       (if (not (every #'sequence? all-lists))
           (map-aligned-by-keys proc li all-lists)
           (loop with result = '()
                 with next-states = (mapcar (lambda (li)
                                              (initial-state li))
                                            all-lists)
                 do (if (any? #'not next-states)
                        (return (dylan-call dylan/as
                                            (dylan-call dylan/class-for-copy li)
                                            (reverse result)))
                        (let ((next-value
                                (dylan-call dylan/apply
                                            proc
                                            (mapcar (lambda (li st)
                                                      (current-element
                                                       li st))
                                                    all-lists next-states))))
                          (push next-value result)
                          (setf next-states
                                (mapcar (lambda (li st)
                                          (next-state li st))
                                        all-lists next-states))))))))))

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
   ;; Chapter 12: the procedure is applied to corresponding elements of
   ;; the source collections (the collection and more-collections), and
   ;; the results replace the elements of the target.  (The manual's one
   ;; example, (map-into x + '(1 2 3 4)), shows the target's elements
   ;; passed as well; its prose, its signature and design note 27 do not.)
   ;; Sequences are processed in step, stopping when the target or any
   ;; source runs out; otherwise the target and the sources are aligned by
   ;; key (chapter 12, "Collection Alignment").  Tables have a method of
   ;; their own, in which the target takes no part in the alignment.
   (lambda (mut-coll proc coll-1 &rest rest)
     (let ((sources (list* coll-1 rest)))
       (if (and (sequence? mut-coll) (every #'sequence? sources))
           (loop with target-state = (initial-state mut-coll)
                 with states = (mapcar (lambda (c)
                                         (initial-state c))
                                       sources)
                 until (or (not target-state) (some #'not states))
                 do (set-current-element mut-coll
                                target-state
                                (dylan-call dylan/apply proc
                                            (mapcar (lambda (c st)
                                                      (current-element
                                                       c st))
                                                    sources states)))
                    (setf target-state
                          (next-state mut-coll target-state)
                          states
                          (mapcar (lambda (c st)
                                    (next-state c st))
                                  sources states))
                 finally (return mut-coll))
           (let ((keys (aligned-keys (cons mut-coll sources))))
             (iterate-until
              (lambda (key)
                (dylan-call dylan/setter/element/ mut-coll key
                            (dylan-call dylan/apply proc
                                        (mapcar (lambda (c)
                                                  (dylan-call dylan/element
                                                              c key))
                                                sources)))
                nil)
              keys)
             mut-coll))))))

(defparameter dylan/any?
  (dylan%generic-fn
   'any?
   (make-param-list `((procedure ,<function>) (collection ,<collection>))
                    nil t nil)
   (lambda (fn &rest collections)
     (collections-iterate fn
                          (lambda (val) (if (true? val) (lambda () val) nil))
                          dylan%false
                          collections))))

(defparameter dylan/every?
  (dylan%generic-fn
   'every?
   (make-param-list `((procedure ,<function>) (collection ,<collection>))
                    nil t nil)
   (lambda (fn &rest collections)
     (collections-iterate fn
                          (lambda (val)
                            (if (true? val) nil (lambda () dylan%false)))
                          t
                          collections))))

(defparameter dylan/reduce
  (dylan%generic-fn
   'reduce
   (make-param-list `((procedure ,<function>) (init-value ,<object>)
                      (collection ,<collection>))
                    nil nil nil)
   (lambda (proc init-value collection)
     (if (true? (dylan-call dylan/empty? collection))
         init-value
         (loop with cur-value = init-value
               with state = (initial-state collection)
               do (if state
                      (let ((next-value
                              (dylan-call proc cur-value
                                          (current-element
                                                      collection state))))
                        (setf state (next-state
                                                collection state))
                        (setf cur-value next-value))
                      (return cur-value)))))))

(defparameter dylan/reduce1
  (dylan%generic-fn
   'reduce1
   (make-param-list `((procedure ,<function>) (collection ,<collection>))
                    nil nil nil)
   (lambda (proc collection)
     (if (true? (dylan-call dylan/empty? collection))
         (dylan-call dylan/error "reduce1 -- collection argument is empty"
                     proc collection)
         (loop with cur-value = (current-element collection
                                            (initial-state
                                                        collection))
               with state = (next-state collection
                                        (initial-state
                                                    collection))
               do (if state
                      (let ((next-value
                              (dylan-call proc cur-value
                                          (current-element
                                                      collection state))))
                        (setf state (next-state
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
                       (when (true? test-value)
                         (funcall return-value test-value))))
                   collection)
         (funcall return-value dylan%false)))))))

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
          (failure (dylan%find-keyword rest :failure (lambda () dylan%false)))
          (key-sequence (dylan-call dylan/key-sequence collection)))
      (loop with key-state = (initial-state key-sequence)
            with satisfied = 0
            do (cond ((not key-state) (return failure))
                     ((true? (dylan-call predicate
                                  (dylan-call dylan/element
                                              collection
                                              (current-element
                                                          key-sequence
                                                          key-state))))
                      (if (> satisfied (- skip 1))
                          (return (current-element
                                              key-sequence key-state))
                          (progn
                            (setf key-state (next-state
                                                        key-sequence key-state))
                            (incf satisfied))))
                     (t (setf key-state (next-state
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
    (let* ((size (collection-size coll))
           (count (dylan%find-keyword rest :count (lambda () size))))
      (loop with state = (initial-state coll)
            with num-changed = 0
            do (cond ((or (>= num-changed count) (not state)) (return coll))
                     ((true? (dylan-call predicate
                                         (current-element
                                                     coll state)))
                      (set-current-element
                                  coll state
                                  (dylan-call new-val-fn
                                              (current-element
                                                          coll state)))
                      (setf state (next-state coll state))
                      (incf num-changed))
                     (t (setf state (next-state
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
                rest :end (lambda () (collection-size collection)))))
      (do ((state (initial-state collection)
                  (next-state collection state))
           (index 0 (+ index 1)))
          ((or (not state) (>= index end)) collection)
        (when (and state (>= index start))
          (set-current-element
                      collection state new-value)))))))
