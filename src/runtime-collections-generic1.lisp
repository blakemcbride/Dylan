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
                                        (library-element coll-1 key)
                                        (library-element coll-2 key))))
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
  (make-param-list `((sequence ,<sequence>)) nil nil nil)
  (lambda (seq) (dylan-call dylan/copy-sequence seq))))

(add-method
 dylan/as
 ;; Chapter 11: an object already of the class is answered unchanged, and
 ;; every collection is a <collection>.
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <collection>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    collection)))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <explicit-key-collection>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (if (true? (dylan-call dylan/instance? collection class))
        collection
        (dylan-call dylan/as <vector> collection)))))

(add-method
 dylan/as
 ;; A sequence of any other class -- a program's own <mutable-sequence> --
 ;; is made by make with size:, and its elements set in their natural
 ;; order, as the compiled library does.  Chapter 12: the default sequence
 ;; functions answer an instance of the class-for-copy, which comes here.
 (dylan%function->method
  (make-param-list `((class ,<class>) (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (cond ((true? (dylan-call dylan/instance? collection class)) collection)
          ((subclass? class <mutable-sequence>)
           (let* ((elements (iterate->list #'identity collection))
                  (result (dylan-call dylan/make class :size (length elements)))
                  (state (initial-state result)))
             (loop while (and state elements)
                   do (set-current-element result state (pop elements))
                      (setf state (next-state result state)))
             (when elements             ; It made too few places: <empty-list>
               (dylan-call dylan/error "as: ~s cannot be converted to ~s"
                           collection class))
             result))
          (t (dylan-call dylan/error "as: ~s cannot be converted to ~s"
                         collection class))))))

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
  "Chapter 12's collection alignment: the keys common to COLLECTIONS.  A
bounded key sequence goes first, so that the intersection, which walks its
first argument, ends."
  (let ((key-sequences (stable-sort (mapcar (lambda (collection)
                                              (dylan-call dylan/key-sequence
                                                          collection))
                                            collections)
                                    (lambda (a b)
                                      (and (collection-size a)
                                           (not (collection-size b)))))))
    (cl:reduce (lambda (keys key-sequence)
                 (dylan-call dylan/intersection keys key-sequence
                             :test dylan/=))
               (cdr key-sequences)
               :initial-value (car key-sequences))))

(defun map-aligned-by-keys (proc first collections
                            &optional (class (dylan-call dylan/class-for-copy first)))
  "MAP over collections that are not all sequences: apply PROC to the
elements at each common key, and store the results under those keys in a
new instance of CLASS, by default FIRST's class-for-copy."
  (let ((keys (iterate->list #'identity (aligned-keys collections))))
    (when (sequence? first)
      (setf keys (sort keys #'<)))
    (flet ((value-at (key)
             (dylan-call dylan/apply proc
                         (mapcar (lambda (c) (library-element c key))
                                 collections))))
      (if (subclass? class <sequence>)
          (dylan-call dylan/as class (mapcar #'value-at keys))
          (let ((result
                  ;; An array is made with the first array's dimensions.
                  (if (subclass? class <array>)
                      (dylan-call dylan/make
                                  (if (eq class <%general-array>) <array> class)
                                  :dimensions (dylan-call dylan/dimensions
                                                          first))
                      (dylan-call dylan/make class))))
            (dolist (key keys result)
              (library-element-setter result key (value-at key))))))))

(defun map-to-class (class proc li rest)
  "The results of PROC on the elements of LI and the REST of the
collections, in a new instance of CLASS (by default LI's class-for-copy)."
     (let ((all-lists (cons li rest))
           (class (or class (dylan-call dylan/class-for-copy li))))
       ;; Sequences into a sequence in their natural order; anything else
       ;; by key, into a collection made by make (chapter 12).
       (if (or (not (every #'sequence? all-lists))
               (not (subclass? class <sequence>)))
           (map-aligned-by-keys proc li all-lists class)
           (loop with result = '()
                 with next-states = (mapcar (lambda (li)
                                              (initial-state li))
                                            all-lists)
                 do (if (any? #'not next-states)
                        (return (sequence-of-class class (reverse result)))
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
                                        all-lists next-states))))))))

(defparameter dylan/map
  (dylan%generic-fn
   'map procedure-and-at-least-one-collection
   (lambda (proc li &rest rest)
     (map-to-class nil proc li rest))))

(defparameter dylan/map-as
  (dylan%generic-fn
   'map-as
   (make-param-list `((class ,<class>) (proc ,<function>)
                      (collection ,<collection>))
                    nil t nil)
   (lambda (class proc collection &rest rest)
     ;; Chapter 12: CLASS must be a subclass of <mutable-collection>
     (unless (subclass? class <mutable-collection>)
       (dylan-call dylan/error "map-as: the class is not a mutable collection: ~s"
                   class))
     (map-to-class class proc collection rest))))

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
                (library-element-setter mut-coll key
                            (dylan-call dylan/apply proc
                                        (mapcar (lambda (c)
                                                  (library-element
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
    ;; In the order of iteration, each element once (as the compiler to C
    ;; does it): the index of a sequence's, else the current key.
    (let ((skip (dylan%find-keyword rest :skip (lambda () 0)))
          (failure (dylan%find-keyword rest :failure (lambda () dylan%false)))
          (sequence? (sequence? collection)))
      (do ((state (initial-state collection) (next-state collection state))
           (index 0 (+ index 1)))
          ((not state) failure)
        (when (true? (dylan-call predicate (current-element collection state)))
          ;; A skip below 1 skips none
          (if (not (< 0 skip))
              (return (if sequence?
                          index
                          (current-key collection state)))
              (decf skip))))))))

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
    ;; No count (or #f) replaces every match; a count below 1, none.
    (let ((count (false->nil (dylan%find-keyword rest :count (lambda () nil)))))
      (loop with state = (initial-state coll)
            with num-changed = 0
            do (cond ((or (and count (>= num-changed count)) (not state)) (return coll))
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

;;; START and END must be indexes of the sequence S, END exclusive and not
;;; before START, as in a compiled program; MESSAGE reports them if not.
;;; END is #f when not given.  Answers the end to stop at.
(defun check-start-end (message s start end)
  (let ((n (collection-size s))
        (end (if (false? end) nil end)))
    (when (or (not (integerp start)) (minusp start)
              (and n (> start n))
              (and end (or (not (integerp end)) (< end start)
                           (and n (> end n)))))
      (dylan-call dylan/error message s start (or end dylan%false)))
    (or end n)))

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
    ;; Chapter 12: start: and end: are for a mutable sequence only
    (let* ((sequence? (true? (dylan-call dylan/instance? collection <sequence>)))
           (start (if sequence? (dylan%find-keyword rest :start (lambda () 0)) 0))
           (end (if sequence?
                    (check-start-end "fill!: bad start: or end: for ~s: ~s ~s"
                                     collection start
                                     (dylan%find-keyword rest :end
                                                         (lambda () dylan%false)))
                    (collection-size collection))))
      (do ((state (initial-state collection)
                  (next-state collection state))
           (index 0 (+ index 1)))
          ((or (not state) (>= index end)) collection)
        (when (and state (>= index start))
          (set-current-element
                      collection state new-value)))))))
