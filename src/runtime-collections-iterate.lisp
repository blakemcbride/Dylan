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
;;;; Handles the iteration protocol of collections, including the method
;;;; specializations for all collection types.
;;;;                     (was Thomas's src/runtime-collections-iterate.scm)
;;;;
;;;; THE ITERATION PROTOCOL (page 122)
;;;; The specializations occur immediately after the definition of the
;;;; generic function.
;;;;
;;;; Implementation of iteration state:
;;;;
;;;; <EMPTY-LIST>: no states available
;;;; <PAIR> and <LIST>: the object itself, with NIL terminating
;;;; <ARRAY>: array of index values, incrementing "odometer style"
;;;; <TABLE>: pair of index into hash table and pointer to current entry in
;;;;          bucket.  Pointer is actually the list whose car is the
;;;;          key/element pair.   [state == (list hash-index bucket-left)]
;;;; <BYTE-STRING>: inherits from <array>
;;;; <STRING>: not handled, any user-specified subclass must supply
;;;;           operations
;;;; <DEQUE>: pointer to current entry
;;;; <RANGE>: current value

(in-package #:dylan)

(declaim (special dylan/next-state dylan/initial-state dylan/current-element
                  dylan/copy-state dylan/final-state dylan/previous-state))

;;; The built-in state methods are written in Lisp, with NIL for "no more
;;; states"; Dylan sees #f.  The runtime's own iteration goes through
;;; INITIAL-STATE, NEXT-STATE and the other functions below, which hand the
;;; Lisp code NIL for #f.  A state may be any other object, the empty list
;;; included (a user's collection may use one), so a state of () is carried
;;; inside the runtime as +EMPTY-LIST-STATE+ and turned back into () when it
;;; goes to a Dylan function.

(defun state-method (param-list lisp-function)
  (dylan%function->method param-list
                          (lambda (&rest arguments)
                            (or-false (apply lisp-function arguments)))))

(defun state-method-1 (name class lisp-function)
  (state-method (make-param-list `((,name ,class)) nil nil nil) lisp-function))

(defvar +empty-list-state+ (make-symbol "EMPTY-LIST-STATE"))

(defun lisp-state (state)
  "The runtime's form of a state from Dylan: NIL for #f."
  (cond ((false? state) nil)
        ((null state) +empty-list-state+)
        (t state)))

(defun dylan-state (state)
  "The Dylan form of a state the runtime holds."
  (if (eq state +empty-list-state+) '() state))

;;; While no method but the predefined ones could apply to a list, a simple
;;; vector or a range (*FAST-ITERATE-LIST*, *FAST-ITERATE-VECTOR* and
;;; *FAST-ITERATE-RANGE*, see runtime-fast-paths.lisp), INITIAL-STATE,
;;; NEXT-STATE and CURRENT-ELEMENT do for one what its predefined methods
;;; do, making the same states: the pair itself for a list, a vector of one
;;; index for a simple vector, and the index for a range.  A range is read
;;; afresh each time, as the methods do, since ADD! and REVERSE! change it.

(declaim (ftype function range-size range-start range-step))

(defun initial-state (collection)
  (cond ((and (listp collection) *fast-iterate-list*)
         (if (consp collection) collection nil))
        ((and (simple-vector-p collection) *fast-iterate-vector*)
         (if (plusp (length collection)) (vector 0) nil))
        ((and *fast-iterate-range* (instance? collection)
              (eq (instance.class collection) <range>))
         (let ((size (range-size collection)))
           (if (or (null size) (plusp size)) 0 nil)))
        (t (lisp-state (dylan-call dylan/initial-state collection)))))
(defun final-state (collection)
  (lisp-state (dylan-call dylan/final-state collection)))
(defun next-state (collection state)
  (cond ((and (listp collection) *fast-iterate-list*)
         (and (consp state) (cdr state)))
        ((and (simple-vector-p collection) *fast-iterate-vector*
              (simple-vector-p state) (= (length state) 1)
              (typep (svref state 0) 'fixnum)
              (< (svref state 0) (length collection)))
         (let ((next (1+ (svref state 0))))
           (if (< next (length collection)) (vector next) nil)))
        ((and *fast-iterate-range* (integerp state) (instance? collection)
              (eq (instance.class collection) <range>))
         (let ((size (range-size collection)))
           (if (or (null size) (< (+ state 1) size)) (+ state 1) nil)))
        (t (lisp-state (dylan-call dylan/next-state collection
                                   (dylan-state state))))))
(defun previous-state (collection state)
  (lisp-state (dylan-call dylan/previous-state collection (dylan-state state))))
(defun copy-state (collection state)
  (lisp-state (dylan-call dylan/copy-state collection (dylan-state state))))
(defun current-element (collection state)
  (cond ((and (listp collection) *fast-iterate-list*)
         (if (consp state) (car state) state))
        ((and (simple-vector-p collection) *fast-iterate-vector*
              (simple-vector-p state) (= (length state) 1)
              (typep (svref state 0) 'fixnum)
              (< -1 (svref state 0) (length collection)))
         (svref collection (svref state 0)))
        ((and *fast-iterate-range* (integerp state) (instance? collection)
              (eq (instance.class collection) <range>))
         (let ((step (range-step collection)))
           ;; The first element is FROM itself, exact or not.
           (if (or (zerop step) (zerop state))
               (range-start collection)
               (+ (range-start collection) (* state step)))))
        (t (dylan-call dylan/current-element collection
                       (dylan-state state)))))
(defun current-key (collection state)
  (dylan-call dylan/current-key collection (dylan-state state)))
(defun set-current-element (collection state new-value)
  (dylan-call dylan/setter/current-element/ collection (dylan-state state)
              new-value))

(defun collection-size (collection)
  "The size of COLLECTION, or NIL where Dylan's SIZE answers #f (an
unbounded collection)."
  (false->nil (dylan-call dylan/size collection)))

;;
;; INITIAL-STATE
;;
(defparameter dylan/initial-state
  (dylan%generic-fn
   'initial-state one-collection
   (lambda (collection)
     (dylan-call dylan/error
                 "initial-state -- not specialized for this type of collection"
                 collection))))

(add-method dylan/initial-state
            (state-method-1 'null <empty-list>
                     (lambda (null) (declare (ignore null)) nil)))
(add-method dylan/initial-state
            (state-method-1 'list <list> #'identity))
(add-method dylan/initial-state
            (state-method-1 'array <array>
                     (lambda (array)
                       (let ((dimensions (dylan-call dylan/dimensions array)))
                         (if (all? #'plusp (iterate->list #'identity dimensions))
                             (make-array (length dimensions) :initial-element 0)
                             nil)))))
(add-method dylan/initial-state
            (state-method-1 'deque <deque>
                     (lambda (deque)
                       (dylan-call dylan/get-deque-front deque))))
;;; A range's state is the index of its element, which is FROM + k*BY.
;;; Computing each element from its index, rather than adding BY to the
;;; last, keeps a range with a float step in agreement with its own SIZE,
;;; LAST and ELEMENT.
(add-method dylan/initial-state
            (state-method-1 'range <range>
                     (lambda (range)
                       (let ((size (collection-size range)))
                         (if (or (null size) (plusp size)) 0 nil)))))
(add-method dylan/initial-state
            (state-method-1 'table <table>
                     (lambda (table)
                       (let* ((hash-table (dylan-call dylan/get-hash-table table))
                              (next-bucket (find-next-non-empty-bucket
                                            hash-table -1)))
                         (if next-bucket
                             (list next-bucket (aref hash-table next-bucket))
                             nil)))))

;;
;; FINAL-STATE
;;
(defparameter dylan/final-state (dylan%generic-fn 'final-state one-object nil))

(add-method dylan/final-state
            (state-method-1 'empty-list <empty-list>
                     (lambda (emp-list) (declare (ignore emp-list)) nil)))
;;; Not specified in the manual; only for efficiency:
;;; (add-method dylan/final-state
;;;             (state-method-1 'string <byte-string>
;;;                      (lambda (string)
;;;                        (let ((length (length string)))
;;;                          (if (zerop length) nil (- length 1))))))
;;; Array states are vectors of indices, as INITIAL-STATE and NEXT-STATE
;;; make them; the Scheme's final states were a list for arrays and an
;;; integer for simple vectors, which the other state functions could not
;;; take back.
(add-method dylan/final-state           ; Not specified in the manual
            (state-method-1 'array <array>
                     (lambda (array)
                       (if (true? (dylan-call dylan/empty? array))
                           nil
                           (map 'simple-vector (lambda (index) (- index 1))
                                (iterate->list
                                 #'identity
                                 (dylan-call dylan/dimensions array)))))))
(add-method dylan/final-state
            (state-method-1 'deque <deque>
                     (lambda (deque)
                       (dylan-call dylan/get-deque-last deque))))

;;
;; NEXT-STATE
;;
(defparameter dylan/next-state
  (dylan%generic-fn 'next-state one-collection-and-a-state nil))

(add-method
 dylan/next-state
 (state-method
  (make-param-list `((list ,<list>) (state ,<object>)) nil nil nil)
  (lambda (list state)
    (declare (ignore list))
    (if (not (consp state))
        nil                             ; If dotted list, and reached end
        (let ((left (cdr state)))
          (cond ((null left) nil)
                ;; For now: if not a pair, return the atom
                ((not (consp left)) left)
                (t left)))))))

(add-method                             ; Array => "Odometer style"
 dylan/next-state
 (state-method
  (make-param-list `((array ,<array>) (state ,<simple-object-vector>))
                   nil nil nil)
  (lambda (array state)
    (let ((dimensions (dylan-call dylan/dimensions array))
          (next-state-indices (copy-seq state)))
      (loop with dim-index = (- (length dimensions) 1)
            do (if (minusp dim-index)
                   (return nil)
                   (cond ((= (+ (aref next-state-indices dim-index) 1)
                             (nth dim-index dimensions))
                          (decf dim-index))
                         ((> (+ (aref next-state-indices dim-index) 1)
                             (nth dim-index dimensions))
                          (dylan-call dylan/error
                                      "next-state -- invalid state" state))
                         (t
                          (loop for i from (+ dim-index 1)
                                  below (length next-state-indices)
                                do (setf (aref next-state-indices i) 0))
                          (incf (aref next-state-indices dim-index))
                          (return next-state-indices)))))))))

(add-method
 dylan/next-state
 (state-method
  (make-param-list `((null ,<empty-list>) (state ,<object>)) nil nil nil)
  (lambda (emp-list state) (declare (ignore emp-list state)) nil)))

(add-method
 dylan/next-state
 (state-method
  (make-param-list `((deque ,<deque>) (state ,<object>)) nil nil nil)
  (lambda (deque state)
    (declare (ignore deque))
    (deque-entry.next state))))

(add-method
 dylan/next-state
 (state-method
  (make-param-list `((range ,<range>) (state ,<object>)) nil nil nil)
  (lambda (range state)
    (let ((size (collection-size range)))
      (if (or (null size) (< (+ state 1) size)) (+ state 1) nil)))))

(add-method
 dylan/next-state
 (state-method
  (make-param-list `((table ,<table>) (state ,<object>)) nil nil nil)
  (lambda (table state)
    (let ((hash-index (car state))
          (bucket-left (cadr state))
          (hash-table (dylan-call dylan/get-hash-table table)))
      (cond ((null bucket-left)
             (dylan-call dylan/error
                         "(next-state <table> <object>) -- bad state"
                         table state))
            ((null (cdr bucket-left))
             (let ((next-bucket (find-next-non-empty-bucket hash-table
                                                            hash-index)))
               (if next-bucket
                   (list next-bucket (aref hash-table next-bucket))
                   nil)))
            (t (list hash-index (cdr bucket-left))))))))

;;
;; PREVIOUS-STATE
;;
(defparameter dylan/previous-state
  (dylan%generic-fn 'previous-state one-collection-and-a-state nil))

(add-method                             ; Array => "Odometer style"
 dylan/previous-state
 (state-method
  (make-param-list `((array ,<array>) (state ,<sequence>)) nil nil nil)
  ;; The odometer run backwards: decrement the last index that can be, and
  ;; set every index after it to its maximum.  The state passed in is not
  ;; modified.
  (lambda (array state)
    (let ((dimensions (iterate->list #'identity
                                     (dylan-call dylan/dimensions array)))
          (previous (copy-seq state)))
      (loop for dim-index from (- (length dimensions) 1) downto 0
            when (plusp (aref previous dim-index))
              do (decf (aref previous dim-index))
                 (loop for i from (+ dim-index 1) below (length previous)
                       do (setf (aref previous i) (- (nth i dimensions) 1)))
                 (return previous)
            finally (return nil))))))

(add-method
 dylan/previous-state
 (state-method
  (make-param-list `((null ,<empty-list>) (state ,<object>)) nil nil nil)
  (lambda (emp-list state) (declare (ignore emp-list state)) nil)))

(add-method
 dylan/previous-state
 (state-method
  (make-param-list `((deque ,<deque>) (state ,<object>)) nil nil nil)
  (lambda (deque state)
    (declare (ignore deque))
    (deque-entry.previous state))))

(add-method                             ; Not defined in the manual
 dylan/previous-state
 (state-method
  (make-param-list `((range ,<range>) (state ,<object>)) nil nil nil)
  (lambda (range state)
    (declare (ignore range))
    (if (plusp state) (- state 1) nil))))

;;
;; CURRENT-ELEMENT
;;
(defparameter dylan/current-element
  (dylan%generic-fn
   'current-element one-collection-and-a-state
   (lambda (collection state)
     (dylan-call dylan/error
                 "current-element -- not specialized for this collection type"
                 collection state))))

(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((array ,<array>) (state ,<sequence>)) nil nil nil)
  (lambda (array state) (dylan-call dylan/element array state))))
(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((sov ,<simple-object-vector>) (state ,<sequence>))
                   nil nil nil)
  (lambda (vec state) (aref vec (aref state 0)))))
(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((list ,<list>) (state ,<object>)) nil nil nil)
  (lambda (list state)
    (declare (ignore list))
    (if (consp state)
        (car state)
        state))))                       ; If reached dotted list end...
(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((null ,<empty-list>) (state ,<sequence>)) nil nil nil)
  (lambda (emp-list state) (declare (ignore emp-list state)) nil)))
(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/current-element
   (dylan%function->method
    (make-param-list `((string ,class) (state ,<sequence>))
                     nil nil nil)
    (lambda (string state) (char string (aref state 0))))))
(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((deque ,<deque>) (state ,<object>)) nil nil nil)
  (lambda (deque state)
    (declare (ignore deque))
    (deque-entry.value state))))
(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((range ,<range>) (state ,<object>)) nil nil nil)
  (lambda (range state)
    (let ((step (dylan-call dylan/get-range-step range)))
      ;; The first element is FROM itself, exact or not.
      (if (or (zerop step) (zerop state))
          (dylan-call dylan/get-range-start range)
          (+ (dylan-call dylan/get-range-start range) (* state step)))))))
(add-method
 dylan/current-element
 (dylan%function->method
  (make-param-list `((table ,<table>) (state ,<object>)) nil nil nil)
  (lambda (table state)
    (declare (ignore table))
    (cadr (car (cadr state))))))

(defparameter dylan/copy-state
  (dylan%generic-fn 'copy-state one-collection-and-a-state
                    (lambda (collection state)
                      (declare (ignore collection))
                      state)))

;;; End of Iteration Functions

(defun iterate-until (fn collection)
  "Given FN and a collection, iterate until the collection runs out of
elements or FN returns a non-NIL value."
  (loop with state = (initial-state collection)
        do (if (not state)
               (return nil)
               (let ((result (funcall fn (current-element
                                                     collection state))))
                 (if result
                     (return result)
                     (setf state (next-state
                                             collection state)))))))

(defun iterate->list (fn collection)
  (loop with state = (initial-state collection)
        with value = '()
        do (if state
               (let ((new-value (funcall fn (current-element
                                                        collection state))))
                 (setf state (next-state collection state))
                 (push new-value value))
               (return (nreverse value)))))

;;;; The Iteration Protocol (page 122)
;;;; Actually, these are internal procedures used elsewhere to iterate over
;;;; collections or sets of collections

(declaim (ftype function aligned-keys sequence?))

(defun collections-iterate (fn done? default-value collections)
  "FN is a Dylan function to be applied to parallel elements from each
collection.  DONE? is a Lisp function applied to the result of FN to test
for loop completion.  It returns NIL to continue the iteration, or a Lisp
thunk to return the value.  DEFAULT-VALUE is returned if any collection
runs out before the DONE? test causes an exit."
  (unless (all? (lambda (collection)
                  (subclass? (get-type collection) <collection>))
                collections)
    (dylan-call dylan/error "do -- not all arguments are collections"
                collections))
  ;; Chapter 12, "Collection Alignment": unless they are all sequences,
  ;; several collections are aligned by key, and FN sees the elements at
  ;; each key they share.
  (when (and (cdr collections) (notevery #'sequence? collections))
    (let ((keys (aligned-keys collections)))
      (when (sequence? (car collections))
        (setf keys (sort (iterate->list #'identity keys) #'<)))
      (let ((result? (iterate-until
                      (lambda (key)
                        (funcall done?
                                 (dylan-apply fn
                                              (mapcar (lambda (collection)
                                                        (dylan-call
                                                         dylan/element
                                                         collection key))
                                                      collections))))
                      keys)))
        (return-from collections-iterate
          (if result? (funcall result?) default-value)))))
  (loop with states = (mapcar (lambda (collection)
                                (initial-state collection))
                              collections)
        do (if (any? #'not states)
               (return default-value)
               (let* ((ins (mapcar (lambda (collection state)
                                     (current-element
                                                 collection state))
                                   collections states))
                      (next-val (dylan-apply fn ins))
                      (result? (funcall done? next-val)))
                 (if result?
                     (return (funcall result?))
                     (setf states (mapcar (lambda (collection state)
                                            (next-state
                                                        collection state))
                                          collections states)))))))

(defun iterate! (fn collection)
  (loop with state = (initial-state collection)
        while state
        do (funcall fn (current-element collection state))
           (setf state (next-state collection state))))

(defun find-next-non-empty-bucket (hash-table index)
  (loop for i from (+ index 1) below (length hash-table)
        unless (null (aref hash-table i)) return i))

;;
;; DYLAN/GET-STATE: given a collection and a state index, return the
;;                  corresponding state.
;;                  initial-state = 0
;;                  If no corresponding state, return NIL
;;
(defparameter dylan/get-state
  (dylan%generic-fn
   'get-state
   (make-param-list `((collection ,<collection>) (index ,<number>))
                    nil nil nil)
   (lambda (collection index)
     (do ((i 0 (+ i 1))
          (state (initial-state collection)
                 (next-state collection state)))
         ((or (= i index) (not state)) state)))))

;;;;
;;;; Collection Keys (page 123)
;;;;

(defparameter dylan/element
  (dylan%generic-fn
   'element
   (make-param-list `((collection ,<collection>) (key ,<object>))
                    nil nil '(:default))
   nil))

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((collection ,<collection>) (key ,<object>))
                   nil nil '(:default))
  (lambda (multiple-values next-method coll key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (dylan-call dylan/error "element -- not specialized for this type"
                coll key rest))))

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((explicit-key-collection ,<explicit-key-collection>)
                     (key ,<object>))
                   nil nil '(:default))
  (lambda (multiple-values next-method exp-coll key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let* ((default-marker (cons 1 2))
           ;; N.B. the Scheme looked for the symbol `default' rather than
           ;; the keyword `default:', so the default: argument was in fact
           ;; never found; fixed.
           (default (dylan%find-keyword rest :default
                                        (lambda () default-marker))))
      (do ((state (initial-state exp-coll)
                  (next-state exp-coll state)))
          ((or (not state)
               (true? (dylan-call dylan/=
                                  (current-key exp-coll state)
                                  key)))
           (cond (state (current-element exp-coll state))
                 ((eq default default-marker)
                  (dylan-call dylan/error
                              "element -- no such element in collection"
                              exp-coll key rest))
                 (t default))))))))

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>) (key ,<integer>))
                   nil nil '(:default))
  (lambda (multiple-values next-method seq key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let* ((default-marker (cons 1 2))
           (default (dylan%find-keyword rest :default
                                        (lambda () default-marker))))
      (do ((state (initial-state seq)
                  (next-state seq state))
           (k 0 (+ k 1)))
          ((or (not state) (= k key))
           (cond (state (current-element seq state))
                 ((eq default default-marker)
                  (dylan-call dylan/error
                              "element -- no such element in collection"
                              seq key rest))
                 (t default))))))))

;;; A sequence's keys are integers, so any other key is not among them: the
;;; answer is default:, as chapter 12 says for a key that is absent.
(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>) (key ,<object>))
                   nil nil '(:default))
  (lambda (multiple-values next-method seq key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (dylan%find-keyword rest :default
                        (lambda ()
                          (dylan-call dylan/error
                                      "element -- no such element in collection"
                                      seq key rest))))))

(defparameter dylan/key-sequence
  (dylan%generic-fn
   'key-sequence one-collection
   (lambda (collection)
     (dylan-call dylan/error
                 "key-sequence -- not defined for this collection type"
                 collection))))

(add-method
 dylan/key-sequence
 (dylan%function->method
  (make-param-list `((explicit-key-collection ,<explicit-key-collection>))
                   nil nil nil)
  (lambda (exp-coll)
    (do ((state (initial-state exp-coll)
                (next-state exp-coll state))
         (keys '() (cons (current-key exp-coll state) keys)))
        ((not state) (reverse keys))))))

(add-method
 dylan/key-sequence
 (dylan%function->method
  one-sequence
  (lambda (seq)
    (let ((size (collection-size seq)))
      ;; An unbounded sequence's keys are all the non-negative integers.
      (if (not size)
          (dylan-call dylan/range :from 0)
          (dylan-call dylan/range :from 0 :through (- size 1) :by 1))))))

;; A vector is an array, whose keys are explicit, but its keys are the
;; range of its indices, as for any sequence (and as the compiler to C
;; answers).
(add-method
 dylan/key-sequence
 (dylan%function->method
  one-vector
  (lambda (vector)
    (dylan-call dylan/range :from 0 :through (- (collection-size vector) 1) :by 1))))

(defparameter dylan/current-key
  (dylan%generic-fn
   'current-key
   (make-param-list `((collection ,<explicit-key-collection>)
                      (state ,<object>))
                    nil nil nil)
   (lambda (collection state)
     (dylan-call dylan/error
                 "current-key -- not implemented for this collection type"
                 collection state))))

(add-method
 dylan/current-key
 (dylan%function->method
  (make-param-list `((table ,<table>) (state ,<object>)) nil nil nil)
  (lambda (table state)
    (declare (ignore table))
    (car (car (cadr state))))))
