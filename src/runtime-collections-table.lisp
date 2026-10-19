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
;;;; Specializations for tables.
;;;;                        (was Thomas's src/runtime-collections-table.scm)

(in-package #:dylan)

(declaim (special dylan/get-hash-table dylan/set-hash-table!))

(add-method
 dylan/shallow-copy
 (dylan%function->method
  (make-param-list `((table ,<table>)) nil nil nil)
  (lambda (table)
    (let* ((new-table (dylan-call dylan/make <table>))
           (key-sequence (dylan-call dylan/key-sequence table)))
      (do ((state (initial-state key-sequence)
                  (next-state key-sequence state)))
          ((not state)
           (dylan-call dylan/as (dylan-call dylan/class-for-copy table)
                       new-table))
        (let ((key (current-element key-sequence state)))
          (dylan-call dylan/setter/element/ new-table key
                      (dylan-call dylan/element table key))))))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <table>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <table>))
        collection
        (let ((table (dylan-call dylan/make <table>))
              (key-sequence (dylan-call dylan/key-sequence collection)))
          (do ((state (initial-state key-sequence)
                      (next-state key-sequence state)))
              ((not state) table)
            (let ((cur-element (current-element
                                           key-sequence state)))
              (dylan-call dylan/setter/element/ table cur-element
                          (dylan-call dylan/element collection
                                      cur-element)))))))))

;;;
;;; TABLE SPECIALIZED MAKE
;;; <table> creates an empty hash table
;;;
(defparameter *hash-table-size* 500)

(defparameter dylan/get-hash-table nil)
(defparameter dylan/set-hash-table! nil)

(create-private-slot <table> <vector> "internal-hash-table"
                     (lambda (set get)
                       (setf dylan/set-hash-table! set)
                       (setf dylan/get-hash-table get)))

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((table ,(dylan%make-singleton <table>))) nil nil :any)
  (lambda (class &rest rest)
    (declare (ignore class rest))
    (let ((instance (dylan%make-<object> <table>)))
      (dylan-call dylan/set-hash-table! instance
                  (make-array *hash-table-size* :initial-element '()))
      instance))))

;;;;
;;;; Operations on Tables (page 120)
;;;;

;;; For a key that is a real, a character or a symbol other than NIL (the
;;; empty list), the predefined BINARY= methods compare with = when the
;;; other key is a number and EQ otherwise, and =HASH hashes it as below.
;;; While no other method could apply to such a key (*FAST-KEY=*, see
;;; runtime-fast-paths.lisp), tables do that themselves.

(defun fast-key? (key)
  (and *fast-key=*
       (or (realp key) (characterp key) (and key (symbolp key)))))

(defun dylan-assoc (key alist)
  "Use DYLAN/BINARY= to compare keys."
  (cond ((not (fast-key? key))
         (loop for rest-alist = alist then (cdr rest-alist)
               do (if rest-alist
                      (when (true? (dylan-call dylan/binary= key
                                               (caar rest-alist)))
                        (return (car rest-alist)))
                      (return nil))))
        ((realp key)
         (loop for entry in alist
               when (let ((other (car entry))) (and (numberp other) (= key other)))
                 return entry))
        (t (assoc key alist :test #'eq))))

(defun table-bucket (hash-table key)
  "The index in HASH-TABLE of KEY's bucket.  MOD, unlike the Scheme's
REMAINDER, never answers a negative index."
  (mod (if (fast-key? key)
           (if (realp key) (real-hash key) (sxhash key))
           (dylan-call dylan/=hash key))
       (length hash-table)))

(defparameter dylan/remove-key!
  (dylan%generic-fn
   'remove-key!
   (make-param-list `((table ,<table>) (key ,<object>)) nil nil nil)
   (lambda (table key)
     (let* ((hash-table (dylan-call dylan/get-hash-table table))
            (hash-index (table-bucket hash-table key))
            (hash-entry (aref hash-table hash-index))
            (match (dylan-assoc key hash-entry)))
       (when match
         (setf (aref hash-table hash-index)
               (dylan-call dylan/remove hash-entry match)))
       table))))                        ; Chapter 12: answers the table

(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list `((table ,<table>) (key ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (table key new-value)
    (let ((hash-index (table-bucket (dylan-call dylan/get-hash-table table) key))
          (hash-table (dylan-call dylan/get-hash-table table)))
      (if (>= hash-index (length hash-table))
          (dylan-call
           dylan/error
           "((setter element) <table> <object> <object>) -- internal error, size out of sync"
           table hash-index key new-value)
          (let* ((hash-entry (aref hash-table hash-index))
                 (match (dylan-assoc key hash-entry)))
            (if match
                (setf (cdr match) (list new-value))
                (progn
                  (setf (aref hash-table hash-index)
                        (cons (list key new-value) hash-entry))
                  (when (> (length hash-entry) 8)
                    (maybe-grow-table table hash-table))))
            new-value))))))

(defun maybe-grow-table (table hash-table)
  "Double TABLE's bucket vector once it holds more entries than buckets, so
that buckets stay short.  Not done when one bucket is long only because
many keys share a hash code; growing would not help them."
  (when (> (loop for bucket across hash-table sum (length bucket))
           (length hash-table))
    (let ((new-table (make-array (* 2 (length hash-table))
                                 :initial-element '())))
      (loop for bucket across hash-table
            do (dolist (entry bucket)
                 (push entry (aref new-table
                                   (table-bucket new-table (car entry))))))
      (dylan-call dylan/set-hash-table! table new-table))))

;;; Chapter 12 calls a table a hashtable: ELEMENT looks in the key's bucket
;;; rather than leaving it to the linear search of the generic
;;; <explicit-key-collection> method.
(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((table ,<table>) (key ,<object>)) nil nil '(:default))
  (lambda (multiple-values next-method table key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let* ((hash-table (dylan-call dylan/get-hash-table table))
           (match (dylan-assoc key (aref hash-table
                                         (table-bucket hash-table key)))))
      (if match
          (cadr match)
          (dylan%find-keyword
           rest :default
           (lambda ()
             (dylan-call dylan/error
                         "element -- no such element in collection"
                         table key rest))))))))

(defun grow-vector-by (v increase)
  (let ((new-v (make-array (+ (length v) increase))))
    (replace new-v v)
    new-v))

(add-method
 dylan/map-into
 (dylan%function->method
  (make-param-list `((table ,<table>) (procedure ,<function>)
                     (collection ,<collection>))
                   nil 'rest nil)
  (lambda (table proc coll-1 &rest rest)
    (loop with key-sequence = (dylan-call dylan/key-sequence coll-1)
          with rest-coll = rest
          do (if rest-coll
                 (progn
                   (setf key-sequence
                         (dylan-call dylan/intersection key-sequence
                                     (dylan-call dylan/key-sequence
                                                 (car rest-coll))))
                   (pop rest-coll))
                 (let ((all-collections (cons coll-1 rest)))
                   (return
                     (do ((state (initial-state key-sequence)
                                 (next-state key-sequence
                                             state)))
                         ((not state) table)
                       (let ((current-key (current-element
                                                      key-sequence state)))
                         (dylan-call
                          dylan/setter/element/
                          table current-key
                          (dylan-call dylan/apply proc
                                      (mapcar (lambda (coll)
                                                (dylan-call dylan/element
                                                            coll current-key))
                                              all-collections))))))))))))

;;;
;;; Mutable Collections
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((table ,<table>) (state ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (table state new-value)
    (dylan-call dylan/setter/element/ table
                (current-key table state)
                new-value))))
