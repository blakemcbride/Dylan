;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright 2026 Blake McBride.  Part of the Common Lisp translation of
;;;; Thomas, and governed by the Digital notice in ../LICENSE.
;;;;
;;;; Turning on the fast paths.                         (new -- not in Thomas)
;;;;
;;;; DYLAN%CALL (runtime-internal.lisp) compiles a call of +, -, *, <, >,
;;;; <=, >=, =, modulo, remainder, element or (setter element) inline when
;;;; the arguments are Lisp reals, characters or integers, or a simple
;;;; vector or string and an index; DYLAN%FOR-EACH and the runtime's
;;;; iteration functions walk a simple vector, list or range themselves.
;;;; Each fast path is guarded by a variable, true only while the generic
;;;; functions that do the work have no methods that could apply to those
;;;; arguments but the predefined ones (CHECK-FAST-PATH in generic.lisp
;;;; clears it).  This file is loaded last, once every predefined method is
;;;; in place.

(in-package #:dylan)

(let* ((reals (list <integer> <ratio> <single-float> <double-float>
                    <extended-float>))
       (two-reals (list reals reals))
       (two-integers (list (list <integer>) (list <integer>)))
       (simple-key (list (append reals (list <character> <symbol> <keyword>))))
       (two-characters (list (list <character>) (list <character>)))
       (vector-index (list (list <simple-object-vector>) (list <integer>)))
       (string-index (list (list <byte-string>) (list <integer>)))
       (one-vector (list (list <simple-object-vector>)))
       (one-list (list (list <pair> <empty-list>)))
       (one-range (list (list <range>))))
  (flet ((guard (generic-function &rest guards-and-classes)
           (let ((data (generic-function-data generic-function)))
             (loop for (guard classes) on guards-and-classes by #'cddr
                   do (push (list guard classes)
                            (generic-function-data.fast-paths data))
                      (setf (symbol-value guard) t)))))
    (guard dylan/binary+ '*fast-binary+* two-reals)
    (guard dylan/binary- '*fast-binary-* two-reals)
    (guard dylan/binary* '*fast-binary** two-reals)
    (guard dylan/binary< '*fast-binary<* two-reals '*fast-binary<=* two-reals)
    (guard dylan/binary= '*fast-binary=* two-reals '*fast-binary<=* two-reals)
    (guard dylan/binary< '*fast-character-compare* two-characters)
    (guard dylan/binary= '*fast-character-compare* two-characters)
    ;; Only the first argument's class is known: a table's own key.
    (guard dylan/binary= '*fast-key=* simple-key)
    (guard dylan/=hash '*fast-key=* simple-key)
    (guard dylan/modulo '*fast-modulo* two-integers)
    (guard dylan/floor/ '*fast-modulo* two-integers)
    (guard dylan/remainder '*fast-remainder* two-integers)
    (guard dylan/element '*fast-element* vector-index
           '*fast-string-element* string-index)
    (guard dylan/setter/element/ '*fast-setter-element* vector-index
           '*fast-string-setter-element* string-index)
    (dolist (generic-function (list dylan/initial-state dylan/next-state
                                    dylan/current-element dylan/dimensions))
      (guard generic-function
             '*fast-iterate-vector* one-vector '*fast-iterate-list* one-list))
    (dolist (generic-function (list dylan/initial-state dylan/next-state
                                    dylan/current-element dylan/size))
      (guard generic-function '*fast-iterate-range* one-range))))
