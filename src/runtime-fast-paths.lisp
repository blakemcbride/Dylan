;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright 2026 Blake McBride.  Part of the Common Lisp translation of
;;;; Thomas, and governed by the Digital notice in ../LICENSE.
;;;;
;;;; Turning on the fast paths.                         (new -- not in Thomas)
;;;;
;;;; DYLAN%CALL (runtime-internal.lisp) compiles a call of +, -, *, <, >,
;;;; <=, >=, =, element or (setter element) inline when the arguments are
;;;; Lisp reals or a simple vector and an index, and DYLAN%FOR-EACH walks a
;;;; single simple vector or proper list itself.  Each fast path is guarded
;;;; by a variable, true only while the generic functions that do the work
;;;; have no methods that could apply to those arguments but the predefined
;;;; ones (CHECK-FAST-PATH in generic.lisp clears it).  This file is loaded
;;;; last, once every predefined method is in place.

(in-package #:dylan)

(let* ((reals (list <integer> <ratio> <single-float> <double-float>
                    <extended-float>))
       (two-reals (list reals reals))
       (vector-index (list (list <simple-object-vector>) (list <integer>)))
       (one-vector (list (list <simple-object-vector>)))
       (one-list (list (list <pair> <empty-list>))))
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
    (guard dylan/element '*fast-element* vector-index)
    (guard dylan/setter/element/ '*fast-setter-element* vector-index)
    (dolist (generic-function (list dylan/initial-state dylan/next-state
                                    dylan/current-element dylan/dimensions))
      (guard generic-function
             '*fast-iterate-vector* one-vector '*fast-iterate-list* one-list))))
