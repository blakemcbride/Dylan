;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright 2026 Blake McBride.  Part of the Common Lisp translation of
;;;; Thomas, and governed by the Digital notice in ../LICENSE.
;;;;
;;;; Turning on the fast paths.                         (new -- not in Thomas)
;;;;
;;;; DYLAN%CALL (runtime-internal.lisp) compiles a call of +, -, *, <, >,
;;;; <=, >=, =, element or (setter element) inline when the arguments are
;;;; Lisp reals or a simple vector and an index.  Each inline form is
;;;; guarded by a variable, true only while the generic functions that do
;;;; the work have no methods that could apply to those arguments but the
;;;; predefined ones (CHECK-FAST-PATH in generic.lisp clears it).  This file
;;;; is loaded last, once every predefined method is in place.

(in-package #:dylan)

(let ((reals (list <integer> <ratio> <single-float> <double-float>
                   <extended-float>))
      (vector-index (list (list <simple-object-vector>) (list <integer>))))
  (flet ((guard (generic-function guards classes)
           (let ((data (generic-function-data generic-function)))
             (setf (generic-function-data.fast-path-guards data) guards
                   (generic-function-data.fast-path-classes data) classes)
             (dolist (guard guards)
               (setf (symbol-value guard) t)))))
    (guard dylan/binary+ '(*fast-binary+*) (list reals reals))
    (guard dylan/binary- '(*fast-binary-*) (list reals reals))
    (guard dylan/binary* '(*fast-binary**) (list reals reals))
    (guard dylan/binary< '(*fast-binary<* *fast-binary<=*) (list reals reals))
    (guard dylan/binary= '(*fast-binary=* *fast-binary<=*) (list reals reals))
    (guard dylan/element '(*fast-element*) vector-index)
    (guard dylan/setter/element/ '(*fast-setter-element*) vector-index)))
