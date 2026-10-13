;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; The class table: every class of the program, with what the runtime
;;;; needs to know of each, emitted as static C data (cdata.lisp).
;;;;
;;;; The program is static, so the compiler knows every class.  It numbers
;;;; them in a preorder walk of the tree each class makes with its first
;;;; direct superclass, so that a class's descendants in that tree have the
;;;; ids from its own to its SUBTREE-END.  Where every subclass of a class
;;;; -- through any of their superclasses -- lies in that range, instance?
;;;; against the class is a range check (RANGE-OK-P); otherwise it tests a
;;;; bit in the ancestor set every class carries (CompilerPlan.md, Phase
;;;; 3).
;;;;
;;;; Each class has a KIND: how its instances are represented in C.  The
;;;; kinds are the DY_KIND_ constants of runtime/c/dylan.h.

(in-package #:dylan-compiler)

(defvar *classes* nil "The class table of the program being compiled.")

(defstruct (class-info (:constructor %make-class-info))
  name                                  ; string, as Dylan writes it
  supers                                ; CLASS-INFOs, in order
  (kind :none)                          ; representation; :NONE for none yet
  (abstract-p nil)                      ; no direct instances
  (sealed-p nil)
  (exported-p t)                        ; visible to programs?
  (cpl '())                             ; class precedence list, self first
  (subclasses '())                      ; direct subclasses, in order
  id
  subtree-end
  range-ok-p
  c-name                                ; C identifier stem
  (size 0)                              ; :INSTANCE: number of slots
  (slot-offsets nil)                    ; vector over the program's instance
                                        ; slots: index, or -1; NIL if none
  (maker nil))                          ; C name of its make, or NIL

(defmethod print-object ((c class-info) stream)
  (print-unreadable-object (c stream :type t)
    (write-string (class-info-name c) stream)))

;;; The built-in classes: those whose instances the runtime represents
;;; itself.  Provisional, like dylan-names.lisp: when the library is
;;; written in Dylan (Phase 8) it defines them.  The hierarchy
;;; is the manual's (chapters 11 to 16 and their figures), as Thomas has
;;; it, less what version 1 leaves out: <complex> stays, abstract, as the
;;; superclass of <real>, but has no instances, and there is no
;;; <rectangular-complex>.  <boolean>, the class of #t and #f, is the
;;; runtime's own: the manual names no class for them.
;;;
;;; (name (superclass ...) kind &rest flags)

(defparameter *built-in-classes*
  '(("<object>" () :none :abstract)
    ("<boolean>" ("<object>") :boolean :sealed :internal)
    ;; Numbers (chapter 14)
    ("<number>" ("<object>") :none :abstract)
    ("<complex>" ("<number>") :none :abstract :sealed)
    ("<real>" ("<complex>") :none :abstract :sealed)
    ("<rational>" ("<real>") :none :abstract :sealed)
    ("<integer>" ("<rational>") :integer :sealed)
    ("<ratio>" ("<rational>") :ratio :sealed)
    ("<float>" ("<real>") :none :abstract :sealed)
    ("<single-float>" ("<float>") :single-float :sealed)
    ("<double-float>" ("<float>") :double-float :sealed)
    ("<extended-float>" ("<float>") :extended-float :sealed)
    ;; Collections (chapter 12)
    ("<collection>" ("<object>") :none :abstract)
    ("<explicit-key-collection>" ("<collection>") :none :abstract)
    ("<mutable-collection>" ("<collection>") :none :abstract)
    ("<sequence>" ("<collection>") :none :abstract)
    ("<mutable-explicit-key-collection>"
     ("<explicit-key-collection>" "<mutable-collection>") :none :abstract)
    ("<mutable-sequence>" ("<mutable-collection>" "<sequence>") :none :abstract)
    ("<array>" ("<mutable-explicit-key-collection>") :none)
    ("<table>" ("<mutable-explicit-key-collection>") :none)
    ("<vector>" ("<array>" "<mutable-sequence>") :none)
    ("<string>" ("<mutable-sequence>") :none)
    ("<deque>" ("<mutable-sequence>") :none)
    ("<range>" ("<sequence>") :none)
    ("<stretchy-vector>" ("<vector>") :none)
    ("<simple-object-vector>" ("<vector>") :simple-object-vector :sealed)
    ("<unicode-string>" ("<vector>" "<string>") :unicode-string :sealed)
    ("<byte-string>" ("<vector>" "<string>") :byte-string :sealed)
    ("<list>" ("<mutable-sequence>") :none :abstract :sealed)
    ("<pair>" ("<list>") :pair :sealed)
    ("<empty-list>" ("<list>") :empty-list :sealed)
    ;; The conditions of chapter 13 are defined by the library
    ;; (runtime/dylan/conditions.dyl).
    ;; Functions, classes and the rest (chapters 6, 11, 15, 16)
    ("<function>" ("<object>") :none :abstract)
    ("<generic-function>" ("<function>") :generic)
    ("<method>" ("<function>") :function :abstract)
    ("<class>" ("<object>") :class :abstract)
    ("<slot-descriptor>" ("<object>") :none :abstract)
    ("<singleton>" ("<object>") :singleton :abstract)
    ("<symbol>" ("<object>") :symbol)
    ("<keyword>" ("<object>") :keyword)
    ("<character>" ("<object>") :character)))

(defun built-in-class-infos ()
  "Fresh CLASS-INFOs for the built-in classes, in order."
  (let ((table (make-hash-table :test #'equal))
        (classes '()))
    (dolist (spec *built-in-classes*)
      (destructuring-bind (name supers kind &rest flags) spec
        (let ((class (%make-class-info
                      :name name
                      :supers (mapcar (lambda (s)
                                        (or (gethash s table)
                                            (error "~a before its superclass ~a"
                                                   name s)))
                                      supers)
                      :kind kind
                      :abstract-p (and (member :abstract flags) t)
                      :sealed-p (and (member :sealed flags) t)
                      :exported-p (not (member :internal flags)))))
          (setf (gethash name table) class)
          (push class classes))))
    (nreverse classes)))

;;; Class precedence lists, as the CLOS algorithm computes them (and as
;;; src/class.lisp does): a topological sort of the class and its
;;; superclasses that keeps each class before its direct superclasses and
;;; those in their declared order, breaking ties in favour of the
;;; candidate with a direct subclass furthest along the list so far.

(defun compute-cpl (class)
  (let ((classes '()))
    (labels ((collect (c)
               (unless (member c classes)
                 (push c classes)
                 (mapc #'collect (class-info-supers c)))))
      (collect class))
    (let ((constraints '())
          (result '())
          (remaining (reverse classes)))
      (dolist (c classes)
        (loop for before in (cons c (class-info-supers c))
              for after in (class-info-supers c)
              do (push (cons before after) constraints)))
      (loop while remaining
            do (let ((candidates
                       (remove-if (lambda (c)
                                    (find-if (lambda (k)
                                               (and (eq (cdr k) c)
                                                    (member (car k) remaining)))
                                             constraints))
                                  remaining)))
                 (when (null candidates)
                   (error "inconsistent class precedence for ~a"
                          (class-info-name class)))
                 (let ((next
                         (if (null (cdr candidates))
                             (car candidates)
                             (or (dolist (c result nil)
                                   (let ((hit (find-if
                                               (lambda (k)
                                                 (member k (class-info-supers c)))
                                               candidates)))
                                     (when hit (return hit))))
                                 (car candidates)))))
                   (push next result)
                   (setf remaining (remove next remaining)))))
      (nreverse result))))

;;; Numbering

(defun c-identifier (name)
  "A C identifier stem for the Dylan NAME: <single-float> is
single_float; a character that cannot appear in one is _xHH."
  (let ((stem (if (and (> (length name) 2)
                       (char= (char name 0) #\<)
                       (char= (char name (1- (length name))) #\>))
                  (subseq name 1 (1- (length name)))
                  name)))
    (with-output-to-string (out)
      (loop for ch across (string-downcase stem)
            do (cond ((or (char<= #\a ch #\z) (char<= #\0 ch #\9)) (write-char ch out))
                     ((char= ch #\-) (write-char #\_ out))
                     (t (format out "_x~2,'0x" (char-code ch))))))))

(defun prepare-classes (classes)
  "Compute the precedence list, id, subtree and C name of each of
CLASSES, which must include <object> and every superclass of each."
  (let ((used (make-hash-table :test #'equal)))
    (dolist (c classes)
      (setf (class-info-cpl c) (compute-cpl c)
            (class-info-subclasses c) '())
      (let* ((stem (c-identifier (class-info-name c)))
             (name (loop for n from 1
                         for candidate = stem then (format nil "~a_~d" stem n)
                         unless (gethash candidate used)
                           return candidate)))
        (setf (gethash name used) t
              (class-info-c-name c) name)))
    (dolist (c (reverse classes))
      (dolist (s (class-info-supers c))
        (push c (class-info-subclasses s)))))
  (let ((root (find "<object>" classes :key #'class-info-name :test #'string=))
        (next 0))
    (unless root (error "no <object> class"))
    (labels ((number-tree (c)
               (setf (class-info-id c) next)
               (incf next)
               ;; The classes whose first superclass is C.
               (dolist (sub (class-info-subclasses c))
                 (when (eq (first (class-info-supers sub)) c)
                   (number-tree sub)))
               (setf (class-info-subtree-end c) (1- next))))
      (number-tree root))
    (dolist (c classes)
      (unless (class-info-id c)
        (error "~a is not a subclass of <object>" (class-info-name c))))
    (dolist (c classes)
      (setf (class-info-range-ok-p c)
            (every (lambda (d)
                     (or (not (member c (class-info-cpl d)))
                         (<= (class-info-id c) (class-info-id d)
                             (class-info-subtree-end c))))
                   classes)))
    (sort (copy-list classes) #'< :key #'class-info-id)))

(defun find-class-info (name classes)
  (find name classes :key #'class-info-name :test #'string=))

(defun subclass-info-p (c1 c2)
  "Is C1 the class C2 or a subclass of it?"
  (and (member c2 (class-info-cpl c1)) t))
