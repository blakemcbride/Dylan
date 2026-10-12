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
;;;; Generic Operation Dispatch Mechanism
;;;;                                         (was Thomas's src/generic.scm)
;;;;
;;;; The Scheme used MIT Scheme 1d-tables to implement the method ->
;;;; param-list and generic-function -> data lookups; weak EQ hash tables
;;;; do the same job here (see MAKE-ONED-TABLE in compat.lisp).

(in-package #:dylan)

;;;; Methods

(defstruct (param-list
            (:conc-name param-list.)
            (:predicate param-list?)
            (:print-object print-opaque-object)
            (:constructor %make-param-list (nrequired specializers next?
                                            rest? keys)))
  nrequired specializers next? rest? keys)

(defun make-param-list (required next? rest? keys)
  (let ((required (guarantee-symbol-and-specializer-list required)))
    (%make-param-list (length required)
                      (mapcar #'cadr required)
                      (guarantee-boolean next?)
                      (guarantee-boolean rest?)
                      (guarantee-keys keys))))

(defun guarantee-symbol-and-specializer-list (original-list)
  (if (all? (lambda (elt)
              (and (consp elt) (consp (cdr elt)) (null (cddr elt))
                   (symbolp (car elt))
                   (or (class? (cadr elt)) (singleton? (cadr elt)))))
            original-list)
      original-list
      (guarantee-symbol-and-specializer-list
       (dylan-call dylan/error "invalid param-list" original-list))))

(defun guarantee-keys (keys)
  (cond ((not keys) nil)
        ((not (consp keys)) (vector))   ; "any keywords are acceptable"
        ((all? #'keyword? keys) keys)
        (t (guarantee-keys
            (dylan-call dylan/error "invalid keywords" keys)))))

(defun guarantee-boolean (bool)
  (if bool t nil))

(defun guarantee-integer (object)
  (if (integerp object)
      object
      (guarantee-integer (dylan-call dylan/error "not an integer" object))))

(defvar *method-data* (make-oned-table))

(defvar *last-function-type* (cons nil nil)
  "The last function GET-TYPE was asked about, and its class (see
FUNCTION-TYPE in runtime-top.lisp).")

(defun method-data (method)
  (let ((data (oned-table/get *method-data* method nil)))
    (if (not data)
        (dylan-call dylan/error "not a method" method)
        data)))

(defun method.param-list (method) (method-data method))
(defun method.specializers (method) (param-list.specializers (method-data method)))
(defun method.nrequired (method) (param-list.nrequired (method-data method)))
(defun method.rest? (method) (param-list.rest? (method-data method)))
(defun method.keys (method) (param-list.keys (method-data method)))

(defun dylan%method? (method)
  (and (oned-table/get *method-data* method nil) t))

(defun dylan%make-method (param-list method)
  (oned-table/put! *method-data* method (guarantee-param-list param-list))
  (setf *last-function-type* (cons nil nil)) ; see FUNCTION-TYPE
  method)

(defvar *dispatch-entries* (make-oned-table)
  "For a compiled method, its LAMBDA without the specializer check, which
generic dispatch, having checked the arguments, calls instead.")

(defun dispatch-entry (method)
  (oned-table/get *dispatch-entries* method method))

(defun dylan%make-compiled-method (param-list lambda)
  "The method of a Dylan METHOD form.  Chapter 6: a method can be called
only with arguments that match its specializers.  Generic dispatch has
checked them; a direct call (next-method #f) is checked here."
  (let ((specializers (param-list.specializers param-list))
        (method nil))
    ;; With no specializer but <object>, there is nothing to check.
    (when (every (lambda (specializer) (eq specializer <object>))
                 specializers)
      (return-from dylan%make-compiled-method
        (dylan%make-method param-list lambda)))
    (setf method
          (lambda (multiple-values next-method &rest arguments)
            (unless (or (eq next-method next-method/last-applicable)
                        (functionp next-method))
              (loop for specializer in specializers
                    for rest on arguments
                    unless (match-specializer? (car rest) specializer)
                    do (dylan-call dylan/error
                                   "method called with an argument that does not match its specializer"
                                   (car rest) specializer method)))
            (apply lambda multiple-values next-method arguments)))
    (oned-table/put! *dispatch-entries* method lambda)
    (dylan%make-method param-list method)))

(defun guarantee-param-list (param-list)
  (if (param-list? param-list)
      param-list
      (guarantee-param-list
       (dylan-call dylan/error "not a param-list" param-list))))

;;;; Generic Functions

(defstruct (generic-function-data
            (:conc-name generic-function-data.)
            (:print-object print-opaque-object)
            (:constructor make-generic-function-data
                (name nrequired keys rest? methods read-only?)))
  name nrequired keys rest? methods read-only?
  (frozen '())                          ; specializer lists FREEZE-METHODS gave
  (cache nil)                           ; see CACHED-APPLICABLE-METHODS
  (cache-epoch 0)
  (cache-singletons nil)
  (fast-paths '()))                     ; see CHECK-FAST-PATH

(defvar *generic-function-data* (make-oned-table))

(defun generic-function-data (fn)
  (let ((data (oned-table/get *generic-function-data* fn nil)))
    (if (not data)
        (dylan-call dylan/error "not a generic function" fn)
        data)))

(defun generic-function.name (generic-function)
  (generic-function-data.name (generic-function-data generic-function)))

(defun generic-function.nrequired (generic-function)
  (generic-function-data.nrequired (generic-function-data generic-function)))

(defun generic-function.keys (generic-function)
  (generic-function-data.keys (generic-function-data generic-function)))

(defun generic-function.rest? (generic-function)
  (generic-function-data.rest? (generic-function-data generic-function)))

(defun generic-function.methods (generic-function)
  (generic-function-data.methods (generic-function-data generic-function)))

(defun generic-function.read-only? (generic-function)
  (generic-function-data.read-only? (generic-function-data generic-function)))

(defun set-generic-function.read-only?! (generic-function read-only?)
  (setf (generic-function-data.read-only?
         (generic-function-data generic-function))
        (if read-only? t nil)))

(defun find-method (generic-function specializers)
  (dolist (method (generic-function.methods generic-function) nil)
    (when (specializers=? specializers (method.specializers method))
      (return method))))

(defun specializer-within? (specializer frozen)
  "Does SPECIALIZER match FROZEN or is it more specific (chapter 10)?"
  (if (singleton? frozen)
      (and (singleton? specializer)
           (id? (singleton.object specializer) (singleton.object frozen)))
      (if (singleton? specializer)
          (subclass? (get-type (singleton.object specializer)) frozen)
          (subclass? specializer frozen))))

(defun check-not-frozen (generic-function method operation)
  (dolist (frozen (generic-function-data.frozen
                   (generic-function-data generic-function)))
    (when (every #'specializer-within? (method.specializers method) frozen)
      (dylan-call dylan/error
                  (concatenate 'string operation
                               " -- the methods are frozen (freeze-methods)")
                  generic-function method))))

(defun delete-method! (generic-function method)
  (check-not-frozen generic-function method "remove-method")
  (let* ((data (generic-function-data generic-function))
         (pair (member method (generic-function-data.methods data))))
    (if (not pair)
        (dylan-call dylan/error "method not in generic function"
                    method generic-function)
        (setf (generic-function-data.methods data)
              (delete-pair! pair (generic-function-data.methods data))))
    (setf (generic-function-data.cache data) nil)
    (check-fast-path data method)
    method))

(defun delete-pair! (pair list)
  "Splice the cons PAIR out of LIST, destructively."
  (if (eq pair list)
      (cdr list)
      (loop for pairs on list
            when (eq pair (cdr pairs))
              do (setf (cdr pairs) (cddr pairs))
                 (return list)
            finally (return list))))

(defun add-method (generic-function method &rest multi-value-receiver)
  (let ((data (generic-function-data generic-function))
        (param-list (method.param-list method)))
    (flet ((congruency-error ()
             (cond
               ((not (= (param-list.nrequired param-list)
                        (generic-function-data.nrequired data)))
                "required argument count mismatch")
               ((and (not (or (generic-function-data.rest? data)
                              (generic-function-data.keys data)))
                     (or (param-list.rest? param-list)
                         (param-list.keys param-list)))
                "generic function doesn't allow rest/keys")
               ((and (or (generic-function-data.rest? data)
                         (generic-function-data.keys data))
                     (not (or (param-list.rest? param-list)
                              (param-list.keys param-list))))
                "generic function requires rest/keys")
               (t nil))))
      (cond ((generic-function-data.read-only? data)
             (dylan-call dylan/error
                         "add-method -- generic function is read-only"
                         generic-function))
            ((and (consp (generic-function-data.keys data))
                  (not (let ((method-keys (param-list.keys param-list)))
                         (or (param-list.rest? param-list)
                             (and method-keys (not (consp method-keys)))
                             (subset? (generic-function-data.keys data)
                                      method-keys)))))
             (dylan-call dylan/error
                         "add-method -- generic function requires certain keys"
                         (generic-function-data.keys data)))
            (t (let ((specific-error (congruency-error)))
                 (when specific-error
                   (dylan-call dylan/error
                               (concatenate 'string
                                            "add-method -- parameter lists not congruent, "
                                            specific-error)
                               generic-function method))))))
    (check-not-frozen generic-function method "add-method")
    (let ((old-method (find-method generic-function
                                   (method.specializers method))))
      (when old-method
        (delete-method! generic-function old-method))
      (push method (generic-function-data.methods data))
      (setf (generic-function-data.cache data) nil)
      (check-fast-path data method)
      (if multi-value-receiver
          (funcall (car multi-value-receiver) method old-method)
          old-method))))

(defun dylan%generic-function? (obj)
  (and (oned-table/get *generic-function-data* obj nil) t))

(defvar no-argument (make-symbol "NO-ARGUMENT"))

(defmacro cached-entry (data &rest arguments)
  "When the cache knows the methods for ARGUMENTS (one for each required
argument) and just one applies, what to call for it; else NIL."
  (let ((table (gensym "TABLE")) (singletons (gensym "SINGLETONS")))
    `(let* ((,table (dispatch-cache ,data))
            (,singletons (generic-function-data.cache-singletons ,data)))
       (declare (ignorable ,singletons))
       (and ,@(loop for (argument . more) on arguments
                    for i from 0
                    collect `(setf ,table
                                   (gethash (dispatch-key ,singletons ,i
                                                          ,argument)
                                            ,table))
                    when more collect `(hash-table-p ,table))
            (null (cdar ,table))
            (caar ,table)))))

(defun dylan%create-generic-function (name nrequired keys rest?)
  (let ((data (make-generic-function-data name
                                          (guarantee-integer nrequired)
                                          (guarantee-keys keys)
                                          (guarantee-boolean rest?)
                                          '() nil))
        (generic-function nil))
    ;; With one or two required arguments, a call with just those whose
    ;; one applicable method the cache knows goes straight to it, without
    ;; the argument list GENERIC-DISPATCH works on.
    (setf generic-function
          (case nrequired
            (1 (lambda (multiple-values next-method
                        &optional (a no-argument) &rest more)
                 (declare (ignore next-method))
                 (let ((entry (and (null more) (not (eq a no-argument))
                                   (cached-entry data a))))
                   (if entry
                       (funcall entry multiple-values
                                next-method/last-applicable a)
                       (generic-dispatch multiple-values
                                         (if (eq a no-argument)
                                             '()
                                             (cons a more))
                                         generic-function data)))))
            (2 (lambda (multiple-values next-method
                        &optional (a no-argument) (b no-argument) &rest more)
                 (declare (ignore next-method))
                 (let ((entry (and (null more) (not (eq b no-argument))
                                   (cached-entry data a b))))
                   (if entry
                       (funcall entry multiple-values
                                next-method/last-applicable a b)
                       (generic-dispatch multiple-values
                                         (cond ((eq a no-argument) '())
                                               ((eq b no-argument) (list a))
                                               (t (list* a b more)))
                                         generic-function data)))))
            (t (lambda (&rest args)
                 (generic-dispatch (car args) (cddr args)
                                   generic-function data)))))
    (oned-table/put! *generic-function-data* generic-function data)
    (setf *last-function-type* (cons nil nil)) ; see FUNCTION-TYPE
    generic-function))

;;;; Generic Dispatch

(defun generic-dispatch (multiple-values original-args generic-function data)
  (let ((nreq (generic-function-data.nrequired data))
        (ngiven (length original-args)))
    (declare (fixnum nreq ngiven))
    (when (> nreq ngiven)
      (dylan-call dylan/error
                  "generic-dispatch -- too few arguments supplied"
                  nreq original-args))
    (let* ((applicable (cached-applicable-methods data original-args))
           (entries (car applicable))
           (applicable-methods (cadr applicable)))
      ;; The usual case: the required arguments only, and one method.
      (when (and (= ngiven nreq) entries (null (cdr entries)))
        (return-from generic-dispatch
          (apply (car entries) multiple-values next-method/last-applicable
                 original-args)))
      (unless (consp applicable-methods)
        (dylan-call dylan/error
                    "generic-dispatch -- no applicable methods"
                    generic-function original-args))
      (when (> ngiven nreq)             ; More supplied than required
        (let ((non-req-args (but-first nreq original-args)))
          (if (or (generic-function-data.keys data)
                  (generic-function-data.rest? data))
              (progn
                ;; Chapter 6: when the generic function takes #key, the
                ;; arguments after the required ones are keyword/value
                ;; pairs, whatever its methods take.
                (when (generic-function-data.keys data)
                  (validate-keywords non-req-args t
                                     (lambda (&rest args)
                                       (dylan-apply dylan/error args))))
                (check-handled-keywords non-req-args (cddr applicable)))
              (dylan-call dylan/error
                          "generic-dispatch -- too many arguments supplied"
                          generic-function nreq original-args))))
      (labels ((next-method-loop (remaining-methods multiple-values current-args)
                 (if (null (cdr remaining-methods))
                     (apply (car remaining-methods)
                            multiple-values next-method/last-applicable
                            current-args)
                     (apply (car remaining-methods)
                            multiple-values
                            (lambda (multiple-values next-method &rest these-args)
                              (declare (ignore next-method))
                              (next-method-loop (cdr remaining-methods)
                                                multiple-values
                                                (if (null these-args)
                                                    current-args
                                                    these-args)))
                            current-args))))
        (next-method-loop entries multiple-values original-args)))))

(defun handled-keywords (methods)
  "What CHECK-HANDLED-KEYWORDS allows for a call to which METHODS apply:
:ANY, T for keyword/value pairs with any keywords, or a list of keywords."
  ;;   gather the keywords for all of the applicable methods
  ;;   if ALL methods specify !rest without !key then the call is allowable
  ;;   if ANY method specifies !rest (or !key with no specific keys), then
  ;;      the call is allowable provided the extra arguments are in
  ;;      keyword/value format
  ;;   otherwise all of the keywords passed must be accepted
  ;; Note that the Scheme walked the method list with a named LET whose
  ;; recursive call appeared in only one of the COND branches: on meeting a
  ;; method that takes !rest or unrestricted !key it simply stopped, never
  ;; reaching the validation below.  Fixed: every method is now examined.
  (let ((all-!rest? t)
        (any-!key? nil)
        (keywords '()))
    (loop
      (if (consp methods)
          (let* ((param-list (method.param-list (car methods)))
                 (keys (param-list.keys param-list))
                 (rest? (param-list.rest? param-list)))
            (when (or (not rest?) keys) (setf all-!rest? nil))
            (cond ((or rest? (and keys (not (consp keys))))
                   (setf any-!key? t))
                  ((consp keys)
                   (setf keywords (append keys keywords))))
            (pop methods))
          (return (cond (all-!rest? :any)
                        (any-!key? t)
                        (t keywords)))))))

(defun check-handled-keywords (non-req-args handled-keywords)
  (if (eq handled-keywords :any)
      'ok
      (dylan%keyword-validate t non-req-args handled-keywords)))

;;;; Fast paths
;;;;
;;;; Calls of a few predefined functions on Lisp numbers and simple vectors
;;;; are compiled inline (see DYLAN%CALL in runtime-internal.lisp), and
;;;; FOR-EACH over a simple vector or a list is done directly (see
;;;; DYLAN%FOR-EACH there).  Each fast path is guarded by a variable that
;;;; stays true only while the generic functions behind it have no method
;;;; but the predefined ones that could apply to its arguments.  A generic
;;;; function's FAST-PATHS are entries (GUARD CLASSES), where CLASSES holds,
;;;; for each leading required argument, the classes of the arguments that
;;;; fast path handles.

(defun method-touches-fast-path? (method fast-path-classes)
  (loop for specializer in (method.specializers method)
        for classes in fast-path-classes
        always (if (singleton? specializer)
                   (member (get-type (singleton.object specializer)) classes)
                   (some (lambda (class) (subclass? class specializer))
                         classes))))

(defun check-fast-path (data method)
  (loop for (guard classes) in (generic-function-data.fast-paths data)
        when (method-touches-fast-path? method classes)
          do (setf (symbol-value guard) nil)))

;;;; Caching applicable methods.
;;;;
;;;; Which methods apply to a call, and in what order, depends only on the
;;;; class of each required argument -- except where some method has a
;;;; singleton specializer for that argument, and the argument is one of
;;;; those singletons' objects.  So the sorted method list is cached in
;;;; nested EQ tables, one level for each required argument, keyed on its
;;;; class or else the index of the singleton object it is.  The cache is
;;;; dropped when a method is added or removed, and when any class's
;;;; precedence list changes (*CLASS-STRUCTURE-EPOCH*).

(defun singleton-objects-by-position (methods nrequired)
  "A vector holding, for each required argument, NIL or an EQL table
mapping each object that singleton specializers of METHODS name in that
position to an index of its own."
  (let ((objects (make-array nrequired :initial-element nil)))
    (dolist (method methods objects)
      (loop for specializer in (method.specializers method)
            for i from 0 below nrequired
            when (singleton? specializer)
              do (let ((table (or (aref objects i)
                                  (setf (aref objects i)
                                        (make-hash-table :test 'eql)))))
                   (unless (nth-value 1 (gethash (singleton.object specializer)
                                                 table))
                     (setf (gethash (singleton.object specializer) table)
                           (hash-table-count table))))))))

(declaim (inline dispatch-key))
(defun dispatch-key (singletons i argument)
  (let ((objects (svref singletons i)))
    (or (and objects (values (gethash argument objects)))
        (get-type argument))))

(defun dispatch-cache (data)
  "DATA's cache, emptied first if it is stale."
  (let ((table (generic-function-data.cache data)))
    (if (and table
             (eql (generic-function-data.cache-epoch data)
                  *class-structure-epoch*))
        table
        (setf (generic-function-data.cache-epoch data) *class-structure-epoch*
              (generic-function-data.cache-singletons data)
              (singleton-objects-by-position
               (generic-function-data.methods data)
               (generic-function-data.nrequired data))
              (generic-function-data.cache data)
              (make-hash-table :test 'eq)))))

(defun cached-applicable-methods (data arguments)
  "For ARGUMENTS, (ENTRIES METHODS . HANDLED-KEYWORDS): the applicable
methods, sorted, what to call for each (see DISPATCH-ENTRY), and what
HANDLED-KEYWORDS answers for them."
  (let ((table (dispatch-cache data)))
    (let ((singletons (generic-function-data.cache-singletons data))
          (last (1- (generic-function-data.nrequired data)))
          (key nil))
      ;; Descend a level for each required argument but the last, whose
      ;; key finds the methods.  With none required, the key is NIL.
      (loop for i from 0 below last
            for argument in arguments
            do (let ((k (dispatch-key singletons i argument)))
                 (setf table (or (gethash k table)
                                 (setf (gethash k table)
                                       (make-hash-table :test 'eq))))))
      (when (>= last 0)
        (setf key (dispatch-key singletons last (nth last arguments))))
      (multiple-value-bind (methods found) (gethash key table)
        (if found
            methods
            (setf (gethash key table)
                  (let ((methods (sorted-applicable-methods
                                  (generic-function-data.methods data)
                                  arguments)))
                    (list* (mapcar #'dispatch-entry methods)
                           methods
                           (handled-keywords methods)))))))))

;;;; Finding and sorting applicable methods.

(defun sorted-applicable-methods (methods arguments)
  (mapcar #'cdr                         ; Strip specificities.
          (scheme-sort
           (find-applicable-method-specificities methods arguments)
           (lambda (specificities/method-1 specificities/method-2)
             ;; Specificities are handled left-to-right through the list.
             (loop with specificities-1 = (car specificities/method-1)
                   with specificities-2 = (car specificities/method-2)
                   do (if (and (consp specificities-1) (consp specificities-2))
                          (let ((specificity-1 (car specificities-1))
                                (specificity-2 (car specificities-2)))
                            (cond ((eql specificity-1 specificity-2)
                                   (pop specificities-1) (pop specificities-2))
                                  ((eq t specificity-1) (return t))
                                  ((eq t specificity-2) (return nil))
                                  ((> specificity-1 specificity-2) (return t))
                                  (t (return nil))))
                          (return nil)))))))

(defun find-applicable-method-specificities (methods arguments)
  "Return a list of (specificities . method) for each applicable method in
METHODS.  SPECIFICITIES is a list containing the specificity of each
specializer of the method.  If there are no required arguments,
SPECIFICITIES is always the empty list.  If there are no applicable
methods, the return value is an empty list."
  (let ((specificities/method-pairs '()))
    (dolist (method methods specificities/method-pairs)
      (let ((specificities (method-applicable? method arguments)))
        (cond ((null specificities))
              ((eq t specificities)
               (push (cons '() method) specificities/method-pairs))
              (t (push (cons specificities method)
                       specificities/method-pairs)))))))

(defun method-applicable? (method arguments)
  "Return NIL if METHOD shouldn't be applied to ARGUMENTS.  Else, return a
list of the specificities of the specializers involved in the match.  If
this list would be empty because there are no required parameters, return T
instead."
  (loop with remaining-arguments = arguments
        with remaining-specializers = (method.specializers method)
        with specificities = '()
        do (if (consp remaining-specializers)
               (if (not (consp remaining-arguments))
                   (dylan-call dylan/error
                               "method-applicable? -- too few arguments"
                               arguments method)
                   (let ((specificity
                           (match-specializer? (car remaining-arguments)
                                               (car remaining-specializers))))
                     (if specificity
                         (progn (pop remaining-arguments)
                                (pop remaining-specializers)
                                (push specificity specificities))
                         (return nil))))
               ;; NOTE (from the Scheme): in MIT Scheme (EQ? #F '()) is
               ;; true, so an empty specificity list has to be reported as
               ;; T to be distinguishable from "not applicable".  Common
               ;; Lisp's NIL behaves the same way, so the test stays.
               (return (if (null specificities) t (reverse specificities))))))

(defun match-specializer? (object specializer)
  "Return NIL if OBJECT doesn't match SPECIALIZER.  Else, return the
specificity of the match.  A high specificity indicates a very specific
match.  A specificity of T indicates an exact match of a singleton.  A
class's specificity is measured by its position in the class precedence
list of OBJECT's class (the Scheme used the class's depth below <object>,
which disagrees with the precedence order under multiple inheritance)."
  (cond ((singleton? specializer)
         (and (id? object (singleton.object specializer)) t))
        ((class? specializer)
         (let* ((precedence-list (class-precedence-list (get-type object)))
                (position (position specializer precedence-list)))
           (and position (- (length precedence-list) position))))
        (t (dylan-call dylan/error
                       "match-specializer? -- weird specializer"
                       specializer))))

(defun specializers=? (specializers1 specializers2)
  "Return T when two lists of specializers contain the same specializer in
each position.  \"Same\" is EQ except for singletons, which are not
guaranteed to be unique for arbitrary objects (e.g. `3')."
  (loop with specs1 = specializers1
        with specs2 = specializers2
        do (if (and (consp specs1) (consp specs2))
               (let ((spec1 (car specs1))
                     (spec2 (car specs2)))
                 (if (or (and (singleton? spec1)
                              (singleton? spec2)
                              (id? (singleton.object spec1)
                                   (singleton.object spec2)))
                         (and (class? spec1)
                              (class? spec2)
                              (eq spec1 spec2)))
                     (progn (pop specs1) (pop specs2))
                     (return nil)))
               (return
                 (if (and (null specs1) (null specs2))
                     t
                     (dylan-call dylan/error
                                 "specializer list length mismatch"
                                 specializers1 specializers2))))))

;;;; Dylan Calling Conventions

(defun make-dylan-callable (lisp-operation &rest n-args)
  "Remove the incoming NEXT-METHOD and multiple-value arguments, so that a
standard Lisp procedure can be called from Dylan."
  (if (or (null n-args) (> (car n-args) 3) (minusp (car n-args)))
      (lambda (&rest args) (apply lisp-operation (cddr args)))
      (case (car n-args)
        ((0) (lambda (multi next)
               (declare (ignore multi next))
               (funcall lisp-operation)))
        ((1) (lambda (multi next a)
               (declare (ignore multi next))
               (funcall lisp-operation a)))
        ((2) (lambda (multi next a b)
               (declare (ignore multi next))
               (funcall lisp-operation a b)))
        ((3) (lambda (multi next a b c)
               (declare (ignore multi next))
               (funcall lisp-operation a b c))))))

(defun dylan-call (dylan-fn &rest args)
  "Fill in the missing multiple-values and next-method parameters.  Useful
if you do NOT want multiple values back."
  (apply dylan-fn nil next-method/not-generic args))

(defun dylan-full-call (dylan-fn multi-value next-method &rest args)
  "Use this ONLY if you must specify multi-value or next-method when
calling from Lisp to Dylan."
  (apply dylan-fn multi-value next-method args))

(defun dylan-mv-call (dylan-fn multi-value &rest args)
  "Use this ONLY if you must specify multi-value when calling from Lisp to
Dylan."
  (apply dylan-fn multi-value next-method/not-generic args))

(defun reformat-apply-args (args)
  (split-last args
              (lambda (early end)
                (append early (if (null end) '() (car end))))))

(defun dylan-apply (dylan-fn &rest args)
  (apply dylan-fn nil next-method/not-generic (reformat-apply-args args)))

(defun dylan-full-apply (dylan-fn multi-value next-method &rest args)
  (apply dylan-fn multi-value next-method (reformat-apply-args args)))

(defun dylan-mv-apply (dylan-fn multi-value &rest args)
  (apply dylan-fn multi-value next-method/not-generic
         (reformat-apply-args args)))

(defun dylan%function->method (param-list lisp-function)
  (dylan%make-method
   param-list
   (let ((nreq (param-list.nrequired param-list))
         (rest? (param-list.rest? param-list))
         (keys (param-list.keys param-list)))
     (make-dylan-callable lisp-function
                          (if (or rest? keys)
                              -1        ; Unknown number of arguments
                              nreq)))))

(defun dylan%dylan-callable->method (param-list dylan-callable)
  (dylan%make-method param-list dylan-callable))
