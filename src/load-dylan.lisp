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
;;;; Load the whole system: the compiler, the runtime, and the interactive
;;;; read-eval-print loop.
;;;;         (was Thomas's src/load-thomas.scm, load-compiler.scm and
;;;;              load-runtime.scm, plus the kits' loaders)
;;;;
;;;;     (load "load-dylan")      everything
;;;;     (load "load-compiler")   just the Dylan -> Lisp compiler
;;;;     (load "load-runtime")    just the runtime
;;;;
;;;; Then:
;;;;     (dylan:rep)                        start an interactive loop
;;;;     (dylan:empty-environment!)         forget its module variables
;;;;     (dylan:compile-to-file <file> . forms)  compile forms to a Lisp file
;;;;     (dylan:compile-file <in> <out>)       compile a file of Dylan

(in-package #:cl-user)

(defparameter *dylan-directory*
  (or *load-truename* *default-pathname-defaults*))

(defparameter *dylan-common-files*
  '("package" "globals" "compat" "support"))

(defparameter *dylan-compiler-files*
  '("comp-util" "comp-module" "compiler" "comp-method" "comp-class" "comp-sf" "comp-exc"))

(defparameter *dylan-runtime-files*
  '("class"                             ; Dylan Class data type.
    "generic"                           ; Generic dispatch mechanism.
    "class-structure"                   ; The heterarchy.
    "runtime-top"                       ; Common parameter lists, etc.
    "runtime-internal"                  ; Procedures called by expansion of
                                        ; code, not by users
    "runtime-methods"                   ; Predefined methods
    "runtime-functions"                 ; Predefined functions and generic
                                        ; functions
    "runtime-extensions"                ; The CRL language extensions
    "runtime-bitstrings"                ; Integers as bitstrings
    "runtime-collections"               ; Collections, sequences, ...
    "runtime-collections-iterate"       ; Iteration + Collection Keys
    "runtime-collections-generic1"      ; Collection generic functions
    "runtime-collections-generic2"      ; More generic collection operations
    "runtime-collections-array"         ; Arrays
    "runtime-collections-deque"         ; Deques
    "runtime-collections-list"          ; Lists
    "runtime-collections-range"         ; Ranges
    "runtime-collections-string"        ; Strings
    "runtime-collections-table"         ; Tables
    "runtime-collections-vector"        ; Vectors
    "runtime-exceptions"                ; Exceptions
    "runtime-fast-paths"))              ; Inline arithmetic and element

(defparameter *dylan-fasl-directory*
  (merge-pathnames
   (make-pathname :directory
                  (list :relative "fasl"
                        (substitute-if #\- (lambda (c)
                                             (not (alphanumericp c)))
                                       (format nil "~a-~a"
                                               (lisp-implementation-type)
                                               (lisp-implementation-version)))))
   *dylan-directory*)
  "Where the compiled files go: one directory for each Lisp and version,
since their fasls differ.")

(defun dylan-load (names)
  "Load the named files, compiling each to a fasl first when the fasl is
missing or not newer than its source.  Once one file is compiled, so are all
after it, since they may use its macros and inline functions.  Nearly every
file refers to things that later files define, so the style warnings that
produces are muffled."
  (let ((stale nil))
    ;; One compilation unit, so that a function a later file defines is
    ;; not reported as undefined; and no optimization notes.
    (with-compilation-unit ()
      (handler-bind ((style-warning #'muffle-warning)
                     #+sbcl (sb-ext:compiler-note #'muffle-warning))
        (dolist (name names)
          (let ((source (merge-pathnames (concatenate 'string name ".lisp")
                                         *dylan-directory*))
                (fasl (merge-pathnames (make-pathname :name name
                                                      :type "fasl")
                                       *dylan-fasl-directory*)))
            ;; File dates are to the second, so a source written in the
            ;; same second as its fasl may be newer than it.
            (when (or stale
                      (not (probe-file fasl))
                      (>= (file-write-date source) (file-write-date fasl)))
              (setf stale t)
              (ensure-directories-exist fasl)
              (let ((*compile-verbose* nil) (*compile-print* nil))
                (compile-file source :output-file fasl)))
            (load fasl)))))))

(dylan-load (append *dylan-common-files*
                    *dylan-compiler-files*
                    *dylan-runtime-files*
                    '("rep")))

(format t "~&;Dylan loaded.  (dylan:rep) starts a read-eval-print loop.~%")
