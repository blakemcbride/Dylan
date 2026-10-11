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
;;;; Support for handling integers as bit strings
;;;;                              (was Thomas's src/runtime-bitstrings.scm)

(in-package #:dylan)

;;; Chapter 14 of the manual defines these "as in Common Lisp", so they are
;;; Common Lisp's own operations on two's-complement integers.  (The Scheme
;;; simulated them on lists of bits, and answered 0 for any result of the
;;; form -2^k; it also accepted only two arguments to logxor and logeqv,
;;; which take any number.)

(defun integers-only (name integers)
  (dolist (n integers integers)
    (unless (integerp n)
      (dylan-call dylan/error
                  (concatenate 'string name " -- not an integer") n))))

(defun logical-op-only-rest-args (name fn)
  (lambda (&rest integers)
    (apply fn (integers-only name integers))))

;;;
;;; DYLAN FUNCTIONS
;;;

(defparameter dylan/ash (dylan%generic-fn 'ash two-integers #'ash))

(defparameter dylan/logand
  (dylan%generic-fn 'logand only-rest-args
                    (logical-op-only-rest-args "logand" #'logand)))

(defparameter dylan/logandc1
  (dylan%generic-fn 'logandc1 two-integers #'logandc1))

(defparameter dylan/logandc2
  (dylan%generic-fn 'logandc2 two-integers #'logandc2))

(defparameter dylan/logbit?
  ;; Bit INDEX of INTEGER, 0 being the low bit, in two's complement.
  (dylan%generic-fn
   'logbit? two-integers
   (lambda (index integer)
     (when (minusp index)
       (dylan-call dylan/error "logbit? -- negative index" index integer))
     (logbitp index integer))))

(defparameter dylan/logeqv
  (dylan%generic-fn 'logeqv only-rest-args
                    (logical-op-only-rest-args "logeqv" #'logeqv)))

(defparameter dylan/logior
  (dylan%generic-fn 'logior only-rest-args
                    (logical-op-only-rest-args "logior" #'logior)))

(defparameter dylan/lognand
  (dylan%generic-fn 'lognand two-integers #'lognand))

(defparameter dylan/lognor
  (dylan%generic-fn 'lognor two-integers #'lognor))

(defparameter dylan/lognot
  (dylan%generic-fn 'lognot one-integer #'lognot))

(defparameter dylan/logorc1
  (dylan%generic-fn 'logorc1 two-integers #'logorc1))

(defparameter dylan/logorc2
  (dylan%generic-fn 'logorc2 two-integers #'logorc2))

(defparameter dylan/logxor
  (dylan%generic-fn 'logxor only-rest-args
                    (logical-op-only-rest-args "logxor" #'logxor)))
