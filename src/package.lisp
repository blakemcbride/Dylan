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
;;;; Package definition.
;;;;
;;;; This is a Common Lisp implementation of the language described in
;;;; "Dylan(TM) an object-oriented dynamic language" (Apple Computer
;;;; Eastern Research and Technology, April 1992).  It is a translation of
;;;; Thomas, the Scheme implementation Digital Equipment Corporation's
;;;; Cambridge Research Lab wrote against that book; the file correspondence
;;;; and the history are in the README.  Every file here says which file of
;;;; Thomas it came from.
;;;;
;;;; Naming conventions carried over from Thomas:
;;;;
;;;;   dylan/X   was  dylan:X    -- names visible to compiled Dylan code.
;;;;                              These are VARIABLES holding Dylan-callable
;;;;                              procedures; they expect the two extra
;;;;                              leading arguments (multiple-values and
;;;;                              next-method).
;;;;   dylan%X   was  dylan::X   -- names used by the runtime library and by
;;;;                              the output of the compiler, but not
;;;;                              directly available from Dylan.  These use
;;;;                              ordinary Lisp calling conventions.
;;;;
;;;; A colon cannot appear in a Common Lisp symbol without escaping, so the
;;;; Dylan-visible names take a slash and the internal ones a percent sign.
;;;; (Dylan's division operator therefore lives in the variable DYLAN//.)

(defpackage #:dylan
  (:use #:common-lisp)
  ;; Names this system defines with meanings incompatible with the Common
  ;; Lisp ones.  COMPILE-FILE is among them: DYLAN:COMPILE-FILE compiles a
  ;; file of Dylan into a file of Lisp.
  (:shadow #:add-method
           #:adjoin
           #:compile-file
           #:find-method
           #:last
           #:reduce
           #:set-difference
           #:union)
  (:export #:rep                        ; the interactive loop
           #:empty-environment!
           #:in-module                  ; the module input is compiled in
           #:compile-file               ; a file of Dylan -> a file of Lisp
           #:compile-to-file            ; Dylan forms -> a file of Lisp
           #:compile-expression         ; one form -> Lisp, in memory
           #:test                       ; run a file of examples
           #:test-examples))

(in-package #:dylan)

;;; SBCL (and friends) will otherwise complain endlessly while the system
;;; is being loaded, because nearly every file refers to variables and
;;; functions that later files define.
(declaim (optimize (speed 1) (safety 3) (debug 2)))
