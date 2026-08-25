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
;;;; Build the class structure for the Dylan runtime environment.
;;;;                                 (was Thomas's src/class-structure.scm)

(in-package #:dylan)

(defparameter <object> (make-dylan-class '<object> '() '() t))

;;;; Number classes

;;; Abstract

(defparameter <number> (make-dylan-class '<number> (list <object>) '() nil))

;;; Sealed

(defparameter <complex> (make-dylan-class '<complex> (list <number>) '() nil))
(defparameter <real> (make-dylan-class '<real> (list <complex>) '() nil))
(defparameter <rectangular-complex>
  (make-dylan-class '<rectangular-complex> (list <complex>) '() nil))
(defparameter <rational> (make-dylan-class '<rational> (list <real>) '() nil))
(defparameter <integer> (make-dylan-class '<integer> (list <rational>) '() nil))
(defparameter <ratio> (make-dylan-class '<ratio> (list <rational>) '() nil))
(defparameter <float> (make-dylan-class '<float> (list <real>) '() nil))
(defparameter <single-float>
  (make-dylan-class '<single-float> (list <float>) '() nil))
(defparameter <double-float>
  (make-dylan-class '<double-float> (list <float>) '() nil))
(defparameter <extended-float>
  (make-dylan-class '<extended-float> (list <float>) '() nil))

;;;; Collections

;;; Abstract

(defparameter <collection>
  (make-dylan-class '<collection> (list <object>) '() nil))
(defparameter <explicit-key-collection>
  (make-dylan-class '<explicit-key-collection> (list <collection>) '() nil))
(defparameter <mutable-collection>
  (make-dylan-class '<mutable-collection> (list <collection>) '() nil))
(defparameter <sequence>
  (make-dylan-class '<sequence> (list <collection>) '() nil))
(defparameter <mutable-explicit-key-collection>
  (make-dylan-class '<mutable-explicit-key-collection>
                    (list <explicit-key-collection> <mutable-collection>)
                    '() nil))
(defparameter <mutable-sequence>
  (make-dylan-class '<mutable-sequence>
                    (list <mutable-collection> <sequence>) '() nil))

;;; Instantiable

(defparameter <array>
  (make-dylan-class '<array> (list <mutable-explicit-key-collection>) '() nil))
(defparameter <table>
  (make-dylan-class '<table> (list <mutable-explicit-key-collection>) '() nil))
(defparameter <vector>
  (make-dylan-class '<vector> (list <array> <mutable-sequence>) '() nil))
(defparameter <string>
  (make-dylan-class '<string> (list <mutable-sequence>) '() nil))
(defparameter <deque>
  (make-dylan-class '<deque> (list <mutable-sequence>) '() nil))
(defparameter <range>
  (make-dylan-class '<range> (list <sequence>) '() nil))
(defparameter <stretchy-vector>
  (make-dylan-class '<stretchy-vector> (list <vector>) '() nil))

;;; Sealed

(defparameter <simple-object-vector>
  (make-dylan-class '<simple-object-vector> (list <vector>) '() nil))
(defparameter <unicode-string>
  (make-dylan-class '<unicode-string> (list <vector> <string>) '() nil))
(defparameter <byte-string>
  (make-dylan-class '<byte-string> (list <vector> <string>) '() nil))
(defparameter <list>
  (make-dylan-class '<list> (list <mutable-sequence>) '() nil))
(defparameter <pair> (make-dylan-class '<pair> (list <list>) '() nil))
(defparameter <empty-list>
  (make-dylan-class '<empty-list> (list <list>) '() nil))

;;;; Conditions

(defparameter <condition>
  (make-dylan-class '<condition> (list <object>) '() nil))
(defparameter <serious-condition>
  (make-dylan-class '<serious-condition> (list <condition>) '() nil))
(defparameter <warning>
  (make-dylan-class '<warning> (list <condition>) '() nil))
(defparameter <restart>
  (make-dylan-class '<restart> (list <condition>) '() nil))
(defparameter <error>
  (make-dylan-class '<error> (list <serious-condition>) '() nil))
(defparameter <simple-error>
  (make-dylan-class '<simple-error> (list <error>) '() nil))
(defparameter <type-error>
  (make-dylan-class '<type-error> (list <error>) '() nil))
(defparameter <simple-warning>
  (make-dylan-class '<simple-warning> (list <warning>) '() nil))
(defparameter <simple-restart>
  (make-dylan-class '<simple-restart> (list <restart>) '() nil))
(defparameter <abort> (make-dylan-class '<abort> (list <restart>) '() nil))

;;;; Others

(defparameter <function>                ; Abstract
  (make-dylan-class '<function> (list <object>) '() nil))
(defparameter <generic-function>        ; Instantiable
  (make-dylan-class '<generic-function> (list <function>) '() nil))
(defparameter <method>                  ; Abstract
  (make-dylan-class '<method> (list <function>) '() nil))
(defparameter <class>                   ; Abstract
  (make-dylan-class '<class> (list <object>) '() nil))
(defparameter <slot-descriptor>         ; Abstract
  (make-dylan-class '<slot-descriptor> (list <object>) '() nil))
(defparameter <singleton>               ; Abstract
  (make-dylan-class '<singleton> (list <object>) '() nil))
(defparameter <symbol>                  ; Instantiable
  (make-dylan-class '<symbol> (list <object>) '() nil))
(defparameter <keyword>                 ; Instantiable
  (make-dylan-class '<keyword> (list <object>) '() nil))
(defparameter <character>               ; Instantiable
  (make-dylan-class '<character> (list <object>) '() nil))
