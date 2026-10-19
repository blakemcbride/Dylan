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
;;;; Global variables of the Dylan runtime.
;;;;
;;;; Every file of the runtime refers to variables that later files define
;;;; -- the class objects of class-structure.lisp and the Dylan-callable
;;;; procedures spread over runtime-*.lisp -- so they are all proclaimed
;;;; special here, up front.  In the Scheme these were simply top-level
;;;; DEFINEs, resolved at call time.
;;;;
;;;; dylan/X was spelled dylan:X in the Scheme sources.

(in-package #:dylan)

;;;; Compiler tables (comp-util.lisp, compiler.lisp, comp-method.lisp)

(declaim (special dylan%lisp-names-of-predefined-names
                  dylan%predefined-names
                  dylan%predefined-variables
                  compilation-functions
                  compiled-sharp-f
                  parse-next
                  parse-rest))

;;;; The classes of the heterarchy (class-structure.lisp)

(declaim (special
          <abort>
          <array>
          <byte-string>
          <character>
          <class>
          <collection>
          <complex>
          <condition>
          <deque>
          <double-float>
          <empty-list>
          <error>
          <explicit-key-collection>
          <extended-float>
          <float>
          <function>
          <generic-function>
          <integer>
          <keyword>
          <list>
          <method>
          <mutable-collection>
          <mutable-explicit-key-collection>
          <mutable-sequence>
          <number>
          <object>
          <pair>
          <range>
          <ratio>
          <rational>
          <real>
          <rectangular-complex>
          <restart>
          <sequence>
          <serious-condition>
          <simple-error>
          <simple-object-vector>
          <simple-restart>
          <simple-warning>
          <single-float>
          <singleton>
          <slot-descriptor>
          <stretchy-vector>
          <stream>
          <string>
          <string-stream>
          <symbol>
          <table>
          <type-error>
          <unicode-string>
          <vector>
          <warning>
          ))

;;;; The Dylan-visible runtime procedures (runtime-*.lisp)

(declaim (special
          dylan/*
          dylan/+
          dylan/-
          dylan//
          dylan//=
          dylan/<
          dylan/<=
          dylan/=
          dylan/=hash
          dylan/>
          dylan/>=
          dylan/abort
          dylan/abs
          dylan/acos
          dylan/acosh
          dylan/add
          dylan/add!
          dylan/add-method
          dylan/add-new
          dylan/add-new!
          dylan/add-slot
          dylan/all-superclasses
          dylan/always
          dylan/angle
          dylan/any?
          dylan/append
          dylan/applicable-method?
          dylan/apply
          dylan/aref
          dylan/as
          dylan/as-lowercase
          dylan/as-lowercase!
          dylan/as-uppercase
          dylan/as-uppercase!
          dylan/ash
          dylan/asin
          dylan/asinh
          dylan/atan
          dylan/atan2
          dylan/atanh
          dylan/binary*
          dylan/binary+
          dylan/binary-
          dylan/binary-gcd
          dylan/binary-lcm
          dylan/binary/
          dylan/binary<
          dylan/binary=
          dylan/break
          dylan/caaar
          dylan/caadr
          dylan/caar
          dylan/cadar
          dylan/caddr
          dylan/cadr
          dylan/car
          dylan/cdaar
          dylan/cdadr
          dylan/cdar
          dylan/cddar
          dylan/cdddr
          dylan/cddr
          dylan/cdr
          dylan/ceiling
          dylan/ceiling/
          dylan/cerror
          dylan/check-type
          dylan/choose
          dylan/choose-by
          dylan/class-for-copy
          dylan/complement
          dylan/compose
          dylan/concatenate
          dylan/concatenate-as
          dylan/condition-format-arguments
          dylan/condition-format-string
          dylan/conjoin
          dylan/cons
          dylan/copy-sequence
          dylan/copy-state
          dylan/cos
          dylan/cosh
          dylan/current-element
          dylan/current-key
          dylan/curry
          dylan/default-handler
          dylan/denominator
          dylan/dimensions
          dylan/direct-subclasses
          dylan/direct-superclasses
          dylan/disjoin
          dylan/display
          dylan/do
          dylan/do-handlers
          dylan/element
          dylan/empty?
          dylan/error
          dylan/even?
          dylan/every?
          dylan/exp
          dylan/expt
          dylan/fill!
          dylan/final-state
          dylan/find-key
          dylan/find-method
          dylan/find-pair
          dylan/first
          dylan/floor
          dylan/floor/
          dylan/freeze-methods
          dylan/function-arguments
          dylan/gcd
          dylan/generic-function-methods
          dylan/get-array-dimensions
          dylan/get-array-value
          dylan/get-deque-front
          dylan/get-deque-last
          dylan/get-hash-table
          dylan/get-range-count
          dylan/get-range-end
          dylan/get-range-start
          dylan/get-range-step
          dylan/get-state
          dylan/id?
          dylan/identity
          dylan/imag-part
          dylan/init-function
          dylan/init-keyword
          dylan/init-value
          dylan/initial-state
          dylan/initialize
          dylan/instance?
          dylan/integral?
          dylan/intersection
          dylan/key-sequence
          dylan/last
          dylan/lcm
          dylan/list
          dylan/list*
          dylan/log
          dylan/logand
          dylan/logandc1
          dylan/logandc2
          dylan/logbit?
          dylan/logeqv
          dylan/logior
          dylan/lognand
          dylan/lognor
          dylan/lognot
          dylan/logorc1
          dylan/logorc2
          dylan/logxor
          dylan/magnitude
          dylan/make
          dylan/make-polar
          dylan/make-read-only
          dylan/make-rectangular
          dylan/map
          dylan/map-as
          dylan/map-into
          dylan/max
          dylan/member?
          dylan/method-specializers
          dylan/min
          dylan/module-get/
          dylan/module-set/
          dylan/modulo
          dylan/negative?
          dylan/newline
          dylan/next-state
          dylan/not
          dylan/numerator
          dylan/object-class
          dylan/odd?
          dylan/pop
          dylan/pop-last
          dylan/positive?
          dylan/previous-state
          dylan/print
          dylan/print-object
          dylan/push
          dylan/push-last
          dylan/range
          dylan/rationalize
          dylan/rcurry
          dylan/real-part
          dylan/reduce
          dylan/reduce1
          dylan/remainder
          dylan/remove
          dylan/remove!
          dylan/remove-duplicates
          dylan/remove-duplicates!
          dylan/remove-key!
          dylan/remove-method
          dylan/remove-slot
          dylan/replace-elements!
          dylan/replace-subsequence!
          dylan/restart-query
          dylan/return-allowed?
          dylan/return-description
          dylan/return-query
          dylan/reverse
          dylan/reverse!
          dylan/round
          dylan/round/
          dylan/scheme-safe/
          dylan/seal
          dylan/second
          dylan/set-array-dimensions!
          dylan/set-array-value!
          dylan/set-deque-front!
          dylan/set-deque-last!
          dylan/set-hash-table!
          dylan/set-range-count!
          dylan/set-range-end!
          dylan/set-range-start!
          dylan/set-range-step!
          dylan/setter/
          dylan/setter/aref/
          dylan/setter/caaar/
          dylan/setter/caadr/
          dylan/setter/caar/
          dylan/setter/cadar/
          dylan/setter/caddr/
          dylan/setter/cadr/
          dylan/setter/car/
          dylan/setter/cdaar/
          dylan/setter/cdadr/
          dylan/setter/cdar/
          dylan/setter/cddar/
          dylan/setter/cdddr/
          dylan/setter/cddr/
          dylan/setter/cdr/
          dylan/setter/current-element/
          dylan/setter/direct-superclasses/
          dylan/setter/element/
          dylan/setter/first/
          dylan/setter/second/
          dylan/setter/slot-value/
          dylan/setter/third/
          dylan/setter/last/
          dylan/setter/size/
          dylan/shallow-copy
          dylan/signal
          dylan/sin
          dylan/singleton
          dylan/sinh
          dylan/size
          dylan/slot-allocation
          dylan/slot-descriptor
          dylan/slot-descriptors
          dylan/slot-getter
          dylan/slot-initialized?
          dylan/slot-setter
          dylan/slot-type
          dylan/slot-value
          dylan/sort
          dylan/sort!
          dylan/sorted-applicable-methods
          dylan/sqrt
          dylan/subclass?
          dylan/subsequence-position
          dylan/tan
          dylan/tanh
          dylan/third
          dylan/truncate
          dylan/truncate/
          dylan/type-error-expected-type
          dylan/type-error-value
          dylan/unary-
          dylan/unary/
          dylan/union
          dylan/values
          dylan/vector
          dylan/write
          dylan/write-line
          dylan/format
          dylan/stream-contents
          dylan/*standard-output*
          dylan/*standard-error*
          dylan/zero?
          ))
