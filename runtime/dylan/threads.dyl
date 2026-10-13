module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Threads: an extension, since the 1992 manual has none
;;; (CompilerPlan.md, Phase 7).
;;;
;;;   (make-thread function)      start a thread calling FUNCTION with no
;;;                               arguments; answer the <thread>
;;;   (join-thread thread)        wait for it to end; answer the
;;;                               function's values
;;;   (current-thread)  (thread-yield)
;;;   (make <lock>)  (make <recursive-lock>)
;;;   (acquire-lock lock)  (release-lock lock)  (with-lock (lock) body ...)
;;;   (make <condition-variable>)
;;;   (wait-condition-variable cv lock)  (notify-condition-variable cv)
;;;   (notify-all-condition-variable cv)
;;;   (make <atomic-integer> value: n)
;;;   (atomic-value a)  (atomic-add! a n)  (atomic-compare-and-swap! a old new)
;;;   (define-thread-variable name value)  (dynamic-bind ((name value)) body ...)
;;;
;;; A serious condition a thread does not handle ends the thread, not the
;;; program; join-thread then signals a <thread-error> holding it.  The
;;; program ends when its top-level forms are done, whatever its other
;;; threads are doing: join those it must wait for.

(define-class <thread-error> (<error>)
  (getter: thread-error-thread setter: #f init-keyword: thread:)
  (getter: thread-error-condition setter: #f init-keyword: condition:))

(define-method %condition-message ((c <thread-error>))
  (%format-message "a thread ended with an unhandled condition: ~a"
                   (list (%condition-message (thread-error-condition c)))))

(define make-thread
  (method (function)
    (%make-thread function)))

(define join-thread
  (method (thread)
    (bind ((results (%join-thread thread))
           (condition (%thread-condition thread)))
      (if condition
          (error (make <thread-error> thread: thread condition: condition))
          (apply values results)))))
