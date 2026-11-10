;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; Build bin/dylan, a stand-alone executable of the Common Lisp
;;;; implementation that starts in the read-eval-print loop:
;;;;
;;;;     sbcl --script src/build-dylan.lisp      (or: make)

(in-package #:cl-user)

;;; A control stack of 1 GB for each thread, the main one too, in place of
;;; SBCL's 2 MB, which ends a recursion some 15,000 calls deep: deep
;;; recursions run as deep as compiled ones.  And a heap of 8 GB at least,
;;; in place of SBCL's usual 1 GB, which a list of 40 million elements
;;; fills: large programs run as compiled ones do.  Only the part used of
;;; either is memory; the rest is address space.  The executable keeps the
;;; sizes of the SBCL that builds it, so the build runs again in one
;;; started with them.
(let ((stack (* 1024 1024 1024))
      (heap (max (sb-ext:dynamic-space-size) (* 8 1024 1024 1024))))
  (when (or (< (extern-alien "thread_control_stack_size" unsigned-long) stack)
            (< (sb-ext:dynamic-space-size) heap))
    (sb-ext:exit
     :code (sb-ext:process-exit-code
            (sb-ext:run-program
             sb-ext:*runtime-pathname*
             (list "--control-stack-size" (format nil "~dMB" (floor stack (* 1024 1024)))
                   "--dynamic-space-size" (format nil "~dMB" (floor heap (* 1024 1024)))
                   "--script" (sb-ext:native-namestring *load-truename*))
             :input t :output t :error t))
     :abort t)))

(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "load-dylan.lisp" *load-truename*)))

(defun dylan-main ()
  "The executable's entry: a read-eval-print loop, then exit.  The
debugger's ABORT, or an interrupt while reading, starts the loop again.
Reading a file, not a terminal, there is no debugger: an unhandled error
ends the run with status 1 (DYLAN::*BATCH*).  The one argument
bin/dylan takes, a file, is read as if it were the standard input; any
other arguments are refused with a usage message."
  ;; Writing to a closed pipe ends the run, as it ends a compiled program,
  ;; not in an error reported on the same output.
  (sb-sys:enable-interrupt sb-unix:sigpipe :default)
  (let* ((arguments (rest sb-ext:*posix-argv*))
         (file (and arguments (null (rest arguments))
                    (plusp (length (first arguments)))
                    (not (eql (char (first arguments) 0) #\-))
                    (first arguments))))
    (cond ((null arguments)
           (run-loop (not (eql 1 (sb-unix:unix-isatty 0)))))
          ((and file (let ((found (probe-file file))) (and found (pathname-name found))))
           (with-open-file (in file :external-format :utf-8)
             (let ((*standard-input* in))
               (run-loop t))))
          (t
           (when file
             (format *error-output* "dylan: cannot read ~a~%" file))
           (format *error-output* "usage: dylan [file.dyl]~%~:
  With no file, the read-eval-print loop reads the standard input.~%")
           (finish-output *error-output*)
           (sb-ext:exit :code 2 :abort t)))))

(defun run-loop (batch)
  "The read-eval-print loop on the standard input; BATCH: there is no one
at a terminal to use the debugger."
  (when batch
    (setf dylan::*batch* t
          sb-ext:*invoke-debugger-hook* #'dylan::end-batch-run)
    ;; A file's header names the module its forms are compiled in
    (multiple-value-bind (module rest) (dylan::read-stream-header *standard-input*)
      (setf *standard-input* rest)
      (when module (dylan::in-module module))))
  (loop
    (with-simple-restart (abort "Return to the Dylan read-eval-print loop.")
      (dylan:rep)
      (terpri)
      (finish-output)
      (sb-ext:exit :code 0))))

(let ((executable (make-pathname
                   :directory (append (butlast (pathname-directory *load-truename*))
                                      '("bin"))
                   :name "dylan"
                   :type #+win32 "exe" #-win32 nil
                   :defaults *load-truename*)))
  (ensure-directories-exist executable)
  (format t "~&;Writing ~a~%" (namestring (translate-logical-pathname executable)))
  (finish-output)
  ;; --script turned the debugger off; the loop needs it for the errors
  ;; Dylan code does not trap.
  (sb-ext:enable-debugger)
  ;; :SAVE-RUNTIME-OPTIONS, so the runtime does not take the command line's
  ;; options for its own, and keeps this one's stack size (above).
  (sb-ext:save-lisp-and-die executable
                            :executable t
                            :toplevel #'dylan-main
                            :save-runtime-options t))
