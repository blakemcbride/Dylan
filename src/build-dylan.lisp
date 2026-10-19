;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; Build bin/dylan, a stand-alone executable of the Common Lisp
;;;; implementation that starts in the read-eval-print loop:
;;;;
;;;;     sbcl --script src/build-dylan.lisp      (or: make)

(in-package #:cl-user)

(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "load-dylan.lisp" *load-truename*)))

(defun dylan-main ()
  "The executable's entry: a read-eval-print loop, then exit.  The
debugger's ABORT, or an interrupt while reading, starts the loop again."
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
  ;; options for its own.
  (sb-ext:save-lisp-and-die executable
                            :executable t
                            :toplevel #'dylan-main
                            :save-runtime-options t))
