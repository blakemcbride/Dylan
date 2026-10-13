;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; The compiler's entry points:
;;;;
;;;;   (dylanc:compile-files files c-file)  Dylan source -> one C file
;;;;   (dylanc:build files executable)      ... and on to an executable
;;;;
;;;; Both answer T on success; the diagnostics are printed to
;;;; *ERROR-OUTPUT*.  BUILD runs the C compiler (CC, default "cc") on the
;;;; C file with the runtime library, which it makes first if need be.
;;;; With :TRAMPOLINE T the program and runtime use trampolines instead of
;;;; musttail for tail calls (CompilerPlan.md, Phase 4).

(in-package #:dylan-compiler)

(defparameter *runtime-directory*
  (merge-pathnames "../runtime/" cl-user::*dylanc-directory*))

(defun report-diagnostics (diagnostics)
  (dolist (d diagnostics)
    (format *error-output* "~a~%" (diagnostic-string d))))

(defun compile-program (program c-file)
  "Generate C-FILE for the front end's PROGRAM.  Answer T, or NIL after
reporting why not."
  (report-diagnostics (program-diagnostics program))
  (when (program-errors program)
    (return-from compile-program nil))
  (let ((*diagnostics* '()))
    (handler-case
        (let ((text (with-output-to-string (out)
                      (generate-c (lower-control (to-ir program)) out))))
          (with-open-file (out c-file :direction :output :if-exists :supersede)
            (write-string text out))
          t)
      (compile-error (e)
        (note-error e)
        (report-diagnostics *diagnostics*)
        nil))))

(defun compile-files (files c-file)
  (compile-program (front-end files :library t) c-file))

(defun run (program arguments)
  "Run PROGRAM; answer its exit code.  Its output goes to ours."
  (sb-ext:process-exit-code
   (sb-ext:run-program program arguments :search t
                                         :output *standard-output*
                                         :error *error-output*)))

(defun runtime-library (trampoline)
  "The runtime library's pathname, made if it does not exist or is out of
date."
  (let ((target (if trampoline "build/libdylan-trampoline.a" "build/libdylan.a")))
    (unless (zerop (run "make" (list "-s" "-C" (namestring *runtime-directory*) target)))
      (error "Could not make the runtime library."))
    (merge-pathnames target *runtime-directory*)))

(defun build (files executable &key trampoline (cc "cc") (optimize "-O2") c-file)
  "Compile FILES to the EXECUTABLE.  The C file is C-FILE, or
EXECUTABLE.c."
  (let ((c-file (or c-file (concatenate 'string (namestring executable) ".c"))))
    (and (compile-files files c-file)
         (build-c c-file executable :trampoline trampoline :cc cc :optimize optimize))))

(defun build-c (c-file executable &key trampoline (cc "cc") (optimize "-O2"))
  (let ((library (runtime-library trampoline)))
    (zerop (run cc (append (list optimize "-g" "-std=gnu11" "-pthread"
                                 "-Wall" "-Wno-unused-variable" "-Wno-unused-but-set-variable"
                                 "-Wno-unused-label"
                                 (format nil "-I~a" (namestring (merge-pathnames "c/" *runtime-directory*))))
                           (and trampoline (list "-DDY_TRAMPOLINE"))
                           (list (namestring c-file) (namestring library)
                                 "-lgc" "-lm" "-lpthread" "-o" (namestring executable)))))))
