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
;;;; musttail for tail calls (CompilerPlan.md, Phase 4); with :SIZE T the
;;;; executable is built small (Phase 9).

(in-package #:dylan-compiler)

(defparameter *runtime-directory*
  (merge-pathnames "../runtime/" cl-user::*dylanc-directory*))

(defun report-diagnostics (diagnostics)
  (dolist (d diagnostics)
    (format *error-output* "~a~%" (diagnostic-string d))))

(defun compile-program (program c-file)
  "Generate C-FILE, and the files that go with it, for the front end's
PROGRAM.  Answer the list of C files, or NIL after reporting why not."
  (report-diagnostics (program-diagnostics program))
  (when (program-errors program)
    (return-from compile-program nil))
  (let ((*diagnostics* '()))
    (handler-case
        (let* ((modules nil)
               (text (with-output-to-string (out)
                       (setf modules (generate-c (shake-library (lower-control (to-ir program)))
                                                 out)))))
          (write-c-program text c-file modules))
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

(defparameter *variants*
  '((:gc-stress "-O2 -g -DDY_GC_STRESS")
    (:address "-O1 -g -fno-omit-frame-pointer -fsanitize=address,undefined -DDY_DEBUG")
    (:thread "-O1 -g -fsanitize=thread -DDY_DEBUG"))
  "Builds for testing (CompilerPlan.md, Phase 11), each with the C flags
of both the runtime library and the program: a collection every few
allocations; AddressSanitizer and UndefinedBehaviorSanitizer; and
ThreadSanitizer.")

(defun variant-flags (variant)
  (if variant
      (let ((entry (assoc variant *variants*)))
        (unless entry (error "No build variant ~s." variant))
        (split-words (second entry)))
      '()))

(defun split-words (string)
  (loop with start = 0
        for space = (position #\Space string :start start)
        collect (subseq string start space)
        while space
        do (setf start (1+ space))))

(defun runtime-library (trampoline &key variant (cc "cc"))
  "The runtime library's pathname, made if it does not exist or is out of
date.  A VARIANT is made with the C compiler CC, which the program must be
compiled with too."
  (let* ((directory (and variant
                         (format nil "build/~(~a~)-~a~:[~;-trampoline~]"
                                 variant (file-namestring cc) trampoline)))
         (target (cond (directory (format nil "~a/libdylan.a" directory))
                       (trampoline "build/libdylan-trampoline.a")
                       (t "build/libdylan.a")))
         (arguments (append (list "-s" "-C" (namestring *runtime-directory*))
                            (and directory
                                 (list (format nil "VDIR=~a" directory)
                                       (format nil "VFLAGS=~{~a~^ ~}~:[~; -DDY_TRAMPOLINE~]"
                                               (variant-flags variant) trampoline)
                                       (format nil "CC=~a" cc)))
                            (list target))))
    (unless (zerop (run "make" arguments))
      (error "Could not make the runtime library."))
    (merge-pathnames target *runtime-directory*)))

(defun build (files executable &key trampoline (cc "cc") (optimize "-O2") size debug variant
                                   c-file)
  "Compile FILES to the EXECUTABLE.  The C goes in C-FILE, or
EXECUTABLE.c, and the files that go with it.  With :SIZE T the executable
is made small: -Os, and the functions and data nothing uses dropped when
linking.  With :DEBUG T it is made for debugging: no optimization, debugging
information, and every inline operation checking its arguments.  VARIANT
is one of *VARIANTS*, for testing."
  (let* ((c-file (or c-file (concatenate 'string (namestring executable) ".c")))
         (c-files (let ((*debug* (or debug (member variant '(:address :thread)))))
                    (compile-files files c-file))))
    (and c-files
         (build-c c-files executable :trampoline trampoline :cc cc :optimize optimize
                                     :size size :debug (or debug (member variant '(:address :thread)))
                                     :variant variant))))

(defun c-compiler-options (&key trampoline (optimize "-O2") size debug variant)
  (append (cond (variant (variant-flags variant))
                (debug (list "-O0" "-g" "-DDY_DEBUG"))
                (size (list "-Os" "-ffunction-sections" "-fdata-sections"
                            "-Wl,--gc-sections" "-s"))
                (t (list optimize "-g")))
          (list "-std=c11" "-pthread"
                "-Wall" "-Wno-unused-variable" "-Wno-unused-but-set-variable"
                "-Wno-unused-label"
                (format nil "-I~a" (namestring (merge-pathnames "c/" *runtime-directory*))))
          (and trampoline (list "-DDY_TRAMPOLINE"))))

(defun build-c (c-files executable &key trampoline (cc "cc") (optimize "-O2") size debug
                                        variant)
  "Compile and link the C-FILES (a list, or one file) of a program into
EXECUTABLE."
  (let ((library (runtime-library trampoline :variant variant :cc cc))
        (c-files (if (listp c-files) c-files (list c-files))))
    (zerop (run cc (append (c-compiler-options :trampoline trampoline :optimize optimize
                                               :size size :debug debug :variant variant)
                           (mapcar #'namestring c-files)
                           (list (namestring library)
                                 "-lgc" "-lm" "-lpthread" "-o" (namestring executable)))))))
