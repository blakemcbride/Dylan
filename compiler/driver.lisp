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
;;;;
;;;; The library's C, NAME-dylan.c, is the same for most programs (Phase 9,
;;;; separate compilation), so BUILD-C compiles it once for each C
;;;; compiler and options: the object is kept in runtime/build/library/,
;;;; named for a digest of all that went into it.

(in-package #:dylan-compiler)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-md5))

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
                       (setf modules (generate-c (let ((ir (lower-control (to-ir program))))
                                                   (if *separate-library* ir (shake-library ir)))
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

(defparameter *lto-flags* "-O2 -g -flto"
  "The C flags of the runtime library for link-time optimization.")

(defun runtime-library (trampoline &key variant (cc "cc") lto)
  "The runtime library's pathname, made if it does not exist or is out of
date.  A VARIANT, or the library for link-time optimization (LTO), is
made with the C compiler CC, which the program must be compiled with
too."
  (let* ((variant (or variant (and lto :lto)))
         (directory (and variant
                         (format nil "build/~(~a~)-~a~:[~;-trampoline~]"
                                 variant (file-namestring cc) trampoline)))
         (target (cond (directory (format nil "~a/libdylan.a" directory))
                       (trampoline "build/libdylan-trampoline.a")
                       (t "build/libdylan.a")))
         (arguments (append (list "-s" "-C" (namestring *runtime-directory*))
                            (and directory
                                 (list (format nil "VDIR=~a" directory)
                                       (format nil "VFLAGS=~{~a~^ ~}~:[~; -DDY_TRAMPOLINE~]"
                                               (if (eq variant :lto)
                                                   (split-words *lto-flags*)
                                                   (variant-flags variant))
                                               trampoline)
                                       (format nil "CC=~a" cc)))
                            (list target))))
    (unless (zerop (run "make" arguments))
      (error "Could not make the runtime library."))
    (merge-pathnames target *runtime-directory*)))

(defun build (files executable &key trampoline (cc "cc") (optimize "-O2") size debug variant
                                   lto c-file whole)
  "Compile FILES to the EXECUTABLE.  The C goes in C-FILE, or
EXECUTABLE.c, and the files that go with it.  With :SIZE T the executable
is made small: -Os, the library compiled with the program, and the
functions and data nothing uses dropped.  With :DEBUG T it is made for
debugging: no optimization, debugging information, and every inline
operation checking its arguments.  With :LTO T the C compiler optimizes
the program and the runtime library together when linking.  With :WHOLE
T the library is compiled with the program, not apart.  VARIANT is one of
*VARIANTS*, for testing."
  (let* ((*separate-library* (separate-library-p size whole))
         (c-file (or c-file (concatenate 'string (namestring executable) ".c")))
         (c-files (let ((*debug* (or debug (member variant '(:address :thread)))))
                    (compile-files files c-file))))
    (and c-files
         (build-c c-files executable :trampoline trampoline :cc cc :optimize optimize
                                     :size size :debug (or debug (member variant '(:address :thread)))
                                     :variant variant :lto lto))))

(defun separate-library-p (size whole)
  "*SEPARATE-LIBRARY* for a build: the library is compiled apart unless
the build is for size, where the program keeps only what it uses of it,
or WHOLE."
  (and *separate-library* (not size) (not whole)))

(defun lto-p (lto debug variant)
  "Is the program to be linked with link-time optimization?  Not when
built for debugging or testing."
  (and lto (not debug) (not variant)))

(defun c-compiler-options (&key trampoline (optimize "-O2") size debug variant lto)
  (append (cond (variant (variant-flags variant))
                (debug (list "-O0" "-g" "-DDY_DEBUG"))
                (size (list "-Os" "-ffunction-sections" "-fdata-sections"
                            "-Wl,--gc-sections" "-s"))
                (t (list optimize "-g")))
          (list "-std=c11" "-pthread"
                "-Wall" "-Wno-unused-variable" "-Wno-unused-but-set-variable"
                "-Wno-unused-label"
                (format nil "-I~a" (namestring (merge-pathnames "c/" *runtime-directory*))))
          (and trampoline (list "-DDY_TRAMPOLINE"))
          (and (lto-p lto debug variant) (list "-flto=auto"))))

(defun build-c (c-files executable &key trampoline (cc "cc") (optimize "-O2") size debug
                                        variant lto)
  "Compile and link the C-FILES (a list, or one file) of a program into
EXECUTABLE.  If *SEPARATE-LIBRARY*, as when they were made, the library's
is compiled once (LIBRARY-OBJECT)."
  (let* ((library (runtime-library trampoline :variant variant :cc cc
                                              :lto (lto-p lto debug variant)))
         (c-files (if (listp c-files) c-files (list c-files)))
         (options (c-compiler-options :trampoline trampoline :optimize optimize
                                      :size size :debug debug :variant variant
                                      :lto lto))
         (objects (mapcar (lambda (file)
                            (if (and *separate-library* (library-c-file-p file))
                                (library-object file cc options)
                                file))
                          c-files)))
    (and (every #'identity objects)
         (zerop (run cc (append options
                                (mapcar #'namestring objects)
                                (list (namestring library)
                                      "-lgc" "-lm" "-lpthread" "-o" (namestring executable))))))))

;;; The library's object

(defparameter *library-cache-directory*
  (merge-pathnames "build/library/" *runtime-directory*)
  "Where the objects of the library's C are kept.")

(defparameter *library-cache-size* 40
  "How many objects of the library's C are kept: the newest.")

(defun library-c-file-p (file)
  (let ((name (pathname-name (pathname file))))
    (and (> (length name) 6) (string= "-dylan" name :start2 (- (length name) 6)))))

(defun file-text (file)
  (with-open-file (in file :external-format :latin-1)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun replace-all (text old new)
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old text :start2 start)
          do (write-string text out :start start :end (or at (length text)))
          while at
          do (write-string new out)
             (setf start (+ at (length old))))))

(defvar *cc-versions* '() "(cc . its --version) for the C compilers asked.")

(defun cc-version (cc)
  (or (cdr (assoc cc *cc-versions* :test #'string=))
      (let ((text (with-output-to-string (out)
                    (ignore-errors
                     (sb-ext:run-program cc '("--version") :search t :output out :error nil)))))
        (push (cons cc text) *cc-versions*)
        text)))

(defun library-object (c-file cc options)
  "The object of the library's C-FILE, compiled by CC with OPTIONS: kept
from a build before, or compiled now (in the cache, so that the #line
directives back to the C file stay true there).  NIL if it does not
compile."
  (let* ((self (c-name-string (namestring c-file)))
         (placeholder "\"<library>\"")
         (text (replace-all (file-text c-file) self placeholder))
         (digest (format nil "~(~{~2,'0x~}~)"
                         (coerce (sb-md5:md5sum-string
                                  (format nil "~a~%~{~a~^ ~}~%~a~%~a~%~a~%~a"
                                          cc options (cc-version cc)
                                          (file-text (merge-pathnames "c/dylan.h" *runtime-directory*))
                                          (file-text (merge-pathnames "c/primitive.h" *runtime-directory*))
                                          text)
                                  :external-format :latin-1)
                                 'list)))
         (object (merge-pathnames (format nil "~a.o" digest) *library-cache-directory*)))
    (if (probe-file object)
        object
        (let* ((source (make-pathname :type "c" :defaults object))
               (temporary (make-pathname :name (format nil "~a-~d" digest (sb-unix:unix-getpid))
                                         :defaults object)))
          (ensure-directories-exist object)
          (with-open-file (out source :direction :output :if-exists :supersede
                                      :external-format :latin-1)
            (write-string (replace-all text placeholder (c-name-string (namestring source))) out))
          (when (zerop (run cc (append (remove-if (lambda (o) (search "-Wl," o)) options)
                                       (list "-c" (namestring source)
                                             "-o" (namestring temporary)))))
            ;; Renamed whole, for builds running at the same time
            (rename-file temporary object)
            (prune-library-cache)
            object)))))

(defun prune-library-cache ()
  "Keep the newest *LIBRARY-CACHE-SIZE* objects in the cache, and their C."
  (let ((objects (sort (directory (merge-pathnames "*.o" *library-cache-directory*))
                       #'> :key #'file-write-date)))
    (dolist (old (nthcdr *library-cache-size* objects))
      (ignore-errors (delete-file old))
      (ignore-errors (delete-file (make-pathname :type "c" :defaults old))))))
