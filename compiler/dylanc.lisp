;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; The command line of the Dylan-to-C compiler (CompilerPlan.md, Phase
;;;; 10), run by bin/dylanc:
;;;;
;;;;     dylanc [options] file.dyl ...
;;;;
;;;; compiles the files, in order, with the library into one program: C,
;;;; then an executable, by the C compiler and the runtime library.  The
;;;; exit status is 0 on success, 1 if the program or the C compiler had
;;;; errors, and 2 for a bad command line.

(in-package #:cl-user)

(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "load.lisp" (or *load-truename* *default-pathname-defaults*))))

(in-package #:dylan-compiler)

(defparameter *usage* "Usage: dylanc [options] file.dyl ...

Compile the Dylan files, in order, into one executable.

Options:
  -o FILE          the executable (default: the first file's name, less .dyl)
  -O0 -O1 -O2 -O3  how much the C compiler optimizes (default -O2)
  -Os              a small executable: optimized for size, unused code dropped
  -g, --debug      for debugging: no optimization, debugging information, and
                   every inline operation checking its arguments when run
  -k, --keep-c     keep the C: FILE.c, FILE-MODULE.c for each module, FILE.h
  -C, --c-only     write the C only (FILE.c and the rest); do not compile it
  --trampoline     tail calls by trampolines, for C compilers without musttail
  --cc CC          the C compiler (default: $CC, or cc)
  --no-lines       no #line directives pointing to the Dylan source
  -h, --help       this
")

(defun usage-error (control &rest arguments)
  (format *error-output* "dylanc: ~?~%~a" control arguments *usage*)
  (sb-ext:exit :code 2 :abort t))

(defun parse-command-line (arguments)
  "A plist of the options in ARGUMENTS, with :FILES."
  (let ((options (list :optimize "-O2" :cc (or (sb-ext:posix-getenv "CC") "cc")))
        (files '()))
    (loop while arguments
          do (let ((a (pop arguments)))
               (flet ((value ()
                        (or (pop arguments) (usage-error "~a needs a value." a))))
                 (cond ((member a '("-h" "--help") :test #'string=)
                        (write-string *usage*)
                        (sb-ext:exit :code 0))
                       ((string= a "-o") (setf (getf options :output) (value)))
                       ((member a '("-O0" "-O1" "-O2" "-O3") :test #'string=)
                        (setf (getf options :optimize) a))
                       ((string= a "-Os") (setf (getf options :size) t))
                       ((member a '("-g" "--debug") :test #'string=) (setf (getf options :debug) t))
                       ((member a '("-k" "--keep-c") :test #'string=) (setf (getf options :keep) t))
                       ((member a '("-C" "--c-only") :test #'string=) (setf (getf options :c-only) t))
                       ((string= a "--trampoline") (setf (getf options :trampoline) t))
                       ((string= a "--cc") (setf (getf options :cc) (value)))
                       ((string= a "--no-lines") (setf (getf options :no-lines) t))
                       ((and (plusp (length a)) (char= (char a 0) #\-))
                        (usage-error "unknown option ~a." a))
                       (t (push a files))))))
    (when (null files) (usage-error "no files to compile."))
    (dolist (f files)
      (unless (probe-file f) (usage-error "~a does not exist." f)))
    (list* :files (nreverse files) options)))

(defun default-output (file)
  (let ((p (pathname file)))
    (namestring (make-pathname :type nil :defaults p))))

(defun main (arguments)
  (let* ((options (parse-command-line arguments))
         (files (getf options :files))
         (output (or (getf options :output) (default-output (first files))))
         (c-file (concatenate 'string output ".c"))
         (c-files (let ((*debug* (getf options :debug))
                        (*emit-lines* (not (getf options :no-lines))))
                    (compile-files files c-file))))
    (unless c-files (sb-ext:exit :code 1 :abort t))
    (unless (getf options :c-only)
      (let ((ok (build-c c-files output
                         :trampoline (getf options :trampoline)
                         :cc (getf options :cc)
                         :optimize (getf options :optimize)
                         :size (getf options :size)
                         :debug (getf options :debug))))
        (unless (getf options :keep)
          (dolist (f (cons (make-pathname :type "h" :defaults (pathname c-file)) c-files))
            (when (probe-file f) (delete-file f))))
        (unless ok
          (format *error-output* "dylanc: the C compiler failed.~%")
          (sb-ext:exit :code 1 :abort t))))
    (finish-output)
    (sb-ext:exit :code 0)))

(main (rest sb-ext:*posix-argv*))
