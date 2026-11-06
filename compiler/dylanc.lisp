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

(require :sb-posix)

(in-package #:dylan-compiler)

(defparameter *usage* "Usage: dylanc [options] file.dyl ...

Compile the Dylan files, in order, into one executable.

Options:
  -o FILE          the executable (default: the first file's name, less .dyl)
  -O0 -O1 -O2 -O3  how much the C compiler optimizes (default -O2)
  -Os              a small executable: optimized for size, unused code dropped
                   (the library compiled with the program, as --whole)
  --whole          compile the library with the program, not apart (the
                   library's C, compiled apart, is compiled once and kept)
  --lto            optimize the program and the runtime library together when
                   linking (link-time optimization)
  -g, --debug      for debugging: no optimization, debugging information, and
                   every inline operation checking its arguments when run
  -k, --keep-c     keep the C: FILE.c, FILE-MODULE.c for each module, FILE.h
  -C, --c-only     write the C only (FILE.c and the rest); do not compile it
  -E, --expand     write each use of the program's macros and its expansion;
                   no C
  --trampoline     tail calls by trampolines, for C compilers without musttail
  --cc CC          the C compiler (default: $CC, or cc)
  --no-lines       no #line directives pointing to the Dylan source
  --sanitize KIND  for testing: a debug build with the sanitizers, KIND
                   address (AddressSanitizer and UndefinedBehaviorSanitizer)
                   or thread (ThreadSanitizer)
  --gc NAME        the garbage collector: mps (the default), the Memory Pool
                   System, generational and moving; or boehm, the installed
                   Boehm-Demers-Weiser collector (libgc)
  --gc-stress      for testing: a collection every DYLAN_GC_STRESS (default
                   100) allocations
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
                       ((member a '("-E" "--expand") :test #'string=) (setf (getf options :expand) t))
                       ((string= a "--trampoline") (setf (getf options :trampoline) t))
                       ((string= a "--lto") (setf (getf options :lto) t))
                       ((string= a "--whole") (setf (getf options :whole) t))
                       ((string= a "--cc") (setf (getf options :cc) (value)))
                       ((string= a "--no-lines") (setf (getf options :no-lines) t))
                       ((string= a "--gc-stress") (setf (getf options :variant) :gc-stress))
                       ((string= a "--gc")
                        (let ((name (value)))
                          (setf (getf options :gc)
                                (cond ((string= name "boehm") :boehm)
                                      ((string= name "mps") :mps)
                                      (t (usage-error "--gc takes boehm or mps."))))))
                       ((string= a "--sanitize")
                        (let ((kind (value)))
                          (setf (getf options :variant)
                                (cond ((string= kind "address") :address)
                                      ((string= kind "thread") :thread)
                                      (t (usage-error "--sanitize takes address or thread."))))))
                       ((and (plusp (length a)) (char= (char a 0) #\-))
                        (usage-error "unknown option ~a." a))
                       (t (push a files))))))
    (when (null files) (usage-error "no files to compile."))
    (dolist (f files)
      (let ((found (probe-file (native-pathname f))))
        (unless found (usage-error "~a does not exist." f))
        (unless (or (pathname-name found) (pathname-type found))
          (usage-error "~a is a directory, not a file." f))))
    (list* :files (nreverse files) options)))

(defun default-output (file)
  (let ((p (native-pathname file)))
    (native-name (make-pathname :type nil :defaults p))))

(defun refuse (control &rest arguments)
  "Report a command line that would destroy a file, and exit with 2."
  (format *error-output* "dylanc: ~?.~%" control arguments)
  (sb-ext:exit :code 2 :abort t))

(defun input-truenames (files)
  (mapcar (lambda (f) (probe-file (native-pathname f))) files))

(defun temporary-directory ()
  "A new, empty directory of this process's own, for the C of a build
that does not keep it, so that no file of the user's is written or
deleted but the executable."
  (let ((base (or (sb-ext:posix-getenv "TMPDIR") "/tmp")))
    (sb-ext:parse-native-namestring
     (concatenate 'string
                  (sb-posix:mkdtemp (concatenate 'string (string-right-trim "/" base)
                                                 "/dylanc-XXXXXX"))
                  "/"))))

(defun file-basename (name)
  "The last component of the file name NAME."
  (let ((slash (position #\/ name :from-end t)))
    (if slash (subseq name (1+ slash)) name)))

(defun expand-only (files)
  "Write each use of the program's macros in FILES and its expansion."
  (let* ((program (let ((*expansion-trace* *standard-output*))
                    (front-end files :library t))))
    (report-diagnostics (program-diagnostics program))
    (finish-output)
    (sb-ext:exit :code (if (program-errors program) 1 0) :abort (program-errors program))))

(defun main (arguments)
  (let* ((options (parse-command-line arguments))
         (*gc* (or (getf options :gc) *gc*))
         (files (getf options :files))
         (output (or (getf options :output) (default-output (first files))))
         (*protected-files* (input-truenames files))
         (variant (progn (when (getf options :expand) (expand-only files))
                         (getf options :variant)))
         (keep-c (or (getf options :keep) (getf options :c-only)))
         ;; The C is written beside the executable only when it is kept;
         ;; otherwise in a directory of dylanc's own, removed afterwards
         (c-directory (unless keep-c (temporary-directory)))
         (c-file (if c-directory
                     (concatenate 'string (native-name c-directory) (file-basename output) ".c")
                     (concatenate 'string output ".c")))
         (*separate-library* (separate-library-p (getf options :size) (getf options :whole))))
    (flet ((finish (code)
             (when c-directory
               (sb-ext:delete-directory c-directory :recursive t))
             (finish-output)
             (sb-ext:exit :code code :abort (/= code 0))))
      (unless (getf options :c-only)
        (let ((existing (probe-file (native-pathname output))))
          (when (and existing (member existing *protected-files* :test #'equal))
            (when c-directory
              (sb-ext:delete-directory c-directory :recursive t))
            (refuse "the executable ~a would replace the input of that name; name another with -o"
                    output))))
      (let ((c-files (handler-case
                         (let ((*debug* (or (getf options :debug)
                                            (member variant '(:address :thread))))
                               (*emit-lines* (not (getf options :no-lines))))
                           (compile-files files c-file))
                       (protected-file (e)
                         (format *error-output* "dylanc: ~a.~%" e)
                         (finish 2))
                       (file-error (e)
                         (format *error-output* "dylanc: cannot write ~a.~%"
                                 (native-name (file-error-pathname e)))
                         (finish 2)))))
        (unless c-files (finish 1))
        (unless (getf options :c-only)
          (let ((ok (handler-case
                        (build-c c-files output
                                 :trampoline (getf options :trampoline)
                                 :cc (getf options :cc)
                                 :optimize (getf options :optimize)
                                 :size (getf options :size)
                                 :debug (getf options :debug)
                                 :variant variant
                                 :lto (getf options :lto))
                      (build-failure (e)
                        (format *error-output* "dylanc: ~a.~%" e)
                        :reported))))
            (unless (eq ok t)
              (unless ok (format *error-output* "dylanc: the C compiler failed.~%"))
              (finish 1))))
        (finish 0)))))

(main (rest sb-ext:*posix-argv*))
