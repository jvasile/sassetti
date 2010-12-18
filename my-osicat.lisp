;; osicat ignores the pathname var for fd streams, but I don't know
;; why.  Since I'd like that info, I'm redefining the make-fd-stream
;; function to use it.
(in-package :osicat)
#+sbcl
(defun make-fd-stream (fd &key direction element-type external-format
                       pathname file)
  (declare (ignore file))
  (let ((in-p (member direction '(:io :input)))
        (out-p (member direction '(:io :output))))
    (sb-sys:make-fd-stream fd :input in-p :output out-p
                           :element-type element-type
                           :pathname pathname
                           :external-format external-format)))

;; Stop the immediate unlinking of temporary files.  We need them.

(defun %open-temporary-file/fd-streams (filename element-type external-format &key (no-unlink nil))
  (handler-case
      (multiple-value-bind (fd path)
          (nix:mkstemp filename)
        (%close-on-error
            (nix:close fd)
          (unless no-unlink (nix:unlink path)))
        (make-fd-stream fd :direction :io
                        :element-type element-type
                        :external-format external-format
                        :pathname (pathname path)
                        :file path))
    (nix:posix-error ()
      (error 'file-error :pathname filename))))

(defun %open-temporary-file/no-fd-streams (filename element-type external-format &key (no-unlink nil))
  (do ((counter 100 (1- counter)))
      ((zerop counter) (error 'file-error :pathname filename))
    (let* ((path (nix:mktemp filename))
           (stream (open path :direction :io
                         :element-type element-type
                         :external-format external-format
                         :if-exists :error
                         :if-does-not-exist :create)))
      (%close-on-error
          (close stream :abort t)
        (unless no-unlink (nix:unlink path)))
      (return stream))))

(defun open-temporary-file (&key (pathspec *temporary-directory*)
                            (element-type 'character)
                            (external-format :default)
                            (no-unlink nil))
  "Creates a temporary file setup for input and output, and returns a
stream connected to that file.

PATHSPEC serves as template for the file to be created: a certain
number of random characters will be concatenated to the file component
of PATHSPEC.  If PATHSPEC has no directory component, the file will be
created inside *TEMPORARY-DIRECTORY*. The file itself is unlinked once
it has been opened unless NO-UNLINK is T.

ELEMENT-TYPE specifies the unit of transaction of the stream.
Consider using WITH-TEMPORARY-FILE instead of this function.

On failure, a FILE-ERROR may be signalled."
  (let ((filename
         (native-namestring
          (merge-pathnames (pathname pathspec) *temporary-directory*))))
    #+osicat-fd-streams
    (%open-temporary-file/fd-streams filename element-type external-format :no-unlink no-unlink)
    #-osicat-fd-streams
    (%open-temporary-file/no-fd-streams filename element-type external-format :no-unlink no-unlink)))

(defmacro with-temporary-file ((stream &key (pathspec *temporary-directory*)
                                       (element-type 'character)
                                       (external-format :default)
                                       (no-unlink nil))
                               &body body)
  "Within the lexical scope of the body, STREAM is connected to a
temporary file as created by OPEN-TEMPORARY-FILE.  The file is
closed automatically once BODY exits."
  `(with-open-stream
       (,stream (open-temporary-file :pathspec ,pathspec
                                     :element-type ,(if (eq element-type 'character)
                                                        (quote 'character)
                                                        element-type)
                                     :external-format ,external-format
                                     :no-unlink ,no-unlink))
     ,@body))

