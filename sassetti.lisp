;;;; sassetti.lisp
;;;;
;;;; Sassetti is a Common Lisp reimplementation of John Wiegley's
;;;; commandline ledger program.  The goal is to harness the power of
;;;; Wiegley's simple tools and extend them with the flexibility of
;;;; lisp.
;;;;
;;;; See README and INSTALL for details.
;;;;
;;;; To load it, start slime (M-x slime), then evaluate this to load
;;;; sassetti and its dependencies: (ql:quickload 'sassetti)
;;;;
;;;; To use this code:
;;;; Eval this (position point after s-expression, then C-x C-e): (preprocess-ledger-file '("filename"))
;;;; Now run ledger on the resulting ledger file.
;;;;
;;;; Don't forget you can compile forms with C-c C-c
;;;;
;;;; Also, M-x slime-eval-buffer and M-x slime-compile-buffer might be
;;;; useful, especially if you're getting useless asdf compile failure
;;;; messages.
;;;;
;;;; See COPYING for copyright and licensing information.

;(ql:quickload 'sassetti)
;(ql:quickload 'sassetti-test)

(in-package #:sassetti)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun preprocess-ledger-file (fname stream)
  "FNAME is the Ledger data file to process.

  STREAM is the stream of the parsed version of the file to
  output.

  xxxGENERATED-NAME is the name of the parsed version of the file to
  output.  If it is nil, tack .gen to the end of FNAME.xxx

  xxxTODO: if GENERATED-NAME is nil, use a temporary file, rather than
  tacking .gen to the end of FNAME.xxx
  "
  (format stream "~a" (string-form (parse-ledger-file fname))))


(defun preprocess-ledger-files (file)
  (dolist (f file)
    (with-open-file (stream (cat (tilde:expand-tilde-namestring f) ".gen")
			    :direction :OUTPUT
			    :if-exists :SUPERSEDE)
      (preprocess-ledger-file f stream))))

(defun call-ledger (ledger file options command args)
  "LEDGER is the path to the ledger file (may be nil)
  FILE is a list of ledger files (they must be specified and cannot be nil)
  OPTIONS is a list of options, some of which might be nil
  COMMAND is the Ledger command
  ARGS is a list of other arguments for Ledger (i.e. regexes)"
  (princ
   (with-output-to-string (out)
     (sb-ext:run-program (if ledger ledger "ledger")
			 (append (mapcan (lambda (f) (list "-f" f)) file)
				 (remove nil (mapcar (lambda (opt)
						       (if (eq opt t)
							   nil
							   (cat "--" (string-downcase opt))))
						     options))
				 (cons command args))
			 :output out :search t :wait t))
   *terminal-io*))

(defun call-ledger-with-temp (ledger files options command args)
  "Preprocesses the files into temporary files, then calls Ledger on
  the generated parsed files."
  (let ((streams))
    (dolist (file files)
      (osicat:with-temporary-file (stream :no-unlink t)
	(push (namestring (pathname stream)) streams)
	(preprocess-ledger-file file stream)))
    (call-ledger ledger
		 streams
		 options
		 command 
		 args)
    (dolist (stream streams)
      (nix:unlink stream))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-argv (opt-spec argv)
  "OPT-SPEC is an option specification suitable for the command-line-arguments package.
  ARGV is the commandline arguments passed to Sassetti.

  Returns a list of files specified (-f option), the non-file options,
  the Ledger command to run and any arguments to that command."

  (multiple-value-bind (options args) (process-command-line-options opt-spec (cdr argv))
    (let ((files))
      (doplist (k v options)
	(when (and v (equal k ':FILE))
	  (push v files)))
      (setf options (remove-from-plist options ':FILE))
      (values files options (pop args) args))))

(defun main (argv)
  "TODO: handle multiple -f parameters instead of assuming there's only one ledger file"
  (in-package #:sassetti)
  (let ((valid-commands '("bal" "balance" "reg" "register" "print" "xml" "emacs" "equity"
			  "parse" "prices" "pricedb" "entry"))
	(opt-spec
	 '((("help" #\h) :type boolean :documentation "Display this help")
	   (("file" #\f) :type string :initial-value nil 
	    :documentation "Read from file instead of environment variable SASETTI_FILE or LEDGER_FILE.  This variable may be specified multiple times.")
	   (("no-cache") :type boolean :documentation "Causes Ledger to always ignore the binary cache.")
	   (("ledger") :type string :documentation "Path to ledger executable" :initial-value "ledger")
	   (("verbose") :type boolean :documentation "Include debugging output (not implemented)")
	   (("debugger") :type boolean 
	    :documentation "Enable the interactive debugger (not implemented-- it's always enabled)")
	   )))
    (multiple-value-bind (file options command args) (parse-argv opt-spec argv)
      ;(format nil "~a~%~A~%~A~%~A" file options command args))))
      (cond ((or (not (or options args command)) (getf options ':HELP))
	     (show-option-help opt-spec))
	    ((not (find command valid-commands :test 'equal))
	     (format t "Error: Unrecognized command '~a'" command))
	    ((equal command "parse")
	     (preprocess-ledger-files file))
	    (t
	     (call-ledger-with-temp (getf options ':LEDGER) file (remove-from-plist options ':LEDGER) command args))
	    ))))

; TODO: Add tests for these commandline invocations
;(main '("bin/sassetti"))
;(main '("bin/sassetti" "-f" "/home/vasile/personal/ocs/main.ledger" "-f" "/home/vasile/personal/ocs/main.ledger.lisp" "parse"))
;(main '("bin/sassetti" "-f" "/home/vasile/personal/ocs/main.ledger" "-f" "/home/vasile/personal/ocs/main.ledger.lisp" "bal"))
;(main '("bin/sassetti" "-f" "/home/vasile/personal/ocs/main.ledger.lisp" "parse"))
;(main '("bin/sassetti" "-h" "-d"))
;(main '("bin/sassetti" "-v" "test"))
