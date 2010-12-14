;;;; sassetti-test.asd

(load "depends.lisp")
(ql:quickload 'FiveAM)

(asdf:defsystem #:sassetti-test
  :serial t
  :description "Common Lisp reimplementation of John Wiegley's commandline Ledger program."
  :version "0.1"
  :author "James Vasile <james@hackervisions.org>"
  :licence "Copyright 2008 James Vasile, Relesaed under GNU General Public License, Version 3 or later"
  :components ((:file "package-test")
	       (:file "util")
	       (:file "util.test")
               (:file "sassetti")
	       (:file "parse")
	       (:file "user-funcs")
	       (:file "user-funcs.test")
	       (:file "user-designed-funcs")
	       (:file "test")
	       )
  :long-description "Sassetti is a Common Lisp reimplementation of
John Wiegley's commandline ledger program.  The goal is to harness the
power of Wiegley's simple tools and extend them with the flexibility
of lisp."  )


