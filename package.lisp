;;;; package.lisp

(ql:quickload 'cl-ppcre)
(ql:quickload 'iterate)
(ql:quickload 'split-sequence)
(ql:quickload 'FiveAM)

(defpackage #:sassetti
  (:use :common-lisp 
	:cl-ppcre 
	:iterate 
	:it.bese.FiveAM
	))

