;;;; package.lisp

(load "tilde.lisp")
(ql:quickload 'cl-ppcre)
(ql:quickload 'iterate)
(ql:quickload 'split-sequence)
(ql:quickload 'COMMAND-LINE-ARGUMENTS)
(ql:quickload 'FiveAM)

(defpackage #:sassetti
  (:use :common-lisp 
	:command-line-arguments
	:cl-ppcre 
	:iterate 
	:it.bese.FiveAM
	))

(in-package #:sassetti)     


