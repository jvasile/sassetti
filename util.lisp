;;;; Utility functions for Sassetti
;;;;
;;;; See COPYING for copyright and licensing information.

(in-package #:sassetti)

(unless (symbolp 'newline)
  (defconstant newline (string #\Newline) "Newline char in string form"))
(defmacro cat (&rest strings) 
  "Concatenate string"
  `(concatenate 'string ,@strings))
(defmacro trim-whitespace (s)
  "Trim leading and trailing whitespace"
  `(string-trim '(#\Space #\Tab #\Newline) ,s))
