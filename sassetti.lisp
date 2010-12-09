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
;;;; Set *ledger-fname* to the path of your ledger file
;;;; Eval this (position point after s-expression, then C-x C-e): (preprocess-ledger-file *ledger-fname*)
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
(in-package #:sassetti)

(defvar *ledger-fname* "main.ledger.lisp" "Ledger file name")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric get-as-list (object)
  (:documentation "Return the slots of object as a list.  This is
  useful for testing because it allows you to test all of an object's
  slots with one test.  This greatly speeds up the running of
  tests."))
(defmethod get-as-list ((object NULL))
  (list object))
(defgeneric string-form (object)
  (:documentation "Return the object in ledger-format string form."))
(defmethod string-form ((object NULL))  "")
(defmethod string-form ((object string)) object)
(defgeneric units (object)
  (:documentation "Returns the units string of an object, trimmed of whitespace."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass amount()
  ((quantity :accessor quantity :initarg :quantity :initform 0)
   (units-before :accessor units-before :initarg :units-before :initform "")
   (units-after :accessor units-after :initarg :units-after :initform ""))
  (:documentation "An amount specified as a quantity of a commodity or
  currency of units.  Units are specified as before or after,
  depending on where labels are applied (e.g. $5 vs. 45 Euros).

  units-before might have trailing whitespace.  units-after might have
  leading whitespace.  Preserve in output so 5 Euros doesn't become
  5Euros.  But do comparisons on trimmed strings so 5 HKD equals 5HKD.
  "))
(defmethod units ((self amount))
  (cat (trim-whitespace (units-before self)) (trim-whitespace (units-after self))))
(defun dollar (amount)
  "Print dollar amount, complete with commas and appropriate decimal
  place and end-zero padding (no dollar sign, though)."
  (let* ((int (truncate (/ (round (* amount 100)) 100.0)))
	 (decimal-places 2)
	 (frac (abs (round (* (expt 10 decimal-places) (- amount int) )))))
    (if (= frac 0)
	(format nil "~:D" int)
	(format nil "~:D.~V,'0D" int decimal-places frac))))

(defmethod string-form ((self amount))
  (format nil "~a~a~a" (units-before self) (dollar (quantity self)) (units-after self)))
(defmethod get-as-list ((self amount))
  (list (units-before self) (quantity self) (units-after self)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass transaction ()
  ((account :accessor account :initarg :account)
   (commodity :accessor commodity :initarg :commodity :initform nil) ;$30 is a commodity if no other commodity is specified
   (unit-price :accessor unit-price :initarg :unit-price :initform nil);$30 is a unit price if another commodity is specified
   (cleared :accessor cleared :initarg :cleared)
   (pending :accessor pending :initarg :pending)
   (note :accessor note :initarg :note)
   ))
(defmethod get-as-list ((self transaction))
  (list (account self) (get-as-list (commodity self)) (get-as-list (unit-price self)) (cleared self) (pending self) (note self)))
(defmethod string-form ((self transaction))
  (format nil "   ~a~a     ~a~a~a"
	  (if (or (cleared self) (pending self))
	      (format nil "~a~a " 
		      (if (cleared self) "*" "")
		      (if (pending self) "!" ""))
	      "")
	  (account self)
	  (string-form (commodity self))
	  (string-form (unit-price self))
	  (cat (if (equal "" (note self)) "" ";") (note self))
	  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass date ()
  ((year :accessor year :initarg :year :initform nil)
   (month :accessor month :initarg :month)
   (day :accessor day :initarg :day)))
(defmethod get-as-list ((self date))
  (list (year self) (month self) (day self)))
(defmethod string-form ((self date))
  (if (year self)
      (format nil "~a-~2,'0d-~2,'0d" (year self) (month self) (day self))
      (format nil "~2,'0d-~2,'0d" (month  self) (day self))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass entry ()
  ((date :accessor date :initarg :date :initform nil)
   (effective-date :accessor effective-date :initarg :effective-date :initform nil)
   (desc :accessor desc :initarg :desc :initform "")
   (cleared :accessor cleared :initarg :cleared :initform nil)
   (pending :accessor pending :initarg :pending :initform nil)
   (code :accessor code :initarg :code :initform nil)
   (transactions :accessor transactions :initarg transactions :initform (list ))
   ))

(defmethod get-as-list ((self entry))
  (list (string-form (date self))
	(if (effective-date self) (string-form (effective-date self)) nil)
	(desc self)
	(cleared self)
	(pending self)
	(code self)
	(mapcar 'get-as-list (transactions self))))
(defmethod string-form ((self entry))
  (format nil "~a~a ~a~a~a~{~%~a~}" 
	  (string-form (date self)) 
	  (if (effective-date self)
	      (cat "=" (string-form (effective-date self)))
	      "")
	  (if (or (cleared self) (pending self))
	      (format nil "~a~a " 
		      (if (cleared self) "*" "")
		      (if (pending self) "!" ""))
	      "")
	  (if (code self) (format nil "(~a) " (code self)) "")
	  (desc self)
	  (mapcar 'string-form (transactions self))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ledger ()
  ((fname :accessor fname :initarg :fname)
   (entries :accessor entries)
   (curr-ledger-year :accessor curr-ledger-year 
		     :initarg curr-ledger-year
		     :documentation "The year that ledger entries are assumed to be in")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(preprocess-ledger-file *ledger-fname*) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-ledger-file (fname)
  (let ((transactions (list ""))
	(leading-whitespace "^[^\\w\\(\\)]"))
    (with-open-file (stream fname)
      (loop for line = (read-line stream nil)
	 while line
	 do
	   (unless (or (scan (cat leading-whitespace "*;") line)
		       (scan (cat leading-whitespace "*$") line))
	     (if (scan leading-whitespace line)
		 (setf (car transactions) (format nil "~a~%~a" (car transactions) line))
		 (push line transactions)))))
      (nreverse transactions)))

;(defun preprocess-ledger-file (fname)
;  (with-open-file (stream (cat fname ".gen")
;			  :direction :OUTPUT
;			  :if-exists :SUPERSEDE)
 ;   (mapcar (lambda (line)
;	      (format stream "~a~%~%" line))
;	    (parse-ledger-file (split-ledger-file fname)))))
;(preprocess-ledger-file *ledger-fname*)

