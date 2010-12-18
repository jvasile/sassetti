;; This file contains the data model for Sassetti
;;
;; See COPYING for copyright and license information.

(in-package #:sassetti)

(defgeneric get-as-list (object)
  (:documentation "Return the slots of object as a list.  This is
  useful for testing because it allows you to test all of an object's
  slots with one test.  This greatly speeds up the running of
  tests."))
(defmethod get-as-list ((object NULL))
  (list object))
(defgeneric string-form (object &key &allow-other-keys)
  (:documentation "Return the object in ledger-format string form."))
(defmethod string-form ((object NULL) &key w) "")
(defmethod string-form ((object string) &key w) object)
(defmethod string-form ((object list) &key w)
  (format nil "~{~a~%~%~}" (mapcar 'string-form object)))
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
(defmethod string-form ((self amount) &key (commas-p nil) (replace nil) (adjust 0) (neg-p nil))
  "Set COMMAS-P to true to return 1000 as 1,000

  Set REPLACE to replace the amount in the return string.

  Amount's quantity will be adjusted by amount ADJUST in the return
  string.  This lets you print figures with rounding fixes a little
  more easily.

  Set NEG-P to multuple amount by -1 in the return string.

  In terms of precedence, NEG-P < ADJUST < REPLACE.  This mean that if
  you replace adjust and neg-p an amount, the value will be replaced,
  then adjust, then multiplied by -1."

  (dollars (* (+ adjust (if replace replace (quantity self)))
	      (if neg-p -1 1))
	   :comma-char (if commas-p #\, nil)
	   :pre-units (units-before self) :post-units (units-after self)))
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
(defmethod account-width ((self transaction))
  "Return the length in chars of the account slot"
  (length (account self)))
(defmethod string-form ((self transaction) &key width)
  "Width is the length in chars to use for the account slot.  It
  allows us to make the amounts for an entry line up."
  (format nil "   ~3a~va     ~a~a~a"
	  (if (or (cleared self) (pending self))
	      (format nil "~a~a " 
		      (if (cleared self) "*" "")
		      (if (pending self) "!" ""))
	      "")
	  width
	  (account self)
	  (string-form (commodity self))
	  (string-form (unit-price self))
	  (cat (if (equal "" (note self)) "" " ;") (note self))
	  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass date ()
  ((year :accessor year :initarg :year :initform nil)
   (month :accessor month :initarg :month)
   (day :accessor day :initarg :day)))
(defmethod get-as-list ((self date))
  (list (year self) (month self) (day self)))
(defmethod string-form ((self date) &key w)
  (if (year self)
      (format nil "~a/~2,'0d/~2,'0d" (year self) (month self) (day self))
      (format nil "~2,'0d/~2,'0d" (month  self) (day self))))
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

(defmethod string-form ((self entry) &key w)
  (let ((width (if (transactions self)
		   (apply 'max (mapcar (lambda (trans) (length (account trans))) (transactions self)))
		   0)))
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
	    (mapcar (lambda (trans) (string-form trans :width width)) (transactions self)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ledger ()
  ((fname :accessor fname :initarg :fname :initform "")
   (entries :accessor entries :initarg :entries :initform nil)
   (curr-ledger-year :accessor curr-ledger-year 
		     :initarg curr-ledger-year
		     :documentation "The year that ledger entries are assumed to be in")
   ))
(defmethod string-form ((self ledger) &key w)
  (format nil "~{~a~%~%~}" (mapcar 'string-form (entries self))))
