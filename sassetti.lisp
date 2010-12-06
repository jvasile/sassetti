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

(in-package #:sassetti)

(defvar *ledger-fname* "main.ledger.lisp" "Ledger file name")
(defconstant *newline* (string #\Newline) "Newline char in string form")

(defmacro cat (&rest strings) 
  "Concatenate string"
  `(concatenate 'string ,@strings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric get-as-list (object)
  (:documentation "Return the slots of object as a list.  This is useful for testing"))
(defgeneric string-form (object)
  (:documentation "Return the object in ledger-format string form."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass amount ()
  ((units :accessor units :initarg :units :initform nil)
   (commodity :accessor commodity :initarg :commodity :initform "")
   (price :accessor price :initarg :price :initform 0)
   (denomination :accessor denomination :initarg :denomination :initform ""))
  (:documentation "An amount specified as units of a commodity at a unit price in a denominating currency."))
(defmethod string-form ((self amount))
  (if (units self)
      (format nil "~a ~a @ ~a~a"
	      (units self)
	      (commodity self)
	      (denomination self)(price self))
      (format nil "~a~a" (denomination self)(price self))))

(defmethod get-as-list ((amt amount))
  (list (price amt) (denomination amt) (units amt) (commodity amt)))

(defun parse-amount-currency (s)
  "Parse a portion of the amount that is a number and a symbol.
  Return the number and the symbol, with leading/trailing whitespace
  trimmed.  Use this to process either the currency or the commodity
  part of the amount.

  '50 AAPL' -=> 50, 'AAPL'
  '$50' -=> 50, '$'
  '20 HKD' -=> 20, 'HKD'"

  (if (equal #\@ (char s 0))
    (parse-amount-currency (subseq s 1))
    (let* ((price (scan-to-strings "(-?[,.\\d]+)\\s*$" s))
	   (denomination (subseq s 0 (- (length s) (length price)))))
      (unless price
	(setf price (scan-to-strings "^\\s*(-?[,.\\d]+)" s))
	(setf denomination (subseq s (length price) (length s))))
      (setf price (or price "0"))
      (values (if (equal price ".") 
		  0
		  (read-from-string (remove #\, price)))
	      (string-trim '(#\Space #\Tab #\Newline) denomination)))))

(defun parse-amount (s)
  "Accept a string like '$3502' or '-23 Euros' or '22 Martian Dollars'.
  Returns an amount object with the units, commodity and unit-price
  set from values extracted from the string.  Leading and trailing
  whitespace will be trimmed as needed."
  ;(if (find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
  (let ((at-pos (position #\@ s))
	(units) (commodity) (price) (denomination))
    (if at-pos
	(progn
	  (multiple-value-setq (price denomination) (parse-amount-currency (subseq s (+ 1 at-pos))))
	  (multiple-value-setq (units commodity) (parse-amount-currency (subseq s 0 at-pos)))
	  (when (scan "@@" s) (setf price (/ price units)))
	  (make-instance 'amount
			 :price price 
			 :denomination denomination
			 :units  units
			 :commodity commodity))	
	(progn
	  (multiple-value-setq (price denomination) (parse-amount-currency s))
	  (make-instance 'amount
			 :price price
			 :denomination denomination)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass transaction ()
  ((account :accessor account :initarg :account)
   (amount :accessor amount :initarg :amount)
   (cleared :accessor cleared :initarg :cleared)
   (pending :accessor pending :initarg :pending)
   (note :accessor note :initarg :note)
   ))
(defun parse-transaction (line)
  "Parse a transaction (e.g. one indented line from an entry).
  Returns a transaction object.

  The format of each following transaction is:

            ACCOUNT  AMOUNT  [; NOTE]

  The 'ACCOUNT' may be surrounded by parentheses if it is a virtual
  transactions, or square brackets if it is a virtual transactions
  that must balance. The 'AMOUNT' can be followed by a per-unit
  transaction cost, by specifying '@ AMOUNT', or a complete
  transaction cost with '@@ AMOUNT'. Lastly, the 'NOTE' may specify an
  actual and/or effective date for the transaction by using the syntax
  '[ACTUAL_DATE]' or '[=EFFECTIVE_DATE]' or
  '[ACTUAL_DATE=EFFECtIVE_DATE]'.
  "
  (let ((parts (coerce (second (multiple-value-list (scan-to-strings "^\\s*([*!]*)\\s*(.*?)(\\s\\s+([^;]*);?(.*?))?$"
								     line))) 'list)))
    (make-instance 'transaction 
		   :account (second parts)
		   :amount (if (equal "" (fourth parts))
			       nil
			       (if (fourth parts)
				   (parse-amount (fourth  parts))
				   nil))
		   :cleared (not (equal (scan "\\*" (first parts)) nil))
		   :pending (not (equal (scan "\\!" (first parts)) nil))
		   :note (or (fifth parts) ""))))

(defmethod get-as-list ((self transaction))
  (list (account self) (if (amount self) (get-as-list (amount self)) nil) (cleared self) (pending self) (note self)))

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
(defun make-date (year month day)
  "Make a date instance from year month and day.
  year may be nil."
  (make-instance 'date :year year :month month :day day))

(defun parse-date (date &key year)
  "Parse date and return a date object with year, month, and day
  specified.  Set year to nil if no year is specified."
  (apply #'make-date
   (mapcar (lambda (d)
	     (if d
		 (read-from-string d)
		 nil))
	   (coerce (second (multiple-value-list (scan-to-strings "(\\d+)?\\D?\\b(\\d+)\\D(\\d+)\\s*$" date))) 'list))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass entry ()
  ((date :accessor date :initarg :date)
   (effective-date :accessor effective-date :initarg :effective-date)
   (desc :accessor desc :initarg :desc)
   (cleared :accessor cleared :initarg :cleared)
   (pending :accessor pending :initarg :pending)
   (code :accessor code :initarg :code)
   (transactions :accessor transactions)
   ))

(defun parse-entry-date (date)
  "Parse the date and any effective date.  Return two date objects,
  the second one is nil if there is no effective date."
  (let ((at-pos (position #\= date)))
    (if at-pos
	(values (parse-date (subseq date 0 at-pos)) (parse-date (subseq date (+ 1 at-pos))))
	(values (parse-date date) nil))))

(defun parse-entry-line (line)
  "  A line beginning with a number denotes an entry. It may be followed
  by any number of lines, each beginning with whitespace, to denote
  the entry's account transactions. The format of the first line is:

          DATE[=EDATE] [*|!] [(CODE)] DESC

  Dates must be numerically specified.  The entry line *must* start
  with a numeral and the date cannot contain whitespace.

  * and ! mean cleared and pending.

  The code is arbitrary, but might be an invoice number or such.  It
  need not be a number.

  Desc is the description of the transactions.
  "
  (let ((parts (coerce (second (multiple-value-list (scan-to-strings "^(\\S+)\\s+([*!]*)\\s*(\\(([^)]*)\\))?\\s*(.*?)$"
								     line))) 'list))
	(actual-date) (effective-date))
    (multiple-value-setq (actual-date effective-date) (parse-entry-date (first parts)))
    (make-instance 'entry
		   :date actual-date
		   :effective-date effective-date
		   :cleared (not (equal (scan "\\*" (second parts)) nil))
		   :pending (not (equal (scan "\\!" (second parts)) nil))
		   :code (fourth parts)
		   :desc (fifth parts))))

(defun parse-entry (entry)
  "Take a textual description of an entry and transactions in ledger
  format and return an entry object.
  "
  (let* ((lines (split-sequence:split-sequence #\Newline entry))
	(entry (parse-entry-line (pop lines))))
    (setf (transactions entry) (mapcar (lambda (line) (parse-transaction line)) lines))
    ;entry
    ))

(parse-entry "2010/2/7=2010/2/14 *! (code) Description of transaction (with parens) and * and ! and 1976/11/29 for confusion.
    Expenses:Bureaucracy                                 $-359.00
    Liabilities:Due to/from Karl                         $179.50
    Liabilities:Due to/from James                        $-179.50
    Assets:Cash")
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ledger ()
  ((fname :accessor fname :initarg :fname)
   (entries :accessor entries)
   (curr-ledger-year :accessor curr-ledger-year 
		     :initarg curr-ledger-year
		     :documentation "The year that ledger entries are assumed to be in")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions called from the ledger file
(defun round-cent (f)
  "Round to the nearest hundreth"
  (/ (round (* f 100)) 100.0))

(defun depreciate-recurse (year month category amount term-left)
  "Do the recursive portion of monthly depreciation"
  (cat (format nil "~a/~a/15 Depreciate ~a~%   Assets:Prepaid:~a            $-~a~%   Expenses:Depreciation:~a     $~a~%~%" 
	       year month category
	       category amount
	       category amount)
       (unless (eq term-left 1)
	 (depreciate-recurse (+ year (floor (/ month 12.0))) 
			     (+ (mod month 12) 1) 
			     category 
			     amount 
			     (- term-left 1)))))

(defun depreciate (year month category total term)
  "Monthly depreciation"
  (let* ((amount (round-cent (/ total (float term))))
	(rounding-error (round-cent (- total (* amount term)))))
    (cat (depreciate-recurse year month category amount term)
	 (unless (equal rounding-error 0.0)
	   (format nil "~%~a/~a/15 Depreciation Rounding Fix~%   Assets:Prepaid:~a            $~a~%   Expenses:Depreciation:~a     $~a"
		   (round (/ (+ (* year 12) term) 12.0))   (+ (mod (+ month (- term 1)) 12) 1)
		   category (* -1 rounding-error)
		   category rounding-error
		   )))))

(defun unbillable (date description category amount)
  "Charge off unbillable work"
  (format nil "~a Charge off unbillable ~a~%   Expenses:Unbillable:~a     $~a~%   * Assets:Unbilled:~a   $-~a"
	  date description category amount category amount))

(defun shared-income (datedescrip category amount)
  "Income shared equally among partners, paid into the checking account"
  (format nil "~a~%   Income:~a   $-~a~%   Assets:Checking:James    $~a~%   Assets:Checking:Karl    $~a"
	  datedescrip category amount (/ amount 2) (/ amount 2)))

(defun shared-expense (datedescrip category amount)
  "Expenses shared equally among the partners, paid from the checking account"
  (format nil "~a~%   Expenses:~a   $~a~%   Assets:Checking:James    $-~a~%   Assets:Checking:Karl    $-~a"
	  datedescrip category amount (/ amount 2) (/ amount 2)))

(defun prepaid-hours (datedescrip partner client amount)
  (format nil "~a~%   Liabilities:~a:~a          $~a~%   Assets:Checking:~a:Unearned      $~a"
	  datedescrip partner client amount partner amount))

(defun satisfied-invoice (date invoice-number partner client amount)
  (format nil "~a Satisfaction of Invoice #~a~%   * Assets:Receivable:~a:~a        $~a~%   Assets:Checking:~a                       $~a"
	  date invoice-number partner client (* -1 amount) partner amount))

;(preprocess-ledger-file *ledger-fname*) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-ledger-file (file-lines)
  "Take a list of lines from a ledger file and return them without
  blank lines, without comment lines and with lisp expressions
  evaluated."
  (mapcar (lambda (x)
	    (if (scan "^\\W*\\(" x)
		(read-from-string (cat "#." x))
		x));(parse-entry x)))
	  file-lines))

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

(defun preprocess-ledger-file (fname)
  (with-open-file (stream (cat fname ".gen")
			  :direction :OUTPUT
			  :if-exists :SUPERSEDE)
    (mapcar (lambda (line)
	      (format stream "~a~%~%" line))
	    (parse-ledger-file (split-ledger-file fname)))))
;(preprocess-ledger-file *ledger-fname*)

