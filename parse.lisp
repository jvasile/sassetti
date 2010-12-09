(in-package #:sassetti)

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
(defun parse-amount (s)
  "Parse a portion of the amount that is a number and a symbol.
  Return an amount object with the number and symbol extracted
  appropriately."

  (if (equal #\@ (char s 0))
    (parse-amount (subseq s 1))
    (let* ((price (scan-to-strings "(-?[,.\\d]+)\\s*$" s))
	   (units-before (subseq s 0 (- (length s) (length price))))
	   (units-after ""))
      (unless price
	(setf price (scan-to-strings "^\\s*(-?[,.\\d]+)" s))
	(setf units-after (subseq s (length price) (length s)))
	(setf units-before ""))
      (setf price (or price "0"))
      (make-instance 'amount 
		     :quantity (if (equal price ".") 
				   0
				   ;(read-from-string (remove #\, price)))
				   (rational-from-float-string (trim-whitespace (remove #\, price))))
		     :units-before (regex-replace "^\\s*" units-before "")
		     :units-after (regex-replace "\\s*$" units-after "")))))

(defun parse-amount-complex (s)
  "Accept a string like '$3502' or '-23 Euros' or '22 Martian Dollars'
  or 'HKD 253.21'.  Returns an amount object with the quantity,
  units-before and units-after set from values extracted from the
  string.  Leading and trailing whitespace will be trimmed as needed.
  "
  (let ((at-pos (position #\@ s))
	(units) (unit-price nil))
    (if at-pos
	(progn
	  (setf units (parse-amount (subseq s 0 at-pos)))
	  (setf unit-price (parse-amount (subseq s (+ 1 at-pos))))
	  (when (scan "@@" s) (setf (quantity unit-price) (/ (quantity unit-price) (quantity units))))
	  )
	(setf units (parse-amount s)))
    (values units unit-price)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  TODO: handle dates in the note section
  TODO: do we need to trim leading space from the note?
  "
  (let ((parts (coerce (second (multiple-value-list (scan-to-strings "^\\s*([*!]*)\\s*(.*?)(\\s\\s+([^;]*);?(.*?))?$"
								     line))) 'list))
	(commodity) (unit-price))
    (multiple-value-setq (commodity unit-price)
      (if (equal "" (fourth parts))
	  (values nil nil)
	  (if (fourth parts)
	      (parse-amount-complex (fourth  parts))
	      (values nil nil))))
    (make-instance 'transaction 
		   :account (second parts)
		   :commodity commodity
		   :unit-price unit-price
		   :cleared (not (equal (scan "\\*" (first parts)) nil))
		   :pending (not (equal (scan "\\!" (first parts)) nil))
		   :note (trim-whitespace (fifth parts)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    entry
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-ledger-chunk (chunk)
  "Parse a chunk of the ledger file.  Chunks start with unindented
  lines.  All other lines are indented."
  (if (scan "^\\d" chunk)
      (parse-entry chunk)
      (if (scan "^\\(" chunk)
	  (read-from-string (cat "#." chunk))
	  chunk)))
(defun split-ledger-file (fname)
  "Returns a list of file FNAME split into chunks by cleaving on
  unindented lines."
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
(defun parse-ledger-file (fname)
  "Step through the ledger file and convert each chunk into a lisp entry or function call.
  For chunks we don't yet know how to convert, just store them as text.
  Returns a ledger object."
  (make-instance 'ledger 
		 :fname fname
		 :entries (mapcar 'parse-ledger-chunk (split-ledger-file (tilde:expand-tilde-namestring fname)))))

;(defun parse-ledger-file (file-lines)
;  "Take a list of lines from a ledger file and return them without
;  blank lines, without comment lines and with lisp expressions
;  evaluated."
;  (mapcar (lambda (x)
;	    (if (scan "^\\W*\\(" x)
;		(read-from-string (cat "#." x))
;		x));(parse-entry x)))
;	  file-lines))



