;;;; These are functions that get called from the ledger file.  They
;;;; are called user-funcs because they are designed to be called by
;;;; end users.
;;;;
;;;; See COPYING for copyright and licensing information.
(in-package #:sassetti)

;; This macro allows you to write $5 instead of "$5" when specifying dollars in your ledger file.
;; If you use ¥ instead, you can change the $ below to ¥.
(set-macro-character #\$
                     #'(lambda (stream char)
                         (cat (string char) (write-to-string (read stream)))))

(defun round-cent (f)
  "Round to the nearest hundreth"
  (values (/ (round f 1/100) 100) 
	  (second (multiple-value-list (round f 1/100)))))

(defun truncate-cent (f)
  "Truncate to the nearest hundreth"
  (values (/ (truncate f 1/100) 100) 
	  (second (multiple-value-list (truncate f 1/100)))))

(defun depreciate-recurse (year month category amount term-left err err+)
   "Do the recursive portion of monthly depreciation

  ERR is the monthly round error we have to account for
  ERR+ is the running total of unaccounted rounding error"
  (if (eq term-left 0) 
      (list)
      (let ((err+. (truncate-cent err+)))
	(cons (parse-entry (format nil 
				   (cat "~a/~a/15 Depreciate ~a~%"
					"   Assets:Prepaid:~a            ~a~%"
					"   Expenses:Depreciation:~a     ~a~%")
				   year month category
				   category (string-form amount :neg-p t :adjust (if (>= err+ 1/100) err+. 0))
				   category (string-form amount :adjust (if (>= err+ 1/100) err+. 0))))
	      (depreciate-recurse (+ year (floor (/ month 12.0))) 
				  (+ (mod month 12) 1) 
				  category 
				  amount 
				  (- term-left 1)
				  err
				  (- (+ err err+)
				     (if (>= err+ 1/100) err+. 0)))))))
(defun depreciate (year month category total term)
  "Monthly depreciation.

  TODO: do depreciation on specified day of month"
  (let* ((amount (parse-amount total))
	 (monthly (round-cent (/ (quantity amount) term)))
	 (rounding-error (/ (- (quantity amount) (* monthly term)) term)))
    (setf (quantity amount) monthly)
    (depreciate-recurse year month category amount term rounding-error rounding-error)))

(defun unbillable (date description category amount)
  "Charge off unbillable work"
  (parse-entry (format nil 
		       "~a Charge off unbillable ~a~%   Expenses:Unbillable:~a     $~a~%   * Assets:Unbilled:~a   $-~a"
		       date description category amount category amount)))

(defun shared-income (datedescrip category amount)
  "Income shared equally among partners, paid into the checking account"
  (parse-entry (format nil 
		       "~a~%   Income:~a   $-~a~%   Assets:Checking:James    $~a~%   Assets:Checking:Karl    $~a"
		       datedescrip category amount (/ amount 2) (/ amount 2))))

(defun shared-expense (datedescrip category amount &key (cleared-p nil) (pending-p nil) (code ""))
  "Expenses shared equally among the partners, paid from the checking account"
  (parse-entry (format nil 
		       "~a~%   Expenses:~a   $~a~%   Assets:Checking:James    $-~a~%   Assets:Checking:Karl    $-~a"
		       datedescrip category amount (/ amount 2) (/ amount 2))))

(defun prepaid-hours (datedescrip partner client amount)
  (parse-entry (format nil 
		       "~a~%   Liabilities:~a:~a          $~a~%   Assets:Checking:~a:Unearned      $~a"
		       datedescrip partner client amount partner amount)))

(defun satisfied-invoice (date invoice-number partner client amount)
  (parse-entry (format nil 
		       (cat "~a Satisfaction of Invoice #~a~%"
				"   * Assets:Receivable:~a:~a        $~a~%"
				"   Assets:Checking:~a        $~a")
		       date invoice-number partner client (* -1 amount) partner amount)))
