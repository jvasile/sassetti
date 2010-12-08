;;;; These are functions that get called from the ledger file.  They
;;;; are called user-funcs because they are designed to be called by
;;;; end users.
;;;;
;;;; See COPYING for copyright and licensing information.
(in-package #:sassetti)

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
