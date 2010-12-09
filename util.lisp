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
  `(if ,s
       (string-trim '(#\Space #\Tab #\Newline) ,s)
       ""))

(defun rational-from-float-string (fstring)
  "Take a string, such as '123112.2551' and return a
  rational (e.g. 123112/2551)"
  (let ((dp (position #\. fstring :test #'equal)))
    (if dp
	(let ((int (subseq fstring 0 dp))
	      (frac (subseq fstring (+ 1 dp))))
	  (+ (parse-integer int) (/ (parse-integer frac) (expt 10 (length frac)))))
	(parse-integer fstring))))

(defun dollars (amount  
		&key (sign-p nil) (trim-cents-p t)
		(width 0) (pad-char #\Space) (comma-char #\,) (decimal-char #\.) (decimal-places 2)
		(pre-units "") (post-units "") )
  "AMOUNT can be a string.  It will be converted to a rational before any calculations occur.

  WIDTH is the width of the field that will be left padded with PAD-CHAR characters.
  COMMA-CHAR is the character to use for the comma, as in 1,000.
  DECIMAL-CHAR is the character to use for the decimal point.
  DECIMAL-PLACES is the number of places after the decimal point.  Usually this is 2.
  If TRIM-CENTS-P is t, cents values of 0 will be trimmed: 4.00 will be written as 4.
  Set SIGN-P to t to force inclusion of the + sign.

  PRE-UNITS and POST-UNITS are strings to print fore and aft of the
  number so we know what the number means.  PRE-UNITS defaults to $.

  Returns a string representing the amount and formatted as the
  parameters dictate.
  "
  (when (stringp amount) (setf amount (rational-from-float-string amount)))
  (let* ((power (expt 10 decimal-places))
	 (digits 
	  (multiple-value-bind (dollars cents) (floor (abs amount) 1); (round (/ (round (* power amount)) power)) 
	    ;(setf cents (format nil "~2,'0D" (* 100 (+ (/ (+ cents (abs cents)) (* -2 cents)) 1 cents)))) ;(round cents 0.01))) ; 
	    (setf cents (format nil "~2,'0D" (round cents 0.01))) ; 
	    (when (and trim-cents-p (equal cents "00")) (setf cents "" decimal-char ""))
	    (if (equal nil comma-char)
		(format nil "~D~a~a" dollars decimal-char cents)
		(format nil "~,,V:D~a~a" comma-char dollars decimal-char cents))))
	 (sign (if (minusp amount) #\- (if sign-p #\+ nil))))
    (format nil "~a~a~a~a~a"
	    (make-string (max 0 (- width (length pre-units) (length post-units) (if sign 1 0) (length digits)))
			 :initial-element pad-char)
	    pre-units
	    (if sign sign "")
	    digits
	    post-units)))
