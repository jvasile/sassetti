;;;; FiveAM testing for Sassetti
;;;;
;;;; See COPYING for copyright and licensing information.
(in-package #:sassetti)

(def-suite sassetti :description "Test Suite for Sasetti")
(in-suite sassetti)

(test get-as-list 
  (is (equal (list nil) (get-as-list nil)))
  (is (equal '("" 5 " HKD") (get-as-list (make-instance 'amount :quantity 5 :units-before "" :units-after " HKD"))))
  (is (equal '("$" 1 "") (get-as-list (make-instance 'amount :quantity 1 :units-before "$" :units-after ""))))
  (is (equal '(1976 11 29) (get-as-list (parse-date "1976/11/29"))))
  (is (equal '("Expenses:Bureaucracy:Add a space" ("$" -359 "") (nil) nil nil "note")
	     (get-as-list (parse-transaction "  Expenses:Bureaucracy:Add a space       $-359.00 ;note"))))
  (is (equal '("Expenses:Bureaucracy:Add a space" (nil) (nil) nil nil "note")
	     (get-as-list (parse-transaction "  Expenses:Bureaucracy:Add a space       ;note"))))
  )

(def-suite string-form :description "Test the object class definitions" :in sassetti)
(in-suite string-form)

(test string-form-simple-cases
  (is (equal "!!" (string-form "!!")))
  (is (equal "" (string-form nil))))

(test string-form-amount
  "Test string-form methods for various objects."
  (is (equal "5 HKD" (string-form (make-instance 'amount :quantity 5 :units-before "" :units-after " HKD"))))
  (is (equal "$1" (string-form (make-instance 'amount :quantity 1 :units-before "$" :units-after ""))))
  (is (equal "$1.50" (string-form (make-instance 'amount :quantity 1.5 :units-before "$" :units-after ""))))
  (is (equal "$1.50" (string-form (make-instance 'amount :quantity 1.50 :units-before "$" :units-after ""))))
  (is (equal "$1.95" (string-form (make-instance 'amount :quantity 1.95 :units-before "$" :units-after ""))))
  (is (equal "HKD 1.95" (string-form (make-instance 'amount :quantity 1.95 :units-before "HKD " :units-after ""))))
  (is (equal "$-117.50" (string-form (parse-amount "$-117.50"))))
  (is (equal "$-117" (string-form (parse-amount "$-117")))))

(test string-form-date
  (is (equal "1975/11/12" (string-form (make-instance 'date :year 1975 :month 11 :day 12))))
  (is (equal "1977/10/01" (string-form (make-instance 'date :year 1977 :month 10 :day 1))))
  (is (equal "1977/04/13" (string-form (make-instance 'date :year 1977 :month 4 :day 13))))
  (is (equal "1975/11/12" (string-form (make-instance 'date :year 1975 :month 11 :day 12))))
  (is (equal "10/01" (string-form (make-instance 'date :year nil :month 10 :day 1))))
  (is (equal "04/13" (string-form (make-instance 'date :year nil :month 4 :day 13))))
  )

(test string-form-transaction
  (is (equal "      Expenses:Bureaucracy:Add a space     $-359 ;note"
	     (string-form (parse-transaction "Expenses:Bureaucracy:Add a space       $-359.00 ;note")))))

(def-suite classes :description "Test the object class definitions" :in sassetti)
(in-suite classes)

(test make-date
  (let ((d (make-date 1976 11 29)))
    (is (equal "DATE" (string (type-of d))))
    (is (= 1976 (year d)))
    (is (= 11 (month d)))
    (is (= 29 (day d)))))

(test date
  (let ((d (make-instance 'date :year 1976 :month 11 :day 29)))
    (is (equal "DATE" (string (type-of d))))
    (is (= 1976 (year d)))
    (is (= 11 (month d)))
    (is (= 29 (day d))))
  (let ((d (make-instance 'date :month 11 :day 29)))
    (is (equal "DATE" (string (type-of d))))
    (is (not (year d)))
    (is (= 11 (month d)))
    (is (= 29 (day d)))))

(test amount
  (let ((amt (make-instance 'amount :quantity 50 :units-before "$" :units-after "")))
    (is (= 50 (quantity amt)))
    (is (equal "$" (units-before amt)))
    (is (equal "" (units-after amt))))
  (is (equal "HKD" (units (make-instance 'amount :quantity 5 :units-before "" :units-after " HKD"))))
  (is (equal "$" (units (make-instance 'amount :quantity 15 :units-before "$" :units-after ""))))
  (is (equal "Euro" (units (make-instance 'amount :quantity 25 :units-before "Euro " :units-after "")))))



(def-suite parser :description "Test the ledger file parser" :in sassetti)
(in-suite parser)

(test parse-date
  (is (equal '(1976 11 29) (get-as-list (parse-date "1976/11/29"))))
  (is (equal '(NIL 11 29) (get-as-list (parse-date "11/29"))))
  (is (equal '(1976 11 29) (get-as-list (parse-date "1976-11-29"))))
  (is (equal '(NIL 11 29) (get-as-list (parse-date "11-29"))))
  )

(test parse-entry-date
  (multiple-value-bind (d e) (parse-entry-date "2010-8-4")
    (is (equal "2010/08/04" (string-form d)))
    (is (not e)))
  (multiple-value-bind (d e) (parse-entry-date "    2010-8-4")
    (is (equal "2010/08/04" (string-form d)))
    (is (not e)))
  (multiple-value-bind (d e) (parse-entry-date "2010-8-4   ")
    (is (equal "2010/08/04" (string-form d)))
    (is (not e)))
  (multiple-value-bind (d e) (parse-entry-date "    2010-8-4   ")
    (is (equal "2010/08/04" (string-form d)))
    (is (not e)))
  (multiple-value-bind (d e) (parse-entry-date "2010-8-14=2010-9-1")
    (is (equal "2010/08/14" (string-form d)))
    (is (equal "2010/09/01" (string-form e))))
  (multiple-value-bind (d e) (parse-entry-date "    2010-8-14=2010-9-1")
    (is (equal "2010/08/14" (string-form d)))
    (is (equal "2010/09/01" (string-form e))))
  (multiple-value-bind (d e) (parse-entry-date "2010-8-14=2010-9-1    ")
    (is (equal "2010/08/14" (string-form d)))
    (is (equal "2010/09/01" (string-form e))))
  (multiple-value-bind (d e) (parse-entry-date "    2010-8-14=2010-9-1    ")
    (is (equal "2010/08/14" (string-form d)))
    (is (equal "2010/09/01" (string-form e))))
  )

(test parse-transaction
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) nil nil "") (get-as-list (parse-transaction "    Liabilities:Due to/from Karl"))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) nil nil "") (get-as-list (parse-transaction "    Liabilities:Due to/from Karl      "))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) nil nil "note")
	     (get-as-list (parse-transaction "    Liabilities:Due to/from Karl      ;note"))))
  (is (equal '("Liabilities:Due to/from Karl" ("$" 23101/100 "") (nil)  nil nil "note") 
	     (get-as-list (parse-transaction "    Liabilities:Due to/from Karl    $231.01; note"))))
  (is (equal '("Liabilities:Due to/from Karl" ("$" 232 "") (nil) nil nil "note") 
	     (get-as-list (parse-transaction "    Liabilities:Due to/from Karl    $232.00    ;  note"))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) nil nil "note")
	     (get-as-list (parse-transaction "    Liabilities:Due to/from Karl    ;note"))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) t nil "") (get-as-list (parse-transaction "    * Liabilities:Due to/from Karl      "))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) nil t "") (get-as-list (parse-transaction "    ! Liabilities:Due to/from Karl      "))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) t t "") (get-as-list (parse-transaction "    *! Liabilities:Due to/from Karl      "))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) t t "") (get-as-list (parse-transaction "    !* Liabilities:Due to/from Karl      "))))
  (is (equal '("Liabilities:Due to/from Karl" (nil) (nil) t t "") (get-as-list (parse-transaction "    *!Liabilities:Due to/from Karl      "))))
  (is (equal '("Assets:Cash" (nil) (nil) nil nil "") (get-as-list (parse-transaction "    Assets:Cash"))))
  (is (equal '("Assets:Cash" (nil) (nil) nil nil "") (get-as-list (parse-transaction "Assets:Cash"))))
  (is (equal '("Assets:Cash" (nil) (nil) t nil "") (get-as-list (parse-transaction "*Assets:Cash"))))
  (is (equal '("Assets:Equines" ("" 10 "") ("$" 5000 "") t nil "") (get-as-list (parse-transaction "*Assets:Equines      10 @ $5000"))))
  (is (equal '("Assets:Equines" ("" 10 " Horses") ("$" 5000 "") t nil "")
	     (get-as-list (parse-transaction "*Assets:Equines      10 Horses @ $5000"))))
  (is (equal '("Assets:Equines" ("" 10 " Horses") ("$" 5000 "") t nil "A monopoly on poloponies!")
	     (get-as-list (parse-transaction "*Assets:Equines      10 Horses @@ $50000;  A monopoly on poloponies!"))))
  )

(test parse-entry-line
  (is (equal (list "2010/02/07" "2010/02/14" 
		   "Description of transaction (with parens) and * and ! and 1976/11/29 for confusion."
		   t t "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 *! (code) Description of transaction (with parens) and * and ! and 1976/11/29 for confusion."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction." t t "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 *! (code) Description of transaction."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction." t t "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 *! (code)Description of transaction."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction." t t "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 *!(code)Description of transaction."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction." nil nil "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 (code)Description of transaction."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction (with parens)." t t nil nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 *! Description of transaction (with parens)."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction (with parens)." nil nil "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 (code) Description of transaction (with parens)."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction (with parens)." t nil "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 * (code) Description of transaction (with parens)."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction (with parens)." nil t "code" nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 ! (code) Description of transaction (with parens)."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction (with parens)." nil t nil nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 ! Description of transaction (with parens)."))))
  (is (equal (list "2010/02/07" "2010/02/14" "Description of transaction (with parens)." t nil nil nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7=2010/2/14 * Description of transaction (with parens)."))))
  (is (equal (list "2010/02/07" nil "Description of transaction (with parens)." t t nil nil)
	     (get-as-list 
	      (parse-entry-line "2010/2/7 *! Description of transaction (with parens)."))))
  (is (equal (list "02/07" nil "Description of transaction (with parens)." t t nil nil)
	     (get-as-list 
	      (parse-entry-line "2/7 *! Description of transaction (with parens)."))))
  )

(test parse-entry 
   (is (equal "2010/02/07=2010/02/14 *! (code) Description of transaction (with parens) and * and ! and 1976/11/29 for confusion.
      Expenses:Bureaucracy              $-359
      Liabilities:Due to/from Karl      $179.50
      Liabilities:Due to/from James     $-179.50
      Assets:Cash                       "
	      (string-form (parse-entry "2010/2/7=2010/2/14 *! (code) Description of transaction (with parens) and * and ! and 1976/11/29 for confusion.
    Expenses:Bureaucracy                                 $-359.00
    Liabilities:Due to/from Karl                         $179.50
    Liabilities:Due to/from James                        $-179.50
    Assets:Cash"))))

   (is (equal "2010/02/07=2010/02/14 *! (code) Description of transaction (with parens) and * and ! and 1976/11/29 for confusion."
	      (string-form 
	       (parse-entry "2010/2/7=2010/2/14 *! (code) Description of transaction (with parens) and * and ! and 1976/11/29 for confusion."))))
  )

(def-suite parse-amount :description "Test the parse-amount routines" :in parser)
(in-suite parse-amount)

(test parse-amount
  (is (equal "$13219.23" (string-form (parse-amount "$13,219.23"))))
  (is (equal "$1732" (string-form (parse-amount "$1,732 "))))
  (is (equal "$1.12" (string-form (parse-amount "   $1.12  "))))
  (is (equal '("$" 1 "") (get-as-list (parse-amount "   $1  "))))
  (is (equal '("HKD " 151299/100 "") (get-as-list (parse-amount "   HKD 1512.99  "))))
  (is (equal '("" 1 " HKD") (get-as-list (parse-amount "   1 HKD  "))))

  (is (equal '("" 0 "$") (get-as-list (parse-amount "$"))))
  (is (equal '("" 1 "") (get-as-list (parse-amount "1"))))

  (is (equal '("$" -255/2 "") (get-as-list (parse-amount "$-127.50"))))
  )

(test parse-amount-complex
  (is (equal '("$" 1 "") (get-as-list (first (multiple-value-list (parse-amount-complex "$1"))))))
  (is (eq nil (second (multiple-value-list (parse-amount-complex "$1")))))
  (is (equal '("" 5 " goats") (get-as-list (first (multiple-value-list (parse-amount-complex "5 goats @ $30"))))))
  (is (equal '("$" 30 "") (get-as-list (second (multiple-value-list (parse-amount-complex "5 goats @ $30"))))))
  (is (equal '("" 5 " goats") (get-as-list (first (multiple-value-list (parse-amount-complex "5 goats @ $300"))))))
  (is (equal '("$" 60 "") (get-as-list (second (multiple-value-list (parse-amount-complex "5 goats @@ $300"))))))
  (is (equal '("$" 60 "") (get-as-list (second (multiple-value-list (parse-amount-complex "    5 goats @@ $300    "))))))
  (is (equal "$350" (string-form (parse-amount-complex "$350"))))
  )

(defun test-all ()
  (setf *break-on-signals* nil)
  (run! 'sassetti)
  (setf *break-on-signals* 'error)
  "done")
;(test-all)
(defun run-test (tst)
  (setf *break-on-signals* nil)
  (run! tst)
  (setf *break-on-signals* 'error)
  "done")
