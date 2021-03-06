;;;; FiveAM testing for Sassetti
;;;;
;;;; See COPYING for copyright and licensing information.
(in-package #:sassetti)

(def-suite utils :description "Test Sassetti's utility functions" :in sassetti)
(in-suite utils)

(test cat
  (is (equal "Hello World!" (cat "Hello " "World" "!"))))
(test trim-whitespace
  "Test trimming of leading and trailing whitespace"
  (is (equal "T E S T" (trim-whitespace "T E S T")))
  (is (equal "TEST" (trim-whitespace " TEST ")))
  (is (equal "TEST" (trim-whitespace "  TEST  ")))
  (is (equal "TEST" (trim-whitespace " TEST")))
  (is (equal "TEST" (trim-whitespace "  TEST")))
  (is (equal "TEST" (trim-whitespace "TEST ")))
  (is (equal "TEST" (trim-whitespace "TEST  "))))

(test rational-from-float-string
  (is (= 2134561/10 (rational-from-float-string "213456.10")))
  (is (= 213456 (rational-from-float-string "213456.0")))
  (is (= 213456 (rational-from-float-string "213456.00")))
  (is (= 21345601/100 (rational-from-float-string "213456.01")))
  (is (= 21345699/100 (rational-from-float-string "213456.99")))
  (is (= 10672800695763/50000000 (rational-from-float-string "213456.01391526")))
  (is (= 21345699165119217619219821/100000000000000000000 (rational-from-float-string "213456.99165119217619219821")))
  (is (= -235/2 (rational-from-float-string "-117.50")))
  (is (= -11749/100 (rational-from-float-string "-117.49")))
  (is (= -11651/100 (rational-from-float-string "-116.51")))
  (is (= 11749/100 (rational-from-float-string "117.49")))
  (is (= 11651/100 (rational-from-float-string "116.51")))
  (is (= 1/25 (rational-from-float-string "0.04")))
  (is (= -1/25 (rational-from-float-string "-0.04")))
  )
(test dollars
  (is (equal "123,112" (dollars 123112.00)))
  (is (equal "123,112.10" (dollars 123112.10)))
  (is (equal "123,112.10" (dollars 123112.1)))
  (is (equal "123,112.01" (dollars 123112.01)))
  (is (equal "123,112.99" (dollars 123112.99)))
  (is (equal "123112" (dollars 123112.00 :comma-char nil)))
  (is (equal "123112.10" (dollars 123112.10 :comma-char nil)))
  (is (equal "123112.10" (dollars 123112.1 :comma-char nil)))
  (is (equal "123112.01" (dollars 123112.01 :comma-char nil)))
  (is (equal "123112.99" (dollars 123112.99 :comma-char nil)))
  (is (equal "+123112.33" (dollars 123112.33 :comma-char nil :sign-p t)))
  (is (equal "-123112.10" (dollars -123112.10 :comma-char nil :sign-p t)))
  (is (equal "$-123112.10" (dollars -123112.10 :comma-char nil :pre-units "$")))
  (is (equal "35 AAPL" (dollars 35 :post-units " AAPL")))
  (is (equal "213,456.10" (dollars "213456.10")))
  (is (equal "213,456" (dollars "213456.0")))
  (is (equal "213,456" (dollars "213456.00")))
  (is (equal "213,456.01" (dollars "213456.01")))
  (is (equal "213,456.99" (dollars "213456.99")))
  (is (equal "$-117.50" (dollars -235/2 :comma-char nil :pre-units "$")))
  (is (equal "$-117.50" (dollars "-117.50" :comma-char nil :pre-units "$")))
  (is (equal "0.06" (dollars "0.06")))
  (is (equal "-0.06" (dollars "-0.06")))
  )
