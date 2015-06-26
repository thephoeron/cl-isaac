;;;; file: t/cl-isaac.lisp

(in-package :cl-user)

(defpackage cl-isaac-test
  (:use cl cl-isaac prove))

(in-package :cl-isaac-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-isaac)' in your Lisp.

(plan 1)

(deftest sanity-check
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4.")
  (is (length "four")
      4
      "Length: (length \"four\") => 4."))

(run-test-all)

;; EOF
