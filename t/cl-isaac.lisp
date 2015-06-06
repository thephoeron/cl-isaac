;;;; file: t/cl-isaac.lisp

(in-package :cl-user)

(defpackage cl-isaac-test
  (:use cl cl-isaac prove))

(in-package :cl-isaac-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-isaac)' in your Lisp.

(plan 5)

(deftest sanity-check
  (is '(+ 1 1)
      2
      "Sane Lisp system."))

(run-test-all)

;; EOF
