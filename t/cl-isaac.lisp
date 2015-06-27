;;;; file: t/cl-isaac.lisp

(in-package :cl-user)

(defpackage cl-isaac-test
  (:use cl cl-isaac prove))

(in-package :cl-isaac-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-isaac)' in your Lisp.

(plan 2)

;; Sanity Test

(deftest sanity-check
  (pass "Prove is loaded and ready to go.")
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4.")
  (is (length "four")
      4
      "Length: (length \"four\") => 4."))

;; Self-seed Context Tests

(defparameter *ssctx32* (isaac:init-self-seed))

(defparameter *ssctx64* (isaac:init-self-seed :is64 t))

(deftest self-seed
  (is-type *ssctx32*
           'cl-isaac:isaac-ctx
           "*SSCTX32* has a type of ISAAC-CTX.")
  (ok (<= (integer-length (isaac:rand32 *ssctx32*))
          32)
      "Generated a random 32-bit integer with RAND32 on *SSCTX32*.")
  (is-type *ssctx64*
           'cl-isaac:isaac64-ctx
           "*SSCTX64* has a type of ISAAC64-CTX.")
  (ok (<= (integer-length (isaac:rand64 *ssctx64*))
          64)
      "Generated a random 64-bit integer with RAND64 on *SSCTX64*."))

(run-test-all)

;; EOF
